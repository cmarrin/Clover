/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

/*

Pond Light Controller

Each controller is a 5V Arduino Nano with serial data coming in on pin D11 and out on pin
D10. Data is output to an 8 pixel WS2812 ring on D6. Serial data, +5V and Gnd comes into 
each post and goes out to the next post.

Serial Protocol

Standard RS232 protocol is used with 1 start bit, 8 data bits and 1 stop bit. Bit rate is
set by the protocol from 1200 bps to 57600 bps. Bit rate is currently fixed at 300 baud.

Each command has a series of bytes with the format:

	Lead-in		'('
	Address		Which controller this command is for (0 is all devices)
	Command		Single char command 
	Size		Payload size in bytes
	Payload		Bytes of payload (in binary)
	Checksum	One byte checksum
	Lead-out	')'

All characters but the payload and payload size are between 0x30 and 0x6f. Max payload size is
256 bytes. Checksum is computed by adding all characters of the buffer, including leading and 
ending characters, truncating the result to 6 bits and adding 0x30 to the result. For purposes 
of computing checksum, a '0' is placed in the checksum location.

To allow for interpreted commands each "normal" command (those which cause the lights to do a 
pattern) are limited to 16 bytes. This is enough for 4 colors (12 bytes) plus 4 byte params. The only
command which can exceed this is the 'X' command.

Command List:

   Command 	Name         	Params						Description
   -------  ----            ------    					-----------
	'C'     Constant Color 	color, n, d					Set lights to passed color
                                                        Flash n times, for d duration (in 100ms units)
                                                        If n == 0, just turn lights on

	'X'		EEPROM[0]		addr, <data>				Write EEPROM starting at addr. Data can be
														up to 64 bytes, due to buffering limitations.

			Rainbow			color, speed, range, mode	Change color through the rainbow by changing hue 
														from the passed color at the passed speed. Range
														is how far from the passed color to go, 0 (very little)
														to 7 (full range). If mode is 0 colors bounce 
														back and forth between start color through range. If
														it is 1 colors cycle the full color spectrum and
														range is ignored.

A Color param is 3 consecutive bytes (hue, saturation, value). Saturation and value are levels between 0 and 255.
Hue is an angle on the color wheel. A 0-360 degree value is obtained with hue / 256 * 360.

*/

#include <Clover.h>

#include <SoftwareSerial.h>
#include <Adafruit_NeoPixel.h>

#include "Flash.h"
#include "InterpretedEffect.h"

constexpr int LEDPin = 6;
constexpr int NumPixels = 8;
constexpr int MaxPayloadSize = 1017; // 1024 - 7 (the size of the header + footer)
constexpr int CommandSizeBefore = 5;
constexpr int CommandSizeAfter = 2;
constexpr char StartChar = '(';
constexpr char EndChar = ')';
constexpr unsigned long SerialTimeOut = 2000; // ms
constexpr int32_t MaxDelay = 1000; // ms

class PostLightController
{
public:
	PostLightController()
		: _pixels(NumPixels, LEDPin, NEO_GRB + NEO_KHZ800)
		, _serial(11, 10)
		, _interpretedEffect(&_pixels)
	{
        _buf = _interpretedEffect.stackBase();
    }

	~PostLightController() { }
	
	void setup()
	{
	    Serial.begin(115200);
        while (!Serial) ; // wait for Arduino Serial Monitor to open
	    _serial.begin(2400);
		
		delay(500);

        randomSeed(millis());

	    _pixels.begin(); // This initializes the NeoPixel library.
	    _pixels.setBrightness(255);
		
        fmt::printf("Post Light Controller v0.4\n");
      
		showStatus(StatusColor::Green, 3, 2);
		
		_timeSinceLastChar = millis();
	}

	void loop()
	{
		int32_t delayInMs = 0;
        if (_effect == Effect::Flash) {
            delayInMs = _flash.loop(&_pixels);
        } else if (_effect == Effect::Interp) {
            delayInMs = _interpretedEffect.loop();
        }
        
        if (delayInMs > MaxDelay) {
            delayInMs = MaxDelay;
        }
		
		if (delayInMs < 0) {
			// An effect has finished. End it and clear the display
			showColor(0, 0, 0, 0, 0);
            _effect = Effect::None;
			delayInMs = 0;
		}
		
		uint32_t newTime = millis();

		// If we're capturing and it's been a while, error
		if (_state != State::NotCapturing) {
			if (newTime - _timeSinceLastChar > SerialTimeOut) {
				_state = State::NotCapturing;
				Serial.print(F("***** char timeout, rst\n"));
				showStatus(StatusColor::Red, 3, 2);
				_state = State::NotCapturing;
			}
		}

	    if (_serial.available()) {
			_timeSinceLastChar = newTime;
			
			char c = char(_serial.read());
			
			// Run the state machine
			
			switch(_state) {
				case State::NotCapturing:
					if (c == StartChar) {
                        _bufIndex = 0;
                        _buf[_bufIndex++] = c;
						_state = State::DeviceAddr;
						_expectedChecksum = c;
                        _cmdPassThrough = false;
                        _cmdExecute = false;
					}
					break;
				case State::DeviceAddr:
                    if (c == '0') {
                        // If addr is '0' then this command is for all devices.
                        // Execute it and pass it through.
                        _cmdPassThrough = true;
                        _cmdExecute = true;
                    } else if (c == '1') {
                        // If addr is '1' then this command is for this
                        // device only. Execute but don't pass through.
                        _cmdPassThrough = false;
                        _cmdExecute = true;
                    } else {
                        // If addr is greater than '1' then this command
                        // is for another device. decrement the address
                        // and pass it along.
                        _cmdPassThrough = true;
                        _cmdExecute = false;
                        c -= 1;
                    }
                    
                    _buf[_bufIndex++] = c;
                    _state = State::Cmd;
					_expectedChecksum += c;
					break;
				case State::Cmd:
                    _buf[_bufIndex++] = c;
					_cmd = c;
					_state = State::SizeHi;
					_expectedChecksum += c;
					break;
				case State::SizeHi:
                    _buf[_bufIndex++] = c;
					_state = State::SizeLo;
					_payloadSize = uint16_t(c) << 8;
					_expectedChecksum += c;
					break;
				case State::SizeLo:
                    _buf[_bufIndex++] = c;
					_payloadSize |= uint16_t(uint8_t(c));
                    _payloadReceived = 0;
                    Serial.print(F("Buffer size="));
                    Serial.println(_payloadSize);
					
                    if (_payloadSize > MaxPayloadSize) {
						Serial.print(F("Buf too big. Size="));
						Serial.println(_payloadSize);
						showStatus(StatusColor::Red, 6, 1);
						_state = State::NotCapturing;
					} else {
						_state = State::Data;
						_expectedChecksum += c;
					}

                    if (_cmd == 'X') {
                        showStatus(StatusColor::Blue, 0, 0);
                    }
					break;
				case State::Data:
					_buf[_bufIndex++] = c;
                    _payloadReceived++;
					_expectedChecksum += c;
					
					if (_payloadReceived >= _payloadSize) {
						_state = State::Checksum;
					}
					break;
				case State::Checksum: {     
                    // If we're passing through and not executing, we've decremented
                    //  the address so we have to decrement the checksum as well
					_actualChecksum = (_cmdPassThrough && !_cmdExecute) ? (c - 1) : c;
					_buf[_bufIndex++] = _actualChecksum;
					_state = State::LeadOut;
					_expectedChecksum += '0';
					break;
				}
				case State::LeadOut:
					if (c != EndChar) {
						Serial.println(F("Exp lead-out"));
						showStatus(StatusColor::Red, 6, 1);
						_state = State::NotCapturing;
					} else {
                        _buf[_bufIndex++] = c;
                        
						// Make sure checksum is right
						_expectedChecksum += c;
						_expectedChecksum = (_expectedChecksum & 0x3f) + 0x30;
						
						if (_expectedChecksum != _actualChecksum) {
							Serial.print(F("CRC ERROR: exp="));
							Serial.print(_expectedChecksum);
							Serial.print(F(", actual="));
							Serial.print(_actualChecksum);
							Serial.print(F(", cmd: "));
							Serial.println(_cmd);
							showStatus(StatusColor::Red, 5, 5);
							_state = State::NotCapturing;
						} else {
							// Have a good buffer
							_state = State::NotCapturing;

                            if (_cmd == 'X') {
                                showStatus(StatusColor::Yellow, 0, 0);
                            }

                            // Pass through buffer if needed
                            if (_cmdPassThrough) {
                                for (uint16_t i = 0; i < _payloadSize + CommandSizeBefore + CommandSizeAfter; i++) {
                                    _serial.write(_buf[i]);
                                }
                            }
       
                            if (_cmd == 'X') {
                                showStatus(StatusColor::Green, 0, 0);
                            }

                            if (!_cmdExecute) {
                                break;
                            }
                            
                            // Handle the command
                            _effect = Effect::None;
							
							switch(_cmd) {
								case 'C':
								showColor(_buf[CommandSizeBefore], 
                                          _buf[CommandSizeBefore + 1], 
                                          _buf[CommandSizeBefore + 2], 
                                          _buf[CommandSizeBefore + 3], 
                                          _buf[CommandSizeBefore + 4]);
								break;
								
								case 'X': {
									// Cancel effect
                                    _effect = Effect::None;
									
									if (_payloadSize > 1024) {
										Serial.print(F("inv EEPROM size="));
										Serial.println(_payloadSize);
										showStatus(StatusColor::Red, 5, 5);
										_state = State::NotCapturing;
									} else {
										Serial.print(F("exec => EEPROM: size="));
										Serial.println(_payloadSize);

										for (uint16_t i = 0; i < _payloadSize; ++i) {
											EEPROM[i] = _buf[i + CommandSizeBefore]; // Skip cmd chars and start addr
										}
									}
                                    Serial.println(F("Finished upload"));
                                    showStatus(StatusColor::Blue, 5, 1);
									break;
								}
								default:
								// See if it's interpreted, payload is after cmd bytes, offset it
								if (!_interpretedEffect.init(_cmd, _buf + CommandSizeBefore, _payloadSize)) {
									String errorMsg;
									switch(_interpretedEffect.error()) {
                                        case clvr::Memory::Error::None:
                                        errorMsg = F("---");
                                        break;
                                        case clvr::Memory::Error::InvalidSignature:
                                        errorMsg = F("bad signature");
                                        break;
                                        case clvr::Memory::Error::InvalidVersion:
                                        errorMsg = F("bad  version");
                                        break;
                                        case clvr::Memory::Error::WrongAddressSize:
                                        errorMsg = F("wrong addr size");
                                        break;
                                        case clvr::Memory::Error::NoEntryPoint:
                                        errorMsg = F("no entry point");
                                        break;
                                        case clvr::Memory::Error::NotInstantiated:
                                        errorMsg = F("not instantiated");
                                        break;
                                        case clvr::Memory::Error::UnexpectedOpInIf:
                                        errorMsg = F("bad op in if");
                                        break;
                                        case clvr::Memory::Error::InvalidOp:
                                        errorMsg = F("inv op");
                                        break;
                                        case clvr::Memory::Error::OnlyMemAddressesAllowed:
                                        errorMsg = F("mem addrs only");
                                        break;
                                        case clvr::Memory::Error::AddressOutOfRange:
                                        errorMsg = F("addr out of rng");
                                        break;
                                        case clvr::Memory::Error::ExpectedSetFrame:
                                        errorMsg = F("SetFrame needed");
                                        break;
                                        case clvr::Memory::Error::InvalidModuleOp:
                                        errorMsg = F("inv mod op");
                                        break;
                                        case clvr::Memory::Error::InvalidNativeFunction:
                                        errorMsg = F("inv native func");
                                        break;
                                        case clvr::Memory::Error::NotEnoughArgs:
                                        errorMsg = F("not enough args");
                                        break;
                                        case clvr::Memory::Error::WrongNumberOfArgs:
                                        errorMsg = F("wrong arg cnt");
                                        break;
                                        case clvr::Memory::Error::StackOverrun:
                                        errorMsg = F("can't call, stack full");
                                        break;
                                        case clvr::Memory::Error::StackUnderrun:
                                        errorMsg = F("stack underrun");
                                        break;
                                        case clvr::Memory::Error::StackOutOfRange:
                                        errorMsg = F("stack out of range");
                                        break;
                                        case clvr::Memory::Error::ImmedNotAllowedHere:
                                        errorMsg = F("immed not allowed here");
                                        break;
                                        case clvr::Memory::Error::InternalError:
                                        errorMsg = F("internal err");
                                        break;
									}

									Serial.print(F("Interp fx err: "));
									Serial.println(errorMsg);
									showStatus(StatusColor::Red, 5, 1);
									_state = State::NotCapturing;
								} else {
                                    _effect = Effect::Interp;
								}
								break;
							}
						}
					}

					break;
			}
	  	}

		// Eventually we can't delay in loop because we have to feed the soft serial port
		// But for now...
		delay(delayInMs);
	}

private:
	static uint8_t checksum(const char* buf, int length) 
	{
	    uint8_t sum = 0;

	    for (int i = 0; i < length; i++) {
			sum += buf[i];
	    }
	    return sum & 0x3f;
	}

	enum class StatusColor { Red, Green, Yellow, Blue };

	void showColor(uint8_t h, uint8_t s, uint8_t v, uint8_t n, uint8_t d)
	{
        _effect = Effect::Flash;
        _flash.init(&_pixels, h, s, v, n, d);
	}
	
	void showStatus(StatusColor color, uint8_t numberOfBlinks = 0, uint8_t interval = 0)
	{
		// Flash half bright red or green at passed interval and passed number of times
        _effect = Effect::Flash;
        uint8_t h = 128;
        
        switch (color) {
            case StatusColor::Red: h = 0; break;
            case StatusColor::Green: h = 85; break;
            case StatusColor::Yellow: h = 30; break;
            case StatusColor::Blue: h = 140; break;
        }
        _flash.init(&_pixels, h, 0xff, 0x80, numberOfBlinks, interval);
	}
	
	Adafruit_NeoPixel _pixels;
	SoftwareSerial _serial;
	
    enum class Effect { None, Flash, Interp };
    Effect _effect = Effect::None;
	Flash _flash;
	InterpretedEffect _interpretedEffect;
	
    // We share the incoming buffer with the interpreter stack
	uint8_t* _buf = nullptr;
	uint16_t _bufIndex = 0;
    
    uint16_t _payloadReceived = 0;
	uint16_t _payloadSize = 0;
    
	unsigned long _timeSinceLastChar = 0;
	
	uint8_t _expectedChecksum = 0;
	uint8_t _actualChecksum = 0;
	
	uint8_t _cmd = '0';
 
    bool _cmdPassThrough = false;
    bool _cmdExecute = true;
	
	// State machine
	enum class State { NotCapturing, DeviceAddr, Cmd, SizeHi, SizeLo, Data, Checksum, LeadOut };
	State _state = State::NotCapturing;
};

PostLightController controller;

void setup()
{
	controller.setup();
}

void loop()
{
	controller.loop();
}
