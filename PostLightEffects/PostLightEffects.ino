/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include <Clover.h>

#include <Adafruit_NeoPixel.h>
#include <EEPROM.h>
#include "PostLightEffects.h"
#include "Defines.h"

/*

PostLightEffects

Each test is included as a .h file generated on the Mac and appears as a
uint_t array with a name of the form 'EEPROM_Upload_<name>'. Each array is
uploaded to EEPROM and then the test is run.
*/

constexpr int LEDPin = 6;
constexpr int NumPixels = 8;

static constexpr uint32_t StackSize = 1024;

class PostLightEffects : public clvr::Interpreter<StackSize>
{
public:
    PostLightEffects() : _pixels(NumPixels, LEDPin, NEO_GRB + NEO_KHZ800) { }

    virtual void setLight(uint8_t i, uint8_t h, uint8_t s, uint8_t v) override
    {
        _pixels.setPixelColor(i, Adafruit_NeoPixel::gamma32(Adafruit_NeoPixel::ColorHSV(uint16_t(h) * 256, s, v)));
		_pixels.show();
    }

    void showError(PostLightEffects::Error error)
    {
        String errorMsg("!!!");
        
        switch(error) {
            case PostLightEffects::Error::None:
            errorMsg = F("???");
            break;
            case PostLightEffects::Error::InvalidSignature:
            errorMsg = F("bad signature");
            break;
            case PostLightEffects::Error::NoEntryPoint:
            errorMsg = F("no entry point");
            break;
            case PostLightEffects::Error::UnexpectedOpInIf:
            errorMsg = F("bad op in if");
            break;
            case PostLightEffects::Error::InvalidOp:
            errorMsg = F("inv op");
            break;
            case PostLightEffects::Error::OnlyMemAddressesAllowed:
            errorMsg = F("mem addrs only");
            break;
            case PostLightEffects::Error::AddressOutOfRange:
            errorMsg = F("addr out of rng");
            break;
            case PostLightEffects::Error::ExpectedSetFrame:
            errorMsg = F("SetFrame needed");
            break;
            case PostLightEffects::Error::InvalidModuleOp:
            errorMsg = F("inv mod op");
            break;
            case PostLightEffects::Error::InvalidNativeFunction:
            errorMsg = F("inv native func");
            break;
            case PostLightEffects::Error::NotEnoughArgs:
            errorMsg = F("not enough args");
            break;
            case PostLightEffects::Error::WrongNumberOfArgs:
            errorMsg = F("wrong arg cnt");
            break;
            case PostLightEffects::Error::StackOverrun:
            errorMsg = F("can't call, stack full");
            break;
            case PostLightEffects::Error::StackUnderrun:
            errorMsg = F("stack underrun");
            break;
            case PostLightEffects::Error::StackOutOfRange:
            errorMsg = F("stack out of range");
            break;
            case PostLightEffects::Error::ImmedNotAllowedHere:
            errorMsg = F("immed not allowed here");
            break;
            case PostLightEffects::Error::InternalError:
            errorMsg = F("internal err");
            break;
        }

        Serial.print(F("Interp err: "));
        Serial.println(errorMsg);
    }
    
	void setup()
	{
	    Serial.begin(115200);
        while (!Serial) ; // wait for Arduino Serial Monitor to open
		delay(500);
        randomSeed(millis());
        
	    _pixels.begin(); // This initializes the NeoPixel library.
	    _pixels.setBrightness(255);
	    _pixels.clear();
        _pixels.show();

		Serial.println(F("PostLightEffects v0.1"));

        Serial.println(F("\nInit PostLightEffects"));

        const uint8_t* code = EEPROM_Upload_PostLightEffects;
        uint32_t size = sizeof(EEPROM_Upload_PostLightEffects);

        // First see if the test is already uploaded to avoid an EEPROM write
        Serial.println(F("Checking EEPROM..."));
        
        bool same = true;
        for (int i = 0; i < size; ++i) {
            if (EEPROM[i] != pgm_read_byte(&(code[i]))) {
                same = false;
                break;
            }
        }
        
        if (same) {
            Serial.println(F("EEPROM has correct code, skipping write..."));
        } else {
            Serial.println(F("EEPROM does not have correct code, writing..."));

            // Upload the test
            for (int i = 0; i < size; ++i) {
                EEPROM[i] = pgm_read_byte(&(code[i]));
            }
        }
        
        // Init
        init();
        
        if (error() == PostLightEffects::Error::None) {
            // Pass in 5 args, a uint8_t command, speed, value, saturation and hue.
            // Push them backwards
            addArg(3, clvr::Type::UInt8); // speed (0-7)
            addArg(150, clvr::Type::UInt8); // value
            addArg(200, clvr::Type::UInt8); // saturation
            addArg(128, clvr::Type::UInt8); // hue
            addArg('f', clvr::Type::UInt8); // cmd
            
            interp(PostLightEffects::ExecMode::Start);
            dropArgs(5);
        }
        
        if (error() != PostLightEffects::Error::None) {
            showError(error());
        }
    }

int iii = 0;
	void loop()
	{
        if (error() != PostLightEffects::Error::None || iii >= 300) {
            _pixels.clear();
            _pixels.show();
            return;
        }
        
        ++iii;
        
        addArg('*', clvr::Type::UInt8);
        uint32_t result = interp(PostLightEffects::ExecMode::Start);
        if (error() != PostLightEffects::Error::None) {
            showError(error());
        } else {
            delay(result);
        }
	}
 
   private:
	Adafruit_NeoPixel _pixels;
};

PostLightEffects postLightEffects;

void setup()
{
	postLightEffects.setup();
}

void loop()
{
	postLightEffects.loop();
}
