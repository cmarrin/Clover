/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include <EEPROM.h>
#include "TestCore.h"
#include "TestExpr.h"
#include "TestIf.h"
#include "TestFunction.h"
#include "TestFor.h"
#include "TestWhile.h"
#include "TestPtrStruct.h"

/*

Test

Each test is included as a .h file generated on the Mac and appears as a
uint_t array with a name of the form 'EEPROM_Upload_<name>'. Each array is
uploaded to EEPROM and then the test is run. All tests are named simply
"test".
*/

#define RunTest(name) runTest(#name, EEPROM_Upload_##name, sizeof(EEPROM_Upload_##name))

static constexpr uint32_t MaxExecutableSize = 65536;
static constexpr uint32_t StackSize = 1024;

class MyInterpreter : public clvr::Interpreter<StackSize>
{
    virtual void setLight(uint8_t i, uint8_t h, uint8_t s, uint8_t v) override
    {
    }
};

class Test
{
public:
	Test() { }
	~Test() { }
 
    void showError(clvr::Memory::Error error)
    {
        String errorMsg("!!!");
        
        switch(error) {
            case clvr::Memory::Error::None:
            errorMsg = F("???");
            break;
            case clvr::Memory::Error::InvalidSignature:
            errorMsg = F("bad signature");
            break;
            case clvr::Memory::Error::NoEntryPoint:
            errorMsg = F("no entry point");
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
            case clvr::Memory::Error::InvalidUserFunction:
            errorMsg = F("inv user func");
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

        Serial.print(F("Interp err: "));
        Serial.println(errorMsg);
    }
    
    void runTest(const char* name, const uint8_t* testCode, uint32_t size)
    {
        Serial.print(F("\nRunning test script '"));
        Serial.print(name);
        Serial.println(F("'..."));

        // First see if the test is already uploaded to avoid an EEPROM write
        Serial.println(F("Checking EEPROM..."));
        
        bool same = true;
        for (int i = 0; i < size; ++i) {
            if (EEPROM[i] != pgm_read_byte(&(testCode[i]))) {
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
                EEPROM[i] = pgm_read_byte(&(testCode[i]));
            }
        }
        
        // Run the test
        MyInterpreter interp;
        
        interp.init();
        
        if (interp.error() == clvr::Memory::Error::None) {
            // Pass in 2 args, a uint8_t command and a uint16_t number.
            // Push them backwards
            interp.addArg(2, clvr::Type::UInt16);
            interp.addArg('f', clvr::Type::UInt8);
            int32_t result = interp.interp(MyInterpreter::ExecMode::Start);
            if (result == 0) {
                Serial.println(F("...Finished running test"));
            }
        }
        
        if (interp.error() != clvr::Memory::Error::None) {
            showError(interp.error());
        }
    }
	
	void setup()
	{
	    Serial.begin(115200);
		delay(500);
        randomSeed(millis());
        
		Serial.println(F("Test v0.1"));
  
        RunTest(TestCore);
        RunTest(TestExpr);
        RunTest(TestIf);
        RunTest(TestFunction);
        RunTest(TestFor);
        RunTest(TestWhile);
        RunTest(TestPtrStruct);
    }

	void loop()
	{
		delay(100);
	}
};

Test test;

void setup()
{
	test.setup();
}

void loop()
{
	test.loop();
}
