/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include <Lucid.h>
#include <EEPROM.h>
//#include "TestExpr.h"
//#include "TestIf.h"
//#include "TestFor.h"
//#include "TestWhileLoop.h"
//#include "TestFunction.h"
//#include "TestPtrStruct.h"
#include "TestCore.h"

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

class MyInterpreter : public lucid::Interpreter<StackSize>
{
};

class Test
{
public:
	Test() { }
	~Test() { }
 
    void showError(MyInterpreter::Error error)
    {
        String errorMsg;
        
        switch(error) {
            case MyInterpreter::Error::None:
            errorMsg = F("???");
            break;
            case MyInterpreter::Error::UnexpectedOpInIf:
            errorMsg = F("bad op in if");
            break;
            case MyInterpreter::Error::InvalidOp:
            errorMsg = F("inv op");
            break;
            case MyInterpreter::Error::OnlyMemAddressesAllowed:
            errorMsg = F("mem addrs only");
            break;
            case MyInterpreter::Error::AddressOutOfRange:
            errorMsg = F("addr out of rng");
            break;
            case MyInterpreter::Error::InvalidModuleOp:
            errorMsg = F("inv mod op");
            break;
            case MyInterpreter::Error::ExpectedSetFrame:
            errorMsg = F("SetFrame needed");
            break;
            case MyInterpreter::Error::InvalidNativeFunction:
            errorMsg = F("inv native func");
            break;
            case MyInterpreter::Error::NotEnoughArgs:
            errorMsg = F("not enough args");
            break;
            case MyInterpreter::Error::StackOverrun:
            errorMsg = F("can't call, stack full");
            break;
            case MyInterpreter::Error::StackUnderrun:
            errorMsg = F("stack underrun");
            break;
            case MyInterpreter::Error::StackOutOfRange:
            errorMsg = F("stack out of range");
            break;
            case MyInterpreter::Error::WrongNumberOfArgs:
            errorMsg = F("wrong arg cnt");
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
        
        if (interp.error() == MyInterpreter::Error::None) {
            // Pass in 2 args, a uint8 command and a uint16 number.
            // Push them backwards
            interp.addArg(2, lucid::Type::UInt16);
            interp.addArg('f', lucid::Type::UInt8);
            int32_t result = interp.interp();
            if (result == 0) {
                Serial.println(F("...Finished running test"));
            }
        }
        
        if (interp.error() != MyInterpreter::Error::None) {
            showError(interp.error());
        }
    }
	
	void setup()
	{
	    Serial.begin(115200);
		delay(500);
        randomSeed(millis());
        
		Serial.println(F("Test v0.1"));
  
//        RunTest(TestExpr);
//        RunTest(TestIf);
//        RunTest(TestFor);
//        RunTest(TestWhileLoop);
//        RunTest(TestFunction);
//        RunTest(TestPtrStruct);
        RunTest(TestCore);
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
