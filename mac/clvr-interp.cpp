//
//  compile.cpp
//  Clover
//
//  Created by Chris Marrin on 3/29/24.
//

#include <iostream>
#include <unistd.h>
#include <filesystem>
#include <fstream>

#include "Defines.h"
#include "Interpreter.h"
#include "NativeColor.h"

// Base pointer of executable code (see Defines.h)
uint8_t* clvr::ROMBase = nullptr;

static constexpr uint32_t StackSize = 2048;

class MyInterpreter : public clvr::Interpreter<StackSize>
{
};

// clvr-interp [-tl] <clvx file>...
//
//      -l      execute resulting binary as a looping test
//
// Multiple input files accepted.

int main(int argc, char * const argv[])
{
    std::cout << "Clover Interpreter v0.1\n\n";
    
    clvr::randomSeed(uint32_t(clock()));

    int c;
    bool looping = false;
    
    while ((c = getopt(argc, argv, "dahtl")) != -1) {
        switch(c) {
            case 'l': looping = true; break;
            default: break;
        }
    }
    
    if (optind >= argc) {
        std::cout << "No input file given\n";
        return 0;
    }
    
    std::vector<std::string> inputFiles;
    
    for (int i = 0; ; ++i) {
        inputFiles.push_back(argv[optind + i]);
        if (optind + i >= argc - 1) {
            break;
        }
    }
    
    int32_t errors = 0;

    for (const auto& it : inputFiles) {
        std::ifstream stream;
        stream.open(it.c_str(), std::fstream::in);
        if (stream.fail()) {
            std::cout << "Can't open '" << it << "'\n";
            return 0;
        }
        
        std::vector<uint8_t> executable;
        while (!stream.eof()) {
            executable.push_back(stream.get());
        }

        // Execute
        clvr::ROMBase = &(executable[0]);
            
        MyInterpreter interp;
        interp.instantiate();
        interp.addModule(callNativeColor);
        
        if (interp.error() == clvr::Memory::Error::None) {
            uint32_t result = 0;
            
            if (looping) {
                std::cout << "Running looping test on '" << it << "'\n";
            
                // Pass in a uint8_t command, value, saturation, hue, speed and range.
                // Push them backwards
                interp.addArg(4, clvr::Type::UInt8); // range (0-7)
                interp.addArg(6, clvr::Type::UInt8); // speed (0-15)
                interp.addArg(120, clvr::Type::UInt8); // value
                interp.addArg(150, clvr::Type::UInt8); // saturation
                interp.addArg(50, clvr::Type::UInt8); // hue
                interp.addArg('r', clvr::Type::UInt8); // cmd
                
                std::cout << "\nInit\n";
                interp.construct();
                interp.dropArgs(6);
        
                if (interp.error() == clvr::Memory::Error::None) {
                    for (int i = 0; i < 100; ++i) {
                        std::cout << "Pass " << i << "\n";
                        result = interp.interp(MyInterpreter::ExecMode::Start);
                        if (interp.error() != clvr::Memory::Error::None) {
                            break;
                        }
                    }
                }
            } else {
                std::cout << "Running single pass test on '" << it << "'\n";

                interp.construct();

                // Pass in 2 args, a uint8_t command and a uint16_t number.
                // Push them backwards
                interp.addArg(2, clvr::Type::UInt16);
                interp.addArg('f', clvr::Type::UInt8);
                result = interp.interp(MyInterpreter::ExecMode::Start);
                interp.dropArgs(14);
                errors += result;
            }
            
            if (interp.error() == clvr::Memory::Error::None) {
                std::cout << "Complete\n\n";
            }
        }
        
        if (interp.error() != clvr::Memory::Error::None) {
            const char* err = "unknown";
            switch(interp.error()) {
                case clvr::Memory::Error::None: err = "no error"; break;
                case clvr::Memory::Error::InternalError: err = "internal error"; break;
                case clvr::Memory::Error::UnexpectedOpInIf: err = "unexpected op in if (internal error)"; break;
                case clvr::Memory::Error::InvalidOp: err = "invalid opcode"; break;
                case clvr::Memory::Error::InvalidNativeFunction: err = "invalid native function"; break;
                case clvr::Memory::Error::OnlyMemAddressesAllowed: err = "only Mem addresses allowed"; break;
                case clvr::Memory::Error::StackOverrun: err = "can't call, stack full"; break;
                case clvr::Memory::Error::StackUnderrun: err = "stack underrun"; break;
                case clvr::Memory::Error::StackOutOfRange: err = "stack access out of range"; break;
                case clvr::Memory::Error::AddressOutOfRange: err = "address out of range"; break;
                case clvr::Memory::Error::InvalidModuleOp: err = "invalid operation in module"; break;
                case clvr::Memory::Error::ExpectedSetFrame: err = "expected SetFrame as first function op"; break;
                case clvr::Memory::Error::NotEnoughArgs: err = "not enough args on stack"; break;
                case clvr::Memory::Error::WrongNumberOfArgs: err = "wrong number of args"; break;
                case clvr::Memory::Error::InvalidSignature: err = "invalid signature"; break;
                case clvr::Memory::Error::InvalidVersion: err = "invalid version"; break;
                case clvr::Memory::Error::WrongAddressSize: err = "wrong address size"; break;
                case clvr::Memory::Error::NoEntryPoint: err = "invalid entry point in executable"; break;
                case clvr::Memory::Error::NotInstantiated: err = "Need to call instantiate, then construct"; break;
                case clvr::Memory::Error::ImmedNotAllowedHere: err = "immediate not allowed here"; break;
            }
            std::cout << "Interpreter failed: " << err;
            
            int16_t errorAddr = interp.errorAddr();
            if (errorAddr >= 0) {
                std::cout << " at addr " << errorAddr;
            }
            
            std::cout << "\n\n";
        }
    }

    std::cout << "\n\n***** Tests Complete: " << errors << " error"<< ((errors != 1) ? "s" : "") << " - " << ((errors == 0) ? "Passed" : "FAILED") << "\n";

    return 1;
}
