//
//  compile.cpp
//  LucidVM
//
//  Created by Chris Marrin on 3/29/24.
//

#include <iostream>
#include <unistd.h>
#include <filesystem>
#include <fstream>

#include "CompileEngine.h"
#include "Decompiler.h"
#include "Defines.h"

static constexpr uint32_t MaxExecutableSize = 65536;

static void showError(lucid::Error error, lucid::Token token, const std::string& str, uint32_t lineno, uint32_t charno)
{
    const char* err = "unknown";
    switch(error) {
        case lucid::Error::None: err = "internal error"; break;
        case lucid::Error::UnrecognizedLanguage: err = "unrecognized language"; break;
        case lucid::Error::ExpectedToken: err = "expected token"; break;
        case lucid::Error::ExpectedKeyword: err = "expected keyword"; break;
        case lucid::Error::ExpectedType: err = "expected type"; break;
        case lucid::Error::ExpectedValue: err = "expected value"; break;
        case lucid::Error::ExpectedString: err = "expected string"; break;
        case lucid::Error::ExpectedRef: err = "expected ref"; break;
        case lucid::Error::ExpectedOpcode: err = "expected opcode"; break;
        case lucid::Error::ExpectedEnd: err = "expected 'end'"; break;
        case lucid::Error::ExpectedIdentifier: err = "expected identifier"; break;
        case lucid::Error::ExpectedExpr: err = "expected expression"; break;
        case lucid::Error::ExpectedLHSExpr: err = "expected left-hand side expression"; break;
        case lucid::Error::ExpectedArgList: err = "expected arg list"; break;
        case lucid::Error::ExpectedFormalParams: err = "expected formal params"; break;
        case lucid::Error::ExpectedFunction: err = "expected function name"; break;
        case lucid::Error::ExpectedStructType: err = "expected Struct type"; break;
        case lucid::Error::ExpectedVar: err = "expected var"; break;
        case lucid::Error::AssignmentNotAllowedHere: err = "assignment not allowed here"; break;
        case lucid::Error::InvalidStructId: err = "invalid Struct identifier"; break;
        case lucid::Error::InvalidParamCount: err = "invalid param count"; break;
        case lucid::Error::UndefinedIdentifier: err = "undefined identifier"; break;
        case lucid::Error::ParamOutOfRange: err = "param must be 0..15"; break;
        case lucid::Error::JumpTooBig: err = "tried to jump too far"; break;
        case lucid::Error::IfTooBig: err = "too many instructions in if"; break;
        case lucid::Error::ElseTooBig: err = "too many instructions in else"; break;
        case lucid::Error::StringTooLong: err = "string too long"; break;
        case lucid::Error::TooManyConstants: err = "too many constants"; break;
        case lucid::Error::TooManyVars: err = "too many vars"; break;
        case lucid::Error::DefOutOfRange: err = "def out of range"; break;
        case lucid::Error::ExpectedDef: err = "expected def"; break;
        case lucid::Error::NoMoreTemps: err = "no more temp variables available"; break;
        case lucid::Error::TempNotAllocated: err = "temp not allocated"; break;
        case lucid::Error::InternalError: err = "internal error"; break;
        case lucid::Error::StackTooBig: err = "stack too big"; break;
        case lucid::Error::MismatchedType: err = "mismatched type"; break;
        case lucid::Error::WrongType: err = "wrong type"; break;
        case lucid::Error::WrongNumberOfArgs: err = "wrong number of args"; break;
        case lucid::Error::OnlyAllowedInLoop: err = "break/continue only allowed in loop"; break;
        case lucid::Error::DuplicateIdentifier: err = "duplicate identifier"; break;
        case lucid::Error::ExecutableTooBig: err = "executable too big"; break;
        case lucid::Error::InitializerNotAllowed: err = "initializer not allowed for this type"; break;
        case lucid::Error::ConstMustBeSimpleType: err = "const must be simple type"; break;
    }
    
    if (token == lucid::Token::EndOfFile) {
        err = "unexpected tokens after EOF";
    }
    
    std::cout << "Compile failed: " << err;
    if (!str.empty()) {
        std::cout << " ('" << str << "')";
    }
    std::cout << " on line " << lineno << ":" << charno << "\n";
}

// compile [-xdsh] <input file>...
//
//      -h      output in include file format. Output file is <root name>.h
//      -d      decompile and print result
//      -x      simulate resulting binary
//
// Multiple input files accepted. Output file(s) are placed in the same dir as input
// files with extension .arlx or .h. If segmented (-s), filename has 2 digit suffix
// added before the .arlx.

// Include file format
//
// This file can be included in an Arduino sketch to upload to EEPROM. The file
// contains 'static const uint8_t PROGMEM EEPROM_Upload = {', followed by each
// byte of the binary data as a hex value. It also has a
// 'static constexpr uint16_t EEPROM_Upload_Size = ' with the number of bytes.

int main(int argc, char * const argv[]) {
    std::cout << "Lucid Compiler v0.1\n\n";
    
    int c;
    bool execute = false;
    bool decompile = false;
    bool headerFile = false;
    
    while ((c = getopt(argc, argv, "dxh")) != -1) {
        switch(c) {
            case 'd': decompile = true; break;
            case 'x': execute = true; break;
            case 'h': headerFile = true; break;
            default: break;
        }
    }
    
    // If headerFile is true, segmented is ignored.
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
    
    lucid::AnnotationList annotations;
    std::vector<std::pair<int32_t, std::string>> functions;

    for (const auto& it : inputFiles) {
        std::fstream stream;
        stream.open(it.c_str(), std::fstream::in);
        if (stream.fail()) {
            std::cout << "Can't open '" << it << "'\n";
            return 0;
        }
        
        std::cout << "Compiling '" << it << "'\n";
        
        std::vector<uint8_t> executable;
        
        lucid::randomSeed(uint32_t(clock()));

        lucid::CompileEngine compiler(&stream, &annotations);

        compiler.compile(executable, MaxExecutableSize, { });
        if (compiler.error() != lucid::Error::None) {
            showError(compiler.error(), compiler.expectedToken(), compiler.expectedString(), compiler.lineno(), compiler.charno());
            std::cout << "          Executable size=" << std::to_string(executable.size()) << "\n";
            return -1;
        }

        std::cout << "Compile succeeded. Executable size=" << std::to_string(executable.size()) << "\n";
        
        // Write executable
        // Use the same name as the input file for the output
        std::string path = it.substr(0, it.find_last_of('.'));
        
        // Delete any old copies
        std::string name = path + ".h";
        remove(name.c_str());

        name = path + ".lcdx";
        remove(name.c_str());

        std::cout << "\nEmitting executable to '" << path << "'\n";
        std::fstream outStream;

        if (headerFile) {
            name = path + ".h";
        } else {
            name = path + ".lcdx";
        }
    
        std::ios_base::openmode mode = std::fstream::out;
        if (!headerFile) {
            mode|= std::fstream::binary;
        }
        
        outStream.open(name.c_str(), mode);
        if (outStream.fail()) {
            std::cout << "Can't open '" << name << "'\n";
            return 0;
        } else {
            char* buf = reinterpret_cast<char*>(&(executable[0]));
            
            if (!headerFile) {
                // Write the buffer
                outStream.write(buf, executable.size());
                if (outStream.fail()) {
                    std::cout << "Save failed\n";
                    return 0;
                } else {
                    outStream.close();
                    std::cout << "    Saved " << name << "\n";
                }
            } else {
                // Write out as a header file
                std::string name = path.substr(path.find_last_of('/') + 1);
                outStream << "static const uint8_t PROGMEM EEPROM_Upload_" << name << "[ ] = {\n";
                
                for (size_t i = 0; i < executable.size(); ++i) {
                    char hexbuf[5];
                    snprintf(hexbuf, 5, "0x%02x", executable[i]);
                    outStream << hexbuf << ", ";
                    if (i % 8 == 7) {
                        outStream << std::endl;
                    }
                }

                outStream << "};\n";
            }
        }
        std::cout << "Executables saved\n";

        // decompile if needed
        if (decompile) {
            std::string out;
            lucid::Decompiler decompiler(&executable, &out, annotations);
            bool success = decompiler.decompile();
            std::cout << "\nDecompiled executable:\n" << out << "\nEnd decompilation\n\n";
            if (!success) {
                const char* err = "unknown";
                switch(decompiler.error()) {
                    case lucid::Decompiler::Error::None: err = "internal error"; break;
                    case lucid::Decompiler::Error::InvalidSignature: err = "invalid signature"; break;
                    case lucid::Decompiler::Error::InvalidOp: err = "invalid op"; break;
                    case lucid::Decompiler::Error::PrematureEOF: err = "premature EOF"; break;
                }
                std::cout << "Decompile failed: " << err << "\n\n";
                return 0;
            }
        }
        
        // Execute if needed
//        if (execute) {
//            Simulator sim;
//            
//            sim.setROM(executable);
//            
//            for (const Test& test : Tests) {
//                std::cout << "Running '" << test._cmd << "' command...\n";
//            
//                bool success = sim.init(test._cmd, &test._buf[0], test._buf.size());
//                if (success && NumLoops > 0) {
//                    for (int i = 0; i < NumLoops; ++i) {
//                        int32_t delay = sim.loop();
//                        if (delay < 0) {
//                            success = false;
//                            break;
//                        }
//                        std::cout << "[" << i << "]: delay = " << delay << "\n";
//                    }
//                
//                    if (success) {
//                        std::cout << "Complete\n\n";
//                    }
//                }
//                
//                if (!success) {
//                    const char* err = "unknown";
//                    switch(sim.error()) {
//                        case clvr::Interpreter::Error::None: err = "internal error"; break;
//                        case clvr::Interpreter::Error::CmdNotFound: err = "command not found"; break;
//                        case clvr::Interpreter::Error::UnexpectedOpInIf: err = "unexpected op in if (internal error)"; break;
//                        case clvr::Interpreter::Error::InvalidOp: err = "invalid opcode"; break;
//                        case clvr::Interpreter::Error::InvalidNativeFunction: err = "invalid native function"; break;
//                        case clvr::Interpreter::Error::OnlyMemAddressesAllowed: err = "only Mem addresses allowed"; break;
//                        case clvr::Interpreter::Error::StackOverrun: err = "can't call, stack full"; break;
//                        case clvr::Interpreter::Error::StackUnderrun: err = "stack underrun"; break;
//                        case clvr::Interpreter::Error::StackOutOfRange: err = "stack access out of range"; break;
//                        case clvr::Interpreter::Error::AddressOutOfRange: err = "address out of range"; break;
//                        case clvr::Interpreter::Error::InvalidModuleOp: err = "invalid operation in module"; break;
//                        case clvr::Interpreter::Error::ExpectedSetFrame: err = "expected SetFrame as first function op"; break;
//                        case clvr::Interpreter::Error::NotEnoughArgs: err = "not enough args on stack"; break;
//                        case clvr::Interpreter::Error::WrongNumberOfArgs: err = "wrong number of args"; break;
//                    }
//                    std::cout << "Interpreter failed: " << err;
//                    
//                    int16_t errorAddr = sim.errorAddr();
//                    if (errorAddr >= 0) {
//                        std::cout << " at addr " << errorAddr;
//                    }
//                    
//                    std::cout << "\n\n";
//                }
//            }
//        }
    }

    return 1;
}
