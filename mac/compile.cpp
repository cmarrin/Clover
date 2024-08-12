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

#include "Compiler.h"
#include "Decompiler.h"
#include "Defines.h"
#include "Interpreter.h"

// Base pointer of executable code (see Defines.h)
uint8_t* lucid::ROMBase = nullptr;

static constexpr uint32_t MaxExecutableSize = 65536;
static constexpr uint32_t StackSize = 2048;

class MyInterpreter : public lucid::Interpreter<StackSize>
{
};

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
        case lucid::Error::ExpectedIndexable: err = "expected indexable variable"; break;
        case lucid::Error::PointerConstantNotAllowed: err = "pointer constant not allowed"; break;
        case lucid::Error::EmptyArrayRequiresInitializer: err = "empty array requires initializer"; break;
        case lucid::Error::PropertyDoesNotExist: err = "property does not exist"; break;
        case lucid::Error::IteratorMustBeScalar: err = "iterator must be scalar"; break;
        case lucid::Error::PtrAssignmentMustMatch: err = "pointer assignment must match"; break;
        case lucid::Error::PtrTypeNotAllowed: err = "pointer type not allowed"; break;
        case lucid::Error::WrongNumberOfInitializers: err = "wrong number of initializers"; break;
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
//      -a      omit annotations in decompiled output
//      -t      execute resulting binary as a single pass test
//      -l      execute resulting binary as a looping test
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

int main(int argc, char * const argv[])
{
    std::cout << "Lucid Compiler v0.1\n\n";
    
    int c;
    bool singlePass = false;
    bool looping = false;
    bool decompile = false;
    bool headerFile = false;
    bool annotate = true;
    
    while ((c = getopt(argc, argv, "dahtl")) != -1) {
        switch(c) {
            case 'd': decompile = true; break;
            case 't': singlePass = true; break;
            case 'l': looping = true; break;
            case 'h': headerFile = true; break;
            case 'a': annotate = false; break;
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

        lucid::Compiler compiler(&stream, &annotations);

        compiler.compile(executable, MaxExecutableSize, { });
        if (compiler.error() != lucid::Error::None) {
            showError(compiler.error(), compiler.expectedToken(), compiler.expectedString(), compiler.lineno(), compiler.charno());
            std::cout << "          Executable size=" << std::to_string(executable.size()) << "\n";
            return -1;
        }

        std::cout << "Compile succeeded. Executable size=" << std::to_string(executable.size()) << "\n";

        if (decompile) {
            std::string out;
            lucid::Decompiler decompiler(&executable, &out, annotate ? &annotations : nullptr);
            bool success = decompiler.decompile();
            
            std::cout << "\nPrinting code:\n" << out << "\nEnd code\n\n";
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

        // Execute if needed
        if (looping || singlePass) {
            // Setup executable pointer
            lucid::ROMBase = &(executable[0]);
            
            MyInterpreter interp;
            
            if (interp.error() == lucid::InterpreterBase::Error::None) {
                int32_t result = 0;
                
                if (looping) {
                    std::cout << "Running looping test on '" << path << "'\n";
                
                    // Pass in 5 args, a uint8 command, speed, value, saturation and hue.
                    // Push them backwards
                    interp.addArg(3, lucid::Type::UInt8); // speed (0-7)
                    interp.addArg(150, lucid::Type::UInt8); // value
                    interp.addArg(200, lucid::Type::UInt8); // saturation
                    interp.addArg(128, lucid::Type::UInt8); // hue
                    interp.addArg('f', lucid::Type::UInt8); // cmd
                    
                    std::cout << "\nInit\n";
                    result = interp.interp(MyInterpreter::ExecMode::Start);

                    if (result == 0) {
                        interp.dropArgs(5);
                        interp.addArg('*', lucid::Type::UInt8);
                        for (int i = 0; i < 10; ++i) {
                            std::cout << "Pass " << i << "\n";
                            result = interp.interp(MyInterpreter::ExecMode::Start);
                            if (result != 0) {
                                break;
                            }
                        }
                    }
                } else {
                    std::cout << "Running single pass test on '" << path << "'\n";
                    // Pass in 2 args, a uint8 command and a uint16 number.
                    // Push them backwards
                    interp.addArg(2, lucid::Type::UInt16);
                    interp.addArg('f', lucid::Type::UInt8);
                    result = interp.interp(MyInterpreter::ExecMode::Start);
                }
                
                if (result == 0) {
                    std::cout << "Complete\n\n";
                }
            }
            
            if (interp.error() != lucid::InterpreterBase::Error::None) {
                const char* err = "unknown";
                switch(interp.error()) {
                    case lucid::InterpreterBase::Error::None: err = "no error"; break;
                    case lucid::InterpreterBase::Error::InternalError: err = "internal error"; break;
                    case lucid::InterpreterBase::Error::UnexpectedOpInIf: err = "unexpected op in if (internal error)"; break;
                    case lucid::InterpreterBase::Error::InvalidOp: err = "invalid opcode"; break;
                    case lucid::InterpreterBase::Error::InvalidNativeFunction: err = "invalid native function"; break;
                    case lucid::InterpreterBase::Error::OnlyMemAddressesAllowed: err = "only Mem addresses allowed"; break;
                    case lucid::InterpreterBase::Error::StackOverrun: err = "can't call, stack full"; break;
                    case lucid::InterpreterBase::Error::StackUnderrun: err = "stack underrun"; break;
                    case lucid::InterpreterBase::Error::StackOutOfRange: err = "stack access out of range"; break;
                    case lucid::InterpreterBase::Error::AddressOutOfRange: err = "address out of range"; break;
                    case lucid::InterpreterBase::Error::InvalidModuleOp: err = "invalid operation in module"; break;
                    case lucid::InterpreterBase::Error::ExpectedSetFrame: err = "expected SetFrame as first function op"; break;
                    case lucid::InterpreterBase::Error::NotEnoughArgs: err = "not enough args on stack"; break;
                    case lucid::InterpreterBase::Error::WrongNumberOfArgs: err = "wrong number of args"; break;
                    case lucid::InterpreterBase::Error::InvalidSignature: err = "invalid signature"; break;
                    case lucid::InterpreterBase::Error::NoEntryPoint: err = "invalid entry point in executable"; break;
                    case lucid::InterpreterBase::Error::ImmedNotAllowedHere: err = "immediate not allowed here"; break;
                }
                std::cout << "Interpreter failed: " << err;
                
                int16_t errorAddr = interp.errorAddr();
                if (errorAddr >= 0) {
                    std::cout << " at addr " << errorAddr;
                }
                
                std::cout << "\n\n";
            }
        }
    }

    return 1;
}
