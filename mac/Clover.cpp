//
//  Clover.cpp
//  Clover
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

static constexpr uint32_t NumPasses = 10;
static constexpr uint8_t Command = 'r';

// Base pointer of executable code (see Defines.h)
uint8_t* clvr::ROMBase = nullptr;

static constexpr uint32_t MaxExecutableSize = 65536;
static constexpr uint32_t StackSize = 2048;

class MyInterpreter : public clvr::Interpreter<StackSize>
{
  public:
    virtual void setLight(uint8_t i, uint8_t h, uint8_t s, uint8_t v) override
    {
        fmt::printf("setLight(%d, 0x%02x, 0x%02x, 0x%02x)\n", i, h, s, v);
    }
};

static void showError(clvr::Error error, clvr::Token token, const std::string& str, uint32_t lineno, uint32_t charno)
{
    const char* err = "unknown";
    switch(error) {
        case clvr::Error::None: err = "internal error"; break;
        case clvr::Error::UnrecognizedLanguage: err = "unrecognized language"; break;
        case clvr::Error::ExpectedToken: err = "expected token"; break;
        case clvr::Error::ExpectedKeyword: err = "expected keyword"; break;
        case clvr::Error::ExpectedType: err = "expected type"; break;
        case clvr::Error::ExpectedValue: err = "expected value"; break;
        case clvr::Error::ExpectedReturnValue: err = "return value required"; break;
        case clvr::Error::UnexpectedReturnValue: err = "return value not allowed"; break;
        case clvr::Error::ExpectedString: err = "expected string"; break;
        case clvr::Error::ExpectedRef: err = "expected ref"; break;
        case clvr::Error::ExpectedOpcode: err = "expected opcode"; break;
        case clvr::Error::ExpectedEnd: err = "expected 'end'"; break;
        case clvr::Error::ExpectedIdentifier: err = "expected identifier"; break;
        case clvr::Error::ExpectedExpr: err = "expected expression"; break;
        case clvr::Error::ExpectedEnum: err = "expected enum"; break;
        case clvr::Error::ExpectedLHSExpr: err = "expected left-hand side expression"; break;
        case clvr::Error::ExpectedArgList: err = "expected arg list"; break;
        case clvr::Error::ExpectedFormalParams: err = "expected formal params"; break;
        case clvr::Error::ExpectedFunction: err = "expected function name"; break;
        case clvr::Error::ExpectedStructType: err = "expected Struct type"; break;
        case clvr::Error::ExpectedVar: err = "expected var"; break;
        case clvr::Error::ExpectedConstExpr: err = "expected constant expr"; break;
        case clvr::Error::AssignmentNotAllowedHere: err = "assignment not allowed here"; break;
        case clvr::Error::InvalidParamCount: err = "invalid param count"; break;
        case clvr::Error::UndefinedIdentifier: err = "undefined identifier"; break;
        case clvr::Error::ParamOutOfRange: err = "param must be 0..15"; break;
        case clvr::Error::JumpTooBig: err = "tried to jump too far"; break;
        case clvr::Error::IfTooBig: err = "too many instructions in if"; break;
        case clvr::Error::ElseTooBig: err = "too many instructions in else"; break;
        case clvr::Error::StringTooLong: err = "string too long"; break;
        case clvr::Error::TooManyConstants: err = "too many constants"; break;
        case clvr::Error::TooManyVars: err = "too many vars"; break;
        case clvr::Error::DefOutOfRange: err = "def out of range"; break;
        case clvr::Error::ExpectedDef: err = "expected def"; break;
        case clvr::Error::NoMoreTemps: err = "no more temp variables available"; break;
        case clvr::Error::TempNotAllocated: err = "temp not allocated"; break;
        case clvr::Error::InternalError: err = "internal error"; break;
        case clvr::Error::StackTooBig: err = "stack too big"; break;
        case clvr::Error::MismatchedType: err = "mismatched type"; break;
        case clvr::Error::WrongType: err = "wrong type"; break;
        case clvr::Error::WrongNumberOfArgs: err = "wrong number of args"; break;
        case clvr::Error::OnlyAllowedInLoop: err = "break/continue only allowed in loop"; break;
        case clvr::Error::DuplicateIdentifier: err = "duplicate identifier"; break;
        case clvr::Error::ExecutableTooBig: err = "executable too big"; break;
        case clvr::Error::InitializerNotAllowed: err = "initializer not allowed for this type"; break;
        case clvr::Error::ExpectedIndexable: err = "expected indexable variable"; break;
        case clvr::Error::PointerConstantNotAllowed: err = "pointer constant not allowed"; break;
        case clvr::Error::RequiresInitializer: err = "var requires initializer"; break;
        case clvr::Error::PropertyDoesNotExist: err = "property does not exist"; break;
        case clvr::Error::IteratorMustBeScalar: err = "iterator must be scalar"; break;
        case clvr::Error::PtrAssignmentMustMatch: err = "pointer assignment must match"; break;
        case clvr::Error::PtrTypeNotAllowed: err = "pointer type not allowed"; break;
        case clvr::Error::WrongNumberOfInitializers: err = "wrong number of initializers"; break;
        case clvr::Error::CodeGenFailed: err = "codegen failed"; break;
    }
    
    if (token == clvr::Token::EndOfFile) {
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
    std::cout << "Clover Compiler v0.1\n\n";
    
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
    
    clvr::Annotations annotations;
    std::vector<std::pair<int32_t, std::string>> functions;
    
    int32_t errors = 0;

    for (const auto& it : inputFiles) {
        std::fstream stream;
        stream.open(it.c_str(), std::fstream::in);
        if (stream.fail()) {
            std::cout << "Can't open '" << it << "'\n";
            return 0;
        }
        
        std::cout << "Compiling '" << it << "'\n";
        
        clvr::randomSeed(uint32_t(clock()));

        clvr::Compiler compiler(clvr::Compiler::OutputFormat::StackVM, &stream, &annotations);

        compiler.compile(MaxExecutableSize, { });
        if (compiler.error() != clvr::Error::None) {
            showError(compiler.error(), compiler.expectedToken(), compiler.expectedString(), compiler.lineno(), compiler.charno());
            std::cout << "          Executable size=" << std::to_string(compiler.code().size()) << "\n";
            return -1;
        }

        std::cout << "Compile succeeded. Executable size=" << std::to_string(compiler.code().size()) << "\n";

        if (decompile) {
            std::string out;
            clvr::Decompiler decompiler(&(compiler.code()), &out, annotate ? &annotations : nullptr);
            bool success = decompiler.decompile();
            
            std::cout << "\nPrinting code:\n" << out << "\nEnd code\n\n";
            if (!success) {
                const char* err = "unknown";
                switch(decompiler.error()) {
                    case clvr::Decompiler::Error::None: err = "internal error"; break;
                    case clvr::Decompiler::Error::InvalidSignature: err = "invalid signature"; break;
                    case clvr::Decompiler::Error::WrongAddressSize: err = "wrong address size"; break;
                    case clvr::Decompiler::Error::InvalidOp: err = "invalid op"; break;
                    case clvr::Decompiler::Error::PrematureEOF: err = "premature EOF"; break;
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

        name = path + ".clvx";
        remove(name.c_str());

        std::cout << "\nEmitting executable to '" << name << "'\n";
        std::fstream outStream;

        if (headerFile) {
            name = path + ".h";
        } else {
            name = path + ".clvx";
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
            char* buf = reinterpret_cast<char*>(&(compiler.code()[0]));

            if (!headerFile) {
                // Write the buffer
                outStream.write(buf, compiler.code().size());
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

                for (size_t i = 0; i < compiler.code().size(); ++i) {
                    char hexbuf[5];
                    snprintf(hexbuf, 5, "0x%02x", compiler.code()[i]);
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
            clvr::ROMBase = &(compiler.code()[0]);
            
            MyInterpreter interp;
            interp.instantiate();
            
            if (interp.error() == clvr::InterpreterBase::Error::None) {
                uint32_t result = 0;
                
                if (looping) {
                    std::cout << "Running looping test with command '" << Command << "' on '" << path << "'\n";
                    uint8_t argCount = 0;
                
                    if (Command == 'm') {
                        // Pass in a uint8_t command, 4 x (value, saturation, hue) and speed.
                        // Push them backwards
                        interp.addArg(2, clvr::Type::UInt8); // speed (0-7)
                        interp.addArg(200, clvr::Type::UInt8); // value
                        interp.addArg(224, clvr::Type::UInt8); // saturation
                        interp.addArg(200, clvr::Type::UInt8); // hue
                        interp.addArg(200, clvr::Type::UInt8); // value
                        interp.addArg(224, clvr::Type::UInt8); // saturation
                        interp.addArg(150, clvr::Type::UInt8); // hue
                        interp.addArg(200, clvr::Type::UInt8); // value
                        interp.addArg(224, clvr::Type::UInt8); // saturation
                        interp.addArg(100, clvr::Type::UInt8); // hue
                        interp.addArg(200, clvr::Type::UInt8); // value
                        interp.addArg(224, clvr::Type::UInt8); // saturation
                        interp.addArg(50, clvr::Type::UInt8); // hue
                        interp.addArg('m', clvr::Type::UInt8); // cmd
                        argCount = 14;
                    } else if (Command == 'f' || Command == 'p') {
                        // Pass in a uint8_t command, value, saturation, hue and speed.
                        // Push them backwards
                        interp.addArg(7, clvr::Type::UInt8); // speed (0-7)
                        interp.addArg(100, clvr::Type::UInt8); // value
                        interp.addArg(200, clvr::Type::UInt8); // saturation
                        interp.addArg(25, clvr::Type::UInt8); // hue
                        interp.addArg(Command, clvr::Type::UInt8); // cmd
                        argCount = 5;
                    } else if (Command == 'r') {
                        // Pass in a uint8_t command, value, saturation, hue, speed and range.
                        // Push them backwards
                        interp.addArg(4, clvr::Type::UInt8); // range (0-7)
                        interp.addArg(6, clvr::Type::UInt8); // speed (0-15)
                        interp.addArg(120, clvr::Type::UInt8); // value
                        interp.addArg(150, clvr::Type::UInt8); // saturation
                        interp.addArg(50, clvr::Type::UInt8); // hue
                        interp.addArg('r', clvr::Type::UInt8); // cmd
                        argCount = 6;
                    }
                    
                    std::cout << "\nInit\n";
                    interp.construct();
                    interp.dropArgs(argCount);
            
                    if (interp.error() == MyInterpreter::Error::None) {
                        for (int i = 0; i < NumPasses; ++i) {
                            std::cout << "Pass " << i << "\n";
                            result = interp.interp(MyInterpreter::ExecMode::Start);
                            if (interp.error() != MyInterpreter::Error::None) {
                                break;
                            }
                        }
                    }
                } else {
                    std::cout << "Running single pass test on '" << path << "'\n";

                    interp.construct();

                    // Pass in 2 args, a uint8_t command and a uint16_t number.
                    // Push them backwards
                    interp.addArg(2, clvr::Type::UInt16);
                    interp.addArg('f', clvr::Type::UInt8);
                    result = interp.interp(MyInterpreter::ExecMode::Start);
                    interp.dropArgs(14);
                    errors += result;
                }
                
                if (interp.error() == MyInterpreter::Error::None) {
                    std::cout << "Complete\n\n";
                }
            }
            
            if (interp.error() != clvr::InterpreterBase::Error::None) {
                const char* err = "unknown";
                switch(interp.error()) {
                    case clvr::InterpreterBase::Error::None: err = "no error"; break;
                    case clvr::InterpreterBase::Error::InternalError: err = "internal error"; break;
                    case clvr::InterpreterBase::Error::UnexpectedOpInIf: err = "unexpected op in if (internal error)"; break;
                    case clvr::InterpreterBase::Error::InvalidOp: err = "invalid opcode"; break;
                    case clvr::InterpreterBase::Error::InvalidNativeFunction: err = "invalid native function"; break;
                    case clvr::InterpreterBase::Error::OnlyMemAddressesAllowed: err = "only Mem addresses allowed"; break;
                    case clvr::InterpreterBase::Error::StackOverrun: err = "can't call, stack full"; break;
                    case clvr::InterpreterBase::Error::StackUnderrun: err = "stack underrun"; break;
                    case clvr::InterpreterBase::Error::StackOutOfRange: err = "stack access out of range"; break;
                    case clvr::InterpreterBase::Error::AddressOutOfRange: err = "address out of range"; break;
                    case clvr::InterpreterBase::Error::InvalidModuleOp: err = "invalid operation in module"; break;
                    case clvr::InterpreterBase::Error::ExpectedSetFrame: err = "expected SetFrame as first function op"; break;
                    case clvr::InterpreterBase::Error::NotEnoughArgs: err = "not enough args on stack"; break;
                    case clvr::InterpreterBase::Error::WrongNumberOfArgs: err = "wrong number of args"; break;
                    case clvr::InterpreterBase::Error::InvalidSignature: err = "invalid signature"; break;
                    case clvr::InterpreterBase::Error::InvalidVersion: err = "invalid version"; break;
                    case clvr::InterpreterBase::Error::WrongAddressSize: err = "wrong address size"; break;
                    case clvr::InterpreterBase::Error::NoEntryPoint: err = "invalid entry point in executable"; break;
                    case clvr::InterpreterBase::Error::NotInstantiated: err = "Need to call instantiate, then construct"; break;
                    case clvr::InterpreterBase::Error::ImmedNotAllowedHere: err = "immediate not allowed here"; break;
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

    if (singlePass) {
        std::cout << "\n\n***** Tests Complete: " << errors << " error"<< ((errors != 1) ? "s" : "") << " - " << ((errors == 0) ? "Passed" : "FAILED") << "\n";
    }

    return 1;
}
