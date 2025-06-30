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

#include "Compiler.h"
#include "Decompiler.h"
#include "Defines.h"

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

// clvr [-hda] <clvr file>...
//
//      -9      output 6809 assembly. Output file is <root name>.asm
//      -h      output in include file format. Output file is <root name>.h
//      -d      decompile and print result
//      -a      omit annotations in decompiled output
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
    std::cout << "Clover Compiler v0.2\n\n";
    
    int c;
    bool asm6809 = false;
    bool decompile = false;
    bool headerFile = false;
    bool annotate = true;
    
    while ((c = getopt(argc, argv, "9dha")) != -1) {
        switch(c) {
            case '9': asm6809 = true; break;
            case 'd': decompile = true; break;
            case 'h': headerFile = true; break;
            case 'a': annotate = false; break;
            default: break;
        }
    }

    // If asm6809 never emit as .h and don't decompile
    if (asm6809) {
        headerFile = false;
        decompile = false;
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
    
    clvr::Annotations annotations;
    std::vector<std::pair<int32_t, std::string>> functions;
    
    for (const auto& it : inputFiles) {
        std::fstream stream;
        stream.open(it.c_str(), std::fstream::in);
        if (stream.fail()) {
            std::cout << "Can't open '" << it << "'\n";
            return -1;
        }
        
        std::cout << "Compiling '" << it << "'\n";
        
        randomSeed(uint32_t(clock()));

        clvr::Compiler::OutputFormat fmt = asm6809 ? clvr::Compiler::OutputFormat::ASM6809 : clvr::Compiler::OutputFormat::StackVM;
        clvr::Compiler compiler(fmt, &annotations);
        compiler.compile(&stream, { });

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
                return -1;
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

        name = path + ".asm";
        remove(name.c_str());

        if (asm6809) {
            name = path + ".asm";
        } else if (headerFile) {
            name = path + ".h";
        } else {
            name = path + ".clvx";
        }

        std::cout << "\nEmitting executable to '" << name << "'\n";
        std::fstream outStream;

        std::ios_base::openmode mode = std::fstream::out;
        if (!headerFile) {
            mode|= std::fstream::binary;
        }

        outStream.open(name.c_str(), mode);
        if (outStream.fail()) {
            std::cout << "Can't open '" << name << "'\n";
            return -1;
        } else {
            char* buf = reinterpret_cast<char*>(&(compiler.code()[0]));

            if (!headerFile) {
                // Write the buffer
                outStream.write(buf, compiler.code().size());
                if (outStream.fail()) {
                    std::cout << "Save failed\n";
                    return -1;
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
    }

    return 0;
}
