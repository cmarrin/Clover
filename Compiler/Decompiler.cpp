/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "Decompiler.h"

#include "Compiler.h"

using namespace lucid;

//bool Decompiler::printFirstPass(const Compiler& compiler)
//{
//    _compiler = &compiler;
//    _out->append("\n");
//
//    // FIXME: Output constants
//
//    // Output top level structs
//    for (auto &it : _compiler->structs()) {
//        firstPassStruct(it);
//    }
//
//    // Output entry point struct instance
//    return true;
//}
//
//const char* Decompiler::typeToString(Type type) const
//{
//    switch(type) {
//        case Type::Int8:    return "int8";
//        case Type::UInt8:   return "uint8";
//        case Type::Int16:   return "int16";
//        case Type::UInt16:  return "uint16";
//        case Type::Int32:   return "int32";
//        case Type::UInt32:  return "uint32";
//        default: {
//            if (uint8_t(type) < StructTypeStart) {
//                return "**UNK**";
//            }
//            const StructPtr struc = _compiler->structFromType(type);
//            return struc ? struc->name().c_str() : "**ILL**";
//        }
//    }
//}
//
//void Decompiler::firstPassStruct(const StructPtr& struc)
//{
//    doIndent();
//    incIndent();
//    
//    _out->append("struct ");
//    _out->append(struc->name());
//    _out->append(" {\n");
//    
//    // Print child structs
//    for (auto& it : struc->structs()) {
//        firstPassStruct(it);
//    }
//    
//    // Print member vars
//    for (auto& it : struc->locals()) {
//        doIndent();
//        _out->append(typeToString(it->type()));
//        _out->append(it->isPointer() ? "* " : " ");
//        _out->append(it->name() + " [");
//        _out->append(std::to_string(it->size()) + " byte");
//        if (it->size() != 1) {
//            _out->append("s");
//        }
//        _out->append(" at ");
//        _out->append(std::to_string(it->addr()));
//        _out->append("]\n");
//    }
//    
//    _out->append("\n");
//
//    // Print functions
//    for (auto& it : struc->functions()) {
//        doIndent();
//        _out->append("function ");
//        if (it.returnType() != Type::None) {
//            _out->append(typeToString(it.returnType()));
//            _out->append(" ");
//        }
//        _out->append(it.name());
//        _out->append("( )");
//        _out->append("\n");
//        doIndent();
//        incIndent();
//        _out->append("{\n");
//        
//        printASTNode(it.astNode());
//        decIndent();
//        doIndent();
//        _out->append("}\n");
//    }
//
//    decIndent();
//    doIndent();
//    _out->append("}\n\n");
//}
//
//void Decompiler::printASTNode(const ASTPtr& ast)
//{
//    if (!ast) {
//        return;
//    }
//    
//    if (ast->isTerminal()) {
//        _out->append(ast->toString());
//        return;
//    }
//
//    if (ast->isList()) {
//        // Just walk all the children. There can be no null child, seeing a null stops the iteration
//        for (uint32_t i = 0; ; ++i) {
//            ASTPtr child = ast->child(i);
//            if (!child) {
//                break;
//            }
//            doIndent();
//            printASTNode(child);
//            _out->append("\n");
//        }
//        return;
//    }
//    
//    // Do an inorder traversal of the ast node. If isTerminal is true then
//    // visit this node. Otherwise there can be one or two children. If
//    // one then it can be on the left or right (for prefix or postfix unary
//    // operations).
//    printASTNode(ast->child(0));
//    _out->append(ast->toString());
//    printASTNode(ast->child(1));
//}

bool Decompiler::decompile()
{
    // Output everything before the first addr
    _annotationIndex = 0;
    for ( ; _annotationIndex < _annotations.size(); ++_annotationIndex) {
        if (_annotations[_annotationIndex].first != -1) {
            break;
        }
        
        _out->append("//    ");
        _out->append(_annotations[_annotationIndex].second);
        _out->append("\n");
    }
    
    try {
        // Make sure we start with 'lucd'
        if (getUInt8() != 'l' || getUInt8() != 'u' || getUInt8() != 'c' || getUInt8() != 'd') {
            _error = Error::InvalidSignature;
            return false;
        }
        
        // Emit code
        while (!atEnd()) {
            statement();
        }
    }
    catch(...) {
        return false;
    }
    
    return true;
}

void
Decompiler::statement()
{
    uint16_t a = addr() - _codeOffset;
    if (!_annotations.empty() && (_annotations[_annotationIndex].first == -1 || _annotations[_annotationIndex].first < a)) {
        for ( ; _annotationIndex < _annotations.size(); ) {
            _out->append("//    ");
            _out->append(_annotations[_annotationIndex++].second);
            if (_annotations[_annotationIndex].first != -1) {
                break;
            }
        }
    }
    

    uint8_t opInt = getUInt8();
    uint8_t size = 0;
    Op opcode = Op::NOP;

    if (opInt < OneBitOperandStart) {
        opcode = Op(opInt);
    } else if (opInt < TwoBitOperandStart) {
        size = opInt & 0x01;
        opcode = Op(opInt & 0xfe);
    } else if (opInt < FoutBitOperandStart) {
        size = opInt & 0x03;
        opcode = Op(opInt & 0xfc);
    } else {
        size = opInt & 0x0f;
        opcode = Op(opInt & 0xf0);
    }
    
    switch (opcode) {
        case Op::NOP     : emitOp("NOP"); break;
        case Op::PUSHREF : emitIndex("PUSHREF", getUInt8()); break;
        case Op::RET     : emitOp("RET"); break;
        case Op::DEREF   : emitSize("DEREF", size); break;
        case Op::PUSH    : emitSizeIndex("PUSH", size, getUInt8()); break;
        case Op::DUP     : emitSize("DUP", size); break;
        case Op::DROP    : emitSize("DROP", size); break;
        case Op::SWAP    : emitSize("SWAP", size); break;
        case Op::ADD     : emitSize("ADD", size); break;
        case Op::SUB     : emitSize("SUB", size); break;
        case Op::IMUL    : emitSize("IMUL", size); break;
        case Op::UMUL    : emitSize("UMUL", size); break;
        case Op::IDIV    : emitSize("IDIV", size); break;
        case Op::UDIV    : emitSize("UDIV", size); break;
        case Op::AND     : emitSize("AND", size); break;
        case Op::OR      : emitSize("OR", size); break;
        case Op::XOR     : emitSize("XOR", size); break;
        case Op::NOT     : emitSize("NOT", size); break;
        case Op::NEG     : emitSize("NEG", size); break;
        case Op::PREINC  : emitSizeIndex("PREINC", size, getUInt8()); break;
        case Op::PREDEC  : emitSizeIndex("PREDEC", size, getUInt8()); break;
        case Op::POSTINC : emitSizeIndex("POSTINC", size, getUInt8()); break;
        case Op::POSTDEC : emitSizeIndex("POSTDEC", size, getUInt8()); break;
        case Op::LE      : emitSize("LE", size); break;
        case Op::LS      : emitSize("LS", size); break;
        case Op::LT      : emitSize("LT", size); break;
        case Op::LO      : emitSize("LO", size); break;
        case Op::GE      : emitSize("GE", size); break;
        case Op::HS      : emitSize("HS", size); break;
        case Op::GT      : emitSize("GT", size); break;
        case Op::HI      : emitSize("HI", size); break;
        case Op::EQ      : emitSize("EQ", size); break;
        case Op::NE      : emitSize("NE", size); break;
        case Op::IF      : emitRelAddr("IF", size); break;
        case Op::BRA     : emitRelAddr("BRA", size); break;
        case Op::CALL    : emitRelAddr("CALL", size); break;
        case Op::ENTER   : emitNumber("ENTER", size ? getUInt16() : getUInt8()); break;
        case Op::ENTERS  : emitNumber("ENTERS", size); break;
        case Op::NCALL   : emitNumber("NCALL", size ? getUInt16() : getUInt8()); break;
        case Op::NCALLS  : emitNumber("NCALLS", size); break;
        case Op::PUSHS   : {
            emitOp("PUSHS");
            _out->append(" \"");
            uint8_t size = getUInt8();
            for (uint8_t i = 0; i < size; ++i) {
                char c = getUInt8();
                if (c == '\n') {
                    _out->append("\\n");
                } else {
                    _out->append(&c, 1);
                }
            }
            _out->append("\"");
            break;
        }
    }
    
    _out->append("\n");
}

void Decompiler::emitOp(const char* opString)
{
    doIndent();
    outputAddr();
    _out->append(opString);
}

void Decompiler::emitRelAddr(const char* opString, uint8_t size)
{
    emitOp(opString);
    _out->append(" ");
    _out->append(std::to_string(size ? getInt16() : getInt8()));
}

void Decompiler::emitNumber(const char* opString, int32_t number)
{
    emitOp(opString);
    _out->append(" ");
    _out->append(std::to_string(number));
}

void Decompiler::emitSizeValue(uint8_t size)
{
    _out->append("<");
    switch (size) {
        case 0: _out->append("1"); break;
        case 1: _out->append("2"); break;
        case 2: _out->append("4"); break;
        case 3: _out->append("F"); break;
        default: _out->append("?"); break;
    }
    
    _out->append(">");
}

void Decompiler::emitIndexValue(uint8_t index)
{
    _out->append(" ");
    uint8_t mode = index & 0x03;
    int32_t value = int32_t(index) >> 2;
    if (value & 0x01) {
        // Long form
        uint8_t size = (value >> 1) & 0x03;
        if (size == 0) {
            value = (value << 8) | getUInt8();
        } else {
            value = int32_t(getInt16());
            if (size == 2) {
                value = (value << 8) | getUInt8();
            } else if (size == 3) {
                value = (value << 16) | getUInt16();
            }
        }
    } else {
        value >>= 1;
    }
    
    if (mode == 0) {
        // Immediate
        _out->append("#");
        _out->append(std::to_string(value));
    } else {
        _out->append(std::to_string(value));
        _out->append(",");
        _out->append((mode == 1) ? "X" : ((mode == 2) ? "Y" : "U"));
    }
}

