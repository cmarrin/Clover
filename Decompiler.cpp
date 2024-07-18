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
        
        // Emit entry point
        uint32_t entryPoint = 0;
        entryPoint |= uint32_t(getUInt8()) << 24;
        entryPoint |= uint32_t(getUInt8()) << 16;
        entryPoint |= uint32_t(getUInt8()) << 8;
        entryPoint |= uint32_t(getUInt8());
        
        _out->append("\nEntry Point: ");
        _out->append(std::to_string(entryPoint));
        _out->append("\n\n");
        
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
        case Op::TOF     : emitOp("TOF"); emitSize(size); break;
        case Op::TOU8    : emitOp("TOU8"); emitSize(size); break;
        case Op::TOI8    : emitOp("TOI8"); emitSize(size); break;
        case Op::TOU16   : emitOp("TOU16"); emitSize(size); break;
        case Op::TOI16   : emitOp("TOI16"); emitSize(size); break;
        case Op::TOU32   : emitOp("TOU32"); emitSize(size); break;
        case Op::TOI32   : emitOp("TOI32"); emitSize(size); break;
        case Op::PUSHREF : emitOp("PUSHREF"); emitIndex(getUInt8()); break;
        case Op::RET     : emitOp("RET"); break;
        case Op::DEREF   : emitOp("DEREF"); emitSize(size); break;
        case Op::PUSH    : emitOp("PUSH"); emitSizeIndex(size, getUInt8()); break;
        case Op::PUSHK11 : emitOp("PUSHK11"); emitSizeIndex(size, getUInt8()); break;
        case Op::PUSHK12 : emitOp("PUSHK12"); emitSizeIndex(size, getUInt8()); break;
        case Op::PUSHK14 : emitOp("PUSHK14"); emitSizeIndex(size, getUInt8()); break;
        case Op::PUSHK22 : emitOp("PUSHK22"); emitSizeIndex(size, getUInt8()); break;
        case Op::PUSHK24 : emitOp("PUSHK24"); emitSizeIndex(size, getUInt8()); break;
        case Op::PUSHK34 : emitOp("PUSHK34"); emitSizeIndex(size, getUInt8()); break;
        case Op::PUSHK44 : emitOp("PUSHK44"); emitSizeIndex(size, getUInt8()); break;
        case Op::PUSHKS1 : emitOp("PUSHKS1"); emitSize(size); break;
        case Op::PUSHKS2 : emitOp("PUSHKS2"); emitSize(size); break;
        case Op::PUSHKS4 : emitOp("PUSHKS4"); emitSize(size); break;
        case Op::DUP     : emitOp("DUP"); emitSize(size); break;
        case Op::DROP1   : emitOp("DROP"); emitNumber(getUInt8()); break;
        case Op::DROP2   : emitOp("DROP"); emitNumber(getUInt16()); break;
        case Op::SWAP    : emitOp("SWAP"); emitSize(size); break;
        case Op::ADD     : emitOp("ADD"); emitSize(size); break;
        case Op::SUB     : emitOp("SUB"); emitSize(size); break;
        case Op::IMUL    : emitOp("IMUL"); emitSize(size); break;
        case Op::UMUL    : emitOp("UMUL"); emitSize(size); break;
        case Op::IDIV    : emitOp("IDIV"); emitSize(size); break;
        case Op::UDIV    : emitOp("UDIV"); emitSize(size); break;
        case Op::AND     : emitOp("AND"); emitSize(size); break;
        case Op::OR      : emitOp("OR"); emitSize(size); break;
        case Op::XOR     : emitOp("XOR"); emitSize(size); break;
        case Op::NOT     : emitOp("NOT"); emitSize(size); break;
        case Op::NEG     : emitOp("NEG"); emitSize(size); break;
        case Op::PREINC  : emitOp("PREINC"); emitSizeIndex(size, getUInt8()); break;
        case Op::PREDEC  : emitOp("PREDEC"); emitSizeIndex(size, getUInt8()); break;
        case Op::POSTINC : emitOp("POSTINC"); emitSizeIndex(size, getUInt8()); break;
        case Op::POSTDEC : emitOp("POSTDEC"); emitSizeIndex(size, getUInt8()); break;
        case Op::LE      : emitOp("LE"); emitSize(size); break;
        case Op::LS      : emitOp("LS"); emitSize(size); break;
        case Op::LT      : emitOp("LT"); emitSize(size); break;
        case Op::LO      : emitOp("LO"); emitSize(size); break;
        case Op::GE      : emitOp("GE"); emitSize(size); break;
        case Op::HS      : emitOp("HS"); emitSize(size); break;
        case Op::GT      : emitOp("GT"); emitSize(size); break;
        case Op::HI      : emitOp("HI"); emitSize(size); break;
        case Op::EQ      : emitOp("EQ"); emitSize(size); break;
        case Op::NE      : emitOp("NE"); emitSize(size); break;
        case Op::IF      : emitOp("IF"); emitRelAddr(size); break;
        case Op::BRA     : emitOp("BRA"); emitRelAddr(size); break;
        case Op::CALL    : emitOp("CALL"); emitRelAddr(size); break;
        case Op::ENTER   : emitOp("ENTER"); emitNumber(size ? getUInt16() : getUInt8()); break;
        case Op::ENTERS  : emitOp("ENTERS"); emitNumber(size); break;
        case Op::NCALL   : emitOp("NCALL"); emitNumber(size ? getUInt16() : getUInt8()); break;
        case Op::NCALLS  : emitOp("NCALLS"); emitNumber(size); break;
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

void Decompiler::emitRelAddr(uint8_t size)
{
    _out->append(" ");
    _out->append(std::to_string(size ? getInt16() : getInt8()));
}

void Decompiler::emitNumber(int32_t number)
{
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

