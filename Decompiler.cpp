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
        
        _out->append("                    //    ");
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
        _out->append("\n");
        
        // Emit size
        uint32_t size = 0;
        size |= uint32_t(getUInt8()) << 24;
        size |= uint32_t(getUInt8()) << 16;
        size |= uint32_t(getUInt8()) << 8;
        size |= uint32_t(getUInt8());
        
        _out->append("Top-level struct size: ");
        _out->append(std::to_string(size));
        _out->append("\n\n");
        
        // Emit constants
        uint16_t constSize = getUInt16();
        
        _out->append("Constants:");
        for (uint16_t i = 0; i < constSize; ++i) {
            if (i % 8 == 0) {
                _out->append("\n        ");
            }
            char buf[8];
            snprintf(buf, 7, "0x%02x ", getUInt8());
            _out->append(buf);
        }

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
            _out->append("                    //    ");
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
        case Op::PUSHREF1: emitOp("PUSHREF1"); emitIndex(getUInt8()); break;
        case Op::PUSHREF2: emitOp("PUSHREF2"); emitIndex(getUInt8()); break;
        case Op::PUSHREF4: emitOp("PUSHREF4"); emitIndex(getUInt8()); break;
        case Op::INDEX   : emitOp("INDEX"); emitNumber(getUInt8()); break;
        case Op::RET     : emitOp("RET"); break;
        case Op::PUSHR1  : emitOp("PUSHR1"); break;
        case Op::PUSHR2  : emitOp("PUSHR2"); break;
        case Op::PUSHR4  : emitOp("PUSHR4"); break;
        case Op::POPDEREF1: emitOp("POPDEREF1"); break;
        case Op::POPDEREF2: emitOp("POPDEREF2"); break;
        case Op::POPDEREF4: emitOp("POPDEREF4"); break;
        case Op::POP1    : emitOp("POP1"); break;
        case Op::POP2    : emitOp("POP2"); break;
        case Op::POP4    : emitOp("POP4"); break;
        case Op::DEREF1  : emitOp("DEREF1"); break;
        case Op::DEREF2  : emitOp("DEREF2"); break;
        case Op::DEREF4  : emitOp("DEREF4"); break;
        case Op::PUSH1   : emitOp("PUSH1"); emitIndex(getUInt8()); break;
        case Op::PUSH2   : emitOp("PUSH2"); emitIndex(getUInt8()); break;
        case Op::PUSH4   : emitOp("PUSH4"); emitIndex(getUInt8()); break;
        case Op::PUSHK11 : emitOp("PUSHK11"); emitConstant(1); break;
        case Op::PUSHK12 : emitOp("PUSHK12"); emitConstant(1); break;
        case Op::PUSHK14 : emitOp("PUSHK14"); emitConstant(1); break;
        case Op::PUSHK22 : emitOp("PUSHK22"); emitConstant(2); break;
        case Op::PUSHK24 : emitOp("PUSHK24"); emitConstant(2); break;
        case Op::PUSHK44 : emitOp("PUSHK44"); emitConstant(4); break;
        case Op::PUSHKS1 : emitOp("PUSHKS1"); emitShortConstant(size); break;
        case Op::PUSHKS2 : emitOp("PUSHKS2"); emitShortConstant(size); break;
        case Op::PUSHKS4 : emitOp("PUSHKS4"); emitShortConstant(size); break;
        case Op::DUP1    : emitOp("DUP1"); emitSize(size); break;
        case Op::DUP2    : emitOp("DUP2"); emitSize(size); break;
        case Op::DUP4    : emitOp("DUP4"); emitSize(size); break;
        case Op::DROP1   : emitOp("DROP"); emitNumber(getUInt8()); break;
        case Op::DROP2   : emitOp("DROP"); emitNumber(getUInt16()); break;
        case Op::SWAP1   : emitOp("SWAP1"); emitSize(size); break;
        case Op::SWAP2   : emitOp("SWAP2"); emitSize(size); break;
        case Op::SWAP4   : emitOp("SWAP4"); emitSize(size); break;
        case Op::ADD     : emitOp("ADD"); emitSize(size); break;
        case Op::SUB     : emitOp("SUB"); emitSize(size); break;
        case Op::IMUL    : emitOp("IMUL"); emitSize(size); break;
        case Op::UMUL    : emitOp("UMUL"); emitSize(size); break;
        case Op::IDIV    : emitOp("IDIV"); emitSize(size); break;
        case Op::UDIV    : emitOp("UDIV"); emitSize(size); break;
        case Op::AND     : emitOp("AND"); emitSize(size); break;
        case Op::OR      : emitOp("OR"); emitSize(size); break;
        case Op::XOR     : emitOp("XOR"); emitSize(size); break;
        case Op::LAND    : emitOp("LAND"); emitSize(size); break;
        case Op::LOR     : emitOp("LOR"); emitSize(size); break;
        case Op::NOT     : emitOp("NOT"); emitSize(size); break;
        case Op::NEG     : emitOp("NEG"); emitSize(size); break;
        case Op::LNOT    : emitOp("LNOT"); emitSize(size); break;
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
        case Op::CALL    : emitOp("CALL"); emitNumber(getUInt16()); break;
        case Op::ENTER   : emitOp("ENTER"); emitNumber(size ? getUInt16() : getUInt8()); break;
        case Op::ENTERS  : emitOp("ENTERS"); emitNumber(size); break;
        case Op::NCALL   : emitOp("NCALL"); emitNumber(size ? getUInt16() : getUInt8()); break;
        case Op::NCALLS  : emitOp("NCALLS"); emitNumber(size); break;
        case Op::PUSHS   : {
            emitOp("PUSHS");
            _out->append(" \"");
            uint8_t size = getUInt8() - 1;
            for (uint8_t i = 0; i < size; ++i) {
                char c = getUInt8();
                if (c == '\n') {
                    _out->append("\\n");
                } else {
                    _out->append(&c, 1);
                }
            }
            
            // Skip trailing nul
            getUInt8();
            
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
    if (size) {
        _out->append(std::to_string(getInt16()));
    } else {
        int8_t v = getInt8();
        _out->append(std::to_string(v));
    }
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

        value = getUInt8();
        if (value > 127) {
            // sign extend
            value |= 0xffffff80;
        }
        if (size > 0) {
            value = (value < 8) | getUInt8();
        }
        if (size > 1) {
            value = (value < 8) | getUInt8();
        }
        if (size > 2) {
            value = (value < 8) | getUInt8();
        }
    } else {
        value >>= 1;
        if (value > 15) {
            // Sign extend
            value |= 0xfffffff0;
        }
    }
    
    
    _out->append(std::to_string(value));
    _out->append(",");
    switch (mode) {
        case 0: _out->append("C"); break;
        case 1: _out->append("X"); break;
        case 2: _out->append("Y"); break;
        case 3: _out->append("U"); break;
        default: _out->append("?"); break;
    }
}

void
Decompiler::emitConstant(uint8_t bytes)
{
    _out->append(" #");
    int32_t value = getUInt8();
    if (value > 127) {
        // sign extend
        value |= 0xffffff80;
    }
    if (bytes > 1) {
        value = (value < 8) | getUInt8();
    }
    if (bytes > 2) {
        value = (value < 8) | getUInt8();
    }
    if (bytes > 3) {
        value = (value < 8) | getUInt8();
    }
    _out->append(std::to_string(value));
}
    
void
Decompiler::emitShortConstant(uint8_t v)
{
    _out->append(" #");
    int32_t value = v;
    if (value > 127) {
        // sign extend
        value |= 0xffffff80;
    }
    _out->append(std::to_string(value));
}
