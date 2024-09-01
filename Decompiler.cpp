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


for (int i = 0; ; ++i) {
    std::string line;
    int32_t addr = _annotations->getLine(i, line);
    
    lucid::printf("[%d:%d] %s\n", i, addr, line.c_str());
    
    if (addr == -2) {
        break;
    }
}

    // Output everything before the first addr
    if (_annotations) {
        _annotationIndex = 0;
        for ( ; ; ++_annotationIndex) {
            std::string line;
            int32_t addr = _annotations->getLine(_annotationIndex, line);
            if (addr == -2 || addr != -1) {
                break;
            }
            
            _out->append("                |    ");
            _out->append(line);
            _out->append("\n");
        }
    }
    
    try {
        // Make sure we start with 'lucd'
        if (getUInt8() != 'l' || getUInt8() != 'u' || getUInt8() != 'c' || getUInt8() != 'd') {
            _error = Error::InvalidSignature;
            return false;
        }
        
        // Emit main entry point
        int32_t entryPoint = getInt32();
        
        _out->append("\nMain entry point                           : ");
        _out->append(std::to_string(entryPoint));
        _out->append("\n");
        
        // Emit ctor entry point
        entryPoint = getInt32();
        
        _out->append("Top-level srruct constructor entry point   : ");
        _out->append(std::to_string(entryPoint));
        _out->append("\n");
        
        // Emit size
        int32_t size = getInt32();
        
        _out->append("Top-level struct size                      : ");
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
        while (!atEnd() && _error == Error::None) {
            statement();
        }
    }
    catch(...) {
        return false;
    }
    
    return _error == Error::None;
}

void
Decompiler::statement()
{
    uint16_t a = addr() - _codeOffset;
    
    if (_annotations) {
        while (true) {
            std::string line;
            int32_t addr = _annotations->getLine(_annotationIndex, line);
            
            if (addr == -2 || (addr != -1 && addr >= a)) {
                break;
            }
            
            _out->append("                |    ");
            _out->append(line);
            
            _annotationIndex += 1;

            if (_out->back() != '\n') {
                _out->append("\n");
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
        
        case Op::CASTF8    : emitOp("CASTF8");    break;
        case Op::CASTF16   : emitOp("CASTF16");   break;
        case Op::CASTF32   : emitOp("CASTF32");   break;
        case Op::CAST32F   : emitOp("CAST32F");   break;
        case Op::CAST3216  : emitOp("CAST3216");  break;
        case Op::CAST328   : emitOp("CAST328");   break;
        case Op::CAST168   : emitOp("CAST168");   break;
        case Op::CASTU16F  : emitOp("CASTU16F");  break;
        case Op::CASTU1632 : emitOp("CASTU1632"); break;
        case Op::CASTI16F  : emitOp("CASTI16F");  break;
        case Op::CASTI1632 : emitOp("CASTI1632"); break;
        case Op::CASTU8F   : emitOp("CASTU8F");   break;
        case Op::CASTU832  : emitOp("CASTU832");  break;
        case Op::CASTU816  : emitOp("CASTU816");  break;
        case Op::CASTI8F   : emitOp("CASTI8F");   break;
        case Op::CASTI832  : emitOp("CASTI832");  break;
        case Op::CASTI816  : emitOp("CASTI816");  break;
        case Op::PUSHREF   : emitOp("PUSHREF "); emitIndex(); break;
        case Op::INDEX1    : emitOp("INDEX1"); emitNumber(getUInt8()); break;
        case Op::INDEX2    : emitOp("INDEX2"); emitNumber(getUInt8()); break;
        case Op::OFFSET1   : emitOp("OFFSET1"); emitNumber(getUInt8()); break;
        case Op::OFFSET2   : emitOp("OFFSET2"); emitNumber(getUInt16()); break;
        case Op::RET     : emitOp("RET"); break;
        case Op::RETR1   : emitOp("RETR1"); break;
        case Op::RETR2   : emitOp("RETR2"); break;
        case Op::RETR4   : emitOp("RETR4"); break;
        case Op::PUSHR1  : emitOp("PUSHR1"); break;
        case Op::PUSHR2  : emitOp("PUSHR2"); break;
        case Op::PUSHR4  : emitOp("PUSHR4"); break;
        case Op::POPDEREF1: emitOp("POPDEREF1"); break;
        case Op::POPDEREF2: emitOp("POPDEREF2"); break;
        case Op::POPDEREF4: emitOp("POPDEREF4"); break;
        case Op::POP1    : emitOp("POP1"); emitIndex(); break;
        case Op::POP2    : emitOp("POP2"); emitIndex(); break;
        case Op::POP4    : emitOp("POP4"); emitIndex(); break;
        case Op::DEREF1  : emitOp("DEREF1"); break;
        case Op::DEREF2  : emitOp("DEREF2"); break;
        case Op::DEREF4  : emitOp("DEREF4"); break;
        case Op::PUSH1   : emitOp("PUSH1"); emitIndex(); break;
        case Op::PUSH2   : emitOp("PUSH2"); emitIndex(); break;
        case Op::PUSH4   : emitOp("PUSH4"); emitIndex(); break;
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
        case Op::DROPS   : emitOp("DROPS"); emitNumber(size + 1); break;
        case Op::DROP1   : emitOp("DROP1"); emitNumber(getUInt8()); break;
        case Op::DROP2   : emitOp("DROP2"); emitNumber(getUInt16()); break;
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
        case Op::LNOT    : emitOp("LNOT"); emitSize(size); break;
        case Op::PREINC  : emitOp("PREINC"); emitSize(size); break;
        case Op::PREDEC  : emitOp("PREDEC"); emitSize(size); break;
        case Op::POSTINC : emitOp("POSTINC"); emitSize(size); break;
        case Op::POSTDEC : emitOp("POSTDEC"); emitSize(size); break;
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
        case Op::BRF     : emitOp("BRF"); emitRelAddr(size, false); break;
        case Op::BRT     : emitOp("BRT"); emitRelAddr(size, false); break;
        case Op::FBRA    : emitOp("FBRA"); emitRelAddr(size, false); break;
        case Op::RBRA    : emitOp("RBRA"); emitRelAddr(size, false); break;
        case Op::SWITCH  : {
            emitOp("SWITCH");
            _out->append("\n");
            uint16_t operand = getUInt16();
            uint16_t n = operand >> 3;
            bool isLongAddr = (operand & 0x04) != 0;
            OpSize opSize = OpSize(operand & 0x03);
            
            for (uint16_t i = 0; i < n; ++i) {
                _out->append("      ");
                emitConstant(opSizeToBytes(opSize));
                _out->append(":");
                emitRelAddr(isLongAddr ? 1 : 0, false);
                _out->append("\n");
            }
            break;
        }
        case Op::CALL    : emitOp("CALL"); emitNumber(getUInt16()); break;
        case Op::MCALL   : emitOp("MCALL"); emitNumber(getUInt16()); break;
        case Op::ENTER   : emitOp("ENTER"); emitNumber(size ? getUInt16() : getUInt8()); break;
        case Op::ENTERS  : emitOp("ENTERS"); emitNumber(size); break;
        case Op::NCALL   : emitOp("NCALL"); emitNumber(size ? getUInt16() : getUInt8()); break;
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

void Decompiler::emitRelAddr(uint8_t size, bool isSigned)
{
    _out->append(" ");
    if (size) {
        _out->append(std::to_string(isSigned ? getInt16() : getUInt16()));
    } else {
        if (isSigned) {
            _out->append(std::to_string(getInt8()));
        } else {
            _out->append(std::to_string(getUInt8()));
        }
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

// Determine the addr mode. Addr is unsigned. See Defines.h (Address Mode)
// for details
uint32_t
Decompiler::addrMode(Index& index)
{
    uint8_t mode = getUInt8();
    index = Index(mode & 0x03);
    uint32_t v;
    
    if ((mode & 0x04) == 0) {
        // Short
        v = mode >> 3;
    } else if ((mode & 0x08) == 0) {
        // Upper 4 bits of mode prepended to next byte
        v = (uint32_t(mode & 0xf0) << 4) | getUInt8();
    } else if (((mode & 0x10) == 0)) {
        v = getUInt16();
    } else {
        v = getUInt32();
    }
    
    return v;
}

void Decompiler::emitIndex()
{
    _out->append(" ");
    Index index;
    uint32_t value = addrMode(index);

    _out->append(std::to_string(value));
    _out->append(",");
    switch (index) {
        case Index::C: _out->append("C"); break;
        case Index::M: _out->append("M"); break;
        case Index::A: _out->append("A"); break;
        case Index::L: _out->append("L"); break;
    }
}

void
Decompiler::emitConstant(uint8_t bytes, bool isSigned)
{
    _out->append(" #");
    int32_t value = getUInt8();
    if (value > 127 && isSigned) {
        // sign extend
        value |= 0xffffff80;
    }
    if (bytes > 1) {
        value = (value << 8) | getUInt8();
    }
    if (bytes > 2) {
        value = (value << 8) | getUInt8();
    }
    if (bytes > 3) {
        value = (value << 8) | getUInt8();
    }
    
    if (isSigned) {
        _out->append(std::to_string(value));
    } else {
        _out->append(std::to_string(uint32_t(value)));
    }
    
    if (bytes == 4) {
        _out->append(" (");
        _out->append(std::to_string(intToFloat(value)));
        _out->append(")");
    }
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
