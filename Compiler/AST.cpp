/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Lucid
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include <map>

#include "AST.h"

using namespace lucid;

std::map<Type, uint8_t> typeToSizeBits {
    { Type::Int8,   0x00 },
    { Type::UInt8,  0x00 },
    { Type::Int16,  0x01 },
    { Type::UInt16, 0x01 },
    { Type::Int32,  0x02 },
    { Type::UInt32, 0x02 },
    { Type::Float,  0x03 },
};

// Operator map. Maps Operators (contained in the AST OpNode) to opcodes
std::map<Operator, Op> opMap {
    { Operator::Equal   , Op::NOP   }, // OpNode knows if this is an assignment.
    { Operator::AddSto  , Op::ADDA  }, // so all the ...Sto operators (including
    { Operator::SubSto  , Op::SUBA  }, // Equal) just need the binary operation
    { Operator::MulSto  , Op::MULA  }, // they need to perform. For equal there
    { Operator::DivSto  , Op::DIVA  }, // is no additional operation, so it is
    { Operator::AndSto  , Op::ANDA  }, //
    { Operator::OrSto   , Op::ORA   }, //
    { Operator::XorSto  , Op::XORA  }, //
    { Operator::LOr     , Op::NOP   }, // Logical AND, OR and NOT don't have opcodes.
    { Operator::LAnd    , Op::NOP   }, // They are short-circuited at compile time.
    { Operator::LNot    , Op::NOP   },
    { Operator::Or      , Op::ORA   },
    { Operator::Xor     , Op::XORA  },
    { Operator::And     , Op::ANDA  },
    { Operator::EQ      , Op::NOP   }, // FIXME: How do the branch opcodes relate to these operators?
    { Operator::NE      , Op::NOP   },
    { Operator::LT      , Op::NOP   },
    { Operator::GT      , Op::NOP   },
    { Operator::GE      , Op::NOP   },
    { Operator::LE      , Op::NOP   },
    { Operator::Plus    , Op::ADDA  },
    { Operator::Minus   , Op::SUBA  },
    { Operator::Mul     , Op::MULA  },
    { Operator::Div     , Op::DIVA  },
    { Operator::Inc     , Op::NOP   },
    { Operator::Dec     , Op::NOP   },
    { Operator::BNot    , Op::NOP   },
    { Operator::ArrIdx  , Op::NOP   },
    { Operator::Dot     , Op::NOP   },
    { Operator::AddrOf  , Op::NOP   },
    { Operator::Deref   , Op::NOP   },
    { Operator::UMinus  , Op::NOP   },








};


//struct OpByTypeEntry
//{
//    uint8_t _size;
//    Op _ldK, _ldX, _ldL, _stX, _stL;
//};
//
//static OpByTypeEntry opByTypeTable[ ] = {
//    /* None    */ {   0, Op::NOP  , Op::NOP  , Op::NOP  , Op::NOP  , Op::NOP   },
//    /* Float   */ {   0, Op::LDAK4, Op::LDAX4, Op::LDAL4, Op::STAX4, Op::STAL4 },
//    /* Fixed   */ {   0, Op::LDAK2, Op::LDAX2, Op::LDAL2, Op::STAX2, Op::STAL2 },
//    /* Int8    */ {   0, Op::LDAK1, Op::LDAX1, Op::LDAL1, Op::STAX1, Op::STAL1 },
//    /* UInt8   */ {   0, Op::LDAK1, Op::LDAX1, Op::LDAL1, Op::STAX1, Op::STAL1 },
//    /* Int16   */ {   0, Op::LDAK2, Op::LDAX2, Op::LDAL2, Op::STAX2, Op::STAL2 },
//    /* UInt16  */ {   0, Op::LDAK2, Op::LDAX2, Op::LDAL2, Op::STAX2, Op::STAL2 },
//    /* Int32   */ {   0, Op::LDAK4, Op::LDAX4, Op::LDAL4, Op::STAX4, Op::STAL4 },
//    /* UInt32  */ {   0, Op::LDAK4, Op::LDAX4, Op::LDAL4, Op::STAX4, Op::STAL4 },
//    /* Int     */ {   0, Op::NOP  , Op::NOP  , Op::NOP  , Op::NOP  , Op::NOP   },
//    /* UInt    */ {   0, Op::NOP  , Op::NOP  , Op::NOP  , Op::NOP  , Op::NOP   },
//    /* String  */ {   0, Op::LDAKP, Op::LDAXP, Op::LDALP, Op::STAXP, Op::STALP },
//    /* Struct  */ {   0, Op::LDAKP, Op::LDAXP, Op::LDALP, Op::STAXP, Op::STALP },
//    /* Ptr     */ {   0, Op::LDAKP, Op::LDAXP, Op::LDALP, Op::STAXP, Op::STALP },
//};

Op opTable[ ][14] = {
    { Op::NOP, Op::NOP, Op::NOP, Op::NOP, Op::NOP, Op::NOP, Op::NOP, Op::NOP, Op::NOP, Op::NOP, Op::NOP, Op::NOP, Op::NOP, Op::NOP },
};

static uint8_t indexByte(int32_t off, bool isL, uint8_t& extraBytes)
{
    // 6  bits, 0 extra bytes, bit 1 = 0
    // 12 bits, 1 extra byte,  bit 3:1 = 001
    // 16 bits, 2 extra bytes, bit 3:1 = 011
    // 24 bits, 3 extra bytes, bit 3:1 = 101
    // 32 bits, 3 extra bytes, bit 3:1 = 111
    
    uint8_t v = isL ? 0x00 : 0x01;
    
    if (off >= -32 && off <= 31) {
        extraBytes = 0;
        return (off << 2) | v;
    }
    if (off >= -2048 && off <= 2047) {
        extraBytes = 1;
        return ((off >> 4) & 0xf0) | 0x02 | v;
    }
    if (off >= -(1 << 15) && off <= (1 << 15) - 1) {
        extraBytes = 2;
        return 0x06 | v;
    }
    if (off >= -(1 << 23) && off <= (1 << 23) - 1) {
        extraBytes = 3;
        return 0x0a | v;
    }
    
    extraBytes = 4;
    return 0x0e | v;
}

static uint8_t constantByte(int32_t v, uint8_t& extraBytes)
{
    // 7  bits, 0 extra bytes, bit 0 = 0
    // 12 bits, 1 extra byte,  bit 2:0 = 001
    // 16 bits, 2 extra bytes, bit 2:0 = 011
    // 24 bits, 3 extra bytes, bit 2:0 = 101
    // 32 bits, 4 extra bytes, bit 2:0 = 111

    if (v >= -64 && v <= 63) {
        extraBytes = 0;
        return v << 1;
    }
    if (v >= -4096 && v <= 4095) {
        extraBytes = 1;
        return ((v >> 3) & 0xf8) | 0x01 | v;
    }
    if (v >= -(1 << 15) && v <= (1 << 15) - 1) {
        extraBytes = 2;
        return 0x03 | v;
    }
    if (v >= -(1 << 23) && v <= (1 << 23) - 1) {
        extraBytes = 3;
        return 0x05 | v;
    }

    extraBytes = 4;
    return 0x07 | v;
}

static void emitExtraBytes(std::vector<uint8_t>& code, int32_t off, uint8_t extraBytes)
{
    switch(extraBytes) {
        case 0: return;
        case 4: code.push_back(uint8_t(off >> 24));
        case 3: code.push_back(uint8_t(off >> 16));
        case 2: code.push_back(uint8_t(off >> 8));
        case 1: code.push_back(uint8_t(off)); return;
        default: assert(false);
    }
}

static void emitCode(std::vector<uint8_t>& code, Op opcode, Type type, Index index, int32_t value)
{
    code.push_back(uint8_t(opcode) | typeToSizeBits[type]);
    
    uint8_t extra = uint8_t(index);
    uint8_t addedBytes = 0;
    
    if (value >= -16 && value <= 15) {
        extra |= uint8_t(value << 3);
    } else if (value >= -2048 && value <= 2047) {
        extra |= uint8_t(value << 5);
        addedBytes = 1;
    } else if (value >= -32768 && value <= 32767) {
        extra |= 0x08;
        addedBytes = 2;
    } else if (value >= -8388608 && value <= 8388607) {
        extra |= 0x10;
        addedBytes = 3;
    } else {
        extra |= 0x18;
        addedBytes = 4;
    }
    
    code.push_back(extra);
    if (addedBytes > 3) {
        code.push_back(uint8_t(value >> 24));
    }
    if (addedBytes > 2) {
        code.push_back(uint8_t(value >> 16));
    }
    if (addedBytes > 1) {
        code.push_back(uint8_t(value >> 8));
    }
    if (addedBytes > 0) {
        code.push_back(uint8_t(value));
    }
}

void VarNode::addCode(std::vector<uint8_t>& code, bool isLHS) const
{
    // If isLHS is true, code generated will be a Ref
    Op op = isLHS ? Op::LEAX : Op::LDA;
    
    // FIXME: We assume we're indexing from U, is this always true? What about using Y as the 'this' ptr?
    // FIXME: Right now _symbol->addr() is a uint16_t. We need positive addrs for args and negative
    // addrs for locals.
    emitCode(code, op, _symbol->type(), Index::U, _symbol->addr());
}

void ConstantNode::addCode(std::vector<uint8_t>& code, bool isLHS) const
{
    assert(!isLHS);
    
    // FIXME: What about Type::Int and Type::Float? These are generic int and float literals
    // and we should auto convert them to the right type?
    emitCode(code, Op::LDA, _type, Index::K, _i);
}

void StringNode::addCode(std::vector<uint8_t>& code, bool isLHS) const
{
    // FIXME: Implement
}

void OpNode::addCode(std::vector<uint8_t>& code, bool isLHS) const
{
    // assignment is handled by the caller
    assert(!isAssignment());
    
    // If this is a binary op we have traversed the left hand side which
    // put the result in A. The caller then stored the value in T1 and
    // traversed the right hand side which left its result in A. It then
    // stored the value in T2. So we have to load T1 into A and perform the
    // op with T2 as the right hand operand.
    //
    // FIXME: if the right hand operand is a var or constant, we can skip
    // storing the right hand operand in T1 and just do A op rhs. Do
    // this optimization.
    
    emitCode(code, Op::LDA, Index::U, T1);
    emitCode(code, _op, type(), Index::U, T2);

    int32_t off = _symbol->addr();
    uint8_t extraBytes = 0;
    code.push_back(indexByte(off, true, extraBytes));
    emitExtraBytes(code, off, extraBytes);
}

void DotNode::addCode(std::vector<uint8_t>& code, bool isLHS) const
{
    // If isLHS is true, code generated will be a Ref
    Op op = isLHS ? Op::LEAXL : opByTypeTable[uint8_t(type())]._ldL;
    code.push_back(uint8_t(op));

    int32_t off = _symbol->addr();
    uint8_t extraBytes = 0;
    code.push_back(indexByte(off, true, extraBytes));
    emitExtraBytes(code, off, extraBytes);
}

