/*-------------------------------------------------------------------------
    This source file is a part of Lucid
    For the latest info, see https://github.com/cmarrin/Lucid
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "AST.h"

#include <assert.h>
#include <map>

using namespace lucid;

// Operator map. Maps Operators (contained in the AST OpNode) to opcodes
std::map<Operator, std::pair<Op, Op>> opMap {
    { Operator::Equal   , { Op::NOP   , Op::NOP    } }, // OpNode knows if this is an assignment.
    { Operator::AddSto  , { Op::ADD   , Op::ADD    } }, // so all the ...Sto operators (including
    { Operator::SubSto  , { Op::SUB   , Op::SUB    } }, // Equal) just need the binary operation
    { Operator::MulSto  , { Op::IMUL  , Op::UMUL   } }, // they need to perform. For equal there
    { Operator::DivSto  , { Op::IDIV  , Op::UDIV   } }, // is no additional operation, so it is
    { Operator::AndSto  , { Op::AND   , Op::AND    } }, //
    { Operator::OrSto   , { Op::OR    , Op::OR     } }, //
    { Operator::XorSto  , { Op::XOR   , Op::XOR    } }, //
    { Operator::LOr     , { Op::NOP   , Op::NOP    } }, // Logical AND, OR and NOT don't have opcodes.
    { Operator::LAnd    , { Op::NOP   , Op::NOP    } }, // They are short-circuited at compile time.
    { Operator::LNot    , { Op::NOP   , Op::NOP    } },
    { Operator::Or      , { Op::OR    , Op::XOR    } },
    { Operator::Xor     , { Op::XOR   , Op::XOR    } },
    { Operator::And     , { Op::AND   , Op::AND    } },
    { Operator::EQ      , { Op::EQ    , Op::EQ     } },
    { Operator::NE      , { Op::NE    , Op::NE     } },
    { Operator::LT      , { Op::LT    , Op::LO     } },
    { Operator::GT      , { Op::GT    , Op::HI     } },
    { Operator::GE      , { Op::GE    , Op::HS     } },
    { Operator::LE      , { Op::LE    , Op::LS     } },
    { Operator::Plus    , { Op::ADD   , Op::ADD    } },
    { Operator::Minus   , { Op::SUB   , Op::SUB    } },
    { Operator::Mul     , { Op::IMUL  , Op::UMUL   } },
    { Operator::Div     , { Op::IDIV  , Op::UDIV   } },
    { Operator::Inc     , { Op::NOP   , Op::NOP    } },
    { Operator::Dec     , { Op::NOP   , Op::NOP    } },
    { Operator::BNot    , { Op::NOT   , Op::NOT    } },
    { Operator::ArrIdx  , { Op::NOP   , Op::NOP    } },
    { Operator::Dot     , { Op::NOP   , Op::NOP    } },
    { Operator::AddrOf  , { Op::NOP   , Op::NOP    } },
    { Operator::Deref   , { Op::NOP   , Op::NOP    } },
    { Operator::UMinus  , { Op::NEG   , Op::NEG    } },
};

// This version is for opcodes where the lower 2 bits hold type info and the following
// bytes hold values or addresses
static void emitCode(std::vector<uint8_t>& code, Op opcode, Type type, Index index, int32_t value)
{
    code.push_back(uint8_t(opcode) | typeToSizeBits(type));
    
    uint8_t extra = uint8_t(index);
    uint8_t addedBytes = 0;
    
    if (value >= -16 && value <= 15) {
        extra |= uint8_t(value << 3);
    } else if (value >= -128 && value <= 127) {
        extra |= 0x4;
        addedBytes = 1;
    } else if (value >= -32768 && value <= 32767) {
        extra |= 0x0c;
        addedBytes = 2;
    } else if (value >= -8388608 && value <= 8388607) {
        extra |= 0x14;
        addedBytes = 3;
    } else {
        extra |= 0x1c;
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
    Op op = isLHS ? Op::PUSHREF : Op::PUSH;
    
    // FIXME: We assume we're indexing from U, is this always true? What about using Y as the 'this' ptr?
    emitCode(code, op, _symbol->type(), Index::U, _symbol->addr());
}

// If short, bytesInOperand is 0
static Op constantOp(uint8_t bytesInOperand, uint8_t bytesToPush)
{
    switch((bytesInOperand << 4) | bytesToPush) {
        case 0x01: return Op::PUSHKS1;
        case 0x02: return Op::PUSHKS2;
        case 0x04: return Op::PUSHKS4;
        case 0x11: return Op::PUSHK11;
        case 0x12: return Op::PUSHK12;
        case 0x14: return Op::PUSHK14;
        case 0x22: return Op::PUSHK22;
        case 0x24: return Op::PUSHK24;
        case 0x34: return Op::PUSHK34;
        case 0x44: return Op::PUSHK44;
        default: return Op::NOP;
    }
}

void ConstantNode::addCode(std::vector<uint8_t>& code, bool isLHS) const
{
    assert(!isLHS);
    
    uint8_t bytesInOperand;
    
    if (_i >= -8 && _i <= 7) {
        bytesInOperand = 0;
    } else if (_i >= -128 && _i <= 127) {
        bytesInOperand = 1;
    } else if (_i >= -32768 && _i <= 32767) {
        bytesInOperand = 2;
    } else if (_i >= -8388608 && _i <= 8388607) {
        bytesInOperand = 3;
    } else {
        bytesInOperand = 4;
    }

    Op op = constantOp(bytesInOperand, typeToBytes(_type));
    
    if (bytesInOperand == 0) {
        code.push_back(uint8_t(op) | _i);
    } else {
        code.push_back(uint8_t(op));
        switch(bytesInOperand) {
            case 4: code.push_back(_i >> 24);
            case 3: code.push_back(_i >> 16);
            case 2: code.push_back(_i >> 8);
            case 1: code.push_back(_i);
            default: break;
        }
    }
}

void StringNode::addCode(std::vector<uint8_t>& code, bool isLHS) const
{
    // We will push a null terminator in addition to the size. Make room for it
    code.push_back(uint8_t(Op::PUSHS));
    code.push_back(_string.size() + 1);
    for (const char& c : _string) {
        code.push_back(c);
    }
    code.push_back('\0');
}

void OpNode::addCode(std::vector<uint8_t>& code, bool isLHS) const
{
    // FIXME: If _isAssignment as op is not a NOP it means this is an
    // arithmethic assignment (e.g., +=). We need to handle that.
    Op op = _op;
    if (_isAssignment && op == Op::NOP) {
        op = Op::DEREF;
    }
    code.push_back(uint8_t(op) | typeToSizeBits(_type));
}

void DotNode::addCode(std::vector<uint8_t>& code, bool isLHS) const
{
    // FIXME: Implement
}

void FunctionCallNode::addCode(std::vector<uint8_t>& code, bool isLHS) const
{
    // Add a function call. args will already be pushed
    int32_t addr = _function->addr();
    if (_function->isNative()) {
        if (addr <= 15) {
            code.push_back(uint8_t(Op::NCALLS) | addr);
        } else {
            if (addr <= 255) {
                code.push_back(uint8_t(Op::NCALL));
                code.push_back(addr);
            } else {
                code.push_back(uint8_t(Op::NCALL) | 0x01);
                code.push_back(addr >> 8);
                code.push_back(addr);
            }
        }
    } else {
        // FIXME: How does addr get set for a function call? 2 pass? Fixup?
        // FIXME: Implement CALL
    }
    
    // Pop the args after the call returns
    uint16_t argSize = 0;
    for (auto& it : _args) {
        argSize += typeToBytes(it->type());
    }

    if (argSize) {
        code.push_back((argSize > 256) ? uint8_t(Op::DROP2) : uint8_t(Op::DROP1));
        if (argSize > 256) {
            code.push_back(argSize >> 8);
        }
        code.push_back(argSize);
    }
}

void EnterNode::addCode(std::vector<uint8_t>& code, bool isLHS) const
{
    uint16_t localSize = _function->localSize();
    if (localSize < 15) {
        code.push_back(uint8_t(Op::ENTERS) | localSize);
    } else {
        bool isLong = localSize > 255;
        code.push_back(uint8_t(Op::ENTER) | (isLong ? 0x01 : 0x00));
        if (isLong) {
            code.push_back(uint8_t(localSize >> 8));
        }
        code.push_back(uint8_t(localSize));
    }
}

void TypeCastNode::addCode(std::vector<uint8_t>& code, bool isLHS) const
{
    code.push_back(uint8_t(castToOp(_type)));
}
