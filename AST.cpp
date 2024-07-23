/*-------------------------------------------------------------------------
    This source file is a part of Lucid
    For the latest info, see https://github.com/cmarrin/Lucid
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "AST.h"

#include "Compiler.h"

#include <assert.h>
#include <map>

using namespace lucid;

void
StatementsNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c) const
{
    for (auto& it : _statements) {
        it->emitCode(code, isLHS, c);
    }
}

void
VarNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c) const
{
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));

    // If isLHS is true, code generated will be a Ref
    Op op = isLHS ? Op::PUSHREF : Op::PUSH;
    
    // FIXME: We assume we're indexing from U, is this always true? What about using Y as the 'this' ptr?
    code.push_back(uint8_t(op) | typeToSizeBits(_symbol->type()));
    
    int32_t value = _symbol->addr();
    uint8_t extra = uint8_t(Index::U);
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

void
ConstantNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c) const
{
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));

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

void
StringNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c) const
{
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));

    // We will push a null terminator in addition to the size. Make room for it
    code.push_back(uint8_t(Op::PUSHS));
    code.push_back(_string.size() + 1);
    for (const char& c : _string) {
        code.push_back(c);
    }
    code.push_back('\0');
}

void
OpNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c) const
{
    if (_left) {
        _left->emitCode(code, _isAssignment, c);
    }
    
    if (_right) {
        _right->emitCode(code, isLHS, c);
    }
    
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));

    // FIXME: If _isAssignment as op is not a NOP it means this is an
    // arithmethic assignment (e.g., +=). We need to handle that.
    Op op = _op;
    if (_isAssignment && op == Op::NOP) {
        op = Op::DEREF;
    }
    code.push_back(uint8_t(op) | typeToSizeBits(_type));
}

void
DotNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c) const
{
    _operand->emitCode(code, isLHS, c);
    _property->emitCode(code, isLHS, c);
    
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));

    // FIXME: Implement
}

void
ModuleNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c) const
{
}

void
FunctionCallNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c) const
{
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));

    // _args has the list of arguments in order from left to right. But stack
    // pushes down, so args will appear in reverse order on the stack
    // (right most arg will be at lowest addr). So return children in reverse
    // order so the first arg is at the lowest address when pushed
    for (auto i = _args.size() - 1; _args.size() > i; --i) {
        _args[i]->emitCode(code, isLHS, c);
    }
    
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

void
EnterNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c) const
{
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));

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

void
TypeCastNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c) const
{
    _arg->emitCode(code, isLHS, c);
    
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));

    code.push_back(uint8_t(castToOp(_type)));
}
