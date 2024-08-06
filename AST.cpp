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
StatementsNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
    for (auto& it : _statements) {
        it->emitCode(code, isLHS, c);
    }
}

void
VarNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));

    // If isLHS is true, code generated will be a Ref
    Op op;
    switch (typeToOpSize(_symbol->type())) {
        case OpSize::i8:  op = isLHS ? Op::PUSHREF1 : Op::PUSH1; break;
        case OpSize::i16: op = isLHS ? Op::PUSHREF2 : Op::PUSH2; break;
        case OpSize::i32:
        case OpSize::flt: op = isLHS ? Op::PUSHREF4 : Op::PUSH4; break;
    }
    
    code.push_back(uint8_t(op));
    
    Index index;
    int32_t value = _symbol->addr(index);
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
    } else {
        extra |= 0x1c;
        addedBytes = 4;
    }
    
    code.push_back(extra);
    if (addedBytes > 2) {
        code.push_back(uint8_t(value >> 24));
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
        case 0x44: return Op::PUSHK44;
        default: return Op::NOP;
    }
}

void
ConstantNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));

    assert(!isLHS);
    
    uint8_t bytesInOperand;
    uint8_t bytesPushed = typeToBytes(_type);
    
    if (_type == Type::Float) {
        bytesInOperand = 4;
    } else if (_i >= -8 && _i <= 7) {
        bytesInOperand = 0;
    } else if (_i >= -128 && _i <= 127) {
        bytesInOperand = 1;
    } else if (_i >= -32768 && _i <= 32767) {
        bytesInOperand = 2;
    } else {
        bytesInOperand = 4;
    }

    Op op = constantOp(bytesInOperand, bytesPushed);
    
    if (op == Op::NOP) {
        // This is a case where bytesInOperand is > bytesPushed.
        // This happens when the value is unsigned and is
        // outside the signed range but inside the unsigned
        // range. We just need to try again with a smaller
        // bytesInOperand.
        bytesInOperand = bytesPushed;
        op = constantOp(bytesInOperand, bytesPushed);
    }
    
    if (bytesInOperand == 0) {
        code.push_back(uint8_t(op) | _i);
    } else {
        code.push_back(uint8_t(op));
        switch(bytesInOperand) {
            case 4: code.push_back(_i >> 24);
                    code.push_back(_i >> 16);
            case 2: code.push_back(_i >> 8);
            case 1: code.push_back(_i);
            default: break;
        }
    }
}

void
StringNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
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
OpNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
    if (_op == Op::PUSHREF1 || _op == Op::PUSHREF2 || _op == Op::PUSHREF4) {
        // This is an addressof operator. There must only be an _right
        // operand and it must be a Var.
        if (_left == nullptr && _right->astNodeType() == ASTNodeType::Var) {
            _right->emitCode(code, true, c);
            return;
        }
    }
    
    // _type is the result type not the type used for operation. We need
    // to get that from the left or right operand
    Type opType = Type::UInt8;
    bool isLogical = _op == Op::LAND || _op == Op::LOR || _op == Op::LNOT;
    
    if (_left) {
        if (isLogical && _left->type() != Type::UInt8) {
            // Cast to uint8 (boolean)
            _left = TypeCastNode::castIfNeeded(_left, Type::UInt8, _annotationIndex);
        }
        if (!isLogical) {
            opType = _left->type();
        }
        _left->emitCode(code, _isRef, c);
    }
    
    if (_right) {
        if (isLogical && _right->type() != Type::UInt8) {
            // Cast to uint8 (boolean)
            _right = TypeCastNode::castIfNeeded(_right, Type::UInt8, _annotationIndex);
        }
        if (!isLogical && _left == nullptr) {
            opType = _right->type();
        }
        
        // If this is a unary operation (like INC) then _isAssignment is used
        _right->emitCode(code, (_left == nullptr) ? _isRef : isLHS, c);
    }
    
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));

    code.push_back(uint8_t(_op) | typeToSizeBits(opType));
}

void
AssignmentNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
    _left->emitCode(code, true, c);
    _right->emitCode(code, false, c);
    
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));

    Op op = Op::NOP;
    switch (typeToOpSize(type())) {
        case OpSize::i8:  op = Op::POPDEREF1; break;
        case OpSize::i16: op = Op::POPDEREF2; break;
        case OpSize::i32:
        case OpSize::flt: op = Op::POPDEREF4; break;
    }
    code.push_back(uint8_t(op));
}

void
DotNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
    _operand->emitCode(code, isLHS, c);
    _property->emitCode(code, isLHS, c);
    
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));

    // FIXME: Implement
}

void
ModuleNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
}

void
FunctionCallNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
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
        code.push_back(uint8_t(Op::CALL));
        code.push_back(addr >> 8);
        code.push_back(addr);
    }
    
    // Pop the args after the call returns. Args pushed is not necessarily the
    // same as the arg list in the function. More args might be passed with
    // VarArgs being used to access them.
    uint16_t argSize = 0;
    for (auto& it : _args) {
        argSize += it->sizeInBytes();
    }

    if (argSize) {
        code.push_back((argSize > 256) ? uint8_t(Op::DROP2) : uint8_t(Op::DROP1));
        if (argSize > 256) {
            code.push_back(argSize >> 8);
        }
        code.push_back(argSize);
    }
    
    // If we want to use the _returnValue, push it
    if (_function->pushReturn()) {
        switch (typeToOpSize(_function->returnType())) {
            case OpSize::i8:  code.push_back(uint8_t(Op::PUSHR1)); break;
            case OpSize::i16: code.push_back(uint8_t(Op::PUSHR2)); break;
            case OpSize::i32: 
            case OpSize::flt: code.push_back(uint8_t(Op::PUSHR4)); break;
        }
    }
}

void
EnterNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
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
TypeCastNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
    _arg->emitCode(code, isLHS, c);
    
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));

    code.push_back(uint8_t(castOp(_arg->type(), _type)));
}

ASTPtr
TypeCastNode::castIfNeeded(ASTPtr& node, Type neededType, int32_t annotationIndex)
{
    // FIXME: Hack to see if this is a pointer, we don't type case pointers
    if (node->astNodeType() == ASTNodeType::Op) {
        OpNode* opNode = reinterpret_cast<OpNode*>(node.get());
        if (opNode->op() == Op::PUSHREF1) {
            return node;
        }
    }
    
    // If types don't match, add a cast operator
    assert(neededType != Type::None && node->type() != Type::None);
    if (node->type() != neededType) {
        // If node is a constant just change its type
        if (node->astNodeType() == ASTNodeType::Constant) {
            ConstantNode* constNode = reinterpret_cast<ConstantNode*>(node.get());
            if (neededType == Type::Float) {
                // Value must be an unsigned int
                constNode->toFloat();
            } else if (constNode->type() == Type::Float) {
                // Convert value to int
                constNode->toUInt();
            }
            constNode->setType(neededType);

        } else {
            node = std::make_shared<TypeCastNode>(neededType, node, annotationIndex);
        }
    }
    
    return node;
}

void
BranchNode::fixup(std::vector<uint8_t>& code, AddrNativeType addr)
{
    // FIXME: For now assume long addresses
    int16_t rel = addr - _fixupIndex - 2;
    code[_fixupIndex] = rel >> 8;
    code[_fixupIndex + 1] = rel;
}

void
BranchNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
    // FIXME: For now all branches are long (16 bit, -32768 to 32767). Eventually we
    // want to do 2 passes. _size will start out as None and The first pass will simply
    // set the appropriate size according to how far the jump needs to be. The second
    // pass will emit BRA and IF opcodes of the appropriate size.
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));
    
    switch (_kind) {
        case Kind::IfStart:
            // Emit the opcode with a 0 branch address and mark it
            // FIXME: for now assume all long addresses
            code.push_back(uint8_t(Op::IF) | 0x01);
            _fixupIndex = AddrNativeType(code.size());
            code.push_back(0);
            code.push_back(0);
            break;
        case Kind::ElseStart:
            // Emit the opcode with a 0 branch address and mark it
            // FIXME: for now assume all long addresses
            code.push_back(uint8_t(Op::BRA) | 0x01);
            _fixupIndex = AddrNativeType(code.size());
            code.push_back(0);
            code.push_back(0);
            
        case Kind::IfEnd:
            // Fixup branch
            assert(_fixupNode != nullptr);
            std::static_pointer_cast<BranchNode>(_fixupNode)->fixup(code, AddrNativeType(code.size()));
            break;
        case Kind::LoopStart:
            // Save this for LoopEnd
            _fixupIndex = AddrNativeType(code.size());
            break;
        case Kind::LoopEnd: {
            // branch back to fixupNode
            AddrNativeType addr = std::static_pointer_cast<BranchNode>(_fixupNode)->fixupIndex();
            int16_t relAddr = int16_t(addr - AddrNativeType(code.size())) - 2;
            
            if (relAddr < 250) {
                code.push_back(uint8_t(Op::BRA));
                code.push_back(uint8_t(relAddr));
            } else {
                code.push_back(uint8_t(Op::BRA) | 0x01);
                code.push_back(uint8_t(relAddr >> 8));
                code.push_back(uint8_t(relAddr));
            }
            break;
        }
        default:
            break;
    }
}

void
IndexNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
    _lhs->emitCode(code, true, c);
    
    // ths must be uint16
    _rhs = TypeCastNode::castIfNeeded(_rhs, Type::UInt16, _annotationIndex);
    _rhs->emitCode(code, false, c);

    c->setAnnotation(_annotationIndex, uint32_t(code.size()));
    
    code.push_back(uint8_t(Op::INDEX));
    code.push_back(typeToBytes(_lhs->type()));
    
    // if isLHS is true then we're done, we have a ref on TOS. If not we need to DEREF
    if (!isLHS) {
        Op op;
        switch (typeToOpSize(type())) {
            case OpSize::i8:  op = Op::DEREF1; break;
            case OpSize::i16: op = Op::DEREF2; break;
            case OpSize::i32:
            case OpSize::flt: op = Op::DEREF4; break;
        }
        
        code.push_back(uint8_t(op));
    }
}

void
ReturnNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
    if (_arg == nullptr) {
        code.push_back(uint8_t(Op::RET));
    } else {
        _arg->emitCode(code, false, c);
        uint8_t size = typeToBytes(_arg->type());
        Op op = (size == 1) ? Op::RETR1 : ((size == 2) ? Op::RETR2 : Op::RETR4);
        code.push_back(uint8_t(op));
    }
}

void
DropNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
    if (_bytesToDrop) {
        code.push_back((_bytesToDrop > 256) ? uint8_t(Op::DROP2) : uint8_t(Op::DROP1));
        if (_bytesToDrop > 256) {
            code.push_back(_bytesToDrop >> 8);
        }
        code.push_back(_bytesToDrop);
    }
}
