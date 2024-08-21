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
VarNode::emitCode(std::vector<uint8_t>& code, bool ref, bool pop, Compiler* c)
{
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));

    // If this is a pointer then we push it as a AddrType not the underlying type
    Op op;
    Type t = isPointer() ? AddrType : _symbol->type();

    if (pop) {
        // Generate POP optimization
        switch (typeToOpSize(t)) {
            case OpSize::i8:  op = Op::POP1; break;
            case OpSize::i16: op = Op::POP2; break;
            case OpSize::i32:
            case OpSize::flt: op = Op::POP4; break;
        }
    } else {
        // If ref is true, code generated will be a Ref
        switch (typeToOpSize(t)) {
            case OpSize::i8:  op = ref ? Op::PUSHREF1 : Op::PUSH1; break;
            case OpSize::i16: op = ref ? Op::PUSHREF2 : Op::PUSH2; break;
            case OpSize::i32:
            case OpSize::flt: op = ref ? Op::PUSHREF4 : Op::PUSH4; break;
        }
    }
    
    code.push_back(uint8_t(op));
    
    // if bit 2 is 0 then bits 7:3 are a signed offset from -16 to 15. If bit 2 is 1
    // and bit 3 is 0, bits 7:4 are prepended to a following byte for a 12 bit
    // address (-2048 to 2047). If bit 3 is 1 then if bit 4 is 0 the next 2 bytes
    // is a signed address. If bit 4 is 1 then the next 4 bytes is a signed address.
    Index index;
    int32_t relAddr = _symbol->addr(index);
    uint8_t extra = uint8_t(index);
    uint8_t addedBytes = 0;
    
    if (relAddr >= -16 && relAddr <= 15) {
        extra |= uint8_t(relAddr << 3);
    } else if (relAddr >= -2048 && relAddr <= 2047) {
        extra |= uint8_t((relAddr & 0xf00) >> 4) | 0x04;
        addedBytes = 1;
    } else if (relAddr >= -32768 && relAddr <= 32767) {
        extra |= 0x0c;
        addedBytes = 2;
    } else {
        extra |= 0x1c;
        addedBytes = 4;
    }
    
    code.push_back(extra);
    appendValue(code, relAddr, addedBytes);
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
    
    // Small floating point numbers is a common case. We currently
    // push these as float constants, which takes 5 bytes. If we
    // push them as small integers and cast, that's only 2 bytes.
    bool isSmallFloat = false;
    Type t = _type;
    int32_t i = _i;
    
    if (_type == Type::Float && _f >= -8 && _f <= 7) {
        i = int32_t(_f);
        if (float(i) == _f) {
            // It's an integer
            isSmallFloat = true;
            t = Type::Int8;
        } else {
            i = _i;
        }
    }
    
    uint8_t bytesInOperand;
    uint8_t bytesPushed = typeToBytes(t);
    
    if (t == Type::Float) {
        
        bytesInOperand = 4;
    } else if (i >= -8 && i <= 7) {
        bytesInOperand = 0;
    } else if (i >= -128 && i <= 127) {
        bytesInOperand = 1;
    } else if (i >= -32768 && i <= 32767) {
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
        code.push_back(uint8_t(op) | i);
    } else {
        code.push_back(uint8_t(op));
        appendValue(code, i, bytesInOperand);
    }
    
    if (isSmallFloat) {
        // Cast it
        code.push_back(uint8_t(castOp(Type::Int8, Type::Float)));
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
    assert(!isLHS);
    
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));

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
    
    code.push_back(uint8_t(_op) | typeToSizeBits(opType));
}

void
AssignmentNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));

    // If op is not NOP this is operator assignment. Handle a += b like a = a + b
    //
    // 1) push lhs
    // 2) push rhs
    // 3) op
    // 4) handle like normal assignment
    //
    // Otherwise push rhs
    //
    // Now TOS has the value. do a pushref of the lhs and then popderef
    if (_op != Op::NOP) {
        _left->emitCode(code, false, c);
    }
    
    _right->emitCode(code, false, c);
    
    if (_op != Op::NOP) {
        code.push_back(uint8_t(_op) | typeToSizeBits(type()));
    }

    // If lhs is a VarNode, we can optimize to turn this:
    //
    //      PUSHREFx i,U
    //      POPDEREFx
    
    // into:
    //
    //      POPx i,U
    if (_left->astNodeType() == ASTNodeType::Var) {
        std::reinterpret_pointer_cast<VarNode>(_left)->emitPopCode(code, c);
        return;
    } else {
        _left->emitCode(code, true, c);
    }
    
    // If this is a pointer assignment, we need to use the pointer
    // type for the POPDEREF
    Type t = _left->isPointer() ? AddrType : type();
    Op op = Op::NOP;
    
    switch (typeToOpSize(t)) {
        case OpSize::i8:  op = Op::POPDEREF1; break;
        case OpSize::i16: op = Op::POPDEREF2; break;
        case OpSize::i32:
        case OpSize::flt: op = Op::POPDEREF4; break;
    }
    code.push_back(uint8_t(op));
}

DotNode::DotNode(const ASTPtr& operand, const SymbolPtr& property, int32_t annotationIndex)
    : ASTNode(annotationIndex)
    , _operand(operand)
    , _property(property)
{
    _type = _property->type();
}

void
DotNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));

    _operand->emitCode(code, true, c);

    Index index;
    uint16_t offset = _property->addr(index);
    if (offset) {
        code.push_back((offset > 255) ? uint8_t(Op::OFFSET2) : uint8_t(Op::OFFSET1));
        appendValue(code, offset, (offset > 255) ? 2 : 1);
    }
    
    if (!isLHS) {
        uint8_t bytes = typeToBytes(type());
        code.push_back(uint8_t((bytes == 1) ? Op::DEREF1 : ((bytes == 2) ? Op::DEREF2 : Op::DEREF4)));
    }
}

void
ModuleNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
}

static void emitDrop(std::vector<uint8_t>& code, uint16_t argSize)
{
    if (argSize) {
        if (argSize <= 16) {
            code.push_back(uint8_t(Op::DROPS) | (argSize - 1));
        } else {
            code.push_back((argSize > 256) ? uint8_t(Op::DROP2) : uint8_t(Op::DROP1));
            appendValue(code, argSize, (argSize > 255) ? 2 : 1);
        }
    }
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
        // Add moduleId to addr
        addr |= _moduleId << BitsPerFunctionId;
        code.push_back(uint8_t(Op::NCALL) | ((addr <= 255) ? 0x00 : 0x01));
        appendValue(code, addr, (addr <= 255) ? 1 : 2);
    } else {
        code.push_back(uint8_t(Op::CALL));
        appendValue(code, addr, 2);
    }
    
    // Pop the args after the call returns. Args pushed is not necessarily the
    // same as the arg list in the function. More args might be passed with
    // VarArgs being used to access them.
    uint16_t argSize = 0;
    for (auto& it : _args) {
        argSize += it->sizeInBytes();
    }
    
    emitDrop(code, argSize);

    // If we want to use the _returnValue, push it
    if (_pushReturn) {
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
        appendValue(code, localSize, isLong ? 2 : 1);
    }
}

void
TypeCastNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));

    _arg->emitCode(code, isLHS, c);
    
    Op op = castOp(_arg->type(), _type);
    if (op != Op::NOP) {
        code.push_back(uint8_t(op));
    }
}

ASTPtr
TypeCastNode::castIfNeeded(ASTPtr& node, Type neededType, int32_t annotationIndex)
{
    // We can only cast scalar nodes
    if (!isScalar(node->type()) || !isScalar(neededType)) {
        return node;
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

static void fixup(std::vector<uint8_t>& code, AddrNativeType fixupIndex, AddrNativeType addr, BranchNode::BranchSize& branchSize)
{
    // If branchSize is Long or Unknown we need to emit a 2 byte branch.
    // But if the branch would fit in 1 byte, set branchSize to Short and
    // tell the compiler we need another pass
    if (branchSize != BranchNode::BranchSize::Short) {
        if (addr <= 255) {
            branchSize = BranchNode::BranchSize::Short;
        }
        code[fixupIndex] = addr >> 8;
        code[fixupIndex + 1] = addr;
    } else {
        code[fixupIndex] = addr;
    }
}

void
BranchNode::fixup(std::vector<uint8_t>& code, AddrNativeType addr)
{
    AddrNativeType rel = addr - _fixupIndex - 2;
    if (_branchSize == BranchSize::Short) {
        rel += 1;
    }
    ::fixup(code, _fixupIndex, rel, _branchSize);
}

void
BranchNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
    switch (_kind) {
        case Kind::IfStart:
            // Emit the opcode with a 0 branch address and mark it
            c->setAnnotation(_annotationIndex, uint32_t(code.size()));
            
            // If this is the first pass, we don't know how long the
            // branch should be so we make enough space for a long
            // branch
            code.push_back(uint8_t(Op::IF) | ((_branchSize == BranchSize::Short) ? 0x00 : 0x01));
            _fixupIndex = AddrNativeType(code.size());
            code.push_back(0);
            if (_branchSize != BranchSize::Short) {
                code.push_back(0);
            }
            break;
        case Kind::Break:
        case Kind::Continue:
        case Kind::ElseStart:
            // Emit the opcode with a 0 branch address and mark it
            c->setAnnotation(_annotationIndex, uint32_t(code.size()));

            // If this is the first pass, we don't know how long the
            // branch should be so we make enough space for a long
            // branch
            code.push_back(uint8_t(Op::FBRA) | ((_branchSize == BranchSize::Short) ? 0x00 : 0x01));
            _fixupIndex = AddrNativeType(code.size());
            code.push_back(0);
            if (_branchSize != BranchSize::Short) {
                code.push_back(0);
            }
            // Fallthrough to fixup the IfStartNode
        case Kind::LoopNext:
        case Kind::IfEnd:
            // Fixup branch
            if (_fixupNode != nullptr) {
                std::static_pointer_cast<BranchNode>(_fixupNode)->fixup(code, AddrNativeType(code.size()));
            }
            break;
        case Kind::LoopStart:
            // Save this for LoopEnd
            _fixupIndex = AddrNativeType(code.size());
            break;
        case Kind::LoopEnd: {
            // branch back to fixupNode
            AddrNativeType addr = std::static_pointer_cast<BranchNode>(_fixupNode)->fixupIndex();
            int16_t relAddr = int16_t(addr - AddrNativeType(code.size())) - 2;
            assert(relAddr < 0);
            
            // RBRA has a positive addr which is subtracted from pc
            relAddr = -relAddr;
            code.push_back(uint8_t(Op::RBRA) | ((relAddr <= 255) ? 0x00 : 0x01));
            appendValue(code, relAddr, (relAddr <= 255) ? 1 : 2);
            break;
        }
        default:
            break;
    }
}

void
CaseClause::fixup(std::vector<uint8_t>& code, AddrNativeType addr)
{
    ::fixup(code, _fixupIndex, addr, _branchSize);
}

void
SwitchNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));
    
    // First emit expression
    _expr->emitCode(code, false, c);
    
    Type type = _expr->type();
    
    // See Defines.h for the definition of the SWITCH opcode.
    uint16_t n = uint16_t(_clauses.size());
    if (_haveDefault) {
        n -= 1;
    }
    uint16_t operand = n << 3;
    
    bool longAddr = _branchSize != BranchNode::BranchSize::Short;
    OpSize opSize = typeToOpSize(type);
    
    if (longAddr) {
        operand |= 0x04;
    }
    
    operand |= uint16_t(opSize);

    // emit the opcode and operand
    code.push_back(uint8_t(Op::SWITCH));
    appendValue(code, operand, 2);
    
    // Jump addresses in list are relative to this point
    AddrNativeType jumpSourceAddr = AddrNativeType(code.size());
    
    // Now we need to sort the clauses, so we can binary search at runtime.
    std::sort(_clauses.begin(), _clauses.end(),
                [](const CaseClause &a, const CaseClause &b)
                {
                    // default clause is always lowest
                    if (a.isDefault()) {
                        return true;
                    }
                    if (b.isDefault()) {
                        return false;
                    }
                    return a.value() < b.value();
                });

    // Default clause should be first if there is one
    // Now emit the list
    for (auto& it : _clauses) {
        if (!it.isDefault()) {
            appendValue(code, it.value(), opSizeToBytes(opSize));
            it.setFixupIndex(AddrNativeType(code.size()));
            appendValue(code, 0, longAddr ? 2 : 1);
        }
    }
    
    // Now emit the statements. As we do so, fixup the addr in the list.
    // At the end of each statement, add a BranchNode so the statement
    // can jump to the end. We need to fixup after
    //
    // FIXME: we can skip the branch on the last case statement
    //
    // If we have a default clause it will be first and it gets emitted right after the list
    // otherwise we need to put a BRA first in place of the default clause
    AddrNativeType missingDefaultFixupAddr = 0;
    
    if (!_haveDefault) {
        code.push_back(uint8_t(Op::FBRA) | ((_defaultBranchSize != BranchNode::BranchSize::Short) ? 0x01 : 0x00));

        // We can reuse the fixupIndex beacuse we're done with it
        missingDefaultFixupAddr = AddrNativeType(code.size());
        appendValue(code, 0, (_defaultBranchSize == BranchNode::BranchSize::Short) ? 1 : 2);
    }
    
    // If all branches in the list are short, we can make the list entry short
    bool allShortBranches = true;
    
    for (auto it = _clauses.begin(); it != _clauses.end(); ++it) {
        if (!it->isDefault()) {
            // No need to fixup default. It's always first
            AddrNativeType addr = AddrNativeType(code.size()) - jumpSourceAddr;
            if (addr >= 256) {
                allShortBranches = true;
            }
            it->fixup(code, addr);
        }
        
        it->stmt()->emitCode(code, false, c);

        // The last clause will always have a branch of 0 so we can skip it
        if (it == _clauses.end() - 1) {
            continue;
        }
        code.push_back(uint8_t(Op::FBRA) | ((it->branchSize() != BranchNode::BranchSize::Short) ? 0x01 : 0x00));

        // We can reuse the fixupIndex beacuse we're done with it
        it->setFixupIndex(AddrNativeType(code.size()));
        appendValue(code, 0, ((it->branchSize() == BranchNode::BranchSize::Short) ? 1 : 2));
    }
    
    // Set branchSize if needed
    if (_branchSize == BranchNode::BranchSize::Unknown) {
        _branchSize = allShortBranches ? BranchNode::BranchSize::Short : BranchNode::BranchSize::Long;
    }
    
    // Finally, fixup the branches at the end of the case statements
    if (!_haveDefault) {
        uint8_t adjustment = (_defaultBranchSize == BranchNode::BranchSize::Short) ? 1 : 2;
        ::fixup(code, missingDefaultFixupAddr, AddrNativeType(code.size()) - missingDefaultFixupAddr - adjustment, _defaultBranchSize);
    }
    
    for (auto it = _clauses.begin(); it != _clauses.end(); ++it) {
        if (it == _clauses.end() - 1) {
            continue;
        }
        uint8_t adjustment = (it->branchSize() == BranchNode::BranchSize::Short) ? 1 : 2;
        it->fixup(code, AddrNativeType(code.size()) - it->fixupIndex() - adjustment);
    }
}

void
IndexNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));
    
    _lhs->emitCode(code, true, c);
    
    // Optimization. If rhs is a constant 0, we can skip the index. Just emit the lhs.
    if (_rhs->astNodeType() == ASTNodeType::Constant) {
        if (std::reinterpret_pointer_cast<ConstantNode>(_rhs)->integerValue() == 0) {
            return;
        }
    }

    // index can be 8 or 16 bit. We know its a valid type because the caller checked it
    _rhs->emitCode(code, false, c);

    code.push_back(uint8_t((_rhs->type() == Type::Int8 || _rhs->type() == Type::UInt8) ? Op::INDEX1 : Op::INDEX2));
    
    // If the underlying type is struct, get that size
    uint8_t size;
    if (isStruct(type())) {
        size = c->typeToStruct(type())->size();
    } else {
        size = typeToBytes(type());
    }
    code.push_back(size);
    
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
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));
    
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
    emitDrop(code, _bytesToDrop);
}

void
RefNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));
    
    // The ref operand can only be used on a right-hand operand.
    // We want to push a ref to it, thus passing 'true' here
    _operand->emitCode(code, true, c);
}

void
DerefNode::emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c)
{
    c->setAnnotation(_annotationIndex, uint32_t(code.size()));
    
    // We're guaranteed that _operand is a ref, so we want to push it
    // as though it's not a LHS. This will push the ref itself and not 
    // a reference to the reference.
    _operand->emitCode(code, false, c);
    
    // If this is LHS then we are done. The ref is on TOS, ready to be assigned to.
    // Otherwise we need to get the refed value.
    if (!isLHS) {
        uint8_t bytes = typeToBytes(type());
        code.push_back(uint8_t((bytes == 1) ? Op::DEREF1 : ((bytes == 2) ? Op::DEREF2 : Op::DEREF4)));
    }
}

