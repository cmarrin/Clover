/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "CodeGen6809.h"

using namespace clvr;

void
CodeGen6809::emitPreamble(const Compiler* compiler)
{
    format("* 6809 assembly generated from Clover source\n\n");
    format("include BOSS9.inc\n");
    format("org $200\n");
}

void
CodeGen6809::emitCodeStatements(const ASTPtr& node, bool isLHS)
{
    for (int i = 0; i < node->numChildren(); ++i) {
        emitCode(node->child(i), isLHS);
    }
}

void
CodeGen6809::emitCodeVar(const ASTPtr& node, bool isLHS)
{
    emitCodeVar(node, Type::None, isLHS, false);
}

void
CodeGen6809::emitCodeVar(const ASTPtr& node, Type type, bool isLHS)
{
    emitCodeVar(node, type, isLHS, false);
}

void
CodeGen6809::emitPopCodeVar(const ASTPtr& node)
{
    emitCodeVar(node, Type::None, false, true);
}

static const char* indexToString(Index index)
{
    switch (index) {
        case Index::A:
        case Index::L: return "U";
        case Index::M: return "Y";
        case Index::C: return "?"; // FIXME: How do we handle constants (abs addressing?)
    }
}

void
CodeGen6809::emitAddr(const SymbolPtr& symbol, AddrNativeType offset, bool is32BitLSB)
{
    // Determine the addr mode. Addr is unsigned. See Defines.h (Address Mode)
    // for details
    Index index;
    AddrNativeType relAddr = symbol->addr(index);
    relAddr += (index == Index::L) ? -offset : offset;
    if (is32BitLSB) {
        relAddr += 2;
    }
    
    format("%d,%s\n", relAddr, indexToString(index));
}

void
CodeGen6809::emitCodeVar(const ASTPtr& node, Type type, bool ref, bool pop)
{
    SymbolPtr symbol = std::static_pointer_cast<VarNode>(node)->symbol();
    AddrNativeType offset = std::static_pointer_cast<VarNode>(node)->offset();
    
    // If this is a pointer then we push it as a AddrType not the underlying type
    if (type == Type::None) {
        type = node->isPointer() ? AddrType : symbol->type();
    }
    
    // We either generate:
    //
    //      Pop optimization    : PUL, ST <addr>
    //      Ref                 : LEAX <addr>, PSH X
    //      Push                : LD <addr>, PSH
    //
    // 8 bit uses A, 16 bit uses D, and 32 bit uses D/X
    
    if (pop) {
        // Generate POP optimization
        switch (typeToOpSize(type)) {
            case OpSize::i8:
                format("PULS A\nSTA ");
                emitAddr(symbol, offset);
                break;
            case OpSize::i16:
                format("PULS D\nSTD ");
                emitAddr(symbol, offset);
                break;
            case OpSize::i32:
            case OpSize::flt:
                format("PULS D,X\nSTD ");
                emitAddr(symbol, offset);
                format("STX ");
                emitAddr(symbol, offset, true);
                break;
        }
    } else if (ref) {
        // If ref is true, code generated will be a Ref
        format("LEAX ");
        emitAddr(symbol, offset);
        format("PSHS X\n");
    } else {
        switch (typeToOpSize(type)) {
            case OpSize::i8:
                format("LDA ");
                emitAddr(symbol, offset);
                format("PSHS A\n");
                break;
            case OpSize::i16:
                format("LDD ");
                emitAddr(symbol, offset);
                format("PSHS D\n");
                break;
            case OpSize::i32:
            case OpSize::flt:
                format("LDD ");
                emitAddr(symbol, offset);
                format("LDX ");
                emitAddr(symbol, offset, true);
                format("PSHS D,X\n");
                break;
        }
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
CodeGen6809::emitCodeConstant(const ASTPtr& node, bool isLHS)
{
    assert(!isLHS);
    
    // Small floating point numbers is a common case. We currently
    // push these as float constants, which takes 5 bytes. If we
    // push them as small integers and cast, that's only 2 bytes.
    bool isSmallFloat = false;
    Type t = node->type();
    int32_t i = std::static_pointer_cast<ConstantNode>(node)->rawInteger();
    float f = intToFloat(i);
    
    if (t == Type::Float && f >= -8 && f <= 7) {
        i = int32_t(f);
        if (float(i) == f) {
            // It's an integer
            isSmallFloat = true;
            t = Type::Int8;
        } else {
            i = std::static_pointer_cast<ConstantNode>(node)->rawInteger();
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
        code().push_back(uint8_t(op) | i);
    } else {
        code().push_back(uint8_t(op));
        appendValue(code(), i, bytesInOperand);
    }
    
    if (isSmallFloat) {
        // Cast it
        code().push_back(uint8_t(castOp(Type::Int8, Type::Float)));
    }
}

void
CodeGen6809::emitCodeString(const ASTPtr& node, bool isLHS)
{
    // We will push a null terminator in addition to the size. Make room for it
    const std::string& string = std::static_pointer_cast<StringNode>(node)->string();
    
    code().push_back(uint8_t(Op::PUSHS));
    code().push_back(string.size() + 1);
    for (const char& c : string) {
        code().push_back(c);
    }
    code().push_back('\0');
}

static bool adjustType(Op& op, uint8_t bytes)
{
    switch (op) {
        case Op::AND1:
        case Op::OR1:
        case Op::XOR1:
        case Op::NOT1:
        case Op::SHL1:
        case Op::SHR1:
        case Op::ASR1:
            op = Op(uint8_t(op) + ((bytes == 1) ? 0 : ((bytes == 2) ? 1 : 2)));
            return true;
        default: return false;
    }
}

void
CodeGen6809::emitCodeOp(const ASTPtr& node, bool isLHS)
{
    assert(!isLHS);
    
    // _type is the result type not the type used for operation. We need
    // to get that from the left or right operand
    auto opNode = std::static_pointer_cast<OpNode>(node);
    Type opType = Type::UInt8;
    bool isLogical = opNode->op() == Op::LNOT;
    
    if (opNode->left()) {
        if (!isLogical) {
            opType = opNode->left()->type();
        }
        emitCode(opNode->left(), opNode->isRef());
    }
    
    if (opNode->right()) {
        if (!isLogical && opNode->left() == nullptr) {
            opType = opNode->right()->type();
        }
        
        // If this is a unary operation (like INC) then _isAssignment is used
        emitCode(opNode->right(), (opNode->left() == nullptr) ? opNode->isRef() : isLHS);
    }
    
    // Adjust the op according to the type (for operators that don't have type in the lower 2 bits
    Op op = opNode->op();
    if (adjustType(op, typeToBytes(opType))) {
        code().push_back(uint8_t(op));
    } else {
        code().push_back(uint8_t(op) | typeToSizeBits(opType));
    }
}

void
CodeGen6809::emitCodeAssignment(const ASTPtr& node, bool isLHS)
{
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
    auto assignmentNode = std::static_pointer_cast<AssignmentNode>(node);

    if (assignmentNode->op() != Op::NOP) {
        emitCode(assignmentNode->left(), false);
    }
    
    emitCode(assignmentNode->right(), false);
    
    if (assignmentNode->op() != Op::NOP) {
        code().push_back(uint8_t(assignmentNode->op()) | typeToSizeBits(assignmentNode->type()));
    }

    // If lhs is a VarNode, we can optimize to turn this:
    //
    //      PUSHREFx i,U
    //      POPDEREFx
    
    // into:
    //
    //      POPx i,U
    //
    if (assignmentNode->left()->astNodeType() == ASTNodeType::Var) {
        emitPopCodeVar(assignmentNode->left());
        return;
    } else {
        emitCode(assignmentNode->left(), true);
    }
    
    // If this is a pointer assignment, we need to use the pointer
    // type for the POPDEREF
    Type t = assignmentNode->left()->isPointer() ? AddrType : assignmentNode->type();
    Op op = Op::NOP;
    
    switch (typeToOpSize(t)) {
        case OpSize::i8:  op = Op::POPDEREF1; break;
        case OpSize::i16: op = Op::POPDEREF2; break;
        case OpSize::i32:
        case OpSize::flt: op = Op::POPDEREF4; break;
    }
    code().push_back(uint8_t(op));
}

void
CodeGen6809::emitCodeDot(const ASTPtr& node, bool isLHS)
{
    ASTPtr operand = std::static_pointer_cast<DotNode>(node)->operand();
    SymbolPtr property = std::static_pointer_cast<DotNode>(node)->property();
    
    Index index;
    uint16_t offset = property->addr(index);

    // If _operand is a Var, we can skip the OFFSET and just add the property
    // offset to the VarNode.
    if (operand->astNodeType() == ASTNodeType::Var) {
        std::static_pointer_cast<VarNode>(operand)->setOffset(offset);
        emitCodeVar(operand, property->type(), isLHS);
        return;
    }

    emitCode(operand, true);

    // We can skip the OFFSET if offset value is 0
    if (offset) {
        code().push_back((offset > 255) ? uint8_t(Op::OFFSET2) : uint8_t(Op::OFFSET1));
        appendValue(code(), offset, (offset > 255) ? 2 : 1);
    }
    
    if (!isLHS) {
        uint8_t bytes = typeToBytes(node->type());
        code().push_back(uint8_t((bytes == 1) ? Op::DEREF1 : ((bytes == 2) ? Op::DEREF2 : Op::DEREF4)));
    }
}

void
CodeGen6809::emitCodeModule(const ASTPtr& node, bool isLHS)
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
CodeGen6809::emitCodeFunctionCall(const ASTPtr& node, bool isLHS)
{
    auto fNode = std::static_pointer_cast<FunctionCallNode>(node);
    
    // _args has the list of arguments in order from left to right. But stack
    // pushes down, so args will appear in reverse order on the stack
    // (right most arg will be at lowest addr). So return children in reverse
    // order so the first arg is at the lowest address when pushed
    for (auto i = fNode->args().size() - 1; fNode->args().size() > i; --i) {
        emitCode(fNode->args()[i], isLHS);
    }
    
    // Add a function call. args will already be pushed
    int32_t addr = fNode->function()->addr();
    if (fNode->function()->isNative()) {
        // Add moduleId to addr
        addr |= fNode->moduleId() << BitsPerFunctionId;
        code().push_back(uint8_t(Op::NCALL) | ((addr <= 255) ? 0x00 : 0x01));
        appendValue(code(), addr, (addr <= 255) ? 1 : 2);
    } else if (addr) {
        // A member function pushes the instance and then calls MCALL. A bare function
        // uses CALL and does not push an instance.
        //
        // When MCALL is executed it first pops the instance, pushes the return address,
        // pushes the current value of the Y register and then places the popped
        // instance pointer in Y, then calls the function.
        //
        // When CALL is executed it pushes the return address, then a 4 bytes of 0 as
        // a dummy address to make arg accesses align correctly, then calls the
        // function.
        if (fNode->instance()) {
            emitCode(fNode->instance(), true);
            code().push_back(uint8_t(Op::MCALL));
        } else {
            code().push_back(uint8_t(Op::CALL));
        }
        appendValue(code(), addr, 2);
    }
    
    // Pop the args after the call returns. Args pushed is not necessarily the
    // same as the arg list in the function. More args might be passed with
    // VarArgs being used to access them.
    uint16_t argSize = 0;
    for (auto& it : fNode->args()) {
        argSize += it->elementSizeInBytes();
    }
    
    emitDrop(code(), argSize);

    // If we want to use the _returnValue, push it
    if (fNode->pushReturn()) {
        switch (typeToOpSize(fNode->function()->returnType())) {
            case OpSize::i8:  code().push_back(uint8_t(Op::PUSHR1)); break;
            case OpSize::i16: code().push_back(uint8_t(Op::PUSHR2)); break;
            case OpSize::i32: 
            case OpSize::flt: code().push_back(uint8_t(Op::PUSHR4)); break;
        }
    }
}

void
CodeGen6809::emitCodeEnter(const ASTPtr& node, bool isLHS)
{
    uint16_t localSize = std::static_pointer_cast<EnterNode>(node)->localSize();
    if (localSize < 15) {
        code().push_back(uint8_t(Op::ENTERS) | localSize);
    } else {
        bool isLong = localSize > 255;
        code().push_back(uint8_t(Op::ENTER) | (isLong ? 0x01 : 0x00));
        appendValue(code(), localSize, isLong ? 2 : 1);
    }
}

void
CodeGen6809::emitCodeTypeCast(const ASTPtr& node, bool isLHS)
{
    ASTPtr arg = std::static_pointer_cast<TypeCastNode>(node)->arg();
    emitCode(arg, isLHS);
    
    Op op = castOp(arg->type(), node->type());
    if (op != Op::NOP) {
        code().push_back(uint8_t(op));
    }
}

void
CodeGen6809::emitCodeBranch(const ASTPtr& node, bool isLHS)
{
    auto bNode = std::static_pointer_cast<BranchNode>(node);
    
    switch (bNode->kind()) {
        case BranchNode::Kind::IfStart:
            // Emit the opcode with a 0 branch address and mark it
            
            // If this is the first pass, we don't know how long the
            // branch should be so we make enough space for a long
            // branch
            code().push_back(uint8_t(Op::BRF) | ((bNode->branchSize() == BranchSize::Short) ? 0x00 : 0x01));
            bNode->setFixupIndex(AddrNativeType(code().size()));
            code().push_back(0);
            if (bNode->branchSize() != BranchSize::Short) {
                code().push_back(0);
            }
            break;
        case BranchNode::Kind::Break:
        case BranchNode::Kind::Continue:
        case BranchNode::Kind::ElseStart:
            // Emit the opcode with a 0 branch address and mark it

            // If this is the first pass, we don't know how long the
            // branch should be so we make enough space for a long
            // branch
            code().push_back(uint8_t(Op::FBRA) | ((bNode->branchSize() == BranchSize::Short) ? 0x00 : 0x01));
            bNode->setFixupIndex(AddrNativeType(code().size()));
            code().push_back(0);
            if (bNode->branchSize() != BranchSize::Short) {
                code().push_back(0);
            }
            // Fallthrough to fixup the IfStartNode
        case BranchNode::Kind::LoopNext:
        case BranchNode::Kind::IfEnd:
            // Fixup branch
            if (bNode->fixupNode() != nullptr) {
                std::static_pointer_cast<BranchNode>(bNode->fixupNode())->fixup(code(), AddrNativeType(code().size()));
            }
            break;
        case BranchNode::Kind::LoopStart:
            // Save this for LoopEnd
            bNode->setFixupIndex(AddrNativeType(code().size()));
            break;
        case BranchNode::Kind::LoopEnd: {
            // branch back to fixupNode
            AddrNativeType addr = std::static_pointer_cast<BranchNode>(bNode->fixupNode())->fixupIndex();
            int16_t relAddr = int16_t(addr - AddrNativeType(code().size())) - 2;
            assert(relAddr < 0);
            
            // RBRA has a positive addr which is subtracted from pc
            relAddr = -relAddr;
            code().push_back(uint8_t(Op::RBRA) | ((relAddr <= 255) ? 0x00 : 0x01));
            appendValue(code(), relAddr, (relAddr <= 255) ? 1 : 2);
            break;
        }
        default:
            break;
    }
}

void
CodeGen6809::emitCodeSwitch(const ASTPtr& node, bool isLHS)
{
    auto sNode = std::static_pointer_cast<SwitchNode>(node);
    
    // First emit expression
    emitCode(sNode->expr(), false);
    
    Type type = sNode->expr()->type();
    
    // See Defines.h for the definition of the SWITCH opcode.
    uint16_t n = uint16_t(sNode->clauses().size());
    if (sNode->haveDefault()) {
        n -= 1;
    }
    uint16_t operand = n << 3;
    
    bool longAddr = sNode->branchSize() != BranchSize::Short;
    OpSize opSize = typeToOpSize(type);
    
    if (longAddr) {
        operand |= 0x04;
    }
    
    operand |= uint16_t(opSize);

    // emit the opcode and operand
    code().push_back(uint8_t(Op::SWITCH));
    appendValue(code(), operand, 2);
    
    // Jump addresses in list are relative to this point
    AddrNativeType jumpSourceAddr = AddrNativeType(code().size());
    
    // Now we need to sort the clauses, so we can binary search at runtime.
    std::sort(sNode->clauses().begin(), sNode->clauses().end(),
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
    for (auto& it : sNode->clauses()) {
        if (!it.isDefault()) {
            appendValue(code(), it.value(), opSizeToBytes(opSize));
            it.setFixupIndex(AddrNativeType(code().size()));
            appendValue(code(), 0, longAddr ? 2 : 1);
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
    AddrNativeType missingDefaultFixupIndex = 0;
    
    if (!sNode->haveDefault()) {
        code().push_back(uint8_t(Op::FBRA) | ((sNode->defaultBranchSize() != BranchSize::Short) ? 0x01 : 0x00));

        missingDefaultFixupIndex = AddrNativeType(code().size());
        appendValue(code(), 0, (sNode->defaultBranchSize() == BranchSize::Short) ? 1 : 2);
    }
    
    // If all branches in the list are short, we can make the list entry short
    bool allShortBranches = true;
    
    for (auto it = sNode->clauses().begin(); it != sNode->clauses().end(); ++it) {
        if (!it->isDefault()) {
            // No need to fixup default. It's always first
            AddrNativeType addr = AddrNativeType(code().size()) - jumpSourceAddr;
            if (addr >= 256) {
                allShortBranches = true;
            }
            it->fixup(code(), addr);
        }
        
        emitCode(it->stmt(), false);

        // The last clause will always have a branch of 0 so we can skip it
        if (it == sNode->clauses().end() - 1) {
            continue;
        }
        code().push_back(uint8_t(Op::FBRA) | ((it->branchSize() != BranchSize::Short) ? 0x01 : 0x00));

        // We can reuse the fixupIndex beacuse we're done with it
        it->setFixupIndex(AddrNativeType(code().size()));
        appendValue(code(), 0, ((it->branchSize() == BranchSize::Short) ? 1 : 2));
    }
    
    // Set branchSize if needed
    if (sNode->branchSize() == BranchSize::Unknown) {
        sNode->setBranchSize(allShortBranches ? BranchSize::Short : BranchSize::Long);
    }
    
    // Finally, fixup the branches at the end of the case statements
    if (!sNode->haveDefault()) {
        uint8_t adjustment = (sNode->defaultBranchSize() == BranchSize::Short) ? 1 : 2;
        BranchNode::fixup(code(), missingDefaultFixupIndex,
            AddrNativeType(code().size()) - missingDefaultFixupIndex - adjustment, sNode->defaultBranchSize());
    }
    
    for (auto it = sNode->clauses().begin(); it != sNode->clauses().end(); ++it) {
        if (it == sNode->clauses().end() - 1) {
            continue;
        }
        uint8_t adjustment = (it->branchSize() == BranchSize::Short) ? 1 : 2;
        it->fixup(code(), AddrNativeType(code().size()) - it->fixupIndex() - adjustment);
    }
}

void
CodeGen6809::emitCodeConditional(const ASTPtr& node, bool isLHS)
{
    auto cNode = std::static_pointer_cast<ConditionalNode>(node);

    // First emit expression
    emitCode(cNode->expr(), false);
    
    // Now emit if. If expr is false, jump to second expression, otherwise fall through to first
    code().push_back(uint8_t(Op::BRF) | ((cNode->ifBranchSize() == BranchSize::Short) ? 0x00 : 0x01));
    AddrNativeType fixupIndex = AddrNativeType(code().size());
    code().push_back(0);
    if (cNode->ifBranchSize() != BranchSize::Short) {
        code().push_back(0);
    }
    
    // Emit the first expr
    emitCode(cNode->first(), false);
    
    // Fixup the IF, It needs to jump past the FBRA
    // Fixed adjustment of 1 for both long and short versions
    BranchNode::fixup(code(), fixupIndex, AddrNativeType(code().size()) - fixupIndex + 1, cNode->ifBranchSize());
    
    // Now emit the branch past the first expr
    code().push_back(uint8_t(Op::FBRA) | ((cNode->elseBranchSize() == BranchSize::Short) ? 0x00 : 0x01));
    fixupIndex = AddrNativeType(code().size());
    code().push_back(0);
    if (cNode->elseBranchSize() != BranchSize::Short) {
        code().push_back(0);
    }
    
    // Emit the second expr
    emitCode(cNode->second(), false);
    
    // Fixup the else
    AddrNativeType adjustment = (cNode->elseBranchSize() == BranchSize::Short) ? -1 : -2;
    BranchNode::fixup(code(), fixupIndex, AddrNativeType(code().size()) - fixupIndex + adjustment, cNode->elseBranchSize());
}

void
CodeGen6809::emitCodeLogical(const ASTPtr& node, bool isLHS)
{
    // Code generated tests lhs. if true and Lor, skip rhs and push true. If
    // false and LAnd, skip rhs and push false
    auto lNode = std::static_pointer_cast<LogicalNode>(node);
    
    // First emit lhs
    emitCode(lNode->lhs(), false);
    
    // Now emit if. We use BRF for LAnd and BRT for LOr
    Op op = (lNode->kind() == LogicalNode::Kind::LAnd) ? Op::BRF : Op::BRT;
    code().push_back(uint8_t(op) | ((lNode->branchSize() == BranchSize::Short) ? 0x00 : 0x01));
    AddrNativeType fixupIndex = AddrNativeType(code().size());
    code().push_back(0);
    if (lNode->branchSize() != BranchSize::Short) {
        code().push_back(0);
    }
    
    // Emit rhs
    emitCode(lNode->rhs(), false);
    
    // Fixup the IF, It needs to jump past the FBRA
    // Fixed adjustment of 1 for both long and short versions
    BranchNode::fixup(code(), fixupIndex, AddrNativeType(code().size()) - fixupIndex + 1, lNode->branchSize());
    
    // Branch past the result push. It will always be short. And we can emit jump address
    // since it will always just jump past a PUSHKS1 which is 1 byte.
    code().push_back(uint8_t(Op::FBRA));
    code().push_back(1);
    
    // Emit the PUSHKS1. If this is LAnd then push a 0, otherwise push a 1
    code().push_back(uint8_t(Op::PUSHKS1) | ((lNode->kind() == LogicalNode::Kind::LAnd) ? 0 : 1));
}

void
CodeGen6809::emitCodeIndex(const ASTPtr& node, bool isLHS)
{
    auto iNode = std::static_pointer_cast<IndexNode>(node);

    emitCode(iNode->lhs(), true);
    
    // Optimization. If rhs is a constant 0, we can skip the index. Just emit the lhs.
    if (iNode->rhs()->astNodeType() == ASTNodeType::Constant) {
        if (std::reinterpret_pointer_cast<ConstantNode>(iNode->rhs())->integerValue() == 0) {
            return;
        }
    }

    // index can be 8 or 16 bit. We know its a valid type because the caller checked it
    emitCode(iNode->rhs(), false);

    uint16_t size = node->elementSizeInBytes();
    code().push_back(uint8_t((size <= 255) ? Op::INDEX1 : Op::INDEX2));
    appendValue(code(), size, (size <= 255) ? 1 : 2);
    
    // if isLHS is true then we're done, we have a ref on TOS. If not we need to DEREF
    if (!isLHS) {
        Op op;
        switch (typeToOpSize(node->type())) {
            case OpSize::i8:  op = Op::DEREF1; break;
            case OpSize::i16: op = Op::DEREF2; break;
            case OpSize::i32:
            case OpSize::flt: op = Op::DEREF4; break;
        }
        
        code().push_back(uint8_t(op));
    }
}

void
CodeGen6809::emitCodeReturn(const ASTPtr& node, bool isLHS)
{
    auto rNode = std::static_pointer_cast<ReturnNode>(node);

    if (rNode->arg() == nullptr) {
        code().push_back(uint8_t(Op::RET));
    } else {
        emitCode(rNode->arg(), false);
        uint8_t size = typeToBytes(rNode->arg()->type());
        Op op = (size == 1) ? Op::RETR1 : ((size == 2) ? Op::RETR2 : Op::RETR4);
        code().push_back(uint8_t(op));
    }
}

void
CodeGen6809::emitCodeDrop(const ASTPtr& node, bool isLHS)
{
    emitDrop(code(), std::static_pointer_cast<DropNode>(node)->bytesToDrop());
}

void
CodeGen6809::emitCodeRef(const ASTPtr& node, bool isLHS)
{
    // The ref operand can only be used on a right-hand operand.
    // We want to push a ref to it, thus passing 'true' here
    emitCode(std::static_pointer_cast<RefNode>(node)->operand(), true);
}

void
CodeGen6809::emitCodeDeref(const ASTPtr& node, bool isLHS)
{
    // We're guaranteed that _operand is a ref, so we want to push it
    // as though it's not a LHS. This will push the ref itself and not 
    // a reference to the reference.
    emitCode(std::static_pointer_cast<RefNode>(node)->operand(), false);
    
    // If this is LHS then we are done. The ref is on TOS, ready to be assigned to.
    // Otherwise we need to get the refed value.
    if (!isLHS) {
        uint8_t bytes = typeToBytes(node->type());
        code().push_back(uint8_t((bytes == 1) ? Op::DEREF1 : ((bytes == 2) ? Op::DEREF2 : Op::DEREF4)));
    }
}

void
CodeGen6809::emitCode(const ASTPtr& node, bool isLHS)
{
    node->setAnnotationAddr(AddrNativeType(code().size()));
    
    switch (node->astNodeType()) {
        case ASTNodeType::Statements    : emitCodeStatements(node, isLHS); break;
        case ASTNodeType::Op            : emitCodeOp(node, isLHS); break;
        case ASTNodeType::Var           : emitCodeVar(node, isLHS); break;
        case ASTNodeType::Constant      : emitCodeConstant(node, isLHS); break;
        case ASTNodeType::String        : emitCodeString(node, isLHS); break;
        case ASTNodeType::Dot           : emitCodeDot(node, isLHS); break;
        case ASTNodeType::Module        : emitCodeModule(node, isLHS); break;
        case ASTNodeType::FunctionCall  : emitCodeFunctionCall(node, isLHS); break;
        case ASTNodeType::Enter         : emitCodeEnter(node, isLHS); break;
        case ASTNodeType::TypeCast      : emitCodeTypeCast(node, isLHS); break;
        case ASTNodeType::Branch        : emitCodeBranch(node, isLHS); break;
        case ASTNodeType::Switch        : emitCodeSwitch(node, isLHS); break;
        case ASTNodeType::Conditional   : emitCodeConditional(node, isLHS); break;
        case ASTNodeType::Logical       : emitCodeLogical(node, isLHS); break;
        case ASTNodeType::Index         : emitCodeIndex(node, isLHS); break;
        case ASTNodeType::Return        : emitCodeReturn(node, isLHS); break;
        case ASTNodeType::Assignment    : emitCodeAssignment(node, isLHS); break;
        case ASTNodeType::Drop          : emitCodeDrop(node, isLHS); break;
        case ASTNodeType::Ref           : emitCodeRef(node, isLHS); break;
        case ASTNodeType::Deref         : emitCodeDeref(node, isLHS); break;
        case ASTNodeType::Initializer   : break;
    }
}
