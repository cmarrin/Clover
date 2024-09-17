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
    format("    include BOSS9.inc\n");
    format("    org $200\n");
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
            default: break;
            case OpSize::i8:
                format("    PULS A\nSTA ");
                emitAddr(symbol, offset);
                break;
            case OpSize::i16:
                format("    PULS D\nSTD ");
                emitAddr(symbol, offset);
                break;
        }
    } else if (ref) {
        // If ref is true, code generated will be a Ref
        format("    LEAX ");
        emitAddr(symbol, offset);
        format("    PSHS X\n");
    } else {
        switch (typeToOpSize(type)) {
            default: break;
            case OpSize::i8:
                format("    LDA ");
                emitAddr(symbol, offset);
                format("    PSHS A\n");
                break;
            case OpSize::i16:
                format("    LDD ");
                emitAddr(symbol, offset);
                format("    PSHS D\n");
                break;
        }
    }
}

void
CodeGen6809::emitCodeConstant(const ASTPtr& node, bool isLHS)
{
    assert(!isLHS);

    int32_t i = std::static_pointer_cast<ConstantNode>(node)->rawInteger();
    
    switch (typeToOpSize(node->type())) {
        default: break;
        case OpSize::i8:
            format("    LDA #$%02x\n", uint8_t(i));
            format("    PSHS A\n");
            break;
        case OpSize::i16:
            format("    LDD #$%04x\n", uint16_t(i));
            format("    PSHS D\n");
            break;
    }
}

void
CodeGen6809::emitCodeString(const ASTPtr& node, bool isLHS)
{
    // Add string to list
    uint32_t addr = uint32_t(_strings.size());
    _strings += std::static_pointer_cast<StringNode>(node)->string();
    
    format("    LEAX %s+$%x\n", StringLabel, addr);
    format("    PSHS X\n");
}


static const char* relopToString(Op op)
{
    switch (op) {
        default: return nullptr;
        case Op::EQ: return "BEQ";
        case Op::NE: return "BNE";
        case Op::LT: return "BLT";
        case Op::LO: return "BLO";
        case Op::LE: return "BLE";
        case Op::LS: return "BLS";
        case Op::GE: return "BGE";
        case Op::HS: return "BHS";
        case Op::GT: return "BGT";
        case Op::HI: return "BHI";
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
        opType = opNode->left()->type();
        emitCode(opNode->left(), opNode->isRef());
    }
    
    if (opNode->right()) {
        if (opNode->left() == nullptr) {
            opType = opNode->right()->type();
        }
        
        // If this is a unary operation (like INC) then _isAssignment is used
        emitCode(opNode->right(), (opNode->left() == nullptr) ? opNode->isRef() : isLHS);
    }
    
    bool is16Bit = typeToOpSize(opType) == OpSize::i16;
    
    // Handle Op::LNOT as a special case. There's no logical not opcode
    // so we just test if TOS is zero. If so, push a 1 otherwise push a 0.
    if (isLogical) {
        uint32_t label = nextLabelId();
        
        format("    CLRB\n");
        if (is16Bit) {
            format("    PULS X\n");
        } else {
            format("    PULS A\n");
        }
        format("    BNE L%d\n", label);
        format("    INCB\n");
        format("L%d\n", label);
        format("    PSHS B\n");
        
        return;
    }

    // If it's a relational operator, do the unoptimized case:
    //
    //	    LDA 1,S
    //	    CMPA 0,S
    //      LEAS 2,S
    //	    BLT L1
    //	    CLRA
    //	    BRA L2
    // L1:  LDA #1
    // L2:  PSHS A
    //
    const char* relOp = relopToString(opNode->op());
    
    uint32_t labelA = nextLabelId();
    uint32_t labelB = nextLabelId();

    if (relOp) {
        format("    LDA 1,S\n");
        format("    CMPA 0,S\n");
        format("    LEAS 2,S\n");
        format("    BLT L%d\n", labelA);
        format("    CLRA\n");
        format("    BRA L2\n", labelB);
        format("L%d\n", labelA);
        format("    LDA #1\n");
        format("L%d\n", labelB);
        format("    PSHS A\n");
        return;
    }

    switch (typeToOpSize(opType)) {
        case OpSize::i8:
            format("LDA 1,S\n");
            format("ADDA 0,S\n"); // FIXME. Handle all the other opcodes
            format("LEAS 2,S\n");
            format("PSHS A\n");
            break;
        case OpSize::i16:
            format("LDD 2,S\n");
            format("ADDD 0,S\n"); // FIXME. Handle all the other opcodes
            format("LEAS 4,S\n");
            format("PSHS D\n");
            break;
        case OpSize::i32:
        case OpSize::flt:
            format("JSR add32\n"); // FIXME. Handle all the other opcodes
            break;
    }
}

void
CodeGen6809::emitCodeInc(const ASTPtr& node, bool isLHS)
{
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
        case ASTNodeType::Inc           : emitCodeInc(node, isLHS); break;
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
