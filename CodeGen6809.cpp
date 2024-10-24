/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "CodeGen6809.h"

#include "Compiler.h"

using namespace clvr;

void
CodeGen6809::emitPreamble(const Compiler* compiler)
{
    format("* 6809 assembly generated from Clover source\n\n");
    format("    pragma autobranchlength\n");
    format("    include BOSS9.inc\n");
    format("    org $200\n\n");

    // On entry the stack is set to TOS and PC is set to $200.
    // We need to instantiate the top level struct, then call
    // its ctor, then call main
    
    // Allocate memory for top-level struct
    if (compiler->topLevelStruct()->sizeInBytes()) {
        format("    LEAS -%d,S\n", compiler->topLevelStruct()->sizeInBytes());
    }
    
    // Set top-level self pointer
    format("    TFR S,Y\n");
    
    // Call top-level ctor, if any
    if (compiler->topLevelStruct()->hasCtor()) {
        std::string ctorName = compiler->topLevelStruct()->name() + "_ctor";
        
        // Push a dummy self pointer
        format("    LEAS -2,S\n");
        
        // Call the ctor
        format("    JSR %s\n", ctorName.c_str());
        
        // Toss the self pointer
        format("    LEAS 2,S\n");
    }
    
    // Now call main
    std::string mainName = compiler->topLevelStruct()->name() + "_main";
    
    // Push a dummy self pointer
    format("    LEAS -2,S\n");
    
    // Call main
    format("    JSR %s\n", mainName.c_str());
    
    // Toss the self pointer
    format("    LEAS 2,S\n");
    
    // Now jump to exit to end the program or enter the monitor
    format("    JMP exit\n");
}

void
CodeGen6809::handleFunction(const Compiler* compiler, const FunctionPtr& function, bool isTopLevel)
{
    // emit function name
    std::string funcName = function->name();
    if (funcName.empty()) {
        funcName = "ctor";
    }
    format("\n%s_%s\n",function->structName().c_str(), funcName.c_str());
}

void
CodeGen6809::emitPostamble(const Compiler* compiler)
{
    // Add constants
    format("\nConstants");
    uint32_t i = 0;
    for (const auto& it : compiler->constants()) {
        if ((i++ % 8) == 0) {
            format("\n    FCB $%02x", it);
        } else {
            format(",$%02x", it);
        }
    }
    
    format("\n");
    
    // add strings
    format("\n%s\n", StringLabel);
    
    for (const auto& it : _strings) {
        // For each string we must split it at escape characters and add them as FCB entries
        std::string str = it;
        
        while (true) {
            const auto& result = std::find_if(str.begin(), str.end(), [ ](char c) { return !isprint(c); });
            std::string printable;
            if (result == str.end()) {
                // At the end of the string without finding a non-printable char
                printable = str;
                str.clear();
            } else {
                printable = str.substr(0, result - str.begin());
                str = str.substr(result - str.begin());
            }
            
            if (!printable.empty()) {
                format("    FCC \"%s\"\n", printable.c_str());
            }
            
            // If str still has chars then the first char is non-printable
            if (!str.empty()) {
                format("    FCB $%02x\n", uint32_t(str[0]));
                str = str.substr(1);
            } else {
                format("    FCB $00\n");
                break;
            }
        }
    }
    
    if ((i++ % 8) != 0) {
        format("\n");
    }
    
    
    // set entry point
    format("\n    end $200\n");
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

void
CodeGen6809::emitAddr(const ASTPtr& node, EmitAddrType emitAddrType)
{
    SymbolPtr symbol = std::static_pointer_cast<VarNode>(node)->symbol();
    AddrNativeType offset = (emitAddrType != EmitAddrType::LSB) ? 0 : 1;
    offset += std::static_pointer_cast<VarNode>(node)->offset();
    
    // Determine the addr mode. Addr is unsigned. See Defines.h (Address Mode)
    // for details
    Index index;
    int16_t relAddr = symbol->addr(index);
    relAddr += (index == Index::L) ? -offset : offset;
    if (index == Index::C) {
        format("Constants+%d\n", relAddr);
    } else {
        const char* indexReg;
        
        switch (index) {
            default: break;
            case Index::L:
                relAddr = -relAddr - 1;
                indexReg = "U";
                break;
            case Index::A:
                relAddr += 6;
                indexReg = "U";
                break;
            case Index::M:
                indexReg = "Y";
                break;
        }
        format("%d,%s\n", relAddr, indexReg);
    }
}

void
CodeGen6809::emitAddrOrConst(const ASTPtr& node, EmitAddrType emitAddrType)
{
    // If emitLSB is false we emit the entire number
    if (node->astNodeType() == ASTNodeType::Var) {
        emitAddr(node, emitAddrType);
    } else {
        assert(node->astNodeType() == ASTNodeType::Constant);
        int32_t i = std::static_pointer_cast<ConstantNode>(node)->rawInteger();
        if (emitAddrType == EmitAddrType::MSB) {
            i = int8_t(i >> 8);
        } else if (emitAddrType == EmitAddrType::LSB) {
            i = int8_t(i);
        }
        format("#%d\n", i);
    }
}

void
CodeGen6809::emitCodeVar(const ASTPtr& node, Type type, bool ref, bool pop)
{
    // If this is a pointer then we push it as a AddrType not the underlying type
    if (type == Type::None) {
        type = node->isPointer() ? AddrType : node->type();
    }

    bool is16Bit = typeToOpSize(type) == OpSize::i16;
    
    // We either generate:
    //
    //      Pop optimization    : PUL, ST <addr>
    //      Ref                 : LEAX <addr>, PSH X
    //      Push                : LD <addr>, PSH
    //
    // 8 bit uses A, 16 bit uses D, and 32 bit uses D/X
    
    if (pop) {
        // Generate POP optimization
        if (!isReg(is16Bit ? RegState::D : RegState::A)) {
            format("    PULS %s\n", is16Bit ? "D" : "A");
        }
        format("    ST%s ", is16Bit ? "D" : "A");
        emitAddr(node);
        clearRegState();
    } else if (ref) {
        stashPtrIfNeeded();

        // If the addr is constant memory, then use LDX to load the immediate address value
        if (std::static_pointer_cast<VarNode>(node)->symbol()->index() == Index::C) {
            format("    LDX #");
        } else {
            format("    LEAX ");
        }
        emitAddr(node);
        setRegState(RegState::X);
    } else {
        stashRegIfNeeded();
        
        format("    LD%s ", is16Bit ? "D" : "A");
        emitAddr(node);
        setRegState(is16Bit ? RegState::D : RegState::A);
    }
}

void
CodeGen6809::emitCodeConstant(const ASTPtr& node, bool isLHS)
{
    stashRegIfNeeded();

    assert(!isLHS);

    int32_t i = std::static_pointer_cast<ConstantNode>(node)->rawInteger();
    
    if (typeToOpSize(node->type()) == OpSize::i16) {
        format("    LDD #%d\n", uint16_t(i));
        setRegState(RegState::D);
    } else {
        format("    LDA #%d\n", uint8_t(i));
        setRegState(RegState::A);
    }
}

void
CodeGen6809::emitCodeString(const ASTPtr& node, bool isLHS)
{
    stashPtrIfNeeded();

    // Add string to list
    uint32_t addr = _stringSize;
    const std::string& s = std::static_pointer_cast<StringNode>(node)->string();
    _strings.push_back(s);
    
    // stringSize includes null terminator
    _stringSize += uint32_t(s.size()) + 1;
    
    format("    LDD #%s+$%x\n", StringLabel, addr);
    setRegState(RegState::D);
}

static const char* relopToString(Op op, bool inv)
{
    switch (op) {
        default: return nullptr;
        case Op::EQ: return inv ? "BNE" : "BEQ";
        case Op::NE: return inv ? "BEQ" : "BNE";
        case Op::LT: return inv ? "BGE" : "BLT";
        case Op::LO: return inv ? "BHS" : "BLO";
        case Op::LE: return inv ? "BGT" : "BLE";
        case Op::LS: return inv ? "BHI" : "BLS";
        case Op::GE: return inv ? "BLT" : "BGE";
        case Op::HS: return inv ? "BLO" : "BHS";
        case Op::GT: return inv ? "BLE" : "BGT";
        case Op::HI: return inv ? "BLS" : "BHI";
    }
}

static bool canDoBinaryOptimization(const ASTPtr& left, const ASTPtr& right)
{
    return  (left->astNodeType() == ASTNodeType::Var || left->astNodeType() == ASTNodeType::Constant) &&
            (right->astNodeType() == ASTNodeType::Var || right->astNodeType() == ASTNodeType::Constant);
}

void
CodeGen6809::emitMulOp(const ASTPtr& left, Op op, const ASTPtr& right)
{
    Type opType = left->type();
    bool is16Bit = typeToOpSize(opType) == OpSize::i16;
    bool isSigned = op == Op::IMUL;
    bool optimize = canDoBinaryOptimization(left, right);

    uint16_t labelA = nextLabelId();
    uint16_t labelB = nextLabelId();
    uint16_t labelC = nextLabelId();
    
    // The binary optimization is only really useful for 8x8 multiply
    if (optimize && !is16Bit) {
        if (isSigned) {
            format("    CLR ,-S\n");
        }
        emitCode(left, false);
        clearRegState();

        // assume value is in A
        
        if (isSigned) {
            format("    BPL L%d\n", labelA);
            format("    NEG 0,S\n");
            format("    NEGA\n");
            format("L%d\n", labelA);
            format("    TFR A,B\n");
        }
        
        emitCode(right, false);
        clearRegState();
        
        if (isSigned) {
            format("    BPL L%d\n", labelB);
            format("    NEG 0,S\n");
            format("    NEGA\n");
            format("L%d\n", labelB);
        }
        
        format("    MUL\n");

        if (isSigned) {
            format("    TST 0,S\n");
            format("    BPL L%d\n", labelC);
            format("    NEGB\n");
            format("L%d\n", labelC);
            format("    LEAS 1,S\n");
        }
        format("    TFR B,A\n");
        setRegState(RegState::A);
        return;
    }
    
    emitCode(right, false);
    emitCode(left, false);

    // operands need to be on the stack
    stashRegIfNeeded();

    // Push storage for sign byte
    if (is16Bit) {
        format("    CLR ,-S\n");
    
        if (isSigned) {
            format("    TST 3,S\n");
            format("    BPL L%d\n", labelA);
            format("    NEG 0,S\n");
            format("    LDD #0\n");
            format("    SUBD 3,S\n");
            format("    STD 3,S\n");
            format("L%d\n", labelA);
            format("    TST 1,S\n");
            format("    BPL L%d\n", labelB);
            format("    NEG 0,S\n");
            format("    LDD #0\n");
            format("    SUBD 1,S\n");
            format("    STD 1,S\n");
            format("L%d\n", labelB);
        }
        
        // TOS is the accumulator, TOS+2 is sign, TOS+3 is lhs, TOS+5 is rhs
        format("    LDA 4,S\n");    // Don't push the accumulator until after the first multiply
        format("    LDB 2,S\n");
        format("    MUL\n");
        format("    PSHS D\n");     // Now TOS is the 16 bit accumulator
        format("    LDA 5,S\n");
        format("    LDB 4,S\n");
        format("    MUL\n");
        format("    ADDB 1,S\n");    // Toss MSB of result
        format("    STB 1,S\n");
        format("    LDA 6,S\n");
        format("    LDB 3,S\n");
        format("    MUL\n");
        format("    ADDB 1,S\n");    // Toss MSB of result
        format("    STB 1,S\n");

        if (isSigned) {
            format("    TST 2,S\n");     // Do the sign
            format("    BPL L%d\n", labelC);
            format("    LDD #0\n");
            format("    SUBD 0,S\n");
            format("L%d\n", labelC);
        }
        format("    PULS D\n");
        format("    LEAS 5,S\n");
        setRegState(RegState::D);
    } else {
        if (isSigned) {
            format("    LDA 2,S\n");
            format("    BPL L%d\n", labelA);
            format("    NEG 0,S\n");
            format("    NEG 2,S\n");
            format("L%d\n", labelA);
            format("    LDA 1,S\n");
            format("    BPL L%d\n", labelB);
            format("    NEG 0,S\n");
            format("    NEG 1,S\n");
            format("L%d\n", labelB);
        }
        
        // At this point TOS has a sign byte either way
        format("    LDB 1,S\n");
        format("    LDA 2,S\n");
        format("    MUL\n");

        if (isSigned) {
            format("    TST 0,S\n");
            format("    BPL L%d\n", labelC);
            format("    NEGB\n");
            format("L%d\n", labelC);
        }
        format("    LEAS 3,S\n");
        format("    TFR B, A\n");
        setRegState(RegState::A);
    }
}

void
CodeGen6809::emitShiftOp(const ASTPtr& left, Op op, const ASTPtr& right)
{
    // We can do 8 or 16 bit shifts, right or left, signed or unsigned.
    // 6809 can only do 8 bit shifts and rotate of 1 bit. For now we will
    // do simple (and very inefficient) shift loops. Next we'll do some
    // basic optimizations
    Type opType = left->type();
    bool is16Bit = typeToOpSize(opType) == OpSize::i16;
    bool isSigned = op == Op::ASR1;
    
    // If shifting a 16 bit value right do ASR Or LSR of msb followed by ROR of lsb
    // If shifting a 16 bit value left do LSL of lsb followed by ROL of msb
    bool isLeft = (op == Op::SHL1);
    const char* opStr = isLeft ? "LSL" : (isSigned ? "ASR" : "LSR");
    uint16_t label = nextLabelId();

    emitCode(left, false);
    stashRegIfNeeded();
    emitCode(right, false);
    
    // Now lhs should be on TOS (1 or 2 bytes) and rhs is in D or A
    if (is16Bit) {
        format("    TFR B,A\n"); // Put count in A
        format("L%d\n", label);
        if (isLeft) {
            format("    %s 1,S\n", opStr);
            format("    ROL 0,S\n");
        } else {
            format("    %s 0,S\n", opStr);
            format("    ROR 1,S\n");
        }
        format("    DECA\n");
        format("    BNE L%d\n", label);
        format("    PULS D\n");
        setRegState(RegState::D);
    } else {
        format("L%d\n", label);
        format("    %s 0,S\n", opStr);
        format("    DECA\n");
        format("    BNE L%d\n", label);
        format("    PULS A\n");
        setRegState(RegState::A);
    }
}

void
CodeGen6809::emitBinaryOp(const ASTPtr& left, Op op, const ASTPtr& right)
{
    Type opType = left->type();
    bool is16Bit = typeToOpSize(opType) == OpSize::i16;
    
    if (op == Op::UDIV || op == Op::IDIV) {
        emitCode(right, false);
        emitCode(left, false);
        
        // operands need to be on the stack
        stashRegIfNeeded();

        // Handle Div (done with a call to BOSS9)
        bool isSigned = op == Op::IDIV;
        format("    JSR %sdiv%s\n", isSigned ? "i" : "u", is16Bit ? "16" : "8");
        format("    LEAS 4,S\n");

        // Result is in A/D
        setRegState(is16Bit ? RegState::D : RegState::A);
        return;
    }
    
    if (op == Op::UMUL || op == Op::IMUL) {
        emitMulOp(left, op, right);
        return;
    }
    
    if (op == Op::SHR1 || op == Op::ASR1 || op == Op::SHL1) {
        emitShiftOp(left, op, right);
        return;
    }
    
    // See if we can do the binary op optimization
    bool optimize = canDoBinaryOptimization(left, right);
    
    if (optimize) {
        emitCode(left, false);
        if (!isReg(is16Bit ? RegState::D : RegState::A)) {
            format("    PULS %s\n", is16Bit ? "D" : "A");
        }
    } else {
        emitCode(right, false);
        emitCode(left, false);
    }

    switch (op) {
        default: break;
        case Op::ADD:
        case Op::SUB: {
            const char* opStr = (op == Op::ADD) ? "ADD" : "SUB";
            
            if (is16Bit) {
                if (optimize) {
                    format("    %sD ", opStr);
                    emitAddrOrConst(right);
                } else {
                    if (isReg(RegState::D)) {
                        format("    %sD 0,S\n", opStr);
                        format("    LEAS 2,S\n");
                    } else {
                        format("    LDD 0,S\n");
                        format("    %sD 2,S\n", opStr);
                        format("    LEAS 4,S\n");
                    }
                }
                setRegState(RegState::D);
            } else {
                if (optimize) {
                    format("    %sA ", opStr);
                    emitAddrOrConst(right);
                } else {
                    if (isReg(RegState::A)) {
                        format("    %sA 0,S\n", opStr);
                        format("    LEAS 1,S\n");
                    } else {
                        format("    LDA 0,S\n");
                        format("    %sA 1,S\n", opStr);
                        format("    LEAS 2,S\n");
                    }
                }
                setRegState(RegState::A);
            }
            break;
        }
        case Op::OR1:
        case Op::AND1:
        case Op::XOR1: {
            const char* opStr = (op == Op::OR1) ? "OR" : ((op == Op::AND1) ? "AND" : "EOR");

            // we can do it efficiently if the rhs is in a reg
            if (is16Bit) {
                if (optimize) {
                    format("    %sD ", opStr);
                    emitAddrOrConst(right, EmitAddrType::MSB);
                    format("    %sD ", opStr);
                    emitAddrOrConst(right, EmitAddrType::LSB);
                } else {
                    if (isReg(RegState::D)) {
                        format("    %sA 0,S\n", opStr);
                        format("    %sB 1,S\n", opStr);
                        format("    LEAS 2,S\n");
                    } else {
                        format("    LDD 2,S\n");
                        format("    %sA 0,S\n", opStr);
                        format("    %sB 1,S\n", opStr);
                        format("    LEAS 4,S\n");
                    }
                }
                setRegState(RegState::D);
            } else {
                if (optimize) {
                    format("    %sA ", opStr);
                    emitAddrOrConst(right);
                } else {
                    if (isReg(RegState::A)) {
                        format("    %sA 0,S\n", opStr);
                        format("    LEAS 1,S\n");
                    } else {
                        format("    LDA 0,S\n");
                        format("    %sA 1,S\n", opStr);
                        format("    LEAS 2,S\n");
                    }
                }
                setRegState(RegState::A);
            }
            break;
        }
    }
}

void
CodeGen6809::emitCodeOp(const ASTPtr& node, bool isLHS)
{
    assert(!isLHS);
    
    // _type is the result type not the type used for operation. We need
    // to get that from the left or right operand
    auto opNode = std::static_pointer_cast<OpNode>(node);
    Type opType = opNode->left() ? opNode->left()->type() : opNode->right()->type();
    bool is16Bit = typeToOpSize(opType) == OpSize::i16;
    Op op = opNode->op();
    
    // Handle Op::LNOT as a special case. There's no logical not opcode
    // so we just test if TOS is zero. If so, put a 1 in A otherwise put a 0.
    if (op == Op::LNOT) {
        // always rhs for LNOT
        
        // If we're trying to do the optimized branch case then we want to invert the sense
        // of the test to account for the LNOT
        if (branchLabel() != -1) {
            setBranchLabel(branchLabel(), !branchTestInverted());
            emitCode(opNode->right(), opNode->isRef());
            return;
        }
        
        emitCode(opNode->right(), opNode->isRef());
        
        uint16_t labelA = nextLabelId();
        uint16_t labelB = nextLabelId();
        
        // Assume CC is correctly set if value is in A or D
        if (is16Bit) {
            if (!isReg(RegState::D)) {
                format("    PULS D\n");
                
                // PULS doesn't set CC, set them now
                format("    LDD 0,S\n");
                format("    LEAS 2,S\n");
            }
        } else {
            if (!isReg(RegState::A)) {
                format("    PULS A\n");
                format("    TSTA\n");
            }
        }
        
        format("    BNE L%d\n", labelA);
        format("    LDA #1\n");
        format("    BRA L%d\n", labelB);
        format("L%d\n", labelA);
        format("    CLRA\n");
        format("L%d\n", labelB);
        setRegState(RegState::A);
        
        return;
    }
    
    if (op == Op::NEG) {
        emitCode(opNode->right(), false);
        
        if (is16Bit) {
            // Make sure the value is on the stack
            stashRegIfNeeded();
            assert(_lastRegState == RegState::StackI16);
            
            format("    LDD #0\n");
            format("    SUBD 0,S\n");
            format("    LEAS 2,S\n");
            setRegState(RegState::D);
        } else {
            // Handle stack and non-stack case
            if (isReg(RegState::A)) {
                format("    NEGA\n");
            } else {
                format("    NEG 0,S\n");
            }
            // RegState already has the right value
        }
        return;
    }
    
    if (op == Op::NOT1) {
        emitCode(opNode->right(), false);

        if (is16Bit) {
            if (isReg(RegState::D)) {
                format("    COMA\n");
                format("    COMB\n");
            } else {
                format("    COM 0,S\n");
                format("    COM 1,S\n");
            }
        } else {
            if (isReg(RegState::A)) {
                format("    COMA\n");
            } else {
                format("    COM 0,S\n");
            }
        }
        // RegState already has the right value
        return;
    }

    // For binary operations, emit the rhs then lhs. That way the rhs ends
    // up on the stack and lhs is in a reg. So you can do a binary op between
    // the reg and TOS and everything is in the right order.
    //
    // There is an optimization we can do for binary operations. If the lhs
    // and rhs are either var or constant we can load the lhs into A or D
    // then perform the operation on the rhs value. But we can only do this
    // for operations that can be done natively. We need to do MUL and DIV
    // differently
    
    // FIXME: I don't understand isRef(). Can a binary op ever be a ref? I don't think so.
    assert(opNode->left() && opNode->right());
    
    const char* relOp = relopToString(op, true);
    if (!relOp) {
        emitBinaryOp(opNode->left(), op, opNode->right());
    } else {
        bool optimize = canDoBinaryOptimization(opNode->left(), opNode->right());
        
        if (optimize) {
            emitCode(opNode->left(), false);
        } else {
            emitCode(opNode->right(), false);
            emitCode(opNode->left(), false);
        }
        
        // If we have a branchLabel we can do the optimized case
        if (branchLabel() != -1) {
            // Branch label is expecting us to branch when the test is false
            if (!isReg(is16Bit ? RegState::D : RegState::A)) {
                format("    PULS %s\n", is16Bit ? "D" : "A");
            }
            
            // Invert test if needed
            if (branchTestInverted()) {
                relOp = relopToString(op, false);
            }
            
            if (optimize) {
                format("    CMP%s ", is16Bit ? "D" : "A");
                emitAddrOrConst(opNode->right());
            } else {
                format("    CMP%s 0,S\n", is16Bit ? "D" : "A");
                format("    LEAS %d,S\n", is16Bit ? 2 : 1);
            }
            format("    %s L%d\n", relOp, branchLabel());
            setBranchLabel(-1);
        } else {
            // If it's a relational operator, do the unoptimized case:
            //
            //	    PULS <A/D>      if lhs is on stack
            //
            //	    CMP<A/D> 0,S
            //	    B<op> L1        compare lhs (in A or D) to rhs on TOS
            //	    LDA #1
            //      BRA L2
            // L1
            //      CLRA
            // L2
            //      LEAS <1/2>,S
            //
            uint16_t labelA = nextLabelId();
            uint16_t labelB = nextLabelId();

            if (optimize) {
                format("    CMP%s ", is16Bit ? "D" : "A");
                emitAddrOrConst(opNode->right());
            } else {
                if (!isReg(is16Bit ? RegState::D : RegState::A)) {
                    format("    PULS %s\n", is16Bit ? "D" : "A");
                }
                format("    CMP%s 0,S\n", is16Bit ? "D" : "A");
            }
            
            format("    %s L%d\n", relOp, labelA);
            format("    LDA #1\n");
            format("    BRA L%d\n", labelB);
            format("L%d\n", labelA);
            format("    CLRA\n");
            format("L%d\n", labelB);
            
            if (!optimize) {
                format("    LEAS %d,S\n", is16Bit ? 2 : 1);
            }
            setRegState(RegState::A);
        }
    }
}

void
CodeGen6809::emitCodeInc(const ASTPtr& node, bool isLHS)
{
    auto incNode = std::static_pointer_cast<IncNode>(node);

    emitCode(incNode->node(), true);
    
    int16_t inc = incNode->inc();

    if (!isReg(RegState::X)) {
        format("    PULS X\n");
    }
    
    bool is16Bit = typeToOpSize(node->type()) == OpSize::i16;
    
    if (is16Bit) {
        format("    LDD 0,X\n");
        if (incNode->isPre()) {
            format("    ADDD #%d\n", inc);
            setRegState(RegState::D);
        } else {
            format("    PSHS D\n");
            format("    ADDD #%d\n", inc);
            setRegState(RegState::StackI16);
        }
        format("    STD 0,X\n");
    } else {
        format("    LDA 0,X\n");
        if (incNode->isPre()) {
            format("    ADDA #%d\n", inc);
            setRegState(RegState::A);
        } else {
            format("    PSHS A\n");
            format("    ADDA #%d\n", inc);
            setRegState(RegState::StackI8);
        }
        format("    STA 0,X\n");
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
    bool is16Bit = typeToOpSize(node->type()) == OpSize::i16;

    if (assignmentNode->op() != Op::NOP) {
        emitBinaryOp(assignmentNode->left(), assignmentNode->op(), assignmentNode->right());
    } else {
        emitCode(assignmentNode->right(), false);
    }
    
    // If lhs is a VarNode, we can optimize to turn this:
    //
    //      LEAX i,U
    //      PSHS X
    //      PULS X
    //      PULS <A/D>
    //      ST<A/D> 0,X
    
    // into:
    //
    //      PUL<A/D>
    //      ST<A/D> i,U
    //
    if (assignmentNode->left()->astNodeType() == ASTNodeType::Var) {
        emitPopCodeVar(assignmentNode->left());
        return;
    }
    
    emitCode(assignmentNode->left(), true);
    
    // If this is a pointer assignment, we need to use the pointer type
    if (assignmentNode->left()->isPointer()) {
        is16Bit = true;
    }
    
    if (!isReg(RegState::X)) {
        format("    PULS X\n");
    }
    
    if (is16Bit) {
        if (!isReg(RegState::D)) {
            format("    PULS D\n");
        }
        format("    STD 0,X\n");
    } else {
        if (!isReg(RegState::A)) {
            format("    PULS A\n");
        }
        format("    STA 0,X\n");
    }
    
   clearRegState();
}

void
CodeGen6809::emitCodeDot(const ASTPtr& node, bool isLHS)
{
    ASTPtr operand = std::static_pointer_cast<DotNode>(node)->operand();
    SymbolPtr property = std::static_pointer_cast<DotNode>(node)->property();
    bool is16Bit = typeToOpSize(node->type()) == OpSize::i16;
    
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

    if (!isReg(RegState::X)) {
        format("    PULS X\n");
    }
    
    setRegState(RegState::X);

    // We can skip the OFFSET if offset value is 0
    if (offset) {
        format("    LEAX %d,X\n", offset);
    }

    clearRegState();
    
    if (!isLHS) {
        // Just Deref
        format("    LD%s 0,X\n", is16Bit ? "D" : "A");
        setRegState(is16Bit ? RegState::D : RegState::A);
    } else {
        // Pass back the addr in X
        setRegState(RegState::X);
    }
}

void
CodeGen6809::emitCodeModule(const ASTPtr& node, bool isLHS)
{    
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
        
        // Values need to be on the stack
        stashRegIfNeeded();
        clearRegState();
    }
    
    std::string callName = fNode->moduleName();
    
    // If this is a native call, push the name of the call. If the
    // module is not "core" prefix it with <module name>_
    if (fNode->function()->isNative()) {
        callName = fNode->moduleName();
        if (callName == "core") {
            callName.clear();
        } else {
            callName += "_";
        }
        
        callName += fNode->function()->name();
        format("    JSR %s\n", callName.c_str());
        return;
    }
    
    // If this is a member call, save the old Y and put the instance
    // pointer in Y. On return pop TOS back to Y. If there's no instance
    // we need to push a dummy pointer so all the arg offsets work
    if (fNode->instance()) {
        format("    PSHS Y\n");
        emitCode(fNode->instance(), true);
        format("    PULS Y\n");
    } else {
        format("    LEAS -2,S\n");
    }

    format("    JSR %s_%s\n", fNode->function()->structName().c_str(), fNode->function()->name().c_str());

    // Now we need to pop the previous Y or toss the dummy pointer
    bool wantLEAS = false;
    if (fNode->instance()) {
        format("    PULS Y\n");
    } else {
        wantLEAS = true;
    }
    
    // Pop the args after the call returns. Args pushed is not necessarily the
    // same as the arg list in the function. More args might be passed with
    // VarArgs being used to access them.
    uint16_t argSize = wantLEAS ? 2 : 0;
    for (auto& it : fNode->args()) {
        argSize += it->elementSizeInBytes();
    }
    
    // FIXME: If we do a LEAS above and we have one here, we can combine them and toss argSize + 2 bytes in one
    if (argSize > 0) {
        format("    LEAS %d,S\n", argSize);
    }
    
    // Return value (in A/D) if any
    clearRegState();
    
    if (fNode->pushReturn()) {
        setRegState((typeToOpSize(fNode->function()->returnType()) == OpSize::i16) ? RegState::D : RegState::A);
    }
}

void
CodeGen6809::emitCodeEnter(const ASTPtr& node, bool isLHS)
{
    uint16_t localSize = std::static_pointer_cast<EnterNode>(node)->localSize();

    format("    PSHS U\n");
    format("    TFR S,U\n");
    if (localSize) {
        format("    LEAS -%d,S\n", localSize);
    }
    clearRegState();
}

void
CodeGen6809::emitCodeTypeCast(const ASTPtr& node, bool isLHS)
{
    ASTPtr arg = std::static_pointer_cast<TypeCastNode>(node)->arg();
    emitCode(arg, isLHS);
    
    Type fromType = arg->type();
    Type toType = node->type();
    
    // If we're type casting from 16 to 8 bits for an if test we don't need
    // to bother. We can just set the CC correctly
    if (typeToOpSize(fromType) == OpSize::i16 && typeToOpSize(toType) == OpSize::i8 && branchLabel() != -1) {
        if (!isReg(RegState::D)) {
            format("    LDD 0,S\n");
            format("    LEAS 2,S\n");
        }
        setRegState(RegState::A);
        return;
    }

    // We're casting from 8 to 16 or from 16 to 8
    // If going from 8 to 16, we sign extend if neededType is signed.
    // If we're casting to the same size (e.g., going from signed to unsigned)
    // just skip it
    if (typeToBytes(fromType, false) != typeToBytes(toType, false)) {
        if (typeToBytes(toType, false) == 2) {
            // going from 8 to 16 bits
            if (isReg(RegState::A)) {
                format("    TFR A,B\n");
            } else {
                format("    PULS B\n");
            }
            if (arg->isSigned()) {
                // Sign extend
                format("    SEX\n");
            } else {
                format("    CLRA\n");
                
                // Set the CC properly
                format("    ADDD #0\n");
            }
            setRegState(RegState::D);
        } else {
            // going from 16 to 8 bits
            format("    TFR B,A\n");
            
            // TFR doesn't set CC. Do it now
            format("    TSTA\n");
            setRegState(RegState::A);
        }
    }
}

void
CodeGen6809::emitCodeBranch(const ASTPtr& node, bool isLHS)
{
    // There are separate BranchNodes for each branch point
    // in an IF or LOOP statement. Some Branch nodes have a
    // "fixupNode" which is the node that matches its branch point.
    // Labels have a unique numeric id. Store that in the fixupIndex
    // field of the BranchNode
    // Nodes that have a fixupNode are:
    //
    //      ElseStart   -> IfStart
    //      IFEnd       -> (ElseStart or IfStart if no else)
    //      LoopEnd     -> LoopStart
    //      Continue    -> LoopNext
    //      Break       -> IfEnd
    auto bNode = std::static_pointer_cast<BranchNode>(node);
    
    switch (bNode->kind()) {
        case BranchNode::Kind::IfStart:
            // emit expression
            // Branch optimization.
            setBranchLabel(getLabelId(bNode));
            emitCode(bNode->expr(), false);
            
            if (branchLabel() != -1) {
                // branchLabel was not used, do it here
                
                // test value is always 8 bit
                if (!isReg(RegState::A)) {
                    format("    PULS A\n");
                    format("    TSTA\n");
                }
                format("    BEQ L%d\n", branchLabel());
            }
            clearRegState();
            break;
        case BranchNode::Kind::ElseStart:
            format("    BRA L%d\n", getLabelId(bNode));
            format("L%d\n", getLabelId(bNode->fixupNode()));
            break;
        case BranchNode::Kind::IfEnd:
            format("L%d\n", getLabelId(bNode->fixupNode()));
            break;
        case BranchNode::Kind::LoopStart:
            format("L%d\n", getLabelId(bNode));
            break;
        case BranchNode::Kind::LoopNext:
            format("L%d\n", getLabelId(bNode));
            break;
        case BranchNode::Kind::LoopEnd:
            format("    BRA L%d\n", getLabelId(bNode->fixupNode()));
            break;
        case BranchNode::Kind::Break:
        case BranchNode::Kind::Continue:
            format("    BRA L%d\n", getLabelId(bNode->fixupNode()));
            break;
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
    
    bool is16Bit = typeToOpSize(sNode->expr()->type()) == OpSize::i16;

    // Value needs to be on stack for call
    if (isReg(is16Bit ? RegState::D : RegState::A)) {
        format("    PSHS %s\n", is16Bit ? "D" : "A");
    }
    
    uint16_t nClauses = uint16_t(sNode->clauses().size());
    if (sNode->haveDefault()) {
        nClauses -= 1;
    }

    // On return from switch<1/2> X contains the value from the
    // table if the value is found. If not X contains the address
    // just past the table. Do an indexed jump to this addr
    uint16_t tableLabel = nextLabelId();
    uint16_t switchSize = is16Bit ? 2 : 1;

    format("    LDD #L%d\n", tableLabel);
    format("    PSHS D\n");
    format("    LDD #%d\n", nClauses);
    format("    PSHS D\n");
    format("    JSR switch%d\n", switchSize);
    format("    LEAS %d,S\n", switchSize + 4);
    format("    JMP 0,X\n");
    format("L%d\n", tableLabel);
    
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
            if (is16Bit) {
                format("    FDB %d\n", it.value());
            } else {
                format("    FCB %d\n", it.value());
            }
            it.setFixupIndex(nextLabelId());
            format("    FDB L%d\n", it.fixupIndex());
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
    uint16_t endLabel = nextLabelId();
    
    if (!sNode->haveDefault()) {
        format("    BRA %d\n", endLabel);
    }
    
    for (auto it = sNode->clauses().begin(); it != sNode->clauses().end(); ++it) {
        if (!it->isDefault()) {
            format("L%d\n", it->fixupIndex());
        }
        emitCode(it->stmt(), false);

        // The last clause will always have a branch of 0 so we can skip it
        if (it == sNode->clauses().end() - 1) {
            continue;
        }

        format("    LBRA L%d\n", endLabel);
    }

    format("L%d\n", endLabel);
}

void
CodeGen6809::emitCodeConditional(const ASTPtr& node, bool isLHS)
{
    auto cNode = std::static_pointer_cast<ConditionalNode>(node);

    uint16_t elseLabel = nextLabelId();
    uint16_t endLabel = nextLabelId();

    // First emit expression, send in else label
    setBranchLabel(elseLabel);
    emitCode(cNode->expr(), false);
    
    // If the branch label comes back -1, it was used and the expr did the branch to elseLabel
    
    if (branchLabel() != -1) {
        // Now emit if. If expr is false, jump to second expression, otherwise fall through to first
        if (!isReg(RegState::A)) {
            format("    PULS A\n");
            format("    TSTA\n");
        }
    
        // Make sure RegState is cleared so we don't try to push during the emitCodes
        format("    BEQ L%d\n", elseLabel);
    }
    
    // Emit the first expr
    clearRegState();
    emitCode(cNode->first(), false);
    format("    BRA L%d\n", endLabel);
    
    // Emit the second expr
    format("L%d\n", elseLabel);
    clearRegState();
    emitCode(cNode->second(), false);
    format("L%d\n", endLabel);
}

void
CodeGen6809::emitCodeLogical(const ASTPtr& node, bool isLHS)
{
    // Code generated tests lhs. if true and Lor, skip rhs and push true. If
    // false and LAnd, skip rhs and push false
    auto lNode = std::static_pointer_cast<LogicalNode>(node);
    
    int16_t labelA = nextLabelId();
    int16_t labelB = nextLabelId();
    int16_t labelPass = -1;
    int16_t labelFail = -1;
    
    if (branchLabel() != -1) {
        labelFail = branchLabel();
        setBranchLabel(-1);
    }
    
    // If no label was passed in we always do slow case
    
    // First emit lhs
    if (lNode->kind() == LogicalNode::Kind::LOr) {
        labelPass = nextLabelId();
        setBranchLabel(labelPass, true);
    } else {
        setBranchLabel(labelFail, false);
    }
    
    clearRegState();
    emitCode(lNode->lhs(), false);
    
    // If branchLabel was not cleared, do it the slow way
    if (branchLabel() != -1) {
        // If the test couldn't use the branch it has returned
        // a 0 or 1 in A (or on the stack). We need to BEQ or
        // BNE past the rhs.
        if (!isReg(RegState::A)) {
            format("    PULS A\n");
            format("    TSTA\n");
        }

        // Make sure RegState is cleared so we don't try to push during the emitCodes
        const char* op = (lNode->kind() == LogicalNode::Kind::LAnd) ? "BEQ" : "BNE";
        format("    %s L%d\n", op, labelA);
    }
    
    // Emit rhs
    setBranchLabel(labelFail, false);

    clearRegState();
    emitCode(lNode->rhs(), false);
    
    if (branchLabel() != -1) {
        // If the lhs used the branch label then on a fail it branched to
        // labelX, which is the fail label. Otherwise it did the rhs test
        // and A has the result (0 or 1).
        // emit the postamble
        format("    BRA L%d\n", labelB);
        format("L%d\n", labelA);
        format("    LDA #%d\n", (lNode->kind() == LogicalNode::Kind::LAnd) ? 0 : 1);
        format("L%d\n", labelB);
        setRegState(RegState::A);
    }
    
    if (labelPass != -1) {
        format("L%d\n", labelPass);
    }
}

void
CodeGen6809::emitCodeIndex(const ASTPtr& node, bool isLHS)
{
    auto iNode = std::static_pointer_cast<IndexNode>(node);

    emitCode(iNode->lhs(), true);
    
    // X will have the addr
    
    // Optimization. If rhs is a constant 0, we can skip the index. Just emit the lhs.
    if (iNode->rhs()->astNodeType() == ASTNodeType::Constant) {
        if (std::reinterpret_pointer_cast<ConstantNode>(iNode->rhs())->integerValue() == 0) {
            return;
        }
    }

    emitCode(iNode->rhs(), false);
    
    //Now X has the un-indexed addr and A or D has the index
    // Multiply index by the element size and add that to X

    uint16_t size = node->elementSizeInBytes();
    bool is16BitIndex = typeToOpSize(iNode->rhs()->type()) == OpSize::i16;
    bool is16BitSize = size > 255;
    
    // index can be 8 or 16 bit. We know its a valid type because the caller checked it.
    // And size can be 8 or 16 bits. If either is 16 bits we need to do a 16x16
    // multiply and get a 16 bit result. If both are 8 bit we can just do an 8x8
    // multiply and use the 16 bit result. Then we add the 16 bit result to the ref.
    if (!isReg(is16BitIndex ? RegState::D : RegState::A)) {
        format("    PULS %s\n", is16BitIndex ? "D" : "A");
    }

    if (!is16BitIndex && !is16BitSize) {
        format("    LDB #%d\n", size);
        format("    MUL\n");
    } else {
        if (!is16BitIndex) {
            // extend to 16 bits
            format("    TFR A,B\n");
            format("    CLRA\n");
        }
        format("    PSHS D\n");     // TOS is the accumulator, TOS+2 is index, size is constant
        format("    PSHS D\n");     // Push accumulator and index (value in TOS doesn't matter it will be replaced)
        format("    LDA #%d\n", uint8_t(size));
        format("    MUL\n");
        format("    STD 0,S\n");
        format("    LDA 2,S\n");
        format("    LDB #%d\n", uint8_t(size));
        format("    MUL\n");
        format("    ADDB 0,S\n");    // Toss MSB of result
        if (is16BitSize) {
            format("    LDA 3,S\n");
            format("    LDB #%d\n", uint8_t(size >> 8));
            format("    MUL\n");
            format("    ADDB 0,S\n");    // Toss MSB of result
        }
        format("    PULS D\n");
        format("    LEAS 2,S\n");
    }
    
    // D now has index * size, and X has the un-indexed addr
    format("    LEAX D,X\n");
    
    clearRegState();
    setRegState(RegState::X);

    // if isLHS is true then we're done, we have a ref in X. If not we need to DEREF
    if (!isLHS) {
        bool is16Bit = typeToOpSize(node->type()) == OpSize::i16;
        const char* reg = is16Bit ? "D" : "A";
        format("    LD%s 0,X\n", reg);
        setRegState(is16Bit ? RegState::D : RegState::A);
    }
}

void
CodeGen6809::emitCodeReturn(const ASTPtr& node, bool isLHS)
{
    auto rNode = std::static_pointer_cast<ReturnNode>(node);

    if (rNode->arg() != nullptr) {
        emitCode(rNode->arg(), false);
        
        // If for some reason the return value is on the stack rather than in A or D, we need
        // to get it into A or D. Then we want to clear the reg state because doing something
        // with the return value will happen in the caller.
        bool is16Bit = typeToOpSize(rNode->arg()->type()) == OpSize::i16;
        if (!isReg(is16Bit ? RegState::D : RegState::A)) {
            format("    PULS %s\n", is16Bit ? "D" : "A");
        }
    }
    
    clearRegState();
    
    // restoreFrame
    format("    TFR U,S\n");
    format("    PULS U\n");

    format("    RTS\n");
}

void
CodeGen6809::emitCodeDrop(const ASTPtr& node, bool isLHS)
{
    format("    LEAS %d,S\n", std::static_pointer_cast<DropNode>(node)->bytesToDrop());
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
        if (!isReg(RegState::X)) {
            format("    PULS X\n");
        }
        
        bool is16Bit = typeToOpSize(node->type()) == OpSize::i16;
        const char* reg = is16Bit ? "D" : "A";
        format("    LD%s 0,X\n", reg);
        setRegState(is16Bit ? RegState::D : RegState::A);
    }
}

void
CodeGen6809::emitCode(const ASTPtr& node, bool isLHS)
{
    emitAnnotations(node->annotationIndex(), ";");

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
