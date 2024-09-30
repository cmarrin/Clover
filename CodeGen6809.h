/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#pragma once

#include "CodeGen.h"

namespace clvr {

class CodeGen6809 : public CodeGen
{
  public:
    CodeGen6809(Annotations* annotations) : CodeGen(annotations) { }
    
    virtual ~CodeGen6809() { }
    
    virtual void init() override
    {
        CodeGen::init();
        _strings.clear();
        _labelId = 1; // Start at 1 so we can use 0 as a "NoId" indicator
    }

    virtual uint16_t majorVersion() const override { return 1; }
    virtual uint8_t minorVersion() const override { return 0; }

    virtual uint8_t passesNeeded() const override { return 1; }

    virtual void emitPreamble(const Compiler*) override;
    virtual void emitPostamble(const Compiler*) override;

    virtual void handleFunction(const Compiler*, const FunctionPtr&, const StructPtr&, bool isTopLevel) override;

    virtual void emitCode(const ASTPtr& node, bool isLHS) override;
    
  private:
    enum class RegState { None, A, D, X, StackI8, StackI16, StackPtr };

    void emitAddr(const SymbolPtr&, AddrNativeType offset);

    void stashRegIfNeeded()
    {
        if (_lastRegState == RegState::A) {
            format("    PSHS A\n");
            _lastRegState = RegState::StackI8;
        } else if (_lastRegState == RegState::D) {
            format("    PSHS D\n");
            _lastRegState = RegState::StackI16;
        }
    }
    
    void stashPtrIfNeeded()
    {
        if (_lastPtrState == RegState::X) {
            format("    PSHS X\n");
            _lastPtrState = RegState::StackPtr;
        }
    }

    // This method returns true if the requested reg is active, false if it's on the stack and throws if neither.
    bool isReg(RegState reg) const
    {
        // reg must be A or D or X
        if (reg == RegState::A) {
            if (_lastRegState == RegState::A) {
                return true;
            }
            expectRegState(RegState::StackI8);
            return false;
        }

        if (reg == RegState::D) {
            if (_lastRegState == RegState::D) {
                return true;
            }
            //expectRegState(RegState::StackI16);
            return false;
        }
        
        if (reg == RegState::X) {
            if (_lastPtrState == RegState::X) {
                return true;
            }
            expectPtrState(RegState::StackPtr);
            return false;
        }
        
        throw true;
    }
    
    void expectRegState(RegState state) const
    {
        if (_lastRegState != state) {
            throw true;
        }
    }
    
    void expectPtrState(RegState state) const
    {
        if (_lastPtrState != state) {
            throw true;
        }
    }
    
    void setRegState(RegState state)
    {
        if (state == RegState::X || state == RegState::StackPtr) {
            _lastPtrState = state;
        } else {
            _lastRegState = state;
        }
    }
    
    void clearRegState() { _lastRegState = RegState::None; _lastPtrState = RegState::None; }
    
    uint16_t nextLabelId() { return _labelId++; }
    int16_t branchLabel() const { return _branchLabel; }
    bool branchTestInverted() const { return _branchTestInverted; }
    void setBranchLabel(int16_t label, bool invert = false) { _branchLabel = label; _branchTestInverted = invert; }

    uint16_t getLabelId(const ASTPtr& node)
    {
        auto bNode = std::static_pointer_cast<BranchNode>(node);
        if (bNode->fixupIndex() == 0) {
            bNode->setFixupIndex(nextLabelId());
        }
        return bNode->fixupIndex();
    }

    // This assumes op is one of:
    //
    //      Op::ADD
    //      Op::SUB
    //      Op::AND1
    //      Op::OR1
    //      Op::XOR1
    //
    // and that the lhs and rhs are pushed. Leave result on TOS
    void emitBinaryOp(const ASTPtr& node);
    void emitMulOp(const ASTPtr& node);

    void emitCodeStatements(const ASTPtr& node, bool isLHS);
    void emitCodeVar(const ASTPtr& node, bool isLHS);
    void emitCodeVar(const ASTPtr& node, Type type, bool isLHS);
    void emitPopCodeVar(const ASTPtr& node);
    void emitCodeVar(const ASTPtr& node, Type type, bool ref, bool pop);
    void emitCodeConstant(const ASTPtr& node, bool isLHS);
    void emitCodeString(const ASTPtr& node, bool isLHS);
    void emitCodeOp(const ASTPtr& node, bool isLHS);
    void emitCodeInc(const ASTPtr& node, bool isLHS);
    void emitCodeAssignment(const ASTPtr& node, bool isLHS);
    void emitCodeDot(const ASTPtr& node, bool isLHS);
    void emitCodeModule(const ASTPtr& node, bool isLHS);
    void emitCodeFunctionCall(const ASTPtr& node, bool isLHS);
    void emitCodeEnter(const ASTPtr& node, bool isLHS);
    void emitCodeTypeCast(const ASTPtr& node, bool isLHS);
    void emitCodeBranch(const ASTPtr& node, bool isLHS);
    void emitCodeSwitch(const ASTPtr& node, bool isLHS);
    void emitCodeConditional(const ASTPtr& node, bool isLHS);
    void emitCodeLogical(const ASTPtr& node, bool isLHS);
    void emitCodeIndex(const ASTPtr& node, bool isLHS);
    void emitCodeReturn(const ASTPtr& node, bool isLHS);
    void emitCodeDrop(const ASTPtr& node, bool isLHS);
    void emitCodeRef(const ASTPtr& node, bool isLHS);
    void emitCodeDeref(const ASTPtr& node, bool isLHS);
    
    // Strings will go after the code in a series of FCB entries.
    // The first entry will have a label and all string addresses
    // will be relative to this. Any ASCII character (0x20-0x7e)
    // are emitted as characters, prefixed with a single quote.
    // All other characters are emitted as hex.
    static constexpr const char* StringLabel = "String";
    std::vector<std::string> _strings;
    uint32_t _stringSize = 0;
    
    // Unique identifier for labels
    uint16_t _labelId = 1; // Start at 1 so we can use 0 as a "NoId" indicator
    
    // Remember how the last emitNode left the regs
    RegState _lastRegState = RegState::None;
    RegState _lastPtrState = RegState::None;
    
    // Pass a label around for optimizing relational, logical and conditional operations
    // When not -1 a target for a branch (like if or a conditional op) will add the passed
    // label to use when the relational operation is false.
    int16_t _branchLabel = -1;
    
    // Normally the branch optimization branches to branchLabel when the test
    // fails. When _invertBranchTest is true we branch if true instead.
    bool _branchTestInverted = false;
};

}
