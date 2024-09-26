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
    
    uint16_t nextLabelId() { return _labelId++; }

  private:
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
    
    // This assumes op is one of:
    //
    //      Op::ADD
    //      Op::SUB
    //      Op::IMUL/Op::UMUL
    //      Op::IDIV/Op::UDIV
    //      Op::AND1
    //      Op::OR1
    //      Op::XOR1
    //
    // and that the lhs and rhs are pushed. Leave result on TOS
    void emitBinaryOp(Op op, bool is16Bit);

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
    enum class RegState { None, A, D, X, StackI8, StackI16, StackPtr };
    
    RegState _lastRegState = RegState::None;
    RegState _lastPtrState = RegState::None;
};

}
