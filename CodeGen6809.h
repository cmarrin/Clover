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
    virtual ~CodeGen6809() { }
    
    virtual uint16_t majorVersion() const override { return 1; }
    virtual uint8_t minorVersion() const override { return 0; }

    virtual void emitPreamble(const Compiler*) override;

    virtual void emitCode(const ASTPtr& node, bool isLHS) override;
    
    uint32_t nextLabelId() { return _labelId++; }

  private:
    void emitAddr(const SymbolPtr&, AddrNativeType offset, bool is32BitLSB = false);

    void emitCodeStatements(const ASTPtr& node, bool isLHS);
    void emitCodeVar(const ASTPtr& node, bool isLHS);
    void emitCodeVar(const ASTPtr& node, Type type, bool isLHS);
    void emitPopCodeVar(const ASTPtr& node);
    void emitCodeVar(const ASTPtr& node, Type type, bool ref, bool pop);
    void emitCodeConstant(const ASTPtr& node, bool isLHS);
    void emitCodeString(const ASTPtr& node, bool isLHS);
    void emitCodeOp(const ASTPtr& node, bool isLHS);
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
    std::string _strings;
    
    // Unique identifier for labels
    uint32_t _labelId = 0;
};

}
