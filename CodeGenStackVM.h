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

/*
    Executable file format
    
        Bytes (16 bit addr)     Bytes (32 bit addr)     Description
        
        0-3                     0-3                     : Signature - 'lucd'
        4-5                     4-5                     : Machine code format - major
        6                       6                       ; Machine code format - minor
        7                       7                       ; Address size (0 - 16 bit, 1 - 32 bit)
        8-9                     8-11                    : Entry point of 'main' function, if any (0 if not)
        10-11                   12-15                   : Location of constructor function of top level struct
        12-13                   16-19                   : Bytes of storage needed for top-level struct
        14-15                   20-21                   : Size of constants in bytes
        16-n                    22-n                    : Bytes of constant structs and arrays
        n-<end>                 n-<end>                 : Executable code
 */
 
class CodeGenStackVM : public CodeGen
{
  public:
    CodeGenStackVM(Annotations* annotations) : CodeGen(annotations) { }

    virtual ~CodeGenStackVM() { }
    
    virtual uint16_t majorVersion() const override { return 1; }
    virtual uint8_t minorVersion() const override { return 0; }

    // Do 3 code generation:
    //
    //      1)  On the first pass we don't know how long most of the branch
    //          instructions need to be so we make them long. But as we fill
    //          in the jump addresses we remember which branches can be short
    //
    //      2)  Now go back for a second pass to shorten the ones that have
    //          been marked as such.
    //
    //      3)  But we're not done. If we have forward function declarations
    //          their addresses will have been set from the previous pass
    //          and if we've shortened any branches they will be wrong so
    //          we need a third pass to make them correct
    //
    virtual uint8_t passesNeeded() const override { return 3; }

    virtual void emitPreamble(const Compiler*) override;
    virtual void handleFunction(const Compiler*, const FunctionPtr&, const StructPtr&, bool isTopLevel) override;

    virtual void emitCode(const ASTPtr& node, bool isLHS) override;

  private:
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
};

}
