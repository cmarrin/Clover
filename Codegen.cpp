/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Lucid
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "Codegen.h"

using namespace lucid;

bool
Codegen::processAST(const ASTPtr& ast)
{
    return processNextASTNode(ast, false);
}

bool
Codegen::processNextASTNode(const ASTPtr& ast, bool isLHS)
{
    // Traverse the AST and use the expression stack to generate the code
    if (ast->isTerminal()) {
        ast->addCode(*_code, isLHS);
        return true;
    }

    if (ast->isList()) {
        assert(!isLHS);
        
        // Just walk all the children. There can be no null child, seeing a null stops the iteration
        for (uint32_t i = 0; ; ++i) {
            ASTPtr child = ast->child(i);
            if (!child) {
                break;
            }
            if (!processNextASTNode(child, false)) {
                return false;
            }
        }
        
        // A node with lists can be a function, so it could do an operation
        ast->addCode(*_code, false);
        
        return true;
    }
    
    // This is a node that performs an operation, process it's operands first
    if (ast->child(0)) {
        processNextASTNode(ast->child(0), ast->isAssignment());
    }
    
    // FIXME: We need to distinguish between pre and post unary operations
    if (ast->child(1)) {
        processNextASTNode(ast->child(1), false);
    }
    
    ast->addCode(*_code, false);
     
    return true;
}

