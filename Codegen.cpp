/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Lucid
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "Codegen.h"

#include "Compiler.h"

#include <assert.h>

using namespace lucid;

Codegen::Codegen(std::vector<uint8_t>* code)
    : _code(code)
{
    // Write signature
    _code->push_back('l');
    _code->push_back('u');
    _code->push_back('c');
    _code->push_back('d');
    
    // Write dummy entry point address, to be filled in later
    _code->push_back(0);
    _code->push_back(0);
    _code->push_back(0);
    _code->push_back(0);
}

bool
Codegen::processAST(const ASTPtr& ast, Compiler* c)
{
    return processNextASTNode(ast, false, c);
}

void
Codegen::setEntryPoint()
{
    uint32_t cur = uint32_t(_code->size());
    _code->at(4) = cur >> 24;
    _code->at(5) = cur >> 16;
    _code->at(6) = cur >> 8;
    _code->at(7) = cur;
}


bool
Codegen::processNextASTNode(const ASTPtr& ast, bool isLHS, Compiler* c)
{
    // Traverse the AST and use the expression stack to generate the code
    if (ast->isTerminal()) {
        c->setAnnotation(ast->annotationIndex(), uint32_t(_code->size()));
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
            if (!processNextASTNode(child, false, c)) {
                return false;
            }
        }
        
        // A node with lists can be a function, so it could do an operation
        c->setAnnotation(ast->annotationIndex(), uint32_t(_code->size()));
        ast->addCode(*_code, false);
        
        return true;
    }
    
    // This is a node that performs an operation, process it's operands first
    if (ast->child(0)) {
        processNextASTNode(ast->child(0), ast->isAssignment(), c);
    }
    
    // FIXME: We need to distinguish between pre and post unary operations
    if (ast->child(1)) {
        processNextASTNode(ast->child(1), false, c);
    }
    
    c->setAnnotation(ast->annotationIndex(), uint32_t(_code->size()));
    ast->addCode(*_code, false);
     
    return true;
}

