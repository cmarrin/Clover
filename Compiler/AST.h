/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

// Clover compiler
//
// A simple imperative language which generates code that can be 
// executed by the Interpreter
//

#pragma once

#include <memory>
#include <vector>
#include <string>

namespace lucid {

// Classes to represent AST

enum class ASTNodeType {
    Program,
    Import,
    Struct,
    StructInstance,
};

class ASTNode;

using ASTNodeList = std::vector<std::shared_ptr<ASTNode>>;

class ASTNode
{
  public:
    ASTNode() { }
    virtual ~ASTNode() { }
    
    virtual ASTNodeType type() const = 0;
    virtual bool addNode(const std::shared_ptr<ASTNode>&) = 0;
};

class ProgramNode : public ASTNode
{
  public:
    virtual ASTNodeType type() const override{ return ASTNodeType::Program; }
    
    virtual bool addNode(const std::shared_ptr<ASTNode>& node) override
    {
        // Imports then structs then the struct instance
        if (node->type() == ASTNodeType::Import) {
            if (!_structs.empty() || _haveStructInstance) {
                return false;
            }
            _imports.push_back(node);
            return true;
        }
        
        if (node->type() == ASTNodeType::Struct) {
            if (_haveStructInstance) {
                return false;
            }
            _structs.push_back(node);
            return true;
        }
        
         if (node->type() == ASTNodeType::StructInstance) {
            if (_haveStructInstance) {
                return false;
            }
            _instance = node;
            _haveStructInstance = true;
            return true;
        }
        return false;
    }
    
  private:
    ASTNodeList _imports;
    ASTNodeList _structs;
    std::shared_ptr<ASTNode> _instance;
    bool _haveStructInstance = false;
};

class ImportNode : public ASTNode
{
  public:
    ImportNode(const std::string& id, const std::string& idAs) : _id(id), _idAs(idAs) { }
    
    virtual ASTNodeType type() const override{ return ASTNodeType::Import; }

    // Nothing to add to import
    virtual bool addNode(const std::shared_ptr<ASTNode>& node) override { return false; }

  private:
    std::string _id, _idAs;
};

}
