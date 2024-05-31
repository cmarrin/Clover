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

#include "Defines.h"

namespace lucid {

// Classes to represent AST

enum class ASTNodeType {
    Statements,
    BinaryOp,
    UnaryOp,
    Var,
    Constant,
    Dot,
};

class ASTNode;

using ASTPtr = std::shared_ptr<ASTNode>;
using ASTNodeList = std::vector<ASTPtr>;

class ASTNode
{
  public:
    ASTNode() { }
    virtual ~ASTNode() { }
    
    virtual ASTNodeType type() const = 0;
    virtual void addNode(const std::shared_ptr<ASTNode>&) { }
};

class StatementsNode : public ASTNode
{
  public:
    virtual ASTNodeType type() const override{ return ASTNodeType::Statements; }
    
    virtual void addNode(const std::shared_ptr<ASTNode>& node) override
    {
        _statements.push_back(node);
    }
    
  private:
    ASTNodeList _statements;
};

class VarNode : public ASTNode
{
  public:
    VarNode(uint32_t symbolIndex) : _symbolIndex(symbolIndex) { }

    virtual ASTNodeType type() const override{ return ASTNodeType::Var; }

  private:
    uint32_t _symbolIndex = 0;
};

// This can hold a numeric float, int or a constant. Constants are
// strongly typed and can't be implicitly cast. A numeric float
// can be implicitly cast to a fixed, but not to an int. A numeric
// int can be implicitly cast to any signed or unsigned int type
// and to a float or fixed. If the value is a constant
// (_numeric == false) if the type is a signed integer the _i
// value is cast to a signed type to get the correct value.
//
class ConstantNode : public ASTNode
{
  public:
    ConstantNode(Type t, float v) : _t(t), _f(v) { }
    ConstantNode(Type t, uint32_t v) : _t(t), _i(v) { }
    ConstantNode(float v) : _numeric(true), _f(v) { }
    ConstantNode(uint32_t v) : _numeric(true), _i(v) { }

    virtual ASTNodeType type() const override{ return ASTNodeType::Constant; }

  private:
    Type _t;
    bool _numeric = false; // If true, type is Float or UInt32
    union {
        float _f;
        uint32_t _i;
    };
};

class BinaryOpNode : public ASTNode
{
  public:
    BinaryOpNode(Operator op, const std::shared_ptr<ASTNode>& left, const std::shared_ptr<ASTNode>& right)
        : _op(op)
        , _left(left)
        , _right(right)
    { }
    
    virtual ASTNodeType type() const override{ return ASTNodeType::BinaryOp; }

  private:
    Operator _op;
    std::shared_ptr<ASTNode> _left, _right;
};

class UnaryOpNode : public ASTNode
{
  public:
    UnaryOpNode(Operator op, const std::shared_ptr<ASTNode>& operand)
        : _op(op)
        , _operand(operand)
    { }
    
    virtual ASTNodeType type() const override{ return ASTNodeType::UnaryOp; }

  private:
    Operator _op;
    std::shared_ptr<ASTNode> _operand;
};

class DotNode : public ASTNode
{
  public:
    DotNode(const std::shared_ptr<ASTNode>& operand, const std::string& id)
        : _operand(operand)
        , _id(id)
    { }
    
    virtual ASTNodeType type() const override{ return ASTNodeType::UnaryOp; }

  private:
    std::shared_ptr<ASTNode> _operand;
    std::string _id;
};

}
