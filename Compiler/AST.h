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
#include "Symbol.h"

namespace lucid {

// Classes to represent AST

enum class ASTNodeType {
    Statements,
    Op,
    Var,
    Constant,
    String,
    Dot,
};

class ASTNode;
class Symbol;

using ASTPtr = std::shared_ptr<ASTNode>;
using ASTNodeList = std::vector<ASTPtr>;

class ASTNode
{
  public:
    ASTNode() { }
    virtual ~ASTNode() { }
    
    virtual ASTNodeType type() const = 0;
    virtual void addNode(const std::shared_ptr<ASTNode>&) { }
    virtual bool isTerminal() const { return false; }
    
    // isList is true if this node can have more than two children and the node itself has not operation
    virtual bool isList() const { return false; }
    virtual const ASTPtr child(uint32_t i) const { return nullptr; }
    virtual std::string toString() const { return ""; }
};

class StatementsNode : public ASTNode
{
  public:
    virtual ASTNodeType type() const override{ return ASTNodeType::Statements; }
    
    virtual bool isList() const override { return true; }

    virtual void addNode(const std::shared_ptr<ASTNode>& node) override
    {
        _statements.push_back(node);
    }
    
    virtual const ASTPtr child(uint32_t i) const override
    {
        if (_statements.size() <= i) {
            return nullptr;
        }
        return _statements[i];
    }

  private:
    ASTNodeList _statements;
};

class VarNode : public ASTNode
{
  public:
    VarNode(Symbol* symbol) : _symbol(symbol) { }

    virtual ASTNodeType type() const override{ return ASTNodeType::Var; }
    virtual bool isTerminal() const override { return true; }

    virtual std::string toString() const override { return _symbol ? _symbol->name() : ""; }

  private:
    Symbol* _symbol = nullptr;
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
    virtual bool isTerminal() const override { return true; }

    virtual std::string toString() const override
    {
        // FIXME: Need to handle Float, etc.
        if (_numeric) {
            return std::to_string(_i);
        }
        return "";
    }

  private:
    Type _t = Type::None;
    bool _numeric = false; // If true, type is Float or UInt32
    union {
        float _f;
        uint32_t _i;
    };
};

// String constant
class StringNode : public ASTNode
{
  public:
    StringNode(const char* s) : _string(s) { }
    StringNode(const std::string& s) : _string(s) { }
    StringNode(char c) { _string = c; }

    virtual ASTNodeType type() const override { return ASTNodeType::Constant; }
    virtual bool isTerminal() const override { return true; }

    virtual std::string toString() const override { return _string; }

  private:
    std::string _string;
};

class OpNode : public ASTNode
{
  public:
    OpNode(const std::shared_ptr<ASTNode>& left, Operator op, const std::shared_ptr<ASTNode>& right)
        : _op(op)
        , _left(left)
        , _right(right)
    { }
    
    OpNode(const std::shared_ptr<ASTNode>& left, Operator op)
        : _op(op)
        , _left(left)
    { }
    
    OpNode(Operator op, const std::shared_ptr<ASTNode>& right)
        : _op(op)
        , _right(right)
    { }
    
    virtual ASTNodeType type() const override{ return ASTNodeType::Op; }

    virtual const ASTPtr child(uint32_t i) const override
    {
        if (i == 0) {
            return _left;
        }
        if (i == 1) {
            return _right;
        }
        return nullptr;
    }

    // FIXME: this only works for single character operators
    virtual std::string toString() const override { return std::string(1, char(_op)); }

  private:
    Operator _op;
    std::shared_ptr<ASTNode> _left, _right;
};

class DotNode : public ASTNode
{
  public:
    DotNode(const std::shared_ptr<ASTNode>& operand, const std::string& id)
        : _operand(operand)
    {
        // Create a StringNode for the id, for consistency
        _property = std::make_shared<StringNode>(id);
    }
    
    virtual ASTNodeType type() const override{ return ASTNodeType::Dot; }

    virtual const ASTPtr child(uint32_t i) const override
    {
        if (i == 0) {
            return _operand;
        }
        if (i == 1) {
            return _property;
        }
        return nullptr;
    }

    virtual std::string toString() const override { return "."; }

  private:
    std::shared_ptr<ASTNode> _operand;
    std::shared_ptr<ASTNode> _property;
};

}
