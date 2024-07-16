/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Lucid
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#pragma once

#include <vector>
#include <string>

#include "Defines.h"
#include "Module.h"
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
    Value,
    Module,
    FunctionCall,
    Enter,
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
    
    virtual ASTNodeType astNodeType() const = 0;
    virtual void addNode(const std::shared_ptr<ASTNode>&) { }
    
    virtual bool isTerminal() const { return false; }       // No children and is an operand
    virtual bool isList() const { return false; }           // Can have many children and has no operation
    virtual bool isAssignment() const { return false; }     // Is an assignment operation (needs to be a ref)
    virtual bool isUnary() const { return false; }          // Has one child, either child[0] or child[1]
    virtual Type type() const { return Type::None; }
    
    virtual const ASTPtr child(uint32_t i) const { return nullptr; }
    virtual const uint32_t numChildren() const { return 0; }
    virtual std::string toString() const { return ""; }

    virtual void addCode(std::vector<uint8_t>& code, bool isLHS) const { }
    
    // Return true if this is an signed integer or float
    bool isSigned() const
    {
        Type t = type();
        return t == Type::Int8 || t == Type::Int16 || t == Type::Int32 || t == Type::Float;
    }
};

class StatementsNode : public ASTNode
{
  public:
    virtual ASTNodeType astNodeType() const override{ return ASTNodeType::Statements; }
    
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

    virtual const uint32_t numChildren() const override { return uint32_t(_statements.size()); }

  private:
    ASTNodeList _statements;
};

class VarNode : public ASTNode
{
  public:
    VarNode(const SymbolPtr& symbol) : _symbol(symbol) { }

    virtual ASTNodeType astNodeType() const override{ return ASTNodeType::Var; }
    virtual bool isTerminal() const override { return true; }
    virtual Type type() const override { return _symbol->type(); }

    virtual std::string toString() const override { return _symbol ? _symbol->name() : ""; }

    virtual void addCode(std::vector<uint8_t>& code, bool isLHS) const override;

  private:
    SymbolPtr _symbol = nullptr;
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
    ConstantNode(Type t, float v) : _type(t), _f(v) { }
    ConstantNode(Type t, uint32_t v) : _type(t), _i(v) { }
    ConstantNode(float v) : _type(Type::Float), _numeric(true), _f(v) { }

    virtual ASTNodeType astNodeType() const override{ return ASTNodeType::Constant; }
    virtual bool isTerminal() const override { return true; }
    virtual Type type() const override { return _type; }

    virtual std::string toString() const override
    {
        // FIXME: Need to handle Float, etc.
        if (_numeric) {
            return std::to_string(_i);
        }
        return "";
    }

    virtual void addCode(std::vector<uint8_t>& code, bool isLHS) const override;
    
    void toFloat() { _f = float(_i); }
    void toUInt() { _i = uint32_t(_f); }
    void setType(Type type) { _type = type; }

  private:
    Type _type = Type::None;
    bool _numeric = false; // If true, type is a Float or UInt32 literal
    union {
        float _f;
        int32_t _i;
    };
};

// String constant
class StringNode : public ASTNode
{
  public:
    StringNode(const char* s) : _string(s) { }
    StringNode(const std::string& s) : _string(s) { }
    StringNode(char c) { _string = c; }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Constant; }
    virtual bool isTerminal() const override { return true; }
    virtual Type type() const override { return Type::String; }

    virtual std::string toString() const override { return _string; }

    virtual void addCode(std::vector<uint8_t>& code, bool isLHS) const override;

  private:
    std::string _string;
};

static Op castToOp(Type type)
{
    switch (type) {
        case Type::Float: return Op::TOF;
        case Type::UInt8: return Op::TOU8;
        case Type::Int8: return Op::TOI8;
        case Type::UInt16: return Op::TOU16;
        case Type::Int16: return Op::TOI16;
        case Type::UInt32: return Op::TOU32;
        case Type::Int32: return Op::TOI32;
        default: return Op::NOP;
    }
}

class OpNode : public ASTNode
{
  public:
    OpNode(const std::shared_ptr<ASTNode>& left, Op op, const std::shared_ptr<ASTNode>& right, bool isAssignment)
        : _isAssignment(isAssignment)
        , _op(op)
        , _left(left)
        , _right(right)
    {
        _type = _left->type();
        
        // If types don't match, add a cast operator
        if (_right->type() != _type) {
            // If _right is a constant just change its type
            if (_right->astNodeType() == ASTNodeType::Constant) {
                ConstantNode* rightNode = reinterpret_cast<ConstantNode*>(_right.get());
                if (_type == Type::Float) {
                    // Value must be an unsigned int
                    rightNode->toFloat();
                } else if (rightNode->type() == Type::Float) {
                    // Convert value to int
                    rightNode->toUInt();
                }
                rightNode->setType(_type);

            } else {
                Op castTo = castToOp(_type);
                _right = std::make_shared<OpNode>(Op(uint8_t(castTo) | typeToSizeBits(_right->type())), _right);
            }
        }
    }
    
    OpNode(const std::shared_ptr<ASTNode>& left, Op op)
        : _op(op)
        , _left(left)
    {
        _type = _left->type();
    }
    
    OpNode(Op op, const std::shared_ptr<ASTNode>& right)
        : _op(op)
        , _right(right)
    {
        _type = _right->type();
    }
    
    OpNode(Op op) : _op(op) { }
    
    virtual ASTNodeType astNodeType() const override{ return ASTNodeType::Op; }

    virtual bool isAssignment() const override { return _isAssignment; }
    virtual bool isUnary() const override { return _left == nullptr || _right == nullptr; }
    virtual Type type() const override { return _type; }
    
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

    virtual void addCode(std::vector<uint8_t>& code, bool isLHS) const override;
    
    Op op() const { return _op; }

  private:
    bool _isAssignment = false;
    Op _op;
    Type _type;
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
    
    virtual ASTNodeType astNodeType() const override{ return ASTNodeType::Dot; }

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

    virtual void addCode(std::vector<uint8_t>& code, bool isLHS) const override;

  private:
    std::shared_ptr<ASTNode> _operand;
    std::shared_ptr<ASTNode> _property;
};

class ModuleNode : public ASTNode
{
  public:
    ModuleNode(const ModulePtr& module) : _module(module) { }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Module; }
    virtual bool isList() const override { return true; }
    virtual bool isTerminal() const override { return true; }
    
    const ModulePtr& module() const { return _module; }

  private:
    ModulePtr _module;
};

class FunctionCallNode : public ASTNode
{
  public:
    FunctionCallNode(Function* func) : _function(func) { }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::FunctionCall; }
    virtual bool isList() const override { return true; }
    virtual Type type() const override { return _function ? _function->returnType() : Type::None; }

    virtual void addNode(const std::shared_ptr<ASTNode>& node) override
    {
        _args.push_back(node);
    }
    
    virtual const ASTPtr child(uint32_t i) const override
    {
        // _args has the list of arguments in order from left to right. But stack
        // pushes down, so args will appear in reverse order on the stack
        // (right most arg will be at lowest addr). So return children in reverse
        // order so the first arg is at the lowest address when pushed
        if (_args.size() <= i) {
            return nullptr;
        }
        return _args[_args.size() - i - 1];
    }

    virtual const uint32_t numChildren() const override { return uint32_t(_args.size()); }

    virtual void addCode(std::vector<uint8_t>& code, bool isLHS) const override;

    Function* function() const { return _function; }
    
  private:
    Function* _function;
    ASTNodeList _args;

};

class EnterNode : public ASTNode
{
  public:
    EnterNode(uint32_t localSize) : _localSize(localSize) { }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Enter; }

    virtual void addCode(std::vector<uint8_t>& code, bool isLHS) const override;
    
  private:
    uint32_t _localSize = 0;
};

}
