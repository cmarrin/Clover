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
    TypeCast,
    Branch,
};

class ASTNode;
class Compiler;
class Symbol;

using ASTPtr = std::shared_ptr<ASTNode>;
using ASTNodeList = std::vector<ASTPtr>;

class ASTNode
{
  public:
    ASTNode(int32_t annotationIndex) : _annotationIndex(annotationIndex) { }
    virtual ~ASTNode() { }
    
    virtual ASTNodeType astNodeType() const = 0;
    virtual void addNode(const std::shared_ptr<ASTNode>&) { }
    
    virtual Type type() const { return Type::None; }
    
    virtual const ASTPtr child(uint32_t i) const { return nullptr; }
    virtual const uint32_t numChildren() const { return 0; }
    virtual std::string toString() const { return ""; }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler* c) { }
    
    int32_t annotationIndex() const { return _annotationIndex; }
    
    // Return true if this is an signed integer or float
    bool isSigned() const
    {
        Type t = type();
        return t == Type::Int8 || t == Type::Int16 || t == Type::Int32 || t == Type::Float;
    }
    
  protected:
      int32_t _annotationIndex = -1;
};

class StatementsNode : public ASTNode
{
  public:
    StatementsNode(int32_t annotationIndex) : ASTNode(annotationIndex) { }
    
    virtual ASTNodeType astNodeType() const override{ return ASTNodeType::Statements; }
    
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

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler*) override;

  private:
    ASTNodeList _statements;
};

class VarNode : public ASTNode
{
  public:
    VarNode(const SymbolPtr& symbol, int32_t annotationIndex) : ASTNode(annotationIndex), _symbol(symbol) { }

    virtual ASTNodeType astNodeType() const override{ return ASTNodeType::Var; }
    virtual Type type() const override { return _symbol->type(); }

    virtual std::string toString() const override { return _symbol ? _symbol->name() : ""; }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler*) override;

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
    ConstantNode(Type t, uint32_t v, int32_t annotationIndex) : ASTNode(annotationIndex), _type(t), _i(v) { }
    ConstantNode(float v, int32_t annotationIndex)  : ASTNode(annotationIndex), _type(Type::Float), _numeric(true), _f(v) { }

    virtual ASTNodeType astNodeType() const override{ return ASTNodeType::Constant; }
    virtual Type type() const override { return _type; }

    virtual std::string toString() const override
    {
        // FIXME: Need to handle Float, etc.
        if (_numeric) {
            return std::to_string(_i);
        }
        return "";
    }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler*) override;
    
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
    StringNode(const char* s, int32_t annotationIndex) : ASTNode(annotationIndex), _string(s) { }
    StringNode(const std::string& s, int32_t annotationIndex) : ASTNode(annotationIndex), _string(s) { }
    StringNode(char c, int32_t annotationIndex) : ASTNode(annotationIndex) { _string = c; }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Constant; }
    virtual Type type() const override { return Type::String; }

    virtual std::string toString() const override { return _string; }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler*) override;

  private:
    std::string _string;
};

class TypeCastNode : public ASTNode
{
  public:
    TypeCastNode(Type t, const std::shared_ptr<ASTNode>& arg, int32_t annotationIndex) : ASTNode(annotationIndex), _type(t), _arg(arg) { }

    virtual Type type() const override { return _type; }
    
    virtual ASTNodeType astNodeType() const override { return ASTNodeType::TypeCast; }

    virtual const ASTPtr child(uint32_t i) const override
    {
        if (i == 0) {
            return _arg;
        }
        return nullptr;
    }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler*) override;
    
    static ASTPtr castIfNeeded(ASTPtr& node, Type neededType, int32_t annotationIndex);
    
  private:
    Type _type;
    ASTPtr _arg;
};

class OpNode : public ASTNode
{
  public:
    OpNode(const std::shared_ptr<ASTNode>& left, Op op, const std::shared_ptr<ASTNode>& right, bool isAssignment, int32_t annotationIndex)
        : ASTNode(annotationIndex)
        , _isAssignment(isAssignment)
        , _op(op)
        , _left(left)
        , _right(right)
    {
        _type = _left->type();
        _right = TypeCastNode::castIfNeeded(_right, _left->type(), annotationIndex);
    }
    
    OpNode(const std::shared_ptr<ASTNode>& left, Op op, int32_t annotationIndex)
        : ASTNode(annotationIndex)
        , _op(op)
        , _left(left)
    {
        assert(op != Op::NOP);
        _type = _left->type();
    }
    
    OpNode(Op op, const std::shared_ptr<ASTNode>& right, int32_t annotationIndex)
        : ASTNode(annotationIndex)
        , _op(op)
        , _right(right)
    {
        assert(op != Op::NOP);
        _type = _right->type();
    }
    
    OpNode(Op op, int32_t annotationIndex) : ASTNode(annotationIndex), _op(op) { }
    
    virtual ASTNodeType astNodeType() const override{ return ASTNodeType::Op; }

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

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler*) override;
    
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
    DotNode(const std::shared_ptr<ASTNode>& operand, const std::string& id, int32_t annotationIndex)
        : ASTNode(annotationIndex)
        , _operand(operand)
    {
        // Create a StringNode for the id, for consistency
        _property = std::make_shared<StringNode>(id, annotationIndex);
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

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler*) override;

  private:
    ASTPtr _operand;
    ASTPtr _property;
};

class ModuleNode : public ASTNode
{
  public:
    ModuleNode(const ModulePtr& module, int32_t annotationIndex) : ASTNode(annotationIndex), _module(module) { }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Module; }
    
    const ModulePtr& module() const { return _module; }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler*) override;

  private:
    ModulePtr _module;
};

class FunctionCallNode : public ASTNode
{
  public:
    FunctionCallNode(FunctionPtr func, int32_t annotationIndex) : ASTNode(annotationIndex), _function(func) { }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::FunctionCall; }
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

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler*) override;

    FunctionPtr function() const { return _function; }
    
  private:
    FunctionPtr _function;
    ASTNodeList _args;

};

class EnterNode : public ASTNode
{
  public:
    EnterNode(const FunctionPtr& function, int32_t annotationIndex) : ASTNode(annotationIndex), _function(function) { }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Enter; }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler*) override;
    
  private:
    FunctionPtr _function;
};

class BranchNode : public ASTNode
{
  public:
    enum class Kind { IfStart, ElseStart, IfEnd, LoopStart, LoopNext, LoopEnd, Break, Continue };
    enum class Size { None, Short, Byte, Long };
    
    BranchNode(Kind k, int32_t annotationIndex) : ASTNode(annotationIndex), _kind(k) { }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Branch; }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS, Compiler*) override;
    
    void setFixupNode(const ASTPtr& f) { _fixupNode = f; }
    
    // Address of where this node should branch to
    void fixup(std::vector<uint8_t>& code, AddrNativeType addr);
    
  private:
    Kind _kind;
    Size _size = Size::Long;
    ASTPtr _fixupNode;
    AddrNativeType _fixupIndex = 0;
};
}
