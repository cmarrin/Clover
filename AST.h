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

#include <assert.h>

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
    Switch,
    Conditional,
    Logical,
    Index,
    Return,
    Assignment,
    Drop,
    Ref,
    Deref,
    Initializer,
};

class ASTNode;
class Compiler;
class Symbol;

static inline void appendValue(std::vector<uint8_t>& container, uint32_t v, uint8_t bytes)
{
    switch (bytes) {
        case 4: container.push_back(v >> 24);
                container.push_back(v >> 16);
        case 2: container.push_back(v >> 8);
        case 1: container.push_back(v);
    }
}

class ASTNode
{
  public:
    virtual ~ASTNode() { }
    
    virtual ASTNodeType astNodeType() const = 0;
    virtual void addNode(const ASTPtr&) { }
    
    virtual Type type() const { return Type::None; }
    
    virtual bool isIndexable() const { return false; }
    virtual bool isAssignable() const { return false; }
    virtual bool isPointer() const { return false; }
    
    virtual bool valueLeftOnStack() const { return false; }
    
    // sizeInBytes is usually the same as typeToBytes(type). But if the
    // node is a ptr to the type then it will be different.
    virtual uint16_t elementSizeInBytes() const { return typeToBytes(type()); }
    
    virtual const ASTPtr child(uint32_t i) const { return nullptr; }
    virtual const uint32_t numChildren() const { return 0; }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) { }
    
    int32_t annotationIndex() const { return _annotationIndex; }
    
    // Return true if this is an signed integer or float
    bool isSigned() const
    {
        Type t = type();
        return t == Type::Int8 || t == Type::Int16 || t == Type::Int32 || t == Type::Float;
    }
    
    void setAnnotationIndex(int32_t index) { _annotationIndex = index; }
    void setAnnotationAddr(AddrNativeType addr)
    {
        if (_annotationAddr == -1) {
            _annotationAddr = addr;
        }
    }
    
    void annotation(int32_t& index, int32_t& addr) { index = _annotationIndex; addr = _annotationAddr; }
    
  private:
      int32_t _annotationIndex = -1;    // Source code line number
      int32_t _annotationAddr = -1;     // Executable address
};

class StatementsNode : public ASTNode
{
  public:    
    virtual ASTNodeType astNodeType() const override{ return ASTNodeType::Statements; }
    
    virtual void addNode(const ASTPtr& node) override
    {
        assert(node != nullptr);
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

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) override;

  private:
    ASTNodeList _statements;
};

class VarNode : public ASTNode
{
  public:
    VarNode(const SymbolPtr& symbol) : _symbol(symbol) { }

    virtual ASTNodeType astNodeType() const override{ return ASTNodeType::Var; }
    virtual Type type() const override { return _symbol->type(); }
    virtual uint16_t elementSizeInBytes() const override { return _symbol->elementSizeInBytes(); }

    virtual bool isIndexable() const override { return _symbol->nElements() != 1; }
    virtual bool isAssignable() const override { return true; }

    virtual bool isPointer() const override { return _symbol->isPointer(); }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) override
    {
        emitCode(code, Type::None, isLHS, false);
    }
    
    void emitCode(std::vector<uint8_t>& code, Type type, bool isLHS)
    {
        emitCode(code, type, isLHS, false);
    }
    
    void emitPopCode(std::vector<uint8_t>& code)
    {
        emitCode(code, Type::None, false, true);
    }

    virtual bool valueLeftOnStack() const override { return true; }
    
    // In cases where we have a constant offset to a member of a struct
    // we can skip the OFFSET op and just offset the address here
    void setOffset(uint32_t offset) { _offset = offset; }

  private:
    void emitCode(std::vector<uint8_t>& code, Type type, bool ref, bool pop);
    
    SymbolPtr _symbol = nullptr;
    uint32_t _offset = 0;
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
    ConstantNode(Type t, uint32_t v) : _type(t), _i(v) { }
    ConstantNode(float v)  : _type(Type::Float), _numeric(true), _f(v) { }

    virtual ASTNodeType astNodeType() const override{ return ASTNodeType::Constant; }
    virtual Type type() const override { return _type; }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) override;
    
    void toFloat() { _f = float(_i); }
    void toUInt() { _i = uint32_t(_f); }
    void setType(Type type) { _type = type; }

    virtual bool valueLeftOnStack() const override { return true; }

    int32_t integerValue() const
    {
        if (isScalar(_type) || isEnum(_type)) {
            if (_type == Type::Float) {
                return int32_t(_f);
            }
            return _i;
        }
        return -1; // Don't return 0. Some callers want to know if value is 0 or not.
    }
    
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
    virtual Type type() const override { return Type::String; }
    virtual bool isPointer() const override { return true; }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) override;

    virtual bool valueLeftOnStack() const override { return true; }

  private:
    std::string _string;
};

class TypeCastNode : public ASTNode
{
  public:
    TypeCastNode(Type t, const ASTPtr& arg) : _type(t), _arg(arg) { }

    virtual Type type() const override { return _type; }
    
    virtual ASTNodeType astNodeType() const override { return ASTNodeType::TypeCast; }

    virtual const ASTPtr child(uint32_t i) const override
    {
        if (i == 0) {
            return _arg;
        }
        return nullptr;
    }

    virtual const uint32_t numChildren() const override { return 1; }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) override;
    
    static ASTPtr castIfNeeded(ASTPtr& node, Type neededType);
    
    virtual bool valueLeftOnStack() const override { return true; }

  private:
    Type _type;
    ASTPtr _arg;
};

class AssignmentNode : public ASTNode
{
  public:
    // op is passed for operator assignment (like +=), NOP if simple assignment
    AssignmentNode(const ASTPtr& left, Op op, const ASTPtr& right)
        : _left(left)
        , _op(op)
        , _right(right)
    {
        // If _left is a pointer don't try to cast. We've already verified that
        // both are pointers if one is.
        if (!_left->isPointer()) {
            _right = TypeCastNode::castIfNeeded(_right, _left->type());
        }
    }

    virtual Type type() const override { return _left->type(); }
    
    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Assignment; }

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

    virtual const uint32_t numChildren() const override { return 2; }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) override;

  private:
    Op _op;
    ASTPtr _left, _right;
};

class OpNode : public ASTNode
{
  public:
    OpNode(const ASTPtr& left, Op op, const ASTPtr& right, Type resultType, bool isRef)
        : _isRef(isRef)
        , _op(op)
        , _left(left)
        , _right(right)
    {
        _type = (resultType == Type::None) ? _left->type() : resultType;

        if (_op == Op::LNOT && _left->type() != Type::UInt8) {
            // Cast to uint8 (boolean)
            _left = TypeCastNode::castIfNeeded(_left, Type::UInt8);
        }

        if (_op == Op::LNOT && _right->type() != Type::UInt8) {
            // Cast to uint8 (boolean)
            _right = TypeCastNode::castIfNeeded(_right, Type::UInt8);
        } else {
            _right = TypeCastNode::castIfNeeded(_right, _left->type());
        }
    }
    
    OpNode(const ASTPtr& left, Op op, Type resultType, bool isRef)
        : _isRef(isRef)
        , _op(op)
        , _left(left)
    {
        assert(op != Op::NOP);
        _type = (resultType == Type::None) ? _left->type() : resultType;

        if (_op == Op::LNOT && _left->type() != Type::UInt8) {
            // Cast to uint8 (boolean)
            _left = TypeCastNode::castIfNeeded(_right, Type::UInt8);
        }
    }
    
    OpNode(Op op, const ASTPtr& right, Type resultType, bool isRef)
        : _isRef(isRef)
        , _op(op)
        , _right(right)
    {
        assert(op != Op::NOP);
        _type = (resultType == Type::None) ? _right->type() : resultType;

        if (_op == Op::LNOT && _right->type() != Type::UInt8) {
            // Cast to uint8 (boolean)
            _right = TypeCastNode::castIfNeeded(_right, Type::UInt8);
        }
    }
    
    OpNode(Op op) : _op(op) { }
    
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

    virtual const uint32_t numChildren() const override { return 2; }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) override;
    
    Op op() const { return _op; }

    virtual bool valueLeftOnStack() const override
    {
        // A value is left on the stack except in the case of assignment
        return true;
    }

  private:
    bool _isRef = false;
    Op _op;
    Type _type;
    ASTPtr _left, _right;
};

class DotNode : public ASTNode
{
  public:
    DotNode(const ASTPtr& operand, const SymbolPtr& property)
        : _operand(operand)
        , _property(property)
    {
        _type = _property->type();
    }

    
    virtual ASTNodeType astNodeType() const override{ return ASTNodeType::Dot; }

    virtual Type type() const override { return _type; }
    virtual uint16_t elementSizeInBytes() const override { return _property->elementSizeInBytes(); }
    virtual bool isPointer() const override { return _property->isPointer(); }
    virtual bool isIndexable() const override { return _property->nElements() != 1; }

    virtual bool isAssignable() const override { return true; }

    virtual const ASTPtr child(uint32_t i) const override
    {
        if (i == 0) {
            return _operand;
        }
        return nullptr;
    }

    virtual const uint32_t numChildren() const override { return 1; }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) override;

    virtual bool valueLeftOnStack() const override { return true; }
    
    SymbolPtr property() const { return _property; }

  private:
    ASTPtr _operand;
    SymbolPtr _property;
    Type _type;
};

class ModuleNode : public ASTNode
{
  public:
    ModuleNode(uint8_t id) : _id(id) { }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Module; }
    
    uint8_t id() const { return _id; }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) override;

  private:
    uint8_t _id;
};

class FunctionCallNode : public ASTNode
{
  public:
    // Call a raw function with no instance value
    FunctionCallNode(FunctionPtr func)
        : _function(func)
    { }
    
    // Call a raw function with no instance value
    FunctionCallNode(FunctionPtr func, const ASTPtr& instance)
        : _function(func)
        , _instance(instance)
    { }
    
    // Call function in the passed module
    FunctionCallNode(FunctionPtr func, uint8_t moduleId)
        : _moduleId(moduleId)
        , _function(func)
    { }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::FunctionCall; }
    
    virtual Type type() const override { return _function ? _function->returnType() : Type::None; }

    virtual void addNode(const ASTPtr& node) override
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

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) override;

    FunctionPtr function() const { return _function; }
    void setPushReturn(bool r) { _pushReturn = r; }
    
  private:
    FunctionPtr _function;
    uint8_t _moduleId = 0;
    ASTNodeList _args;
    bool _pushReturn = true;
    ASTPtr _instance;

};

class EnterNode : public ASTNode
{
  public:
    EnterNode(const FunctionPtr& function) : _function(function) { }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Enter; }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) override;
    
  private:
    FunctionPtr _function;
};

enum class BranchSize { Unknown, Short, Long };

class BranchNode : public ASTNode
{
  public:
    enum class Kind { IfStart, ElseStart, IfEnd, LoopStart, LoopNext, LoopEnd, Break, Continue };
    
    BranchNode(Kind k) : _kind(k) { }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Branch; }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) override;
    
    void setFixupNode(const ASTPtr& f) { _fixupNode = f; }
    
    // Address of where this node should branch to
    void fixup(std::vector<uint8_t>& code, AddrNativeType addr);
    
    AddrNativeType fixupIndex() const { return _fixupIndex; }
    
    Kind kind() const { return _kind; }
    
  private:
    Kind _kind;
    BranchSize _branchSize = BranchSize::Unknown;
    ASTPtr _fixupNode;
    AddrNativeType _fixupIndex = 0;
};

class CaseClause
{
  public:
    CaseClause(const ASTPtr& stmt) : _stmt(stmt), _isDefault(true) { }
    CaseClause(int32_t value, const ASTPtr& stmt) : _value(value), _stmt(stmt) { }
    
    int32_t value() const { return _value; }
    const ASTPtr& stmt() const { return _stmt; }
    void setFixupIndex(AddrNativeType addr) { _fixupIndex = addr; }
    AddrNativeType fixupIndex() const { return _fixupIndex; }
    void fixup(std::vector<uint8_t>& code, AddrNativeType addr);
    bool isDefault() const { return _isDefault; }
    BranchSize branchSize() const { return _branchSize; }
    
  private:
    int32_t _value = 0;
    ASTPtr _stmt;
    AddrNativeType _fixupIndex = 0;
    bool _isDefault = false;
    BranchSize _branchSize = BranchSize::Unknown;
};

class SwitchNode : public ASTNode
{
  public:
    SwitchNode(const ASTPtr& expr)
        : _expr(expr)
    { }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Switch; }
    
    virtual const ASTPtr child(uint32_t i) const override
    {
        if (_clauses.size() <= i) {
            return nullptr;
        }
        return _clauses[i].stmt();
    }

    virtual const uint32_t numChildren() const override { return uint32_t(_clauses.size()); }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) override;

    void addCaseClause(const ASTPtr& stmt) { _clauses.emplace_back(stmt); _haveDefault = true; }
    void addCaseClause(int32_t value, const ASTPtr& stmt) { _clauses.emplace_back(value, stmt); }
    
  private:
    ASTPtr _expr;
    std::vector<CaseClause> _clauses;
    bool _haveDefault = false;
    BranchSize _branchSize = BranchSize::Unknown;
    BranchSize _defaultBranchSize = BranchSize::Unknown;
};

class ConditionalNode : public ASTNode
{
  public:
  public:
    ConditionalNode(const ASTPtr& expr, const ASTPtr& first, const ASTPtr& second)
        : _expr(expr)
        , _first(first)
        , _second(second)
    { }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Conditional; }
    
    // first and second have to be the same type. That should have been validated by the caller
    virtual Type type() const override { return _first->type(); }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) override;

  private:
    ASTPtr _expr;
    ASTPtr _first;
    ASTPtr _second;

    BranchSize _ifBranchSize = BranchSize::Unknown;
    BranchSize _elseBranchSize = BranchSize::Unknown;
};

class LogicalNode : public ASTNode
{
  public:
  public:
    enum class Kind { LAnd, LOr };
    
    LogicalNode(Kind kind, const ASTPtr& lhs, const ASTPtr& rhs)
        : _kind(kind)
        , _lhs(lhs)
        , _rhs(rhs)
    { }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Logical; }
    
    // first and second have to be the same type. That should have been validated by the caller
    virtual Type type() const override { return Type::UInt8; }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) override;

  private:
    Kind _kind;
    ASTPtr _lhs;
    ASTPtr _rhs;

    BranchSize _branchSize = BranchSize::Unknown;
};

class IndexNode : public ASTNode
{
  public:
    IndexNode(const ASTPtr& lhs, const ASTPtr& rhs)
        : _lhs(lhs)
        , _rhs(rhs)
    { }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Index; }
    virtual Type type() const override { return _lhs->type(); }
    virtual uint16_t elementSizeInBytes() const override { return _lhs->elementSizeInBytes(); }

    virtual bool isAssignable() const override { return true; }

    virtual bool isPointer() const override
    {
        // The underlying type of the array is what determines whether or not it's a
        // pointer. If it's a pointer type or a struct it is, otherwise it's not
        return _lhs->isPointer();
    }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) override;
    
    virtual bool valueLeftOnStack() const override { return true; }

  private:
    ASTPtr _lhs, _rhs;
};

class ReturnNode : public ASTNode
{
  public:
    ReturnNode(const ASTPtr& arg)
        : _arg(arg)
    { }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Return; }
    virtual Type type() const override { return _arg->type(); }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) override;
    
  private:
    ASTPtr _arg;
};

class DropNode : public ASTNode
{
  public:
    DropNode(uint16_t bytesToDrop)
        : _bytesToDrop(bytesToDrop)
    { }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Drop; }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) override;
    
  private:
    uint16_t _bytesToDrop;
};

// addressof (&) operator
class RefNode : public ASTNode
{
  public:
    RefNode(const ASTPtr& operand)
        : _operand(operand)
    { }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Ref; }
    virtual Type type() const override { return _operand->type(); }
    virtual bool isPointer() const override { return true; }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) override;
    
  private:
    ASTPtr _operand;
};

// deref (*) operator
class DerefNode : public ASTNode
{
  public:
    DerefNode(const ASTPtr& operand)
        : _operand(operand)
    { }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Deref; }
    virtual Type type() const override { return _operand->type(); }
    virtual bool isAssignable() const override { return true; }
    //virtual bool isPointer() const override { return _operand->isPointer(); }
    virtual bool isIndexable() const override { return true; }

    virtual void emitCode(std::vector<uint8_t>& code, bool isLHS) override;
    
  private:
    ASTPtr _operand;
};

class InitializerNode : public ASTNode
{
  public:
    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Initializer; }

    virtual void addNode(const ASTPtr& node) override
    {
        _list.push_back(node);
    }
    
    virtual const ASTPtr child(uint32_t i) const override
    {
        if (_list.size() <= i) {
            return nullptr;
        }
        return _list[i];
    }

    virtual const uint32_t numChildren() const override { return uint32_t(_list.size()); }
    
  private:
    ASTNodeList _list;
};


}
