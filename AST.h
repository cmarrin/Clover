/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
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
#include "Function.h"
#include "Module.h"
#include "Symbol.h"

namespace clvr {

// Classes to represent AST

enum class ASTNodeType {
    Statements,
    Op,
    Inc,
    Var,
    Constant,
    String,
    Dot,
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
    virtual uint16_t elementSizeInBytes() const { return typeToBytes(type(), isPointer()); }
    
    virtual const ASTPtr child(uint32_t i) const { return nullptr; }
    virtual const uint32_t numChildren() const { return 0; }

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

    virtual bool valueLeftOnStack() const override { return true; }
    
    // In cases where we have a constant offset to a member of a struct
    // we can skip the OFFSET op and just offset the address here
    void setOffset(AddrNativeType offset) { _offset = offset; }
    AddrNativeType offset() const { return _offset; }
    SymbolPtr symbol() const { return _symbol; }

  private:    
    SymbolPtr _symbol = nullptr;
    AddrNativeType _offset = 0;
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
    ConstantNode(Type t, int32_t v) : _type(t), _i(v) { }
    ConstantNode(float v)  : _type(Type::Float), _numeric(true), _f(v) { }

    virtual ASTNodeType astNodeType() const override{ return ASTNodeType::Constant; }
    virtual Type type() const override { return _type; }

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
    
    int32_t rawInteger() const { return _i; }
    
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

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::String; }
    virtual Type type() const override { return Type::UInt8; }
    virtual bool isPointer() const override { return true; }

    virtual bool valueLeftOnStack() const override { return true; }
    
    const std::string& string() const { return _string; }

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

    static ASTPtr castIfNeeded(ASTPtr& node, Type neededType);
    
    virtual bool valueLeftOnStack() const override { return true; }

    ASTPtr arg() const { return _arg; }
    
  private:
    Type _type;
    ASTPtr _arg;
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
            // Cast to uint8_t (boolean)
            _left = TypeCastNode::castIfNeeded(_left, Type::UInt8);
        }

        if (_op == Op::LNOT && _right->type() != Type::UInt8) {
            // Cast to uint8_t (boolean)
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
            // Cast to uint8_t (boolean)
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
            // Cast to uint8_t (boolean)
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

    Op op() const { return _op; }
    const ASTPtr& left() const { return _left; }
    const ASTPtr& right() const { return _right; }
    bool isRef() const { return _isRef; }

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

class IncNode : public ASTNode
{
  public:
    IncNode(const ASTPtr& node, bool isPre, int16_t inc)
        :_node(node)
        , _isPre(isPre)
        , _inc(inc)
    {
        // If node is a pointer, multiply size by inc
        if (node->isPointer()) {
            _inc *= node->elementSizeInBytes();
        }
    }
    
    virtual ASTNodeType astNodeType() const override{ return ASTNodeType::Inc; }

    virtual Type type() const override { return _node->type(); }

    virtual const ASTPtr child(uint32_t i) const override
    {
        if (i == 0) {
            return _node;
        }
        return nullptr;
    }

    virtual const uint32_t numChildren() const override { return 1; }

    const ASTPtr& node() const { return _node; }
    bool isPre() const { return _isPre; }
    int16_t inc() const { return _inc; }

  private:
    ASTPtr _node;
    bool _isPre = false;
    int16_t _inc = 1;
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

    Op op() const { return _op; }
    const ASTPtr& left() const { return _left; }
    const ASTPtr& right() const { return _right; }

  private:
    Op _op;
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

    virtual bool valueLeftOnStack() const override { return true; }
    
    ASTPtr operand() const { return _operand; }
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
    
    // Call a function with an instance value
    FunctionCallNode(FunctionPtr func, const ASTPtr& instance)
        : _function(func)
        , _instance(instance)
    { }
    
    // Call function in the passed module
    FunctionCallNode(FunctionPtr func, uint8_t moduleId, const std::string& moduleName)
        : _function(func)
        , _moduleId(moduleId)
        , _moduleName(moduleName)
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

    FunctionPtr function() const { return _function; }
    const ASTNodeList& args() const { return _args; }
    uint8_t moduleId() const { return _moduleId; }
    const std::string& moduleName() const { return _moduleName; }
    ASTPtr instance() const { return _instance; }
    bool pushReturn() const { return _pushReturn; }
    
    void setPushReturn(bool r) { _pushReturn = r; }
    
  private:
    FunctionPtr _function;
    uint8_t _moduleId = 0;
    std::string _moduleName;
    ASTNodeList _args;
    bool _pushReturn = true;
    ASTPtr _instance;
};

class EnterNode : public ASTNode
{
  public:
    EnterNode(const FunctionPtr& function) : _function(function) { }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Enter; }

    uint16_t localSize() const { return _function->localSize(); }
  private:
    FunctionPtr _function;
};

enum class BranchSize { Unknown, Short, Long };

class BranchNode : public ASTNode
{
  public:
    enum class Kind { IfStart, ElseStart, IfEnd, LoopStart, LoopNext, LoopEnd, Break, Continue };
    
    BranchNode(Kind k) : _kind(k) { }
    BranchNode(const ASTPtr& expr) : _kind(Kind::IfStart), _expr(expr)
    {
        // expr needs to be uint8_t
        _expr = TypeCastNode::castIfNeeded(_expr, Type::UInt8);
    }

    virtual ASTNodeType astNodeType() const override { return ASTNodeType::Branch; }

    static void fixup(std::vector<uint8_t>& code, AddrNativeType fixupIndex, AddrNativeType addr, BranchSize& branchSize);

    void setFixupNode(const ASTPtr& f) { _fixupNode = f; }
    
    // Address of where this node should branch to
    void fixup(std::vector<uint8_t>& code, AddrNativeType addr);
    
    void setFixupIndex(AddrNativeType index) { _fixupIndex = index; }
    AddrNativeType fixupIndex() const { return _fixupIndex; }
    Kind kind() const { return _kind; }
    BranchSize branchSize() const { return _branchSize; }
    ASTPtr fixupNode() const { return _fixupNode; }
    ASTPtr expr() const { return _expr; }
    
  private:
    Kind _kind;
    BranchSize _branchSize = BranchSize::Unknown;
    ASTPtr _fixupNode;
    AddrNativeType _fixupIndex = 0;
    ASTPtr _expr;
};

class CaseClause
{
  public:
    CaseClause(const ASTPtr& stmt) : _stmt(stmt), _isDefault(true) { }
    CaseClause(int32_t value, const ASTPtr& stmt) : _value(value), _stmt(stmt) { }
    
    // When default return the most negative value, so default alway is the first value in a sorted list
    int32_t value() const { return _isDefault ? std::numeric_limits<int32_t>::min() : _value; }
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

    void addCaseClause(const ASTPtr& stmt) { _clauses.emplace_back(stmt); _haveDefault = true; }
    void addCaseClause(int32_t value, const ASTPtr& stmt) { _clauses.emplace_back(value, stmt); }

    //void fixupDefault(std::vector<uint8_t>& code, AddrNativeType index, AddrNativeType addr);

    ASTPtr expr() const { return _expr; }
    std::vector<CaseClause>& clauses() { return _clauses; }
    bool haveDefault() const { return _haveDefault; }
    void setBranchSize(BranchSize s) { _branchSize = s; }
    BranchSize branchSize() const { return _branchSize; }
    BranchSize& defaultBranchSize() { return _defaultBranchSize; }
    
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

    virtual bool isIndexable() const override { return _first->isIndexable(); }
    virtual bool isAssignable() const override { return _first->isAssignable(); }
    virtual bool isPointer() const override { return _first->isPointer(); }

    ASTPtr expr() const { return _expr; }
    ASTPtr first() const { return _first; }
    ASTPtr second() const { return _second; }
    BranchSize& ifBranchSize() { return _ifBranchSize; }
    BranchSize& elseBranchSize() { return _elseBranchSize; }

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

    Kind kind() const { return _kind; }
    ASTPtr lhs() const { return _lhs; }
    ASTPtr rhs() const { return _rhs; }
    BranchSize& branchSize() { return _branchSize; }

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

    virtual bool valueLeftOnStack() const override { return true; }

    ASTPtr lhs() const { return _lhs; }
    ASTPtr rhs() const { return _rhs; }

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
    virtual Type type() const override { return _arg ? _arg->type() : Type::None; }

    ASTPtr arg() const { return _arg; }

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
    
    uint16_t bytesToDrop() const { return _bytesToDrop; }

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

    ASTPtr operand() const { return _operand; }

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
    
    // The operand is obviously a pointer because it has to be to be deref'ed. But once deref'ed
    // it's no longer a pointer
    //virtual bool isPointer() const override { return _operand->isPointer(); }
    
    virtual bool isIndexable() const override { return true; }

    ASTPtr operand() const { return _operand; }

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
