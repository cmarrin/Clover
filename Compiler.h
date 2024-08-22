/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Lucid
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

// Lucid compiler
//
// A simple imperative language which generates code that can be 
// executed by the Interpreter
//

#pragma once

#include <cstdint>
#include <istream>
#include <variant>

#include "AST.h"
#include "Function.h"
#include "Defines.h"
#include "Module.h"
#include "Scanner.h"
#include "Struct.h"

namespace lucid {

//*********************************
//
//  Class: Compiler
//
//*********************************

/*

A program consists of zero or more imports followed by
zero or more structs, optionally followed by an
instantiation of a struct. There can only be one
such instantiation in a program but it can appear in
the main module or any imported module. On start this
struct is instantiated and its ctor is called.

BNF:

program:
    { import } struct ;

import:
    'import' <id> [ 'as' <id> ] ;
    
struct:
    'struct' <id> '{' { structEntry ';' } '}' ;
    
structEntry:
    struct | varStatement | function | init  ;

varStatement:
    [ 'const' ] type [ '*' ] var ';' ;

var:
    <id> [ '[' <integer> ']' ] [ '=' initializer ] ;
    
initializer:
    arithmeticExpression | '{' [ initializer { ',' initializer } ] '}'

function:
    'function' [ <type> ] <id> '(' formalParameterList ')' compoundStatement ;

init:
    'initialize' '(' ')' compoundStatement ;
    
// <id> is a struct name
type:
      'float'
    | 'hfloat'
    | 'int8_t'
    | 'uint8_t'
    | 'int16_t'
    | 'uint16_t'
    | 'int32_t'
    | 'uint32_t'
    | <id>
    ;
    
statement:
      compoundStatement
    | ifStatement
    | switchStatement
    | forStatement
    | whileStatement
    | loopStatement
    | returnStatement
    | jumpStatement
    | varStatement
    | expressionStatement
    ;
  
compoundStatement:
    '{' { statement } '}' ;

ifStatement:
    'if' '(' arithmeticExpression ')' statement ['else' statement ] ;

switchStatement:
    'switch' '(' arithmeticExpression ')' '{' { caseClause } '}' ;

caseClause:
    ('case' value | 'default) ':' statement ;

forStatement:
    'for' '(' [ [ <type> ] identifier '=' arithmeticExpression ] ';'
            [ arithmeticExpression ] ';' [ arithmeticExpression ] ')' statement ;
    
whileStatement:
    'while' '(' arithmeticExpression ')' statement ;

loopStatement:
    'loop' statement ;

returnStatement:
      'return' [ arithmeticExpression ] ';' ;
      
jumpStatement:
      'break' ';'
    | 'continue' ';'
    ;

expressionStatement:
    [ arithmeticExpression ] ';' ;
    
arithmeticExpression:
      conditionalExpression
    | unaryExpression operator arithmeticExpression

conditionalExpression:
      unaryExpression
    | conditionalExpression '?' expression : expression
    ;
    
unaryExpression:
      postfixExpression
    | '-' unaryExpression
    | '~' unaryExpression
    | '!' unaryExpression
    | '++' unaryExpression
    | '--' unaryExpression
    | '&' unaryExpression
    ;

postfixExpression:
      primaryExpression
    | postfixExpression '(' argumentList ')'
    | postfixExpression '[' arithmeticExpression ']'
    | postfixExpression '.' identifier
    | postfixExpression '++'
    | postfixExpression '--'
    ;

primaryExpression:
      '(' arithmeticExpression ')'
    | <id>
    | <float>
    | <integer>
    ;
    
formalParameterList:
      (* empty *)
    | type ['*'] identifier { ',' type identifier }
    ;

argumentList:
        (* empty *)
      | arithmeticExpression { ',' arithmeticExpression }
      ;

value:
    
operator: (* operator   precedence   association *)
               '='     (*   1          Right    *)
    |          '+='    (*   1          Right    *)
    |          '-='    (*   1          Right    *)
    |          '*='    (*   1          Right    *)
    |          '/='    (*   1          Right    *)
    |          '&='    (*   1          Right    *)
    |          '|='    (*   1          Right    *)
    |          '^='    (*   1          Right    *)
    |          '||'    (*   2          Left     *)
    |          '&&'    (*   3          Left     *)
    |          '|'     (*   4          Left     *)
    |          '^'     (*   5          Left     *)
    |          '&'     (*   6          Left     *)
    |          '=='    (*   7          Left     *)
    |          '!='    (*   7          Left     *)
    |          '<'     (*   8          Left     *)
    |          '>'     (*   8          Left     *)
    |          '>='    (*   8          Left     *)
    |          '<='    (*   8          Left     *)
    |          '+'     (*   10         Left     *)
    |          '-'     (*   10         Left     *)
    |          '*'     (*   11         Left     *)
    |          '/'     (*   11         Left     *)
    |          '%'     (*   11         Left     *)
    ;
    
*/

enum class Error {
    None,
    UnrecognizedLanguage,
    ExpectedToken,
    ExpectedKeyword,
    ExpectedType,
    ExpectedValue,
    ExpectedString,
    ExpectedRef,
    ExpectedOpcode,
    ExpectedEnd,
    ExpectedIdentifier,
    ExpectedExpr,
    ExpectedArgList,
    ExpectedFormalParams,
    ExpectedFunction,
    ExpectedLHSExpr,
    ExpectedStructType,
    ExpectedVar,
    AssignmentNotAllowedHere,
    InvalidParamCount,
    UndefinedIdentifier,
    ParamOutOfRange,
    JumpTooBig,
    IfTooBig,
    ElseTooBig,
    StringTooLong,
    TooManyConstants,
    TooManyVars,
    DefOutOfRange,
    ExpectedDef,
    NoMoreTemps,
    TempNotAllocated,
    InternalError,
    StackTooBig,
    MismatchedType,
    WrongNumberOfArgs,
    WrongType,
    OnlyAllowedInLoop,
    DuplicateIdentifier,
    ExecutableTooBig,
    InitializerNotAllowed,
    ExpectedIndexable,
    PointerConstantNotAllowed,
    RequiresInitializer,
    PropertyDoesNotExist,
    IteratorMustBeScalar,
    PtrAssignmentMustMatch,
    PtrTypeNotAllowed,
    WrongNumberOfInitializers,
};

class Compiler {
public:
  	Compiler(std::istream* stream, AnnotationList* annotations)
        : _scanner(stream, annotations)
    { }

    bool compile(std::vector<uint8_t>& executable, uint32_t maxExecutableSize,
                 const std::vector<Module*>&);

    bool program();

    Error error() const { return _error; }
    Token expectedToken() const { return _expectedToken; }
    const std::string& expectedString() const { return _expectedString; }
    uint32_t lineno() const { return _scanner.lineno(); }
    uint32_t charno() const { return _scanner.charno(); }

    const StructPtr typeToStruct(Type type) const
    {
        uint8_t i = uint8_t(type);
        if (i < StructTypeStart) {
            return nullptr;
        }
        i -= StructTypeStart;
        if (i >= _structTypes.size()) {
            return nullptr;
        }
        return _structTypes[i];
    }
    
    void setAnnotation(int32_t index, uint32_t addr) { _scanner.setAnnotation(index, addr); }

protected:
    bool statement(const ASTPtr& parent);
    bool type(Type&);
  
    bool function();
    bool init();
    bool varStatement(const ASTPtr& parent);

    bool var(const ASTPtr& parent, Type, bool isPointer, bool isConstant);

    bool value(uint32_t& i, Type);

private:
    bool import();
    bool strucT();
    
    bool structEntry();
    
    bool compoundStatement(const ASTPtr& parent);
    bool ifStatement(const ASTPtr& parent);
    bool switchStatement(const ASTPtr& parent);
    bool loopStatement(const ASTPtr& parent);
    bool returnStatement(const ASTPtr& parent);
    bool jumpStatement(const ASTPtr& parent);
    bool expressionStatement(const ASTPtr& parent);

    ASTPtr expression();
    ASTPtr arithmeticExpression(const ASTPtr& lhs, uint8_t minPrec = 1);
    ASTPtr conditionalExpression();
    ASTPtr unaryExpression();
    ASTPtr postfixExpression();
    ASTPtr primaryExpression();

    bool formalParameterList();
    bool argumentList(const ASTPtr& fun);
    bool caseClause(int32_t& value, bool& isDefault);
    
    void collectConstants(Type type, bool isArray, AddrNativeType& addr, uint16_t& nElements, bool& isScalarConstant);
    
    bool isReserved(Token token, const std::string str, Reserved&);

    StructPtr addStruct(const std::string& name, Type type)
    {
        _structs.push_back(std::make_shared<Struct>(name, type));
        return _structs.back();
    }

    StructPtr findStruct(const std::string&);
    SymbolPtr findSymbol(const std::string&);
    int16_t findModuleId(const std::string&);

    // The expect methods validate the passed param and if
    // there is no match, the passed error is saved and
    // throw is called. The first version also retires the
    // current token.
    void expect(Token token, const char* str = nullptr);
    void expect(bool passed, Error error);
    bool match(Reserved r);
    bool match(Token r);
    void ignoreNewLines();
    
    // These methods check to see if the next token is of the
    // appropriate type. Some versions just return true or
    // false, others also return details about the token
    bool identifier(std::string& id, bool retire = true);
    bool integerValue(uint32_t& i);
    bool floatValue(float& f);
    bool stringValue(std::string&);
    bool reserved();
    bool reserved(Reserved &r);
    
    uint16_t sizeInBytes(Type type) const;
    
    void appendValue(std::vector<uint8_t>& container, uint32_t v, Type t)
    {
        switch (typeToOpSize(t)) {
            case OpSize::i32:
            case OpSize::flt: container.push_back(v >> 24);
                              container.push_back(v >> 16);
            case OpSize::i16: container.push_back(v >> 8);
            case OpSize::i8 : container.push_back(v);
        }
    }
    
    FunctionPtr currentFunction()
    {
        expect(_inFunction && _currentFunction, Error::InternalError);
        return _currentFunction;
    }
    
    uint32_t annotationIndex() const  { return _scanner.annotationIndex(); }

    Error _error = Error::None;
    Token _expectedToken = Token::None;
    std::string _expectedString;
    
    StructPtr currentStruct()
    {
        expect(!_structStack.empty(), Error::InternalError);
        return _structStack.back();
    }

    void enterJumpContext() { _jumpList.emplace_back(); }
    void exitJumpContext() { _jumpList.pop_back(); }
    void addJumpEntry(const ASTPtr& jump);
    
    void addJumpFixupNodes(const ASTPtr& parent, BranchNode::Kind jumpKind, const BranchNode::Kind targetKind);
    
    Scanner _scanner;

    bool _inFunction = false;
    FunctionPtr _currentFunction = nullptr;
    
    uint8_t _nextStructType = StructTypeStart;
    std::vector<StructPtr> _structTypes; // array of structs, ordered by (type-StructTypeStart)

    uint32_t _entryStructIndex;
    
    // Scalar constants are embedded in the code as constant opcodes. Structs and arrays
    // are stored at the start of the executable and are accessed as Constant values
    std::vector<uint8_t> _constants;
    std::vector<uint32_t> _scalarConstants;
    
    StructList _structs;
    StructList _structStack;
    std::vector<ModulePtr> _modules;
    
    // The jump list is an array of arrays of JumpEntries. The outermost array
    // is a stack of active looping statements (for, loop, etc.). Each of these
    // has an array of break or continue statements that need to be resolved
    // when the looping statement ends.
    std::vector<std::vector<ASTPtr>> _jumpList;

    uint16_t _nextMem = 0; // next available location in mem
    uint16_t _localHighWaterMark = 0;
};

}
