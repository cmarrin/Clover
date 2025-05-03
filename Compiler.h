/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2024, Chris Marrin
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

#include <cstdint>
#include <istream>
#include <functional>
#include <variant>

#include "AST.h"
#include "CodeGen.h"
#include "Enum.h"
#include "Function.h"
#include "Defines.h"
#include "Module.h"
#include "Scanner.h"
#include "Struct.h"

namespace clvr {

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
    struct | varStatement | function | ctor | enum  ;

varStatement:
    [ 'const' ] type [ '*' ] var ';' ;

var:
    <id> [ '[' <integer> ']' ] [ '=' initializer ] ;
    
initializer:
    arithmeticExpression | '{' [ initializer { ',' initializer } ] '}'

function:
    'function' [ <type> ] <id> '(' formalParameterList ')' compoundStatement ;

ctor:
    <id> '(' ')' compoundStatement ;

enum:
    'enum' <id> '{' [enumEntry ] { ',' enumEntry } '}' ;

enumEntry:
    <id> [ '=' <integer> ] ;

// <id> is a struct or enum name
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
    |          '%='    (*   1          Right    *)
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
    ExpectedReturnValue,
    UnexpectedReturnValue,
    ExpectedString,
    ExpectedRef,
    ExpectedOpcode,
    ExpectedEnd,
    ExpectedIdentifier,
    ExpectedExpr,
    ExpectedEnum,
    ExpectedArgList,
    ExpectedFormalParams,
    ExpectedFunction,
    ExpectedLHSExpr,
    ExpectedStructType,
    ExpectedConstExpr,
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
    CodeGenFailed,
};

class Compiler {
public:
    enum class OutputFormat { StackVM, ASM6809 };
    
  	Compiler(OutputFormat, std::istream* stream, uint32_t maxExecutableSize, const std::vector<Module*>&, Annotations*);

    std::vector<uint8_t>& code() { return _codeGen->code(); }
    const std::vector<uint8_t>& constants() const { return _constants; }
    const StructPtr& topLevelStruct() const { return _topLevelStruct; }
    
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
    
    const EnumPtr typeToEnum(Type type) const
    {
        uint8_t i = uint8_t(type);
        if (i < EnumTypeStart || i >= StructTypeStart) {
            return nullptr;
        }
        i -= EnumTypeStart;
        if (i >= _enumTypes.size()) {
            return nullptr;
        }
        return _enumTypes[i];
    }
    
    void setAnnotation(int32_t index, int32_t addr) { _scanner.setAnnotation(index, addr); }

protected:
    bool compile(uint32_t maxExecutableSize, const std::vector<Module*>&);

    bool statement(const ASTPtr& parent);
    bool type(Type&);
  
    bool function();
    bool ctor();
    bool varStatement(const ASTPtr& parent);

    bool var(const ASTPtr& parent, Type, bool isPointer, bool isConstant);

    bool value(uint32_t& i, Type);

private:
    bool import();
    bool strucT();
    bool enuM();
    
    bool structEntry();
    bool enumEntry(EnumPtr);
    
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

    EnumPtr addEnum(const std::string& name)
    {
        EnumPtr e = std::make_shared<Enum>(name, Type(_enumTypes.size() + EnumTypeStart));
        _enumTypes.push_back(e);
        return e;
    }

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

    void emitStruct(CodeGen*, const StructPtr&);

    using TraversalVisitor = std::function<void(const ASTPtr&)>;
    void traverseStruct(const StructPtr& struc, TraversalVisitor func) const;
    void traverseNode(const ASTPtr& node, TraversalVisitor) const;
    
    Scanner _scanner;

    bool _inFunction = false;
    FunctionPtr _currentFunction = nullptr;
    
    std::vector<StructPtr> _structTypes; // array of structs, ordered by (type-StructTypeStart)
    std::vector<EnumPtr> _enumTypes; // array of enums, ordered by (type-EnumTypeStart)

    // Scalar constants are embedded in the code as constant opcodes. Structs and arrays
    // are stored at the start of the executable and are accessed as Constant values
    std::vector<uint8_t> _constants;
    std::vector<uint32_t> _scalarConstants;
    
    StructPtr _topLevelStruct;
    StructList _structStack;
    std::vector<ModulePtr> _modules;
    
    // The jump list is an array of arrays of JumpEntries. The outermost array
    // is a stack of active looping statements (for, loop, etc.). Each of these
    // has an array of break or continue statements that need to be resolved
    // when the looping statement ends.
    std::vector<std::vector<ASTPtr>> _jumpList;

    uint16_t _nextMem = 0; // next available location in mem
    uint16_t _localHighWaterMark = 0;
    
    CodeGen* _codeGen = nullptr;
};

}
