/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "CompileEngine.h"

#include <map>
#include <vector>
#include <cmath>

#include "AST.h"
#include "OpInfo.h"

using namespace lucid;

bool
CompileEngine::program()
{
    _scanner.setIgnoreNewlines(true);
    
    try {
        while(import()) { }
        while(constant()) { }
        while(strucT()) { }
        
        // Handle top level struct if it exists
        uint32_t structIndex;
        std::string id;
        if (identifier(id)) {
            expect(findStruct(id, structIndex), Compiler::Error::ExpectedStructType);
            expect(identifier(id), Compiler::Error::ExpectedIdentifier);
        }

        expect(Token::Semicolon);
        expect(Token::EndOfFile);
    }
    catch(...) {
        return false;
    }
    
    return _error == Compiler::Error::None;
}

bool
CompileEngine::import()
{
    if (!match(Reserved::Import)) {
        return false;
    }
    
    std::string id, idAs;
    expect(identifier(id), Compiler::Error::ExpectedIdentifier);

    if (match(Reserved::As)) {
        expect(identifier(idAs), Compiler::Error::ExpectedIdentifier);
    }
    
    // FIXME: Compile the import inline.
    // An import is a regular Lucid program but only the first struct is
    // used. What about imports in the imported file? Are there warnings
    // if there are more structs? What about an entry struct?
    // Need to rename struct if there is an idAs. How do we deal with
    // duplicate struct names?
    return true;
}

bool
CompileEngine::strucT()
{
    if (!match(Reserved::Struct)) {
        return false;
    }
    
    std::string id;
    expect(identifier(id), Compiler::Error::ExpectedIdentifier);

    // Add a struct entry
    _structs.emplace_back(id);
    
    if (!_structStack.empty()) {
        // This is a child of another struct
        currentStruct().addStruct(uint32_t(_structs.size()) - 1);
    }
    
    _structStack.push_back(uint32_t(_structs.size() - 1));
    
    expect(Token::OpenBrace);
    
    while(structEntry()) { }
    
    expect(Token::CloseBrace);
    expect(Token::Semicolon);
    _structStack.pop_back();
    return true;
}

bool
CompileEngine::structEntry()
{
    if (strucT() || varStatement() || function() || init()) {
        return true;
    }
    
    return false;
}

bool
CompileEngine::constant()
{
    if (!match(Reserved::Const)) {
        return false;
    }
    
    Type t;
    std::string id;
    uint32_t val;
    
    expect(type(t), Compiler::Error::ExpectedType);
    
    // Only built-in types allowed for types
    expect(uint8_t(t) < StructTypeStart, Compiler::Error::ConstMustBeSimpleType);
    expect(identifier(id), Compiler::Error::ExpectedIdentifier);
    expect(value(val, t), Compiler::Error::ExpectedValue);
    expect(Token::Semicolon);

    expect(findConstant(id, t, val), Compiler::Error::DuplicateIdentifier);
    
    _constants.emplace_back(t, id, val);
    
    return true;
}

bool
CompileEngine::value(uint32_t& i, Type t)
{
    bool neg = false;
    if (match(Token::Minus)) {
        neg = true;
    }
    
    float f;
    if (floatValue(f)) {
        if (neg) {
            f = -f;
        }

        // If we're expecting an Integer, convert it
        if (t == Type::Int8) {
            i = roundf(f);
        } else {
            i = *(reinterpret_cast<int32_t*>(&f));
        }
        return true;
    }
    
    if (integerValue(i)) {
        if (neg) {
            i = -i;
        }
        
        // If we're expecting a float, convert it
        if (t == Type::Float) {
            f = float(i);
            i = *(reinterpret_cast<int32_t*>(&f));
        }
        return true;
    }
    return false;
}

bool
CompileEngine::varStatement()
{
    Type t = Type::None;
    std::string id;
    
    if (!type(t)) {
        return false;
    }

    bool isPointer = false;
    if (match(Token::Mul)) {
        isPointer = true;
    }
    
    bool haveOne = false;
    while (true) {
        if (!var(t, isPointer)) {
            if (!haveOne) {
                break;
            }
            expect(false, Compiler::Error::ExpectedVar);
        }
        
        haveOne = true;
        if (!match(Token::Comma)) {
            break;
        }
    }

    expect(Token::Semicolon);
    return true;
}

bool
CompileEngine::var(Type type, bool isPointer)
{
    std::string id;
    expect(identifier(id), Compiler::Error::ExpectedIdentifier);
    
    uint32_t size = 1;

    if (match(Token::OpenBracket)) {
        expect(integerValue(size), Compiler::Error::ExpectedValue);
        expect(Token::CloseBracket);
    }
    
    size *= sizeInBytes(type);

    // Put the var in the current struct unless we're in a function, then put it in _locals
    if (_inFunction) {
        expect(currentFunction().addLocal(id, type, size, isPointer), Compiler::Error::DuplicateIdentifier);
    } else {
        expect(!_structStack.empty(), Compiler::Error::InternalError);
        
        // FIXME: Need to deal with ptr and size
        expect(currentStruct().addLocal(id, type, size, false), Compiler::Error::DuplicateIdentifier);
    }
    
    // Check for an initializer. We only allow initializers on Int and Float
    if (match(Token::Equal)) {
        if (uint8_t(type) < StructTypeStart) {
            // Built-in type. Generate an expression
            ASTPtr ast = expression();
            expect(ast != nullptr, Compiler::Error::ExpectedExpr);
        } else {
            // Struct type, collect initializers
            expect(Token::OpenBrace);
            ASTPtr ast = expression();
            if (ast) {
                // FIXME: For now ignore the initializers
                while (match(Token::Comma)) {
                    ast = expression();
                    expect(ast != nullptr, Compiler::Error::ExpectedExpr);
                }
            }
            expect(Token::CloseBrace);
        }
    }
    
    return true;
}

bool
CompileEngine::type(Type& t)
{
    if (match(Reserved::Float)) {
        t = Type::Float;
        return true;
    }
    if (match(Reserved::Fixed)) {
        t = Type::Fixed;
        return true;
    }
    if (match(Reserved::Int8)) {
        t = Type::Int8;
        return true;
    }
    if (match(Reserved::UInt8)) {
        t = Type::Int8;
        return true;
    }
    if (match(Reserved::Int16)) {
        t = Type::Int16;
        return true;
    }
    if (match(Reserved::UInt16)) {
        t = Type::Int16;
        return true;
    }
    if (match(Reserved::Int32)) {
        t = Type::Int32;
        return true;
    }
    if (match(Reserved::UInt32)) {
        t = Type::Int32;
        return true;
    }
        
    // See if it's a struct
    std::string id;
    if (!identifier(id, false)) {
        return false;
    }
    
    auto it = find_if(_structs.begin(), _structs.end(),
                    [id](const Struct s) { return s.name() == id; });
    if (it != _structs.end()) {
        // Types from StructTypeStart - 0xff are structs. Make the enum the struct
        // index + StructTypeStart
        t = Type(StructTypeStart + (it - _structs.begin()));
        _scanner.retireToken();
        return true;
    }
    return false;
}

bool
CompileEngine::function()
{
    if (!match(Reserved::Function)) {
        return false;
    }
    
    _nextMem = 0;
    
    // Type is optional
    Type t = Type::None;
    type(t);
    
    std::string id;
    expect(identifier(id), Compiler::Error::ExpectedIdentifier);

    // Remember the function
    _functions.emplace_back(id, t);
    _inFunction = true;
    
    expect(Token::OpenParen);
    
    expect(formalParameterList(), Compiler::Error::ExpectedFormalParams);
    
    expect(Token::CloseParen);
    expect(Token::OpenBrace);

    // SetFrame has to be the first instruction in the Function. Pass Params and
    // set Locals byte to 0 and remember it's location so we can fix it at the
    // end of the function
//    addOpSingleByteIndex(Op::SetFrameS, currentFunction().args());
//    auto localsIndex = _rom8.size();
//    addInt(0);

    // Remember the rom addr so we can check to see if we've emitted any code
//    uint16_t size = romSize();
    
    while(statement()) { }
    
    expect(Token::CloseBrace);

    // Update locals size
//    _rom8[localsIndex] = currentFunction().localSize();

    // Set the high water mark
    if (_nextMem > _localHighWaterMark) {
        _localHighWaterMark = _nextMem;
    }
    
    // Emit Return at the end if there's not already one
//    if (size == romSize() || lastOp() != Op::Return) {
//        addOpSingleByteIndex(Op::PushIntConstS, 0);
//        addOp(Op::Return);
//    }
    
    _inFunction = false;
    return true;
}

bool
CompileEngine::init()
{
    if (!match(Reserved::Initialize)) {
        return false;
    }
    
    // Remember the function
    _functions.emplace_back();
    _inFunction = true;
    
    expect(Token::OpenParen);
    
    expect(formalParameterList(), Compiler::Error::ExpectedFormalParams);
    
    expect(Token::CloseParen);
    expect(Token::OpenBrace);

    // SetFrame has to be the first instruction in the Function. Pass Params and
    // set Locals byte to 0 and remember it's location so we can fix it at the
    // end of the function
//    addOpSingleByteIndex(Op::SetFrameS, currentFunction().args());
//    auto localsIndex = _rom8.size();
//    addInt(0);

    // Remember the rom addr so we can check to see if we've emitted any code
//    uint16_t size = romSize();
    
    while(statement()) { }
    
    expect(Token::CloseBrace);

    // Update locals size
//    _rom8[localsIndex] = currentFunction().localSize();

    // Set the high water mark
    if (_nextMem > _localHighWaterMark) {
        _localHighWaterMark = _nextMem;
    }
    
    // Emit Return at the end if there's not already one
//    if (size == romSize() || lastOp() != Op::Return) {
//        addOpSingleByteIndex(Op::PushIntConstS, 0);
//        addOp(Op::Return);
//    }
    
    _inFunction = false;
    return true;
}

bool
CompileEngine::statement()
{
    if (compoundStatement()) return true;
    if (ifStatement()) return true;
    if (forStatement()) return true;
    if (whileStatement()) return true;
    if (loopStatement()) return true;
    if (returnStatement()) return true;
    if (jumpStatement()) return true;
    if (varStatement()) return true;
    if (expressionStatement()) return true;
    return false;
}

bool
CompileEngine::compoundStatement()
{
    if (!match(Token::OpenBrace)) {
        return false;
    }

    // If we're in a function, remember the number of local variables
    // added so we can toss them at the end
    auto numLocals = 0;
    if (_inFunction) {
        numLocals = currentFunction().numLocals();
    }
    
    while(statement()) { }

    expect(Token::CloseBrace);
    
    // prune the locals added in this block
    if (_inFunction) {
        currentFunction().pruneLocals(currentFunction().numLocals() - numLocals);
    }
    return true;
}

bool
CompileEngine::ifStatement()
{
    if (!match(Reserved::If)) {
        return false;
    }
    
    expect(Token::OpenParen);
    
    expression();
    expect(Token::CloseParen);

//    auto ifJumpAddr = _rom8.size();
//    addOpTarg(Op::If, 0);

    statement();
    
    // This ifTargetAddr will be used if there is no else
//    uint16_t ifTargetAddr = _rom8.size();
    
    if (match(Reserved::Else)) {
//        auto elseJumpAddr = _rom8.size();
//        addOpTarg(Op::Jump, 0);
//        
//        // Set ifTargetAddr to jump to else clause
//        ifTargetAddr = _rom8.size();
        statement();

        // Resolve the else address
//        uint16_t offset = _rom8.size() - elseJumpAddr - 2;
//        _rom8[elseJumpAddr] |= uint8_t((offset >> 8) & 0x0f);
//        _rom8[elseJumpAddr + 1] = uint8_t(offset);
    }
    
    // Resolve the if address
//    uint16_t offset = ifTargetAddr - ifJumpAddr - 2;
//    _rom8[ifJumpAddr] |= uint8_t((offset >> 8) & 0x0f);
//    _rom8[ifJumpAddr + 1] = uint8_t(offset);

    return true;
}

bool
CompileEngine::forStatement()
{
    if (!match(Reserved::For)) {
        return false;
    }

    expect(Token::OpenParen);
    
    // Handle iterator init
    if (!match(Token::Semicolon)) {
        Type t = Type::None;
        type(t);
        
        // If we have a type this is a var declaration and must be an assignment
        // expression. Otherwise it can be a general arithmeticExpression
        ASTPtr ast;
        
        if (t != Type::None) {
            expect(t == Type::Int || t == Type::Float, Compiler::Error::WrongType);

            std::string id;
            expect(identifier(id), Compiler::Error::ExpectedIdentifier);
            expect(Token::Equal);

            // Generate an expression
            expect(_inFunction, Compiler::Error::InternalError);
            expect(currentFunction().addLocal(id, t, sizeInBytes(t), false), Compiler::Error::DuplicateIdentifier);
            _nextMem += 1;

            // FIXME: This needs to be an arithmeticExpression?
            ast = expression();
            expect(ast != nullptr, Compiler::Error::ExpectedExpr);
        } else {
            ast = expression();
            expect(ast != nullptr, Compiler::Error::ExpectedExpr);
        }
        expect(Token::Semicolon);
    }
    
    enterJumpContext();
    
    // The for loop has a loop test and an iteration expression. The loop expression
    // appears first, followed by an if test which breaks out of the loop if
    // false. The next instruction is a jump to the first intruction of the for
    // loop contents. This is followed by the iteration expression and then a Loop
    // instruction back to the first instruction of the loop expression. This is
    // followed by the first instruction of the for loop contents then a Loop back 
    // to the first instruction of the iteration expression. This tangled web looks
    // like this:
    //
    //                  <init expression>
    //      startAddr:  <loop expression>
    //                  If breakAddr
    //      stmtAddr:   <statement> (break jumps to breakAddr, continue jumps to contAddr)
    //      contAddr:   <iteration expression>
    //                  Jump startAddr
    //      breakAddr:
    //
//    uint16_t startAddr = _rom8.size();

    if (!match(Token::Semicolon)) {
        expression();

        // At this point the expresssion has been executed and the result is on TOS
        addJumpEntry(Op::If, JumpEntry::Type::Break);
        expect(Token::Semicolon);
    }
    
    // If we don't have an iteration, contAddr is just the startAddr
//    uint16_t contAddr = startAddr;
    
    // Handle iteration
//    uint8_t* iterBuf = nullptr;
//    uint16_t iterSize = 0;
    
    if (!match(Token::CloseParen)) {
        expression();
        
        // This must not be an assignment expression, so it must have
        // a leftover expr on the stack that we need to get rid of
        expect(Token::CloseParen);
        
        // Move the iteration code into the iterBuf.
//        iterSize = _rom8.size() - iterAddr;
//        if (iterSize > 0) {
//            iterBuf = new uint8_t[iterSize];
//            memcpy(iterBuf, &(_rom8[iterAddr]), iterSize);
//            _rom8.resize(iterAddr);
//        }
    }

    statement();
    
    // Retore the iteration code, if any
//    if (iterSize > 0) {
//        contAddr = _rom8.size();
//        _rom8.resize(_rom8.size() + iterSize);
//        memcpy(&_rom8[contAddr], iterBuf, iterSize);
//        delete [ ] iterBuf;
//    }

    addJumpEntry(Op::Jump, JumpEntry::Type::Start);

//    uint16_t breakAddr = _rom8.size();

    // Now resolve all the jumps
//    exitJumpContext(startAddr, contAddr, breakAddr);
    return true;
}

bool
CompileEngine::whileStatement()
{
    if (!match(Reserved::While)) {
        return false;
    }

    enterJumpContext();

    expect(Token::OpenParen);

    // Loop starts with an if test of expr
//    uint16_t loopAddr = _rom8.size();
    
    expression();

    addJumpEntry(Op::If, JumpEntry::Type::Break);

    expect(Token::CloseParen);
    
    statement();

    // Loop back to the beginning
    addJumpEntry(Op::Jump, JumpEntry::Type::Continue);
        
    // Now resolve all the jumps (stmtAddr will never be used so just reuse loopAddr)
//    exitJumpContext(loopAddr, loopAddr, _rom8.size());

    return true;
}

bool
CompileEngine::loopStatement()
{
    if (!match(Reserved::Loop)) {
        return false;
    }

    enterJumpContext();

//    uint16_t loopAddr = _rom8.size();
    
    statement();

    // Loop back to the beginning
    addJumpEntry(Op::Jump, JumpEntry::Type::Continue);
    
    // Now resolve all the jumps (stmtAddr will never be used so just reuse loopAddr)
//    exitJumpContext(loopAddr, loopAddr, _rom8.size());

    return true;
}

bool
CompileEngine::returnStatement()
{
    if (!match(Reserved::Return)) {
        return false;
    }
    
    ASTPtr ast = expression();
    if (ast) {
        // Push the return value
    } else {
        // If the function return type not None, we need a return value
        expect(currentFunction().returnType() == Type::None, Compiler::Error::MismatchedType);
    }
    
    expect(Token::Semicolon);
    return true;
}

bool
CompileEngine::jumpStatement()
{
    JumpEntry::Type type;
    
    if (match(Reserved::Break)) {
        type = JumpEntry::Type::Break;
    } else if (match(Reserved::Continue)) {
        type = JumpEntry::Type::Continue;
    } else {
        return false;
    }
    
    // Make sure we're in a loop
    expect(!_jumpList.empty(), Compiler::Error::OnlyAllowedInLoop);
    
    addJumpEntry(Op::Jump, type);

    expect(Token::Semicolon);
    return true;
}

bool
CompileEngine::expressionStatement()
{
    if (!assignmentExpression()) {
        return false;
    }
    
    // The exprStack may or may not have one entry.
    // If it does it means that there was an unused
    // value from the expression, for instance, a
    // return value from a function. If not it means
    // the expression ended in an assignment. Do 
    // what's needed.
    
    expect(Token::Semicolon);
    return true;
}

ASTPtr
CompileEngine::expression()
{
    return arithmeticExpression(primaryExpression(), 1);
}

ASTPtr
CompileEngine::assignmentExpression()
{
    return arithmeticExpression(unaryExpression(), 1);
}

ASTPtr
CompileEngine::arithmeticExpression(const ASTPtr& node, uint8_t minPrec)
{
    ASTPtr lhs = node;
    
    while(1) {
        OpInfo opInfo;
        if (!findOpInfo(Operator(_scanner.getToken()), opInfo) || opInfo.prec() < minPrec) {
            return lhs;
        }
        
        _scanner.retireToken();
        
        ASTPtr rhs = unaryExpression();
        
        while (true) {
            OpInfo nextOpInfo;
            if (!findOpInfo(Operator(_scanner.getToken()), nextOpInfo) || nextOpInfo.prec() < minPrec) {
                break;
            }
            
            rhs = arithmeticExpression(rhs, nextOpInfo.prec());
        }
        
        // Generate an ASTNode
        lhs = std::make_shared<BinaryOpNode>(opInfo.oper(), lhs, rhs);
    }
    
    return lhs;
}

ASTPtr
CompileEngine::unaryExpression()
{
    ASTPtr node = postfixExpression();
    if (node) {
        return node;
    }

    Operator oper;
    
    if (match(Token::Minus)) {
        oper = Operator(Token::Minus);
    } else if (match(Token::Twiddle)) {
        oper = Operator(Token::Twiddle);
    } else if (match(Token::Bang)) {
        oper = Operator(Token::Bang);
    } else if (match(Token::Inc)) {
        oper = Operator(Token::Inc);
    } else if (match(Token::Dec)) {
        oper = Operator(Token::Dec);
    } else if (match(Token::And)) {
        oper = Operator(Token::And);
    } else {
        return nullptr;
    }
    
    return std::make_shared<UnaryOpNode>(oper, unaryExpression());
}

ASTPtr
CompileEngine::postfixExpression()
{
    ASTPtr lhs = primaryExpression();
    if (!lhs) {
        return nullptr;
    }
    
    while (true) {
        if (match(Token::OpenParen)) {
            // FIXME: Handle function
            //expect(argumentList(fun), Compiler::Error::ExpectedArgList);
            expect(Token::CloseParen);
        } else if (match(Token::OpenBracket)) {
            ASTPtr rhs = expression();
            ASTPtr result = std::make_shared<BinaryOpNode>(Operator::ArrIdx, lhs, rhs);
            expect(Token::CloseBracket);
            return result;
        } else if (match(Token::Dot)) {
            std::string id;
            expect(identifier(id), Compiler::Error::ExpectedIdentifier);
            return std::make_shared<DotNode>(lhs, id);
        } else if (match(Token::Inc)) {
            return std::make_shared<UnaryOpNode>(Operator::Inc, lhs);
        } else if (match(Token::Dec)) {
            return std::make_shared<UnaryOpNode>(Operator::Dec, lhs);
        } else {
            return lhs;
        }
    }
}

ASTPtr
CompileEngine::primaryExpression()
{
    if (match(Token::OpenParen)) {
        ASTPtr ast = expression();
        expect(!ast, Compiler::Error::ExpectedExpr);
        expect(Token::CloseParen);
        return ast;
    }
    
    std::string id;
    if (identifier(id)) {
        expect(_inFunction, Compiler::Error::InternalError);
        uint32_t symbolIndex;
        if (findSymbol(id, symbolIndex)) {
            return std::make_shared<VarNode>(symbolIndex);
        }
        
        Type t;
        uint32_t v;
        if (findConstant(id, t, v)) {
            return std::make_shared<ConstantNode>(t, v);
        }
        expect(false, Compiler::Error::ExpectedVar);
    }
    
    float f;
    if (floatValue(f)) {
        return std::make_shared<ConstantNode>(f);
    }
        
    uint32_t i;
    if (integerValue(i)) {
        return std::make_shared<ConstantNode>(i);
    }
    
    return nullptr;
}

bool
CompileEngine::formalParameterList()
{
    Type t;
    while (true) {
        if (!type(t)) {
            return true;
        }
        
        bool isPointer = false;
        if (match(Token::Mul)) {
            isPointer = true;
        }
    
        std::string id;
        expect(identifier(id), Compiler::Error::ExpectedIdentifier);
        currentFunction().addArg(id, t, sizeInBytes(t), isPointer);
        
        if (!match(Token::Comma)) {
            return true;
        }
    }
    
    return true;
}

bool
CompileEngine::argumentList(const Function& fun)
{
    int i = 0;
    while (true) {
        if (!expression()) {
            if (i == 0) {
                break;
            }
            expect(false, Compiler::Error::ExpectedExpr);
        }
        
        i++;
        
        //expect(fun.args() >= i, Compiler::Error::WrongNumberOfArgs);
    
        // Bake the arithmeticExpression, leaving the result in r0.
        // Make sure the type matches the formal argument and push

        if (!match(Token::Comma)) {
            break;
        }
    }

    //expect(fun.args() == i, Compiler::Error::WrongNumberOfArgs);
    return true;
}

bool
CompileEngine::identifier(std::string& id, bool retire)
{
    if (_scanner.getToken() != Token::Identifier) {
        return false;
    }
    
    if (reserved()) {
        return false;
    }
    
    id = _scanner.getTokenString();
    
    if (retire) {
        _scanner.retireToken();
    }
    return true;
}

bool
CompileEngine::integerValue(uint32_t& i)
{
    if (_scanner.getToken() != Token::Integer) {
        return false;
    }
    
    i = _scanner.getTokenValue().integer;
    _scanner.retireToken();
    return true;
}

bool
CompileEngine::floatValue(float& f)
{
    if (_scanner.getToken() != Token::Float) {
        return false;
    }
    
    f = _scanner.getTokenValue().number;
    _scanner.retireToken();
    return true;
}

bool
CompileEngine::stringValue(std::string& str)
{
    if (_scanner.getToken() != Token::String) {
        return false;
    }
    
    str = _scanner.getTokenString();
    _scanner.retireToken();
    return true;
}

void
CompileEngine::expect(Token token, const char* str)
{
    bool err = false;
    if (_scanner.getToken() != token) {
        _error = Compiler::Error::ExpectedToken;
        err = true;
    }
    
    if (str && _scanner.getTokenString() != str) {
        _error = Compiler::Error::ExpectedToken;
        err = true;
    }
    
    if (err) {
        _expectedToken = token;
        if (str) {
            _expectedString = str;
        } else if (uint8_t(token) < 0x80) {
            _expectedString = char(token);
        } else {
            _expectedString = "";
        }
        throw true;
    }

    _scanner.retireToken();
}

void
CompileEngine::expect(bool passed, Compiler::Error error)
{
    if (!passed) {
        _error = error;
        throw true;
    }
}

void
CompileEngine::expectWithoutRetire(Token token)
{
    if (_scanner.getToken() != token) {
        _expectedToken = token;
        _expectedString = "";
        _error = Compiler::Error::ExpectedToken;
        throw true;
    }
}

bool
CompileEngine::match(Reserved r)
{
    Reserved rr;
    if (!isReserved(_scanner.getToken(), _scanner.getTokenString(), rr)) {
        return false;
    }
    if (r != rr) {
        return false;
    }
    _scanner.retireToken();
    return true;
}

bool
CompileEngine::match(Token t)
{
    if (_scanner.getToken() != t) {
        return false;
    }
    _scanner.retireToken();
    return true;
}

bool
CompileEngine::reserved()
{
    Reserved r;
    return isReserved(_scanner.getToken(), _scanner.getTokenString(), r);
}

bool
CompileEngine::reserved(Reserved &r)
{
    return isReserved(_scanner.getToken(), _scanner.getTokenString(), r);
}

bool
CompileEngine::isReserved(Token token, const std::string str, Reserved& r)
{
    static std::map<std::string, Reserved> reserved = {
        { "const",      Reserved::Const },
        { "import",     Reserved::Import },
        { "as",         Reserved::As },
        { "function",   Reserved::Function },
        { "initialize", Reserved::Initialize },
        { "for",        Reserved::For },
        { "if",         Reserved::If },
        { "else",       Reserved::Else },
        { "struct",     Reserved::Struct },
        { "return",     Reserved::Return },
        { "break",      Reserved::Break },
        { "continue",   Reserved::Continue },
        { "while",      Reserved::While },
        { "loop",       Reserved::Loop },
        { "float",      Reserved::Float },
        { "fixed",      Reserved::Fixed },
        { "int8",       Reserved::Int8 },
        { "uint8",      Reserved::UInt8 },
        { "int16",      Reserved::Int16 },
        { "uint16",     Reserved::UInt16 },
        { "int32",      Reserved::Int32 },
        { "uint32",     Reserved::UInt32 },
    };

    if (token != Token::Identifier) {
        return false;
    }
    
    auto it = reserved.find(str);
    if (it != reserved.end()) {
        r = it->second;
        return true;
    }
    return false;
}

uint8_t
CompileEngine::findInt(int32_t i)
{
    // Try to find an existing int const. If found, return
    // its address. If not found, create one and return 
    // that address.
    
// FIXME: How do we deal with constants
//    auto it = find_if(_rom32.begin(), _rom32.end(),
//                    [i](uint32_t v) { return uint32_t(i) == v; });
//    if (it != _rom32.end()) {
//        return it - _rom32.begin();
//    }
//    
//    _rom32.push_back(uint32_t(i));
//    return _rom32.size() - 1;

    return 0;
}

const
Function&
CompileEngine::handleFunctionName()
{
    std::string targ;
    expect(identifier(targ), Compiler::Error::ExpectedIdentifier);
    
    auto it = find_if(_functions.begin(), _functions.end(),
                    [targ](const Function& fun) { return fun.name() == targ; });
    expect(it != _functions.end(), Compiler::Error::UndefinedIdentifier);

    return *it;
}

uint8_t
CompileEngine::findFloat(float f)
{
    // Try to find an existing fp const. If found, return
    // its address. If not found, create one and return 
    // that address.
    uint32_t i = floatToInt(f);
    (void) i;
//    auto it = find_if(_rom32.begin(), _rom32.end(),
//                    [i](uint32_t v) { return i == v; });
//    if (it != _rom32.end()) {
//        return it - _rom32.begin();
//    }
//    
//    _rom32.push_back(i);
//    return _rom32.size() - 1;

// FIXME: How do we deal with constants

    return 0;
}

bool
CompileEngine::findFunction(const std::string& s, Function& fun)
{
    auto it = find_if(_functions.begin(), _functions.end(),
                    [s](const Function& fun) { return fun.name() == s; });

    if (it != _functions.end()) {
        fun = *it;
        return true;
    }
    
    return false;
}

bool
CompileEngine::findStruct(const std::string& id, uint32_t& structIndex)
{
    auto it = find_if(_structs.begin(), _structs.end(),
                    [id](const Struct s) { return s.name() == id; });
    if (it != _structs.end()) {
        structIndex = uint32_t(_structs.begin() - it);
        return true;
    }
    return false;
}

bool
CompileEngine::findSymbol(const std::string& s, uint32_t& symbolIndex)
{
    // First look in the current function and then in the parent struct
    if (currentFunction().findLocal(s, symbolIndex)) {
        return true;
    }
    
    // Next look in the current struct
    Struct& strucT = currentStruct();
    if (strucT.findLocal(s, symbolIndex)) {
        return true;
    }
    
    // Finally look up the struct chain
    for (auto i : strucT.structIndexes()) {
        if (_structs[i].findLocal(s, symbolIndex)) {
            return true;
        }
    }
    return false;
}

bool
CompileEngine::isExprFunction()
{
//    expect(!_exprStack.empty(), Compiler::Error::InternalError);
    
    Function fun;
    return true; //findFunction(_exprStack.back(), fun);
}

bool
CompileEngine::structFromType(Type type, Struct& s)
{
    if (uint8_t(type) < StructTypeStart) {
        return false;
    }
    uint8_t index = uint8_t(type) - StructTypeStart;
    expect(index < _structs.size(), Compiler::Error::InternalError);
    
    s = _structs[index];
    return true;
}

void
CompileEngine::exitJumpContext(uint16_t startAddr, uint16_t contAddr, uint16_t breakAddr)
{
    expect(!_jumpList.empty(), Compiler::Error::InternalError);

    // Go through all the entries in the last _jumpList entry and fill in
    // the addresses.
    for (const auto& it : _jumpList.back()) {
        expect(it._addr < breakAddr, Compiler::Error::InternalError);
        
        uint16_t addr;
        
        switch(it._type) {
            case JumpEntry::Type::Start: addr = startAddr; break;
            case JumpEntry::Type::Continue: addr = contAddr; break;
            case JumpEntry::Type::Break: addr = breakAddr; break;
        }         
         
//        int16_t offset = int16_t(addr) - int16_t(it._addr) - 2;
         
//        expect(_rom8[it._addr + 1] == 0, Compiler::Error::InternalError);
//        
//        _rom8[it._addr] |= (offset >> 8) & 0x0f;
//        _rom8[it._addr + 1] = uint8_t(offset);
    }
    
    _jumpList.pop_back();
}

void
CompileEngine::addJumpEntry(Op op, JumpEntry::Type type)
{
    expect(!_jumpList.empty(), Compiler::Error::InternalError);
    
//    uint16_t addr = _rom8.size();
//    addOpTarg(op, 0);
//    _jumpList.back().emplace_back(type, addr);
}

uint16_t
CompileEngine::sizeInBytes(Type type) const
{
    switch(type) {
        case Type::UInt8:
        case Type::Int8:    return 1;
        case Type::Fixed:
        case Type::UInt16:
        case Type::Int16:   return 2;
        case Type::Float:
        case Type::UInt32:
        case Type::Int32:   return 4;
        default:
            // Handle Structs
            int16_t structIndex = uint16_t(type) < StructTypeStart;
            if (structIndex < 0 || structIndex > (255 - StructTypeStart)) {
                return 0;
            }
            return _structs[structIndex].size();
    }
}

