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

using namespace lucid;

static std::vector<OpData> _opcodes = {
    { "Push",           Op::Push            , OpParams::Id },
    { "Pop",            Op::Pop             , OpParams::Id },
    { "PushIntConst",   Op::PushIntConst    , OpParams::Const },
    { "PushIntConstS",  Op::PushIntConstS   , OpParams::Index },

    { "PushRef",        Op::PushRef         , OpParams::Id },
    { "PushDeref",      Op::PushDeref       , OpParams::None },
    { "PopDeref",       Op::PopDeref        , OpParams::None },

//    { "Dup",            Op::Dup             , OpParams::None },
//    { "Drop",           Op::Drop            , OpParams::None },
//    { "Swap",           Op::Swap            , OpParams::None },
    
    { "if",             Op::If              , OpParams::RelTarg },

    { "Call",           Op::Call            , OpParams::AbsTarg },
    { "CallNative",     Op::CallNative      , OpParams::Const },
    { "Return",         Op::Return          , OpParams::None },
    { "SetFrame",       Op::SetFrameS       , OpParams::P_L },

    { "Jump",           Op::Jump            , OpParams::RelTarg },
    { "log",            Op::Log             , OpParams::Idx_Len_S },
    
    { "Or",             Op::Or              , OpParams::None },
    { "Xor",            Op::Xor             , OpParams::None },
    { "And",            Op::And             , OpParams::None },
    { "Not",            Op::Not             , OpParams::None },
    { "LOr",            Op::LOr             , OpParams::None },
    { "LAnd",           Op::LAnd            , OpParams::None },
    { "LNot",           Op::LNot            , OpParams::None },
    { "LTInt",          Op::LTInt           , OpParams::None },
    { "LTFloat",        Op::LTFloat         , OpParams::None },
    { "LEInt",          Op::LEInt           , OpParams::None },
    { "LEFloat",        Op::LEFloat         , OpParams::None },
    { "EQInt",          Op::EQInt           , OpParams::None },
    { "EQFloat",        Op::EQFloat         , OpParams::None },
    { "NEInt",          Op::NEInt           , OpParams::None },
    { "NEFloat",        Op::NEFloat         , OpParams::None },
    { "GEInt",          Op::GEInt           , OpParams::None },
    { "GEFloat",        Op::GEFloat         , OpParams::None },
    { "GTInt",          Op::GTInt           , OpParams::None },
    { "GTFloat",        Op::GTFloat         , OpParams::None },
    { "AddInt",         Op::AddInt          , OpParams::None },
    { "AddFloat",       Op::AddFloat        , OpParams::None },
    { "SubInt",         Op::SubInt          , OpParams::None },
    { "SubFloat",       Op::SubFloat        , OpParams::None },
    { "MulInt",         Op::MulInt          , OpParams::None },
    { "MulFloat",       Op::MulFloat        , OpParams::None },
    { "DivInt",         Op::DivInt          , OpParams::None },
    { "DivFloat",       Op::DivFloat        , OpParams::None },
    { "NegInt",         Op::NegInt          , OpParams::None },
    { "NegFloat",       Op::NegFloat        , OpParams::None },

    { "PreIncInt",      Op::PreIncInt       , OpParams::None },
    { "PreIncFloat",    Op::PreIncFloat     , OpParams::None },
    { "PreDecInt",      Op::PreDecInt       , OpParams::None },
    { "PreDecFloat",    Op::PreDecFloat     , OpParams::None },
    { "PostIncInt",     Op::PostIncInt      , OpParams::None },
    { "PostIncFloat",   Op::PostIncFloat    , OpParams::None },
    { "PostDecInt",     Op::PostDecInt      , OpParams::None },
    { "PostDecFloat",   Op::PostDecFloat    , OpParams::None },
    
    { "Offset",         Op::Offset          , OpParams::Index },
    { "Index",          Op::Index           , OpParams::Index },
};

bool
CompileEngine::opInfo(Token token, OpInfo& op) const
{
    static std::vector<OpInfo> opInfo = {
        { Token::Equal,     1, Op::Pop,     Op::Pop,      OpInfo::Assign::Only , Type::None },
        { Token::AddSto,    1, Op::AddInt,  Op::AddFloat, OpInfo::Assign::Op   , Type::None },
        { Token::SubSto,    1, Op::SubInt,  Op::SubFloat, OpInfo::Assign::Op   , Type::None },
        { Token::MulSto,    1, Op::MulInt,  Op::MulFloat, OpInfo::Assign::Op   , Type::None },
        { Token::DivSto,    1, Op::DivInt,  Op::DivFloat, OpInfo::Assign::Op   , Type::None },
        { Token::AndSto,    1, Op::And,     Op::None,     OpInfo::Assign::Op   , Type::Int },
        { Token::OrSto,     1, Op::Or,      Op::None,     OpInfo::Assign::Op   , Type::Int },
        { Token::XorSto,    1, Op::Xor,     Op::None,     OpInfo::Assign::Op   , Type::Int },
        { Token::LOr,       6, Op::LOr,     Op::None,     OpInfo::Assign::None , Type::Int },
        { Token::LAnd,      7, Op::LAnd,    Op::None,     OpInfo::Assign::None , Type::Int },
        { Token::Or,        8, Op::Or,      Op::None,     OpInfo::Assign::None , Type::Int },
        { Token::Xor,       9, Op::Xor,     Op::None,     OpInfo::Assign::None , Type::Int },
        { Token::And,      10, Op::And,     Op::None,     OpInfo::Assign::None , Type::Int },
        { Token::EQ,       11, Op::EQInt,   Op::EQFloat,  OpInfo::Assign::None , Type::Int },
        { Token::NE,       11, Op::NEInt,   Op::NEFloat,  OpInfo::Assign::None , Type::Int },
        { Token::LT,       12, Op::LTInt,   Op::LTFloat,  OpInfo::Assign::None , Type::Int },
        { Token::GT,       12, Op::GTInt,   Op::GTFloat,  OpInfo::Assign::None , Type::Int },
        { Token::GE,       12, Op::GEInt,   Op::GEFloat,  OpInfo::Assign::None , Type::Int },
        { Token::LE,       12, Op::LEInt,   Op::LEFloat,  OpInfo::Assign::None , Type::Int },
        { Token::Plus,     14, Op::AddInt,  Op::AddFloat, OpInfo::Assign::None , Type::None },
        { Token::Minus,    14, Op::SubInt,  Op::SubFloat, OpInfo::Assign::None , Type::None },
        { Token::Mul,      15, Op::MulInt,  Op::MulFloat, OpInfo::Assign::None , Type::None },
        { Token::Div,      15, Op::DivInt,  Op::DivFloat, OpInfo::Assign::None , Type::None },
        { Token::Mod,      15, Op::ModInt,  Op::ModFloat, OpInfo::Assign::None , Type::None },
    };

    auto it = find_if(opInfo.begin(), opInfo.end(),
                    [token](const OpInfo& op) { return op.token() == token; });
    if (it != opInfo.end()) {
        op = *it;
        return true;
    }
    return false;
}

void
CompileEngine::emit(std::vector<uint8_t>& executable)
{
    executable.push_back('l');
    executable.push_back('u');
    executable.push_back('c');
    executable.push_back('d');

    char* buf = reinterpret_cast<char*>(&(_rom8[0]));
    executable.insert(executable.end(), buf, buf + _rom8.size());
}

bool
CompileEngine::program()
{
    _scanner.setIgnoreNewlines(true);
    
    try {
        while(import()) { }
        while(strucT()) { }
        
        // Handle top level struct if it exists
        Type t;
        std::string id;
        if (type(t)) {
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
    
    std::string id;
    if (identifier(id)) {
        while (match(Token::Comma)) {
            expect(identifier(id), Compiler::Error::ExpectedIdentifier);
        }
        expect(match(Reserved::From), Compiler::Error::ExpectedKeyword);
    }
    
    expect(identifier(id), Compiler::Error::ExpectedIdentifier);
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
    _structStack.push_back(uint32_t(_structs.size() - 1));
    
    expect(Token::OpenBrace);
    
    while(structEntry()) { }
    
    expect(Token::CloseBrace);
    expect(Token::Semicolon);
    _structStack.pop_back();
    return true;
}

// structEntry:
//     constant | varStatement | function | ctor | dtor ;
//
bool
CompileEngine::structEntry()
{
    // FIXME: Handle ctor and dtor
    if (constant() || varStatement() || function() || init()) {
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
    int32_t val;
    
    expect(type(t), Compiler::Error::ExpectedType);
    expect(identifier(id), Compiler::Error::ExpectedIdentifier);
    expect(Token::Semicolon);
    expect(value(val, t), Compiler::Error::ExpectedValue);
    
// FIXME: What do we do with constants?
//    if (t == Type::Int8 && val >= -128 && val <= 127) {
//        _defs.emplace_back(id, val);
//    } else {
//        expect(addGlobal(id, _rom32.size(), t, Symbol::Storage::Const), Compiler::Error::DuplicateIdentifier);
//        _rom32.push_back(val);
//    }
    
    return true;
}

bool
CompileEngine::value(int32_t& i, Type t)
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
        // Wasn't any of the built-in types. See if it's a struct definition
        if (strucT()) {
            t = Type::Struct;
            return true;
        }
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
    
    int32_t size = 1;

    if (match(Token::OpenBracket)) {
        expect(integerValue(size), Compiler::Error::ExpectedValue);
        expect(Token::CloseBracket);

    }
    
    size *= elementSize(type);

    // Put the var in the current struct unless we're in a function, then put it in _locals
    if (_inFunction) {
        expect(currentFunction().addLocal(id, type, isPointer, size), Compiler::Error::DuplicateIdentifier);
    } else {
        expect(!_structStack.empty(), Compiler::Error::InternalError);
        
        // FIXME: Need to deal with ptr and size
        expect(currentStruct().addLocal(id, type, false, 1), Compiler::Error::DuplicateIdentifier);
    }
    
    // Check for an initializer. We only allow initializers on Int and Float
    if (match(Token::Equal)) {
        if (uint8_t(type) < StructTypeStart) {
            // Built-in type. Generate an assignment expression
            _exprStack.emplace_back(id);
            bakeExpr(ExprAction::Ref);
            expect(arithmeticExpression(), Compiler::Error::ExpectedExpr);
            expect(bakeExpr(ExprAction::Right, type) == type, Compiler::Error::WrongType);
            expect(bakeExpr(ExprAction::Left, type) == type, Compiler::Error::MismatchedType);
        } else {
            // Struct type, collect initializers
            expect(Token::OpenBrace);
            if (arithmeticExpression()) {
                // FIXME: For now ignore the initializers
                _exprStack.pop_back();
                while (match(Token::Comma)) {
                    expect(arithmeticExpression(), Compiler::Error::ExpectedExpr);
                    _exprStack.pop_back();
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
    _functions.emplace_back(id, uint16_t(_rom8.size()), t);
    _inFunction = true;
    
    expect(Token::OpenParen);
    
    expect(formalParameterList(), Compiler::Error::ExpectedFormalParams);
    
    expect(Token::CloseParen);
    expect(Token::OpenBrace);

    // SetFrame has to be the first instruction in the Function. Pass Params and
    // set Locals byte to 0 and remember it's location so we can fix it at the
    // end of the function
    addOpSingleByteIndex(Op::SetFrameS, currentFunction().args());
    auto localsIndex = _rom8.size();
    addInt(0);

    // Remember the rom addr so we can check to see if we've emitted any code
    uint16_t size = romSize();
    
    while(statement()) { }
    
    expect(Token::CloseBrace);

    // Update locals size
    _rom8[localsIndex] = currentFunction().localSize();

    // Set the high water mark
    if (_nextMem > _localHighWaterMark) {
        _localHighWaterMark = _nextMem;
    }
    
    // Emit Return at the end if there's not already one
    if (size == romSize() || lastOp() != Op::Return) {
        addOpSingleByteIndex(Op::PushIntConstS, 0);
        addOp(Op::Return);
    }
    
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
    // FIXME: probably don't save a string as the function name
    _functions.emplace_back("initialize", uint16_t(_rom8.size()), Type::None);
    _inFunction = true;
    
    expect(Token::OpenParen);
    
    expect(formalParameterList(), Compiler::Error::ExpectedFormalParams);
    
    expect(Token::CloseParen);
    expect(Token::OpenBrace);

    // SetFrame has to be the first instruction in the Function. Pass Params and
    // set Locals byte to 0 and remember it's location so we can fix it at the
    // end of the function
    addOpSingleByteIndex(Op::SetFrameS, currentFunction().args());
    auto localsIndex = _rom8.size();
    addInt(0);

    // Remember the rom addr so we can check to see if we've emitted any code
    uint16_t size = romSize();
    
    while(statement()) { }
    
    expect(Token::CloseBrace);

    // Update locals size
    _rom8[localsIndex] = currentFunction().localSize();

    // Set the high water mark
    if (_nextMem > _localHighWaterMark) {
        _localHighWaterMark = _nextMem;
    }
    
    // Emit Return at the end if there's not already one
    if (size == romSize() || lastOp() != Op::Return) {
        addOpSingleByteIndex(Op::PushIntConstS, 0);
        addOp(Op::Return);
    }
    
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
    
    arithmeticExpression();
    expect(bakeExpr(ExprAction::Right) == Type::Int, Compiler::Error::WrongType);
    expect(Token::CloseParen);

    auto ifJumpAddr = _rom8.size();
    addOpTarg(Op::If, 0);

    statement();
    
    // This ifTargetAddr will be used if there is no else
    uint16_t ifTargetAddr = _rom8.size();
    
    if (match(Reserved::Else)) {
        auto elseJumpAddr = _rom8.size();
        addOpTarg(Op::Jump, 0);
        
        // Set ifTargetAddr to jump to else clause
        ifTargetAddr = _rom8.size();
        statement();

        // Resolve the else address
        uint16_t offset = _rom8.size() - elseJumpAddr - 2;
        _rom8[elseJumpAddr] |= uint8_t((offset >> 8) & 0x0f);
        _rom8[elseJumpAddr + 1] = uint8_t(offset);
    }
    
    // Resolve the if address
    uint16_t offset = ifTargetAddr - ifJumpAddr - 2;
    _rom8[ifJumpAddr] |= uint8_t((offset >> 8) & 0x0f);
    _rom8[ifJumpAddr + 1] = uint8_t(offset);

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
        if (t != Type::None) {
            expect(t == Type::Int || t == Type::Float, Compiler::Error::WrongType);

            std::string id;
            expect(identifier(id), Compiler::Error::ExpectedIdentifier);
            expect(Token::Equal);

            // Generate an assignment expression
            expect(_inFunction, Compiler::Error::InternalError);
            expect(currentFunction().addLocal(id, t, false, 1), Compiler::Error::DuplicateIdentifier);
            _nextMem += 1;

            _exprStack.emplace_back(id);
            bakeExpr(ExprAction::Ref);
            expect(arithmeticExpression(), Compiler::Error::ExpectedExpr);
            expect(bakeExpr(ExprAction::Right, t) == t, Compiler::Error::WrongType);
            expect(bakeExpr(ExprAction::Left, t) == t, Compiler::Error::MismatchedType);
        } else {
            expect(assignmentExpression(), Compiler::Error::ExpectedExpr);
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
    uint16_t startAddr = _rom8.size();

    if (!match(Token::Semicolon)) {
        arithmeticExpression();
        expect(bakeExpr(ExprAction::Right) == Type::Int, Compiler::Error::WrongType);

        // At this point the expresssion has been executed and the result is on TOS
        addJumpEntry(Op::If, JumpEntry::Type::Break);
        expect(Token::Semicolon);
    }
    
    // If we don't have an iteration, contAddr is just the startAddr
    uint16_t contAddr = startAddr;
    
    // Handle iteration
    uint8_t* iterBuf = nullptr;
    uint16_t iterSize = 0;
    
    if (!match(Token::CloseParen)) {
        uint16_t iterAddr = _rom8.size();

        arithmeticExpression();
        
        // This must not be an assignment expression, so it must have
        // a leftover expr on the stack that we need to get rid of
        expect(_exprStack.size() == 1, Compiler::Error::InternalError);
        _exprStack.pop_back();
//        addOp(Op::Drop);
        expect(Token::CloseParen);
        
        // Move the iteration code into the iterBuf.
        iterSize = _rom8.size() - iterAddr;
        if (iterSize > 0) {
            iterBuf = new uint8_t[iterSize];
            memcpy(iterBuf, &(_rom8[iterAddr]), iterSize);
            _rom8.resize(iterAddr);
        }
    }

    statement();
    
    // Retore the iteration code, if any
    if (iterSize > 0) {
        contAddr = _rom8.size();
        _rom8.resize(_rom8.size() + iterSize);
        memcpy(&_rom8[contAddr], iterBuf, iterSize);
        delete [ ] iterBuf;
    }

    addJumpEntry(Op::Jump, JumpEntry::Type::Start);

    uint16_t breakAddr = _rom8.size();

    // Now resolve all the jumps
    exitJumpContext(startAddr, contAddr, breakAddr);
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
    uint16_t loopAddr = _rom8.size();
    
    arithmeticExpression();
    expect(bakeExpr(ExprAction::Right) == Type::Int, Compiler::Error::WrongType);

    addJumpEntry(Op::If, JumpEntry::Type::Break);

    expect(Token::CloseParen);
    
    statement();

    // Loop back to the beginning
    addJumpEntry(Op::Jump, JumpEntry::Type::Continue);
        
    // Now resolve all the jumps (stmtAddr will never be used so just reuse loopAddr)
    exitJumpContext(loopAddr, loopAddr, _rom8.size());

    return true;
}

bool
CompileEngine::loopStatement()
{
    if (!match(Reserved::Loop)) {
        return false;
    }

    enterJumpContext();

    uint16_t loopAddr = _rom8.size();
    
    statement();

    // Loop back to the beginning
    addJumpEntry(Op::Jump, JumpEntry::Type::Continue);
    
    // Now resolve all the jumps (stmtAddr will never be used so just reuse loopAddr)
    exitJumpContext(loopAddr, loopAddr, _rom8.size());

    return true;
}

bool
CompileEngine::returnStatement()
{
    if (!match(Reserved::Return)) {
        return false;
    }
    
    if (arithmeticExpression()) {
        // Push the return value
        expect(bakeExpr(ExprAction::Right) == currentFunction().type(), Compiler::Error::MismatchedType);
    } else {
        // If the function return type not None, we need a return value
        expect(currentFunction().type() == Type::None, Compiler::Error::MismatchedType);
        
        // No return value, push a zero
        addOpSingleByteIndex(Op::PushIntConstS, 0);
    }
    
    addOp(Op::Return);
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
    if (!_exprStack.empty()) {
        expect(_exprStack.size() == 1, Compiler::Error::InternalError);
        _exprStack.pop_back();
//        addOp(Op::Drop);
    }
    
    expect(Token::Semicolon);
    return true;
}

bool
CompileEngine::arithmeticExpression(uint8_t minPrec, ArithType arithType)
{
    if (!unaryExpression()) {
        return false;
    }
    
    while(1) {
        OpInfo info;
        if (!opInfo(_scanner.getToken(), info) || info.prec() < minPrec) {
            return true;
        }
        
        // If this is an assignment operator, we only allow one so handle it separately
        uint8_t nextMinPrec = info.prec() + 1;
        _scanner.retireToken();
                
        expect(!(arithType != ArithType::Assign && info.assign() != OpInfo::Assign::None), Compiler::Error::AssignmentNotAllowedHere);
        
        Type leftType = Type::None;
        Type rightType = Type::None;
        
        if (info.assign() != OpInfo::Assign::None) {
            // Turn TOS into Ref
            leftType = bakeExpr(ExprAction::Ref);
        } else {
            leftType = bakeExpr(ExprAction::Right);
        }
        
        if (info.assign() == OpInfo::Assign::Op) {
            // The ref is on TOS, dup and get the value. That will
            // leave the value, then the ref to that value on the stack.
//            addOp(Op::Dup);
            addOp(Op::PushDeref);
        }
        
        expect(arithmeticExpression(nextMinPrec), Compiler::Error::ExpectedExpr);

        rightType = bakeExpr(ExprAction::Right, leftType);

        switch(info.assign()) {
            case OpInfo::Assign::Only: {
                break;
            }
            case OpInfo::Assign::Op:
                expect(leftType == rightType, Compiler::Error::MismatchedType);
                addOp((leftType == Type::Int) ? info.intOp() : info.floatOp());
                break;
            case OpInfo::Assign::None: {
                expect(leftType == rightType, Compiler::Error::MismatchedType);
                
                Op op = (leftType == Type::Int) ? info.intOp() : info.floatOp();
                expect(op != Op::None, Compiler::Error::WrongType);
                addOp(op);

                if (info.resultType() != Type::None) {
                    leftType = info.resultType();
                }
                _exprStack.push_back(ExprEntry::Value(leftType));
                break;
            }
            default: break;
        }
        
        if (info.assign() != OpInfo::Assign::None) {
            expect(bakeExpr(ExprAction::Left, rightType) == rightType, Compiler::Error::MismatchedType);
        }
    }
    
    return true;
}

bool
CompileEngine::unaryExpression()
{
    if (postfixExpression()) {
        return true;
    }

    Token token;
    
    if (match(Token::Minus)) {
        token = Token::Minus;
    } else if (match(Token::Twiddle)) {
        token = Token::Twiddle;
    } else if (match(Token::Bang)) {
        token = Token::Bang;
    } else if (match(Token::Inc)) {
        token = Token::Inc;
    } else if (match(Token::Dec)) {
        token = Token::Dec;
    } else if (match(Token::And)) {
        token = Token::And;
    } else {
        return false;
    }
    
    expect(unaryExpression(), Compiler::Error::ExpectedExpr);
    
    // If this is ampersand, make it into a pointer, otherwise bake it into a value
    Type type;

    switch(token) {
        default:
            break;
        case Token::And:
            // Bake this as a Ref. That means there will be a Ref to
            // a var on the stack. Change the ref into a ref ptr
            type = bakeExpr(ExprAction::Ref);
            _exprStack.pop_back();
            _exprStack.push_back(ExprEntry::Ref(type, true));
            break;
        case Token::Inc:
        case Token::Dec:
            type = bakeExpr(ExprAction::Ref);
            _exprStack.pop_back();
            _exprStack.push_back(ExprEntry::Value(type));

            if (type == Type::Float) {
                addOp((token == Token::Inc) ? Op::PreIncFloat : Op::PreDecFloat);
            } else {
                expect(type == Type::Int, Compiler::Error::MismatchedType);
                addOp((token == Token::Inc) ? Op::PreIncInt : Op::PreDecInt);
            }
            break;
        case Token::Minus:
        case Token::Twiddle:
        case Token::Bang:
            // if the exprStack has an Int or Float constant, apply the operator
            // to that value rather than generating an Op. It avoids an
            // unnecessary instruction and in the case of minus, allows an
            // implicit conversion to float when needed.
            expect(!_exprStack.empty(), Compiler::Error::InternalError);
            ExprEntry::Type exprType = _exprStack.back().type();
            if (exprType == ExprEntry::Type::Float && token == Token::Minus) {
                float f = _exprStack.back();
                f = -f;
                _exprStack.pop_back();
                _exprStack.push_back(f);
                break;
            }
            
            if (exprType == ExprEntry::Type::Int) {
                int32_t i = _exprStack.back();
                if (token == Token::Minus) {
                    i = -i;
                } else if (token == Token::Twiddle) {
                    i = ~i;
                } else if (token == Token::Bang) {
                    i = !i;
                }
                _exprStack.pop_back();
                _exprStack.push_back(i);
                break;
            }
            
            type = bakeExpr(ExprAction::Right);
            _exprStack.push_back(ExprEntry::Value(type));

            if (token == Token::Minus) {
                if (type == Type::Float) {
                    addOp(Op::NegFloat);
                } else {
                    expect(type == Type::Int, Compiler::Error::MismatchedType);
                    addOp(Op::NegInt);
                }
            } else {
                expect(type == Type::Int, Compiler::Error::WrongType);
                addOp((token == Token::Twiddle) ? Op::Not : Op::LNot);
            }
            break;
    }
    
    return true;
}

bool
CompileEngine::postfixExpression()
{
    if (!primaryExpression()) {
        return false;
    }
    
    while (true) {
        if (match(Token::OpenParen)) {
            // Top of exprStack must be a function id
            Function fun;
            expect(findFunction(_exprStack.back(), fun), Compiler::Error::ExpectedFunction);
            expect(argumentList(fun), Compiler::Error::ExpectedArgList);
            expect(Token::CloseParen);
            
            // Replace the top of the exprStack with the function return value
            _exprStack.pop_back();
            
            _exprStack.push_back(ExprEntry::Value(fun.type()));
            
            if (fun.isNative()) {
                addOpId(Op::CallNative, uint16_t(fun.nativeId()));
            } else { 
                addOpTarg(Op::Call, fun.addr());
            }
        } else if (match(Token::OpenBracket)) {
            bakeExpr(ExprAction::Ref);
            expect(arithmeticExpression(), Compiler::Error::ExpectedExpr);
            expect(Token::CloseBracket);
            
            // Bake the contents of the brackets, leaving the result on TOS
            expect(bakeExpr(ExprAction::Right) == Type::Int, Compiler::Error::WrongType);

            // TOS now has the index.
            // Index the Ref
            bakeExpr(ExprAction::Index);
        } else if (match(Token::Dot)) {
            std::string id;
            expect(identifier(id), Compiler::Error::ExpectedIdentifier);
            bakeExpr(ExprAction::Ref);
            
            _exprStack.emplace_back(id);
            bakeExpr(ExprAction::Offset);
            return true;
        } else if (match(Token::Inc)) {
            Type type = bakeExpr(ExprAction::Ref);
            _exprStack.pop_back();
            _exprStack.push_back(ExprEntry::Value(type));

            if (type == Type::Float) {
                addOp(Op::PostIncFloat);
            } else {
                expect(type == Type::Int, Compiler::Error::MismatchedType);
                addOp(Op::PostIncInt);
            }
        } else if (match(Token::Dec)) {
            Type type = bakeExpr(ExprAction::Ref);
            _exprStack.pop_back();
            _exprStack.push_back(ExprEntry::Value(type));

            if (type == Type::Float) {
                addOp(Op::PostDecFloat);
            } else {
                expect(type == Type::Int, Compiler::Error::MismatchedType);
                addOp(Op::PostDecInt);
            }
        } else {
            return true;
        }
    }
}

bool
CompileEngine::primaryExpression()
{
    if (match(Token::OpenParen)) {
        expect(arithmeticExpression(), Compiler::Error::ExpectedExpr);
        expect(Token::CloseParen);
        return true;
    }
    
    std::string id;
    if (identifier(id)) {
        // See if this is a def
        _exprStack.emplace_back(id);
        return true;
    }
        
    float f;
    if (floatValue(f)) {
        _exprStack.emplace_back(f);
        return true;
    }
        
    int32_t i;
    if (integerValue(i)) {
        _exprStack.emplace_back(i);
        return true;
    }
    return false;
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
        currentFunction().addArg(id, t, isPointer);
        
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
        if (!arithmeticExpression()) {
            if (i == 0) {
                break;
            }
            expect(false, Compiler::Error::ExpectedExpr);
        }
        
        i++;
        
        expect(fun.args() >= i, Compiler::Error::WrongNumberOfArgs);
    
        // Bake the arithmeticExpression, leaving the result in r0.
        // Make sure the type matches the formal argument and push
        const Symbol& sym = fun.local(i - 1);
        Type t = sym.isPointer() ? Type::Ptr : sym.type();
        expect(bakeExpr(ExprAction::Right, t) == t, Compiler::Error::MismatchedType);

        if (!match(Token::Comma)) {
            break;
        }
    }

    expect(fun.args() == i, Compiler::Error::WrongNumberOfArgs);
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
CompileEngine::integerValue(int32_t& i)
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
CompileEngine::opDataFromString(const std::string str, OpData& data)
{
    auto it = find_if(_opcodes.begin(), _opcodes.end(),
                    [str](const OpData& opData) { return opData._str == str; });
    if (it != _opcodes.end()) {
        data = *it;
        return true;
    }
    return false;
}

bool
CompileEngine::opDataFromOp(const Op op, OpData& data)
{
    auto it = find_if(_opcodes.begin(), _opcodes.end(),
                    [op](const OpData& opData) { return opData._op == op; });
    if (it != _opcodes.end()) {
        data = *it;
        return true;
    }
    return false;
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
CompileEngine::findSymbol(const std::string& s, Symbol& sym)
{
    // First look in the current function and then in the parent struct
    if (currentFunction().findLocal(s, sym)) {
        return true;
    }
    
    return currentStruct().findLocal(s, sym);
}

Type
CompileEngine::bakeExpr(ExprAction action, Type matchingType)
{
    expect(!_exprStack.empty(), Compiler::Error::InternalError);
    
    Type type = Type::None;
    ExprEntry entry = _exprStack.back();   
    Symbol sym;
     
    switch(action) {
        default: break;
        case ExprAction::Right:
            switch(entry.type()) {
                default:
                    expect(false, Compiler::Error::InternalError);
                case ExprEntry::Type::Int: {
                    uint32_t i = uint32_t(int32_t(entry));
                    
                    if (matchingType == Type::Float) {
                        // Promote to float
                        float f = float(int32_t(i));
                        addOpInt(Op::Push, findFloat(f));
                        type = Type::Float;
                        break;
                    }
                    
                    if (i <= 15) {
                        addOpSingleByteIndex(Op::PushIntConstS, i);
                    } else if (i <= 255) {
                        addOpInt(Op::PushIntConst, i);
                    } else {
                        // Add an int const
                        addOpInt(Op::Push, findInt(i));
                    }
                    type = Type::Int;
                    break;
                }
                case ExprEntry::Type::Float:
                    // Use an fp constant
                    addOpInt(Op::Push, findFloat(entry));
                    type = Type::Float;
                    break;
                case ExprEntry::Type::Id:
                    // Push the value
                    expect(findSymbol(entry, sym), Compiler::Error::UndefinedIdentifier);
                    addOpId(Op::Push, sym.addr());
                    
                    if (sym.isPointer() && matchingType != Type::Ptr) {
                        addOp(Op::PushDeref);
                        type = sym.type();
                        break;
                    }
                    
                    type = sym.isPointer() ? Type::Ptr : sym.type();
                    break;
                case ExprEntry::Type::Ref: {
                    // FIXME: ???
                    // If this is a ptr and the matchingType is not Ptr
                    // then we want to leave the ref on TOS, not the value
                    const ExprEntry::Ref& ref = entry;
                    type = ref._type;
                    if (!ref._ptr) {
                        addOp(Op::PushDeref);
                    } else {
                        type = Type::Ptr;
                    }
                    break;
                }
                case ExprEntry::Type::Value: {
                    // Nothing to do, this just indicates that TOS is a value
                    const ExprEntry::Value& value = entry;
                    type = value._type;
                    break;
                }
            }
            break;
        case ExprAction::Index: {
            // TOS has an index, get the sym for the var so 
            // we know the size of each element
            if (entry.type() == ExprEntry::Type::Ref) {
                // This is a ref, get the size from the type
                const ExprEntry::Ref& ref = entry;
                type = ref._type;
            } else {
                expect(entry.type() == ExprEntry::Type::Id, Compiler::Error::ExpectedIdentifier);
                expect(findSymbol(entry, sym), Compiler::Error::UndefinedIdentifier);
                expect(sym.storage() == Symbol::Storage::Local || 
                       sym.storage() == Symbol::Storage::Global, Compiler::Error::ExpectedVar);

                type = sym.type();
                _exprStack.pop_back();
                _exprStack.push_back(ExprEntry::Ref(type));
            }
            
            Struct s;
            addOpSingleByteIndex(Op::Index, structFromType(type, s) ? s.size() : 1);
            return type;
        }
        case ExprAction::Offset: {
            // Prev entry has a Ref. Get the type so we can get an element index
            // we know the size of each element
            expect(_exprStack.size() >= 2, Compiler::Error::InternalError);
            const ExprEntry& prevEntry = _exprStack.end()[-2];            
            expect(prevEntry.type() == ExprEntry::Type::Ref, Compiler::Error::InternalError);
            const ExprEntry::Ref& ref = prevEntry;
            
            // If the Ref is a Ptr then we need to deref
            if (ref._ptr) {
                addOp(Op::PushDeref);
            }
            uint8_t index;
            Type elementType;
            findStructElement(ref._type, entry, index, elementType);
            _exprStack.pop_back();
            _exprStack.pop_back();
            _exprStack.push_back(ExprEntry::Ref(elementType));
            
            addOpSingleByteIndex(Op::Offset, index);
            return elementType;
        }
        case ExprAction::Left:
        case ExprAction::Ref:
            if (entry.type() == ExprEntry::Type::Ref) {
                // Already have a ref
                const ExprEntry::Ref& ref = entry;
                type = ref._type;
                
                // If this is a Ptr action, we want to say that we want the stack to have 
                // a reference to a value rather than the value. So add the _ptr value
                // to the Ref
                if (action == ExprAction::Left) {
                    // If the matching type is a Ptr then we just assign as usual.
                    // If the ref is a pointer, we need to get the value at the
                    // ref and use that as the ref for the PopDeref.
                    if (ref._ptr && matchingType != Type::Ptr) {
//                        addOp(Op::Swap);
                        addOp(Op::PushDeref);
//                        addOp(Op::Swap);
                        type = ref._type;
                    } else {
                        type = ref._ptr ? Type::Ptr : ref._type;
                    }
                    addOp(Op::PopDeref);
                    break;
                }
                return type;
            }
            
            expect(entry.type() == ExprEntry::Type::Id, Compiler::Error::ExpectedIdentifier);

            // Turn this into a Ref
            expect(findSymbol(entry, sym), Compiler::Error::UndefinedIdentifier);
            _exprStack.pop_back();
            _exprStack.push_back(ExprEntry::Ref(sym.type(), sym.isPointer()));
            
            addOpId(Op::PushRef, sym.addr());
            return sym.isPointer() ? Type::Ptr : sym.type();
    }
    
    _exprStack.pop_back();
    return type;
}
        
bool
CompileEngine::isExprFunction()
{
    expect(!_exprStack.empty(), Compiler::Error::InternalError);
    
    Function fun;
    return findFunction(_exprStack.back(), fun);
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
CompileEngine::findStructElement(Type type, const std::string& id, uint8_t& index, Type& elementType)
{
    Struct s;
    expect(structFromType(type, s), Compiler::Error::ExpectedStructType);
    
    const std::vector<Symbol>& locals = s.locals();

    auto it = find_if(locals.begin(), locals.end(),
                    [id](const Symbol& sym) { return sym.name() == id; });
    expect(it != locals.end(), Compiler::Error::InvalidStructId);
    
    // FIXME: For now assume structs can only have 1 word entries. If we ever support Structs with Structs this is not true
    index = it - locals.begin();
    elementType = it->type();
}

uint8_t
CompileEngine::elementSize(Type type)
{
    if (uint8_t(type) < StructTypeStart) {
        return 1;
    }
    
    uint8_t structIndex = uint8_t(type) - 0x80;
    expect(structIndex < _structs.size(), Compiler::Error::InternalError);
    return _structs[structIndex].size();
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
         
        int16_t offset = int16_t(addr) - int16_t(it._addr) - 2;
         
        expect(_rom8[it._addr + 1] == 0, Compiler::Error::InternalError);
        
        _rom8[it._addr] |= (offset >> 8) & 0x0f;
        _rom8[it._addr + 1] = uint8_t(offset);
    }
    
    _jumpList.pop_back();
}

void
CompileEngine::addJumpEntry(Op op, JumpEntry::Type type)
{
    expect(!_jumpList.empty(), Compiler::Error::InternalError);
    
    uint16_t addr = _rom8.size();
    addOpTarg(op, 0);
    _jumpList.back().emplace_back(type, addr);
}
