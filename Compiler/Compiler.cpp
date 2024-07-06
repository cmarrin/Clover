/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "Compiler.h"
#include "Codegen.h"

#include <map>
#include <vector>
#include <cmath>

#include "AST.h"
#include "OpInfo.h"

using namespace lucid;

bool Compiler::compile(std::vector<uint8_t>& executable, uint32_t maxExecutableSize,
                            const std::vector<Module*>& modules)
{
    // Add built-in native modules
    ModulePtr coreModule = std::make_shared<Module>("core");
    coreModule->addNativeFunction("printf", NativeId::Print, Type::None, {{ "s", Type::String, 1, 0 }});
//    coreModule->addNativeFunction("int8ToString", NativeId::Int8ToString, Type::String, {{ "v", Type::Int8, 1, 0 }});
//    coreModule->addNativeFunction("uint8ToString", NativeId::UInt8ToString, Type::String, {{ "v", Type::UInt8, 1, 0 }});
//    coreModule->addNativeFunction("int16ToString", NativeId::Int16ToString, Type::String, {{ "v", Type::Int16, 1, 0 }});
//    coreModule->addNativeFunction("uint16ToString", NativeId::UInt16ToString, Type::String, {{ "v", Type::UInt16, 1, 0 }});
//    coreModule->addNativeFunction("int32ToString", NativeId::Int32ToString, Type::String, {{ "v", Type::Int32, 1, 0 }});
//    coreModule->addNativeFunction("uint32ToString", NativeId::UInt32ToString, Type::String, {{ "v", Type::UInt32, 1, 0 }});
//    coreModule->addNativeFunction("floatToString", NativeId::FloatToString, Type::String, {{ "v", Type::Float, 1, 0 }});

    _modules.push_back(coreModule);
    
    program();
    
    if (_error == Error::None && executable.size() > maxExecutableSize) {
        _error = Error::ExecutableTooBig;
    }
    
    // Do second pass
    Codegen codeGen(&executable);
    
    // Write signature
    executable.push_back('l');
    executable.push_back('u');
    executable.push_back('c');
    executable.push_back('d');

    for (auto& itStruct : _structs) {
        codeGen.processAST(itStruct->astNode());
        
        for (auto& itFunc : itStruct->functions()) {
            codeGen.processAST(itFunc.astNode());
        }
        
        // FIXME: We need to tell the functions and the struct where their code starts
    }

    return _error == Error::None;
}
bool
Compiler::program()
{
    _scanner.setIgnoreNewlines(true);
    
    try {
        while(import()) { }
        while(constant()) { }
        strucT();

        expect(Token::EndOfFile);
    }
    catch(...) {
        return false;
    }
    
    return _error == Error::None;
}

bool
Compiler::import()
{
    if (!match(Reserved::Import)) {
        return false;
    }
    
    std::string id, idAs;
    expect(identifier(id), Error::ExpectedIdentifier);

    if (match(Reserved::As)) {
        expect(identifier(idAs), Error::ExpectedIdentifier);
    }
    
    // FIXME: For now only support built-in imports and only built-ins
    // we already know about (like "System").
    //
    // If and when we support Lucid imports, compile the import inline.
    // An import is a regular Lucid program but only the first struct is
    // used. What about imports in the imported file? Are there warnings
    // if there are more structs? What about an entry struct?
    // Need to rename struct if there is an idAs. How do we deal with
    // duplicate struct names?
    
    

    expect(Token::Semicolon);

    return true;
}

bool
Compiler::strucT()
{
    if (!match(Reserved::Struct)) {
        return false;
    }
    
    std::string id;
    expect(identifier(id), Error::ExpectedIdentifier);

    // Add a struct
    if (!_structStack.empty()) {
        // This is a child of another struct
        _structStack.push_back(currentStruct()->addStruct(id, Type(_nextStructType++)));
        _structTypes.push_back(_structStack.back());
    } else {
        _structStack.push_back(addStruct(id, Type(_nextStructType++)));
        _structTypes.push_back(_structStack.back());
    }
    
    expect(Token::OpenBrace);
    
    while(structEntry()) { }
    
    expect(Token::CloseBrace);
    expect(Token::Semicolon);
    _structStack.pop_back();
    return true;
}

bool
Compiler::structEntry()
{
    if (strucT() || varStatement() || function() || init()) {
        return true;
    }
    
    return false;
}

bool
Compiler::constant()
{
    if (!match(Reserved::Const)) {
        return false;
    }
    
    Type t;
    std::string id;
    uint32_t val;
    
    expect(type(t), Error::ExpectedType);
    
    // Only built-in types allowed for types
    expect(uint8_t(t) < StructTypeStart, Error::ConstMustBeSimpleType);
    expect(identifier(id), Error::ExpectedIdentifier);
    expect(value(val, t), Error::ExpectedValue);
    expect(Token::Semicolon);

    expect(findConstant(id, t, val), Error::DuplicateIdentifier);
    
    _constants.emplace_back(t, id, val);
    
    return true;
}

bool
Compiler::value(uint32_t& i, Type t)
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
    
    std::string s;
    if (stringValue(s)) {
        return true;
    }
    return false;
}

bool
Compiler::varStatement()
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
            expect(false, Error::ExpectedVar);
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
Compiler::var(Type type, bool isPointer)
{
    std::string id;
    expect(identifier(id), Error::ExpectedIdentifier);
    
    uint32_t size = 1;

    if (match(Token::OpenBracket)) {
        expect(integerValue(size), Error::ExpectedValue);
        expect(Token::CloseBracket);
    }
    
    size *= sizeInBytes(type);

    // Put the var in the current struct unless we're in a function, then put it in _locals
    SymbolPtr idSym;
    
    if (_inFunction) {
        expect(currentFunction()->addLocal(id, type, size, isPointer), Error::DuplicateIdentifier);
        idSym = currentFunction()->findLocal(id);
    } else {
        expect(!_structStack.empty(), Error::InternalError);
        
        // FIXME: Need to deal with ptr and size
        expect(currentStruct()->addLocal(id, type, size, false), Error::DuplicateIdentifier);
        idSym = currentStruct()->findLocal(id);
    }
    
    // Check for an initializer. We only allow initializers on Int and Float
    ASTPtr ast;
    
    if (match(Token::Equal)) {
        if (uint8_t(type) < StructTypeStart) {
            // Built-in type. Generate an expression
            ast = expression();
            expect(ast != nullptr, Error::ExpectedExpr);
        } else {
            // Struct type, collect initializers
            expect(Token::OpenBrace);
            ast = expression();
            if (ast) {
                // FIXME: For now ignore the initializers
                while (match(Token::Comma)) {
                    ast = expression();
                    expect(ast != nullptr, Error::ExpectedExpr);
                }
            }
            expect(Token::CloseBrace);
        }
    }
    
    if (ast) {
        ASTPtr idNode = std::make_shared<VarNode>(idSym);
        ASTPtr assignment = std::make_shared<OpNode>(idNode, Op::DEREF, ast, true);
    
        if (_inFunction) {
            currentFunction()->addASTNode(assignment);
        } else {
            currentStruct()->addASTNode(assignment);
        }
    }
    
    return true;
}

bool
Compiler::type(Type& t)
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
    
    // First look in the child structs then at the peers
    StructPtr struc = currentStruct();
    expect(struc != nullptr, Error::InternalError);
    struc = struc->findStruct(id);
    
    if (struc) {
        t = struc->type();
        _scanner.retireToken();
        return true;
    }
    
    struc = findStruct(id);
    if (struc) {
        t = struc->type();
        _scanner.retireToken();
        return true;
    }
    
    return false;
}

bool
Compiler::function()
{
    if (!match(Reserved::Function)) {
        return false;
    }
    
    _nextMem = 0;
    
    // Type is optional
    Type t = Type::None;
    type(t);
    
    std::string id;
    expect(identifier(id), Error::ExpectedIdentifier);

    // Remember the function
    expect(currentStruct() != nullptr, Error::InternalError);
    _currentFunction = currentStruct()->addFunction(id, t);
    _inFunction = true;
    
    expect(Token::OpenParen);
    
    expect(formalParameterList(), Error::ExpectedFormalParams);
    
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
    _currentFunction = nullptr;
    return true;
}

bool
Compiler::init()
{
    if (!match(Reserved::Initialize)) {
        return false;
    }
    
    // The init method is a function that has no name and no return type
    // Remember the function
    expect(currentStruct() != nullptr, Error::InternalError);
    _currentFunction = currentStruct()->addFunction("", Type::None);
    _inFunction = true;
    
    expect(Token::OpenParen);
    
    expect(formalParameterList(), Error::ExpectedFormalParams);
    
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
    _currentFunction = nullptr;
    return true;
}

bool
Compiler::statement()
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
Compiler::compoundStatement()
{
    if (!match(Token::OpenBrace)) {
        return false;
    }

    // If we're in a function, remember the number of local variables
    // added so we can toss them at the end
    auto numLocals = 0;
    if (_inFunction) {
        numLocals = currentFunction()->numLocals();
    }
    
    while(statement()) { }

    expect(Token::CloseBrace);
    
    // prune the locals added in this block
    if (_inFunction) {
        currentFunction()->pruneLocals(currentFunction()->numLocals() - numLocals);
    }
    return true;
}

bool
Compiler::ifStatement()
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
Compiler::forStatement()
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
            expect(t == Type::Int || t == Type::Float, Error::WrongType);

            std::string id;
            expect(identifier(id), Error::ExpectedIdentifier);
            expect(Token::Equal);

            // Generate an expression
            expect(_inFunction, Error::InternalError);
            expect(currentFunction()->addLocal(id, t, sizeInBytes(t), false), Error::DuplicateIdentifier);
            _nextMem += 1;

            // FIXME: This needs to be an arithmeticExpression?
            ast = expression();
            expect(ast != nullptr, Error::ExpectedExpr);
        } else {
            ast = expression();
            expect(ast != nullptr, Error::ExpectedExpr);
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
        //addJumpEntry(Op::If, JumpEntry::Type::Break);
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

    //addJumpEntry(Op::Jump, JumpEntry::Type::Start);

//    uint16_t breakAddr = _rom8.size();

    // Now resolve all the jumps
//    exitJumpContext(startAddr, contAddr, breakAddr);
    return true;
}

bool
Compiler::whileStatement()
{
    if (!match(Reserved::While)) {
        return false;
    }

    enterJumpContext();

    expect(Token::OpenParen);

    // Loop starts with an if test of expr
//    uint16_t loopAddr = _rom8.size();
    
    expression();

    //addJumpEntry(Op::If, JumpEntry::Type::Break);

    expect(Token::CloseParen);
    
    statement();

    // Loop back to the beginning
    //addJumpEntry(Op::Jump, JumpEntry::Type::Continue);
        
    // Now resolve all the jumps (stmtAddr will never be used so just reuse loopAddr)
//    exitJumpContext(loopAddr, loopAddr, _rom8.size());

    return true;
}

bool
Compiler::loopStatement()
{
    if (!match(Reserved::Loop)) {
        return false;
    }

    enterJumpContext();

//    uint16_t loopAddr = _rom8.size();
    
    statement();

    // Loop back to the beginning
    //addJumpEntry(Op::Jump, JumpEntry::Type::Continue);
    
    // Now resolve all the jumps (stmtAddr will never be used so just reuse loopAddr)
//    exitJumpContext(loopAddr, loopAddr, _rom8.size());

    return true;
}

bool
Compiler::returnStatement()
{
    if (!match(Reserved::Return)) {
        return false;
    }
    
    ASTPtr ast = expression();
    if (ast) {
        // Push the return value
    } else {
        // If the function return type not None, we need a return value
        expect(currentFunction()->returnType() == Type::None, Error::MismatchedType);
    }
    
    expect(Token::Semicolon);
    return true;
}

bool
Compiler::jumpStatement()
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
    expect(!_jumpList.empty(), Error::OnlyAllowedInLoop);
    
    //addJumpEntry(Op::Jump, type);

    expect(Token::Semicolon);
    return true;
}

bool
Compiler::expressionStatement()
{
    ASTPtr node = expression();
    if (!node) {
        return false;
    }
    
    expect(_inFunction, Error::InternalError);
    currentFunction()->addASTNode(node);
    expect(Token::Semicolon);
    return true;
}

ASTPtr
Compiler::expression()
{
    return arithmeticExpression(unaryExpression(), 1);
}

ASTPtr
Compiler::arithmeticExpression(const ASTPtr& node, uint8_t minPrec)
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
        Op opcode = lhs->isSigned() ? opInfo.opcodeS() : opInfo.opcodeU();
        lhs = std::make_shared<OpNode>(lhs, opcode, rhs, opInfo.assoc() == Assoc::Right);
    }
    
    return lhs;
}

ASTPtr
Compiler::unaryExpression()
{
    ASTPtr node = postfixExpression();
    if (node) {
        return node;
    }

    Op opcode;
    
    if (match(Token::Minus)) {
        opcode = Op::NEG;
    } else if (match(Token::Twiddle)) {
        opcode = Op::NOT;
    } else if (match(Token::Bang)) {
        opcode = Op::NOP;       // FIXME: Implement
    } else if (match(Token::Inc)) {
        opcode = Op::PREINC;
    } else if (match(Token::Dec)) {
        opcode = Op::PREDEC;
    } else if (match(Token::And)) {
        opcode = Op::NOP;       // FIXME: Implement
    } else {
        return nullptr;
    }
    
    return std::make_shared<OpNode>(opcode, unaryExpression());
}

ASTPtr
Compiler::postfixExpression()
{
    ASTPtr lhs = primaryExpression();
    if (!lhs) {
        return nullptr;
    }
    
    while (true) {
        if (match(Token::OpenParen)) {
            expect(lhs->astNodeType() == ASTNodeType::FunctionCall, Error::ExpectedFunction);
            Function* fun = std::static_pointer_cast<FunctionCallNode>(lhs)->function();

            expect(argumentList(fun), Error::ExpectedArgList);
            expect(Token::CloseParen);
        } else if (match(Token::OpenBracket)) {
            ASTPtr rhs = expression();
            ASTPtr result = std::make_shared<OpNode>(lhs, Op::NOP, rhs, false); // FIXME: Implement
            expect(Token::CloseBracket);
            return result;
        } else if (match(Token::Dot)) {
            std::string id;
            expect(identifier(id), Error::ExpectedIdentifier);
            
            // If lhs is a module, this has to be a function inside that module.
            if (lhs->astNodeType() == ASTNodeType::Module) {
                Function* f = std::static_pointer_cast<ModuleNode>(lhs)->module()->findFunction(id);
                expect(f, Error::UndefinedIdentifier);
                lhs = std::make_shared<FunctionCallNode>(f);
                continue;
            }
            
            return std::make_shared<DotNode>(lhs, id);
        } else if (match(Token::Inc)) {
            return std::make_shared<OpNode>(lhs, Op::POSTINC);
        } else if (match(Token::Dec)) {
            return std::make_shared<OpNode>(lhs, Op::POSTDEC);
        } else {
            return lhs;
        }
    }
}

ASTPtr
Compiler::primaryExpression()
{
    if (match(Token::OpenParen)) {
        ASTPtr ast = expression();
        expect(!ast, Error::ExpectedExpr);
        expect(Token::CloseParen);
        return ast;
    }
    
    std::string id;
    if (identifier(id)) {
        expect(_inFunction, Error::InternalError);
        SymbolPtr symbol = findSymbol(id);
        if (symbol) {
            return std::make_shared<VarNode>(symbol);
        }
        
        Type t;
        uint32_t v;
        if (findConstant(id, t, v)) {
            return std::make_shared<ConstantNode>(t, v);
        }

        ModulePtr module = findModule(id);
        if (module) {
            return std::make_shared<ModuleNode>(module);
        }

        expect(false, Error::ExpectedVar);
    }
    
    float f;
    if (floatValue(f)) {
        return std::make_shared<ConstantNode>(f);
    }
        
    uint32_t i;
    if (integerValue(i)) {
        return std::make_shared<ConstantNode>(i);
    }
    
    std::string s;
    if (stringValue(s)) {
        return std::make_shared<StringNode>(s);
    }
    
    return nullptr;
}

bool
Compiler::formalParameterList()
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
        expect(identifier(id), Error::ExpectedIdentifier);
        currentFunction()->addArg(id, t, sizeInBytes(t), isPointer);
        
        if (!match(Token::Comma)) {
            return true;
        }
    }
    
    return true;
}

bool
Compiler::argumentList(Function* fun)
{
    int i = 0;
    while (true) {
        ASTPtr arg = expression();
        if (!arg) {
            if (i == 0) {
                break;
            }
            expect(false, Error::ExpectedExpr);
        }
        
        i++;
        
        //expect(fun.args() >= i, Error::WrongNumberOfArgs);
    
        // Bake the arithmeticExpression, leaving the result in r0.
        // Make sure the type matches the formal argument and push

        if (!match(Token::Comma)) {
            break;
        }
    }

    //expect(fun.args() == i, Error::WrongNumberOfArgs);
    return true;
}

bool
Compiler::identifier(std::string& id, bool retire)
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
Compiler::integerValue(uint32_t& i)
{
    if (_scanner.getToken() != Token::Integer) {
        return false;
    }
    
    i = _scanner.getTokenValue().integer;
    _scanner.retireToken();
    return true;
}

bool
Compiler::floatValue(float& f)
{
    if (_scanner.getToken() != Token::Float) {
        return false;
    }
    
    f = _scanner.getTokenValue().number;
    _scanner.retireToken();
    return true;
}

bool
Compiler::stringValue(std::string& str)
{
    if (_scanner.getToken() != Token::String) {
        return false;
    }
    
    str = _scanner.getTokenString();
    _scanner.retireToken();
    return true;
}

void
Compiler::expect(Token token, const char* str)
{
    bool err = false;
    if (_scanner.getToken() != token) {
        _error = Error::ExpectedToken;
        err = true;
    }
    
    if (str && _scanner.getTokenString() != str) {
        _error = Error::ExpectedToken;
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
Compiler::expect(bool passed, Error error)
{
    if (!passed) {
        _error = error;
        throw true;
    }
}

void
Compiler::expectWithoutRetire(Token token)
{
    if (_scanner.getToken() != token) {
        _expectedToken = token;
        _expectedString = "";
        _error = Error::ExpectedToken;
        throw true;
    }
}

bool
Compiler::match(Reserved r)
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
Compiler::match(Token t)
{
    if (_scanner.getToken() != t) {
        return false;
    }
    _scanner.retireToken();
    return true;
}

bool
Compiler::reserved()
{
    Reserved r;
    return isReserved(_scanner.getToken(), _scanner.getTokenString(), r);
}

bool
Compiler::reserved(Reserved &r)
{
    return isReserved(_scanner.getToken(), _scanner.getTokenString(), r);
}

bool
Compiler::isReserved(Token token, const std::string str, Reserved& r)
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
Compiler::findInt(int32_t i)
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

uint8_t
Compiler::findFloat(float f)
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

StructPtr
Compiler::findStruct(const std::string& id)
{
    auto it = find_if(_structs.begin(), _structs.end(),
                    [id](const StructPtr& s) { return s->name() == id; });
    if (it != _structs.end()) {
        return *it;
    }
    return nullptr;
}

SymbolPtr
Compiler::findSymbol(const std::string& s)
{
    // If we're in a function, look at the local function vars first.
    // Then look at the vars in this struct
    //
    // First look in the current function and then in the parent struct
    if (_inFunction) {
        SymbolPtr symbol = currentFunction()->findLocal(s);
        if (symbol) {
            return symbol;
        }
    }
    
    // Next look at the vars in the current struct
    StructPtr strucT = currentStruct();
    SymbolPtr symbol = strucT->findLocal(s);
    if (symbol) {
        return symbol;
    }
    
    return nullptr;
}

ModulePtr
Compiler::findModule(const std::string& id)
{
    auto it = find_if(_modules.begin(), _modules.end(),
                    [id](const ModulePtr& m) { return m->name() == id; });
    if (it != _modules.end()) {
        return *it;
    }
    return nullptr;
}

bool
Compiler::structFromType(Type type, StructPtr& s)
{
    if (uint8_t(type) < StructTypeStart) {
        return false;
    }
    uint8_t index = uint8_t(type) - StructTypeStart;
    expect(index < _structs.size(), Error::InternalError);
    
    s = _structs[index];
    return true;
}

void
Compiler::exitJumpContext(uint16_t startAddr, uint16_t contAddr, uint16_t breakAddr)
{
    expect(!_jumpList.empty(), Error::InternalError);

    // Go through all the entries in the last _jumpList entry and fill in
    // the addresses.
    for (const auto& it : _jumpList.back()) {
        expect(it._addr < breakAddr, Error::InternalError);
        
        uint16_t addr;
        
        switch(it._type) {
            case JumpEntry::Type::Start: addr = startAddr; break;
            case JumpEntry::Type::Continue: addr = contAddr; break;
            case JumpEntry::Type::Break: addr = breakAddr; break;
        }         
         
//        int16_t offset = int16_t(addr) - int16_t(it._addr) - 2;
         
//        expect(_rom8[it._addr + 1] == 0, Error::InternalError);
//        
//        _rom8[it._addr] |= (offset >> 8) & 0x0f;
//        _rom8[it._addr + 1] = uint8_t(offset);
    }
    
    _jumpList.pop_back();
}

void
Compiler::addJumpEntry(Op op, JumpEntry::Type type)
{
    expect(!_jumpList.empty(), Error::InternalError);
    
//    uint16_t addr = _rom8.size();
//    addOpTarg(op, 0);
//    _jumpList.back().emplace_back(type, addr);
}

uint16_t
Compiler::sizeInBytes(Type type) const
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
            return _structs[structIndex]->size();
    }
}

