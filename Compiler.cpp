/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "Compiler.h"

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
    coreModule->addNativeFunction("printf", NativeId::PrintF, Type::None, {{ "s", Type::String, false, 1, 1 }});
    coreModule->addNativeFunction("memset", NativeId::MemSet, Type::None, {{ "p", Type::UInt8, true, 1, 1 },
                                                                           { "v", Type::UInt8, false, 1, 1 },
                                                                           { "n", Type::UInt32, false, 1, 1 }});
    coreModule->addNativeFunction("irand", NativeId::RandomInt, Type::Int32, {{ "min", Type::Int32, false, 1, 1 }, { "max", Type::Int32, false, 1, 1 }});
    coreModule->addNativeFunction("frand", NativeId::RandomFloat, Type::Float, {{ "min", Type::Float, false, 1, 1 }, { "max", Type::Float, false, 1, 1 }});
    coreModule->addNativeFunction("imin", NativeId::MinInt, Type::Int32, {{ "a", Type::Int32, false, 1, 1 }, { "b", Type::Int32, false, 1, 1 }});
    coreModule->addNativeFunction("imax", NativeId::MaxInt, Type::Int32, {{ "a", Type::Int32, false, 1, 1 }, { "b", Type::Int32, false, 1, 1 }});
    coreModule->addNativeFunction("fmin", NativeId::MinFloat, Type::Float, {{ "a", Type::Float, false, 1, 1 }, { "b", Type::Float, false, 1, 1 }});
    coreModule->addNativeFunction("fmax", NativeId::MaxFloat, Type::Float, {{ "a", Type::Float, false, 1, 1 }, { "b", Type::Float, false, 1, 1 }});
    coreModule->addNativeFunction("initArgs", NativeId::InitArgs, Type::None, { });
    coreModule->addNativeFunction("argint8", NativeId::ArgInt8, Type::Int8, { });
    coreModule->addNativeFunction("argint16", NativeId::ArgInt16, Type::Int16, { });
    coreModule->addNativeFunction("argint32", NativeId::ArgInt32, Type::Int32, { });
    coreModule->addNativeFunction("argFloat", NativeId::ArgFloat, Type::Float, { });

    _modules.push_back(coreModule);
    
    program();
    
    if (_error == Error::None && executable.size() > maxExecutableSize) {
        _error = Error::ExecutableTooBig;
    }
    
    if (_error != Error::None) {
        return false;
    }
    
    // Do second pass
    // Write signature
    executable.push_back('l');
    executable.push_back('u');
    executable.push_back('c');
    executable.push_back('d');

    // Write dummy entry point address, to be filled in later
    executable.push_back(0);
    executable.push_back(0);
    executable.push_back(0);
    executable.push_back(0);
    
    // Write top level struct size, to be filled in later
    int32_t topLevelSize = _structs[0]->size();
    executable.push_back(topLevelSize >> 24);
    executable.push_back(topLevelSize >> 16);
    executable.push_back(topLevelSize >> 8);
    executable.push_back(topLevelSize);
    
    for (auto& itStruct : _structs) {
        itStruct->astNode()->emitCode(executable, false, this);
        
        for (auto& itFunc : itStruct->functions()) {
            // If this is the initialize function of the top level
            // struct, set the entry point
            if (itStruct == _structs[0] && itFunc->name() == "") {
                uint32_t cur = uint32_t(executable.size());
                executable.at(4) = cur >> 24;
                executable.at(5) = cur >> 16;
                executable.at(6) = cur >> 8;
                executable.at(7) = cur;
            }
            
            // Set addr of this function
            itFunc->setAddr(AddrNativeType(executable.size()));
            itFunc->astNode()->emitCode(executable, false, this);
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
        // This is the top level struct
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
    
    StructPtr struc;
    
    if (isStruct(type)) {
        struc = structFromType(type);
    }
    
    SymbolPtr sym;
    uint32_t nElements = 1;

    if (match(Token::OpenBracket)) {
        expect(integerValue(nElements), Error::ExpectedValue);
        expect(Token::CloseBracket);
    }
    
    sym = std::make_shared<Symbol>(id, type, isPointer, struc ? struc->size() : typeToBytes(type), nElements);
    expect(sym != nullptr, Error::DuplicateIdentifier);
    
    // Put the var in the current struct unless we're in a function, then put it in _locals    
    if (_inFunction) {
        currentFunction()->addLocal(sym);
    } else {
        expect(!_structStack.empty(), Error::InternalError);
        currentStruct()->addLocal(sym);
    }
    
    // Check for an initializer.
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
        ASTPtr idNode = std::make_shared<VarNode>(sym, annotationIndex());
        ASTPtr assignment = std::make_shared<OpNode>(idNode, Op::NOP, ast, Type::None, true, annotationIndex());
    
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
    if (match(Reserved::Int8)) {
        t = Type::Int8;
        return true;
    }
    // 'bool' is just an alias for 'uint8'
    if (match(Reserved::UInt8) || match(Reserved::Bool)) {
        t = Type::UInt8;
        return true;
    }
    if (match(Reserved::Int16)) {
        t = Type::Int16;
        return true;
    }
    if (match(Reserved::UInt16)) {
        t = Type::UInt16;
        return true;
    }
    if (match(Reserved::Int32)) {
        t = Type::Int32;
        return true;
    }
    if (match(Reserved::UInt32)) {
        t = Type::UInt32;
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

    // ENTER has to be the first instruction in the Function.
    _currentFunction->addASTNode(std::make_shared<EnterNode>(_currentFunction, annotationIndex()));
    
    while(statement()) { }
    
    expect(Token::CloseBrace);

    // Set the high water mark
    if (_nextMem > _localHighWaterMark) {
        _localHighWaterMark = _nextMem;
    }
    
    // Emit Return at the end if there's not already one
    ASTPtr nodes = _currentFunction->astNode();
    ASTPtr lastNode = nodes->child(nodes->numChildren() - 1);
    if (lastNode->astNodeType() != ASTNodeType::Op || reinterpret_cast<OpNode*>(lastNode.get())->op() != Op::RET) {
        _currentFunction->addASTNode(std::make_shared<OpNode>(Op::RET, annotationIndex()));
    }
        
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

    // ENTER has to be the first instruction in the Function.
    _currentFunction->addASTNode(std::make_shared<EnterNode>(_currentFunction, annotationIndex()));

    while(statement()) { }
    
    expect(Token::CloseBrace);

    // Set the high water mark
    if (_nextMem > _localHighWaterMark) {
        _localHighWaterMark = _nextMem;
    }
    
    // Emit Return at the end if there's not already one
    ASTPtr nodes = _currentFunction->astNode();
    ASTPtr lastNode = nodes->child(nodes->numChildren() - 1);
    if (lastNode->astNodeType() != ASTNodeType::Op || reinterpret_cast<OpNode*>(lastNode.get())->op() != Op::RET) {
        _currentFunction->addASTNode(std::make_shared<OpNode>(Op::RET, annotationIndex()));
    }
    
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
    currentFunction()->addASTNode(expression());
    expect(Token::CloseParen);
    
    ASTPtr ifNode = std::make_shared<BranchNode>(BranchNode::Kind::IfStart, annotationIndex());
    currentFunction()->addASTNode(ifNode);

    statement();
    
    bool haveElse = false;
    ASTPtr elseNode;
    
    if (match(Reserved::Else)) {
        haveElse = true;
        elseNode = std::make_shared<BranchNode>(BranchNode::Kind::ElseStart, annotationIndex());
        
        std::static_pointer_cast<BranchNode>(elseNode)->setFixupNode(ifNode);
        currentFunction()->addASTNode(elseNode);
        statement();
    }

    ASTPtr endNode = std::make_shared<BranchNode>(BranchNode::Kind::IfEnd, annotationIndex());
    currentFunction()->addASTNode(endNode);
    std::static_pointer_cast<BranchNode>(endNode)->setFixupNode(haveElse ? elseNode : ifNode);

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
            //expect(t == Type::Int || t == Type::Float, Error::WrongType);

            std::string id;
            expect(identifier(id), Error::ExpectedIdentifier);
            expect(Token::Equal);

            // Generate an expression
            expect(_inFunction, Error::InternalError);
            SymbolPtr sym = std::make_shared<Symbol>(id, t, false, 1, 1);
            expect(currentFunction()->addLocal(sym), Error::DuplicateIdentifier);
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
    
    // If this is a function call, a return value will have been
    // pushed which we don't need. Tell the function call not to push it
    if (node->astNodeType() == ASTNodeType::FunctionCall) {
        FunctionPtr function = std::static_pointer_cast<FunctionCallNode>(node)->function();
        function->setPushReturn(false);
    }

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
            if (!findOpInfo(Operator(_scanner.getToken()), nextOpInfo) || nextOpInfo.prec() < opInfo.prec()) {
                break;
            }
            
            rhs = arithmeticExpression(rhs, nextOpInfo.prec());
        }
        
        // Generate an ASTNode
        Op opcode = lhs->isSigned() ? opInfo.opcodeS() : opInfo.opcodeU();
        lhs = std::make_shared<OpNode>(lhs, opcode, rhs, opInfo.resultType(), opInfo.assoc() == Assoc::Right, annotationIndex());
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
    Type resultType = Type::None;
    
    if (match(Token::Minus)) {
        opcode = Op::NEG;
    } else if (match(Token::Twiddle)) {
        opcode = Op::NOT;
    } else if (match(Token::Bang)) {
        opcode = Op::LNOT;
        resultType = Type::UInt8;
    } else if (match(Token::Inc)) {
        opcode = Op::PREINC;
    } else if (match(Token::Dec)) {
        opcode = Op::PREDEC;
    } else if (match(Token::And)) {
        opcode = Op::PUSHREF1; // FIXME...
    } else {
        return nullptr;
    }
    
    return std::make_shared<OpNode>(opcode, unaryExpression(), resultType, annotationIndex());
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
            // Function call
            expect(argumentList(lhs), Error::ExpectedArgList);
            expect(Token::CloseParen);
        } else if (match(Token::OpenBracket)) {
            ASTPtr rhs = expression();
            
            expect(lhs->isIndexable(), Error::ExpectedIndexable);
            
            // array index does a PUSHREF of the lhs, then a PUSH of the rhs and then an INDEX with element size as operand
            ASTPtr result = std::make_shared<IndexNode>(lhs, rhs, annotationIndex());
            expect(Token::CloseBracket);
            return result;
        } else if (match(Token::Dot)) {
            std::string id;
            expect(identifier(id), Error::ExpectedIdentifier);
            
            // If lhs is a module, this has to be a function inside that module.
            if (lhs->astNodeType() == ASTNodeType::Module) {
                FunctionPtr f = std::static_pointer_cast<ModuleNode>(lhs)->module()->findFunction(id);
                expect(f != nullptr, Error::UndefinedIdentifier);
                lhs = std::make_shared<FunctionCallNode>(f, annotationIndex());
                continue;
            }
            
            return std::make_shared<DotNode>(lhs, id, annotationIndex());
        } else if (match(Token::Inc)) {
            return std::make_shared<OpNode>(lhs, Op::POSTINC, Type::None, annotationIndex());
        } else if (match(Token::Dec)) {
            return std::make_shared<OpNode>(lhs, Op::POSTDEC, Type::None, annotationIndex());
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
    
    // See if this is a type cast
    Type t;
    if (type(t)) {
        // A type cast looks like a function with a single arg
        expect(Token::OpenParen);
        ASTPtr arg = expression();
        expect(Token::CloseParen);        
        return std::make_shared<TypeCastNode>(t, arg, annotationIndex());
    }
    
    std::string id;
    if (identifier(id)) {
        expect(_inFunction, Error::InternalError);
        SymbolPtr symbol = findSymbol(id);
        if (symbol) {
            // This could be a var or function. Create the proper ASTNode
            if (symbol->type() == Type::Function) {
                return std::make_shared<FunctionCallNode>(symbol->function(), annotationIndex());
            } else {
                return std::make_shared<VarNode>(symbol, annotationIndex());
            }
        }
        
        Type t;
        uint32_t v;
        if (findConstant(id, t, v)) {
            return std::make_shared<ConstantNode>(t, v, annotationIndex());
        }

        ModulePtr module = findModule(id);
        if (module) {
            return std::make_shared<ModuleNode>(module, annotationIndex());
        }

        expect(false, Error::ExpectedVar);
    }

    if (match(Reserved::True)) {
        return std::make_shared<ConstantNode>(Type::UInt8, 1, annotationIndex());
    }
    
    if (match(Reserved::False)) {
        return std::make_shared<ConstantNode>(Type::UInt8, 0, annotationIndex());
    }
    
    float f;
    if (floatValue(f)) {
        return std::make_shared<ConstantNode>(f, annotationIndex());
    }
        
    uint32_t i;
    if (integerValue(i)) {
        // Set the type to the smallest type value fits in.
        Type type;
        if (i <= 255) {
            type = Type::UInt8;
        } else if (i <= 65535) {
            type = Type::UInt16;
        } else {
            type = Type::UInt32;
        }
        
        return std::make_shared<ConstantNode>(type, i, annotationIndex());
    }
    
    std::string s;
    if (stringValue(s)) {
        return std::make_shared<StringNode>(s, annotationIndex());
    }
    
    return nullptr;
}

bool
Compiler::formalParameterList()
{
    Type t;
    if (!type(t)) {
        return true;
    }
    
    while (true) {
        bool isPointer = false;
        if (match(Token::Mul)) {
            isPointer = true;
        }
    
        std::string id;
        expect(identifier(id), Error::ExpectedIdentifier);
        
        // FIXME: set addr
        SymbolPtr sym = std::make_shared<Symbol>(id, t, isPointer, 1, 1);
        expect(currentFunction()->addArg(sym), Error::DuplicateIdentifier);
        
        if (!match(Token::Comma)) {
            return true;
        }

        expect(type(t), Error::ExpectedType);
    }
    
    return true;
}

bool
Compiler::argumentList(const ASTPtr& fun)
{
    expect(fun->astNodeType() == ASTNodeType::FunctionCall, Error::ExpectedFunction);
    FunctionPtr function = std::static_pointer_cast<FunctionCallNode>(fun)->function();
    
    int i = 0;
    while (true) {
        ASTPtr arg = expression();
        if (!arg) {
            if (i == 0) {
                break;
            }
            expect(false, Error::ExpectedExpr);
        }
        
        // Typecast arg as needed
        Type neededType;
        
        if (function->argCount() > i) {
            neededType = function->argType(i);
            expect(neededType != Type::None, Error::MismatchedType);
        } else {
            // This is past the last arg, it can be any type
            neededType = arg->type();
        }

        arg = TypeCastNode::castIfNeeded(arg, neededType, annotationIndex());
        fun->addNode(arg);
        
        i++;
        
        if (!match(Token::Comma)) {
            break;
        }
    }

    // FIXME: Handle case where fewer args are passed - push 0 if so
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
        { "true",       Reserved::True },
        { "false",      Reserved::False },
        { "float",      Reserved::Float },
        { "fixed",      Reserved::Fixed },
        { "int8",       Reserved::Int8 },
        { "uint8",      Reserved::UInt8 },
        { "int16",      Reserved::Int16 },
        { "uint16",     Reserved::UInt16 },
        { "int32",      Reserved::Int32 },
        { "uint32",     Reserved::UInt32 },
        { "bool",       Reserved::Bool },
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

