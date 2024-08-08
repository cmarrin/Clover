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
    coreModule->addNativeFunction("printf", NativeId::PrintF, Type::None, {{ "s", Type::String, true, 1, 1 }});
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
    appendValue(executable, topLevelSize, Type::UInt32);
    
    // Write constant size and then constants
    appendValue(executable, uint16_t(_constants.size()), Type::UInt16);
    for (auto it : _constants) {
        executable.push_back(it);
    }
    
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
    }

    return _error == Error::None;
}
bool
Compiler::program()
{
    _scanner.setIgnoreNewlines(true);
    
    try {
        while(import()) { }
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
    bool isConstant = false;
    
    if (match(Reserved::Const)) {
        isConstant = true;
    }
    
    Type t = Type::None;
    std::string id;
    
    if (!type(t)) {
        return false;
    }

    bool isPointer = false;
    if (match(Token::Mul)) {
        expect(!isConstant, Error::PointerConstantNotAllowed);
        isPointer = true;
    }
    
    expect(var(t, isPointer, isConstant), Error::ExpectedVar);

    expect(Token::Semicolon);
    return true;
}

bool
Compiler::var(Type type, bool isPointer, bool isConstant)
{
    std::string id;
    expect(identifier(id), Error::ExpectedIdentifier);
    
    StructPtr struc;
    
    if (isStruct(type)) {
        struc = typeToStruct(type);
    }
    
    SymbolPtr sym;
    uint32_t nElements = 1;

    if (match(Token::OpenBracket)) {
        if (!integerValue(nElements)) {
            nElements = 0;
        }
        expect(Token::CloseBracket);
    }
    
    sym = std::make_shared<Symbol>(id, type, isPointer, struc ? struc->size() : typeToBytes(type), nElements, isConstant);
    expect(sym != nullptr, Error::DuplicateIdentifier);
    
    // Check for an initializer.
    ASTPtr ast;
    AddrNativeType addr = 0;
    uint16_t nConstElements = 0;
    
    if (match(Token::Equal)) {
        if (isConstant) {
            // Collect the constant initializers
            collectConstants(type, nElements == 1, addr, nConstElements);
        } else if ((uint8_t(type) < StructTypeStart && nElements == 1) || isPointer) {
            // Built-in scalar type. Generate an expression
            ast = expression();
            expect(ast != nullptr, Error::ExpectedExpr);
        } else {
            // Struct or array type, collect initializers
            expect(Token::OpenBrace);
            ast = expression();
            if (ast) {
                // FIXME: For now ignore the initializers
                while (match(Token::Comma)) {
                    ast = expression();
                    
                    // Allow trailing comma
                    if (ast == nullptr) {
                        break;
                    }
                }
            }
            expect(Token::CloseBrace);
        }
    } else {
        expect(nElements != 0, Error::EmptyArrayRequiresInitializer);
    }

    // Put the var in the current struct unless we're in a function, then put it in _locals    
    if (_inFunction) {
        currentFunction()->addLocal(sym, addr, nConstElements);
    } else {
        expect(!_structStack.empty(), Error::InternalError);
        currentStruct()->addLocal(sym, addr, nConstElements);
    }
    
    if (ast) {
        ASTPtr idNode = std::make_shared<VarNode>(sym, annotationIndex());
        
        // If var is a pointer, the rhs must be a pointer too. And both must be the same type
        if (isPointer) {
            expect(ast->isPointer(), Error::ExpectedRef);
            expect(type == ast->type(), Error::MismatchedType);
        }
        
        ASTPtr assignment = std::make_shared<AssignmentNode>(idNode, Op::NOP, ast, annotationIndex());
    
        if (_inFunction) {
            currentFunction()->addASTNode(assignment);
        } else {
            currentStruct()->addASTNode(assignment);
        }
    }
    
    return true;
}

void
Compiler::collectConstants(Type type, bool isScalar, AddrNativeType& addr, uint16_t& nElements)
{
    uint32_t i;
    addr = uint32_t(_constants.size());
    
    if (isScalar) {
        expect(value(i, type), Error::ExpectedValue);
        nElements = 1;
        appendValue(_constants, i, type);
        return;
    }
        
    expect(Token::OpenBrace);
    expect(value(i, type), Error::ExpectedValue);
    appendValue(_constants, i, type);

    nElements = 1;

    while (match(Token::Comma)) {
        // Allow trailing comma
        if (!value(i, type)) {
            break;
        }
        
        nElements += 1;
        appendValue(_constants, i, type);
    }
    expect(Token::CloseBrace);
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
    if (lastNode->astNodeType() != ASTNodeType::Return) {
        _currentFunction->addASTNode(std::make_shared<ReturnNode>(nullptr, annotationIndex()));
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
    if (lastNode->astNodeType() != ASTNodeType::Return) {
        _currentFunction->addASTNode(std::make_shared<ReturnNode>(nullptr, annotationIndex()));
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
Compiler::loopStatement()
{
    bool forLoop;
    
    if (match(Reserved::For)) {
        forLoop = true;
    } else if (match(Reserved::While)) {
        forLoop = false;
    } else {
        return false;
    }
    
    expect(Token::OpenParen);

    enterJumpContext();

    if (forLoop) {
        // Handle iterator init
        if (!match(Token::Semicolon)) {
            Type t = Type::None;
            type(t);
            
            // If we have a type this is a var declaration and must be an assignment
            // expression. Otherwise it can be a general arithmeticExpression
            ASTPtr lhs, rhs;
            SymbolPtr sym;
            
            std::string id;
            expect(identifier(id), Error::ExpectedIdentifier);

            // Create or access iterator symbol
            if (t != Type::None) {
                expect(isScalar(t), Error::WrongType);

                sym = std::make_shared<Symbol>(id, t, false, typeToBytes(t), 1, false);
                expect(sym != nullptr, Error::DuplicateIdentifier);
        
                currentFunction()->addLocal(sym);
            } else {
                sym = findSymbol(id);
                expect(sym != nullptr, Error::ExpectedVar);
            }
            
            // Get assignment
            expect(Token::Equal);
            lhs = std::make_shared<VarNode>(sym, annotationIndex());
            
            rhs = expression();
            expect(rhs != nullptr, Error::ExpectedExpr);
            
            // Iterator must be a scalar
            expect(!lhs->isPointer() && isScalar(lhs->type()), Error::IteratorMustBeScalar);
            expect(!rhs->isPointer() && isScalar(rhs->type()), Error::IteratorMustBeScalar);

            ASTPtr assignment = std::make_shared<AssignmentNode>(lhs, Op::NOP, rhs, annotationIndex());
            currentFunction()->addASTNode(assignment);

            expect(Token::Semicolon);
        }
    }
    
    // This is the start of the loop
    ASTPtr loopStartNode = std::make_shared<BranchNode>(BranchNode::Kind::LoopStart, annotationIndex());
    currentFunction()->addASTNode(loopStartNode);

    ASTPtr ifStartNode, ifEndNode, loopNextNode, iterNode, loopEndNode;

    // See if we have a loop condition
    Token endToken = forLoop ? Token::Semicolon : Token::CloseParen;
        
    if (!match(endToken)) {
        currentFunction()->addASTNode(expression());
        expect(endToken);

        ifStartNode = std::make_shared<BranchNode>(BranchNode::Kind::IfStart, annotationIndex());
        currentFunction()->addASTNode(ifStartNode);
    }

    if (forLoop) {
        // Handle iteration
        if (!match(Token::CloseParen)) {
            // This will go at the bottom of the loop
            iterNode = expression();
            expect(Token::CloseParen);
        }
    }
    
    statement();
    
    // Now emit the iteration. This is where continue goes
    loopNextNode = std::make_shared<BranchNode>(BranchNode::Kind::LoopNext, annotationIndex());
    currentFunction()->addASTNode(loopNextNode);

    // All continues branch to loopNextNode
    addJumpFixupNodes(BranchNode::Kind::Continue, BranchNode::Kind::LoopNext);

    if (iterNode) {
        currentFunction()->addASTNode(iterNode);
    
        // If the iterNode leaves something on the stack get rid of it
        if (iterNode->valueLeftOnStack()) {
            currentFunction()->addASTNode(std::make_shared<DropNode>(typeToBytes(iterNode->type()), annotationIndex()));
        }
    }
    
    // Now we're at the end of the loop
    loopEndNode = std::make_shared<BranchNode>(BranchNode::Kind::LoopEnd, annotationIndex());
    currentFunction()->addASTNode(loopEndNode);
    
    if (ifStartNode) {
        // IfStart branches to IfEnd
        ifEndNode = std::make_shared<BranchNode>(BranchNode::Kind::IfEnd, annotationIndex());
        currentFunction()->addASTNode(ifEndNode);
        std::static_pointer_cast<BranchNode>(ifEndNode)->setFixupNode(ifStartNode);
    }
    
    // All breaks branch to IfEnd
    addJumpFixupNodes(BranchNode::Kind::Break, BranchNode::Kind::IfEnd);

    // LoopEnd branches back to LoopStart
    std::static_pointer_cast<BranchNode>(loopEndNode)->setFixupNode(loopStartNode);
    
    exitJumpContext();

    return true;
}

bool
Compiler::returnStatement()
{
    if (!match(Reserved::Return)) {
        return false;
    }
    
    ASTPtr ast = expression();
    expect(ast != nullptr || currentFunction()->returnType() == Type::None, Error::MismatchedType);
    currentFunction()->addASTNode(std::make_shared<ReturnNode>(ast, annotationIndex()));
    
    expect(Token::Semicolon);
    return true;
}

bool
Compiler::jumpStatement()
{
    BranchNode::Kind kind;
    if (match(Reserved::Break)) {
        kind = BranchNode::Kind::Break;
    } else if (match(Reserved::Continue)) {
        kind = BranchNode::Kind::Continue;
    } else {
        return false;
    }
    
    // Make sure we're in a loop
    expect(!_jumpList.empty(), Error::OnlyAllowedInLoop);

    ASTPtr jump = std::make_shared<BranchNode>(kind, annotationIndex());
    addJumpEntry(jump);
    currentFunction()->addASTNode(jump);

    expect(Token::Semicolon);
    return true;
}

void
Compiler::addJumpFixupNodes(BranchNode::Kind jumpKind, const BranchNode::Kind targetKind)
{
    // Create a BranchNode for each branch of the right kind
    for (auto& it : _jumpList.back()) {
        if (std::reinterpret_pointer_cast<BranchNode>(it)->kind() == jumpKind) {
            ASTPtr node = std::make_shared<BranchNode>(targetKind, annotationIndex());
            currentFunction()->addASTNode(node);
            std::reinterpret_pointer_cast<BranchNode>(node)->setFixupNode(it);
        }
    }
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
    
    // If this is a function call, a return value will be pushed by
    // default. We don't need that so tell the function call to skip it
    if (node->astNodeType() == ASTNodeType::FunctionCall) {
        FunctionPtr function = std::static_pointer_cast<FunctionCallNode>(node)->function();
        function->setPushReturn(false);
    } else if (node->valueLeftOnStack()) {
        // We don't need the value, toss it
        currentFunction()->addASTNode(std::make_shared<DropNode>(typeToBytes(node->type()), annotationIndex()));
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
        
        if (opInfo.assignment()) {
            expect(lhs->isAssignable(), Error::ExpectedLHSExpr);
            
            // Validate types. Pointers are allowed for assignment as long as they match
            if (lhs->isPointer() || rhs->isPointer()) {
                expect(lhs->isPointer() && rhs->isPointer(), Error::PtrAssignmentMustMatch);
                expect(lhs->type() == rhs->type(), Error::PtrAssignmentMustMatch);
            }
            lhs = std::make_shared<AssignmentNode>(lhs, opcode, rhs, annotationIndex());
        } else {
            // Validate types. Only scalars allowed for binary ops
            expect(!lhs->isPointer() && !rhs->isPointer(), Error::PtrTypeNotAllowed);
            expect(isScalar(lhs->type()) && isScalar(rhs->type()), Error::MismatchedType);
            lhs = std::make_shared<OpNode>(lhs, opcode, rhs, opInfo.resultType(), false, annotationIndex());
        }
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
    bool isRef = false;
    bool isSigned = false;
    
    if (match(Token::Minus)) {
        opcode = Op::NEG;
        isSigned = true;
    } else if (match(Token::Twiddle)) {
        opcode = Op::NOT;
    } else if (match(Token::Bang)) {
        opcode = Op::LNOT;
        resultType = Type::UInt8;
    } else if (match(Token::Inc)) {
        opcode = Op::PREINC;
        isRef = true;
    } else if (match(Token::Dec)) {
        opcode = Op::PREDEC;
        isRef = true;
    } else if (match(Token::And)) {
        // Ref (addressof)
        node = unaryExpression();
        return std::make_shared<RefNode>(node, annotationIndex());
    }  else if (match(Token::Mul)) {
        // Deref (*)
        node = unaryExpression();
        
        // Node must be a ref
        expect(node->isPointer(), Error::ExpectedRef);
        return std::make_shared<DerefNode>(node, annotationIndex());
    } else {
        return nullptr;
    }
    
    node = unaryExpression();
    
    // If result needs to be signed (for Op::NEG) do a cast here
    if (isSigned && !node->isSigned()) {
        Type t = node->type();
        if (t == Type::UInt8) {
            t = Type::Int8;
        } else if (t == Type::UInt16) {
            t = Type::Int16;
        } else if (t == Type::UInt32) {
            t = Type::Int32;
        }
        node = TypeCastNode::castIfNeeded(node, t, annotationIndex());
    }
    
    return std::make_shared<OpNode>(opcode, node, resultType, isRef, annotationIndex());
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
            
            // If this is a pointer it might point at an array of the
            // underlying type. Assume that and deref. That's not
            // safe, but it's how C does it.
            if (lhs->isPointer()) {
                lhs = std::make_shared<DerefNode>(lhs, annotationIndex());
            } else {
                expect(lhs->isIndexable(), Error::ExpectedIndexable);
            }
            
            // array index does a PUSHREF of the lhs, then a PUSH of the rhs and then an INDEX with element size as operand
            lhs = std::make_shared<IndexNode>(lhs, rhs, annotationIndex());
            expect(Token::CloseBracket);
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
            
            // lhs must be a struct or pointer to struct. Find its definition
            Type structType = lhs->type();
            expect(isStruct(structType), Error::ExpectedStructType);
            expect(!lhs->isIndexable(), Error::MismatchedType);
            
            // Get the property
            SymbolPtr prop = typeToStruct(structType)->findLocal(id);
            expect(prop != nullptr, Error::PropertyDoesNotExist);
            
            // If this is a pointer to struct, deref it
            if (lhs->isPointer()) {
                lhs = std::make_shared<DerefNode>(lhs, annotationIndex());
            }
            
            // Make the node
            lhs = std::make_shared<DotNode>(lhs, prop, annotationIndex());
        } else if (match(Token::Inc)) {
            lhs = std::make_shared<OpNode>(lhs, Op::POSTINC, Type::None, true, annotationIndex());
        } else if (match(Token::Dec)) {
            lhs = std::make_shared<OpNode>(lhs, Op::POSTDEC, Type::None, true, annotationIndex());
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
        expect(ast != nullptr, Error::ExpectedExpr);
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
        
        ModulePtr module = findModule(id);
        if (module) {
            return std::make_shared<ModuleNode>(module, annotationIndex());
        }

        expect(false, Error::ExpectedVar);
    }

    float f;
    if (floatValue(f)) {
        return std::make_shared<ConstantNode>(f, annotationIndex());
    }
        
    uint32_t i;
    if (integerValue(i)) {
        // Set the type to the smallest type value fits in.
        Type type;
        if (i <= 127) {
            type = Type::UInt8;
        } else if (i <= 32767) {
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
            SymbolPtr sym = function->arg(i);
            expect(sym != nullptr, Error::InternalError);
            neededType = sym->type();
            
            // If both the sym and arg are scalar then we can cast.
            // Otherwise it's a type mismatch
            if (sym->isPointer() || arg->isPointer()) {
                expect(sym->isPointer() && arg->isPointer(), Error::MismatchedType);
                expect(sym->type() == arg->type(), Error::MismatchedType);
            }
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

    // if there are not enough args, pad with 0's
    while (i < function->argCount()) {
        fun->addNode(std::make_shared<ConstantNode>(function->arg(i)->type(), 0, annotationIndex()));
        i += 1;
    }
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
    if (match(Reserved::True)) {
        i = 1;
        return true;
    }
    
    if (match(Reserved::False)) {
        i = 0;
        return true;
    }
    
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

void
Compiler::addJumpEntry(const ASTPtr& jump)
{
    expect(!_jumpList.empty(), Error::InternalError);
    _jumpList.back().emplace_back(jump);
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

