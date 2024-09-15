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
#include "CodeGen6809.h"
#include "CodeGenStackVM.h"
#include "NativeColor.h"
#include "NativeCore.h"
#include "OpInfo.h"

using namespace clvr;

Compiler::Compiler(OutputFormat fmt, std::istream* stream, Annotations* annotations) : _scanner(stream, annotations)
{
    if (fmt == OutputFormat::ASM6809) {
        _codeGen = new CodeGen6809;
    } else {
        _codeGen = new CodeGenStackVM;
    }
}

bool Compiler::compile(uint32_t maxExecutableSize, const std::vector<Module*>& modules)
{    
    // Add built-in native modules
    ModulePtr coreModule = NativeCore::createModule();
    _modules.push_back(coreModule);
    
    // FIXME: Modules other than core should be loaded somehow by 'import'
    ModulePtr colorModule = NativeColor::createModule();
    _modules.push_back(colorModule);
    
    program();
    
    if (_error != Error::None) {
        return false;
    }
    
    // Do 3 code generation:
    //
    //      1)  On the first pass we don't know how long most of the branch
    //          instructions need to be so we make them long. But as we fill
    //          in the jump addresses we remember which branches can be short
    //
    //      2)  Now go back for a second pass to shorten the ones that have
    //          been marked as such.
    //
    //      3)  But we're not done. If we have forward function declarations
    //          their addresses will have been set from the previous pass
    //          and if we've shortened any branches they will be wrong so
    //          we need a third pass to make them correct
    
    for (int i = 0; i < 3; i ++) {
        _codeGen->code().clear();
        _codeGen->emitPreamble(this);
        emitStruct(_codeGen, _topLevelStruct);
    }
    
    // Add the annotation info
    TraversalVisitor func = [this] (const ASTPtr& node) {
        int32_t index;
        int32_t addr;
        node->annotation(index, addr);
        if (index >= 0 && addr >= 0) {
            setAnnotation(index, addr);
        }
    };
    
    traverseStruct(_topLevelStruct, func);
    
    return _error == Error::None;
}

void
Compiler::traverseStruct(const StructPtr& struc, TraversalVisitor func) const
{
    //Emit child structs
    for (auto& itStruc : struc->structs()) {
        traverseStruct(itStruc, func);
    }
    
    // Emit functions
    for (auto& itFunc : struc->functions()) {
        traverseNode(itFunc->astNode(), func);
    }
}

void
Compiler::traverseNode(const ASTPtr& node,TraversalVisitor func) const
{
    if (node == nullptr) {
        return;
    }
    
    func(node);
    
    for (int i = 0; i < node->numChildren(); ++i) {
        traverseNode(node->child(i), func);
    }
}

void
Compiler::emitStruct(CodeGen* codeGen, const StructPtr& struc)
{
    //Emit structs
    for (auto& itStruc : struc->structs()) {
        emitStruct(codeGen, itStruc);
    }
    
    // Emit functions
    for (auto& itFunc : struc->functions()) {
        if (struc == _topLevelStruct) {
            // If this is the main function of the top level
            // struct, set the entry point
            if (itFunc->name() == "main") {
                setValue(codeGen->code(), MainEntryPointAddr, AddrNativeType(codeGen->code().size()), AddrSize);
            }
            
            // If this is the ctor function of the top level
            // struct, set the entry point
            if (itFunc->name() == "") {
                setValue(codeGen->code(), TopLevelCtorEntryPointAddr, AddrNativeType(codeGen->code().size()), AddrSize);
            }
        }
        
        // Set addr of this function
        itFunc->setAddr(AddrNativeType(codeGen->code().size()));
        codeGen->emitCode(itFunc->astNode(), false);
    }
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
    // If and when we support Clover imports, compile the import inline.
    // An import is a regular Clover program but only the first struct is
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
    Type structType = Type(_structTypes.size() + StructTypeStart);
    
    if (!_structStack.empty()) {
        // This is a child of another struct
        _structStack.push_back(currentStruct()->addStruct(id, structType));
        _structTypes.push_back(_structStack.back());
    } else {
        // This is the top level struct
        _topLevelStruct = std::make_shared<Struct>(id, structType);
        _structStack.push_back(_topLevelStruct);
        _structTypes.push_back(_structStack.back());
    }
    
    expect(Token::OpenBrace);
    
    while(structEntry()) { }
    
    expect(Token::CloseBrace);
    expect(Token::Semicolon);
    
    // If there was no ctor but there is init code, we need to add a dummy ctor.
    if (!currentStruct()->ctor() && currentStruct()->initASTNode()->numChildren() != 0) {
        FunctionPtr function = currentStruct()->addFunction("", Type::None);
    
        // ENTER has to be the first instruction in the Function.
        function->addASTNode(std::make_shared<EnterNode>(function));
    
        // init code goes right after ENTER
        function->addASTNode(currentStruct()->initASTNode());

        // Emit Return at the end
        function->addASTNode(std::make_shared<ReturnNode>(nullptr));
    }
    
    _structStack.pop_back();
    return true;
}

bool
Compiler::structEntry()
{
    if (strucT() || function() || ctor() || enuM()) {
        return true;
    }
    
    // varStatement needs to be last. It's looking for a type which could get confused with the ctor.
    // The parent passed to the varStatement is the ctor of the current struct because that's where
    // initialization code goes.
    return varStatement(currentStruct()->initASTNode());
}

bool
Compiler::enuM()
{
    if (!match(Reserved::Enum)) {
        return false;
    }
    
    std::string id;
    expect(identifier(id), Error::ExpectedIdentifier);

    // Add an emum
    // FIXME: handle too many enums
    EnumPtr e = addEnum(id);    
    currentStruct()->addEnum(e);
    
    expect(Token::OpenBrace);
    
    if (enumEntry(e)) {
        while(match(Token::Comma)) {
            expect(enumEntry(e), Error::ExpectedValue);
        }
    }
    
    expect(Token::CloseBrace);
    expect(Token::Semicolon);
    return true;
}

bool
Compiler::enumEntry(EnumPtr e)
{
    std::string id;
    if (!identifier(id)) {
        return false;
    }
    
    uint32_t i;
    
    if (match(Token::Equal)) {
        expect(integerValue(i), Error::ExpectedValue);
    } else {
        // Use the value after the prev enumEntry
        i = e->nextValue() + 1;
    }
    
    e->addValue(id, i);
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
Compiler::varStatement(const ASTPtr& parent)
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
    
    expect(var(parent, t, isPointer, isConstant), Error::ExpectedVar);

    expect(Token::Semicolon);
    return true;
}

bool
Compiler::var(const ASTPtr& parent, Type type, bool isPointer, bool isConstant)
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
    
    sym = std::make_shared<Symbol>(id, type, isPointer, struc ? struc->sizeInBytes() : typeToBytes(type), nElements);
    expect(sym != nullptr, Error::DuplicateIdentifier);
    
    // Check for an initializer.
    ASTPtr ast;
    AddrNativeType addr = 0;
    uint16_t nConstElements = nElements;
    
    if (match(Token::Equal)) {
        if (isConstant) {
            // Collect the constant initializers
            bool isScalarConstant;
            collectConstants(type, nElements != 1, addr, nConstElements, isScalarConstant);
            sym->setNElements(nConstElements);
            sym->setKind(isScalarConstant ? Symbol::Kind::ScalarConstant : Symbol::Kind::Constant);
        } else if ((!struc && nElements == 1) || isPointer) {
            // Built-in scalar or enum type. Generate an expression
            ast = expression();
            expect(ast != nullptr, Error::ExpectedExpr);
        } else {
            // Struct, enum or array type, collect initializers
            expect(Token::OpenBrace);
            
            // FIXME: For now only support struct
            if (struc != nullptr) {
                ASTPtr list = std::make_shared<InitializerNode>();
                
                ast = expression();
                if (ast) {
                    list->addNode(ast);
                    
                    while (match(Token::Comma)) {
                        ast = expression();
                        
                        // Allow trailing comma
                        if (ast == nullptr) {
                            break;
                        }
                        
                        list->addNode(ast);
                    }
                }
                
                ast = list;
            }
            
            expect(Token::CloseBrace);
        }
    } else {
        expect(nElements != 0 && !isConstant, Error::RequiresInitializer);
    }

    // Put the var in the current struct unless we're in a function, then put it in _locals    
    if (_inFunction) {
        currentFunction()->addLocal(sym, addr, nConstElements);
    } else {
        expect(!_structStack.empty(), Error::InternalError);
        currentStruct()->addLocal(sym, addr, nConstElements);
    }
    
    // If this is a struct, we want to call its ctor
    if (struc && struc->ctor() && !isConstant) {
        // We need to pass a ref to the struct instance we are constructing
        ASTPtr self = std::make_shared<VarNode>(sym);
        ASTPtr ctor = std::make_shared<FunctionCallNode>(struc->ctor(), self);
        std::static_pointer_cast<FunctionCallNode>(ctor)->setPushReturn(false);
        
        // See if we're passing args to ctor
        // If we have an initializer, we can't also have args
        if (match(Token::OpenParen)) {
            expect(ast == nullptr, Error::InitializerNotAllowed);
            expect(argumentList(ctor), Error::ExpectedArgList);
            expect(Token::CloseParen);
        } else {
            // if the ctor expects args, pad with 0's
            FunctionPtr function = std::static_pointer_cast<FunctionCallNode>(ctor)->function();
            for (int i = 0; i < function->argCount(); ++i) {
                ctor->addNode(std::make_shared<ConstantNode>(function->arg(i)->type(), 0));
            }
        }

        ctor->setAnnotationIndex(annotationIndex());
        parent->addNode(ctor);
    }
    
    if (ast) {
        ASTPtr idNode = std::make_shared<VarNode>(sym);
        
        // If var is a pointer, the rhs must be a pointer too. And both must be the same type
        if (isPointer) {
            expect(ast->isPointer(), Error::ExpectedRef);
            expect(type == ast->type(), Error::MismatchedType);
        }
        
        // If we have an initializer do an assignment on each value
        if (ast->astNodeType() == ASTNodeType::Initializer) {
            uint32_t n = ast->numChildren();
            StructPtr s = typeToStruct(type);
            
            for (uint32_t i = 0; i < n; ++i) {
                SymbolPtr sym = s->findLocal(i);
                ASTPtr node = ast->child(i);
                
                // We now have idNode, which is the struct, sym, which is the property in
                // that struct and node, which is the value to assign.
                ASTPtr dot = std::make_shared<DotNode>(idNode, sym);
                ASTPtr assignment = std::make_shared<AssignmentNode>(dot, Op::NOP, node);
                parent->addNode(assignment);
            }
        } else {
            ASTPtr assignment = std::make_shared<AssignmentNode>(idNode, Op::NOP, ast);
            parent->addNode(assignment);
        }
    }
    
    return true;
}

static Type nextStructProperty(const StructPtr& struc, uint16_t& propIndex)
{
    Type t;
    
    while (true) {
        t = struc->localType(propIndex++);
        if (t != Type::Function) {
            break;
        }
    }
    return t;
}

void
Compiler::collectConstants(Type type, bool isArray, AddrNativeType& addr, uint16_t& nElements, bool& isScalarConstant)
{
    uint32_t v;
    
    if (!isArray && isScalar(type)) {
        expect(value(v, type), Error::ExpectedValue);
        nElements = 1;
        addr = AddrNativeType(_scalarConstants.size());
        _scalarConstants.push_back(v);
        isScalarConstant = true;
        return;
    }

    isScalarConstant = false;
    addr = AddrNativeType(_constants.size());
       
    // This is an array of scalars, a struct or an array of structs
    expect(Token::OpenBrace);
    
    // There is an underlying type. If it's an array of scalars the type
    // is correct. Otherwise it's the type of the first struct property
    Type underlyingType = type;
    StructPtr struc = typeToStruct(type);
    uint16_t propIndex = 0;
    
    if (struc) {
        // Find the type of the next struct property
        underlyingType = nextStructProperty(struc, propIndex);
        
        // If there are no properties in the struct, it's an error
        expect(underlyingType != Type::None, Error::WrongNumberOfInitializers);
    }

    expect(value(v, type), Error::ExpectedValue);
    appendValue(_constants, v, typeToBytes(underlyingType));

    uint16_t nCollectedElements = 1;
    bool advanceNElements = false;

    while (match(Token::Comma)) {
        if (struc) {
            // Find the type of the next struct property
            underlyingType = nextStructProperty(struc, propIndex);
            if (underlyingType == Type::None) {
                // Start over (this will go to the next element if there is one
                propIndex = 0;
                underlyingType = nextStructProperty(struc, propIndex);
                if (isArray) {
                    advanceNElements = true;
                }
            }
        }
        
        // Allow trailing comma
        if (!value(v, underlyingType)) {
            break;
        }
        
        if (struc == nullptr || advanceNElements) {
            nCollectedElements += 1;
            advanceNElements = false;
        }
        
        appendValue(_constants, v, typeToBytes(underlyingType));
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
        
    // See if it's a struct or enum
    std::string id;
    if (!identifier(id, false)) {
        return false;
    }
    
    // First look in the child structs
    StructPtr struc = currentStruct();
    expect(struc != nullptr, Error::InternalError);
    struc = struc->findStruct(id);
    
    if (struc) {
        t = struc->type();
        _scanner.retireToken();
        return true;
    }
    
    // Now look at the peers, if any
    if (_structStack.size() > 1) {
        struc = _structStack[_structStack.size() - 2];
        struc = struc->findStruct(id);
        
        if (struc) {
            t = struc->type();
            _scanner.retireToken();
            return true;
        }
    }
    
    // Now see if it's an enum
    EnumPtr e = currentStruct()->findEnum(id);
    if (e) {
        t = e->type();
        _scanner.retireToken();
        return true;
    }        
    
    if (_topLevelStruct->name() == id) {
        t = _topLevelStruct->type();
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
    _currentFunction->addASTNode(std::make_shared<EnterNode>(_currentFunction));
    
    while(statement(_currentFunction->astNode())) { }
    
    expect(Token::CloseBrace);

    // Set the high water mark
    if (_nextMem > _localHighWaterMark) {
        _localHighWaterMark = _nextMem;
    }
    
    // If there's not a return at the end of the function, add one if
    // no return value is expected, or emit an error if one is.
    ASTPtr nodes = _currentFunction->astNode();
    ASTPtr lastNode = nodes->child(nodes->numChildren() - 1);
    if (lastNode->astNodeType() != ASTNodeType::Return) {
        expect(_currentFunction->returnType() == Type::None, Error::ExpectedReturnValue);
        _currentFunction->addASTNode(std::make_shared<ReturnNode>(nullptr));
    }
        
    _inFunction = false;
    _currentFunction = nullptr;
    return true;
}

bool
Compiler::ctor()
{
    // identifier may or may not be the name of the struct, if not, don't retire the id.
    std::string id;
    if (!identifier(id, false)) {
        return false;
    }
    
    if (id != currentStruct()->name()) {
        return false;
    }
    
    _scanner.retireToken();
    
    // The ctor method is a function that has no name and no return type
    // Remember the function. It gets created when the struct is created.
    // Implicit initializations go there (e.g., var initializations in
    // the struct). Make sure this is the only explicit ctor in this 
    // struct.
    expect(!currentStruct()->ctor(), Error::DuplicateIdentifier);
    _currentFunction = currentStruct()->addFunction("", Type::None);

    expect(_currentFunction->name().empty(), Error::InternalError);

    _inFunction = true;
    
    expect(Token::OpenParen);
    
    expect(formalParameterList(), Error::ExpectedFormalParams);
    
    expect(Token::CloseParen);
    expect(Token::OpenBrace);

    // ENTER has to be the first instruction in the Function.
    _currentFunction->addASTNode(std::make_shared<EnterNode>(_currentFunction));
    
    // init code goes right after ENTER
    _currentFunction->addASTNode(currentStruct()->initASTNode());

    while(statement(_currentFunction->astNode())) { }
    
    expect(Token::CloseBrace);

    // Set the high water mark
    if (_nextMem > _localHighWaterMark) {
        _localHighWaterMark = _nextMem;
    }
    
    // Emit Return at the end if there's not already one
    ASTPtr nodes = _currentFunction->astNode();
    ASTPtr lastNode = nodes->child(nodes->numChildren() - 1);
    if (lastNode->astNodeType() != ASTNodeType::Return) {
        _currentFunction->addASTNode(std::make_shared<ReturnNode>(nullptr));
    }
    
    _inFunction = false;
    _currentFunction = nullptr;
    return true;
}

bool
Compiler::statement(const ASTPtr& parent)
{
    if (compoundStatement(parent)) return true;
    if (ifStatement(parent)) return true;
    if (switchStatement(parent)) return true;
    if (loopStatement(parent)) return true;
    if (returnStatement(parent)) return true;
    if (jumpStatement(parent)) return true;
    if (varStatement(parent)) return true;
    if (expressionStatement(parent)) return true;
    return false;
}

bool
Compiler::compoundStatement(const ASTPtr& parent)
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
    
    while(statement(parent)) { }

    expect(Token::CloseBrace);
    
    // prune the locals added in this block
    if (_inFunction) {
        currentFunction()->pruneLocals(currentFunction()->numLocals() - numLocals);
    }
    return true;
}

bool
Compiler::ifStatement(const ASTPtr& parent)
{
    if (!match(Reserved::If)) {
        return false;
    }
    
    expect(Token::OpenParen);
    
    ASTPtr ast = expression();
    ast->setAnnotationIndex(annotationIndex());
    parent->addNode(ast);
    expect(Token::CloseParen);
    
    ASTPtr ifNode = std::make_shared<BranchNode>(BranchNode::Kind::IfStart);
    parent->addNode(ifNode);

    statement(parent);
    
    bool haveElse = false;
    ASTPtr elseNode;
    
    if (match(Reserved::Else)) {
        haveElse = true;
        elseNode = std::make_shared<BranchNode>(BranchNode::Kind::ElseStart);
        
        std::static_pointer_cast<BranchNode>(elseNode)->setFixupNode(ifNode);
        parent->addNode(elseNode);
        statement(parent);
    }

    ASTPtr endNode = std::make_shared<BranchNode>(BranchNode::Kind::IfEnd);
    parent->addNode(endNode);
    std::static_pointer_cast<BranchNode>(endNode)->setFixupNode(haveElse ? elseNode : ifNode);

    return true;
}

bool
Compiler::switchStatement(const ASTPtr& parent)
{
    if (!match(Reserved::Switch)) {
        return false;
    }
    
    expect(Token::OpenParen);
    
    ASTPtr expr = expression();
    expect(expr != nullptr, Error::ExpectedExpr);
    
    expect(Token::CloseParen);
    
    expect(Token::OpenBrace);
    
    ASTPtr switchStmt = std::make_shared<SwitchNode>(expr);
    switchStmt->setAnnotationIndex(annotationIndex());

    int32_t value;
    bool isDefault;
    while (caseClause(value, isDefault)) {
        ASTPtr stmt = std::make_shared<StatementsNode>();
        expect(statement(stmt), Error::ExpectedExpr);
        if (isDefault) {
            std::static_pointer_cast<SwitchNode>(switchStmt)->addCaseClause(stmt);
        } else {
            std::static_pointer_cast<SwitchNode>(switchStmt)->addCaseClause(value, stmt);
        }
    }
    
    expect(Token::CloseBrace);
    
    parent->addNode(switchStmt);
    
    return true;
}

bool
Compiler::caseClause(int32_t& value, bool& isDefault)
{
    isDefault = false;
    
    if (match(Reserved::Default)) {
        isDefault = true;
    } else if (!match(Reserved::Case)) {
        return false;
    }
    
    if (!isDefault) {
        // Case value must be integer constant. That means literal (including true and false), a scalar constant or an enum
        // FIXME: add support for enum
        ASTPtr ast = primaryExpression();
        expect(ast != nullptr && ast->astNodeType() == ASTNodeType::Constant && 
                (isInteger(ast->type()) || isEnum(ast->type())), Error::WrongType);

        // We know this is an integer constant node. Get the value
        value = std::static_pointer_cast<ConstantNode>(ast)->integerValue();
    }
    
    expect(Token::Colon);
    return true;
}

bool
Compiler::loopStatement(const ASTPtr& parent)
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

                sym = std::make_shared<Symbol>(id, t, false, typeToBytes(t), 1);
                expect(sym != nullptr, Error::DuplicateIdentifier);
        
                currentFunction()->addLocal(sym);
            } else {
                sym = findSymbol(id);
                expect(sym != nullptr, Error::ExpectedVar);
            }
            
            // Get assignment
            expect(Token::Equal);
            lhs = std::make_shared<VarNode>(sym);
            
            rhs = expression();
            expect(rhs != nullptr, Error::ExpectedExpr);
            
            // Iterator must be a scalar
            expect(!lhs->isPointer() && isScalar(lhs->type()), Error::IteratorMustBeScalar);
            expect(!rhs->isPointer() && isScalar(rhs->type()), Error::IteratorMustBeScalar);

            ASTPtr assignment = std::make_shared<AssignmentNode>(lhs, Op::NOP, rhs);
            parent->addNode(assignment);

            expect(Token::Semicolon);
        }
    }
    
    // This is the start of the loop
    ASTPtr loopStartNode = std::make_shared<BranchNode>(BranchNode::Kind::LoopStart);
    loopStartNode->setAnnotationIndex(annotationIndex());
    parent->addNode(loopStartNode);

    ASTPtr ifStartNode, ifEndNode, loopNextNode, iterNode, loopEndNode;

    // See if we have a loop condition
    Token endToken = forLoop ? Token::Semicolon : Token::CloseParen;
        
    if (!match(endToken)) {
        parent->addNode(expression());
        expect(endToken);

        ifStartNode = std::make_shared<BranchNode>(BranchNode::Kind::IfStart);
        parent->addNode(ifStartNode);
    }

    if (forLoop) {
        // Handle iteration
        if (!match(Token::CloseParen)) {
            // This will go at the bottom of the loop
            iterNode = expression();
            expect(Token::CloseParen);
        }
    }
    
    statement(parent);
    
    // Now emit the iteration. This is where continue goes
    loopNextNode = std::make_shared<BranchNode>(BranchNode::Kind::LoopNext);
    parent->addNode(loopNextNode);

    // All continues branch to loopNextNode
    addJumpFixupNodes(parent, BranchNode::Kind::Continue, BranchNode::Kind::LoopNext);

    if (iterNode) {
        parent->addNode(iterNode);
    
        // If the iterNode leaves something on the stack get rid of it
        if (iterNode->valueLeftOnStack()) {
            parent->addNode(std::make_shared<DropNode>(typeToBytes(iterNode->type())));
        }
    }
    
    // Now we're at the end of the loop
    loopEndNode = std::make_shared<BranchNode>(BranchNode::Kind::LoopEnd);
    parent->addNode(loopEndNode);
    
    if (ifStartNode) {
        // IfStart branches to IfEnd
        ifEndNode = std::make_shared<BranchNode>(BranchNode::Kind::IfEnd);
        parent->addNode(ifEndNode);
        std::static_pointer_cast<BranchNode>(ifEndNode)->setFixupNode(ifStartNode);
    }
    
    // All breaks branch to IfEnd
    addJumpFixupNodes(parent, BranchNode::Kind::Break, BranchNode::Kind::IfEnd);

    // LoopEnd branches back to LoopStart
    std::static_pointer_cast<BranchNode>(loopEndNode)->setFixupNode(loopStartNode);
    
    exitJumpContext();

    return true;
}

bool
Compiler::returnStatement(const ASTPtr& parent)
{
    if (!match(Reserved::Return)) {
        return false;
    }
    
    ASTPtr ast = expression();
    Type neededType = currentFunction()->returnType();
    
    if (neededType == Type::None) {
        expect(ast == nullptr, Error::UnexpectedReturnValue);
    } else {
        expect(ast != nullptr, Error::ExpectedReturnValue);
    }
    
    // If expr and return type are both scalar they can be cast,
    // otherwise it's a type mismatch
    if (ast) {
        if (isScalar(ast->type()) && isScalar(neededType)) {
            ast = TypeCastNode::castIfNeeded(ast, neededType);
        } else {
            expect(ast->type() == neededType, Error::MismatchedType);
        }
    }
    
    ASTPtr returnNode = std::make_shared<ReturnNode>(ast);
    returnNode->setAnnotationIndex(annotationIndex());
    parent->addNode(returnNode);
    
    expect(Token::Semicolon);
    return true;
}

bool
Compiler::jumpStatement(const ASTPtr& parent)
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

    ASTPtr jump = std::make_shared<BranchNode>(kind);
    jump->setAnnotationIndex(annotationIndex());

    addJumpEntry(jump);
    parent->addNode(jump);

    expect(Token::Semicolon);
    return true;
}

void
Compiler::addJumpFixupNodes(const ASTPtr& parent, BranchNode::Kind jumpKind, const BranchNode::Kind targetKind)
{
    // Create a BranchNode for each branch of the right kind
    for (auto& it : _jumpList.back()) {
        if (std::reinterpret_pointer_cast<BranchNode>(it)->kind() == jumpKind) {
            ASTPtr node = std::make_shared<BranchNode>(targetKind);
            parent->addNode(node);
            std::reinterpret_pointer_cast<BranchNode>(node)->setFixupNode(it);
        }
    }
}

bool
Compiler::expressionStatement(const ASTPtr& parent)
{
    ASTPtr node = expression();
    if (!node) {
        // allow empty statement
        return (match(Token::Semicolon));
    }
    
    expect(_inFunction, Error::InternalError);
    node->setAnnotationIndex(annotationIndex());
    parent->addNode(node);
    expect(Token::Semicolon);
    
    // If this is a function call, a return value will be pushed by
    // default. We don't need that so tell the function call to skip it
    if (node->astNodeType() == ASTNodeType::FunctionCall) {
        std::static_pointer_cast<FunctionCallNode>(node)->setPushReturn(false);
    } else if (node->valueLeftOnStack()) {
        // We don't need the value, toss it
        parent->addNode(std::make_shared<DropNode>(typeToBytes(node->type())));
    }
    return true;
}

ASTPtr
Compiler::expression()
{
    return arithmeticExpression(conditionalExpression(), 1);
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
            if (!findOpInfo(Operator(_scanner.getToken()), nextOpInfo) || nextOpInfo.prec() <= opInfo.prec()) {
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
            lhs = std::make_shared<AssignmentNode>(lhs, opcode, rhs);
        } else {
            // Validate types. Only scalars allowed for binary ops
            expect(!lhs->isPointer() && !rhs->isPointer(), Error::PtrTypeNotAllowed);
            expect(isScalar(lhs->type()) && isScalar(rhs->type()), Error::MismatchedType);
            
            // If this is Operator::LAnd or Operator::Lor generate a Logical AST
            if (opInfo.oper() == Operator::LOr || opInfo.oper() == Operator::LAnd) {
                LogicalNode::Kind kind = (opInfo.oper() == Operator::LOr) ? LogicalNode::Kind::LOr : LogicalNode::Kind::LAnd;
                lhs = std::make_shared<LogicalNode>(kind, lhs, rhs);
            } else {
                lhs = std::make_shared<OpNode>(lhs, opcode, rhs, opInfo.resultType(), false);
            }
        }
    }
    
    return lhs;
}

ASTPtr
Compiler::conditionalExpression()
{
    ASTPtr node = unaryExpression();
    if (!node) {
        return nullptr;
    }
    
    if (match(Token::Question)) {
        ASTPtr first = expression();
        expect(Token::Colon);
        ASTPtr second = expression();
        node = std::make_shared<ConditionalNode>(node, first, second);
    }
    return node;
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
        opcode = Op::NOT1;
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
        return std::make_shared<RefNode>(node);
    }  else if (match(Token::Mul)) {
        // Deref (*)
        node = unaryExpression();
        
        // Node must be a ref
        expect(node->isPointer(), Error::ExpectedRef);
        return std::make_shared<DerefNode>(node);
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
        node = TypeCastNode::castIfNeeded(node, t);
    }
    
    return std::make_shared<OpNode>(opcode, node, resultType, isRef);
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
                lhs = std::make_shared<DerefNode>(lhs);
            } else {
                expect(lhs->isIndexable(), Error::ExpectedIndexable);
            }
            
            // array index does a PUSHREF of the lhs, then a PUSH of the rhs and then an INDEX with element size as operand
            // Index can be 8 or 16 bit only
            expect(rhs->type() == Type::Int8  || rhs->type() == Type::UInt8 ||
                   rhs->type() == Type::Int16 || rhs->type() == Type::UInt16, Error::WrongType);
            lhs = std::make_shared<IndexNode>(lhs, rhs);
            expect(Token::CloseBracket);
        } else if (match(Token::Dot)) {
            std::string id;
            expect(identifier(id), Error::ExpectedIdentifier);
            
            // If lhs is a module, this has to be a function inside that module.
            if (lhs->astNodeType() == ASTNodeType::Module) {
                uint8_t moduleId = std::static_pointer_cast<ModuleNode>(lhs)->id();
                
                expect(moduleId < _modules.size(), Error::InternalError);
                FunctionPtr f = _modules[moduleId]->findFunction(id);
                expect(f != nullptr, Error::UndefinedIdentifier);
                lhs = std::make_shared<FunctionCallNode>(f, moduleId);
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
                lhs = std::make_shared<DerefNode>(lhs);
            }
            
            // If this is a function, make it a member function with lhs as the instance
            if (prop->type() == Type::Function) {
                lhs = std::make_shared<FunctionCallNode>(prop->function(), lhs);
            } else {
                // Otherwise make the dot node
                lhs = std::make_shared<DotNode>(lhs, prop);
            }
        } else if (match(Token::Inc)) {
            lhs = std::make_shared<OpNode>(lhs, Op::POSTINC, Type::None, true);
        } else if (match(Token::Dec)) {
            lhs = std::make_shared<OpNode>(lhs, Op::POSTDEC, Type::None, true);
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
    
    // See if this is a type cast or an enum
    Type t;
    if (type(t)) {
        // A type cast looks like a function with a single arg
        if (match(Token::OpenParen)) {
            // This is a type cast
            ASTPtr arg = expression();
            expect(Token::CloseParen);        
            return std::make_shared<TypeCastNode>(t, arg);
        } else {
            // The only other choice is an Enum deref, which means a dot must follow then an id
            expect(isEnum(t), Error::ExpectedEnum);
            expect(Token::Dot);
            
            std::string id;
            expect(identifier(id), Error::ExpectedEnum);
            
            EnumPtr e = typeToEnum(t);
            int32_t value;
            expect(e->findValue(id, value), Error::ExpectedEnum);
            return std::make_shared<ConstantNode>(t, value);
        }
    }
    
    std::string id;
    if (identifier(id)) {
        SymbolPtr symbol = findSymbol(id);
        if (symbol) {
            // This could be a var, scalar constant or function. Create the proper ASTNode
            if (symbol->type() == Type::Function) {
                return std::make_shared<FunctionCallNode>(symbol->function());
            } else if (symbol->kind() == Symbol::Kind::ScalarConstant) {
                Index index;
                AddrNativeType addr = symbol->addr(index);
                int32_t v = _scalarConstants[addr];
                if (symbol->type() == Type::Float) {
                    return std::make_shared<ConstantNode>(intToFloat(v));
                } else {
                    return std::make_shared<ConstantNode>(symbol->type(), v);
                }
            } else {
                // It's a Var or Constant symbol
                return std::make_shared<VarNode>(symbol);
            }
        }
        
        int16_t moduleId = findModuleId(id);
        if (moduleId >= 0) {
            return std::make_shared<ModuleNode>(moduleId);
        }

        expect(false, Error::ExpectedVar);
    }

    float f;
    if (floatValue(f)) {
        return std::make_shared<ConstantNode>(f);
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
        
        return std::make_shared<ConstantNode>(type, i);
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
        
        StructPtr struc = typeToStruct(t);
        
        // If this is a struct, pass it as a pointer
        if (struc) {
            isPointer = true;
        }

        SymbolPtr sym = std::make_shared<Symbol>(id, t, isPointer, struc ? struc->sizeInBytes() : typeToBytes(t), 1);
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
            
            // If the arg is a struct or an array and we're expecting a pointer ref it
            if (sym->isPointer() && !arg->isPointer() && (isStruct(arg->type()) || arg->isIndexable())) {
                arg = std::make_shared<RefNode>(arg);
            } else if (sym->isPointer() || arg->isPointer()) {
                // If both the sym and arg are scalar then we can cast.
                // Otherwise it's a type mismatch
                expect(sym->isPointer() && arg->isPointer(), Error::MismatchedType);
                
                // We have a special case. If the format arg is of Type::None it
                // means it can take a pointer to any type. This is to allow
                // core.memset to take any pointer and initialize it
                expect(sym->type() == Type::None || sym->type() == arg->type(), Error::MismatchedType);
            } else if (!isScalar(arg->type()) || !isScalar(neededType)) {
                // We only automatically cast between scalars
                expect(arg->type() == neededType, Error::MismatchedType);
            }
        } else {
            // We are past the last arg. That makes this a vararg call. We upcast any integral
            // types to 32 bits.
            if (!arg->isPointer() && (isStruct(arg->type()) || arg->isIndexable())) {
                arg = std::make_shared<RefNode>(arg);
            }
            
            neededType = arg->type();
            if (isEnum(neededType)) {
                neededType = Type::UInt8;
            }
            
            switch (neededType) {
                case Type::Int8:
                case Type::Int16: neededType = Type::Int32; break;
                case Type::UInt8:
                case Type::UInt16: neededType = Type::UInt32; break;
                default: break;
            }
        }

        arg = TypeCastNode::castIfNeeded(arg, neededType);
        fun->addNode(arg);
        
        i++;
        
        if (!match(Token::Comma)) {
            break;
        }
    }

    // if there are not enough args, pad with 0's
    while (i < function->argCount()) {
        fun->addNode(std::make_shared<ConstantNode>(function->arg(i)->type(), 0));
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
        { "for",        Reserved::For },
        { "if",         Reserved::If },
        { "else",       Reserved::Else },
        { "switch",     Reserved::Switch },
        { "case",       Reserved::Case },
        { "default",    Reserved::Default },
        { "struct",     Reserved::Struct },
        { "enum",       Reserved::Enum },
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

int16_t
Compiler::findModuleId(const std::string& id)
{
    auto it = find_if(_modules.begin(), _modules.end(),
                    [id](const ModulePtr& m) { return m->name() == id; });
    if (it != _modules.end()) {
        return it - _modules.begin();
    }
    return -1;
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
            StructPtr s = typeToStruct(type);
            return (s == nullptr) ? 0 : s->sizeInBytes();
    }
}

