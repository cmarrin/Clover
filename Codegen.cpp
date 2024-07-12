/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Lucid
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "Codegen.h"

using namespace lucid;

bool
Codegen::processAST(const ASTPtr& ast)
{
    return processNextASTNode(ast, false);
}

bool
Codegen::processNextASTNode(const ASTPtr& ast, bool isLHS)
{
    // Traverse the AST and use the expression stack to generate the code
    if (ast->isTerminal()) {
        ast->addCode(*_code, isLHS);
        return true;
    }

    if (ast->isList()) {
        assert(!isLHS);
        
        // Just walk all the children. There can be no null child, seeing a null stops the iteration
        for (uint32_t i = 0; ; ++i) {
            ASTPtr child = ast->child(i);
            if (!child) {
                break;
            }
            if (!processNextASTNode(child, false)) {
                return false;
            }
        }
        
        // A node with lists can be a function, so it could do an operation
        ast->addCode(*_code, false);
        
        return true;
    }
    
    // This is a node that performs an operation, process it's operands first
    if (ast->child(0)) {
        processNextASTNode(ast->child(0), ast->isAssignment());
    }
    
    // FIXME: We need to distinguish between pre and post unary operations
    if (ast->child(1)) {
        processNextASTNode(ast->child(1), false);
    }
    
    ast->addCode(*_code, false);
     
    return true;
}

    
    
    
//
//    Type type = Type::None;
//    ExprEntry entry = _exprStack.back();
//    Symbol sym;
//
//    switch(action) {
//        default: break;
//        case ExprAction::Right:
//            // Types must match
//            if (entry.type() != matchingType) {
//                return Type::None;
//            }
//
//            switch(entry.type()) {
//                default:
//                    assert(false);
//                    break;
//                case ExprEntry::Type::Int: {
//
//                    int32_t i = int32_t(entry);
//
//                    if (i <= 15) {
//                        addOpSingleByteIndex(Op::PushIntConstS, i);
//                    } else if (i <= 255) {
//                        addOpInt(Op::PushIntConst, i);
//                    } else {
//                        // Add an int const
//                        addOpInt(Op::Push, findInt(i));
//                    }
//                    type = Type::Int;
//                    break;
//                }
//                case ExprEntry::Type::Float:
//                    // Use an fp constant
//                    addOpInt(Op::Push, findFloat(entry));
//                    type = Type::Float;
//                    break;
//                case ExprEntry::Type::Id:
//                    // Push the value
//                    expect(findSymbol(entry, sym), Compiler::Error::UndefinedIdentifier);
//                    addOpId(Op::Push, sym.addr());
//
//                    if (sym.isPointer() && matchingType != Type::Ptr) {
//                        addOp(Op::PushDeref);
//                        type = sym.type();
//                        break;
//                    }
//
//                    type = sym.isPointer() ? Type::Ptr : sym.type();
//                    break;
//                case ExprEntry::Type::Ref: {
//                    // FIXME: ???
//                    // If this is a ptr and the matchingType is not Ptr
//                    // then we want to leave the ref on TOS, not the value
//                    const ExprEntry::Ref& ref = entry;
//                    type = ref._type;
//                    if (!ref._ptr) {
//                        addOp(Op::PushDeref);
//                    } else {
//                        type = Type::Ptr;
//                    }
//                    break;
//                }
//                case ExprEntry::Type::Value: {
//                    // Nothing to do, this just indicates that TOS is a value
//                    const ExprEntry::Value& value = entry;
//                    type = value._type;
//                    break;
//                }
//            }
//            break;
//        case ExprAction::Index: {
//            // TOS has an index, get the sym for the var so
//            // we know the size of each element
//            if (entry.type() == ExprEntry::Type::Ref) {
//                // This is a ref, get the size from the type
//                const ExprEntry::Ref& ref = entry;
//                type = ref._type;
//            } else {
//                expect(entry.type() == ExprEntry::Type::Id, Compiler::Error::ExpectedIdentifier);
//                expect(findSymbol(entry, sym), Compiler::Error::UndefinedIdentifier);
//                expect(sym.storage() == Symbol::Storage::Local ||
//                       sym.storage() == Symbol::Storage::Global, Compiler::Error::ExpectedVar);
//
//                type = sym.type();
//                _exprStack.pop_back();
//                _exprStack.push_back(ExprEntry::Ref(type));
//            }
//
//            Struct s;
//            addOpSingleByteIndex(Op::Index, structFromType(type, s) ? s.size() : 1);
//            return type;
//        }
//        case ExprAction::Offset: {
//            // Prev entry has a Ref. Get the type so we can get an element index
//            // we know the size of each element
//            expect(_exprStack.size() >= 2, Compiler::Error::InternalError);
//            const ExprEntry& prevEntry = _exprStack.end()[-2];
//            expect(prevEntry.type() == ExprEntry::Type::Ref, Compiler::Error::InternalError);
//            const ExprEntry::Ref& ref = prevEntry;
//
//            // If the Ref is a Ptr then we need to deref
//            if (ref._ptr) {
//                addOp(Op::PushDeref);
//            }
//            uint8_t index;
//            Type elementType;
//            findStructElement(ref._type, entry, index, elementType);
//            _exprStack.pop_back();
//            _exprStack.pop_back();
//            _exprStack.push_back(ExprEntry::Ref(elementType));
//
//            addOpSingleByteIndex(Op::Offset, index);
//            return elementType;
//        }
//        case ExprAction::Left:
//        case ExprAction::Ref:
//            if (entry.type() == ExprEntry::Type::Ref) {
//                // Already have a ref
//                const ExprEntry::Ref& ref = entry;
//                type = ref._type;
//
//                // If this is a Ptr action, we want to say that we want the stack to have
//                // a reference to a value rather than the value. So add the _ptr value
//                // to the Ref
//                if (action == ExprAction::Left) {
//                    // If the matching type is a Ptr then we just assign as usual.
//                    // If the ref is a pointer, we need to get the value at the
//                    // ref and use that as the ref for the PopDeref.
//                    if (ref._ptr && matchingType != Type::Ptr) {
////                        addOp(Op::Swap);
//                        addOp(Op::PushDeref);
////                        addOp(Op::Swap);
//                        type = ref._type;
//                    } else {
//                        type = ref._ptr ? Type::Ptr : ref._type;
//                    }
//                    addOp(Op::PopDeref);
//                    break;
//                }
//                return type;
//            }
//
//            expect(entry.type() == ExprEntry::Type::Id, Compiler::Error::ExpectedIdentifier);
//
//            // Turn this into a Ref
//            expect(findSymbol(entry, sym), Compiler::Error::UndefinedIdentifier);
//            _exprStack.pop_back();
//            _exprStack.push_back(ExprEntry::Ref(sym.type(), sym.isPointer()));
//
//            addOpId(Op::PushRef, sym.addr());
//            return sym.isPointer() ? Type::Ptr : sym.type();
//    }
//
//    _exprStack.pop_back();
//    return type;
