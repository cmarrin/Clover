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
    // Probably do some init
    
    return processNextASTNode(ast);
}

bool
Codegen::processNextASTNode(const ASTPtr& ast)
{
    // Traverse the AST and use the expression stack to generate the code
    if (ast->isTerminal()) {
        _exprStack.push_back(ast);
        return true;
    }

    if (ast->isList()) {
        // Just walk all the children. There can be no null child, seeing a null stops the iteration
        for (uint32_t i = 0; ; ++i) {
            ASTPtr child = ast->child(i);
            if (!child) {
                break;
            }
            if (!processNextASTNode(child)) {
                return false;
            }
        }
        return true;
    }
    
    // This is a node that performs some operation, bake it
    bakeExpr(ast);
     
    return false;
}

Type
Codegen::bakeExpr(const ASTPtr& ast)
{
    // Passed ast is either a unary or binary operation.
    // exprStack will have one or two entries which are
    // the operands. If it takes one param is could be
    // either a pre or post op, depending on whether it
    // has a left or right child.We need to generate code
    // for them and then for this operation
    
    if (!ast->isUnary()) {
        // There should be 2 entries on the stack, right is on tos
        assert(_exprStack.size() >= 2);
        
        const ExprEntry& right = _exprStack.back();
        _exprStack.pop_back();
        
        right.addCode(*_code, false);
        
        const ExprEntry& left = _exprStack.back();
        _exprStack.pop_back();
        left.addCode(*_code, ast->isAssignment());
        
        // Now emit the op
        ast->addCode(*_code, false);
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

    return Type::None;
}

void Codegen::addCode(std::vector<uint8_t>& code)
{
    //code.push_back(opByTypeTable[uint8_t(_symbol->type())]._ldL);
    
    // emit the index byte
}
    
