/*-------------------------------------------------------------------------
    This source file is a part of Lucid
    For the latest info, see https://github.com/cmarrin/LucidVM
    Copyright (c) 2021-20224, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

// base compiler internals

#pragma once

#include "Enum.h"
#include "Function.h"
#include "Module.h"
#include "Symbol.h"

namespace lucid {

class Struct : public Module
{
public:
    Struct(const std::string& name, Type type)
        : Module(name)
        , _type(type)
    {
        // Add ctor
        Module::addFunction("", Type::None);
    }
    
    Type localType(uint16_t i) const { return (i >= _locals.size()) ? Type::None : _locals[i]->type(); }
    uint16_t numLocals() const { return _locals.size(); }
    
    Type type() const { return _type; }
    uint8_t size() const { return _localSize; }
    
    StructPtr addStruct(const std::string& name, Type type)
    {
        _structs.push_back(std::make_shared<Struct>(name, type));
        return _structs.back();
    }

    void addEnum(const EnumPtr& e) { _enums.push_back(e); }

    FunctionPtr addFunction(const std::string& name, Type returnType)
    {
        SymbolPtr symbol = findLocal(name);
        if (symbol) {
            return nullptr;
        }
        
        FunctionPtr function = Module::addFunction(name, returnType);
        _locals.push_back(std::make_shared<Symbol>(function));
        return function;
    }

    bool addLocal(const SymbolPtr& sym, AddrNativeType addr = 0, uint16_t nElements = 0)
    {
        // Check for duplicates
        SymbolPtr symbol = findLocal(sym->name());
        if (symbol) {
            return false;
        }
        
        _locals.push_back(sym);
        
        if (sym->kind() != Symbol::Kind::Var) {
            sym->setAddr(addr, Index::C);
            sym->setNElements(nElements);
            return true;
        }
        
        // Locals start at 0 and go positive. Their addresses are relative
        // to the structure's self pointer, stored in the Y register.
        sym->setAddr(_localSize, Index::Y);
        _localSize += sym->size();
        return true;
    }

    SymbolPtr findLocal(const std::string& s)
    {
        const auto& it = find_if(_locals.begin(), _locals.end(),
                [s](const SymbolPtr& p) { return p->name() == s; });

        if (it != _locals.end()) {
            return *it;
        }
        return nullptr;
    }

    StructPtr findStruct(const std::string& s)
    {
        const auto& it = find_if(_structs.begin(), _structs.end(),
                [s](const StructPtr& p) { return p->name() == s; });

        if (it != _structs.end()) {
            return *it;
        }
        return nullptr;
    }

    EnumPtr findEnum(const std::string& s)
    {
        const auto& it = find_if(_enums.begin(), _enums.end(),
                [s](const EnumPtr& p) { return p->name() == s; });

        if (it != _enums.end()) {
            return *it;
        }
        return nullptr;
    }

    const ASTPtr& astNode() const { return _astNode; }
    const ASTPtr& initASTNode() const { return _initASTNode; }

    void addASTNode(const ASTPtr& node) { _astNode->addNode(node); }
    
    bool haveExplicitCtor() const { return _haveExplicitCtor; }
    void setHaveExplicitCtor() { _haveExplicitCtor = true; }

private:
    std::vector<StructPtr> _structs;
    std::vector<EnumPtr> _enums;
    std::vector<SymbolPtr> _locals;
    uint8_t _localSize = 0;
    Type _type = Type::None;
    ASTPtr _astNode = std::make_shared<StatementsNode>(-1);
    ASTPtr _initASTNode = std::make_shared<StatementsNode>(-1);
    bool _haveExplicitCtor = false;
};

}
