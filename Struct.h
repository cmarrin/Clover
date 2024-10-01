/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
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

namespace clvr {

class Struct : public Module
{
public:
    Struct(const std::string& name, Type type)
        : Module(name)
        , _type(type)
    { }
    
    Type localType(uint16_t i) const { return (i >= _locals.size()) ? Type::None : _locals[i]->type(); }
    uint16_t numLocals() const { return _locals.size(); }
    
    Type type() const { return _type; }
    uint8_t sizeInBytes() const { return _localSize; }
    
    FunctionPtr ctor() const { return _ctor; }
    
    const std::vector<StructPtr>& structs() const { return _structs; }
    
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
        
        FunctionPtr function = Module::addFunction(Module::name(), name, returnType);
        _locals.push_back(std::make_shared<Symbol>(function));

        if (name.empty()) {
            // This is the ctor
            // We're not checking to see if we already have a ctor. That should 
            // be done by the caller
            _ctor = function;
        }
        
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
        sym->setAddr(_localSize, Index::M);
        _localSize += sym->sizeInBytes();
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

    // With this form, get the local at the given index, skipping functions.
    SymbolPtr findLocal(uint32_t i)
    {
        for (auto& it : _locals) {
            if (it->type() == Type::Function) {
                continue;
            }
            if (i == 0) {
                return it;
            }
            i -= 1;
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

    const ASTPtr& initASTNode() const { return _initASTNode; }

    bool hasCtor() const { return _ctor != nullptr; }

private:
    std::vector<StructPtr> _structs;
    std::vector<EnumPtr> _enums;
    std::vector<SymbolPtr> _locals;
    uint8_t _localSize = 0;
    Type _type = Type::None;
    ASTPtr _initASTNode = std::make_shared<StatementsNode>();
    FunctionPtr _ctor;
};

}
