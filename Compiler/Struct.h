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

#include "Function.h"
#include "Symbol.h"

namespace lucid {

class Struct;
using StructPtr = std::shared_ptr<Struct>;

class Struct
{
public:
    Struct(const std::string& name, Type type)
        : _name(name)
        , _type(type)
    { }
    
    const std::vector<Function>& functions() const { return _functions; }
    const std::vector<SymbolPtr>& locals() const { return _locals; }
    
    const std::string& name() const { return _name; }
    Type type() const { return _type; }
    uint8_t size() const { return _localSize; }
    
    const std::vector<StructPtr>& structs() const { return _structs; }
    
    Function* addFunction(const std::string& name, Type returnType)
    {
        _functions.emplace_back(name, returnType);
        return &(_functions.back());
    }

    StructPtr addStruct(const std::string& name, Type type)
    {
        _structs.push_back(std::make_shared<Struct>(name, type));
        return _structs.back();
    }

    bool addLocal(const std::string& name, Type type, uint8_t size, bool ptr)
    {
        // Check for duplicates
        SymbolPtr symbol = findLocal(name);
        if (symbol) {
            return false;
        }
        _locals.push_back(std::make_shared<Symbol>(name, type, size, ptr));
        _localSize += size;
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

    const ASTPtr& astNode() const { return _astNode; }

    void addASTNode(const ASTPtr& node) { _astNode->addNode(node); }

private:
    std::string _name;
    std::vector<StructPtr> _structs;
    std::vector<SymbolPtr> _locals;
    std::vector<Function> _functions;
    uint8_t _localSize = 0;
    uint8_t _size = 0;
    Type _type = Type::None;
    ASTPtr _astNode = std::make_shared<StatementsNode>();
};

}
