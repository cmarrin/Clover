/*-------------------------------------------------------------------------
    This source file is a part of Lucid
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#pragma once

#include <vector>

#include "Defines.h"
#include "Function.h"
#include "Symbol.h"

namespace lucid {

class Module
{
  public:
    Module(const std::string& name) : _name(name) { }
    
    const std::string& name() const { return _name; }
    const std::vector<Function>& functions() const { return _functions; }
    
    Function* addFunction(const std::string& name, Type returnType)
    {
        _functions.emplace_back(name, returnType);
        return &(_functions.back());
    }

    void addNativeFunction(const char* name, NativeId nativeId, Type returnType, const SymbolList& locals)
    {
        _functions.emplace_back(name, nativeId, returnType, locals);
    }
    
    Function* findFunction(const std::string& s)
    {
        const auto& it = find_if(_functions.begin(), _functions.end(),
                [s](Function& f) { return f.name() == s; });

        if (it != _functions.end()) {
            return &(*it);
        }
        return nullptr;
    }

    
  private:
    std::string _name;
    std::vector<Function> _functions;
};

using ModulePtr = std::shared_ptr<Module>;

}
