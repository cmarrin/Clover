/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "Module.h"

#include "Function.h"
#include "Symbol.h"

using namespace clvr;

Module::~Module()
{
    setName(nullptr);
}

void
Module::setName(const char* name)
{
    // We don't store as a std::string so we don't need it on Arduino
    if (_name) {
        delete [ ] _name;
        _name = nullptr;
    }
    if (!name) {
        return;
    }
    
    _name = new char[strlen(name) + 1];
    strcpy(_name, name);
}

FunctionPtr
Module::findFunction(const std::string& s)
{
    const auto& it = find_if(_functions.begin(), _functions.end(),
            [s](FunctionPtr& f) { return f->name() == s; });

    if (it != _functions.end()) {
        return *it;
    }
    return nullptr;
}

const FunctionList&
Module::functions() const
{
    return _functions;
}

FunctionPtr
Module::addFunction(const std::string& structName, const std::string& name, Type returnType)
{
    _functions.push_back(std::make_shared<Function>(structName, name, returnType));
    return _functions.back();
}

void
Module::addFunctions(FunctionList list)
{
    _functions.insert(_functions.end(), list.begin(), list.end());
}
