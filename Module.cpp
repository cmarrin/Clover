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
Module::addNativeFunction(const char* name, uint16_t nativeId, Type returnType, const SymbolList& locals)
{
    _functions.push_back(std::make_shared<Function>(name, nativeId, returnType, locals));
}
