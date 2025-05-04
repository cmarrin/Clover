/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#pragma once

#include "Defines.h"

namespace clvr {

#ifndef ARDUINO
class Module
{
  public:
    Module(const std::string& name)
        : _name(name)
    { }
    
    const std::string& name() const { return _name; }
    const FunctionList& functions() const;
    FunctionPtr addFunction(const std::string& structName, const std::string& name, Type returnType);
    void addNativeFunction(const char* name, uint16_t nativeId, Type returnType, const SymbolList& locals);
    FunctionPtr findFunction(const std::string& s);

  private:
    std::string _name;
    FunctionList _functions;
};

using ModulePtr = std::shared_ptr<Module>;
#else
using ModulePtr = void*;
#endif
}
