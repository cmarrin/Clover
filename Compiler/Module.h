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

class Module
{
  public:
    Module() { }
    ~Module();
    
    void setName(const char* name);
    const char* name() const { return _name; }
    const FunctionList& functions() const;
    FunctionPtr addFunction(const std::string& structName, const std::string& name, Type returnType);
    FunctionPtr findFunction(const std::string& s);
    void addFunctions(FunctionList);

  private:
    char* _name = nullptr;
    FunctionList _functions;
};

using ModulePtr = std::shared_ptr<Module>;
}
