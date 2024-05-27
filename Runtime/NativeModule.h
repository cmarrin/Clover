/*-------------------------------------------------------------------------
    This source file is a part of Lucid
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#pragma once

namespace lucid {

class ExecutionUnit;
class CompileEngine;

class NativeModule
{
public:
    virtual ~NativeModule() { }
    
    virtual bool hasId(uint8_t id) const = 0;
    virtual uint8_t numParams(uint8_t id) const = 0;
    virtual int32_t call(ExecutionUnit*, uint8_t id) = 0;
    
#ifndef ARDUINO
    virtual void addFunctions(CompileEngine*) = 0;
#endif
};

}
