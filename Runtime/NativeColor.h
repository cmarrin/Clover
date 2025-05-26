/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

// NativeColor
//
// static color specific methods

#pragma once

#include "Defines.h"
#include "Interpreter.h"

#include <stdint.h>

namespace clvr {

class NativeColor
{
public:
    enum class Id {
        SetLight      = 0,
    };
    
    static const FunctionList create();
    
    static void callNative(uint16_t id, InterpreterBase*);
};

}
