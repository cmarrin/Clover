/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

// NativeCore
//
// core methods

// Native functions
//
// These are implemented in the ExecutionUnit and are recognized by the Compiler.
// CallNative op has an operand which is the Native enum value. All params are
// passed on the stack and must be the expected size and type. Return value is
// sent back as an int32_t but can represent any type of value as defined in the
// native function signature
//
// Functions:
//
//      void    print(string)       - prints the passed string to the console
//      string  int8ToString(int8_t)  - return passed int8_t value converted to string

#pragma once

#include "Defines.h"
#include "Interpreter.h"

#include <stdint.h>

namespace clvr {

class InterpreterBase;

class NativeCore
{
  public:
    enum class Id {
        Monitor         = 0,
        PrintF          = 1,
        Format          = 2,
        MemSet          = 3,
        RandomInt       = 4,
        MinInt          = 5,
        MaxInt          = 6,
        InitArgs        = 7,
        ArgInt8         = 8,
        ArgInt16        = 9,
        ArgInt32        = 10,
        ArgFloat        = 11,
        UserCall        = 12,
    };
    
    static const FunctionList create();
    
    static void callNative(uint16_t id, InterpreterBase*, void*);
};

}
