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

#include "Compiler.h"
#include "ExecutionUnit.h"
#include "Scanner.h"
#include <cstdint>
#include <istream>
#include <vector>

namespace lucid {

class CompileEngine
{
public:
    CompileEngine(std::istream* stream, AnnotationList* annotations);
    
protected:
    enum class Reserved {
        None,
        Struct,
        Const,
        Import,
        From,
        Function,
        Initialize,
        Return,
        Break,
        Continue,
        End,
        Loop,
        While,
        For,
        If,
        Else,
        Float,
        Fixed,
        Int8,
        UInt8,
        Int16,
        UInt16,
        Int32,
        UInt32,
    };
    
    
    // Vars are defined in 2 places. At global scope (when there are
    // no active functions) they are placed in _global memory.
    // When a function is being defined the vars are placed on the
    // stack.
    
    uint16_t _globalSize = 0;
    bool _inFunction = false;
};

}
