/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#pragma once

#include "AST.h"

#include <vector>

namespace clvr {

static inline void appendValue(std::vector<uint8_t>& container, uint32_t v, uint8_t bytes)
{
    switch (bytes) {
        case 4: container.push_back(v >> 24);
                container.push_back(v >> 16);
        case 2: container.push_back(v >> 8);
        case 1: container.push_back(v);
    }
}

static inline void setValue(std::vector<uint8_t>& container, AddrNativeType addr, uint32_t v, uint8_t bytes)
{
    switch (bytes) {
        case 4: container[addr++] = v >> 24;
                container[addr++] = v >> 16;
        case 2: container[addr++] = v >> 8;
        case 1: container[addr++] = v;
    }
}
    
class CodeGen
{
  public:
    virtual ~CodeGen() { }
    
    virtual uint16_t majorVersion() const = 0;
    virtual uint8_t minorVersion() const = 0;
    
    virtual void emitPreamble(const Compiler*) = 0;
    virtual void handleFunction(const Compiler*, const FunctionPtr&, bool isTopLevel) { }
    
    virtual void emitCode(const ASTPtr& node, bool isLHS) = 0;

    std::vector<uint8_t>& code() { return _code; }
    
    void format(const char* fmt, ...)
    {
        va_list args;
        va_start(args, fmt);
        vformat(_code, fmt, args);
        
        // vformat adds a null terminator. We don't want that
        _code.pop_back();
    }
    
  private:
    std::vector<uint8_t> _code;
};

}
