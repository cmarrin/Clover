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

#include <string>

#include "Defines.h"

namespace lucid {

// A symbol is a variable inside a struct or function. They have a name and type.
// They also hae a flag to indicate if this is a pointer to that type. They also
// have the size in bytes of that variable and its offset in bytes. For functions
// this is the byte offset from the start of the frame base pointer. For structs
// it is the byte offset from the start of the struct instance.

class Symbol
{
public:
    enum class Storage { None, Const, Global, Local };
    
    Symbol() { }
    
    Symbol(const std::string& name, Type type, uint8_t size, uint16_t addr, bool ptr = false)
        : _name(name)
        , _type(type)
        , _ptr(ptr)
        , _size(size)
        , _addr(addr)
    { }
    
    const std::string& name() const { return _name; }
    Type type() const { return _type; }
    bool isPointer() const { return _ptr; }
    uint8_t size() const { return _size; }
    uint16_t addr() const { return _addr; }
    
private:
    std::string _name;
    Type _type = Type::None;
    bool _ptr = false;
    uint8_t _size = 0;
    uint16_t _addr = 0;
};

using SymbolPtr = std::shared_ptr<Symbol>;

}
