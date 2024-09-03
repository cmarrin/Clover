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

class Function;

class Symbol
{
public:    
    Symbol() { }
    
    Symbol(const FunctionPtr& func) : _name(func->name()), _type(Type::Function), _function(func) { }
    
    // Symbol can be:
    //
    //      scalar              - size is derived from typeToBytes().
    //      struct              - size comes from struct definition
    //      array               - size is typeToBytes of underlying type * nElements
    //      pointer to scalar   - size is AddrSize
    //      pointer to struct   - size is AddrSize
    //
    // Pointer to array is not allowed. When an array is passed as an arg, you always pass
    // a pointer to it. So the arg type is a pointer to the underlying type.

    enum class Kind { Var, Constant, ScalarConstant };
    Symbol(const std::string& name, Type type, bool ptr, uint16_t size, uint16_t nElements, Kind kind = Kind::Var)
        : _name(name)
        , _type(type)
        , _ptr(ptr)
        , _structSize(size)
        , _nElements(nElements)
        , _kind(kind)
    { }
    
    void setAddr(uint32_t addr, Index index) { _addr = addr; _index = index; }
    AddrNativeType addr(Index& index) const { index = _index; return _addr; }
    
    void setNElements(uint16_t n) { _nElements = n; }
    uint16_t nElements() const { return _nElements; }
    
    const std::string& name() const { return _name; }
    Type type() const { return _type; }
    bool isPointer() const { return _ptr; }
    Kind kind() const { return _kind; }
    void setKind(Kind kind) { _kind = kind; }
    FunctionPtr function() const { return _function; }
    uint16_t sizeInBytes() const
    {
        if (_ptr) {
            return AddrSize;
        }
        return elementSizeInBytes() * _nElements;
    }
    
    uint16_t elementSizeInBytes() const
    {
        if (_ptr) {
            return AddrSize;
        }
        
        if (isStruct(_type)) {
            return _structSize;
        }
        
        return typeToBytes(_type);
    }
    
private:
    std::string _name;
    Type _type = Type::None;
    bool _ptr = false;
    AddrNativeType _addr = 0;
    Index _index;
    FunctionPtr _function;
    uint16_t _nElements = 1;
    uint16_t _structSize = 0;
    Kind _kind = Kind::Var;
};

}
