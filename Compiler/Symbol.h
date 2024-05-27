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

namespace lucid {

    class Symbol
    {
    public:
        enum class Storage { None, Const, Global, Local };
        
        Symbol() { }
        
        Symbol(const std::string& name, uint16_t addr, Type type, Storage storage = Storage::Local, bool ptr = false, uint8_t size = 1)
            : _name(name)
            , _addr(addr)
            , _type(type)
            , _ptr(ptr)
            , _storage(storage)
            , _size(size)
        {
            if (type == Type::Ptr) {
                _ptr = true;
            }
        }
        
        const std::string& name() const { return _name; }
        uint16_t addr() const { return _addr; }
        Type type() const { return _type; }
        bool isPointer() const { return _ptr; }
        Storage storage() const { return _storage; }
        uint8_t size() const { return _size; }
        
    private:
        std::string _name;
        uint16_t _addr = 0;
        Type _type = Type::None;
        bool _ptr = false;
        Storage _storage = Storage::None;
        uint8_t _size = 0;
    };
    
    using SymbolList = std::vector<Symbol>;

}
