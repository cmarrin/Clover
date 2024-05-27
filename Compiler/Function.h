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

#include "Defines.h"
#include "Symbol.h"

namespace lucid {

class Function
{
public:
    Function() { }
    
    Function(const std::string& name, uint16_t addr, Type type = Type::None)
        : _name(name)
        , _addr(addr)
        , _type(type)
        , _native(false)
    { }

    // Used to create built-in native functions
    Function(const char* name, uint8_t nativeId, Type type, const SymbolList& locals)
        : _name(name)
        , _addr(int16_t(nativeId))
        , _locals(locals)
        , _args(locals.size())
        , _type(type)
        , _native(true)
    { }

    const std::string& name() const { return _name; }
    int16_t addr() const { return _addr; }
    uint8_t& args() { return _args; }
    const uint8_t& args() const { return _args; }
    Type type() const { return _type; }
    uint8_t localSize() const { return _localHighWaterMark; }
    const Symbol& local(uint8_t i) const { return _locals[i]; }

    uint32_t numLocals() const { return uint32_t(_locals.size()); }
    void pruneLocals(uint32_t n)
    {
        // remove the last n locals and reduce _localSize
        while (n-- > 0) {
            _localSize -= _locals.back().size();
            _locals.pop_back();
        }
    }
    
    bool isNative() const { return _native; }
    int16_t nativeId() const { return _addr; }

    // Args are always 1 word and will always come before locals. So the
    // addr is the current locals size.
    void addArg(const std::string& name, Type type, bool isPtr)
    {
        _locals.emplace_back(name, _locals.size(), type, Symbol::Storage::Local, isPtr);
        _args++;
    }
    
    bool addLocal(const std::string& name, Type type, bool ptr, uint8_t size)
    {
        // Check for duplicates
        Symbol sym;
        if (findLocal(name, sym)) {
            return false;
        }
        _locals.emplace_back(name, _localSize + _args, type, Symbol::Storage::Local, ptr, size);
        _localSize += size;
        if (_localHighWaterMark < _localSize) {
            _localHighWaterMark = _localSize;
        }
        return true;
    }

    bool findLocal(const std::string& s, Symbol& sym)
    {
        const auto& it = find_if(_locals.begin(), _locals.end(),
                [s](const Symbol& p) { return p.name() == s; });

        if (it != _locals.end()) {
            sym = *it;
            return true;
        }
        return false;
    }
    
private:
    std::string _name;
    int16_t _addr = 0;
    std::vector<Symbol> _locals;
    uint8_t _args = 0;
    Type _type;
    bool _native = false;
    uint8_t _localSize = 0;
    uint8_t _localHighWaterMark = 0;
};

}
