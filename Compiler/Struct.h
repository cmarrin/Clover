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

    class Struct
    {
    public:
        Struct() { }
        
        Struct(const std::string& name)
            : _name(name)
        { }
        
        const std::vector<Symbol>& locals() const { return _locals; }
        
        const std::string& name() const { return _name; }
        uint8_t size() const { return _localSize; }
        
        bool addLocal(const std::string& name, Type type, bool ptr, uint8_t size)
        {
            // Check for duplicates
            Symbol sym;
            if (findLocal(name, sym)) {
                return false;
            }
            _locals.emplace_back(name, _localSize, type, Symbol::Storage::Local, ptr, size);
            _localSize += size;
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
        std::vector<Symbol> _locals;
        std::vector<Function> _functions;
        uint8_t _localSize = 0;
        uint8_t _size = 0;
    };
    
}
