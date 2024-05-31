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
        const std::vector<uint32_t>& structIndexes() const { return _structIndexes; }
        
        const std::string& name() const { return _name; }
        uint8_t size() const { return _localSize; }
        
        bool addStruct(uint32_t index)
        {
            _structIndexes.emplace_back(index);
            return true;
        }

        bool addLocal(const std::string& name, Type type, uint8_t size, bool ptr)
        {
            // Check for duplicates
            uint32_t symbolIndex;
            if (findLocal(name, symbolIndex)) {
                return false;
            }
            _locals.emplace_back(name, type, size, ptr);
            _localSize += size;
            return true;
        }

        bool findLocal(const std::string& s, uint32_t& symbolIndex)
        {
            const auto& it = find_if(_locals.begin(), _locals.end(),
                    [s](const Symbol& p) { return p.name() == s; });

            if (it != _locals.end()) {
                symbolIndex = uint32_t(it - _locals.begin());
                return true;
            }
            return false;
        }

    private:
        std::string _name;
        std::vector<uint32_t> _structIndexes;
        std::vector<Symbol> _locals;
        std::vector<Function> _functions;
        uint8_t _localSize = 0;
        uint8_t _size = 0;
    };
    
}
