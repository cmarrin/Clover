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

class Enum
{
public:
    Enum(const std::string& name, Type type)
        : _name(name)
        , _type(type)
    { }
    
    Type type() const { return _type; }
    const std::string& name() const { return _name; }
    
    void addValue(const std::string& name, int32_t value)
    {
        _values.emplace_back(name, value);
    }

    bool findValue(const std::string& s, int32_t& value)
    {
        const auto& it = find_if(_values.begin(), _values.end(),
                [s](std::pair<std::string, int32_t> p) { return p.first == s; });

        if (it != _values.end()) {
            value = it->second;
            return true;
        }
        return false;
    }
    
    int32_t nextValue() const
    {
        if (_values.empty()) {
            return -1;
        }
        return _values.back().second;
    }

private:
    std::string _name;
    std::vector<std::pair<std::string, int32_t>> _values;
    Type _type = Type::None;
};

}
