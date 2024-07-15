/*-------------------------------------------------------------------------
    This source file is a part of Lucid

    For the latest info, see http:www.marrin.org/

    Copyright (c) 2018-2024, Chris Marrin
    All rights reserved.

    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#pragma once

#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>

namespace fmt {
    
    // Formatter - Formatted printer

    class Formatter {
    public:
        class Generator
        {
          public:
            Generator(char* buf, uint16_t size) : _buf(buf), _size(size) { }
            
            bool append(char c)
            {
                if (_index >= _size - 1) {
                    return false;
                }
                _buf[_index++] = c;
                return true;
            }
            
          private:
            char* _buf = nullptr;
            uint16_t _size = 0;
            uint16_t _index = 0;
        };

        enum class Flag {
            leftJustify = 0x01,
            plus = 0x02,
            space = 0x04,
            alt = 0x08,
            zeroPad = 0x10,
        };

        static bool isFlag(uint8_t flags, Flag flag) { return (flags & static_cast<uint8_t>(flag)) != 0; }
        static void setFlag(uint8_t& flags, Flag flag) { flags |= static_cast<uint8_t>(flag); }

        enum class Capital { Yes, No };
        
        static constexpr uint32_t MaxIntegerBufferSize = 24; // Big enough for a 64 bit integer in octal

        static int32_t format(Generator& gen, const char* fmt, ...)
        {
            va_list va;
            va_start(va, fmt);
            int32_t result = vformat(gen, fmt, va);
            va_end(va);
            return result;
        }
        
        static int32_t vformat(Generator&, const char *format, va_list);

        static uint32_t toString(char* buf, uint16_t size, float v)
        {
            Generator g(buf, size);
            return format(g, "%f", v);
        }
        
        static uint32_t toString(char* buf, uint16_t size, int32_t v)
        {
            Generator g(buf, size);
            return format(g, "%i", v);
        }
        
        static uint32_t toString(char* buf, uint16_t size, uint32_t v)
        {
            Generator g(buf, size);
            return format(g, "%u", v);
        }
        
        static uint32_t toString(char* buf, uint16_t size, int16_t v)
        {
            Generator g(buf, size);
            return format(g, "%hi", v);
        }
        
        static uint32_t toString(char* buf, uint16_t size, uint16_t v)
        {
            Generator g(buf, size);
            return format(g, "%hu", v);
        }
        
        static uint32_t toString(char* buf, uint16_t size, int8_t v)
        {
            Generator g(buf, size);
            return format(g, "%hhi", v);
        }
        
        static uint32_t toString(char* buf, uint16_t size, uint8_t v)
        {
            Generator g(buf, size);
            return format(g, "%hhu", v);
        }
        
        static bool toNumber(const char*& s, uint32_t& n);
        
    private:
        template<typename T> static void emitSign(Generator& gen, T& v)
        {
            if (v < 0) {
                gen.append('-');
                v = -v;
            }
        }
    };
    
}
