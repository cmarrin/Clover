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

#include "Memory.h"

namespace fmt {

// Formatter - Formatted printer

class FormatterArgs
{
  public:
    virtual ~FormatterArgs() { }
    
    virtual uint8_t getChar(uint32_t i) const = 0;
    virtual uint8_t getStringChar(uintptr_t p) const = 0;
    virtual uintptr_t getArg(lucid::Type type) = 0;
};

class Formatter {
public:
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

    static bool toNumber(FormatterArgs*f, uint32_t& fmt, uint32_t& n);

    static int32_t printf(lucid::AddrNativeType fmt, lucid::VarArg&);    
    static int32_t printf(const char* fmt, ...);
    static int32_t vprintf(const char* fmt, va_list args);
    
private:
    static int32_t doprintf(FormatterArgs* f);
    
    template<typename T> static void emitSign(T& v)
    {
        if (v < 0) {
            lucid::putChar('-');
            v = -v;
        }
    }
};
    
}
