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
#include <functional>

#include "Memory.h"

namespace fmt {

using Generator = std::function<void(uint8_t)>;

    // Formatter - Formatted printer

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

        static int32_t format(Generator gen, lucid::AddrNativeType fmt, lucid::VarArg&);

        static bool toNumber(lucid::AddrNativeType& s, uint32_t& n);
        
    private:
        template<typename T> static void emitSign(Generator gen, T& v)
        {
            if (v < 0) {
                gen('-');
                v = -v;
            }
        }
    };
    
}
