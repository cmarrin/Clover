/*-------------------------------------------------------------------------
    This source file is a part of Lucid

    For the latest info, see http:www.marrin.org/

    Copyright (c) 2018-2024, Chris Marrin
    All rights reserved.

    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

//#include "bare.h"

#include "Formatter.h"

#include "Defines.h"
#include "Memory.h"

#include <assert.h>
#include <string.h>

using namespace fmt;

bool Formatter::toNumber(lucid::AddrNativeType& s, uint32_t& n)
{
    n = 0;
    bool haveNumber = false;
    while (1) {
        uint8_t c = lucid::rom(s);
        if (c < '0' || c > '9') {
            return haveNumber;
        }
        n = n * 10 + c - '0';
        s += 1;
        haveNumber = true;
    }
}

enum class Signed { Yes, No };
enum class FloatType { Float, Exp, Shortest };
 
static void handleFlags(lucid::AddrNativeType& format, uint8_t& flags)
{
    while (1) {
        switch (lucid::rom(format)) {
        case '-': Formatter::setFlag(flags, Formatter::Flag::leftJustify); break;
        case '+': Formatter::setFlag(flags, Formatter::Flag::plus); break;
        case ' ': Formatter::setFlag(flags, Formatter::Flag::space); break;
        case '#': Formatter::setFlag(flags, Formatter::Flag::alt); break;
        case '0': Formatter::setFlag(flags, Formatter::Flag::zeroPad); break;
        default: return;
        }
        ++format;
    }
}

static int32_t handleWidth(lucid::AddrNativeType& format, lucid::VarArg& va)
{
    if (lucid::rom(format) == '*') {
        ++format;
        return va.arg(lucid::Type::UInt32);
    }
    
    uint32_t n;
    return Formatter::toNumber(format, n) ? static_cast<int32_t>(n) : -1;
}

enum class Length { None, H, HH, L, LL, J, Z, T };

static Length handleLength(lucid::AddrNativeType& format)
{
    Length length = Length::None;
    if (lucid::rom(format) == 'h') {
        ++format;
        if (lucid::rom(format) == 'h') {
            ++format;
            length = Length::HH;
        } else {
            length = Length::H;
        }
    } else if (lucid::rom(format) == 'l') {
        ++format;
        if (lucid::rom(format) == 'l') {
            ++format;
            length = Length::LL;
        } else {
            length = Length::L;
        }
    } else if (lucid::rom(format) == 'j') {
        length = Length::J;
    } else if (lucid::rom(format) == 'z') {
        length = Length::Z;
    } else if (lucid::rom(format) == 't') {
        length = Length::T;
    } else {
        return length;
    }
    return length;
}

// 32 bit values are the max supported. Return int32_t for L, LL, J, Z, and T
// Always return int32_t. Unsigned values less than 32 bits will be sign extended,
// but reinterpret_cast to unsigned values will work corrently
static int32_t getInteger(Length length, lucid::VarArg& va)
{
    switch(length) {
        case Length::H: return int32_t(int16_t(va.arg(lucid::Type::UInt16)));
        case Length::HH: return int32_t(int8_t(va.arg(lucid::Type::UInt8)));
        case Length::L:
        case Length::LL:
        case Length::J:
        case Length::Z:
        case Length::T:
        case Length::None: return va.arg(lucid::Type::UInt32);
    }
    return 0;
}

static char* intToString(uint64_t value, char* buf, size_t size, uint8_t base = 10, Formatter::Capital cap = Formatter::Capital::No)
{
    if (value == 0) {
        buf[0] = '0';
        buf[1] = '\0';
        return buf;
    }
    
    char hexBase = (cap == Formatter::Capital::Yes) ? 'A' : 'a';
    char* p = buf + size;
    *--p = '\0';
    
    while (value) {
        uint8_t digit = value % base;
        *--p = (digit > 9) ? (digit - 10 + hexBase) : (digit + '0');
        value /= base;
    }
    return p;
}

static int32_t outInteger(Generator gen, uintmax_t value, Signed sign, int32_t width, int32_t precision, uint8_t flags, uint8_t base, Formatter::Capital cap)
{
    uint32_t size = 0;
    if (sign == Signed::Yes) {
        intmax_t signedValue = value;
        if (signedValue < 0) {
            value = -signedValue;
            gen('-');
            size = 1;
            width--;
        }
    }
    
    if (Formatter::isFlag(flags, Formatter::Flag::alt) && base != 10) {
        gen('0');
        size++;
        width--;
        if (base == 16) {
            gen((cap == Formatter::Capital::Yes) ? 'X' : 'x');
            size++;
            width--;
        }
    }
    
    char buf[Formatter::MaxIntegerBufferSize];
    char* p = intToString(static_cast<uint64_t>(value), buf, Formatter::MaxIntegerBufferSize, base, cap);
    size += static_cast<uint32_t>(p - buf);

    int32_t pad = static_cast<int32_t>(width) - static_cast<int32_t>(strlen(p));
    
    char padChar = Formatter::isFlag(flags, Formatter::Flag::zeroPad) ? '0' : ' ';
    
    while (pad > 0) {
        gen(padChar);
        size++;
        pad--;
    }
    
    for ( ; *p; ++p) {
        gen(*p);
    }

    return size;
}

static int32_t outString(Generator gen, lucid::AddrNativeType addr, int32_t width, int32_t precision, uint8_t flags)
{
    // FIXME: Handle flags.leftJustify
    // FIXME: Handle width
    // FIXME: Handle precision
    
    // String addr is a ROM addr
    int32_t size = 0;
    while (true) {
        uint8_t c = lucid::rom(addr++);
        if (c == '\0') {
            break;
        }
        gen(c);
        ++size;
    }
    return size;
}

// Unsupported features:
//
//     'n' specifier - returns number of characters written so far
//     'a', 'A' specifiers - prints hex floats
//     'L' length - long double
//     'l' length for 'c' and 's' specifiers - wide characters
//
// 
 
int32_t Formatter::format(Generator gen, lucid::AddrNativeType fmt, lucid::VarArg& va)
{
    uint8_t flags = 0;
        
    int32_t size = 0;
    
    while (lucid::rom(fmt)) {
        if (lucid::rom(fmt) != '%') {
            gen(lucid::rom(fmt++));
            size++;
            continue;
        }
        
        fmt++;
        
        // We have a format, do the optional part
        handleFlags(fmt, flags);
        int32_t width = handleWidth(fmt, va);
        int32_t precision = -1;
        if (lucid::rom(fmt) == '.') {
            precision = handleWidth(++fmt, va);
        }
        Length length = handleLength(fmt);
        
        // Handle the specifier
        switch (lucid::rom(fmt))
        {
        case 'd':
        case 'i':
            size += outInteger(gen, getInteger(length, va), Signed::Yes, width, precision, flags, 10, Formatter::Capital::No);
            break;
        case 'u':
            size += outInteger(gen, getInteger(length, va), Signed::No, width, precision, flags, 10, Formatter::Capital::No);
            break;
        case 'o':
            size += outInteger(gen, getInteger(length, va), Signed::No, width, precision, flags, 8, Formatter::Capital::No);
            break;
        case 'x':
        case 'X':
            size += outInteger(gen, getInteger(length, va), Signed::No, width, precision, flags, 16, (lucid::rom(fmt) == 'X') ? Formatter::Capital::Yes : Formatter::Capital::No);
            break;
        case 'f':
        case 'F':
        case 'e':
        case 'E':
        case 'g':
        case 'G': {
            Formatter::Capital cap = Formatter::Capital::No;
            FloatType type = FloatType::Shortest;
            switch(lucid::rom(fmt))
            {
            case 'f': cap = Formatter::Capital::No; type = FloatType::Float; break;
            case 'F': cap = Formatter::Capital::Yes; type = FloatType::Float; break;
            case 'e': cap = Formatter::Capital::No; type = FloatType::Exp; break;
            case 'E': cap = Formatter::Capital::Yes; type = FloatType::Exp; break;
            case 'g': cap = Formatter::Capital::No; type = FloatType::Shortest; break;
            case 'G': cap = Formatter::Capital::Yes; type = FloatType::Shortest; break;
            }

            char buf[20];
            lucid::toString(buf, lucid::intToFloat(va.arg(lucid::Type::Float)), width, (precision < 0) ? 6 : precision);
            for (int i = 0; buf[i] != '\0'; ++i) {
                gen(buf[i]);
            }
            //size += outFloat(gen, flt::Float::fromArg(va_arg(va.value, flt::Float::arg_type)), width, precision, flags, cap, type);
            break;
        }
        case 'c':
            gen(static_cast<char>(va.arg(lucid::Type::UInt8)));
            size++;
            break;
        case 'b': {
            // Boolean
            const char* s = va.arg(lucid::Type::UInt8) ? "true" : "false";
            for (int i = 0; s[i] != '\0'; ++i) {
                gen(s[i]);
            }
            break;
        }
        case 's':
            size += outString(gen, va.arg(lucid::Type::String), width, precision, flags);
            break;
        case 'p':
            size += outInteger(gen, va.arg(lucid::Type::UInt32), Signed::No, width, precision, flags, 16, Formatter::Capital::No);
            break;
        default:
            gen(lucid::rom(fmt++));
            size++;
            break;
        }
        ++fmt;
    }
    
    return size;
}
