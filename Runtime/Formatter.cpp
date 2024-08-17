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

class InterpFormatterArgs : public FormatterArgs
{
  public:
    InterpFormatterArgs(lucid::AddrNativeType fmt, lucid::VarArg& args)
        : _fmt(fmt)
        , _args(&args)
    { }
        
    virtual ~InterpFormatterArgs() { }
    virtual uint8_t getChar(uint32_t i) const override { return lucid::rom(_fmt + i); }
    virtual uintptr_t getArg(lucid::Type type) override { return _args->arg(type); }

    // The interpreter keeps strings in ROM. The p pointer is actually an offset in the rom
    virtual uint8_t getStringChar(uintptr_t p) const override
    {
        lucid::AddrNativeType addr = lucid::AddrNativeType(p);
        return lucid::rom(addr);
    }

  private:
    lucid::AddrNativeType _fmt;
    lucid::VarArg* _args;
};

class NativeFormatterArgs : public FormatterArgs
{
  public:
    NativeFormatterArgs(const char* fmt, va_list args)
        : _fmt(fmt)
    {
        va_copy(_args, args);
    }
        
    virtual ~NativeFormatterArgs() { }
    virtual uint8_t getChar(uint32_t i) const override { return _fmt[i]; }
    virtual uintptr_t getArg(lucid::Type type) override
    {
        if (type == lucid::Type::Float) {
            double d = va_arg(_args, double);
            return lucid::floatToInt(float(d));
        }
        if (type == lucid::Type::String) {
            return uintptr_t(va_arg(_args, const char*));
        }
        return va_arg(_args, uint32_t);
    }

    // For native, p is the actual pointer to the char
    virtual uint8_t getStringChar(uintptr_t p) const override
    {
        const char* addr = reinterpret_cast<const char*>(p);
        return *addr;
    }

  private:
    const char* _fmt;
    va_list _args;
};

int32_t
Formatter::printf(lucid::AddrNativeType fmt, lucid::VarArg& args)
{
    InterpFormatterArgs f(fmt, args);
    return Formatter::doprintf(&f);
}

int32_t
Formatter::printf(const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    return vprintf(fmt, args);
}

int32_t
Formatter::vprintf(const char* fmt, va_list args)
{
    NativeFormatterArgs f(fmt, args);
    return Formatter::doprintf(&f);
}

bool Formatter::toNumber(FormatterArgs*f, uint32_t& fmt, uint32_t& n)
{
    n = 0;
    bool haveNumber = false;
    while (1) {
        uint8_t c = f->getChar(fmt);
        if (c < '0' || c > '9') {
            return haveNumber;
        }
        n = n * 10 + c - '0';
        fmt += 1;
        haveNumber = true;
    }
}

enum class Signed { Yes, No };
enum class FloatType { Float, Exp, Shortest };
 
static void handleFlags(FormatterArgs*f, uint32_t& fmt, uint8_t& flags)
{
    while (1) {
        switch (f->getChar(fmt)) {
        case '-': Formatter::setFlag(flags, Formatter::Flag::leftJustify); break;
        case '+': Formatter::setFlag(flags, Formatter::Flag::plus); break;
        case ' ': Formatter::setFlag(flags, Formatter::Flag::space); break;
        case '#': Formatter::setFlag(flags, Formatter::Flag::alt); break;
        case '0': Formatter::setFlag(flags, Formatter::Flag::zeroPad); break;
        default: return;
        }
        ++fmt;
    }
}

static int32_t handleWidth(FormatterArgs*f, uint32_t& fmt)
{
    if (f->getChar(fmt) == '*') {
        ++fmt;
        return int32_t(f->getArg(lucid::Type::UInt32));
    }
    
    uint32_t n;
    return Formatter::toNumber(f, fmt, n) ? static_cast<int32_t>(n) : -1;
}

enum class Length { None, H, HH, L, LL, J, Z, T };

static Length handleLength(FormatterArgs*f, uint32_t& fmt)
{
    Length length = Length::None;
    if (f->getChar(fmt) == 'h') {
        ++fmt;
        if (f->getChar(fmt) == 'h') {
            ++fmt;
            length = Length::HH;
        } else {
            length = Length::H;
        }
    } else if (f->getChar(fmt) == 'l') {
        ++fmt;
        if (f->getChar(fmt) == 'l') {
            ++fmt;
            length = Length::LL;
        } else {
            length = Length::L;
        }
    } else if (f->getChar(fmt) == 'j') {
        length = Length::J;
    } else if (f->getChar(fmt) == 'z') {
        length = Length::Z;
    } else if (f->getChar(fmt) == 't') {
        length = Length::T;
    } else {
        return length;
    }
    return length;
}

// 8 and 16 bit integers are upcast by the caller to 32 bit. Ignore the length field
static int32_t getInteger(Length length, FormatterArgs* f)
{
    return int32_t(f->getArg(lucid::Type::UInt32));
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

static int32_t outInteger(uintmax_t value, Signed sign, int32_t width, int32_t precision, uint8_t flags, uint8_t base, Formatter::Capital cap)
{
    uint32_t size = 0;
    if (sign == Signed::Yes) {
        intmax_t signedValue = value;
        if (signedValue < 0) {
            value = -signedValue;
            lucid::putChar('-');
            size = 1;
            width--;
        }
    }
    
    if (Formatter::isFlag(flags, Formatter::Flag::alt) && base != 10) {
        lucid::putChar('0');
        size++;
        width--;
        if (base == 16) {
            lucid::putChar((cap == Formatter::Capital::Yes) ? 'X' : 'x');
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
        lucid::putChar(padChar);
        size++;
        pad--;
    }
    
    for ( ; *p; ++p) {
        lucid::putChar(*p);
    }

    return size;
}

static int32_t outString(FormatterArgs* f, uintptr_t p, int32_t width, int32_t precision, uint8_t flags)
{
    // FIXME: Handle flags.leftJustify
    // FIXME: Handle width
    // FIXME: Handle precision
    
    int32_t size = 0;
    while (true) {
        uint8_t c = f->getStringChar(p++);
        if (c == '\0') {
            break;
        }
        lucid::putChar(c);
        ++size;
    }
    
    if (width > size) {
        width -= size;
        while (width--) {
            lucid::putChar(' ');
        }
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
 
int32_t Formatter::doprintf(FormatterArgs* f)
{
    uint8_t flags = 0;
    int32_t size = 0;
    uint32_t fmt = 0;
    
    while (f->getChar(fmt)) {
        if (f->getChar(fmt) != '%') {
            lucid::putChar(f->getChar(fmt++));
            size++;
            continue;
        }
        
        fmt++;
        
        // We have a format, do the optional part
        handleFlags(f, fmt, flags);
        int32_t width = handleWidth(f, fmt);
        int32_t precision = -1;
        if (f->getChar(fmt) == '.') {
            precision = handleWidth(f, ++fmt);
        }
        Length length = handleLength(f, fmt);
        
        // Handle the specifier
        switch (f->getChar(fmt))
        {
        case 'd':
        case 'i':
            size += outInteger(getInteger(length, f), Signed::Yes, width, precision, flags, 10, Formatter::Capital::No);
            break;
        case 'u':
            size += outInteger(getInteger(length, f), Signed::No, width, precision, flags, 10, Formatter::Capital::No);
            break;
        case 'o':
            size += outInteger(getInteger(length, f), Signed::No, width, precision, flags, 8, Formatter::Capital::No);
            break;
        case 'x':
        case 'X':
            size += outInteger(getInteger(length, f), Signed::No, width, precision, flags, 16, (f->getChar(fmt) == 'X') ? Formatter::Capital::Yes : Formatter::Capital::No);
            break;
        case 'f':
        case 'F':
        case 'e':
        case 'E':
        case 'g':
        case 'G': {
            Formatter::Capital cap = Formatter::Capital::No;
            FloatType type = FloatType::Shortest;
            switch(f->getChar(fmt))
            {
            case 'f': cap = Formatter::Capital::No; type = FloatType::Float; break;
            case 'F': cap = Formatter::Capital::Yes; type = FloatType::Float; break;
            case 'e': cap = Formatter::Capital::No; type = FloatType::Exp; break;
            case 'E': cap = Formatter::Capital::Yes; type = FloatType::Exp; break;
            case 'g': cap = Formatter::Capital::No; type = FloatType::Shortest; break;
            case 'G': cap = Formatter::Capital::Yes; type = FloatType::Shortest; break;
            }

            char buf[20];
            lucid::toString(buf, lucid::intToFloat(int32_t(f->getArg(lucid::Type::Float))), width, (precision < 0) ? 6 : precision);
            for (int i = 0; buf[i] != '\0'; ++i) {
                lucid::putChar(buf[i]);
            }
            //size += outFloat(gen, flt::Float::fromArg(va_arg(va.value, flt::Float::arg_type)), width, precision, flags, cap, type);
            break;
        }
        case 'c':
            // Chars are passed in as uint32
            lucid::putChar(static_cast<char>(f->getArg(lucid::Type::UInt32)));
            size++;
            break;
        case 'b': {
            // Booleans are passed in as uint32
            const char* s = f->getArg(lucid::Type::UInt32) ? "true" : "false";
            for (int i = 0; s[i] != '\0'; ++i) {
                lucid::putChar(s[i]);
            }
            break;
        }
        case 's':
            size += outString(f, f->getArg(lucid::Type::String), width, precision, flags);
            break;
        case 'p':
            size += outInteger(f->getArg(lucid::Type::UInt32), Signed::No, width, precision, flags, 16, Formatter::Capital::No);
            break;
        default:
            lucid::putChar(f->getChar(fmt++));
            size++;
            break;
        }
        ++fmt;
    }
    
    return size;
}
