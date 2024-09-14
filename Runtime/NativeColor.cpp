/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "NativeColor.h"

#include "Interpreter.h"

#include <math.h>

#ifdef ARDUINO
#include <Adafruit_NeoPixel.h>
static uint32_t gamma(uint32_t rgb) { return Adafruit_NeoPixel::gamma32(rgb); }
#else
#if RUNTIME == 1
static uint32_t gamma(uint32_t rgb) { return rgb; }
#endif
#endif

using namespace clvr;

#if COMPILE == 1
ModulePtr
NativeColor::createModule()
{
    ModulePtr module = std::make_shared<NativeColor>();
    module->addNativeFunction("loadColorArg", uint16_t(Id::LoadColorArg), Type::None, {{ "c", Type::None, true, 1, 1 }});
    module->addNativeFunction("setLight", uint16_t(Id::SetLight), Type::None, {{ "i", Type::UInt8, false, 1, 1 }, { "c", Type::None, true, 1, 1 }});
    return module;
}
#endif
#if RUNTIME == 1
void
NativeColor::callNative(uint16_t id, InterpreterBase* interp)
{
    switch (Id(id)) {
        case Id::LoadColorArg: {
            // Arg is a pointer to a Color struct, which holds h, s and v floats.
            // Next incoming args are uint8 h, s, v values. Convert to 0-1 floats
            // and store in struct
            AddrNativeType addr = interp->memMgr()->getArg(0, AddrType);
            float fh = float(interp->topLevelArgs()->arg(Type::Int8)) / 255;
            float fs = float(interp->topLevelArgs()->arg(Type::Int8)) / 255;
            float fv = float(interp->topLevelArgs()->arg(Type::Int8)) / 255;
            interp->memMgr()->setAbs(addr, floatToInt(fh), OpSize::flt);
            interp->memMgr()->setAbs(addr + 4, floatToInt(fs), OpSize::flt);
            interp->memMgr()->setAbs(addr + 8, floatToInt(fv), OpSize::flt);
            break;
        }
        case Id::SetLight: {
            uint8_t i = interp->memMgr()->getArg(0, Type::UInt8);
            AddrNativeType addr = interp->memMgr()->getArg(1, AddrType);
            float fh = intToFloat(interp->memMgr()->getAbs(addr, OpSize::flt));
            float fs = intToFloat(interp->memMgr()->getAbs(addr + 4, OpSize::flt));
            float fv = intToFloat(interp->memMgr()->getAbs(addr + 8, OpSize::flt));
            
            interp->setLight(i, hsvToRGB(fh, fs, fv));
            break;
        }
    }
}

uint32_t
NativeColor::hsvToRGB(float h, float s, float v)
{
    uint16_t hue = fmin(fmax(h * 65535.0f, 0.0f), 65535.0f);
    uint8_t sat = fmin(fmax(s * 255.0f, 0.0f), 255.0f);
    uint8_t val = fmin(fmax(v * 255.0f, 0.0f), 255.0f);

    uint8_t r, g, b;
    
    hue = (uint32_t(hue) * 1530 + 32768) / 65536;

    if (hue < 510) { // Red to Green-1
        b = 0;
        if (hue < 255) { //   Red to Yellow-1
            r = 255;
            g = hue;       //     g = 0 to 254
        } else {         //   Yellow to Green-1
            r = 510 - hue; //     r = 255 to 1
            g = 255;
        }
    } else if (hue < 1020) { // Green to Blue-1
        r = 0;
        if (hue < 765) { //   Green to Cyan-1
            g = 255;
            b = hue - 510;  //     b = 0 to 254
        } else {          //   Cyan to Blue-1
            g = 1020 - hue; //     g = 255 to 1
            b = 255;
        }
    } else if (hue < 1530) { // Blue to Red-1
        g = 0;
        if (hue < 1275) { //   Blue to Magenta-1
            r = hue - 1020; //     r = 0 to 254
            b = 255;
        } else { //   Magenta to Red-1
            r = 255;
            b = 1530 - hue; //     b = 255 to 1
        }
    } else { // Last 0.5 Red (quicker than % operator)
        r = 255;
        g = b = 0;
    }

    // Apply saturation and value to R,G,B, pack into 32-bit result:
    uint32_t v1 = 1 + val;  // 1 to 256; allows >>8 instead of /255
    uint16_t s1 = 1 + sat;  // 1 to 256; same reason
    uint8_t s2 = 255 - sat; // 255 to 0
    uint32_t rgb = ((((((r * s1) >> 8) + s2) * v1) & 0xff00) << 8) |
            (((((g * s1) >> 8) + s2) * v1) & 0xff00) |
            (((((b * s1) >> 8) + s2) * v1) >> 8);

    return gamma(rgb);
}
#endif
