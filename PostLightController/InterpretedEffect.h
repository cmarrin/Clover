/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

// InterpretedEffect Class
//
// This class runs the Interpreter

#pragma once

#include <Clover.h>

#include <Adafruit_NeoPixel.h>

static constexpr uint32_t StackSize = 1024;

class MyInterpreter : public clvr::Interpreter<StackSize>
{
public:
	MyInterpreter(Adafruit_NeoPixel* pixels)
        : _pixels(pixels)
    { }

    virtual void setLight(uint8_t i, uint8_t h, uint8_t s, uint8_t v) override
    {
        uint32_t adargb = Adafruit_NeoPixel::gamma32(Adafruit_NeoPixel::ColorHSV(uint16_t(h) * 256, s, v));
        _pixels->setPixelColor(i, adargb);
		_pixels->show();
    }

private:
	Adafruit_NeoPixel* _pixels;
};

class InterpretedEffect
{
public:
	InterpretedEffect(Adafruit_NeoPixel* pixels) : _interp(pixels) { }
	
	bool init(uint8_t cmd, const uint8_t* buf, uint32_t size);
	int32_t loop();
	
	MyInterpreter::Error error() const { return _interp.error(); }

    uint8_t* stackBase() { return &(_interp.memMgr()->stack().getAbs(0)); }
private:
	MyInterpreter _interp;
};
