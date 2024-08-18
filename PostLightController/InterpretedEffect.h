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

#include <Lucid.h>

#include "Effect.h"
#include "NativeColor.h"

#include <Adafruit_NeoPixel.h>

static constexpr uint32_t StackSize = 1024;

class MyInterpreter : public lucid::Interpreter<StackSize>
{
public:
	MyInterpreter(Adafruit_NeoPixel* pixels)
        : _pixels(pixels)
    { }

    virtual void setLight(uint8_t i, uint32_t rgb) override
    {
        _pixels->setPixelColor(i, rgb);
		_pixels->show();
    }

private:
	Adafruit_NeoPixel* _pixels;
};

class InterpretedEffect : public Effect
{
public:
	InterpretedEffect(Adafruit_NeoPixel* pixels);
	virtual ~InterpretedEffect() {}
	
	virtual bool init(uint8_t cmd, const uint8_t* buf, uint32_t size) override;
	virtual int32_t loop() override;
	
	MyInterpreter::Error error() const { return _interp.error(); }
				
private:
	MyInterpreter _interp;
};
