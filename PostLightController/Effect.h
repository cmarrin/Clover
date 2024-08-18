/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

// Effect Base Class
//
// This class is the base class for all effects

#pragma once

#include <stdint.h>

class Adafruit_NeoPixel;

class Effect
{
public:
	Effect(Adafruit_NeoPixel* pixels) : _pixels(pixels) { }
	
	virtual ~Effect() { }

	virtual bool init(uint8_t cmd, const uint8_t* buf = nullptr, uint32_t size = 0)
	{
		(void) cmd;
		_buf = buf;
		_size = size;
		return true;
	}
	
	// Return delay in ms
	virtual int32_t loop() = 0;
		
protected:
	Adafruit_NeoPixel* _pixels = nullptr;
	uint32_t _size = 0;
	
private:
};
