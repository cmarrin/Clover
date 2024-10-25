/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "Flash.h"

#include "Defines.h"

#include <Adafruit_NeoPixel.h>

bool
Flash::init(Adafruit_NeoPixel* pixels, uint8_t h, uint8_t s, uint8_t v, uint8_t count, uint16_t duration)
{
	_countCompleted = 0;
	
    _color = Adafruit_NeoPixel::gamma32(Adafruit_NeoPixel::ColorHSV(uint16_t(h) * 256, s, v));
    _count = count;
    _duration = uint16_t(duration) * 100;
    _lastFlash = millis();

    // If we will be flashing (count != 0) then start with the lights off.
    // Otherwise set the lights to the passed color
    setAllLights(pixels, _count ? 0 : _color);

	return true;
}
	
int32_t
Flash::loop(Adafruit_NeoPixel* pixels)
{
	if (_countCompleted >= _count) {
        // If count == 0 we leave the lights on forever
		return _count ? -1 : 0;
	}
	
	uint32_t t = millis();
	bool showColor = false;
	uint32_t color = 0;
	
	if (_on) {
		if (t > _lastFlash + _duration) {
			color = 0;
			showColor = true;
			_lastFlash = t;
			_on = false;
			_countCompleted++;
		}
	} else {
		if (t > _lastFlash + _duration) {
			color = _color;
			showColor = true;
			_lastFlash = t;
			_on = true;
		}
	}
	
	if (showColor) {
		setAllLights(pixels, color);
	}
	
    return 0;
}

void
Flash::setAllLights(Adafruit_NeoPixel* pixels, uint32_t color)
{
    for (uint32_t i = 0; i < pixels->numPixels(); i++) {
        pixels->setPixelColor(i, color);
        pixels->show();
    }
}
