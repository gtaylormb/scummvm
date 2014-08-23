/* ScummVM - Graphic Adventure Engine
 *
 * ScummVM is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

#include "common/algorithm.h"
#include "common/endian.h"
#include "common/rect.h"
#include "common/textconsole.h"
#include "common/system.h"
#include "graphics/palette.h"
#include "access/access.h"
#include "access/screen.h"
#include "access/resources.h"

namespace Access {

#define VGA_COLOR_TRANS(x) ((x) * 255 / 63)

Screen::Screen(AccessEngine *vm) : _vm(vm) {
	create(320, 200);
	Common::fill(&_tempPalette[0], &_tempPalette[PALETTE_SIZE], 0);
	Common::fill(&_manPal[0], &_manPal[0x60], 0);
	Common::fill(&_scaleTable1[0], &_scaleTable1[256], 0);
	Common::fill(&_scaleTable2[0], &_scaleTable2[256], 0);
	_savedPaletteCount = 0;
	_vesaMode = 0;
	_vesaCurrentWin = 0;
	_currentPanel = 0;
	_hideFlag = true;
	_loadPalFlag = false;
	_startColor = _numColors = 0;
	_scrollCol = _scrollRow = 0;
	_windowXAdd = _windowYAdd = 0;
	_screenYOff = 0;
	_screenChangeFlag = false;

	_bufferBytesWide = _vWindowBytesWide = this->w;
	_vWindowLinesTall = this->h;
	_clipWidth = _vWindowBytesWide - 1;
	_clipHeight = _vWindowLinesTall - 1;
}

void Screen::setDisplayScan() {
	_clipWidth = this->w - 1;
	_clipHeight = this->h - 1;
	_windowXAdd = _windowYAdd = 0;
	_scrollX = _scrollY = 0;
	_scrollCol = _scrollRow = 0;
	_bufferStart.x = _bufferStart.y = 0;
	_screenYOff = 0;
}

void Screen::setPanel(int num) {
	assert(num < 4);
	_currentPanel = num;
	_msVirtualOffset = _virtualOffsetsTable[num];
}

void Screen::updateScreen() {
	g_system->copyRectToScreen((byte *)getPixels(), this->pitch, 0, 0,
		this->w, this->h);
	g_system->updateScreen();
}

void Screen::setInitialPalettte() {
	Common::copy(&INITIAL_PALETTE[0], &INITIAL_PALETTE[18 * 3], _rawPalette);
	Common::fill(&_rawPalette[18 * 3], &_rawPalette[PALETTE_SIZE], 0);

	g_system->getPaletteManager()->setPalette(INITIAL_PALETTE, 0, 18);
}

void Screen::loadPalette(Common::SeekableReadStream *stream) {
	loadRawPalette(stream);
	setPalette();
	_loadPalFlag = true;
}

void Screen::loadPalette(int fileNum, int subfile) {
	byte *palette = _vm->_files->loadFile(fileNum, subfile);
	Common::copy(palette, palette + (_numColors * 3), &_rawPalette[_startColor * 3]);
	delete[] palette;
}

void Screen::setPalette() {
	g_system->getPaletteManager()->setPalette(&_rawPalette[0], 0, PALETTE_COUNT);
}

void Screen::loadRawPalette(Common::SeekableReadStream *stream) {
	stream->read(&_rawPalette[0], PALETTE_SIZE);
	for (byte *p = &_rawPalette[0]; p < &_rawPalette[PALETTE_SIZE]; ++p)
		*p = VGA_COLOR_TRANS(*p);
}

void Screen::updatePalette() {
	g_system->getPaletteManager()->setPalette(&_tempPalette[0], 0, PALETTE_COUNT);
	updateScreen();
}

void Screen::savePalette() {
	Common::copy(&_rawPalette[0], &_rawPalette[PALETTE_SIZE],
		&_savedPalettes[_savedPaletteCount][0]);

	if (++_savedPaletteCount == 2)
		_savedPaletteCount = 1;
}

void Screen::restorePalette() {
	if (--_savedPaletteCount < 0)
		_savedPaletteCount = 0;

	Common::copy(&_savedPalettes[_savedPaletteCount][0],
		&_savedPalettes[_savedPaletteCount][PALETTE_SIZE], &_rawPalette[0]);
}


void Screen::forceFadeOut() {
	const int FADE_AMOUNT = 2;
	bool repeatFlag;
	byte *srcP;
	int count;

	do {
		repeatFlag = false;
		for (srcP = &_tempPalette[0], count = 0; count < PALETTE_SIZE; ++count, ++srcP) {
			int v = *srcP;
			if (v) {
				repeatFlag = true;
				*srcP = MAX(*srcP - FADE_AMOUNT, 0);
			}
		}

		updatePalette();
		g_system->delayMillis(10);
	} while (repeatFlag && !_vm->shouldQuit());
}

void Screen::forceFadeIn() {
	Common::fill(&_tempPalette[0], &_tempPalette[PALETTE_SIZE], 0);

	const int FADE_AMOUNT = 2;
	bool repeatFlag;
	do {
		repeatFlag = false;
		const byte *srcP = &_rawPalette[0];
		byte *destP = &_tempPalette[0];

		for (int idx = 0; idx < PALETTE_SIZE; ++idx, ++srcP, ++destP) {
			if (*destP != *srcP) {
				repeatFlag = true;
				*destP = MAX((int)*destP + FADE_AMOUNT, (int)*srcP);
			}
		}

		updatePalette();
		g_system->delayMillis(10);
	} while (repeatFlag);
}

void Screen::copyBuffer(const byte *data) {
	byte *destP = (byte *)getPixels();
	Common::copy(data, data + (h * w), destP);
	g_system->copyRectToScreen(destP, w, 0, 0, w, h);
}

void Screen::setBufferScan() {
	_clipWidth = _vWindowBytesWide - 1;
	_windowXAdd = (320 - _clipWidth) >> 1;
	_clipHeight = _vWindowLinesTall - 1;
	_windowYAdd = (176 - _clipHeight) >> 1;
}

void Screen::setScaleTable(int scale) {
	int total = 0;
	for (int idx = 0; idx < 256; ++idx) {
		_scaleTable1[idx] = total >> 8;
		_scaleTable2[idx] = total & 0xff;
		total += scale;
	}
}

void Screen::saveScreen() {
	_screenSave._clipWidth = _clipWidth;
	_screenSave._clipHeight = _clipHeight;
	_screenSave._windowXAdd = _windowXAdd;
	_screenSave._windowYAdd = _windowYAdd;
	_screenSave._scroll.x = _scrollX;
	_screenSave._scroll.y = _scrollY;
	_screenSave._scrollCol = _scrollCol;
	_screenSave._scrollRow = _scrollRow;
	_screenSave._bufferStart.x = _bufferStart.x;
	_screenSave._bufferStart.y = _bufferStart.y;
	_screenSave._screenYOff = _screenYOff;
}

void Screen::restoreScreen() {
	_clipWidth = _screenSave._clipWidth;
	_clipHeight = _screenSave._clipHeight;
	_windowXAdd = _screenSave._windowXAdd;
	_windowYAdd = _screenSave._windowYAdd;
	_scrollX = _screenSave._scroll.x;
	_scrollY = _screenSave._scroll.y;
	_scrollCol = _screenSave._scrollCol;
	_scrollRow = _screenSave._scrollRow;
	_bufferStart.x = _screenSave._bufferStart.x;
	_bufferStart.y = _screenSave._bufferStart.y;
	_screenYOff = _screenSave._screenYOff;
}

void Screen::copyBlock(ASurface *src, const Common::Rect &bounds) {
	Common::Rect destBounds = bounds;
	destBounds.translate(_windowXAdd, _windowYAdd + _screenYOff);

	copyRectToSurface(*src, destBounds.left, destBounds.top, bounds);
}

} // End of namespace Access
