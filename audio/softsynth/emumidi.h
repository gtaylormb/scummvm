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
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

#ifndef AUDIO_SOFTSYNTH_EMUMIDI_H
#define AUDIO_SOFTSYNTH_EMUMIDI_H

#include "common/system.h"
#include "audio/audiostream.h"
#include "audio/mididrv.h"
#include "audio/mixer.h"

class MidiDriver_Emulated : public Audio::AudioStream, public MidiDriver {
protected:
	bool _isOpen;
	Audio::Mixer *_mixer;
	Audio::SoundHandle _mixerSoundHandle;

private:
	Common::TimerManager::TimerProc _timerProc;
	void *_timerParam;

	static void timer(void *c) {
		MidiDriver_Emulated *drv = static_cast<MidiDriver_Emulated *>(c);
		if (drv->_timerProc)
			(*drv->_timerProc)(drv->_timerParam);
		drv->onTimer();
	}

	void installTimer() {
		g_system->getTimerManager()->installTimerProc(timer, 1000000 / _baseFreq, this, "EMUMIDI");		
	}

	void removeTimer() {
		g_system->getTimerManager()->removeTimerProc(timer);
	}
protected:
	int _baseFreq;

	virtual void generateSamples(int16 *buf, int len) = 0;
	virtual void onTimer() {}

public:
	MidiDriver_Emulated(Audio::Mixer *mixer) :
		_mixer(mixer),
		_isOpen(false),
		_timerProc(0),
		_timerParam(0),
		_baseFreq(100) {
	}

	~MidiDriver_Emulated() {
		if (_isOpen)
			close();
	}

	// MidiDriver API
	virtual int open() {
		installTimer();
		_isOpen = true;

		return 0;
	}

	virtual void close() {
		_isOpen = false;
		removeTimer();
	}

	bool isOpen() const { return _isOpen; }

	virtual void setTimerCallback(void *timer_param, Common::TimerManager::TimerProc timer_proc) {
		if (_isOpen)
			removeTimer();
		_timerProc = timer_proc;
		_timerParam = timer_param;
		if (_isOpen)
			installTimer();
	}

	virtual uint32 getBaseTempo() {
		return 1000000 / _baseFreq;
	}

	// AudioStream API
	virtual int readBuffer(int16 *data, const int numSamples) {
		memset(data, 0, numSamples * 2);
		return numSamples;
	}

	virtual bool endOfData() const {
		return false;
	}
};

#endif
