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

#include "common/debug.h"
#include "audio/fmopl.h"
#include "audio/mididrv.h"
#include "audio/musicplugin.h"

namespace OPL {
namespace FPGA {

class OPL : public ::OPL::RealOPL {
private:
	Config::OplType _type;
	int index[2];
	MidiDriver *_midi;

	void writeOplReg(int c, int r, int v);
	MidiDriver::DeviceHandle getDeviceByName(const Common::String &name);

public:
	OPL(Config::OplType type);
	~OPL();

	bool init();
	void reset();

	void write(int a, int v);
	byte read(int a);

	void writeReg(int r, int v);
};

OPL::OPL(Config::OplType type) : _type(type), _midi(nullptr) {
}

OPL::~OPL() {
	stop();
	reset();

	if (_midi && _midi->isOpen())
		_midi->close();

	delete _midi;
}

MidiDriver::DeviceHandle OPL::getDeviceByName(const Common::String &name) {
	MusicPlugin::List p = MusicMan.getPlugins();

	for (MusicPlugin::List::const_iterator m = p.begin(); m != p.end(); m++) {
		MusicDevices i = (**m)->getDevices();
		for (MusicDevices::iterator d = i.begin(); d != i.end(); d++) {
			if (d->getName().equals(name)) {
				MidiDriver::DeviceHandle hdl = d->getHandle();
				if (MidiDriver::checkDevice(hdl))
					return hdl;
			}
		}
	}

	return 0;
}

bool OPL::init() {
	// This requires that MIDI backends report device names. Currently
	// the CoreMIDI backend doesn't do this
	MidiDriver::DeviceHandle dev = getDeviceByName("MIDI function");
	if (!dev) {
		warning("Could not find OPL3 FPGA MIDI device");
		return false;
	}

	_midi = MidiDriver::createMidi(dev);
	if (!_midi || _midi->open())
		return false;

	reset();

	if (_type == Config::kOpl2)
		writeOplReg(1, 0x05, 0x00);
	else if (_type == Config::kDualOpl2) {
		// Here we set up the OPL3 for dual OPL2 panning
		for (int i = 0; i < 9; ++i)
			writeOplReg(0, 0xc0 | i, 0);
		for (int i = 0; i < 9; ++i)
			writeOplReg(1, 0xc0 | i, 0);
	}

	return true;
}

void OPL::reset() {
	index[0] = index[1] = 0;

	// Reset code based on OPL3 Synth Driver by James Alan Nguyen
	writeOplReg(1, 0x05, 0x01); // OPL3 Enable
	writeOplReg(0, 0x01, 0x20); // Test Register
	writeOplReg(0, 0x02, 0x20); // Timer 1
	writeOplReg(0, 0x03, 0x00); // Timer 2
	writeOplReg(0, 0x04, 0x00); // IRQ Mask Clear
	writeOplReg(1, 0x04, 0x00); // 4-op mode disable
	writeOplReg(0, 0x08, 0x00); // Keyboard split

	for (int i = 0; i < 9; i++) {
		writeOplReg(0, 0xc0 | i, 0x00);
		writeOplReg(1, 0xc0 | i, 0x00);
	}

	for (int i = 0; i < 0x16; i++) {
		if ((i & 0x07) >= 0x06)
			continue;

		writeOplReg(0, 0x40 | i, 0x3f);
		writeOplReg(1, 0x40 | i, 0x3f);

		writeOplReg(0, 0x80 | i, 0xff);
		writeOplReg(1, 0x80 | i, 0xff);
		writeOplReg(0, 0x60 | i, 0xff);
		writeOplReg(1, 0x60 | i, 0xff);

		writeOplReg(0, 0x20 | i, 0x00);
		writeOplReg(1, 0x20 | i, 0x00);

		writeOplReg(0, 0xe0 | i, 0x00);
		writeOplReg(1, 0xe0 | i, 0x00);
	}

	writeOplReg(0, 0xbd, 0x00);

	for (int i = 0; i < 9; i++) {
		writeOplReg(0, 0xb0 | i, 0x00);
		writeOplReg(1, 0xb0 | i, 0x00);
		writeOplReg(0, 0xa0 | i, 0x00);
		writeOplReg(1, 0xa0 | i, 0x00);
	}

	for (int i = 0x40; i < 0xa0; i++) {
		if ((i & 0x07) >= 0x06 || (i & 0x1f) >= 0x18)
			continue;

		writeOplReg(0, 0x00 | i, 0x00);
		writeOplReg(1, 0x00 | i, 0x00);
	}

	// Unmute output (OPL3-FPGA extension)
	writeOplReg(1, 0x02, 1);
}

void OPL::write(int port, int val) {
	val &= 0xff;
	int chip = (port & 2) >> 1;

	if (port & 1) {
		switch(_type) {
		case Config::kOpl2:
			writeOplReg(0, index[0], val);
			break;
		case Config::kDualOpl2:
			if (port & 8) {
				writeOplReg(0, index[0], val);
				writeOplReg(1, index[1], val);
			} else
				writeOplReg(chip, index[chip], val);
			break;
		case Config::kOpl3:
			writeOplReg(chip, index[chip], val);
		}
	} else {
		switch(_type) {
		case Config::kOpl2:
			index[0] = val;
			break;
		case Config::kDualOpl2:
			if (port & 8) {
				index[0] = val;
				index[1] = val;
			} else
				index[chip] = val;
			break;
		case Config::kOpl3:
			index[chip] = val;
		}
	}
}

byte OPL::read(int port) {
	return 0;
}

void OPL::writeReg(int r, int v) {
	switch (_type) {
	case Config::kOpl2:
		writeOplReg(0, r, v);
		break;
	case Config::kDualOpl2:
		writeOplReg(0, r, v);
		writeOplReg(1, r, v);
		break;
	case Config::kOpl3:
		writeOplReg(r >= 0x100, r & 0xff, v);
	}
}

void OPL::writeOplReg(int c, int r, int v) {
	// Maintain panning bits for dual OPL2 mode
	if (_type == Config::kDualOpl2 && r >= 0xc0 && r <= 0xc8) {
		v &= 0xf;
		v |= (c ? 0x20 : 0x10);
	}

	byte msg[3];

	msg[0] = (0x90 | ((v >> 7) << 2) | (c << 1) | (r >> 7));
	msg[1] = r & 0x7f;
	msg[2] = v & 0x7f;

	_midi->send((msg[2] << 16) | (msg[1] << 8) | (msg[0]));
}

OPL *create(Config::OplType type) {
	return new OPL(type);
}

} // End of namespace FPGA
} // End of namespace OPL