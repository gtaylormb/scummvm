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

#define FORBIDDEN_SYMBOL_ALLOW_ALL
#include "common/scummsys.h"

#ifdef USE_ALSA

#include <sys/ioctl.h>
#include <alsa/asoundlib.h>
#include <sound/asound_fm.h>

#include "sci/sci.h"

#include "common/file.h"
#include "common/system.h"
#include "common/textconsole.h"

#include "sci/resource.h"
#undef MIDI_CHANNELS // HACK
#include "sci/sound/drivers/mididriver.h"

namespace Sci {

// FIXME: We don't seem to be sending the polyphony init data, so disable this for now
#define ADLIB_DISABLE_VOICE_MAPPING

class MidiPlayer_AdLibALSA : public MidiPlayer {
public:
	enum {
		kVoices = 9,
		kRhythmKeys = 62
	};

	MidiPlayer_AdLibALSA(SciVersion version) : MidiPlayer(version), _playSwitch(true), _masterVolume(15), _rhythmKeyMap(0),
		_timerProc(nullptr), _timerParam(nullptr) { }
	virtual ~MidiPlayer_AdLibALSA() { }

	// MidiDriver
	int open(ResourceManager *resMan);
	int openAdLib(bool isSCI0);
	void close();
	void send(uint32 b);
	MidiChannel *allocateChannel() { return NULL; }
	MidiChannel *getPercussionChannel() { return NULL; }

	void setVolume(byte volume);
	void playSwitch(bool play);
	bool loadResource(const byte *data, uint size);
	virtual uint32 property(int prop, uint32 param);

	bool useRhythmChannel() const { return _rhythmKeyMap != NULL; }

	byte getPlayId() const;
	int getPolyphony() const { return kVoices; }
	bool hasRhythmChannel() const { return false; }
	void loadInstrument(int idx, byte *data);

	int getLastChannel() const { return (useRhythmChannel() ? 8 : 15); }

private:
	enum ChannelID {
		kLeftChannel = 1,
		kRightChannel = 2
	};

	struct AdLibOperator {
		bool amplitudeMod;
		bool vibrato;
		bool envelopeType;
		bool kbScaleRate;
		byte frequencyMult;		// (0-15)
		byte kbScaleLevel;		// (0-3)
		byte totalLevel;		// (0-63, 0=max, 63=min)
		byte attackRate;		// (0-15)
		byte decayRate;			// (0-15)
		byte sustainLevel;		// (0-15)
		byte releaseRate;		// (0-15)
		byte waveForm;			// (0-3)
	};

	struct AdLibModulator {
		byte feedback;			// (0-7)
		bool algorithm;
	};

	struct AdLibPatch {
		AdLibOperator op[2];
		AdLibModulator mod;
	};

	struct Channel {
		uint8 patch;			// Patch setting
		uint8 volume;			// Channel volume (0-63)
		uint8 pan;				// Pan setting (0-127, 64 is center)
		uint8 holdPedal;		// Hold pedal setting (0 to 63 is off, 127 to 64 is on)
		uint8 extraVoices;		// The number of additional voices this channel optimally needs
		uint16 pitchWheel;		// Pitch wheel setting (0-16383, 8192 is center)
		uint8 lastVoice;		// Last voice used for this MIDI channel
		bool enableVelocity;	// Enable velocity control (SCI0)

		Channel() : patch(0), volume(63), pan(64), holdPedal(0), extraVoices(0),
					pitchWheel(8192), lastVoice(0), enableVelocity(false) { }
	};

	struct AdLibVoice {
		int8 channel;			// MIDI channel that this voice is assigned to or -1
		int8 note;				// Currently playing MIDI note or -1
		int patch;				// Currently playing patch or -1
		uint8 velocity;			// Note velocity
		bool isSustained;		// Flag indicating a note that is being sustained by the hold pedal
		uint16 age;				// Age of the current note

		AdLibVoice() : channel(-1), note(-1), patch(-1), velocity(0), isSustained(false), age(0) { }
	};

	bool _stereo;
	bool _isSCI0;
	bool _playSwitch;
	int _masterVolume;
	Channel _channels[MIDI_CHANNELS];
	AdLibVoice _voices[kVoices];
	byte *_rhythmKeyMap;
	Common::Array<AdLibPatch> _patches;
	snd_hwdep_t *_opl;
	Common::TimerManager::TimerProc _timerProc;
	void *_timerParam;

	void loadInstrument(const byte *ins);
	void voiceOn(int voice, int note, int velocity);
	void voiceOff(int voice);
	void setPatch(int voice, int patch);
	void setNote(int voice, int note, bool key);
	void setVelocity(int voice);
	void setOperator(int voice, int opIdx, int patch, int volume, int pan);
	void renewNotes(int channel, bool key);
	void noteOn(int channel, int note, int velocity);
	void noteOff(int channel, int note);
	int findVoice(int channel);
	void voiceMapping(int channel, int voices);
	void assignVoices(int channel, int voices);
	void releaseVoices(int channel, int voices);
	void donateVoices();
	int findVoiceBasic(int channel);
	int calcVelocity(int voice, int op);

	bool alsaOpenHwDep(const Common::String &name, int iface);
	bool alsaOpen(int iface);
	static void midiTimerCallback(void *p);
	void setTimerCallback(void *timer_param, Common::TimerManager::TimerProc timer_proc);
	uint32 getBaseTempo() { return 10000; }
};

static const byte velocityMap1[64] = {
	0x00, 0x0c, 0x0d, 0x0e, 0x0f, 0x11, 0x12, 0x13,
	0x14, 0x16, 0x17, 0x18, 0x1a, 0x1b, 0x1c, 0x1d,
	0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26,
	0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2d, 0x2d, 0x2e,
	0x2f, 0x30, 0x31, 0x32, 0x32, 0x33, 0x34, 0x34,
	0x35, 0x36, 0x36, 0x37, 0x38, 0x38, 0x39, 0x3a,
	0x3b, 0x3b, 0x3b, 0x3c, 0x3c, 0x3c, 0x3d, 0x3d,
	0x3d, 0x3e, 0x3e, 0x3e, 0x3e, 0x3f, 0x3f, 0x3f
};

static const byte velocityMap2[64] = {
	0x00, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a,
	0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x21,
	0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29,
	0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 0x2f, 0x30,
	0x31, 0x32, 0x32, 0x33, 0x34, 0x34, 0x35, 0x36,
	0x36, 0x37, 0x38, 0x38, 0x39, 0x39, 0x3a, 0x3a,
	0x3b, 0x3b, 0x3b, 0x3c, 0x3c, 0x3c, 0x3d, 0x3d,
	0x3d, 0x3e, 0x3e, 0x3e, 0x3e, 0x3f, 0x3f, 0x3f
};

static const int ym3812_note[13] = {
	0x157, 0x16b, 0x181, 0x198, 0x1b0, 0x1ca,
	0x1e5, 0x202, 0x220, 0x241, 0x263, 0x287,
	0x2ae
};

// From sbiload
bool MidiPlayer_AdLibALSA::alsaOpenHwDep(const Common::String &name, int iface)
{
	snd_hwdep_info_t *info;

	if (snd_hwdep_open(&_opl, name.c_str(), SND_HWDEP_OPEN_WRITE) < 0)
		return false;

	snd_hwdep_info_alloca(&info);
	if (!snd_hwdep_info(_opl, info)) {
		if (snd_hwdep_info_get_iface(info) == iface) {
			return true;}
	}
	snd_hwdep_close(_opl);
	_opl = nullptr;
	return false;
}

bool MidiPlayer_AdLibALSA::alsaOpen(int iface)
{
	int card = -1;
	snd_ctl_t *ctl;

	while (!snd_card_next(&card) && card >= 0) {
		int dev;
		Common::String name = Common::String::format("hw:%d", card);
		if (snd_ctl_open(&ctl, name.c_str(), 0) < 0)
			continue;
		dev = -1;
		while (!snd_ctl_hwdep_next_device(ctl, &dev) && dev >= 0) {
			name = Common::String::format("hw:%d,%d", card, dev);
			if (alsaOpenHwDep(name, iface)) {
				snd_ctl_close(ctl);
				return true;
			}
		}
		snd_ctl_close(ctl);
	}
	return false;
}

int MidiPlayer_AdLibALSA::openAdLib(bool isSCI0) {
	debug(3, "ADLIB: Starting driver in %s mode", (isSCI0 ? "SCI0" : "SCI1"));
	_isSCI0 = isSCI0;

	if (alsaOpen(SND_HWDEP_IFACE_OPL3)) {
		debug("AdLibALSA: Found OPL3");
		_stereo = true;
	} else if (alsaOpen(SND_HWDEP_IFACE_OPL2)) {
		debug("AdLibALSA: Found OPL2");
		_stereo = false;
	} else {
		debug("AdLibALSA: No OPL2/3 found");
		return -1;
	}

	snd_hwdep_ioctl(_opl, SNDRV_DM_FM_IOCTL_RESET, nullptr);
	snd_hwdep_ioctl(_opl, SNDRV_DM_FM_IOCTL_SET_MODE, (void *)SNDRV_DM_FM_MODE_OPL3);

	snd_dm_fm_params params;
	memset(&params, 0, sizeof(params));
	snd_hwdep_ioctl(_opl, SNDRV_DM_FM_IOCTL_SET_PARAMS, (void *)&params);

	return 0;
}

void MidiPlayer_AdLibALSA::midiTimerCallback(void *p) {
	MidiPlayer_AdLibALSA *m = (MidiPlayer_AdLibALSA *)p;

	// Increase the age of the notes
	for (int i = 0; i < kVoices; i++) {
		if (m->_voices[i].note != -1)
			m->_voices[i].age++;
	}

	if (m->_timerProc)
		m->_timerProc(m->_timerParam);
}

void MidiPlayer_AdLibALSA::setTimerCallback(void *timer_param, Common::TimerManager::TimerProc timer_proc) {
	if (_timerProc)
		g_system->getTimerManager()->removeTimerProc(midiTimerCallback);

	_timerParam = timer_param;
	_timerProc = timer_proc;

	g_system->getTimerManager()->installTimerProc(midiTimerCallback, 10000, this, "AdLib ALSA");
}

void MidiPlayer_AdLibALSA::close() {
	if (_timerProc) {
		g_system->getTimerManager()->removeTimerProc(midiTimerCallback);
		_timerProc = nullptr;
	}

	snd_hwdep_ioctl(_opl, SNDRV_DM_FM_IOCTL_RESET, nullptr);
	snd_hwdep_close(_opl);
	delete[] _rhythmKeyMap;
}

void MidiPlayer_AdLibALSA::setVolume(byte volume) {
	_masterVolume = volume;
	renewNotes(-1, true);
}

// MIDI messages can be found at http://www.midi.org/techspecs/midimessages.php
void MidiPlayer_AdLibALSA::send(uint32 b) {
	byte command = b & 0xf0;
	byte channel = b & 0xf;
	byte op1 = (b >> 8) & 0xff;
	byte op2 = (b >> 16) & 0xff;

	switch (command) {
	case 0x80:
		noteOff(channel, op1);
		break;
	case 0x90:
		noteOn(channel, op1, op2);
		break;
	case 0xb0:
		switch (op1) {
		case 0x07:
			_channels[channel].volume = op2 >> 1;
			renewNotes(channel, true);
			break;
		case 0x0a:
			_channels[channel].pan = op2;
			renewNotes(channel, true);
			break;
		case 0x40:
			_channels[channel].holdPedal = op2;
			if (op2 == 0) {
				for (int i = 0; i < kVoices; i++) {
					if ((_voices[i].channel == channel) && _voices[i].isSustained)
						voiceOff(i);
				}
			}
			break;
		case 0x4b:
#ifndef ADLIB_DISABLE_VOICE_MAPPING
			voiceMapping(channel, op2);
#endif
			break;
		case 0x4e:
			_channels[channel].enableVelocity = op2;
			break;
		case SCI_MIDI_CHANNEL_NOTES_OFF:
			for (int i = 0; i < kVoices; i++)
				if ((_voices[i].channel == channel) && (_voices[i].note != -1))
					voiceOff(i);
			break;
		default:
			//warning("ADLIB: ignoring MIDI command %02x %02x %02x", command | channel, op1, op2);
			break;
		}
		break;
	case 0xc0:
		_channels[channel].patch = op1;
		break;
	// The original AdLib driver from sierra ignores aftertouch completely, so should we
	case 0xa0: // Polyphonic key pressure (aftertouch)
	case 0xd0: // Channel pressure (aftertouch)
		break;
	case 0xe0:
		_channels[channel].pitchWheel = (op1 & 0x7f) | ((op2 & 0x7f) << 7);
		renewNotes(channel, true);
		break;
	default:
		warning("ADLIB: Unknown event %02x", command);
	}
}

void MidiPlayer_AdLibALSA::loadInstrument(const byte *ins) {
	AdLibPatch patch;

	// Set data for the operators
	for (int i = 0; i < 2; i++) {
		const byte *op = ins + i * 13;
		patch.op[i].kbScaleLevel = op[0] & 0x3;
		patch.op[i].frequencyMult = op[1] & 0xf;
		patch.op[i].attackRate = op[3] & 0xf;
		patch.op[i].sustainLevel = op[4] & 0xf;
		patch.op[i].envelopeType = op[5];
		patch.op[i].decayRate = op[6] & 0xf;
		patch.op[i].releaseRate = op[7] & 0xf;
		patch.op[i].totalLevel = op[8] & 0x3f;
		patch.op[i].amplitudeMod = op[9];
		patch.op[i].vibrato = op[10];
		patch.op[i].kbScaleRate = op[11];
	}
	patch.op[0].waveForm = ins[26] & 0x3;
	patch.op[1].waveForm = ins[27] & 0x3;

	// Set data for the modulator
	patch.mod.feedback = ins[2] & 0x7;
	patch.mod.algorithm = !ins[12]; // Flag is inverted

	_patches.push_back(patch);
}

void MidiPlayer_AdLibALSA::voiceMapping(int channel, int voices) {
	int curVoices = 0;

	for (int i = 0; i < kVoices; i++)
		if (_voices[i].channel == channel)
			curVoices++;

	curVoices += _channels[channel].extraVoices;

	if (curVoices < voices) {
		debug(3, "ADLIB: assigning %i additional voices to channel %i", voices - curVoices, channel);
		assignVoices(channel, voices - curVoices);
	} else if (curVoices > voices) {
		debug(3, "ADLIB: releasing %i voices from channel %i", curVoices - voices, channel);
		releaseVoices(channel, curVoices - voices);
		donateVoices();
	}
}

void MidiPlayer_AdLibALSA::assignVoices(int channel, int voices) {
	assert(voices > 0);

	for (int i = 0; i < kVoices; i++)
		if (_voices[i].channel == -1) {
			_voices[i].channel = channel;
			if (--voices == 0)
				return;
		}

	_channels[channel].extraVoices += voices;
}

void MidiPlayer_AdLibALSA::releaseVoices(int channel, int voices) {
	if (_channels[channel].extraVoices >= voices) {
		_channels[channel].extraVoices -= voices;
		return;
	}

	voices -= _channels[channel].extraVoices;
	_channels[channel].extraVoices = 0;

	for (int i = 0; i < kVoices; i++) {
		if ((_voices[i].channel == channel) && (_voices[i].note == -1)) {
			_voices[i].channel = -1;
			if (--voices == 0)
				return;
		}
	}

	for (int i = 0; i < kVoices; i++) {
		if (_voices[i].channel == channel) {
			voiceOff(i);
			_voices[i].channel = -1;
			if (--voices == 0)
				return;
		}
	}
}

void MidiPlayer_AdLibALSA::donateVoices() {
	int freeVoices = 0;

	for (int i = 0; i < kVoices; i++)
		if (_voices[i].channel == -1)
			freeVoices++;

	if (freeVoices == 0)
		return;

	for (int i = 0; i < MIDI_CHANNELS; i++) {
		if (_channels[i].extraVoices >= freeVoices) {
			assignVoices(i, freeVoices);
			_channels[i].extraVoices -= freeVoices;
			return;
		} else if (_channels[i].extraVoices > 0) {
			assignVoices(i, _channels[i].extraVoices);
			freeVoices -= _channels[i].extraVoices;
			_channels[i].extraVoices = 0;
		}
	}
}

void MidiPlayer_AdLibALSA::renewNotes(int channel, bool key) {
	for (int i = 0; i < kVoices; i++) {
		// Update all notes playing this channel
		if ((channel == -1) || (_voices[i].channel == channel)) {
			if (_voices[i].note != -1)
				setNote(i, _voices[i].note, key);
		}
	}
}

void MidiPlayer_AdLibALSA::noteOn(int channel, int note, int velocity) {
	if (velocity == 0)
		return noteOff(channel, note);

	velocity >>= 1;

	// Check for playable notes
	if ((note < 12) || (note > 107))
		return;

	for (int i = 0; i < kVoices; i++) {
		if ((_voices[i].channel == channel) && (_voices[i].note == note)) {
			voiceOff(i);
			voiceOn(i, note, velocity);
			return;
		}
	}

#ifdef ADLIB_DISABLE_VOICE_MAPPING
	int voice = findVoiceBasic(channel);
#else
	int voice = findVoice(channel);
#endif

	if (voice == -1) {
		debug(3, "ADLIB: failed to find free voice assigned to channel %i", channel);
		return;
	}

	voiceOn(voice, note, velocity);
}

// FIXME: Temporary, see comment at top of file regarding ADLIB_DISABLE_VOICE_MAPPING
int MidiPlayer_AdLibALSA::findVoiceBasic(int channel) {
	int voice = -1;
	int oldestVoice = -1;
	int oldestAge = -1;

	// Try to find a voice assigned to this channel that is free (round-robin)
	for (int i = 0; i < kVoices; i++) {
		int v = (_channels[channel].lastVoice + i + 1) % kVoices;

		if (_voices[v].note == -1) {
			voice = v;
			break;
		}

		// We also keep track of the oldest note in case the search fails
		if (_voices[v].age > oldestAge) {
			oldestAge = _voices[v].age;
			oldestVoice = v;
		}
	}

	if (voice == -1) {
		if (oldestVoice >= 0) {
			voiceOff(oldestVoice);
			voice = oldestVoice;
		} else {
			return -1;
		}
	}

	_voices[voice].channel = channel;
	_channels[channel].lastVoice = voice;
	return voice;
}

int MidiPlayer_AdLibALSA::findVoice(int channel) {
	int voice = -1;
	int oldestVoice = -1;
	uint32 oldestAge = 0;

	// Try to find a voice assigned to this channel that is free (round-robin)
	for (int i = 0; i < kVoices; i++) {
		int v = (_channels[channel].lastVoice + i + 1) % kVoices;

		if (_voices[v].channel == channel) {
			if (_voices[v].note == -1) {
				voice = v;
				break;
			}

			// We also keep track of the oldest note in case the search fails
			// Notes started in the current time slice will not be selected
			if (_voices[v].age > oldestAge) {
				oldestAge = _voices[v].age;
				oldestVoice = v;
			}
		}
	}

	if (voice == -1) {
		if (oldestVoice >= 0) {
			voiceOff(oldestVoice);
			voice = oldestVoice;
		} else {
			return -1;
		}
	}

	_channels[channel].lastVoice = voice;
	return voice;
}

void MidiPlayer_AdLibALSA::noteOff(int channel, int note) {
	for (int i = 0; i < kVoices; i++) {
		if ((_voices[i].channel == channel) && (_voices[i].note == note)) {
			if (_channels[channel].holdPedal)
				_voices[i].isSustained = true;
			else
				voiceOff(i);
			return;
		}
	}
}

void MidiPlayer_AdLibALSA::voiceOn(int voice, int note, int velocity) {
	int channel = _voices[voice].channel;
	int patch;

	_voices[voice].age = 0;

	if ((channel == 9) && _rhythmKeyMap) {
		patch = CLIP(note, 27, 88) + 101;
	} else {
		patch = _channels[channel].patch;
	}

	// Set patch if different from current patch
	if (patch != _voices[voice].patch)
		setPatch(voice, patch);

	_voices[voice].velocity = velocity;
	setNote(voice, note, true);
}

void MidiPlayer_AdLibALSA::voiceOff(int voice) {
	_voices[voice].isSustained = false;
	setNote(voice, _voices[voice].note, 0);
	_voices[voice].note = -1;
	_voices[voice].age = 0;
}

void MidiPlayer_AdLibALSA::setNote(int voice, int note, bool key) {
	int channel = _voices[voice].channel;
	int n, fre, oct;
	float delta;
	int bend = _channels[channel].pitchWheel;

	if ((channel == 9) && _rhythmKeyMap) {
		note = _rhythmKeyMap[CLIP(note, 27, 88) - 27];
	}

	_voices[voice].note = note;

	n = note % 12;

	if (bend < 8192)
		bend = 8192 - bend;
	delta = (float)pow(2.0, (bend % 8192) / 8192.0);

	if (bend > 8192)
		fre = (int)(ym3812_note[n] * delta);
	else
		fre = (int)(ym3812_note[n] / delta);

	oct = note / 12 - 1;

	if (oct < 0)
		oct = 0;

	if (oct > 7)
		oct = 7;

	snd_dm_fm_note fmNote;

	fmNote.voice = voice;
	fmNote.octave = oct;
	fmNote.fnum = fre;
	fmNote.key_on = key;

	snd_hwdep_ioctl(_opl, SNDRV_DM_FM_IOCTL_PLAY_NOTE, (void *)&fmNote);

	if (_stereo) {
		fmNote.voice += 9;
		snd_hwdep_ioctl(_opl, SNDRV_DM_FM_IOCTL_PLAY_NOTE, (void *)&fmNote);
	}

	setVelocity(voice);
}

void MidiPlayer_AdLibALSA::setVelocity(int voice) {
	int pan = _channels[_voices[voice].channel].pan;
	setOperator(voice, 1, _voices[voice].patch, (_playSwitch ? calcVelocity(voice, 1) : 0), pan);

	// In AM mode we need to set the level for both operators
	if (_patches[_voices[voice].patch].mod.algorithm == 1)
		setOperator(voice, 0, _voices[voice].patch, (_playSwitch ? calcVelocity(voice, 0) : 0), pan);
}

int MidiPlayer_AdLibALSA::calcVelocity(int voice, int op) {
	if (_isSCI0) {
		int velocity = _masterVolume;

		if (velocity > 0)
			velocity += 3;

		if (velocity > 15)
			velocity = 15;

		int insVelocity;
		if (_channels[_voices[voice].channel].enableVelocity)
			insVelocity = _voices[voice].velocity;
		else
			insVelocity = 63 - _patches[_voices[voice].patch].op[op].totalLevel;

		// Note: Later SCI0 has a static table that is close to this formula, but not exactly the same.
		// Early SCI0 does (velocity * (insVelocity / 15))
		return velocity * insVelocity / 15;
	} else {
		AdLibOperator &oper = _patches[_voices[voice].patch].op[op];
		int velocity = _channels[_voices[voice].channel].volume + 1;
		velocity = velocity * (velocityMap1[_voices[voice].velocity] + 1) / 64;
		velocity = velocity * (_masterVolume + 1) / 16;

		if (--velocity < 0)
			velocity = 0;

		return velocityMap2[velocity] * (63 - oper.totalLevel) / 63;
	}
}

void MidiPlayer_AdLibALSA::setPatch(int voice, int patch) {
	if ((patch < 0) || ((uint)patch >= _patches.size())) {
		warning("ADLIB: Invalid patch %i requested", patch);
		// Substitute instrument 0
		patch = 0;
	}

	_voices[voice].patch = patch;

	setOperator(voice, 0, patch, 63 - _patches[patch].op[0].totalLevel, 0x40);
	setOperator(voice, 1, patch, 63 - _patches[patch].op[1].totalLevel, 0x40);
}

void MidiPlayer_AdLibALSA::setOperator(int voice, int opIdx, int patch, int volume, int pan) {
	const AdLibOperator &op = _patches[patch].op[opIdx];
	const AdLibModulator &mod = _patches[patch].mod;

	int velLeft = volume;
	int velRight = volume;

	if (_stereo) {
		if (pan > 0x40)
			velLeft = velLeft * (0x7f - pan) / 0x3f;
		else if (pan < 0x40)
			velRight = velRight * pan / 0x40;
	}

	snd_dm_fm_voice fmVoice;

	fmVoice.op = opIdx;
	fmVoice.am = op.amplitudeMod;
	fmVoice.vibrato = op.vibrato;
	fmVoice.do_sustain = op.envelopeType;
	fmVoice.kbd_scale = op.kbScaleRate;
	fmVoice.harmonic = op.frequencyMult;
	fmVoice.scale_level = op.kbScaleLevel;

	fmVoice.attack = op.attackRate;
	fmVoice.decay = op.decayRate;
	fmVoice.sustain = op.sustainLevel;
	fmVoice.release = op.releaseRate;

	fmVoice.feedback = mod.feedback;
	fmVoice.connection = mod.algorithm;
	fmVoice.waveform = op.waveForm;

	fmVoice.voice = voice;
	fmVoice.left = 1;

	if (_stereo)
		fmVoice.right = 0;
	else
		fmVoice.right = 1; // For OPL3, no effect on OPL2

	fmVoice.volume = velLeft;

	snd_hwdep_ioctl(_opl, SNDRV_DM_FM_IOCTL_SET_VOICE, &fmVoice);

	if (_stereo) {
		fmVoice.voice += 9;
		fmVoice.left = 0;
		fmVoice.right = 1;
		fmVoice.volume = velRight;

		snd_hwdep_ioctl(_opl, SNDRV_DM_FM_IOCTL_SET_VOICE, &fmVoice);
	}
}

void MidiPlayer_AdLibALSA::playSwitch(bool play) {
	_playSwitch = play;
	renewNotes(-1, play);
}

bool MidiPlayer_AdLibALSA::loadResource(const byte *data, uint size) {
	if ((size != 1344) && (size != 2690) && (size != 5382)) {
		error("ADLIB: Unsupported patch format (%i bytes)", size);
		return false;
	}

	for (int i = 0; i < 48; i++)
		loadInstrument(data + (28 * i));

	if (size == 1344) {
		byte dummy[28] = {0};

		// Only 48 instruments, add dummies
		for (int i = 0; i < 48; i++)
			loadInstrument(dummy);
	} else if (size == 2690) {
		for (int i = 48; i < 96; i++)
			loadInstrument(data + 2 + (28 * i));
	} else {
		// SCI1.1 and later
		for (int i = 48; i < 190; i++)
			loadInstrument(data + (28 * i));
		_rhythmKeyMap = new byte[kRhythmKeys];
		memcpy(_rhythmKeyMap, data + 5320, kRhythmKeys);
	}

	return true;
}

uint32 MidiPlayer_AdLibALSA::property(int prop, uint32 param) {
	switch(prop) {
	case MIDI_PROP_MASTER_VOLUME:
		if (param != 0xffff)
			_masterVolume = param;
		return _masterVolume;
	default:
		break;
	}
	return 0;
}


int MidiPlayer_AdLibALSA::open(ResourceManager *resMan) {
	assert(resMan != NULL);

	// Load up the patch.003 file, parse out the instruments
	Resource *res = resMan->findResource(ResourceId(kResourceTypePatch, 3), 0);
	bool ok = false;

	if (res) {
		ok = loadResource(res->data, res->size);
	} else {
		// Early SCI0 games have the sound bank embedded in the AdLib driver

		Common::File f;

		if (f.open("ADL.DRV")) {
			int size = f.size();
			const uint patchSize = 1344;

			// Note: Funseeker's Guide also has another version of adl.drv, 8803 bytes.
			// This isn't supported, but it's not really used anywhere, as that demo
			// doesn't have sound anyway.
			if ((size == 5684) || (size == 5720) || (size == 5727)) {
				byte *buf = new byte[patchSize];

				if (f.seek(0x45a) && (f.read(buf, patchSize) == patchSize))
					ok = loadResource(buf, patchSize);

				delete[] buf;
			}
		}
	}

	if (!ok) {
		warning("ADLIB: Failed to load patch.003");
		return -1;
	}

	return openAdLib(_version <= SCI_VERSION_0_LATE);
}

byte MidiPlayer_AdLibALSA::getPlayId() const {
	switch (_version) {
	case SCI_VERSION_0_EARLY:
		return 0x01;
	case SCI_VERSION_0_LATE:
		return 0x04;
	default:
		return 0x00;
	}
}

MidiPlayer *MidiPlayer_AdLibALSA_create(SciVersion _soundVersion) {
	return new MidiPlayer_AdLibALSA(_soundVersion);
}

} // End of namespace Sci

#endif // USE_ALSA
