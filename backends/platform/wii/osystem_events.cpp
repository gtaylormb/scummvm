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
 */

#include <unistd.h>
#include <malloc.h>

#ifndef GAMECUBE
#include <wiiuse/wpad.h>
#include <wiikeyboard/keyboard.h>
#endif

#include <ogc/lwp_watchdog.h>

#include "osystem.h"

#define TIMER_THREAD_STACKSIZE (1024 * 32)
#define TIMER_THREAD_PRIO 64

#define KBD_THREAD_STACKSIZE (1024 * 8)
#define KBD_THREAD_PRIO 64

#define PAD_CHECK_TIME 40

#ifndef GAMECUBE
#define PADS_A (PAD_BUTTON_A | (WPAD_BUTTON_A << 16))
#define PADS_B (PAD_BUTTON_B | (WPAD_BUTTON_B << 16))
#define PADS_X (PAD_BUTTON_X | (WPAD_BUTTON_MINUS << 16))
#define PADS_Y (PAD_BUTTON_Y | (WPAD_BUTTON_PLUS << 16))
#define PADS_Z (PAD_TRIGGER_Z | (WPAD_BUTTON_2 << 16))
#define PADS_START (PAD_BUTTON_START | (WPAD_BUTTON_HOME << 16))
#define PADS_UP (PAD_BUTTON_UP | (WPAD_BUTTON_UP << 16))
#define PADS_DOWN (PAD_BUTTON_DOWN | (WPAD_BUTTON_DOWN << 16))
#define PADS_LEFT (PAD_BUTTON_LEFT | (WPAD_BUTTON_LEFT << 16))
#define PADS_RIGHT (PAD_BUTTON_RIGHT | (WPAD_BUTTON_RIGHT << 16))
#else
#define PADS_A PAD_BUTTON_A
#define PADS_B PAD_BUTTON_B
#define PADS_X PAD_BUTTON_X
#define PADS_Y PAD_BUTTON_Y
#define PADS_Z PAD_TRIGGER_Z
#define PADS_START PAD_BUTTON_START
#define PADS_UP PAD_BUTTON_UP
#define PADS_DOWN PAD_BUTTON_DOWN
#define PADS_LEFT PAD_BUTTON_LEFT
#define PADS_RIGHT PAD_BUTTON_RIGHT
#endif

#ifndef GAMECUBE
static int keymap[][2] = {
	{ KBD_return, Common::KEYCODE_RETURN },
	{ KBD_Up, Common::KEYCODE_UP },
	{ KBD_Down, Common::KEYCODE_DOWN },
	{ KBD_Left, Common::KEYCODE_LEFT },
	{ KBD_Right, Common::KEYCODE_RIGHT },
	{ KBD_KP_0, Common::KEYCODE_KP0 },
	{ KBD_KP_1, Common::KEYCODE_KP1 },
	{ KBD_KP_2, Common::KEYCODE_KP2 },
	{ KBD_KP_3, Common::KEYCODE_KP3 },
	{ KBD_KP_4, Common::KEYCODE_KP4 },
	{ KBD_KP_5, Common::KEYCODE_KP5 },
	{ KBD_KP_6, Common::KEYCODE_KP6 },
	{ KBD_KP_7, Common::KEYCODE_KP7 },
	{ KBD_KP_8, Common::KEYCODE_KP8 },
	{ KBD_KP_9, Common::KEYCODE_KP9 },
	{ KBD_Home, Common::KEYCODE_HOME },
	{ KBD_Insert, Common::KEYCODE_INSERT },
	{ KBD_End, Common::KEYCODE_END },
	{ KBD_Pageup, Common::KEYCODE_PAGEUP },
	{ KBD_Pagedown, Common::KEYCODE_PAGEDOWN },
	{ KBD_KP_period, Common::KEYCODE_KP_PERIOD },
	{ KBD_KP_slash, Common::KEYCODE_KP_DIVIDE },
	{ KBD_KP_asterisk, Common::KEYCODE_KP_MULTIPLY },
	{ KBD_KP_plus, Common::KEYCODE_KP_PLUS },
	{ KBD_KP_minus, Common::KEYCODE_KP_MINUS },
	{ KBD_KP_equal, Common::KEYCODE_KP_EQUALS },
	{ KBD_KP_enter, Common::KEYCODE_KP_ENTER },
	{ 0, 0 }
};
#endif

static lwpq_t timer_queue;
static lwp_t timer_thread;
static u8 *timer_stack;
static bool timer_thread_running = false;
static bool timer_thread_quit = false;

static void * timer_thread_func(void *arg) {
	while (!timer_thread_quit) {
		DefaultTimerManager *tm =
			(DefaultTimerManager *) g_system->getTimerManager();
		tm->handler();

		usleep(1000 * 10);
	}

	return NULL;
}

#ifndef GAMECUBE
static lwpq_t kbd_queue;
static lwp_t kbd_thread;
static u8 *kbd_stack;
static bool kbd_thread_running = false;
static bool kbd_thread_quit = false;

static void * kbd_thread_func(void *arg) {
	while (!kbd_thread_quit) {
		KEYBOARD_Scan();
		usleep(1000 * 10);
	}

	return NULL;
}
#endif

void OSystem_Wii::initEvents() {
	timer_thread_quit = false;

	timer_stack = (u8 *) memalign(32, TIMER_THREAD_STACKSIZE);
	memset(timer_stack, 0, TIMER_THREAD_STACKSIZE);

	LWP_InitQueue(&timer_queue);

	s32 res = LWP_CreateThread(&timer_thread, timer_thread_func, NULL,
								timer_stack, TIMER_THREAD_STACKSIZE,
								TIMER_THREAD_PRIO);

	if (res) {
		printf("ERROR creating timer thread: %d\n", res);
		LWP_CloseQueue(timer_queue);
	}

	timer_thread_running = res == 0;
#ifndef GAMECUBE
	WPAD_Init();
	WPAD_SetDataFormat(WPAD_CHAN_0, WPAD_FMT_BTNS_ACC_IR);
	WPAD_SetIdleTimeout(120);

	if(KEYBOARD_Init() > 0) {
		kbd_thread_quit = false;

		kbd_stack = (u8 *) memalign(32, KBD_THREAD_STACKSIZE);
		memset(kbd_stack, 0, KBD_THREAD_STACKSIZE);

		LWP_InitQueue(&kbd_queue);

		s32 res = LWP_CreateThread(&kbd_thread, kbd_thread_func, NULL,
									kbd_stack, KBD_THREAD_STACKSIZE,
									KBD_THREAD_PRIO);

		if (res) {
			printf("ERROR creating keyboard thread: %d\n", res);
			LWP_CloseQueue(kbd_queue);
		}

		kbd_thread_running = res == 0;
	}
#endif
}

void OSystem_Wii::deinitEvents() {
	if (timer_thread_running) {
		timer_thread_quit = true;
		LWP_ThreadBroadcast(timer_queue);

		LWP_JoinThread(timer_thread, NULL);
		LWP_CloseQueue(timer_queue);

		timer_thread_running = false;
	}

#ifndef GAMECUBE
	if (kbd_thread_running) {
		kbd_thread_quit = true;
		LWP_ThreadBroadcast(kbd_queue);

		LWP_JoinThread(kbd_thread, NULL);
		LWP_CloseQueue(kbd_queue);

		kbd_thread_running = false;

		KEYBOARD_Deinit();
	}

	WPAD_Shutdown();
#endif
}

void OSystem_Wii::updateEventScreenResolution() {
#ifndef GAMECUBE
	WPAD_SetVRes(WPAD_CHAN_0, _currentWidth + _currentWidth / 5,
					_currentHeight + _currentHeight / 5);
#endif
}

#ifndef GAMECUBE
bool OSystem_Wii::pollKeyboard(Common::Event &event) {
	int i;
	keyboard_event kbdEvent;

	if (!KEYBOARD_GetEvent(&kbdEvent) > 0)
		return false;

	switch (kbdEvent.type) {
	case KEYBOARD_PRESSED:
		event.type = Common::EVENT_KEYDOWN;
		break;

	case KEYBOARD_RELEASED:
		event.type = Common::EVENT_KEYUP;
		break;

	case KEYBOARD_CONNECTED:
		printf("keyboard connected\n");
		return false;

	case KEYBOARD_DISCONNECTED:
		printf("keyboard disconnected\n");
		return false;

	default:
		return false;
	}

	if (kbdEvent.keysym.mod & KMOD_LSHIFT ||
	    kbdEvent.keysym.mod & KMOD_RSHIFT)
		event.kbd.flags |= Common::KBD_SHIFT;
	if (kbdEvent.keysym.mod & KMOD_LCTRL ||
	    kbdEvent.keysym.mod & KMOD_RCTRL)
		event.kbd.flags |= Common::KBD_CTRL;
	if (kbdEvent.keysym.mod & KMOD_LALT ||
	    kbdEvent.keysym.mod & KMOD_RALT)
		event.kbd.flags |= Common::KBD_ALT;

	i = 0;
	while (keymap[i][0] != 0) {
		if (keymap[i][0] == kbdEvent.keysym.sym) {
			event.kbd.keycode = static_cast<Common::KeyCode>(keymap[i][1]);
			event.kbd.ascii = kbdEvent.keysym.sym & 0xff;
			return true;
		}

		i++;
	}

	// function keys
	if (kbdEvent.keysym.sym >> 8 == 0xd8) {
		event.kbd.keycode =
			static_cast<Common::KeyCode>(kbdEvent.keysym.sym - 0xd6e7);
		event.kbd.ascii = kbdEvent.keysym.sym - 0xd6c6;

		return true;
	}

	// skip unmapped special keys
	if (kbdEvent.keysym.sym > 0xff)
		return false;

	event.kbd.keycode = static_cast<Common::KeyCode>(kbdEvent.keysym.sym);
	event.kbd.ascii = kbdEvent.keysym.sym;

	return true;
}
#endif

#define KBD_EVENT(pad_button, kbd_keycode, kbd_ascii, modifier) \
	do { \
		if ((bd | bu) & pad_button) { \
			if (bd & pad_button) \
				event.type = Common::EVENT_KEYDOWN; \
			else \
				event.type = Common::EVENT_KEYUP; \
			event.kbd.keycode = kbd_keycode; \
			event.kbd.ascii = kbd_ascii; \
			event.kbd.flags = modifier; \
			return true; \
		} \
	} while (0)

bool OSystem_Wii::pollEvent(Common::Event &event) {
	if ((reset_btn_pressed || power_btn_pressed) && !_event_quit) {
		_event_quit = true;
		event.type = Common::EVENT_QUIT;

		printf("quit event\n");

		return true;
	}

	u32 bd, bh, bu;

	PAD_ScanPads();

	bd = PAD_ButtonsDown(0);
	bh = PAD_ButtonsHeld(0);
	bu = PAD_ButtonsUp(0);

#ifndef GAMECUBE
	WPAD_ScanPads();

	s32 res = WPAD_Probe(0, NULL);

	if (res == WPAD_ERR_NONE) {

		bd |= WPAD_ButtonsDown(0) << 16;
		bh |= WPAD_ButtonsHeld(0) << 16;
		bu |= WPAD_ButtonsUp(0) << 16;
	}
#endif

	if (bd || bu) {
		KBD_EVENT(PADS_Z, Common::KEYCODE_RETURN, Common::ASCII_RETURN, 0);
		KBD_EVENT(PADS_X, Common::KEYCODE_ESCAPE, Common::ASCII_ESCAPE, 0);
		KBD_EVENT(PADS_Y, Common::KEYCODE_PERIOD, '.', 0);
		KBD_EVENT(PADS_START, Common::KEYCODE_F5, Common::ASCII_F5, 0);
		KBD_EVENT(PADS_UP, Common::KEYCODE_F5, Common::ASCII_F5, Common::KBD_CTRL);
		KBD_EVENT(PADS_DOWN, Common::KEYCODE_F7, Common::ASCII_F7, 0);
		//KBD_EVENT(PADS_LEFT, Common::KEYCODE_F8, Common::ASCII_F8, 0);
		//KBD_EVENT(PADS_RIGHT, Common::KEYCODE_n, 'n');

		if ((bd | bu) & (PADS_A | PADS_B)) {
			if (bd & PADS_A)
				event.type = Common::EVENT_LBUTTONDOWN;
			else if (bu & PADS_A)
				event.type = Common::EVENT_LBUTTONUP;
			else if (bd & PADS_B)
				event.type = Common::EVENT_RBUTTONDOWN;
			else if (bu & PADS_B)
				event.type = Common::EVENT_RBUTTONUP;

			event.mouse.x = _mouseX;
			event.mouse.y = _mouseY;

			return true;
		}
	}

	s32 mx = _mouseX;
	s32 my = _mouseY;

#ifndef GAMECUBE
	if (res == WPAD_ERR_NONE) {
		struct ir_t ir;

		WPAD_IR(0, &ir);

		if (ir.valid) {
			mx = ir.x - _currentWidth / 10;
			my = ir.y - _currentHeight / 10;

			if (mx < 0)
				mx = 0;

			if (mx >= _currentWidth)
				mx = _currentWidth - 1;

			if (my < 0)
				my = 0;

			if (my >= _currentHeight)
				my = _currentHeight - 1;

			if ((mx != _mouseX) || (my != _mouseY)) {
				event.type = Common::EVENT_MOUSEMOVE;
				event.mouse.x = _mouseX = mx;
				event.mouse.y = _mouseY = my;

				return true;
			}
		}
	}
#endif

	uint32 time = getMillis();
	if (time - _lastPadCheck > PAD_CHECK_TIME) {
		_lastPadCheck = time;

		if (abs (PAD_StickX(0)) > 16)
			mx += PAD_StickX(0) / (4 * _overlayWidth / _currentWidth);
		if (abs (PAD_StickY(0)) > 16)
			my -= PAD_StickY(0) / (4 * _overlayHeight / _currentHeight);

		if (mx < 0)
			mx = 0;

		if (mx >= _currentWidth)
			mx = _currentWidth - 1;

		if (my < 0)
			my = 0;

		if (my >= _currentHeight)
			my = _currentHeight - 1;

		if ((mx != _mouseX) || (my != _mouseY)) {
			event.type = Common::EVENT_MOUSEMOVE;
			event.mouse.x = _mouseX = mx;
			event.mouse.y = _mouseY = my;

			return true;
		}
	}

#ifndef GAMECUBE
	if (kbd_thread_running && pollKeyboard(event))
		return true;
#endif

	return false;
}

