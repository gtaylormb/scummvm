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

#ifndef TITANIC_BRIDGE_VIEW_H
#define TITANIC_BRIDGE_VIEW_H

#include "titanic/core/background.h"

namespace Titanic {

enum BridgeAction {
	BA_NONE = 0, BA_GO = 1, BA_CRUISE = 2, BA_ENDING1 = 3, BA_ENDING2 = 4
};

class CBridgeView : public CBackground {
	DECLARE_MESSAGE_MAP;
	bool ActMsg(CActMsg *msg);
	bool MovieEndMsg(CMovieEndMsg *msg);
public:
	BridgeAction _action;
public:
	CLASSDEF;
	CBridgeView() : CBackground(), _action(BA_NONE) {}

	/**
	 * Save the data for the class to file
	 */
	virtual void save(SimpleFile *file, int indent);

	/**
	 * Load the data for the class from file
	 */
	virtual void load(SimpleFile *file);
};

} // End of namespace Titanic

#endif /* TITANIC_BRIDGE_VIEW_H */
