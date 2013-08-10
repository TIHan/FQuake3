/*
===========================================================================
Copyright (C) 2013 William F. Smith
Copyright (C) 1999-2005 Id Software, Inc.

This file is part of Quake III Arena source code.

Quake III Arena source code is free software; you can redistribute it
and/or modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the License,
or (at your option) any later version.

Quake III Arena source code is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Foobar; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
===========================================================================
*/

// IMPORTANT: This file is only temporary and will not exist once most of the existing codebase
//	gets converted to F#. It is only intended as interop helpers.

#include "qm.h"

MObject
qm_map_cvar (const cvar_t *cvar)
{
	gpointer args[9];

	args [0] = m_string_as_arg (m_string (cvar->name));
	args [1] = m_string_as_arg (m_string (cvar->string));
	args [2] = m_string_as_arg (m_string (cvar->resetString));
	args [3] = m_string_as_arg (m_string (cvar->latchedString));
	args [4] = &cvar->flags;
	args [5] = &cvar->modified;
	args [6] = &cvar->modificationCount;
	args [7] = &cvar->value;
	args [8] = &cvar->integer;

	return m_object ("Engine", "Engine", "Cvar", 9, args);
}