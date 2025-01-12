
unit SDL_tray;
interface

{
  Automatically converted by H2Pas 1.0.0 from SDL_tray.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_tray.h
}

{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

Type
Pchar  = ^char;
Plongint  = ^longint;
PSDL_Surface  = ^SDL_Surface;
PSDL_Tray  = ^SDL_Tray;
PSDL_TrayEntry  = ^SDL_TrayEntry;
PSDL_TrayEntryFlags  = ^SDL_TrayEntryFlags;
PSDL_TrayMenu  = ^SDL_TrayMenu;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{
  Simple DirectMedia Layer
  Copyright (C) 1997-2025 Sam Lantinga <slouken@libsdl.org>

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
 }
{*
 * # CategoryTray
 *
 * System tray menu support.
  }
{$ifndef SDL_tray_h_}
{$define SDL_tray_h_}
{$include <SDL3/SDL_stdinc.h>}
{$include <SDL3/SDL_error.h>}
{$include <SDL3/SDL_surface.h>}
{$include <SDL3/SDL_video.h>}
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{*
 * An opaque handle representing a toplevel system tray object.
 *
 * \since This struct is available since SDL 3.2.0.
  }
type
{*
 * An opaque handle representing a menu/submenu on a system tray object.
 *
 * \since This struct is available since SDL 3.2.0.
  }
{*
 * An opaque handle representing an entry on a system tray object.
 *
 * \since This struct is available since SDL 3.2.0.
  }
{*
 * Flags that control the creation of system tray entries.
 *
 * Some of these flags are required; exactly one of them must be specified at
 * the time a tray entry is created. Other flags are optional; zero or more of
 * those can be OR'ed together with the required flag.
 *
 * \since This datatype is available since SDL 3.2.0.
 *
 * \sa SDL_InsertTrayEntryAt
  }

  PSDL_TrayEntryFlags = ^TSDL_TrayEntryFlags;
  TSDL_TrayEntryFlags = TUint32;
{*< Make the entry a simple button. Required.  }

const
  SDL_TRAYENTRY_BUTTON = $00000001;  
{*< Make the entry a checkbox. Required.  }
  SDL_TRAYENTRY_CHECKBOX = $00000002;  
{*< Prepare the entry to have a submenu. Required  }
  SDL_TRAYENTRY_SUBMENU = $00000004;  
{*< Make the entry disabled. Optional.  }
  SDL_TRAYENTRY_DISABLED = $80000000;  
{*< Make the entry checked. This is valid only for checkboxes. Optional.  }
  SDL_TRAYENTRY_CHECKED = $40000000;  
{*
 * A callback that is invoked when a tray entry is selected.
 *
 * \param userdata an optional pointer to pass extra data to the callback when
 *                 it will be invoked.
 * \param entry the tray entry that was selected.
 *
 * \since This datatype is available since SDL 3.2.0.
 *
 * \sa SDL_SetTrayEntryCallback
  }
type

  TSDL_TrayCallback = procedure (userdata:pointer; entry:PSDL_TrayEntry);cdecl;
{*
 * Create an icon to be placed in the operating system's tray, or equivalent.
 *
 * Many platforms advise not using a system tray unless persistence is a
 * necessary feature. Avoid needlessly creating a tray icon, as the user may
 * feel like it clutters their interface.
 *
 * Using tray icons require the video subsystem.
 *
 * \param icon a surface to be used as icon. May be NULL.
 * \param tooltip a tooltip to be displayed when the mouse hovers the icon in
 *                UTF-8 encoding. Not supported on all platforms. May be NULL.
 * \returns The newly created system tray icon.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_CreateTrayMenu
 * \sa SDL_GetTrayMenu
 * \sa SDL_DestroyTray
  }
(* Const before type ignored *)

function SDL_CreateTray(icon:PSDL_Surface; tooltip:Pchar):PSDL_Tray;cdecl;external;
{*
 * Updates the system tray icon's icon.
 *
 * \param tray the tray icon to be updated.
 * \param icon the new icon. May be NULL.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_CreateTray
  }
procedure SDL_SetTrayIcon(tray:PSDL_Tray; icon:PSDL_Surface);cdecl;external;
{*
 * Updates the system tray icon's tooltip.
 *
 * \param tray the tray icon to be updated.
 * \param tooltip the new tooltip in UTF-8 encoding. May be NULL.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_CreateTray
  }
(* Const before type ignored *)
procedure SDL_SetTrayTooltip(tray:PSDL_Tray; tooltip:Pchar);cdecl;external;
{*
 * Create a menu for a system tray.
 *
 * This should be called at most once per tray icon.
 *
 * This function does the same thing as SDL_CreateTraySubmenu(), except that
 * it takes a SDL_Tray instead of a SDL_TrayEntry.
 *
 * A menu does not need to be destroyed; it will be destroyed with the tray.
 *
 * \param tray the tray to bind the menu to.
 * \returns the newly created menu.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_CreateTray
 * \sa SDL_GetTrayMenu
 * \sa SDL_GetTrayMenuParentTray
  }
function SDL_CreateTrayMenu(tray:PSDL_Tray):PSDL_TrayMenu;cdecl;external;
{*
 * Create a submenu for a system tray entry.
 *
 * This should be called at most once per tray entry.
 *
 * This function does the same thing as SDL_CreateTrayMenu, except that it
 * takes a SDL_TrayEntry instead of a SDL_Tray.
 *
 * A menu does not need to be destroyed; it will be destroyed with the tray.
 *
 * \param entry the tray entry to bind the menu to.
 * \returns the newly created menu.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_InsertTrayEntryAt
 * \sa SDL_GetTraySubmenu
 * \sa SDL_GetTrayMenuParentEntry
  }
function SDL_CreateTraySubmenu(entry:PSDL_TrayEntry):PSDL_TrayMenu;cdecl;external;
{*
 * Gets a previously created tray menu.
 *
 * You should have called SDL_CreateTrayMenu() on the tray object. This
 * function allows you to fetch it again later.
 *
 * This function does the same thing as SDL_GetTraySubmenu(), except that it
 * takes a SDL_Tray instead of a SDL_TrayEntry.
 *
 * A menu does not need to be destroyed; it will be destroyed with the tray.
 *
 * \param tray the tray entry to bind the menu to.
 * \returns the newly created menu.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_CreateTray
 * \sa SDL_CreateTrayMenu
  }
function SDL_GetTrayMenu(tray:PSDL_Tray):PSDL_TrayMenu;cdecl;external;
{*
 * Gets a previously created tray entry submenu.
 *
 * You should have called SDL_CreateTraySubenu() on the entry object. This
 * function allows you to fetch it again later.
 *
 * This function does the same thing as SDL_GetTrayMenu(), except that it
 * takes a SDL_TrayEntry instead of a SDL_Tray.
 *
 * A menu does not need to be destroyed; it will be destroyed with the tray.
 *
 * \param entry the tray entry to bind the menu to.
 * \returns the newly created menu.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_InsertTrayEntryAt
 * \sa SDL_CreateTraySubmenu
  }
function SDL_GetTraySubmenu(entry:PSDL_TrayEntry):PSDL_TrayMenu;cdecl;external;
{*
 * Returns a list of entries in the menu, in order.
 *
 * \param menu The menu to get entries from.
 * \param size An optional pointer to obtain the number of entries in the
 *             menu.
 * \returns a NULL-terminated list of entries within the given menu. The
 *          pointer becomes invalid when any function that inserts or deletes
 *          entries in the menu is called.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_RemoveTrayEntry
 * \sa SDL_InsertTrayEntryAt
  }
(* Const before type ignored *)
function SDL_GetTrayEntries(menu:PSDL_TrayMenu; size:Plongint):^PSDL_TrayEntry;cdecl;external;
{*
 * Removes a tray entry.
 *
 * \param entry The entry to be deleted.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_GetTrayEntries
 * \sa SDL_InsertTrayEntryAt
  }
procedure SDL_RemoveTrayEntry(entry:PSDL_TrayEntry);cdecl;external;
{*
 * Insert a tray entry at a given position.
 *
 * If label is NULL, the entry will be a separator. Many functions won't work
 * for an entry that is a separator.
 *
 * An entry does not need to be destroyed; it will be destroyed with the tray.
 *
 * \param menu the menu to append the entry to.
 * \param pos the desired position for the new entry. Entries at or following
 *            this place will be moved. If pos is -1, the entry is appended.
 * \param label the text to be displayed on the entry, in UTF-8 encoding, or
 *              NULL for a separator.
 * \param flags a combination of flags, some of which are mandatory.
 * \returns the newly created entry, or NULL if pos is out of bounds.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_TrayEntryFlags
 * \sa SDL_GetTrayEntries
 * \sa SDL_RemoveTrayEntry
 * \sa SDL_GetTrayEntryParent
  }
(* Const before type ignored *)
function SDL_InsertTrayEntryAt(menu:PSDL_TrayMenu; pos:longint; _label:Pchar; flags:TSDL_TrayEntryFlags):PSDL_TrayEntry;cdecl;external;
{*
 * Sets the label of an entry.
 *
 * An entry cannot change between a separator and an ordinary entry; that is,
 * it is not possible to set a non-NULL label on an entry that has a NULL
 * label (separators), or to set a NULL label to an entry that has a non-NULL
 * label. The function will silently fail if that happens.
 *
 * \param entry the entry to be updated.
 * \param label the new label for the entry in UTF-8 encoding.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_GetTrayEntries
 * \sa SDL_InsertTrayEntryAt
 * \sa SDL_GetTrayEntryLabel
  }
(* Const before type ignored *)
procedure SDL_SetTrayEntryLabel(entry:PSDL_TrayEntry; _label:Pchar);cdecl;external;
{*
 * Gets the label of an entry.
 *
 * If the returned value is NULL, the entry is a separator.
 *
 * \param entry the entry to be read.
 * \returns the label of the entry in UTF-8 encoding.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_GetTrayEntries
 * \sa SDL_InsertTrayEntryAt
 * \sa SDL_SetTrayEntryLabel
  }
(* Const before type ignored *)
function SDL_GetTrayEntryLabel(entry:PSDL_TrayEntry):Pchar;cdecl;external;
{*
 * Sets whether or not an entry is checked.
 *
 * The entry must have been created with the SDL_TRAYENTRY_CHECKBOX flag.
 *
 * \param entry the entry to be updated.
 * \param checked SDL_TRUE if the entry should be checked; SDL_FALSE
 *                otherwise.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_GetTrayEntries
 * \sa SDL_InsertTrayEntryAt
 * \sa SDL_GetTrayEntryChecked
  }
procedure SDL_SetTrayEntryChecked(entry:PSDL_TrayEntry; checked:Tbool);cdecl;external;
{*
 * Gets whether or not an entry is checked.
 *
 * The entry must have been created with the SDL_TRAYENTRY_CHECKBOX flag.
 *
 * \param entry the entry to be read.
 * \returns SDL_TRUE if the entry is checked; SDL_FALSE otherwise.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_GetTrayEntries
 * \sa SDL_InsertTrayEntryAt
 * \sa SDL_SetTrayEntryChecked
  }
function SDL_GetTrayEntryChecked(entry:PSDL_TrayEntry):Tbool;cdecl;external;
{*
 * Sets whether or not an entry is enabled.
 *
 * \param entry the entry to be updated.
 * \param enabled SDL_TRUE if the entry should be enabled; SDL_FALSE
 *                otherwise.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_GetTrayEntries
 * \sa SDL_InsertTrayEntryAt
 * \sa SDL_GetTrayEntryEnabled
  }
procedure SDL_SetTrayEntryEnabled(entry:PSDL_TrayEntry; enabled:Tbool);cdecl;external;
{*
 * Gets whether or not an entry is enabled.
 *
 * \param entry the entry to be read.
 * \returns SDL_TRUE if the entry is enabled; SDL_FALSE otherwise.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_GetTrayEntries
 * \sa SDL_InsertTrayEntryAt
 * \sa SDL_SetTrayEntryEnabled
  }
function SDL_GetTrayEntryEnabled(entry:PSDL_TrayEntry):Tbool;cdecl;external;
{*
 * Sets a callback to be invoked when the entry is selected.
 *
 * \param entry the entry to be updated.
 * \param callback a callback to be invoked when the entry is selected.
 * \param userdata an optional pointer to pass extra data to the callback when
 *                 it will be invoked.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_GetTrayEntries
 * \sa SDL_InsertTrayEntryAt
  }
procedure SDL_SetTrayEntryCallback(entry:PSDL_TrayEntry; callback:TSDL_TrayCallback; userdata:pointer);cdecl;external;
{*
 * Destroys a tray object.
 *
 * This also destroys all associated menus and entries.
 *
 * \param tray the tray icon to be destroyed.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_CreateTray
  }
procedure SDL_DestroyTray(tray:PSDL_Tray);cdecl;external;
{*
 * Gets the menu contianing a certain tray entry.
 *
 * \param entry the entry for which to get the parent menu.
 * \returns the parent menu.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_InsertTrayEntryAt
  }
function SDL_GetTrayEntryParent(entry:PSDL_TrayEntry):PSDL_TrayMenu;cdecl;external;
{*
 * Gets the entry for which the menu is a submenu, if the current menu is a
 * submenu.
 *
 * Either this function or SDL_GetTrayMenuParentTray() will return non-NULL
 * for any given menu.
 *
 * \param menu the menu for which to get the parent entry.
 * \returns the parent entry, or NULL if this menu is not a submenu.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_CreateTraySubmenu
 * \sa SDL_GetTrayMenuParentTray
  }
function SDL_GetTrayMenuParentEntry(menu:PSDL_TrayMenu):PSDL_TrayEntry;cdecl;external;
{*
 * Gets the tray for which this menu is the first-level menu, if the current
 * menu isn't a submenu.
 *
 * Either this function or SDL_GetTrayMenuParentEntry() will return non-NULL
 * for any given menu.
 *
 * \param menu the menu for which to get the parent enttrayry.
 * \returns the parent tray, or NULL if this menu is a submenu.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_CreateTrayMenu
 * \sa SDL_GetTrayMenuParentEntry
  }
function SDL_GetTrayMenuParentTray(menu:PSDL_TrayMenu):PSDL_Tray;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_tray_h_  }

implementation


end.
