unit SDL_tray;

interface

uses
  ctypes, SDL_stdinc;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


type
  PSDL_TrayEntryFlags = ^TSDL_TrayEntryFlags;
  TSDL_TrayEntryFlags = TUint32;

const
  SDL_TRAYENTRY_BUTTON = $00000001;  
  SDL_TRAYENTRY_CHECKBOX = $00000002;  
  SDL_TRAYENTRY_SUBMENU = $00000004;  
  SDL_TRAYENTRY_DISABLED = $80000000;  
  SDL_TRAYENTRY_CHECKED = $40000000;  
type
  TSDL_TrayCallback = procedure (userdata:pointer; entry:PSDL_TrayEntry);cdecl;

function SDL_CreateTray(icon:PSDL_Surface; tooltip:Pchar):PSDL_Tray;cdecl;external libSDL3;
procedure SDL_SetTrayIcon(tray:PSDL_Tray; icon:PSDL_Surface);cdecl;external libSDL3;
procedure SDL_SetTrayTooltip(tray:PSDL_Tray; tooltip:Pchar);cdecl;external libSDL3;
function SDL_CreateTrayMenu(tray:PSDL_Tray):PSDL_TrayMenu;cdecl;external libSDL3;
function SDL_CreateTraySubmenu(entry:PSDL_TrayEntry):PSDL_TrayMenu;cdecl;external libSDL3;
function SDL_GetTrayMenu(tray:PSDL_Tray):PSDL_TrayMenu;cdecl;external libSDL3;
function SDL_GetTraySubmenu(entry:PSDL_TrayEntry):PSDL_TrayMenu;cdecl;external libSDL3;
function SDL_GetTrayEntries(menu:PSDL_TrayMenu; size:Plongint):^PSDL_TrayEntry;cdecl;external libSDL3;
procedure SDL_RemoveTrayEntry(entry:PSDL_TrayEntry);cdecl;external libSDL3;
function SDL_InsertTrayEntryAt(menu:PSDL_TrayMenu; pos:longint; _label:Pchar; flags:TSDL_TrayEntryFlags):PSDL_TrayEntry;cdecl;external libSDL3;
procedure SDL_SetTrayEntryLabel(entry:PSDL_TrayEntry; _label:Pchar);cdecl;external libSDL3;
function SDL_GetTrayEntryLabel(entry:PSDL_TrayEntry):Pchar;cdecl;external libSDL3;
procedure SDL_SetTrayEntryChecked(entry:PSDL_TrayEntry; checked:Tbool);cdecl;external libSDL3;
function SDL_GetTrayEntryChecked(entry:PSDL_TrayEntry):Tbool;cdecl;external libSDL3;
procedure SDL_SetTrayEntryEnabled(entry:PSDL_TrayEntry; enabled:Tbool);cdecl;external libSDL3;
function SDL_GetTrayEntryEnabled(entry:PSDL_TrayEntry):Tbool;cdecl;external libSDL3;
procedure SDL_SetTrayEntryCallback(entry:PSDL_TrayEntry; callback:TSDL_TrayCallback; userdata:pointer);cdecl;external libSDL3;
procedure SDL_DestroyTray(tray:PSDL_Tray);cdecl;external libSDL3;
function SDL_GetTrayEntryParent(entry:PSDL_TrayEntry):PSDL_TrayMenu;cdecl;external libSDL3;
function SDL_GetTrayMenuParentEntry(menu:PSDL_TrayMenu):PSDL_TrayEntry;cdecl;external libSDL3;
function SDL_GetTrayMenuParentTray(menu:PSDL_TrayMenu):PSDL_Tray;cdecl;external libSDL3;

implementation


end.
