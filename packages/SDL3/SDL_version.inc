/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   19:37                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

const
  SDL_MAJOR_VERSION = 3;
  SDL_MINOR_VERSION = 1;
  SDL_MICRO_VERSION = 3;

function SDL_VERSIONNUM(major, minor, patch: longint): longint;
function SDL_VERSIONNUM_MAJOR(version: longint): longint;
function SDL_VERSIONNUM_MINOR(version: longint): longint;
function SDL_VERSIONNUM_MICRO(version: longint): longint;
function SDL_VERSION: longint;
function SDL_VERSION_ATLEAST(X, Y, Z: longint): boolean;

function SDL_GetVersion: longint; cdecl; external libSDL3;
function SDL_GetRevision: pansichar; cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}

function SDL_VERSIONNUM(major, minor, patch: longint): longint;
begin
  SDL_VERSIONNUM := ((major * 1000000) + (minor * 1000)) + patch;
end;

function SDL_VERSIONNUM_MAJOR(version: longint): longint;
begin
  SDL_VERSIONNUM_MAJOR := version div 1000000;
end;

function SDL_VERSIONNUM_MINOR(version: longint): longint;
begin
  SDL_VERSIONNUM_MINOR := (version div 1000) mod 1000;
end;

function SDL_VERSIONNUM_MICRO(version: longint): longint;
begin
  SDL_VERSIONNUM_MICRO := version mod 1000;
end;

function SDL_VERSION: longint;
begin
  SDL_VERSION := SDL_VERSIONNUM(SDL_MAJOR_VERSION, SDL_MINOR_VERSION, SDL_MICRO_VERSION);
end;

function SDL_VERSION_ATLEAST(X, Y, Z: longint): boolean;
begin
  SDL_VERSION_ATLEAST := SDL_VERSION >= (SDL_VERSIONNUM(X, Y, Z));
end;


{$ENDIF read_implementation}
