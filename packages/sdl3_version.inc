/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 06-07-2024   17:47                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_Version = ^TSDL_Version;

  TSDL_Version = record
    major: uint8;
    minor: uint8;
    patch: uint8;
  end;

const
  SDL_MAJOR_VERSION = 3;
  SDL_MINOR_VERSION = 0;
  SDL_PATCHLEVEL = 0;

procedure SDL_VERSION(x: PSDL_Version);
function SDL_VERSIONNUM(X, Y, Z: longint): longint;
function SDL_COMPILEDVERSION: longint;
function SDL_VERSION_ATLEAST(X, Y, Z: longint): TSDL_bool;

function SDL_GetVersion(ver: PSDL_Version): longint; cdecl; external sdl3_lib;
function SDL_GetRevision: PChar; cdecl; external sdl3_lib;

{$ENDIF read_interface}


{$IFDEF read_implementation}

procedure SDL_VERSION(x: PSDL_Version);
begin
  x^.major := SDL_MAJOR_VERSION;
  x^.minor := SDL_MINOR_VERSION;
  x^.patch := SDL_PATCHLEVEL;
end;

function SDL_VERSIONNUM(X, Y, Z: longint): longint;
begin
  SDL_VERSIONNUM := ((X shl 24) or (Y shl 8)) or (Z shl 0);
end;

function SDL_COMPILEDVERSION: longint;
begin
  SDL_COMPILEDVERSION := SDL_VERSIONNUM(SDL_MAJOR_VERSION, SDL_MINOR_VERSION, SDL_PATCHLEVEL);
end;

function SDL_VERSION_ATLEAST(X, Y, Z: longint): TSDL_bool;
begin
  SDL_VERSION_ATLEAST := TSDL_bool(SDL_COMPILEDVERSION >= (SDL_VERSIONNUM(X, Y, Z)));
end;

{$ENDIF read_implementation}
