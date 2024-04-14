# Übersetzen der C-Header
```
h2pas -p -T -d -c -e xxx.h
```

# Spezielle Behandlungen:
## sdl_pixels.h 
Die *.pp mit folgendem Tool bearbeiten "convert_SDL_pixels.pp", wegen den Makros.

## sdl3_log.pas
Folgendes ergänzen:
```pascal
procedure SDL_Log(fmt: PChar); varargs; cdecl; external name 'SDL_Log';
```

## sdl3_stdinc.pas
Folgendes ergänzen:
```pascal
type
  TUint8  = uint8;
  TUint16 = uint16;
  TUint32 = uint32;
  TUint64 = uint64;

  Tint8  = uint8;
  Tint16 = uint16;
  Tint32 = uint32;
  Tint64 = uint64;

  Tsize_t = SizeInt;
//  Tsize_t = SizeUInt;
  Tuintptr_t = PtrUInt;
                               

  Psize_t=^Tsize_t;

  Twchar_t = word;
  Pwchar_t = ^Twchar_t;
  PPwchar_t = ^Pwchar_t;

  PSDL_iconv_data_t = Pointer;
  Tintptr_t = Pointer;

  PPUint8 = ^PUint8;

const
  SDL_FALSE = False;  
  SDL_TRUE = True;  
type
  PSDL_bool = ^TSDL_bool;
  TSDL_bool = boolean32;


// modifizieren
function SDL_log(x: cdouble): cdouble; cdecl; external name 'SDL_log';
```



# Gröbere Änderungen
  sdl_dialogs.h neu

 include/SDL3/{SDL_rwops.h => SDL_iostream.h}                             | 646 +++++++++++++++++++++++++++++++++++---------------------------------------
 include/SDL3/SDL_oldnames.h                                              |  46 ++++--
 include/SDL3/SDL_storage.h 

include/SDL3/SDL_time.h                             | 208 +++++++++++++++++++++++++++++++++++++++
 include/SDL3/SDL_filesystem.h                       |  85 +++++++++++++++-



 include/SDL3/SDL_filesystem.h         |  49 +++++++++++++++++++-
 include/SDL3/SDL_storage.h            |  43 +++++++++++++++++-
 src/dynapi/SDL_dynapi.sym             |   2 +

SDL_renderer.h




