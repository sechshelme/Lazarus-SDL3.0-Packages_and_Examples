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




