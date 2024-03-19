# Übersetzen der C-Header
```
h2pas -p -T -d -c -e xxx.h
```

# Muss übewrarbeitet werden:
sdl_pixels


# Gröbere Änderungen
  sdl_dialogs.h neu

 include/SDL3/{SDL_rwops.h => SDL_iostream.h}                             | 646 +++++++++++++++++++++++++++++++++++---------------------------------------
 include/SDL3/SDL_oldnames.h                                              |  46 ++++--
 include/SDL3/SDL_storage.h 

