# Einleitung

Die folgende Anleitung gilt, wen man den Istallations-Ordner von `CMAKE` auf default belässt.
**Als erstes diesen Ordner sichern.** `usr/local/` 
- Einen beliebigen Ordner für die Sourcen erstellen, Leerzeichen im Pfad sind zu vermeiden.

# FreeType bauen
FreeType ist die Voraussetzung, das man `SDL3_ttf.dll` bauen kann.
- In den Sourcen Ordner wechseln ein Ordner `freetype` erstellen und in diesen wechseln.
- Dort eine Datei `win64_cross,txt` mit folgendem Inhalt erstellen: 
```ini
[binaries]
c = 'x86_64-w64-mingw32-gcc'
cpp = 'x86_64-w64-mingw32-g++'
ar = 'x86_64-w64-mingw32-ar'
strip = 'x86_64-w64-mingw32-strip'
exe_wrapper = 'wine64'
windres = 'x86_64-w64-mingw32-windres'

[host_machine]
system = 'windows'
cpu_family = 'x86_64'
cpu = 'x86_64'
endian = 'little'
```
- Dann folgende Komandos ausführen um die FreeType DLLs zu bauen 
```bash
git clone https://github.com/freetype/freetype.git
mkdir win64_build 
cd win64_build 
meson setup --cross-file ../cross.txt ../freetype/
ninja -j16 # Je nach CPU-Kerne
ninja install
```


# SDL3_ttf.dll

Folgender Fehler kommt wen man FreeType nicht installiert hat.
```
CMake Error at /usr/share/cmake-3.22/Modules/FindPackageHandleStandardArgs.cmake:230 (message):
  Could NOT find Freetype (missing: FREETYPE_LIBRARY) (found version
  "2.11.1")
Call Stack (most recent call first):
  /usr/share/cmake-3.22/Modules/FindPackageHandleStandardArgs.cmake:594 (_FPHSA_FAILURE_MESSAGE)
  /usr/share/cmake-3.22/Modules/FindFreetype.cmake:162 (find_package_handle_standard_args)
  CMakeLists.txt:253 (find_package)
```

cmake-gui installieren
```bash
sudo apt install cmake-qt-gui
```

In den Ordner mit `.../SDL/SDL3_ttf/build_win64` wechseln.
Doppelklick auf `CMakeCache.txt` , dies sollte es dannn mit `cmake-gui` öffnen.

Folgende Pfade ändern:
- FREETYPE_INCLUDE_DIR_freetype `usr/include/freetype2` -> `/usr/local/include/freetype2`
- FREETYPE_INCLUDE_DIR_ft2build `usr/include/freetype2` -> `/usr/local/include/freetype2`

- `[Generate]` klicken.
- Tool beenden.
- In der Konsole `make`/`make install`, dann sollte die `SDL3_ttf.dll` gebaut werden.

# SDL3_rtf.dll
In den Ordner mit `.../SDL/SDL3_ttf/build_win64` wechseln.
Doppelklick auf `CMakeCache.txt` , dies sollte es dannn mit `cmake-gui` öffnen.

Folgende Pfad ändern:
- SDL3_ttf_DIR `SDL3_ttf_DIR-NOTFOUND` -> `/usr/local/lib/cmake/SDL3_ttf`

- `[Generate]` klicken.
- Tool beenden.
- In der Konsole `make`/`make install`, dann sollte die `SDL3_rtf.dll` gebaut werden.

















