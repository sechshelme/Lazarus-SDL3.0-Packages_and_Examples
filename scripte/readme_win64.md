# Einleitung

Die folgende Anleitung gilt, wen man den Istallations-Ordner von `CMAKE` auf default belässt.
**Als erstes diesen Ordner sichern.** `usr/local/` 


# SDL3_ttf.dll

Wen folgender Fehler kommt unteres befolgen.
```
CMake Error at /usr/share/cmake-3.22/Modules/FindPackageHandleStandardArgs.cmake:230 (message):
  Could NOT find Freetype (missing: FREETYPE_LIBRARY) (found version
  "2.11.1")
Call Stack (most recent call first):
  /usr/share/cmake-3.22/Modules/FindPackageHandleStandardArgs.cmake:594 (_FPHSA_FAILURE_MESSAGE)
  /usr/share/cmake-3.22/Modules/FindFreetype.cmake:162 (find_package_handle_standard_args)
  CMakeLists.txt:253 (find_package)
```

Das neuste FreeType runterladen.

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

















