# Einleitung
- Dies ist ein SDL3.1.10 - Header für FPC/Lazarus

# Neuerungen:
| Datum      | Änderungen 
| ---        | ---
| 24.01.2025 | Angepasst an SDL_image-prerelease-3.1.1
| 22.01.2025 | Angepasst an SDL-release-3.2.0
| 17.01.2025 | Angepasst an SDL-prerelease-3.1.10
| 08.01.2025 | Angepasst an SDL-preview-3.1.8
| 08.12.2024 | Angepasst an SDL_image-preview-3.1.0
| 03.11.2024 | Angepasst an SDL-preview-3.1.6
| 22.10.2024 | Angepasst an SDL-preview-3.1.3
| 19.03.2024 | Eröfffnung des Repository

# Installation
- Passendes Paket runterladen [SDL 3.1.3](https://github.com/libsdl-org/SDL/releases/tag/preview-3.1.3)

## Linux
- Folgende Anleitung beachten: [Readme](https://github.com/libsdl-org/SDL/blob/main/docs/README-cmake.md)

## Windows
- Die DLLs sind hier enthalten [SDL3](https://github.com/libsdl-org/SDL/releases/download/release-2.30.8/SDL2-2.30.8-win32-x64.zip) und muss nach `C:\windows\system32` kopiert werden.

## Manuelle installation
SDL3 lib installieren:
- [Installation](install_sdl.md) SDL3 für Lazarus
- Package [SDL3](packages) installieren.

# Examples
- [Examples](examples)

# Diverses
Eventuell muss folgends für `SDL_mix` installiert werden:
- `sudo apt install fluidsynth`


# Speicherleek
Wen folgendes ein Speicherleek anzeigt `valgrind --leak-check=full ./main`, muss folgendes im Code eingefügt werden.
```
SDL_SetHint(SDL_HINT_SHUTDOWN_DBUS_ON_QUIT, "1");
```

Genauer testen `valgrind --leak-check=full --trace-children=yes ./main`





