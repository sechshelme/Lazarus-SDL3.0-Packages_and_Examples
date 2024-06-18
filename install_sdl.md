# Installation von SDL3

## Linux
Ordner erstellen, zB. ~/SDL3 und in diesen wechseln.

SDL3 herunterladen und installieren.
Folgendes ausführen:
```bash
git clone https://github.com/libsdl-org/SDL.git
mkdir build
cd build
cmake ../SDL
make -j16
sudo make install
```

### Test-Programme mitkompilieren
```bash
cmake -S ../SDL -DSDL_TESTS=ON
```
https://github.com/libsdl-org/SDL/blob/main/docs/README-cmake.md

## Windows Cross-Compiler

### Installieren
```bash
sudo apt-get install gcc-mingw-w64
sudo apt-get install mingw-w64-x86-64-dev 

sudo apt install g++-mingw-w64-x86-64-posix 
sudo apt install g++-mingw-w64-x86-64-win32 

```




### SDL3.DLL für wine und Windows erstellen.
```bash
git clone https://github.com/libsdl-org/SDL.git
mkdir build_win64
cd build_win64
cmake -S ../SDL/ -B . -DCMAKE_CXX_COMPILER="/usr/bin/x86_64-w64-mingw32-g++" -DCMAKE_C_COMPILER="/usr/bin/x86_64-w64-mingw32-gcc" -DCMAKE_RC_COMPILER="/usr/bin/x86_64-w64-mingw32-windres" -DCMAKE_FIND_ROOT_PATH="/usr/x86_64-w64-mingw32" -DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE="BOTH" -DCMAKE_FIND_ROOT_PATH_MODE_LIBRARY="ONLY" -DCMAKE_FIND_ROOT_PATH_MODE_PROGRAM="BOTH" -DCMAKE_SYSTEM_NAME="Windows"
make -j16
sudo make install
```

### SDL3_image.DLL für wine und Windows erstellen.
```bash
git clone https://github.com/libsdl-org/SDL_image.git
mkdir build_win64
cd build_win64
cmake -S ../SDL_image/ -B . -DCMAKE_C_COMPILER="/usr/bin/x86_64-w64-mingw32-gcc" -DCMAKE_RC_COMPILER="/usr/bin/x86_64-w64-mingw32-windres" -DCMAKE_FIND_ROOT_PATH="/usr/x86_64-w64-mingw32" -DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE="BOTH" -DCMAKE_FIND_ROOT_PATH_MODE_LIBRARY="ONLY" -DCMAKE_FIND_ROOT_PATH_MODE_PROGRAM="BOTH" -DCMAKE_SYSTEM_NAME="Windows"
make -j16
sudo make install
```


# Alternatives
```bash
cmake -S . -B build && cmake --build build && cmake --install build
cmake -S . -B build
cmake --build build

su
cmake --install build --install-prefix /usr/local
```

# Libs aktualisieren
```bash
sudo ldconfig
```

#CMAKE-Tools
```bash
sudo apt install cmake-qt-gui

make edit_cache

sudo apt install cmake-curses-gui 
ccmake .
```





