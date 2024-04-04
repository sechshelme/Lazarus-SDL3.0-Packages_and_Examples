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

### SDL3.DLL für wine und Windows erstellen.
```bash
git clone https://github.com/libsdl-org/SDL.git
mkdir build_win64
cd build_win64
cmake -S ../SDL/ -B . -DCMAKE_CXX_COMPILER="/usr/bin/x86_64-w64-mingw32-g++" -DCMAKE_C_COMPILER="/usr/bin/x86_64-w64-mingw32-gcc" -DCMAKE_RC_COMPILER="/usr/bin/x86_64-w64-mingw32-windres" -DCMAKE_FIND_ROOT_PATH="/usr/x86_64-w64-mingw32" -DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE="BOTH" -DCMAKE_FIND_ROOT_PATH_MODE_LIBRARY="ONLY" -DCMAKE_FIND_ROOT_PATH_MODE_PROGRAM="BOTH" -DCMAKE_SYSTEM_NAME="Windows" --install-prefix /home/tux/Schreibtisch/von_Git/SDL3/build_win64/make
make -j16
```




# Alternatives
cmake -S . -B build && cmake --build build && cmake --install build
cmake -S . -B build
cmake --build build

su
cmake --install build --prefix=/usr/local

#Libs aktualisieren
sudo ldconfig


