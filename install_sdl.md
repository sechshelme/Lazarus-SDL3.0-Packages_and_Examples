# SDL3 herunterladen

git clone https://github.com/libsdl-org/SDL.git


# cmake -S . -B build && cmake --build build && cmake --install build

cmake -S . -B build
cmake --build build

su
cmake --install build --prefix=/usr/local

#Libs aktualisieren
sudo ldconfig


