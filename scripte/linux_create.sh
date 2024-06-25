echo =========================== Linux 64bit *.so ========
mkdir SDL
cd SDL

echo ======================= SDL3 Basic ==============================
mkdir SDL3
cd SDL3
git clone https://github.com/libsdl-org/SDL.git

# Linux
mkdir build
cd build
cmake ../SDL -DSDL_TESTS=ON
make -j16
sudo make install
cd ../..

echo =======================  SDL3_mixer ==============================
mkdir SDL3_mixer
cd SDL3_mixer
git clone https://github.com/libsdl-org/SDL_mixer.git

# Linux
mkdir build
cd build
cmake ../SDL_mixer
make -j16
sudo make install
cd ../..

echo =======================  SDL3_net ==============================
mkdir SDL3_net
cd SDL3_net
git clone https://github.com/libsdl-org/SDL_net.git

# Linux
mkdir build
cd build
cmake ../SDL_net
make -j16
sudo make install
cd ../..

echo =======================  SDL3_image ==============================
mkdir SDL3_image
cd SDL3_image
git clone https://github.com/libsdl-org/SDL_image.git

# Linux
mkdir build
cd build
cmake ../SDL_image
make -j16
sudo make install
cd ../..

echo =======================  SDL3_ttf ==============================
mkdir SDL3_ttf
cd SDL3_ttf
git clone https://github.com/libsdl-org/SDL_ttf.git

# Linux
mkdir build
cd build
cmake ../SDL_ttf
make -j16
sudo make install
cd ../..

echo =======================  SDL3_rtf ==============================
mkdir SDL3_rtf
cd SDL3_rtf
git clone https://github.com/libsdl-org/SDL_rtf.git

# Linux
mkdir build
cd build
cmake ../SDL_rtf
make -j16
sudo make install
cd ../..

echo =======================  SDL2_comapat ==============================
mkdir SDL2-compat
cd SDL2-compat
git clone https://github.com/libsdl-org/sdl2-compat.git

# Linux
mkdir build
cd build
cmake ../sdl2-compat
make -j16
sudo make install
cd ../..

echo ==== Ende ====

cd ..




