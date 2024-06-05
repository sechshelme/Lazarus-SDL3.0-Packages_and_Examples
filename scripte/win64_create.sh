echo ======================== Windows 64bit DLL ========
mkdir SDL
cd SDL

echo ==================== SDL3 Basic
mkdir SDL3
cd SDL3
# git clone https://github.com/libsdl-org/SDL.git --recursive 
git clone https://github.com/libsdl-org/SDL.git

mkdir build_win64
cd build_win64
cmake -S ../SDL/ -B . -DCMAKE_CXX_COMPILER="/usr/bin/x86_64-w64-mingw32-g++" -DCMAKE_C_COMPILER="/usr/bin/x86_64-w64-mingw32-gcc" -DCMAKE_RC_COMPILER="/usr/bin/x86_64-w64-mingw32-windres" -DCMAKE_FIND_ROOT_PATH="/usr/x86_64-w64-mingw32" -DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE="BOTH" -DCMAKE_FIND_ROOT_PATH_MODE_LIBRARY="ONLY" -DCMAKE_FIND_ROOT_PATH_MODE_PROGRAM="BOTH" -DCMAKE_SYSTEM_NAME="Windows" 
make -j16
sudo make install
cd ../..

echo ====================  SDL3_mixer
mkdir SDL3_mixer
cd SDL3_mixer
# git clone https://github.com/libsdl-org/SDL_mixer.git --recursive 
git clone https://github.com/libsdl-org/SDL_mixer.git

mkdir build_win64
cd build_win64
cmake -S ../SDL_mixer -B . -DCMAKE_CXX_COMPILER="/usr/bin/x86_64-w64-mingw32-g++" -DCMAKE_C_COMPILER="/usr/bin/x86_64-w64-mingw32-gcc" -DCMAKE_RC_COMPILER="/usr/bin/x86_64-w64-mingw32-windres" -DCMAKE_FIND_ROOT_PATH="/usr/x86_64-w64-mingw32" -DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE="BOTH" -DCMAKE_FIND_ROOT_PATH_MODE_LIBRARY="ONLY" -DCMAKE_FIND_ROOT_PATH_MODE_PROGRAM="BOTH" -DCMAKE_SYSTEM_NAME="Windows"
make -j16
sudo make install
cd ../..

echo ====================  SDL3_net
mkdir SDL3_net
cd SDL3_net
# git clone https://github.com/libsdl-org/SDL_net.git --recursive 
git clone https://github.com/libsdl-org/SDL_net.git

mkdir build_win64
cd build_win64
cmake -S ../SDL_net -B . -DCMAKE_CXX_COMPILER="/usr/bin/x86_64-w64-mingw32-g++" -DCMAKE_C_COMPILER="/usr/bin/x86_64-w64-mingw32-gcc" -DCMAKE_RC_COMPILER="/usr/bin/x86_64-w64-mingw32-windres" -DCMAKE_FIND_ROOT_PATH="/usr/x86_64-w64-mingw32" -DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE="BOTH" -DCMAKE_FIND_ROOT_PATH_MODE_LIBRARY="ONLY" -DCMAKE_FIND_ROOT_PATH_MODE_PROGRAM="BOTH" -DCMAKE_SYSTEM_NAME="Windows"
make -j16
sudo make install
cd ../..

echo ====================  SDL3_image
mkdir SDL3_image
cd SDL3_image
# git clone https://github.com/libsdl-org/SDL_image.git --recursive 
git clone https://github.com/libsdl-org/SDL_image.git 

mkdir build_win64
cd build_win64
cmake -S ../SDL_image/ -B . -DCMAKE_CXX_COMPILER="/usr/bin/x86_64-w64-mingw32-g++" -DCMAKE_C_COMPILER="/usr/bin/x86_64-w64-mingw32-gcc" -DCMAKE_RC_COMPILER="/usr/bin/x86_64-w64-mingw32-windres" -DCMAKE_FIND_ROOT_PATH="/usr/x86_64-w64-mingw32" -DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE="BOTH" -DCMAKE_FIND_ROOT_PATH_MODE_LIBRARY="ONLY" -DCMAKE_FIND_ROOT_PATH_MODE_PROGRAM="BOTH" -DCMAKE_SYSTEM_NAME="Windows"
make -j16
sudo make install
cd ../..

echo ====================  SDL3_ttf
mkdir SDL3_ttf
cd SDL3_ttf
# git clone https://github.com/libsdl-org/SDL_ttf.git --recursive 
git clone https://github.com/libsdl-org/SDL_ttf.git

# Windows
mkdir build_win64
cd build_win64
cmake -S ../SDL_ttf -B . -DCMAKE_CXX_COMPILER="/usr/bin/x86_64-w64-mingw32-g++" -DCMAKE_C_COMPILER="/usr/bin/x86_64-w64-mingw32-gcc" -DCMAKE_RC_COMPILER="/usr/bin/x86_64-w64-mingw32-windres" -DCMAKE_FIND_ROOT_PATH="/usr/x86_64-w64-mingw32" -DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE="BOTH" -DCMAKE_FIND_ROOT_PATH_MODE_LIBRARY="ONLY" -DCMAKE_FIND_ROOT_PATH_MODE_PROGRAM="BOTH" -DCMAKE_SYSTEM_NAME="Windows"
make -j16
sudo make install
cd ../..

echo ====================  SDL3_rtf
mkdir SDL3_rtf
cd SDL3_rtf
# git clone https://github.com/libsdl-org/SDL_rtf.git --recursive 
git clone https://github.com/libsdl-org/SDL_rtf.git

mkdir build_win64
cd build_win64
cmake -S ../SDL_rtf -B . -DCMAKE_CXX_COMPILER="/usr/bin/x86_64-w64-mingw32-g++" -DCMAKE_C_COMPILER="/usr/bin/x86_64-w64-mingw32-gcc" -DCMAKE_RC_COMPILER="/usr/bin/x86_64-w64-mingw32-windres" -DCMAKE_FIND_ROOT_PATH="/usr/x86_64-w64-mingw32" -DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE="BOTH" -DCMAKE_FIND_ROOT_PATH_MODE_LIBRARY="ONLY" -DCMAKE_FIND_ROOT_PATH_MODE_PROGRAM="BOTH" -DCMAKE_SYSTEM_NAME="Windows"
make -j16
sudo make install
cd ../..

echo =======================  SDL2_comapat ==============================
mkdir SDL2-compat
cd SDL2-compat
# git clone https://github.com/libsdl-org/sdl2-compat.git --recursive 
git clone https://github.com/libsdl-org/sdl2-compat.git

# Linux
mkdir build_win64
cd build_win64
cmake -S ../sdl2-compat -B . -DCMAKE_CXX_COMPILER="/usr/bin/x86_64-w64-mingw32-g++" -DCMAKE_C_COMPILER="/usr/bin/x86_64-w64-mingw32-gcc" -DCMAKE_RC_COMPILER="/usr/bin/x86_64-w64-mingw32-windres" -DCMAKE_FIND_ROOT_PATH="/usr/x86_64-w64-mingw32" -DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE="BOTH" -DCMAKE_FIND_ROOT_PATH_MODE_LIBRARY="ONLY" -DCMAKE_FIND_ROOT_PATH_MODE_PROGRAM="BOTH" -DCMAKE_SYSTEM_NAME="Windows"
make -j16
sudo make install
cd ../..

echo ==== Ende ====

cd ..




