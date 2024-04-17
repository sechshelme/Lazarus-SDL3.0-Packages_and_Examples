/*
  Linux:
  gcc main.c -o main -lSDL3 -lSDL3_image

  Windows: 
  x86_64-w64-mingw32-gcc main.c -o main.exe -lSDL3 -lSDL3_image -I/usr/local/include -L/usr/local/bin
*/

#include <SDL3/SDL.h>
#include <SDL3_image/SDL_image.h>

int WinMain(int argc, char* argv[]) {
    SDL_Log("Start");
    SDL_Init(SDL_INIT_VIDEO);

    int flag = 0;
    IMG_Init(flag);

    SDL_Delay(5000);

    SDL_Log("Ende");
    return 0;
}

int main(int argc, char* argv[]) {
  return WinMain(argc, argv);
}
