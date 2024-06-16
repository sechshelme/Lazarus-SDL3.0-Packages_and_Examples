/*
  Linux:
  gcc main.c -o main -lSDL2

  Windows:
  Folgende Datei muss im Stamm-Ordner des Projektes sein libSDL2main.a
  x86_64-w64-mingw32-gcc main.c -o main.exe -lmingw32 -lSDL2main -lSDL2 -mwindows -I/usr/local/include -L/usr/local/bin -L.
*/

#include <SDL2/SDL.h>
#include <SDL2/SDL_main.h>

int main(int argc, char *argv[]) {
    SDL_Init(SDL_INIT_VIDEO);   
    SDL_Window *window = SDL_CreateWindow("An SDL2 window", 0, 0, 640, 480, SDL_WINDOW_OPENGL);
    SDL_Delay(3000);  
    SDL_DestroyWindow(window);
    SDL_Quit();
    return 0;
}

