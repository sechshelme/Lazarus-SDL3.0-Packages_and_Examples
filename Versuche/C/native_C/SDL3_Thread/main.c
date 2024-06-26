/*
  Linux:
  gcc main.c -o main -lSDL3

  Windows:
  x86_64-w64-mingw32-gcc main.c -o main.exe -lSDL3 -I/usr/local/include -L/usr/local/bin
*/

#include <SDL3/SDL.h>
#include <stdio.h>

int TestThread(void * Data)
{
  int i;
  for (i=0;i<8;i++){
    SDL_Log("%i", i);
    SDL_Delay(50);
  }
  return i;
}

#define len 8

int main(int argc, char* argv[]) {
  SDL_Thread * thread[len];
  int threadReturnValue;

  SDL_Init(SDL_INIT_VIDEO);
  SDL_Log("Simple SDL_CreateThread test:");

  for (int i=0; i<len; i++){
    thread[i] = SDL_CreateThread(&TestThread,"TestThread", NULL);
    if (!thread[i]) {
      SDL_LogError(0, "Thread Error: %i", SDL_GetError());
      return 1;
    }
  }

  for (int i=0; i<len; i++){
    SDL_WaitThread(thread[i], &threadReturnValue);
    SDL_Log("Thread returned value: %i", threadReturnValue);
  }

  SDL_Log("Ende");
  SDL_Quit();
  return 0;
}
