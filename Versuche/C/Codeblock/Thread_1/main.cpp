#include <SDL3/SDL.h>


int TestThread(void * Data)
{
  int i;
  for (i=0;i<8;i++){
    SDL_Log("%i", i);
    SDL_Delay(50);
  }
  return i;
}

int main(int argc, char *argv[])
{
  SDL_Thread * thread = nullptr;
  int threadReturnValue;

  SDL_Init(SDL_INIT_VIDEO);
  SDL_Log("Simple SDL_CreateThread test:");


  thread = SDL_CreateThread(&TestThread,"TestThread", nullptr);
  if (!thread) {
    SDL_Log("Thread Error: %i", SDL_GetError());
  } else {
    SDL_WaitThread(thread, &threadReturnValue);
    SDL_Log("Thread returned value: %i", threadReturnValue);
  }

  SDL_Log("Ende");
  SDL_Quit();
  return 0;
}
