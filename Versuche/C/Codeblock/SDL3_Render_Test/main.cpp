#include <SDL3/SDL.h>

int main(int argc, char *argv[])
{
  SDL_Window *window = SDL_CreateWindow("test", 800, 600, 0);
  SDL_Renderer *renderer = SDL_CreateRenderer(window, NULL);


  SDL_Delay(3000);

  //SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_DestroyRenderer(renderer);

  SDL_Quit();
  return 0;
}
