#include <SDL3/SDL.h>


int main(int argc, char *argv[])
{
  SDL_bool quit = SDL_FALSE;
  SDL_Window *window = SDL_CreateWindow("Triangle Example", 800, 600, 0);

  SDL_Renderer *renderer = SDL_CreateRenderer(window, NULL, SDL_RENDERER_PRESENTVSYNC);

  SDL_Vertex vert[3];

  // center
  vert[0].position.x = 400;
  vert[0].position.y = 150;
  vert[0].color.r = 255;
  vert[0].color.g = 0;
  vert[0].color.b = 0;
  vert[0].color.a = 255;

  // left
  vert[1].position.x = 200;
  vert[1].position.y = 450;
  vert[1].color.r = 0;
  vert[1].color.g = 0;
  vert[1].color.b = 255;
  vert[1].color.a = 255;

  // right
  vert[2].position.x = 600;
  vert[2].position.y = 450;
  vert[2].color.r = 0;
  vert[2].color.g = 255;
  vert[2].color.b = 0;
  vert[2].color.a = 255;

  while (!quit) {
   SDL_Event ev;
   while (SDL_PollEvent(&ev) != 0) {
      switch(ev.type) {
        case SDL_EVENT_QUIT:
        quit = SDL_TRUE;
        break;
      }
    }
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);

    SDL_RenderGeometry(renderer, NULL, vert, 3, NULL, 0);

    SDL_RenderPresent(renderer);
  }

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();
  return 0;
}
