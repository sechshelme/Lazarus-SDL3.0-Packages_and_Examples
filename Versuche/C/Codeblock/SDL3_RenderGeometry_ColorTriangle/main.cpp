#include <SDL3/SDL.h>
#include <SDL3_image/SDL_image.h>

int main(int argc, char *argv[])
{

  SDL_bool quit = SDL_FALSE;
  SDL_Window *window = SDL_CreateWindow("Triangle Example", 800, 600, 0);

  SDL_Renderer *renderer = SDL_CreateRenderer(window, NULL);

  #define vertLen 3
  SDL_Vertex vert[vertLen];

  // center
  vert[0].position.x = 400;
  vert[0].position.y = 150;
  vert[0].color.r = 1.0;
  vert[0].color.g = 0.0;
  vert[0].color.b = 0.0;
  vert[0].color.a = 1.0;

  // left
  vert[1].position.x = 200;
  vert[1].position.y = 450;
  vert[1].color.r = 0.0;
  vert[1].color.g = 0.0;
  vert[1].color.b = 1.0;
  vert[1].color.a = 1.0;

  // right
  vert[2].position.x = 600;
  vert[2].position.y = 450;
  vert[2].color.r = 0.0;
  vert[2].color.g = 1.0;
  vert[2].color.b = 0.0;
  vert[2].color.a = 1.0;

   SDL_SetEventEnabled(SDL_EVENT_MOUSE_BUTTON_DOWN, 034);
   SDL_Log("%i", SDL_FALSE );
   SDL_Log("%i", SDL_TRUE );
   SDL_Log("%i", !SDL_FALSE );
   SDL_Log("%i", !SDL_TRUE );
   SDL_Log("%i", !0 );
   SDL_Log("%i", !1 );
   while (!quit) {
   SDL_Event ev;
     while (SDL_PollEvent(&ev) != 0) {
      switch(ev.type) {
        case SDL_EVENT_QUIT:
          quit = SDL_TRUE;
        break;
        case SDL_EVENT_MOUSE_BUTTON_DOWN:
          SDL_Log("down");
        break;
      }
    }
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);

    SDL_RenderGeometry(renderer, NULL, vert, vertLen, NULL, 0);

    SDL_RenderPresent(renderer);
  }

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();
  return 0;
}
