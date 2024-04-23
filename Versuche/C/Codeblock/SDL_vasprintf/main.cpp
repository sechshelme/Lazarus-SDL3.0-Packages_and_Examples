#include <SDL3/SDL.h>

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

void test(const char *fmt, ...)
{
    char* dyn_buf;

    printf("Demo asprintf:\n");
    const int written_1 = asprintf(&dyn_buf, "%s", fmt);
    printf("dyn_buf: \"%s\"; %i chars were written\n", dyn_buf, written_1);
    free(dyn_buf);

    printf("Demo vasprintf:\n");
    va_list args;
    va_start(args, fmt);
    const int written_2 = SDL_vasprintf(&dyn_buf, fmt, args);
    va_end(args);
    SDL_Log("data: %s", dyn_buf);
    SDL_free(dyn_buf);
}

int main(int argc, char *argv[])
{
    test("Testing... %d, %d, %d", 1, 2, 3);
char * s;
  //SDL_vasprintf(&s, "X: %s   Y: %s", &argv);
  //SDL_Log(s);
  //SDL_free(s);




  SDL_bool quit = SDL_FALSE;
  SDL_Window *window = SDL_CreateWindow("SDL_EVENT_WINDOW_RESIZED Example", 800, 600, SDL_WINDOW_RESIZABLE);

  SDL_Renderer *renderer = SDL_CreateRenderer(window, NULL, SDL_RENDERER_PRESENTVSYNC);


   while (!quit) {
   SDL_Event ev;
     while (SDL_PollEvent(&ev) != 0) {
      switch(ev.type) {
        case SDL_EVENT_QUIT:
          quit = SDL_TRUE;
        break;
        case SDL_EVENT_WINDOW_RESIZED:
          char * s;
          SDL_asprintf(&s, "X: %i   Y: %i", ev.window.data1, ev.window.data2);
          SDL_SetWindowTitle(window, s);
          SDL_free(s);
        break;
      }
    }
    SDL_SetRenderDrawColor(renderer, 128, 0, 0, 255);
    SDL_RenderClear(renderer);

    SDL_RenderPresent(renderer);
  }

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();
  return 0;
}
