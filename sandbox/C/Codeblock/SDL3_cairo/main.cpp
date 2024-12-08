#include <SDL3/SDL.h>
#include <cairo/cairo.h>
#include <cairo/cairo-gobject.h>

void CairoDraw(SDL_Texture *texture, int w, int h){
  void *pixels;
  int pitch;
  SDL_LockTexture(texture, nullptr, &pixels, &pitch);
  cairo_surface_t *surface = cairo_image_surface_create_for_data((char unsigned*)pixels, CAIRO_FORMAT_RGB24, w, h, pitch);
  cairo_t *context = cairo_create(surface);

  cairo_set_source_rgba(context, 1.0, 0.0, 0.0, 1.0);
  cairo_arc(context, w / 2, h / 2, w / 4, 0, SDL_PI_D * 2);
  cairo_stroke(context);

  cairo_destroy(context);
  cairo_surface_destroy(surface);
  SDL_UnlockTexture(texture);
}

int main(int argc, char *argv[])
{
    int Width = 320;
    int Height = 200;
    SDL_Init(SDL_INIT_VIDEO);
    SDL_Window *win = SDL_CreateWindow("Test", 320, 200, SDL_WINDOW_RESIZABLE);
    SDL_Renderer *renderer = SDL_CreateRenderer(win, nullptr);

    SDL_Texture *texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_STREAMING, Width, Height);

    SDL_bool quit = SDL_FALSE;
    while (!quit)
    {
        SDL_Event event;
        while (SDL_PollEvent(&event))
        {
            switch (event.type)
            {
                case SDL_EVENT_QUIT:
                    quit = SDL_TRUE;
                    break;
                case SDL_EVENT_WINDOW_RESIZED:
                    SDL_DestroyTexture(texture);
                    SDL_GetWindowSize(win, &Width, &Height);
                    texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_STREAMING, Width, Height);
                    break;
            }
        }
        CairoDraw(texture, Width, Height);

        SDL_RenderTexture(renderer, texture, nullptr, nullptr);
        SDL_RenderPresent(renderer);
    }

    SDL_DestroyTexture(texture);
    SDL_DestroyWindow(win);
    SDL_DestroyRenderer(renderer);
    SDL_Quit();

    return 0;
}
