#include <SDL3/SDL.h>

void printFormat(SDL_Surface *sur){
  SDL_Log("format: %u", sur->format->format);
  SDL_Log("bit per pixel: %u    bytes per Pixel: %u", sur->format->bits_per_pixel, sur->format->bytes_per_pixel);
  SDL_Log("Rmask:  %08X   Gmask:  %08X   Bmask:  %08X   Amask:  %08X   ", sur->format->Rmask, sur->format->Gmask, sur->format->Bmask, sur->format->Amask);
  SDL_Log("Rshift: %u   Gshift: %u   Bshift: %u   Ashift: %u   ", sur->format->Rshift, sur->format->Gshift, sur->format->Bshift, sur->format->Ashift);
  SDL_Log("Rloss: %u   Gloss: %u   Bloss: %u   Aloss: %u\n\n", sur->format->Rloss, sur->format->Gloss, sur->format->Bloss, sur->format->Aloss);
}

int main(int argc, char *argv[])
{
    SDL_bool loopShouldStop = SDL_FALSE;
    SDL_Init(SDL_INIT_VIDEO);

    SDL_Window *win = SDL_CreateWindow("Keyboard", 640, 480, SDL_WINDOW_RESIZABLE);

    SDL_Surface *winSurface = SDL_GetWindowSurface(win);
    SDL_Surface *BMP_Surface = SDL_LoadBMP("mauer.bmp");
    if (!BMP_Surface) {
      SDL_Log("BMP nicht gefunden !  (%s)", SDL_GetError());
    } else {
      printFormat(BMP_Surface);
    }

    SDL_Surface *surfaceTexture = SDL_ConvertSurfaceFormat(BMP_Surface, SDL_PIXELFORMAT_RGBA32);
    printFormat(surfaceTexture);


    SDL_Event event;

    while (!loopShouldStop)
    {
        while (SDL_PollEvent(&event))
        {
            switch (event.type)
            {
                case SDL_EVENT_KEY_DOWN:
                    switch (event.key.keysym.sym)
                    {
                       case SDLK_ESCAPE:
                       loopShouldStop = SDL_TRUE;
                       break;
                    }
                    break;
                case SDL_EVENT_QUIT:
                    loopShouldStop = SDL_TRUE;
                    break;
            }
        }

        SDL_Rect rDest = {100,100,200,200};

        SDL_BlitSurfaceScaled(BMP_Surface, nullptr, winSurface, &rDest, SDL_SCALEMODE_NEAREST);
        SDL_BlitSurface(BMP_Surface, nullptr, winSurface, nullptr);
        SDL_UpdateWindowSurface(win);
    }

    SDL_DestroySurface(BMP_Surface);
    SDL_DestroySurface(winSurface);
    SDL_DestroyWindow(win);

    SDL_Quit();

    return 0;
}
