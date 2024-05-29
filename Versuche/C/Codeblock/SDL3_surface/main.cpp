#include <SDL3/SDL.h>

void printSurface(SDL_Surface *src) {
  unsigned char *ch ;
  ch = (unsigned char*) src->pixels;
  SDL_Log("Pixel: %2X %2X %2X %2X ", ch[0], ch[1], ch[2], ch[3]);
}

int main(int argc, char *argv[])
{
    uint8_t pixels_1[] = {0xFF, 0xFF, 0x00, 0xFF};
    uint32_t pixels_2[] = {0xFFFF00FF};

    SDL_Surface *sur1 = SDL_CreateSurfaceFrom(&pixels_1, 1, 1, 8, SDL_PIXELFORMAT_BGR24);
    SDL_Surface *sur2 = SDL_ConvertSurfaceFormat(sur1, SDL_PIXELFORMAT_RGB24);

    SDL_DestroySurface(sur2);
    SDL_DestroySurface(sur1);



    SDL_Log("log");
    SDL_bool loopShouldStop = SDL_FALSE;

    SDL_Init(SDL_INIT_VIDEO);

    SDL_Window *win = SDL_CreateWindow("Hello World", 320, 200, SDL_WINDOW_RESIZABLE);
    SDL_Surface *winSurface = SDL_GetWindowSurface(win);

    SDL_Surface *SrcSurface = SDL_CreateSurface(1, 1, SDL_PIXELFORMAT_BGR24);
    SrcSurface->pixels = &pixels_1;
    printSurface(SrcSurface);

    SDL_Surface *DestSurface24 = SDL_ConvertSurfaceFormat(SrcSurface, SDL_PIXELFORMAT_RGB24);
    printSurface(DestSurface24);

    SDL_Surface *Surf1 = SDL_CreateSurface(1, 1, SDL_PIXELFORMAT_RGBA32);
    SDL_memcpy(Surf1->pixels, &pixels_1, sizeof(pixels_1));
    printSurface(Surf1);

    // warped
    SDL_Surface *Surf2 = SDL_CreateSurface(1, 1, SDL_PIXELFORMAT_RGBA32);
    SDL_memcpy(Surf2->pixels, &pixels_2, sizeof(pixels_2));
    printSurface(Surf2);

    // io.
    SDL_Surface *Surf3 = SDL_CreateSurface(1, 1, SDL_PIXELFORMAT_RGBA8888);
    SDL_memcpy(Surf3->pixels, &pixels_2, sizeof(pixels_2));
    printSurface(Surf3);

    while (!loopShouldStop)
    {
        SDL_Event event;
        while (SDL_PollEvent(&event))
        {
            switch (event.type)
            {
                case SDL_EVENT_QUIT:
                    loopShouldStop = SDL_TRUE;
                    break;
                case SDL_EVENT_KEY_DOWN:
                    SDL_Log("Key: %i", event.key.keysym.sym);
                    break;
            }
        }

        SDL_Rect r = {10, 10, 30, 30};
//        SDL_BlitSurfaceScaled(SrcSurface, nullptr, winSurface, &r, SDL_SCALEMODE_NEAREST);

        r = {50, 10, 30, 30};
        //SDL_BlitSurfaceScaled(DestSurface24, nullptr, winSurface, &r, SDL_SCALEMODE_NEAREST);

        r = {10, 100, 30, 30};
        SDL_BlitSurfaceScaled(Surf1, nullptr, winSurface, &r, SDL_SCALEMODE_NEAREST);

        r = {50, 100, 30, 30};
        SDL_BlitSurfaceScaled(Surf2, nullptr, winSurface, &r, SDL_SCALEMODE_NEAREST);

        r = {90, 100, 30, 30};
        SDL_BlitSurfaceScaled(Surf3, nullptr, winSurface, &r, SDL_SCALEMODE_NEAREST);

        SDL_UpdateWindowSurface(win);
    }

    SDL_DestroyWindow(win);
    SDL_DestroySurface(winSurface);
    SDL_DestroySurface(SrcSurface);
    SDL_DestroySurface(DestSurface24);
    SDL_DestroySurface(Surf1);
    SDL_DestroySurface(Surf2);
    SDL_DestroySurface(Surf3);

    SDL_Quit();

    return 0;
}
