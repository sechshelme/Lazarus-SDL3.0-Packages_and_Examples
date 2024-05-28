#include <SDL3/SDL.h>

void printSurface(SDL_Surface *src) {
  unsigned char *ch ;
  ch = (unsigned char*) src->pixels;
  SDL_Log("Pixel: %2X %2X %2X %2X ", ch[0], ch[1], ch[2], ch[3]);
}

int main(int argc, char *argv[])
{
SDL_Log("log");
    SDL_bool loopShouldStop = SDL_FALSE;

    SDL_Init(SDL_INIT_VIDEO);

    SDL_Window *win = SDL_CreateWindow("Hello World", 320, 200, SDL_WINDOW_RESIZABLE);
    SDL_Surface *winSurface = SDL_GetWindowSurface(win);

    SDL_Surface *SrcSurface = SDL_CreateSurface(1, 1, SDL_PIXELFORMAT_BGR24);
    unsigned char pixels[] = {0xFF, 0x00, 0x00, 0x00};
    SrcSurface->pixels = &pixels;
    printSurface(SrcSurface);

    SDL_Surface *DestSurface24 = SDL_ConvertSurfaceFormat(SrcSurface, SDL_PIXELFORMAT_RGB24);
    printSurface(DestSurface24);

    SDL_Surface *DestSurface32 = SDL_ConvertSurfaceFormat(SrcSurface, SDL_PIXELFORMAT_RGBA32);
    printSurface(DestSurface32);

//    DestSurface32 = SDL_ConvertSurfaceFormat(SrcSurface, SDL_PIXELFORMAT_RGBA8888);
    //printSurface(DestSurface32);




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
        SDL_BlitSurfaceScaled(SrcSurface, nullptr, winSurface, &r, SDL_SCALEMODE_NEAREST);

        r = {50, 10, 30, 30};
        SDL_BlitSurfaceScaled(DestSurface24, nullptr, winSurface, &r, SDL_SCALEMODE_NEAREST);

        r = {100, 10, 30, 30};
        SDL_BlitSurfaceScaled(DestSurface32, nullptr, winSurface, &r, SDL_SCALEMODE_NEAREST);

        SDL_UpdateWindowSurface(win);
    }

    SDL_DestroyWindow(win);
    SDL_DestroySurface(winSurface);
    SDL_DestroySurface(SrcSurface);
    SDL_DestroySurface(DestSurface24);
    SDL_DestroySurface(DestSurface32);

    SDL_Quit();

    return 0;
}
