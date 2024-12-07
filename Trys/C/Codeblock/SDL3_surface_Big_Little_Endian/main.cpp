#include <SDL3/SDL.h>

void printSurface(SDL_Surface *sur) {
  unsigned char *ch ;
  ch = (unsigned char*) sur->pixels;
  SDL_Log("Pixel: %2X %2X %2X %2X ", ch[0], ch[1], ch[2], ch[3]);
  SDL_Log("format: %u", sur->format->format);
  SDL_Log("bit per pixel: %u    bytes per Pixel: %u", sur->format->bits_per_pixel, sur->format->bytes_per_pixel);
  SDL_Log("Rmask:  %08X   Gmask:  %08X   Bmask:  %08X   Amask:  %08X   ", sur->format->Rmask, sur->format->Gmask, sur->format->Bmask, sur->format->Amask);
  SDL_Log("Rshift: %u   Gshift: %u   Bshift: %u   Ashift: %u   ", sur->format->Rshift, sur->format->Gshift, sur->format->Bshift, sur->format->Ashift);
  SDL_Log("Rloss: %u   Gloss: %u   Bloss: %u   Aloss: %u\n\n", sur->format->Rloss, sur->format->Gloss, sur->format->Bloss, sur->format->Aloss);
}

int main(int argc, char *argv[])
{
    uint8_t  pixels_1[] = {0xFF, 0xFF, 0x00, 0xFF};
    uint32_t pixels_2[] = {0xFFFF00FF};

    SDL_Init(SDL_INIT_VIDEO);
    SDL_Window *win = SDL_CreateWindow("Bit/Little-Endian", 320, 200, SDL_WINDOW_RESIZABLE);
    SDL_Surface *winSurface = SDL_GetWindowSurface(win);

    // io.
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

    SDL_Rect r;
    r = {10, 10, 30, 30};
    SDL_BlitSurfaceScaled(Surf1, nullptr, winSurface, &r, SDL_SCALEMODE_NEAREST);

    r = {50, 10, 30, 30};
    SDL_BlitSurfaceScaled(Surf2, nullptr, winSurface, &r, SDL_SCALEMODE_NEAREST);

    r = {90, 10, 30, 30};
    SDL_BlitSurfaceScaled(Surf3, nullptr, winSurface, &r, SDL_SCALEMODE_NEAREST);

    SDL_UpdateWindowSurface(win);

    SDL_Delay(5000);  // for Test

    SDL_DestroyWindow(win);
    SDL_DestroySurface(winSurface);
    SDL_DestroySurface(Surf1);
    SDL_DestroySurface(Surf2);
    SDL_DestroySurface(Surf3);

    SDL_Quit();

    return 0;
}
