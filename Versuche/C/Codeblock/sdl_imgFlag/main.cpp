#include <stdio.h>
#include <stdarg.h>
#include <stddef.h>

#include <SDL3/SDL.h>
#include <SDL3_image/SDL_image.h>

int main(int argc, char *argv[])
{
    SDL_Log("Start");
    SDL_Init(SDL_INIT_VIDEO);

    int flag = 0;
    IMG_Init(flag);
    SDL_Log("Flag: %i", flag);

    SDL_Delay(5000);

    SDL_Log("Ende");
    return 0;
}
