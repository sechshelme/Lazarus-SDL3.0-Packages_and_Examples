#include <stdio.h>
#include <stdarg.h>
#include <stddef.h>

#include <SDL3/SDL.h>

int main(int argc, char *argv[])
{
    SDL_Window *win = SDL_CreateWindow("Hello World", 320, 200, SDL_WINDOW_RESIZABLE);
    SDL_Delay(3000);
    SDL_DestroyWindow(win);
    SDL_Quit();
    return 0;
}
