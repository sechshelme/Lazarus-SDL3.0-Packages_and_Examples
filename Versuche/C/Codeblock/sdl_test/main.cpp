#include <stdio.h>
#include <stdarg.h>
#include <stddef.h>

#include <SDL3/SDL.h>
#include <SDL3/SDL_main.h>
#include <SDL3/SDL_egl.h>
#include <SDL3/SDL_opengl.h>

#include <GL/glext.h>


extern  void  SDL_Log(const char * fmt, ...);


int main(int argc, char *argv[])
{
SDL_Log("log");
    SDL_Window *win = NULL;
    SDL_Renderer *renderer = NULL;
    SDL_Texture *bitmapTex = NULL;
    SDL_Surface *bitmapSurface = NULL;
    SDL_bool loopShouldStop = SDL_FALSE;


    SDL_LogCritical(0, "dsfdsf");

    SDL_Init(SDL_INIT_VIDEO);

    win = SDL_CreateWindow("Hello World", 320, 200, SDL_WINDOW_RESIZABLE);

     //   SDL_Delay(3000);

    renderer = SDL_CreateRenderer(win, NULL, SDL_RENDERER_ACCELERATED);

  //  bitmapSurface = SDL_LoadBMP("img/hello.bmp");
    bitmapTex = SDL_CreateTextureFromSurface(renderer, bitmapSurface);
    SDL_DestroySurface(bitmapSurface);

    SDL_Log("log");
    SDL_Log("log");
    SDL_Log("log");
    SDL_Log("log");
    SDL_Log("log");
    SDL_Log("log");


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

        SDL_RenderClear(renderer);
        SDL_RenderTexture(renderer, bitmapTex, NULL, NULL);
        SDL_RenderPresent(renderer);

        float x, y;
        Uint32 buttons;

        SDL_PumpEvents();  // make sure we have the latest mouse state.

        buttons = SDL_GetMouseState(&x, &y);

        //SDL_Log("Mouse cursor is at %f, %f", x, y);
        if ((buttons & SDL_BUTTON_LMASK) != 0) {
           SDL_Log("Mouse Button 1 (left) is pressed.");
        }
    }

    SDL_DestroyTexture(bitmapTex);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(win);

    SDL_Quit();

    return 0;
}
