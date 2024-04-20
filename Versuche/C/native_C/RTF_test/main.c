/*
  showrtf:  An example of using the SDL_rtf library
  Copyright (C) 2003-2024 Sam Lantinga <slouken@libsdl.org>

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
*/

/* A simple program to test the RTF rendering of the SDL_rtf library */


/*
  Linux:
  gcc main.c -o main -lSDL3 -lSDL3_ttf -lSDL3_rtf

  Windows: 
////  x86_64-w64-mingw32-gcc main.c -o main.exe -lSDL3 -lSDL3_image -I/usr/local/include -L/usr/local/bin
*/


#include <SDL3/SDL.h>
#include <SDL3_ttf/SDL_ttf.h>
#include <SDL3_rtf/SDL_rtf.h>

#define SCREEN_WIDTH    640
#define SCREEN_HEIGHT   480

static const char *FontList[8];

/* Note, this is only one way of looking up fonts */
static int FontFamilyToIndex(RTF_FontFamily family)
{
    switch(family) {
        case RTF_FontDefault:
            return 0;
        case RTF_FontRoman:
            return 1;
        case RTF_FontSwiss:
            return 2;
        case RTF_FontModern:
            return 3;
        case RTF_FontScript:
            return 4;
        case RTF_FontDecor:
            return 5;
        case RTF_FontTech:
            return 6;
        case RTF_FontBidi:
            return 7;
        default:
            return 0;
    }
}

static void * SDLCALL CreateFont(const char *name, RTF_FontFamily family, int charset, int size, int style)
{
    TTF_Font *font;

    font = TTF_OpenFont(FontList[0], size);
    if (font) {
        TTF_SetFontStyle(font, style);
    }
    return font;
}

static int SDLCALL GetLineSpacing(void *_font)
{
    TTF_Font *font = (TTF_Font *)_font;
    return TTF_FontLineSkip(font);
}

static int SDLCALL GetCharacterOffsets(void *_font, const char *text, int *byteOffsets, int *pixelOffsets, int maxOffsets)
{
    TTF_Font *font = (TTF_Font *)_font;
    int i = 0;
    int bytes = 0;
    int pixels = 0;
    int advance;
    Uint16 ch;
    while ( *text && i < maxOffsets ) {
        byteOffsets[i] = bytes;
        pixelOffsets[i] = pixels;
        ++i;

ch=text;
advance=1;

        text += 1;
        bytes += 1;
SDL_Log("vorher: %i", advance);
        TTF_GlyphMetrics(font, ch, NULL, NULL, NULL, NULL, &advance);
SDL_Log("nachher: %i", advance);
        pixels += advance;
    }
    if ( i < maxOffsets ) {
        byteOffsets[i] = i;
        pixelOffsets[i] = pixels;
    }
    SDL_Log("offset: %i", i);
    return i;
}

static SDL_Texture * SDLCALL RenderText(void *_font, SDL_Renderer *renderer, const char *text, SDL_Color fg)
{
    TTF_Font *font = (TTF_Font *)_font;
    SDL_Texture *texture = NULL;
    SDL_Surface *surface = TTF_RenderUTF8_Blended(font, text, fg);
    if (surface) {
        texture = SDL_CreateTextureFromSurface(renderer, surface);
        SDL_DestroySurface(surface);
    }
    return texture;
}

static void SDLCALL FreeFont(void *_font)
{
    TTF_Font *font = (TTF_Font *)_font;
    TTF_CloseFont(font);
}

static void LoadRTF(RTF_Context *ctx, const char *file)
{
    if ( RTF_Load(ctx, file) < 0 ) {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "Couldn't load %s: %s\n", file, RTF_GetError());
        return;
    }
}

static void PrintUsage(const char *argv0)
{
    SDL_Log("Usage: %s -fdefault font.ttf [-froman font.ttf] [-fswiss font.ttf] [-fmodern font.ttf] [-fscript font.ttf] [-fdecor font.ttf] [-ftech font.ttf] file.rtf\n", argv0);
}

static void cleanup(void)
{
    TTF_Quit();
    SDL_Quit();
}

void test() {};

int main(int argc, char *argv[])
{
    int i, start, stop;
    int w, h;
    int done;
    int height;
    int offset;
    SDL_Window *window;
    SDL_Renderer *renderer;
    RTF_Context *ctx;
    RTF_FontEngine fontEngine;
    const Uint8 *keystate;

    /* Parse command line arguments */
            FontList[FontFamilyToIndex(RTF_FontDefault)] = "lazy.ttf";

    start = i;
    stop = (argc-1);
    if ( !FontList[0] || (start > stop) ) {
        PrintUsage(argv[0]);
//        return(1);
    }

    /* Initialize the TTF library */
//    if ( TTF_Init() < 0 ) {
//        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "Couldn't initialize TTF: %s\n",SDL_GetError());
//        SDL_Quit();
//        return(3);
//    }

    if (SDL_CreateWindowAndRenderer(SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_RESIZABLE, &window, &renderer) < 0) {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "SDL_CreateWindowAndRenderer() failed: %s\n", SDL_GetError());
        cleanup();
        return(4);
    }

     TTF_Init();
    /* Create and load the RTF document */
    fontEngine.version = RTF_FONT_ENGINE_VERSION;
    fontEngine.CreateFont = CreateFont;
    fontEngine.GetLineSpacing = GetLineSpacing;
    fontEngine.GetCharacterOffsets = GetCharacterOffsets;
    fontEngine.RenderText = RenderText;
    fontEngine.FreeFont = FreeFont;
    ctx = RTF_CreateContext(renderer, &fontEngine);
    if ( ctx == NULL ) {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "Couldn't create RTF context: %s\n", RTF_GetError());
        cleanup();
        return(5);
    }
    LoadRTF(ctx, "text.rtf");
    SDL_SetWindowTitle(window, RTF_GetTitle(ctx));

    /* Render the document to the screen */
    done = 0;
    offset = 0;
    SDL_GetWindowSize(window, &w, &h);
    height = RTF_GetHeight(ctx, w);
    while (!done) {
        SDL_Event event;
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_EVENT_WINDOW_RESIZED) {
                float ratio = (float)offset / height;
                SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION, "Resetting window\n");
                SDL_GetWindowSize(window, &w, &h);
                SDL_SetRenderViewport(renderer, NULL);
                height = RTF_GetHeight(ctx, w);
                offset = (int)(ratio * height);
            }
            if (event.type == SDL_EVENT_KEY_DOWN) {
                switch(event.key.keysym.sym) {
                    case SDLK_ESCAPE:
                        done = 1;
                        break;
                    case SDLK_LEFT:
                        if ( i > start ) {
                            --i;
                            LoadRTF(ctx, argv[i]);
                            offset = 0;
                            height = RTF_GetHeight(ctx, w);
                        }
                        break;
                    case SDLK_RIGHT:
                        if ( i < stop ) {
                            ++i;
                            LoadRTF(ctx, argv[i]);
                            offset = 0;
                            height = RTF_GetHeight(ctx, w);
                        }
                        break;
                    case SDLK_HOME:
                        offset = 0;
                        break;
                    case SDLK_END:
                        offset = (height - h);
                        break;
                    case SDLK_PAGEUP:
                        offset -= h;
                        if ( offset < 0 )
                            offset = 0;
                        break;
                    case SDLK_PAGEDOWN:
                    case SDLK_SPACE:
                        offset += h;
                        if ( offset > (height - h) )
                            offset = (height - h);
                        break;
                    default:
                        break;
                }
            }
            if (event.type == SDL_EVENT_QUIT) {
                done = 1;
            }
        }
        keystate = SDL_GetKeyboardState(NULL);
        if ( keystate[SDL_SCANCODE_UP] ) {
            offset -= 1;
            if ( offset < 0 )
                offset = 0;
        }
        if ( keystate[SDL_SCANCODE_DOWN] ) {
            offset += 1;
            if ( offset > (height - h) )
                offset = (height - h);
        }

        SDL_SetRenderDrawColor(renderer, 0xFF, 0xFF, 0xFF, 0xFF);
        SDL_RenderClear(renderer);
        RTF_Render(ctx, NULL, offset);
        SDL_RenderPresent(renderer);
        SDL_Delay(10);
    }

    /* Clean up and exit */
    RTF_FreeContext(ctx);
    cleanup();

    return 0;
}

/* vi: set ts=4 sw=4 expandtab: */
