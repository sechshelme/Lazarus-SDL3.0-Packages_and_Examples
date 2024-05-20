//  g++ main.c -o main -lSDL3

#define SDL_MAIN_USE_CALLBACKS

#include <SDL3/SDL.h>
#include <SDL3/SDL_main.h>

struct AppContext {
    int blublu;
    SDL_Window* window;
    SDL_Renderer* renderer;
    SDL_bool app_quit;
};

int SDL_Fail(){
    SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, "Error %s", SDL_GetError());
    return -1;
}

int SDL_AppInit(void** appstate, int argc, char* argv[]) {
    // init the library, here we make a window so we only need the Video capabilities.
    if (SDL_Init(SDL_INIT_VIDEO)){
        return SDL_Fail();
    }
    
    // create a window
    SDL_Window* window = SDL_CreateWindow("Window", 352, 430, SDL_WINDOW_RESIZABLE);
    if (!window){
        return SDL_Fail();
    }
    
    SDL_Renderer* renderer = SDL_CreateRenderer(window, NULL);
    if (!renderer){
        return SDL_Fail();
    }
    
    // print some information about the window
    SDL_ShowWindow(window);
    {
        int width, height, bbwidth, bbheight;
        SDL_GetWindowSize(window, &width, &height);
        SDL_GetWindowSizeInPixels(window, &bbwidth, &bbheight);
        SDL_Log("Window size: %ix%i", width, height);
        SDL_Log("Backbuffer size: %ix%i", bbwidth, bbheight);
        if (width != bbwidth){
            SDL_Log("This is a highdpi environment.");
        }
    }

    // set up the application data
    *appstate = new AppContext{
       1234,
       window,
       renderer,
       SDL_FALSE
    };

//    auto* app = (AppContext*)appstate;
//    app->app_quit = SDL_FALSE;

    
    SDL_Log("Application started successfully!");

    return 0;
}

int SDL_AppEvent(void *appstate, const SDL_Event* event) {
    AppContext* app = (AppContext*)appstate;

    SDL_Log("blublu: %i", app->blublu);
    
    if (event->type == SDL_EVENT_QUIT) {
        app->app_quit = SDL_TRUE;
    }

    return 0;
}

int SDL_AppIterate(void *appstate) {
    AppContext* app = (AppContext*)appstate;

    // draw a color
    float time = SDL_GetTicks() / 1000.f;
    int red = (SDL_sin(time) + 1) / 2.0 * 255;
    int green = (SDL_sin(time / 2) + 1) / 2.0 * 255;
    int blue = (SDL_sin(time) * 2 + 1) / 2.0 * 255;
    
    SDL_SetRenderDrawColor(app->renderer, red, green, blue, SDL_ALPHA_OPAQUE);
    SDL_RenderClear(app->renderer);
    SDL_RenderPresent(app->renderer);

    return app->app_quit;
}

void SDL_AppQuit(void* appstate) {
    AppContext* app = (AppContext*)appstate;
    if (app) {
        SDL_DestroyRenderer(app->renderer);
        SDL_DestroyWindow(app->window);
        delete app;
    }

    SDL_Quit();
    SDL_Log("Application quit successfully!");
}
