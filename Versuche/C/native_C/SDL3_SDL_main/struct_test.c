//  g++ main.c -o main

#include <stdio.h>

struct AppContext {
    int blublu;
} appContext;

void SDL_AppInit(void** appstate) {
    appContext.blublu = 8888;
    *appstate = &appContext;
}

void SDL_AppEvent(void *appstate) {
    struct     AppContext app;
    app = (AppContext) appstate; // geht nicht !

    printf("blublu: %i\n", app.blublu);
}

int main(){
   void *p;
   SDL_AppInit(&p);
   SDL_AppEvent(p);
}
