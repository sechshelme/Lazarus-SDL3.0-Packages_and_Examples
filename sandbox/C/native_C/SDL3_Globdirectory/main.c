/*
  Linux:
  g++ main.c -o main -lSDL3
  
  Windows:
  x86_64-w64-mingw32-gcc main.c -o main.exe -lSDL3 -I/usr/local/include -L/usr/local/bin
*/

#include <SDL3/SDL.h>

int main(int argc, char *argv[]) {
    int count;
    char **list;
    int i;

    SDL_Init(SDL_INIT_VIDEO);

    list = SDL_GlobDirectory("c:\\windows\\", "*.*", SDL_PATHTYPE_FILE, &count);
    // list = SDL_GlobDirectory("/usr/local/lib/", "*.*", SDL_PATHTYPE_FILE, &count);

    if (list == NULL) {
        SDL_Log("file not found");
    } else {
        for (i = 0; i < count; i++) {
            SDL_Log("%3d.  %s", i, list[i]);
        }

        SDL_free(list);
    }

    SDL_Quit();
    return 0;
}


