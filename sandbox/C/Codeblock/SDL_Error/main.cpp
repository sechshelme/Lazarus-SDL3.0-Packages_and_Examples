#include <SDL3/SDL.h>

void * CreateObject()
{
  void * test = nullptr; // as test NULL
  if (!test) {
    SDL_SetError("Couldn't build !");
  }
  return test;
}

int main(int argc, char *argv[])
{
  void * object;
  object = CreateObject();
  if (!object) {
    SDL_Log("Error: %s", SDL_GetError());
  }
}
