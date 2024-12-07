#include <stdlib.h>
#include <SDL3/SDL.h>

int main (int argc, char *argv[])
{
#if (SDL_BYTEORDER == SDL_BIG_ENDIAN)
	SDL_Log("Detected Byte Order is Big Endian");
#endif
#if (SDL_BYTEORDER == SDL_LIL_ENDIAN)
	SDL_Log("Detected Byte Order is Little Endian");
#endif

  //SDL_version compiled;
  //SDL_version linked;

//  SDL_VERSION(&compiled);
//  SDL_GetVersion(&linked);
//  SDL_Log("We compiled against SDL version %u.%u.%u ...\n", compiled.major, compiled.minor, compiled.patch);
//  SDL_Log("But we are linking against SDL version %u.%u.%u.\n", linked.major, linked.minor, linked.patch);

  return 0;
}
