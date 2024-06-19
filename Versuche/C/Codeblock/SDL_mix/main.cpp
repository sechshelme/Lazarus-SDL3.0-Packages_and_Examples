#include <SDL3/SDL.h>
#include <SDL3_mixer/SDL_mixer.h>

int main(int argc, char *argv[])
{
  SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO);

  Mix_OpenAudio(0, nullptr);

  Mix_Music *music1 = Mix_LoadMUS("/n4800/Multimedia/Music/Disco/C.C. Catch/1988 - Diamonds/05  - Heartbreak Hotel.flac");
  if (!music1) {
    SDL_Log("Konnte music1 nicht laden!  %s", Mix_GetError);
  }
  Mix_PlayMusic(music1, 1);

  Mix_Music *music2 = Mix_LoadMUS("/n4800/Multimedia/Music/Disco/C.C. Catch/1988 - Diamonds/06  - Soul Survivor.flac");
  if (!music1) {
    SDL_Log("Konnte music2 nicht laden!  %s", Mix_GetError);
  }
  Mix_PlayMusic(music2, 1);

  SDL_Delay(1000000);

  return 0;
}
