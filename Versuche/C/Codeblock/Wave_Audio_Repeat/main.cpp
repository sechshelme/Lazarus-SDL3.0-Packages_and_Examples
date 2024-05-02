#include <SDL3/SDL.h>

struct {
  SDL_AudioSpec spec;
  Uint8 *sound;
  Uint32 soundlen;
} wave;

SDL_AudioStream * stream;

void AdioStreamCallback(void *usrdata, SDL_AudioStream *stream, int additional_amount, int total_amount)
{
  if (additional_amount == total_amount) {
    SDL_PutAudioStreamData(stream, wave.sound, wave.soundlen);
  }
}

int main(int argc, char *argv[])
{
  SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO);
  SDL_Window *win = SDL_CreateWindow("Hello World", 640, 480, 0);

  SDL_LoadWAV("/home/tux/Schreibtisch/sound/dia.wav", &wave.spec, &wave.sound, &wave.soundlen);
  stream = SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_OUTPUT, &wave.spec, AdioStreamCallback, nullptr);
  SDL_PutAudioStreamData(stream, wave.sound, wave.soundlen);
  SDL_ResumeAudioDevice(SDL_GetAudioStreamDevice(stream));

  bool quit = false;
  while(!quit){
    SDL_Event event;
    while(SDL_PollEvent(&event)) {
      switch (event.type) {
        case SDL_EVENT_QUIT:
          quit = true;
          break;
        case SDL_EVENT_KEY_DOWN:
          switch (event.key.keysym.sym) {
            case SDLK_ESCAPE:
              quit = true;
              break;
            case SDLK_p:
              SDL_AudioDeviceID id = SDL_GetAudioStreamDevice(stream);
              if (SDL_AudioDevicePaused(id)){
                SDL_ResumeAudioDevice(id);
              } else {
                SDL_PauseAudioDevice(id);
              }
              break;
         }
         break;
      }
    }
  }

  SDL_DestroyAudioStream(stream);
  SDL_free(wave.sound);
  SDL_DestroyWindow(win);
  SDL_Quit();
  return 0;
}
