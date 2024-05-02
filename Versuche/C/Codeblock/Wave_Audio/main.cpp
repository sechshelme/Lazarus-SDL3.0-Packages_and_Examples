#include <SDL3/SDL.h>

int main(int argc, char *argv[])
{
  SDL_AudioSpec wave_spec;
  Uint8 * wave_sound;
  Uint32 wave_soundlen;
  SDL_AudioStream * stream;

  SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO);
  SDL_LoadWAV("/home/tux/Schreibtisch/sound/test.wav", &wave_spec, &wave_sound, &wave_soundlen);
  stream = SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_OUTPUT, &wave_spec, nullptr, nullptr);
  SDL_PutAudioStreamData(stream, wave_sound, wave_soundlen);
  SDL_ResumeAudioDevice(SDL_GetAudioStreamDevice(stream));
  SDL_Delay(1000);
  SDL_PauseAudioDevice(SDL_GetAudioStreamDevice(stream));
  SDL_Delay(1000);
  SDL_ResumeAudioDevice(SDL_GetAudioStreamDevice(stream));
  SDL_Delay(10000);

    // Restart sound
    SDL_ClearAudioStream(stream);
  SDL_PutAudioStreamData(stream, wave_sound, wave_soundlen);
  SDL_Delay(10000);

  //SDL_DestroyAudioStream(stream);
  SDL_free(wave_sound);
  SDL_Log("Ende");
  SDL_Quit();
  return 0;
}
