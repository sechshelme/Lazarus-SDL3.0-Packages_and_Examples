
/*
  Linux:
  gcc main.c -o main -lSDL3 -lSDL3_mixer


  Windows: 
  x86_64-w64-mingw32-gcc main.c -o main.exe -lSDL3 -lSDL3_mixer -I/usr/local/include -L/usr/local/bin
*/

#include <SDL3/SDL.h>
//#include <SDL3/SDL_main.h>
#include <SDL3_mixer/SDL_mixer.h>

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif


static int audio_open = 0;
static Mix_Music *music = NULL;

int main(int argc, char *argv[])
{
    int audio_volume = MIX_MAX_VOLUME;
    int looping = 0;
    SDL_bool interactive = SDL_FALSE;
    SDL_bool use_io = SDL_FALSE;
    int i;
    double loop_start, loop_end, loop_length, current_position;
    SDL_AudioSpec spec;

    (void) argc;

    /* Initialize variables */
    spec.freq = MIX_DEFAULT_FREQUENCY;
    spec.format = MIX_DEFAULT_FORMAT;
    spec.channels = MIX_DEFAULT_CHANNELS;

    /* Initialize the SDL library */
    SDL_Init(SDL_INIT_AUDIO);

    Mix_OpenAudio(0, &spec);

    Mix_QuerySpec(&spec.freq, &spec.format, &spec.channels);

//            music = Mix_LoadMUS("/home/tux/Schreibtisch/sound/dia.wav");
    music = Mix_LoadMUS("/home/tux/Schreibtisch/sound/test.wav");
//    Mix_GetMusicLoopStartTime(music);
//    Mix_GetMusicLoopEndTime(music);
//    Mix_GetMusicLoopLengthTime(music);

Mix_PlayMusic(music, 3);

//    Mix_FadeInMusic(music,looping,2000);

SDL_Delay(5000);
//        while ((Mix_PlayingMusic() || Mix_PausedMusic())) {
  //        current_position = Mix_GetMusicPosition(music);
    //      SDL_Log("time %f", current_position);
      //  }
        Mix_FreeMusic(music);
    return 0;
}

/* vi: set ts=4 sw=4 expandtab: */
