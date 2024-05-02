program Project1;

uses
  sdl3,
  SDL3_mixer;

var
  spec: TSDL_AudioSpec;
  music: PMix_Music;
begin
  spec.freq:=MIX_DEFAULT_FREQUENCY;
  spec.format:=MIX_DEFAULT_FORMAT;
  spec.channels:=MIX_DEFAULT_CHANNELS;

  SDL_Init(SDL_INIT_AUDIO);
  if Mix_Init(MIX_INIT_WAVPACK) <> 0 then begin
    SDL_Log('Fehler: MixInit: %s', SDL_GetError());
    halt;
  end;

  if Mix_OpenAudio(0, @spec) < 0 then  begin
    SDL_Log('Fehler: Kann Audio nicht öffnen: %s', SDL_GetError());
  end;


  Mix_QuerySpec(@spec.freq, @spec.format, @spec.channels);
  WriteLn(spec.freq);
  WriteLn(spec.channels);
  WriteLn(SDL_AUDIO_BITSIZE(spec.format));

  Mix_VolumeMusic(MIX_MAX_VOLUME);

  Mix_SetMusicCMD(SDL_getenv('MUSIC_CMD'));

  music := Mix_LoadMUS('/home/tux/Schreibtisch/sound/test.wav');
//  music := Mix_LoadMUS('/home/tux/Schreibtisch/sound/dia.wav');
  if music = nil then begin
    WriteLn('WAV nicht gefunden !  ', Mix_GetError);
  end;

  if Mix_GetMusicType(music)=MUS_WAV then WriteLn('WAV');

  WriteLn(Mix_MusicDuration(music):4:2);

  Mix_FadeInMusic(music, 0, 2000);

  Mix_PlayMusic(music, 0);

  //  Mix_PlayChannel(-1,music,0);

  //Mix_FreeChunk(music);
end.
