program Project1;

uses
  sdl3,
  SDL3_mixer;

var
  spec: TSDL_AudioSpec;
  music: PMix_Music;
  window: PSDL_Window;


  procedure EventHandle;
  var quit:TSDL_bool=SDL_FALSE;
    event: TSDL_Event;
  begin
    while not quit do begin
         while SDL_PollEvent(@event) do begin
           case event.type_ of
             SDL_EVENT_KEY_DOWN: begin
               SDL_Log('key: %i', event.key.keysym.sym); // neu

               case event.key.keysym.sym of
                 SDLK_ESCAPE: begin
                   quit := True;
                 end;
                 SDLK_m: begin
                   //                SwitchMouseButton;
                 end;

               end;
             end;
             SDL_EVENT_MOUSE_BUTTON_DOWN: begin
               SDL_Log('Mouse down');
             end;
             SDL_EVENT_MOUSE_BUTTON_UP: begin
               SDL_Log('Mouse up');
             end;
             SDL_EVENT_QUIT: begin
               quit := True;
             end;
           end;
         end;    end;
  end;

begin
  spec.freq:=MIX_DEFAULT_FREQUENCY;
  spec.format:=MIX_DEFAULT_FORMAT;
  spec.channels:=MIX_DEFAULT_CHANNELS;

  SDL_Init(SDL_INIT_VIDEO or SDL_INIT_AUDIO);
  if Mix_Init(MIX_INIT_WAVPACK) <> 0 then begin
    SDL_Log('Fehler: MixInit: %s', SDL_GetError());
    halt;
  end;

      window := SDL_CreateWindow('SDL3 Window', 320, 200, SDL_WINDOW_RESIZABLE);
    if window = nil then begin
      SDL_Log('Kann kein SDL-Fenster erzeugen !   %s',SDL_GetError);
    end;


  if Mix_OpenAudio(0, @spec) < 0 then  begin
    SDL_Log('Fehler: Kann Audio nicht öffnen: %s', SDL_GetError());
  end;


  Mix_QuerySpec(@spec.freq, @spec.format, @spec.channels);
  WriteLn(spec.freq);
  WriteLn(spec.channels);
  WriteLn(SDL_AUDIO_BITSIZE(spec.format));

//  Mix_VolumeMusic(MIX_MAX_VOLUME);

//  Mix_SetMusicCMD(SDL_getenv('MUSIC_CMD'));

  music := Mix_LoadMUS('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/examples/Audio/10_-_SDL_LoadWav_2/Boing_1.wav');
//  music := Mix_LoadMUS('/home/tux/Schreibtisch/sound/dia.wav');
  if music = nil then begin
    WriteLn('WAV nicht gefunden !  ', Mix_GetError);
  end;

  if Mix_GetMusicType(music)=MUS_WAV then WriteLn('WAV');

  WriteLn(Mix_MusicDuration(music):4:2);

//  Mix_FadeInMusic(music, 0, 2000);

  Mix_PlayMusic(music, -10);

  EventHandle;

      SDL_DestroyWindow(window);

    Mix_FreeMusic(music);
  Mix_CloseAudio;
  SDL_Quit;
end.
