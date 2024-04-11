program Project1;

uses
  SDL3;

  // https://github.com/libsdl-org/SDL/blob/main/docs/README-migration.md#sdl_syswmh


  procedure ShowVideo;
  var
    display: TSDL_DisplayID;
    num_modes: longint;
    modes: PPSDL_DisplayMode;
    mode: PSDL_DisplayMode;
    i: integer;
    s: string;
  begin
    display := SDL_GetPrimaryDisplay;
    modes := SDL_GetFullscreenDisplayModes(display, @num_modes);
    WriteLn(num_modes);
    if modes <> nil then begin
      for i := 0 to num_modes - 1 do begin
        mode := modes[i];
        WriteStr(s, 'Display: ', display: 3, ' mode: ', i: 4, '     ', mode^.w: 4, ' x ', mode^.h: 4, '   ', mode^.refresh_rate: 5: 1);
        SDL_Log(PChar(s));
      end;
      SDL_free(modes);
    end;
  end;

  procedure ShowAudio;
  var
    devices: PSDL_AudioDeviceID;
    num_devices: longint;
    i: integer;
    instance_id: TSDL_AudioDeviceID;
    Name: PChar;
  begin
    devices := SDL_GetAudioOutputDevices(@num_devices);
    if devices <> nil then begin
      for i := 0 to num_devices - 1 do begin
        instance_id := devices[i];
        Name := SDL_GetAudioDeviceName(instance_id);
        SDL_Log('AudioDevice %i: %s', instance_id, Name);
        SDL_free(Name);
      end;
      SDL_free(devices);
    end;
  end;


begin
  SDL_Init(SDL_INIT_VIDEO or SDL_INIT_AUDIO);

  ShowVideo;
  ShowAudio;

end.
