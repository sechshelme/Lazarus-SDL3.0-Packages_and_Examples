program Project1;

{$modeswitch ArrayOperators}

uses
  SysUtils,
  SDL3;

  procedure printSurface(sur: PSDL_Surface);
  var
    ch: pbyte;
  begin
    ch := sur^.pixels;
    SDL_Log('Pixel: %02X %02X %02X %02X ', ch[0], ch[1], ch[2], ch[3]);
    SDL_Log('format: %u', sur^.format^.format);
    SDL_Log('bit per pixel: %u    bytes per Pixel: %u', sur^.format^.bits_per_pixel, sur^.format^.bytes_per_pixel);
    SDL_Log('Rmask:  %08X   Gmask:  %08X   Bmask:  %08X   Amask:  %08X   ', sur^.format^.Rmask, sur^.format^.Gmask, sur^.format^.Bmask, sur^.format^.Amask);
    SDL_Log('Rshift: %u   Gshift: %u   Bshift: %u   Ashift: %u   ', sur^.format^.Rshift, sur^.format^.Gshift, sur^.format^.Bshift, sur^.format^.Ashift);
    SDL_Log('Rloss: %u   Gloss: %u   Bloss: %u   Aloss: %u'#10#10, sur^.format^.Rloss, sur^.format^.Gloss, sur^.format^.Bloss, sur^.format^.Aloss);
  end;


  function CreateSurface1: PSDL_Surface;
  begin
    Result := SDL_LoadBMP('mauer.bmp');
    if Result = nil then begin
      SDL_Log('Konnte BMP nicht laden!:  %s', SDL_GetError);
    end;
  end;

  function CreateSurface2: PSDL_Surface;
  const
    Data: array of DWord = (
      $000000FF, $FF0000FF, $00FF00FF, $0000FFFF,
      $444444FF, $FF4444FF, $44FF44FF, $4444FFFF,
      $888888FF, $FF8888FF, $88FF88FF, $8888FFFF,
      $AAAAAAFF, $FFAAAAFF, $AAFFAAFF, $AAAAAAFF);
  begin
    Result := SDL_CreateSurfaceFrom(PDWord(Data), 4, 4, 16, SDL_PIXELFORMAT_RGBA8888);
    if Result = nil then begin
      SDL_Log('Konnte BMP nicht laden!:  %s', SDL_GetError);
    end;
  end;

type
  TTriByte = bitpacked record
    rgb: 0..$FFFFFF;
  end;
  PTriByte = ^TTriByte;

  function CreateSurface3: PSDL_Surface;
  const
    Data: array of TTriByte = (
      (rgb: $000000), (rgb: $FF0000), (rgb: $00FF00), (rgb: $0000FF),
      (rgb: $444444), (rgb: $FF4444), (rgb: $44FF44), (rgb: $4444FF),
      (rgb: $888888), (rgb: $FF8888), (rgb: $88FF88), (rgb: $8888FF),
      (rgb: $AAAAAA), (rgb: $FFAAAA), (rgb: $AAFFAA), (rgb: $AAAAAA));
  begin
    Result := SDL_CreateSurfaceFrom(PDWord(Data), 4, 4, 12, SDL_PIXELFORMAT_RGB24);
    if Result = nil then begin
      SDL_Log('Konnte BMP nicht laden!:  %s', SDL_GetError);
    end;
  end;

  operator := (const AValue: integer): TTriByte; inline;
  begin
    Result.rgb := AValue;
  end;

  operator := (const AValue: TTriByte): integer; inline;
  begin
    Result := AValue.rgb;
  end;

  function CreateSurface4: PSDL_Surface;
  var
    Data: array of TTriByte = nil;
  begin
    Data := [
      $000000, $FF0000, $00FF00, $0000FF,
      $444444, $FF4444, $44FF44, $4444FF,
      $888888, $FF8888, $88FF88, $8888FF,
      $AAAAAA, $FFAAAA, $AAFFAA, $AAAAAA];

    Result := SDL_CreateSurfaceFrom(PTriByte(Data), 4, 4, 12, SDL_PIXELFORMAT_RGB24);
    if Result = nil then begin
      SDL_Log('Konnte BMP nicht laden!:  %s', SDL_GetError);
    end;
  end;


  procedure main;
  const
    pixels_1: array [0..3] of byte = ($FF, $FF, $00, $FF);
    pixels_2: array [0..0] of DWord = ($FFFF00FF);
  var
    win: PSDL_Window;
    winSurface: PSDL_Surface;
    Surface: array of PSDL_Surface = nil;
    r: TSDL_Rect;
    i: integer;
    quit: boolean = False;
    event: TSDL_Event;
    w, h: longint;
  begin
    SDL_Init(SDL_INIT_VIDEO);
    win := SDL_CreateWindow('Big / Little-Endian', 320, 200, SDL_WINDOW_RESIZABLE);

    // io.
    Surface += [SDL_CreateSurface(1, 1, SDL_PIXELFORMAT_RGBA32)];
    SDL_memcpy(Surface[0]^.pixels, @pixels_1, sizeof(pixels_1));

    // warped
    Surface += [SDL_CreateSurface(1, 1, SDL_PIXELFORMAT_RGBA32)];
    SDL_memcpy(Surface[1]^.pixels, @pixels_2, sizeof(pixels_2));

    // io.
    Surface += [SDL_CreateSurface(1, 1, SDL_PIXELFORMAT_RGBA8888)];
    SDL_memcpy(Surface[2]^.pixels, @pixels_2, sizeof(pixels_2));

    Surface += [SDL_LoadBMP('mauer.bmp')];
    Surface += [SDL_LoadBMP('autos4bit.bmp')];
    Surface += [SDL_LoadBMP('autos8bit.bmp')];

    Surface += [SDL_ConvertSurfaceFormat(Surface[4], SDL_PIXELFORMAT_RGBA32)];

    Surface += [CreateSurface2];
    Surface += [CreateSurface3];
    Surface += [CreateSurface4];

    for i := 0 to Length(Surface) - 1 do begin
      printSurface(Surface[i]);
    end;

    SDL_SetWindowIcon(win, Surface[Length(Surface) - 1]);


    while not quit do begin
      while SDL_PollEvent(@event) do begin
        case event._type of
          SDL_EVENT_KEY_DOWN: begin
            case event.key.key of
              SDLK_ESCAPE: begin
                quit := True;
              end;
            end;
          end;
          SDL_EVENT_QUIT: begin
            quit := True;
          end;
        end;
      end;
      winSurface := SDL_GetWindowSurface(win);
      SDL_FillSurfaceRect(winSurface, nil, $004400);

      SDL_GetWindowSize(win, @w, @h);

      for i := 0 to Length(Surface) - 1 do begin
        r.items := [10 + (i mod 3) * (w div 5), 10 + (i div 3) * (h div 5), w div 6, h div 6];
        SDL_BlitSurfaceScaled(Surface[i], nil, winSurface, @r, SDL_SCALEMODE_NEAREST);
      end;

      SDL_UpdateWindowSurface(win);
    end;

    SDL_DestroyWindow(win);
    SDL_DestroySurface(winSurface);

    for i := 0 to Length(Surface) - 1 do begin
      SDL_DestroySurface(Surface[i]);
    end;

    SDL_Quit();
  end;

begin
  main;
end.
