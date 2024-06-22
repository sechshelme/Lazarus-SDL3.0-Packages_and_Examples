program Project1;

uses
  ctypes,
  SDL3;

// https://discourse.libsdl.org/t/sdl-pixelformat-index1lsb-and-sdl-pixelformat-index1msb/51583
// https://stackoverflow.com/questions/77314858/is-sdl-createsurfacefrom-sdl3-reading-my-data-wrong

const
  ScreenWidth = 640;
  ScreenHeight = 480;

var
  window: PSDL_Window;
  renderer: PSDL_Renderer;

  procedure test_sdl;
  var
    arr: array of uint8=nil;
    Width, Height, i: integer;
    surface: PSDL_Surface;
    colors: array[0..3] of TSDL_Color;
    texture: PSDL_Texture;
  begin
    Width := ScreenWidth div 20;
    Height := ScreenHeight div 20;
    SetLength(arr, Width * Height);
    for i := 0 to Length(arr) - 1 do begin
      if i <= 2 then begin
        arr[i] := %11110000;
      end else begin
        arr[i] := %00000000;
      end;
//      arr[i]:=Random($ff);
    end;

     surface:=SDL_CreateSurfaceFrom(Pointer(arr),width,height,16, SDL_PIXELFORMAT_INDEX1LSB);
//    surface := SDL_CreateSurface(Width, Height, SDL_PIXELFORMAT_INDEX1MSB);
    if surface = nil then begin
      SDL_Log('Kann kein SDL-Surface erzeugen !  %s',SDL_GetError);
    end;
  //  surface^.pixels:=Pointer(arr);

    colors[0].items := [$FF, $00, $00, $FF];
    colors[1].items := [$00, $FF, $00, $FF];
    colors[2].items := [$00, $00, $FF, $FF];
    colors[3].items := [$FF, $00, $FF, $FF];
    SDL_SetPaletteColors(surface^.format^.palette, colors, 0, Length(colors));

    texture := SDL_CreateTextureFromSurface(renderer, surface);
    if texture = nil then begin
      SDL_Log('Kann kein SDL-Textur erzeugen !  %s',SDL_GetError)
    end;

    SDL_RenderTexture(renderer, texture, nil, nil);
    SDL_RenderPresent(renderer);
  end;

  procedure EventHandle;
  var
    event: TSDL_Event;
    quit: boolean = False;
  begin
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
      test_sdl;
    end;
  end;

  procedure main;
  begin
    SDL_init(SDL_INIT_VIDEO);

    window := SDL_CreateWindow('SDL3 Window', ScreenWidth, ScreenHeight, SDL_WINDOW_RESIZABLE);
    if window = nil then begin
      SDL_Log('Kann kein SDL-Fenster erzeugen !  %s',SDL_GetError);
    end;

    renderer := SDL_CreateRenderer(window, nil);
    if renderer = nil then begin
      SDL_Log('Kann kein SDL-Renderer erzeugen !  %s',SDL_GetError);
    end;

    EventHandle;

    SDL_DestroyWindow(window);
    SDL_DestroyRenderer(renderer);

    SDL_Quit;
  end;

begin
  main;
end.
