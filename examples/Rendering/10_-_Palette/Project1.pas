program Project1;

// https://github.com/libsdl-org/SDL/issues/9876

uses
  SDL3;

const
  widht = 320;
  Height = 240;

  function createPalette: PSDL_Palette;
  begin
    Result := SDL_CreatePalette(2);
    Result^.colors[1].r := $FF;
    Result^.colors[1].g := $FF;
    Result^.colors[1].b := $FF;
  end;

  procedure draw_screen(pixels: PUInt8; pitch: integer);
  begin
    SDL_memset(pixels, 1, pitch * Height);
  end;

  procedure main;
  var
    window: PSDL_Window;
    winSurface, surface: PSDL_Surface;
    palette: PSDL_Palette;
    pixels: PUInt8;
    quit: boolean = False;
    event: TSDL_Event;
    rect: TSDL_Rect;
  begin
    SDL_init(SDL_INIT_VIDEO);

    window := SDL_CreateWindow('Palette', widht * 2, Height * 2, 0);
    if window = nil then begin
      SDL_Log('Cannot create SDL window !');
    end;

    winSurface := SDL_GetWindowSurface(window);
    if winSurface = nil then begin
      SDL_Log('Cannot create SDL window surface !  :  %s', SDL_GetError);
    end;

    surface := SDL_CreateSurface(widht, Height, SDL_PIXELFORMAT_INDEX8);
    if surface = nil then begin
      SDL_Log('Cannot create SDL surface !  :  %s', SDL_GetError);
    end;

    palette := createPalette;
    if palette = nil then begin
      SDL_Log('Cannot create SDL palette !  :  %s', SDL_GetError);
    end;

    if not SDL_SetSurfacePalette(surface, palette) then begin
      SDL_Log('Unable to set palette');
    end;

    SDL_SetSurfaceBlendMode(winSurface, SDL_BLENDMODE_NONE);
    SDL_SetSurfaceBlendMode(surface, SDL_BLENDMODE_NONE);

    pixels := PUInt8(surface^.pixels);
    draw_screen(pixels, surface^.pitch);


    rect.items:=[10,10,100,100];

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

      SDL_Delay(10);

      SDL_FillSurfaceRect(winSurface,nil,$80);
      SDL_FillSurfaceRect(surface, @rect, $FFFFFFFF);
      SDL_BlitSurfaceScaled(surface, nil, winSurface, nil, SDL_SCALEMODE_NEAREST);
      SDL_UpdateWindowSurface(window);
    end;

    SDL_DestroySurface(surface);
    SDL_DestroySurface(winSurface);
    SDL_DestroyWindow(window);
    SDL_Quit;
  end;

begin
  main;
end.
