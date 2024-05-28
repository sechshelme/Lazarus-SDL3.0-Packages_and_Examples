program Project1;

uses
  SDL3;

  function CreateSurface: PSDL_Surface;
  begin
    Result := SDL_LoadBMP('mauer.bmp');
    if Result = nil then begin
      SDL_Log('Konnte BMP nicht laden!:  %s', SDL_GetError);
    end;
  end;

  procedure main;
  var
    window: PSDL_Window;
    winSurface, imageSurface: PSDL_Surface;
    rDest: TSDL_Rect;
    quit: boolean = False;
    event: TSDL_Event;
  begin
    if SDL_init(SDL_INIT_VIDEO) < 0 then begin
      SDL_Log('Konnte SDL-VIDEO nicht laden!:  %s', SDL_GetError);
    end;

    window := SDL_CreateWindow('Surface Example', 640, 480, SDL_WINDOW_RESIZABLE);
    if window = nil then begin
      SDL_Log('Konnte kein Windows erzeugen!:  %s', SDL_GetError);
    end;

    winSurface := SDL_GetWindowSurface(window);
    if winSurface = nil then begin
      SDL_Log('Konnte kein Surface erzeugen!:  %s', SDL_GetError);
    end;

    imageSurface := CreateSurface;

    while not quit do begin
      while SDL_PollEvent(@event) do begin
        case event.type_ of
          SDL_EVENT_KEY_DOWN: begin
            case event.key.keysym.sym of
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

      rDest.items := [100, 100, 200, 200];
      SDL_BlitSurfaceScaled(imageSurface, nil, winSurface, @rDest, SDL_SCALEMODE_NEAREST);
      SDL_BlitSurface(imageSurface, nil, winSurface, nil);
      SDL_UpdateWindowSurface(window);
    end;

    SDL_DestroySurface(imageSurface);
    SDL_DestroySurface(winSurface);
    SDL_DestroyWindow(window);
    SDL_Quit;
  end;

begin
  main;
end.
