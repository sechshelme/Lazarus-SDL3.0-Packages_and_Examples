program Project1;

uses
  SDL3;

  function CreateSurface(parent: PSDL_Surface; x, y: integer): PSDL_Surface;
  begin
    Result := SDL_CreateSurfaceFrom(PDWord(parent^.pixels + y * parent^.pitch + x * 4), 50, 50, parent^.pitch, parent^.format^.format);
    if Result = nil then begin
      SDL_Log('Konnte Surface nicht laden!:  %s', SDL_GetError);
    end;
  end;

  procedure main;
  var
    window: PSDL_Window;
    winSurface, Surface1, Surface2: PSDL_Surface;
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

    Surface1 := CreateSurface(winSurface, 20, 20);
    Surface2 := CreateSurface(winSurface, 20, 120);

    while not quit do begin
      while SDL_PollEvent(@event) do begin
        case event.type_ of
          SDL_EVENT_KEY_DOWN: begin
            case event.key.keysym.sym of
              SDLK_ESCAPE: begin
                quit := True;
              end;
              SDLK_SPACE: begin
              end;
            end;
          end;
          SDL_EVENT_QUIT: begin
            quit := True;
          end;
        end;
      end;

      SDL_FillSurfaceRect(Surface1, nil, Random($FFFFFF));
      SDL_FillSurfaceRect(Surface2, nil, Random($FFFFFF));
      //      rDest.items := [350, 100, 200, 200];
      //      SDL_BlitSurfaceScaled(Surface1, nil, winSurface, @rDest, SDL_SCALEMODE_NEAREST);
      SDL_UpdateWindowSurface(window);
    end;

    SDL_DestroySurface(Surface1);
    SDL_DestroySurface(winSurface);
    SDL_DestroyWindow(window);
    SDL_Quit;
  end;

begin
  main;
end.
