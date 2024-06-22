program Project1;

uses
  SDL3;

type
  Tint24=0..$FFFFFF;

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
  WriteLn(PtrUInt(@data));
  WriteLn(PtrUInt(Result^.pixels));
end;

function CreateSurface3: PSDL_Surface;
const
  Data: array of Tint24 = (
    $000000, $FF0000, $00FF00, $0000FF,
    $444444, $FF4444, $44FF44, $4444FF,
    $888888, $FF8888, $88FF88, $8888FF,
    $AAAAAA, $FFAAAA, $AAFFAA, $AAAAAA);
begin
  Result := SDL_CreateSurfaceFrom(PDWord(Data), 4, 4, 16, SDL_PIXELFORMAT_RGBX8888);
  if Result = nil then begin
    SDL_Log('Konnte BMP nicht laden!:  %s', SDL_GetError);
  end;
  WriteLn(PtrUInt(@data));
  WriteLn(PtrUInt(Result^.pixels));
end;

  procedure main;
  var
    window: PSDL_Window;
    winSurface, imageSurface1, imageSurface2: PSDL_Surface;
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

    imageSurface1 := CreateSurface1;
    imageSurface2 := CreateSurface2;

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

      rDest.items := [100, 100, 200, 200];
      SDL_BlitSurfaceScaled(imageSurface1, nil, winSurface, @rDest, SDL_SCALEMODE_NEAREST);
      rDest.items := [350, 100, 200, 200];
      SDL_BlitSurfaceScaled(imageSurface2, nil, winSurface, @rDest, SDL_SCALEMODE_NEAREST);
      SDL_UpdateWindowSurface(window);
    end;

    SDL_DestroySurface(imageSurface1);
    SDL_DestroySurface(imageSurface2);
    SDL_DestroySurface(winSurface);
    SDL_DestroyWindow(window);
    SDL_Quit;
  end;

begin
  main;
end.
