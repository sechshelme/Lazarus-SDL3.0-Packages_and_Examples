program Project1;

// https://wiki.libsdl.org/SDL3/SDL_GetWindowSurface

uses
  SDL3,
  SDL3_ttf;

  function Createsurface: PSDL_Surface;
  begin
    Result := SDL_LoadBMP('mauer.bmp');
    if Result = nil then begin
      SDL_Log('Konnte BMP nicht laden!:  %s', SDL_GetError);
    end;
  end;

  function LoafFont: PSDL_Surface;
  const
    fileName = '/usr/share/fonts/truetype/freefont/FreeMono.ttf';
    //  fileName = '/usr/share/fonts/truetype/noto/NotoSansMono-Bold.ttf';
    //  fileName = '/usr/share/fonts/truetype/ubuntu/Ubuntu-MI.ttf';
    //    fileName = '/usr/share/wine/fonts/courier.ttf';

    hello=                              'Hello World !\n'#10'Hallo Welt !';
  var
    font: PTTF_Font;
    fg, bg: TSDL_Color;
    w, h: Longint;

  begin
    font := TTF_OpenFont(fileName, 20);
    if font = nil then begin
      SDL_Log('Konnte Font nicht laden!:  %s', SDL_GetError);
    end;
    fg.items := [$FF, $00, $00, $FF];
    bg.items := [$00, $FF, $00, $FF];
//    Result := TTF_RenderText(font, hello, fg, bg);
    Result := TTF_RenderText_Solid_Wrapped(font, hello, fg, 40);
//    Result := TTF_RenderUTF8(font, 'Hello World !'#10'Hallo Welt !', fg, bg);
 TTF_SizeText(font,hello,@w,@h);
 SDL_Log('Text size: %i x %i',w,h);

    TTF_CloseFont(font);
  end;

  procedure main;
  var
    window: PSDL_Window;
    winSurface, imageSurface, FontSurface: PSDL_Surface;
    rSrc, rDest: TSDL_Rect;
    quit: boolean = False;
    event: TSDL_Event;
  begin
    if SDL_init(SDL_INIT_VIDEO) < 0 then begin
      SDL_Log('Konnte SDL-VIDEO nicht laden!:  %s', SDL_GetError);
    end;

    window := SDL_CreateWindow('Surface Example', 640, 480, 0);
    if window = nil then begin
      SDL_Log('Konnte kein Windows erzeugen!:  %s', SDL_GetError);
    end;

    winSurface := SDL_GetWindowSurface(window);
    if winSurface = nil then begin
      SDL_Log('Konnte kein Surface erzeugen!:  %s', SDL_GetError);
    end;

    if TTF_Init < 0 then begin
      SDL_Log('Konnte TTF nicht inizialisieren !:  %s', TTF_GetError);
    end;

    imageSurface := Createsurface;
    FontSurface := LoafFont;

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
      rSrc.items := [0, 0, 20, 20];
      SDL_BlitSurfaceScaled(imageSurface, nil, winSurface, @rDest, SDL_SCALEMODE_NEAREST);
      SDL_BlitSurface(imageSurface, nil, winSurface, nil);
      SDL_BlitSurface(FontSurface, nil, winSurface, nil);
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
