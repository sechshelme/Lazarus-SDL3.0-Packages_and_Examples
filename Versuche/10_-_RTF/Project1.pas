program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  ctypes,
  SDL3,
  SDL_ttf,
  SDL_rtf;

var
  window: PSDL_Window;
  bitmapSurface: PSDL_Surface;
  renderer: PSDL_Renderer;
  bitmapTex: PSDL_Texture;
  ctx: PRTF_Context;
  fontEngine: TRTF_FontEngine;

  function CreateSurface: PSDL_Surface;
  var
    r: TSDL_Rect;
  begin

    Result := SDL_LoadBMP('mauer.bmp');
    if Result = nil then begin
      SDL_Log('Kann keine textur erzeugen !');
    end;
    r.x := 1;
    r.y := 1;
    r.w := 2;
    r.h := 2;
    SDL_FillSurfaceRect(Result, @r, $FFFF);
  end;

  procedure SDLMain;
  const
    step = 0.01;
  var
    e: TSDL_Event;
    quit: boolean = False;
    rSrc, rDest: TSDL_FRect;
    keyStat: PUInt8;
  begin
    rDest.x := 0;
    rDest.y := 0;
    rDest.w := 100;
    rDest.h := 100;
    while not quit do begin
      keyStat := SDL_GetKeyboardState(nil);
      if keyStat[SDL_SCANCODE_SPACE] <> 0 then begin
      end;

      if keyStat[SDL_SCANCODE_RIGHT] <> 0 then begin
        if keyStat[SDL_SCANCODE_LSHIFT] <> 0 then begin
          rDest.x -= step;
          rDest.w += step * 2;
        end else begin
          rDest.x += step;
        end;
      end;
      if keyStat[SDL_SCANCODE_LEFT] <> 0 then begin
        if keyStat[SDL_SCANCODE_LSHIFT] <> 0 then begin
          if rDest.w > 1 then begin
            rDest.x += step;
            rDest.w -= step * 2;
          end;
        end else begin
          rDest.x -= step;
        end;
      end;
      if keyStat[SDL_SCANCODE_DOWN] <> 0 then begin
        if keyStat[SDL_SCANCODE_LSHIFT] <> 0 then begin
          rDest.y -= step;
          rDest.h += step * 2;
        end else begin
          rDest.y += step;
        end;
      end;
      if keyStat[SDL_SCANCODE_UP] <> 0 then begin
        if keyStat[SDL_SCANCODE_LSHIFT] <> 0 then begin
          if rDest.h > 1 then begin
            rDest.y += step;
            rDest.h -= step * 2;
          end;
        end else begin
          rDest.y -= step;
        end;
      end;

      if rDest.h < 1 then begin
        rDest.h := 1;
      end;


      while SDL_PollEvent(@e) do begin
        case e.type_ of
          SDL_EVENT_KEY_DOWN: begin
            case e.key.keysym.sym of

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

      SDL_RenderClear(renderer);
      //   SDL_RenderTexture(renderer, bitmapTex, nil, @distrect);
      rSrc.x := 0;
      rSrc.y := 0;
      rSrc.w := 400;
      rSrc.h := 400;

      SDL_RenderTexture(renderer, bitmapTex, @rSrc, @rDest);
      SDL_RenderPresent(renderer);
    end;
  end;

function CreateFont(name: Pchar; family: TRTF_FontFamily; charset: longint;  size: longint; style: longint): pointer; cdecl;
var
  font: PTTF_Font;
begin
  font:=TTF_OpenFont('lazy.ttf',size);
  if font = nil then begin
    SDL_Log('Kann kein Font laden !    %s', TTF_GetError);
  end;

  WriteLn('CreateFont');

  Result:=font;
end;

function GetLineSpacing(font: pointer): longint; cdecl;
var
  font2: PTTF_Font;
begin
    font2:=PTTF_Font(font);
    Result:=TTF_FontLineSkip(font2);
    WriteLn('GetLineSpacing');
end;

function GetCharacterOffsets(font: pointer; text: Pchar; byteOffsets: Plongint;  pixelOffsets: Plongint; maxOffsets: longint): longint; cdecl;
begin
   Result:=10;
   WriteLn('Offset');
end;

function RenderText(font: pointer; renderer: PSDL_Renderer; text: Pchar;  fg: TSDL_Color): PSDL_Texture; cdecl;
begin
     Result:=nil;
     WriteLn('Rendertext');
end;

procedure FreeFont(font: pointer); cdecl;
begin
  WriteLn('CloseFont');
     TTF_CloseFont(font);
end;

begin
  SDL_init(SDL_INIT_VIDEO);
  TTF_Init;

  window := SDL_CreateWindow('SDL3 Window', 800, 600, SDL_WINDOW_RESIZABLE);
  if window = nil then begin
    SDL_Log('Kann kein SDL-Fenster erzeugen !');
  end;

  renderer := SDL_CreateRenderer(window, nil, SDL_RENDERER_ACCELERATED);
  if renderer = nil then begin
    SDL_Log('Kann kein SDL-Renderer erzeugen !');
  end;

  fontEngine.version := RTF_FONT_ENGINE_VERSION;
  fontEngine.CreateFont:=@CreateFont;
  fontEngine.GetLineSpacing:=@GetLineSpacing;
  fontEngine.GetCharacterOffsets:=@GetCharacterOffsets;
  fontEngine.RenderText:=@RenderText;
  fontEngine.FreeFont:=@FreeFont;

  ctx := RTF_CreateContext(renderer, @fontEngine);
  if ctx = nil then begin
    SDL_Log('Kann kein rtf erzeugen !    %s', RTF_GetError);
  end;


  SDL_Log('io');
  if RTF_Load(ctx, 'text.rtf') <> 0 then  begin
    SDL_Log('Kann kein RTF-Datei nicht laden !    %s', RTF_GetError);
  end;
  SDL_Log('io');

  bitmapSurface := CreateSurface;

  bitmapTex := SDL_CreateTextureFromSurface(renderer, bitmapSurface);
  if bitmapSurface = nil then begin
    SDL_Log('Kann bmp nicht laden !');
  end;

  SDL_DestroySurface(bitmapSurface);

  SDLMain;

  SDL_DestroyTexture(bitmapTex);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  SDL_Quit;
end.
