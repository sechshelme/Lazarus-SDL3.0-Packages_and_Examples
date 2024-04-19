program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  ctypes,
  SDL3,
  SDL_ttf,
  SDL_rtf;

var
  window: PSDL_Window;
  renderer: PSDL_Renderer;
  ctx: PRTF_Context;
  fontEngine: TRTF_FontEngine;

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

      rSrc.x := 0;
      rSrc.y := 0;
      rSrc.w := 400;
      rSrc.h := 400;

      SDL_SetRenderDrawColor(renderer, $FF, $88, $FF, $FF);
      SDL_RenderClear(renderer);
      // WriteLn('main1');
      RTF_Render(ctx, nil, -10);
      //      WriteLn('main2');

      SDL_RenderPresent(renderer);
    end;
  end;

  function CreateFont(Name: PChar; family: TRTF_FontFamily; charset: longint; size: longint; style: longint): pointer; cdecl;
  var
    font: PTTF_Font;
  begin
    font := TTF_OpenFont('lazy.ttf', size);
    if font = nil then begin
      SDL_Log('Kann kein Font laden !    %s', TTF_GetError);
    end;

    WriteLn('CreateFont io.');
    Result := font;
  end;

  function GetLineSpacing(font: pointer): longint; cdecl;
  var
    font2: PTTF_Font;
  begin
    font2 := PTTF_Font(font);
    Result := TTF_FontLineSkip(font2);
    WriteLn('GetLineSpacing io.');
  end;

  function GetCharacterOffsets(font: pointer; Text: PChar; byteOffsets: Plongint; pixelOffsets: Plongint; maxOffsets: longint): longint; cdecl;
  var
    i: cint = 0;
  begin
    while (Text^ <> #0) and (i < maxOffsets) do begin
      byteOffsets[i] := i;
      pixelOffsets[i] := i;
      Inc(i);

      Inc(Text, 1);
    end;

    if i < maxOffsets then begin
      byteOffsets[i] := i;
      pixelOffsets[i] := i;
    end;

    SDL_Log('offset: %i',i );
    SDL_Log('max offset: %i', maxOffsets);
    Result := i;
  end;

  function RenderText(font: pointer; renderer: PSDL_Renderer; Text: PChar; fg: TSDL_Color): PSDL_Texture; cdecl;
  var
    font2: PTTF_Font;
    surface: PSDL_Surface;
  begin
    font2 := PTTF_Font(font);

    WriteLn('font2: ', PtrUInt(Text));
    WriteLn('font2: ', PtrUInt(renderer));

    WriteLn(Text);
//    Text := 'sdfgffdsgsfd';
    WriteLn(Text);

    Result := nil;
    surface := TTF_RenderUTF8_Blended(font2, Text, fg);
    if surface = nil then begin
      SDL_Log('Suface Fehler');
    end else begin
      Result := SDL_CreateTextureFromSurface(renderer, surface);
      SDL_DestroySurface(surface);
    end;

    WriteLn('Rendertext io.');
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
  fontEngine.CreateFont := @CreateFont;
  fontEngine.GetLineSpacing := @GetLineSpacing;
  fontEngine.GetCharacterOffsets := @GetCharacterOffsets;
  fontEngine.RenderText := @RenderText;
  fontEngine.FreeFont := @FreeFont;

  ctx := RTF_CreateContext(renderer, @fontEngine);
  if ctx = nil then begin
    SDL_Log('Kann kein rtf erzeugen !    %s', RTF_GetError);
  end;


  SDL_Log('io');
  if RTF_Load(ctx, 'text.rtf') <> 0 then  begin
    SDL_Log('Kann kein RTF-Datei nicht laden !    %s', RTF_GetError);
  end;
  SDL_Log('io');

  SDLMain;

  RTF_FreeContext(ctx);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  SDL_Quit;
end.
