program Project1;

// https://wiki.libsdl.org/SDL3/SDL_CreateCursor

uses
  SDL3,
  ctypes;

  function init_system_cursor: PSDL_Cursor;
  const
    arrow: array of PChar = (
      'X',
      'XX',
      'X.X',
      'X..X',
      'X...X',
      'X....X',
      'X.....X',
      'X......X',
      'X.......X',
      'X........X',
      'X.....XXXXX',
      'X..X..X',
      'X.X X..X',
      'XX  X..X',
      'X    X..X',
      '     X..X',
      '      X..X',
      '      X..X',
      '       XX');
    size = 32;
  type
    TByteArray = array [0..(size div 8) * size - 1] of byte;
  var
    mask: TByteArray;
    Data: TByteArray;
    col, row: integer;
    i: integer = -1;
  begin
    WriteLn(SizeOf(Data));
    Data := Default(TByteArray);
    mask := Default(TByteArray);
    for row := 0 to Length(arrow) - 1 do begin
      for col := 0 to size - 1 do begin
        if col mod 8 <> 0 then begin
          Data[i] := Data[i] shl 1;
          mask[i] := mask[i] shl 1;
        end else begin
          Inc(i);
        end;
        if col < Length(arrow[row]) then begin
          case arrow[row, col] of
            'X': begin
              Data[i] := Data[i] or $01;
              mask[i] := mask[i] or $01;
            end;
            '.': begin
              mask[i] := mask[i] or $01;
            end;
          end;
        end;
      end;
    end;
    Result := SDL_CreateCursor(Data, mask, size, size, 0, 0);
  end;

var
  win: PSDL_Window;
  renderer: PSDL_Renderer;
  customCursor: PSDL_Cursor;

  procedure Init;
  begin
    SDL_Init(SDL_INIT_VIDEO);
    win := SDL_CreateWindow('Cursor', 640, 480, SDL_WINDOW_RESIZABLE);
    if win = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'Konnte Fenster nicht erzeugen !');
      Halt(-1);
    end;

    renderer := SDL_CreateRenderer(win, nil, SDL_RENDERER_ACCELERATED);
    if renderer = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'Kann kein SDL-Renderer erzeugen !');
      Halt(-1);
    end;

    customCursor := init_system_cursor;
    if customCursor = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'Kann kein Cursor laden !');
      Halt(-1);
    end;
    SDL_SetCursor(customCursor);
  end;

  procedure Render;
  begin
    SDL_SetRenderDrawColor(renderer, $FF, $80, $80, 255);
    SDL_RenderClear(renderer);
    SDL_RenderPresent(renderer);
  end;

  procedure Event;
  var
    quit: boolean = False;
    e: TSDL_Event;
    sym: TSDL_KeyCode;
  begin
    while not quit do begin
      while SDL_PollEvent(@e) do begin
        case e.type_ of
          SDL_EVENT_MOUSE_BUTTON_DOWN: begin
            case e.button.button of
              SDL_BUTTON_LEFT: begin
              end;
              SDL_BUTTON_RIGHT: begin
              end;
            end;
          end;
          SDL_EVENT_KEY_DOWN: begin
            sym := e.key.keysym.sym;
            case sym of
              SDLK_ESCAPE, SDLK_AC_BACK: begin
                quit := True;
              end;
            end;
          end;
          SDL_EVENT_QUIT: begin
            quit := True;
          end;
        end;
      end;

      Render;
    end;
  end;

  procedure Destroy;
  begin
    SDL_DestroyCursor(customCursor);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(win);
  end;

begin
  Init;
  Event;
  Destroy;
  SDL_Log('Ende');
end.
