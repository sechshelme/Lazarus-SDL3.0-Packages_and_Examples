program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  ctypes,
  SDL3;

var
  Count: longint;
  list: PPChar;
  i: integer;
begin
  SDL_init(SDL_INIT_VIDEO);

//  list := SDL_GlobDirectory('c:\windows', '*.*', SDL_PATHTYPE_FILE, @Count);
  list := SDL_GlobDirectory('/usr/local/lib/', '*.*', SDL_PATHTYPE_FILE, @Count);

  if list = nil then begin
    SDL_Log('file not found');
  end else begin

    for i := 0 to Count - 1 do begin
      SDL_Log('%3d.  %s', i, list[i]);
    end;

    SDL_free(list);
  end;

  SDL_Quit;
end.
