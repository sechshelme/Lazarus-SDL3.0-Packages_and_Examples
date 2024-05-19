program Project1;

// https://wiki.libsdl.org/SDL3/SDL_GetWindowSurface

uses
  sysutils, SDL3;

procedure printFormat(sur:PSDL_Surface);
begin
  WriteLn('format: ',IntToHex(sur^.format^.format, 8));
  WriteLn('bit per pixel: ',sur^.format^.bits_per_pixel,'   bytes per pixel: ', sur^.format^.bytes_per_pixel);
  WriteLn('Rmask: ',IntToHex(sur^.format^.Rmask, 8),'  Gmask: ',IntToHex(sur^.format^.Gmask,8),'  Bmask: ',IntToHex(sur^.format^.Bmask,8),'  Amask: ',IntToHex(sur^.format^.Amask,8));
  WriteLn('Rshift: ',sur^.format^.Rshift:7,'  Gshift: ',sur^.format^.Gshift:7,'  Bshift: ',sur^.format^.Bshift:7,'  Ashift: ',sur^.format^.Ashift:7);
  WriteLn('Rloss: ',sur^.format^.Rloss:8,'  Gloss: ',sur^.format^.Gloss:8,'  Bloss: ',sur^.format^.Bloss:8,'  Aloss: ',sur^.format^.Aloss:8);
  WriteLn();
  end;

  procedure CreateSurface;
  var
    surface: PSDL_Surface;
  begin
    surface := SDL_LoadBMP('mauer.bmp');
    printFormat(surface);
    SDL_DestroySurface(surface);

        surface:=SDL_CreateSurface(128,128,SDL_PIXELFORMAT_ABGR8888);
        printFormat(surface);
        SDL_DestroySurface(surface);

            surface:=SDL_CreateSurface(128,128,SDL_PIXELFORMAT_RGBA4444);
            printFormat(surface);
            SDL_DestroySurface(surface);

            surface:=SDL_CreateSurface(128,128,SDL_PIXELFORMAT_RGB332);
            printFormat(surface);
            SDL_DestroySurface(surface);

            surface:=SDL_CreateSurface(128,128,SDL_PIXELFORMAT_RGB48);
            printFormat(surface);
            SDL_DestroySurface(surface);
            surface:=SDL_CreateSurface(128,128,SDL_PIXELFORMAT_XBGR1555);
            printFormat(surface);
            SDL_DestroySurface(surface);
  end;

  procedure main;
  begin
    if SDL_init(SDL_INIT_VIDEO) < 0 then begin
      SDL_Log('Konnte SDL-VIDEO nicht laden!:  %s', SDL_GetError);
    end;

    CreateSurface;

    SDL_Quit;
  end;

begin
  main;
end.
