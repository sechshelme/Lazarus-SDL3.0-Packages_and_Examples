program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

// {$UNITPATH ../units/units}

//uses
//  SDL2;

const
//  SDL_LibName = '/usr/local/lib/libSDL';
    SDL_LibName = 'libSDL3.so.0';
//  SDL_LibName = 'libSDL2.so.0';

type
  cuint8 = byte;
  PSDL_Version = ^TSDL_Version;

  TSDL_Version = record
    major,           {**< major version *}
    minor,           {**< minor version *}
    patch: cuint8;   {**< update version *}
  end;

  procedure SDL_GetVersion(ver: PSDL_Version); cdecl; external SDL_LibName;

var
  ver: TSDL_Version;

begin
  //  SDL_VERSION(ver);
  //WriteLn(ver.major,' .',ver.minor,'.',ver.patch);
  SDL_GetVersion(@ver);
  WriteLn(ver.major, '.', ver.minor, '.', ver.patch);
end.
