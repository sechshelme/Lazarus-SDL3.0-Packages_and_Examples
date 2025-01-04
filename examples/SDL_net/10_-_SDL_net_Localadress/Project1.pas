program Project1;

uses
  sdl3,
  SDL3_net;

var
  num_addrs: longint;
  i: integer;
  addrs: PPSDLNet_Address;

begin
  if SDLNet_Init < 0 then begin
    SDL_Log('Fehler: SDLNet_Init: %s', SDL_GetError());
    halt;
  end;

  addrs := SDLNet_GetLocalAddresses(@num_addrs);
  if addrs = nil then begin
    SDL_Log('Fehler: LocalAdress: %s', SDL_GetError());
    halt;
  end;

  for i := 0 to num_addrs - 1 do begin
    SDL_Log('  - %s', SDLNet_GetAddressString(addrs[i]));
  end;

  SDLNet_FreeLocalAddresses(addrs);
  SDLNet_Quit();
end.
