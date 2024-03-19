unit SDL3_quit;

interface

uses
  SDL3_stdinc, SDL3_events;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

function SDL_QuitRequested: TSDL_bool;   // ????

// #define SDL_QuitRequested() (SDL_PumpEvents(), (SDL_PeepEvents(NULL,0,SDL_PEEKEVENT,SDL_EVENT_QUIT,SDL_EVENT_QUIT) > 0))

implementation

function SDL_QuitRequested: TSDL_bool;
begin
  SDL_PumpEvents();
  Result := TSDL_bool(SDL_PeepEvents(nil, 0, SDL_PEEKEVENT, SDL_EVENT_QUIT, SDL_EVENT_QUIT) > 0);
end;

end.
