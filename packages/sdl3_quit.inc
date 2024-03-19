/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

function SDL_QuitRequested: TSDL_bool;   // ????

// #define SDL_QuitRequested() (SDL_PumpEvents(), (SDL_PeepEvents(NULL,0,SDL_PEEKEVENT,SDL_EVENT_QUIT,SDL_EVENT_QUIT) > 0))

{$ENDIF read_interface}


{$IFDEF read_implementation}

function SDL_QuitRequested: TSDL_bool;
begin
  SDL_PumpEvents();
  Result := TSDL_bool(SDL_PeepEvents(nil, 0, SDL_PEEKEVENT, SDL_EVENT_QUIT, SDL_EVENT_QUIT) > 0);
end;

{$ENDIF read_implementation}
