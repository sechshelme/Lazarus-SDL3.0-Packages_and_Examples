/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   17:58                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  TSDL_MetalView = pointer;
  PSDL_MetalView = ^TSDL_MetalView;

function SDL_Metal_CreateView(window: PSDL_Window): TSDL_MetalView; cdecl; external libSDL3;
procedure SDL_Metal_DestroyView(view: TSDL_MetalView); cdecl; external libSDL3;
function SDL_Metal_GetLayer(view: TSDL_MetalView): pointer; cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}
