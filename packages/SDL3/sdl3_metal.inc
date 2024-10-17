/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   19:37                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_MetalView = ^TSDL_MetalView;
  TSDL_MetalView = pointer;

function SDL_Metal_CreateView(window: PSDL_Window): TSDL_MetalView; cdecl; external sdl3_lib;
procedure SDL_Metal_DestroyView(view: TSDL_MetalView); cdecl; external sdl3_lib;
function SDL_Metal_GetLayer(view: TSDL_MetalView): pointer; cdecl; external sdl3_lib;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}
