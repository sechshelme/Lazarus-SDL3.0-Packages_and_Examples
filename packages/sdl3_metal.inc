/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_MetalView = ^TSDL_MetalView;
  TSDL_MetalView = pointer;

function SDL_Metal_CreateView(window: PSDL_Window): TSDL_MetalView; cdecl; external;
procedure SDL_Metal_DestroyView(view: TSDL_MetalView); cdecl; external;
function SDL_Metal_GetLayer(view: TSDL_MetalView): pointer; cdecl; external;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}
