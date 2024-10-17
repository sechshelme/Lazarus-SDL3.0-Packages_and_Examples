/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   17:58                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_MessageBoxFlags = ^TSDL_MessageBoxFlags;
  TSDL_MessageBoxFlags = TUint32;

const
  SDL_MESSAGEBOX_ERROR = $00000010;
  SDL_MESSAGEBOX_WARNING = $00000020;
  SDL_MESSAGEBOX_INFORMATION = $00000040;
  SDL_MESSAGEBOX_BUTTONS_LEFT_TO_RIGHT = $00000080;
  SDL_MESSAGEBOX_BUTTONS_RIGHT_TO_LEFT = $00000100;

type
  PSDL_MessageBoxButtonFlags = ^TSDL_MessageBoxButtonFlags;
  TSDL_MessageBoxButtonFlags = TUint32;

const
  SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT = $00000001;
  SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT = $00000002;

type
  TSDL_MessageBoxButtonData = record
    flags: TSDL_MessageBoxButtonFlags;
    buttonID: longint;
    Text: pansichar;
  end;
  PSDL_MessageBoxButtonData = ^TSDL_MessageBoxButtonData;

  TSDL_MessageBoxColor = record
    r: TUint8;
    g: TUint8;
    b: TUint8;
  end;
  PSDL_MessageBoxColor = ^TSDL_MessageBoxColor;

  PSDL_MessageBoxColorType = ^TSDL_MessageBoxColorType;
  TSDL_MessageBoxColorType = longint;

const
  SDL_MESSAGEBOX_COLOR_BACKGROUND = 0;
  SDL_MESSAGEBOX_COLOR_TEXT = 1;
  SDL_MESSAGEBOX_COLOR_BUTTON_BORDER = 2;
  SDL_MESSAGEBOX_COLOR_BUTTON_BACKGROUND = 3;
  SDL_MESSAGEBOX_COLOR_BUTTON_SELECTED = 4;
  SDL_MESSAGEBOX_COLOR_COUNT = 5;

type
  TSDL_MessageBoxColorScheme = record
    colors: array[0..(SDL_MESSAGEBOX_COLOR_COUNT) - 1] of TSDL_MessageBoxColor;
  end;
  PSDL_MessageBoxColorScheme = ^TSDL_MessageBoxColorScheme;

  TSDL_MessageBoxData = record
    flags: TSDL_MessageBoxFlags;
    window: PSDL_Window;
    title: pansichar;
    message: pansichar;
    numbuttons: longint;
    Buttons: PSDL_MessageBoxButtonData;
    colorScheme: PSDL_MessageBoxColorScheme;
  end;
  PSDL_MessageBoxData = ^TSDL_MessageBoxData;

function SDL_ShowMessageBox(messageboxdata: PSDL_MessageBoxData; buttonid: Plongint): Tbool; cdecl; external libSDL3;
function SDL_ShowSimpleMessageBox(flags: TSDL_MessageBoxFlags; title: pansichar; message: pansichar; window: PSDL_Window): Tbool; cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}
