unit SDL3_messagebox;

interface

uses
  SDL3_video;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  PSDL_MessageBoxFlags = ^TSDL_MessageBoxFlags;
  TSDL_MessageBoxFlags = longint;

const
  SDL_MESSAGEBOX_ERROR = $00000010;
  SDL_MESSAGEBOX_WARNING = $00000020;
  SDL_MESSAGEBOX_INFORMATION = $00000040;
  SDL_MESSAGEBOX_BUTTONS_LEFT_TO_RIGHT = $00000080;
  SDL_MESSAGEBOX_BUTTONS_RIGHT_TO_LEFT = $00000100;

type
  PSDL_MessageBoxButtonFlags = ^TSDL_MessageBoxButtonFlags;
  TSDL_MessageBoxButtonFlags = longint;

const
  SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT = $00000001;
  SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT = $00000002;

type
  PSDL_MessageBoxButtonData = ^TSDL_MessageBoxButtonData;

  TSDL_MessageBoxButtonData = record
    flags: uint32;
    buttonID: longint;
    Text: PChar;
  end;

  PSDL_MessageBoxColor = ^TSDL_MessageBoxColor;

  TSDL_MessageBoxColor = record
    r: uint8;
    g: uint8;
    b: uint8;
  end;

  PSDL_MessageBoxColorType = ^TSDL_MessageBoxColorType;
  TSDL_MessageBoxColorType = longint;

const
  SDL_MESSAGEBOX_COLOR_BACKGROUND = 0;
  SDL_MESSAGEBOX_COLOR_TEXT = 1;
  SDL_MESSAGEBOX_COLOR_BUTTON_BORDER = 2;
  SDL_MESSAGEBOX_COLOR_BUTTON_BACKGROUND = 3;
  SDL_MESSAGEBOX_COLOR_BUTTON_SELECTED = 4;
  SDL_MESSAGEBOX_COLOR_MAX = 5;

type
  PSDL_MessageBoxColorScheme = ^TSDL_MessageBoxColorScheme;

  TSDL_MessageBoxColorScheme = record
    colors: array[0..(SDL_MESSAGEBOX_COLOR_MAX) - 1] of TSDL_MessageBoxColor;
  end;

  PSDL_MessageBoxData = ^TSDL_MessageBoxData;

  TSDL_MessageBoxData = record
    flags: uint32;
    window: PSDL_Window;
    title: PChar;
    message: PChar;
    numbuttons: longint;
    Buttons: PSDL_MessageBoxButtonData;
    colorScheme: PSDL_MessageBoxColorScheme;
  end;

function SDL_ShowMessageBox(messageboxdata: PSDL_MessageBoxData; buttonid: Plongint): longint; cdecl; external;
function SDL_ShowSimpleMessageBox(flags: uint32; title: PChar; message: PChar; window: PSDL_Window): longint; cdecl; external;

implementation

end.
