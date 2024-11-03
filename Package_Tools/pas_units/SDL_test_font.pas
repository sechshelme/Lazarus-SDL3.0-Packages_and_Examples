unit SDL_test_font;

interface

uses
  ctypes, SDL3;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

var
  FONT_CHARACTER_SIZE: longint; cvar;external;

  //const
  //  FONT_LINE_HEIGHT = FONT_CHARACTER_SIZE + 2;

function SDLTest_DrawCharacter(renderer: PSDL_Renderer; x: single; y: single; c: TUint32): Tbool; cdecl; external;
function SDLTest_DrawString(renderer: PSDL_Renderer; x: single; y: single; s: pansichar): Tbool; cdecl; external;

type
  PSDLTest_TextWindow = ^TSDLTest_TextWindow;

  TSDLTest_TextWindow = record
    rect: TSDL_FRect;
    current: longint;
    numlines: longint;
    Lines: ^pansichar;
  end;

function SDLTest_TextWindowCreate(x: single; y: single; w: single; h: single): PSDLTest_TextWindow; cdecl; external;
procedure SDLTest_TextWindowDisplay(textwin: PSDLTest_TextWindow; renderer: PSDL_Renderer); cdecl; external;
procedure SDLTest_TextWindowAddText(textwin: PSDLTest_TextWindow; fmt: pansichar);varargs cdecl; external;
procedure SDLTest_TextWindowAddTextWithLength(textwin: PSDLTest_TextWindow; Text: pansichar; len: Tsize_t); cdecl; external;
procedure SDLTest_TextWindowClear(textwin: PSDLTest_TextWindow); cdecl; external;
procedure SDLTest_TextWindowDestroy(textwin: PSDLTest_TextWindow); cdecl; external;
procedure SDLTest_CleanupTextDrawing; cdecl; external;

function FONT_LINE_HEIGHT: longword;

implementation

function FONT_LINE_HEIGHT: longword;
begin
  Result := FONT_CHARACTER_SIZE + 2;
end;


end.
