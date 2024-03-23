unit Unit1;

interface

{$LinkLib 'SDL3'}

uses
  ctypes;

type
  PSDL_Version = ^TSDL_Version;

  TSDL_Window = record
  end;

  PSDL_Window = ^TSDL_Window;

  TSDL_Version = record
    major,           {**< major version *}
    minor,           {**< minor version *}
    patch: cuint8;   {**< update version *}
  end;

const
  SDL_INIT_VIDEO = $00000020; //,  /**< `SDL_INIT_VIDEO` implies `SDL_INIT_EVENTS` */
  SDL_WINDOW_RESIZABLE = $00000020;

procedure SDL_GetVersion(ver: PSDL_Version); cdecl; external;

function SDL_Init(flags: cuint32): cint; cdecl; external;
function SDL_CreateWindow(title: PChar; w, h: cint; flags: cuint32): PSDL_Window; cdecl; external;
function SDL_DestroyWindow(window: PSDL_Window): Pointer; cdecl; external;
procedure SDL_Delay(ms: cuint32); cdecl; external;


implementation

end.

