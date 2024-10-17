unit SDL3;

//{$modeswitch typehelpers}

interface

uses
  {$IFDEF Linux}
  x, xlib,
  {$ENDIF}
  ctypes;

const
  {$IFDEF Linux}
  sdl3_lib = 'SDL3';
  {$ENDIF}

  {$IFDEF Windows}
  sdl3_lib = 'SDL3.dll';
  {$ENDIF}

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

  {$DEFINE read_interface}
  {$include SDL3_includes.inc}
  {$UNDEF read_interface}


implementation

{$DEFINE read_implementation}
{$include SDL3_includes.inc}
{$UNDEF read_implementation}

end.

