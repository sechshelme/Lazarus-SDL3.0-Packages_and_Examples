unit SDL3;

interface

uses
  {$IFDEF Linux}
  x, xlib,
  {$ENDIF}
  {$IFDEF Windows}
  Windows,
  {$ENDIF}
  ctypes;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

const
  {$IFDEF Linux}
  libSDL3 = 'SDL3';
  {$ENDIF}

  {$IFDEF Windows}
  libSDL3 = 'SDL3';
  {$ENDIF}

  {$IFDEF Darwin}
  libSDL3 = 'libSDL3.dylib';
  {$ENDIF}

  {$DEFINE read_interface}
  {$include SDL3_includes.inc}
  {$UNDEF read_interface}


implementation

{$DEFINE read_implementation}
{$include SDL3_includes.inc}
{$UNDEF read_implementation}

end.
