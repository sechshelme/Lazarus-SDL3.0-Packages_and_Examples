unit SDL_test_fuzzer;

interface

uses
  ctypes, SDL3;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

procedure SDLTest_FuzzerInit(execKey:TUint64);cdecl;external ;
function SDLTest_RandomUint8:TUint8;cdecl;external ;
function SDLTest_RandomSint8:TSint8;cdecl;external ;
function SDLTest_RandomUint16:TUint16;cdecl;external ;
function SDLTest_RandomSint16:TSint16;cdecl;external ;
function SDLTest_RandomSint32:TSint32;cdecl;external ;
function SDLTest_RandomUint32:TUint32;cdecl;external ;
function SDLTest_RandomUint64:TUint64;cdecl;external ;
function SDLTest_RandomSint64:TSint64;cdecl;external ;
function SDLTest_RandomUnitFloat:single;cdecl;external ;
function SDLTest_RandomUnitDouble:Tdouble;cdecl;external ;
function SDLTest_RandomFloat:single;cdecl;external ;
function SDLTest_RandomDouble:Tdouble;cdecl;external ;

function SDLTest_RandomUint8BoundaryValue(boundary1:TUint8; boundary2:TUint8; validDomain:Tbool):TUint8;cdecl;external ;
function SDLTest_RandomUint16BoundaryValue(boundary1:TUint16; boundary2:TUint16; validDomain:Tbool):TUint16;cdecl;external ;
function SDLTest_RandomUint32BoundaryValue(boundary1:TUint32; boundary2:TUint32; validDomain:Tbool):TUint32;cdecl;external ;
function SDLTest_RandomUint64BoundaryValue(boundary1:TUint64; boundary2:TUint64; validDomain:Tbool):TUint64;cdecl;external ;
function SDLTest_RandomSint8BoundaryValue(boundary1:TSint8; boundary2:TSint8; validDomain:Tbool):TSint8;cdecl;external ;
function SDLTest_RandomSint16BoundaryValue(boundary1:TSint16; boundary2:TSint16; validDomain:Tbool):TSint16;cdecl;external ;
function SDLTest_RandomSint32BoundaryValue(boundary1:TSint32; boundary2:TSint32; validDomain:Tbool):TSint32;cdecl;external ;
function SDLTest_RandomSint64BoundaryValue(boundary1:TSint64; boundary2:TSint64; validDomain:Tbool):TSint64;cdecl;external ;
function SDLTest_RandomIntegerInRange(min:TSint32; max:TSint32):TSint32;cdecl;external ;
function SDLTest_RandomAsciiString:Pansichar;cdecl;external ;
function SDLTest_RandomAsciiStringWithMaximumLength(maxLength:longint):Pansichar;cdecl;external ;
function SDLTest_RandomAsciiStringOfSize(size:longint):Pansichar;cdecl;external ;
function SDLTest_GetFuzzerInvocationCount:longint;cdecl;external ;

implementation

end.
