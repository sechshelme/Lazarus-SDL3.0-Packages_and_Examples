unit SDL_test_md5;

interface

uses
  ctypes, SDL3;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  PMD5UINT4 = ^TMD5UINT4;
  TMD5UINT4 = TUint32;

  PSDLTest_Md5Context = ^TSDLTest_Md5Context;

  TSDLTest_Md5Context = record
    i: array[0..1] of TMD5UINT4;
    buf: array[0..3] of TMD5UINT4;
    in_: array[0..63] of ansichar;
    digest: array[0..15] of ansichar;
  end;

procedure SDLTest_Md5Init(mdContext: PSDLTest_Md5Context); cdecl; external;
procedure SDLTest_Md5Update(mdContext: PSDLTest_Md5Context; inBuf: pansichar; inLen: dword); cdecl; external;
procedure SDLTest_Md5Final(mdContext: PSDLTest_Md5Context); cdecl; external;

implementation


end.
