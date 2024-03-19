/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

const
  SDL_ASSERT_LEVEL = 1;

type
  PSDL_AssertState = ^TSDL_AssertState;
  TSDL_AssertState = longint;

const
  SDL_ASSERTION_RETRY = 0;
  SDL_ASSERTION_BREAK = 1;
  SDL_ASSERTION_ABORT = 2;
  SDL_ASSERTION_IGNORE = 3;
  SDL_ASSERTION_ALWAYS_IGNORE = 4;

type
  PSDL_AssertData = ^TSDL_AssertData;

  TSDL_AssertData = record
    always_ignore: longint;
    trigger_count: dword;
    condition: PChar;
    filename: PChar;
    linenum: longint;
    _function: PChar;
    Next: PSDL_AssertData;
  end;

function SDL_ReportAssertion(Data: PSDL_AssertData; func: PChar; file_: PChar; line: longint): TSDL_AssertState; cdecl; external;

type
  TSDL_AssertionHandler = function(Data: PSDL_AssertData; userdata: pointer): TSDL_AssertState; cdecl;

procedure SDL_SetAssertionHandler(handler: TSDL_AssertionHandler; userdata: pointer); cdecl; external;
function SDL_GetDefaultAssertionHandler: TSDL_AssertionHandler; cdecl; external;
function SDL_GetAssertionHandler(puserdata: Ppointer): TSDL_AssertionHandler; cdecl; external;
function SDL_GetAssertionReport: PSDL_AssertData; cdecl; external;
procedure SDL_ResetAssertionReport; cdecl; external;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}
