
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
    always_ignore: Tbool;
    trigger_count: dword;
    condition: pansichar;
    filename: pansichar;
    linenum: longint;
    _function: pansichar;
    Next: PSDL_AssertData;
  end;

function SDL_ReportAssertion(Data: PSDL_AssertData; func: pansichar; file_: pansichar; line: longint): TSDL_AssertState; cdecl; external libSDL3;

type
  TSDL_AssertionHandler = function(Data: PSDL_AssertData; userdata: pointer): TSDL_AssertState; cdecl;

procedure SDL_SetAssertionHandler(handler: TSDL_AssertionHandler; userdata: pointer); cdecl; external libSDL3;
function SDL_GetDefaultAssertionHandler: TSDL_AssertionHandler; cdecl; external libSDL3;
function SDL_GetAssertionHandler(puserdata: Ppointer): TSDL_AssertionHandler; cdecl; external libSDL3;
function SDL_GetAssertionReport: PSDL_AssertData; cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}
