/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 22-10-2024   15:23                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_LogCategory = ^TSDL_LogCategory;
  TSDL_LogCategory = longint;

const
  SDL_LOG_CATEGORY_APPLICATION = 0;
  SDL_LOG_CATEGORY_ERROR = 1;
  SDL_LOG_CATEGORY_ASSERT = 2;
  SDL_LOG_CATEGORY_SYSTEM = 3;
  SDL_LOG_CATEGORY_AUDIO = 4;
  SDL_LOG_CATEGORY_VIDEO = 5;
  SDL_LOG_CATEGORY_RENDER = 6;
  SDL_LOG_CATEGORY_INPUT = 7;
  SDL_LOG_CATEGORY_TEST = 8;
  SDL_LOG_CATEGORY_GPU = 9;
  SDL_LOG_CATEGORY_RESERVED2 = 10;
  SDL_LOG_CATEGORY_RESERVED3 = 11;
  SDL_LOG_CATEGORY_RESERVED4 = 12;
  SDL_LOG_CATEGORY_RESERVED5 = 13;
  SDL_LOG_CATEGORY_RESERVED6 = 14;
  SDL_LOG_CATEGORY_RESERVED7 = 15;
  SDL_LOG_CATEGORY_RESERVED8 = 16;
  SDL_LOG_CATEGORY_RESERVED9 = 17;
  SDL_LOG_CATEGORY_RESERVED10 = 18;
  SDL_LOG_CATEGORY_CUSTOM = 19;

type
  PSDL_LogPriority = ^TSDL_LogPriority;
  TSDL_LogPriority = longint;

const
  SDL_LOG_PRIORITY_INVALID = 0;
  SDL_LOG_PRIORITY_TRACE = 1;
  SDL_LOG_PRIORITY_VERBOSE = 2;
  SDL_LOG_PRIORITY_DEBUG = 3;
  SDL_LOG_PRIORITY_INFO = 4;
  SDL_LOG_PRIORITY_WARN = 5;
  SDL_LOG_PRIORITY_ERROR = 6;
  SDL_LOG_PRIORITY_CRITICAL = 7;
  SDL_LOG_PRIORITY_COUNT = 8;

procedure SDL_SetLogPriorities(priority: TSDL_LogPriority); cdecl; external libSDL3;
procedure SDL_SetLogPriority(category: longint; priority: TSDL_LogPriority); cdecl; external libSDL3;
function SDL_GetLogPriority(category: longint): TSDL_LogPriority; cdecl; external libSDL3;
procedure SDL_ResetLogPriorities; cdecl; external libSDL3;
function SDL_SetLogPriorityPrefix(priority: TSDL_LogPriority; prefix: pansichar): Tbool; cdecl; external libSDL3;
procedure SDL_Log(fmt: PChar); varargs; cdecl; external libSDL3 name 'SDL_Log';
procedure SDL_LogTrace(category: longint; fmt: pansichar); varargs cdecl; external libSDL3;
procedure SDL_LogVerbose(category: longint; fmt: pansichar); varargs cdecl; external libSDL3;
procedure SDL_LogDebug(category: longint; fmt: pansichar); varargs cdecl; external libSDL3;
procedure SDL_LogInfo(category: longint; fmt: pansichar); varargs cdecl; external libSDL3;
procedure SDL_LogWarn(category: longint; fmt: pansichar); varargs cdecl; external libSDL3;
procedure SDL_LogError(category: longint; fmt: pansichar); varargs cdecl; external libSDL3;
procedure SDL_LogCritical(category: longint; fmt: pansichar); varargs cdecl; external libSDL3;
procedure SDL_LogMessage(category: longint; priority: TSDL_LogPriority; fmt: pansichar); varargs cdecl; external libSDL3;
procedure SDL_LogMessageV(category: longint; priority: TSDL_LogPriority; fmt: pansichar; ap: Tva_list); cdecl; external libSDL3;

type
  TSDL_LogOutputFunction = procedure(userdata: pointer; category: longint; priority: TSDL_LogPriority; message: pansichar); cdecl;
  PSDL_LogOutputFunction = ^TSDL_LogOutputFunction;

function SDL_GetDefaultLogOutputFunction:TSDL_LogOutputFunction;cdecl;external libSDL3;
procedure SDL_GetLogOutputFunction(callback: PSDL_LogOutputFunction; userdata: Ppointer); cdecl; external libSDL3;
procedure SDL_SetLogOutputFunction(callback: TSDL_LogOutputFunction; userdata: pointer); cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}
