unit SDL3_log;

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

const
  SDL_MAX_LOG_MESSAGE = 4096;

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
  SDL_LOG_CATEGORY_RESERVED1 = 9;
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
  SDL_LOG_PRIORITY_VERBOSE = 1;
  SDL_LOG_PRIORITY_DEBUG = 2;
  SDL_LOG_PRIORITY_INFO = 3;
  SDL_LOG_PRIORITY_WARN = 4;
  SDL_LOG_PRIORITY_ERROR = 5;
  SDL_LOG_PRIORITY_CRITICAL = 6;
  SDL_NUM_LOG_PRIORITIES = 7;

procedure SDL_LogSetAllPriority(priority: TSDL_LogPriority); cdecl; external;
procedure SDL_LogSetPriority(category: longint; priority: TSDL_LogPriority); cdecl; external;
function SDL_LogGetPriority(category: longint): TSDL_LogPriority; cdecl; external;
procedure SDL_LogResetPriorities; cdecl; external;
procedure SDL_Log(fmt: PChar; args: array of const); cdecl; external;
procedure SDL_Log(fmt: PChar); cdecl; external;
procedure SDL_LogVerbose(category: longint; fmt: PChar; args: array of const); cdecl; external;
procedure SDL_LogVerbose(category: longint; fmt: PChar); cdecl; external;
procedure SDL_LogDebug(category: longint; fmt: PChar; args: array of const); cdecl; external;
procedure SDL_LogDebug(category: longint; fmt: PChar); cdecl; external;
procedure SDL_LogInfo(category: longint; fmt: PChar; args: array of const); cdecl; external;
procedure SDL_LogInfo(category: longint; fmt: PChar); cdecl; external;
procedure SDL_LogWarn(category: longint; fmt: PChar; args: array of const); cdecl; external;
procedure SDL_LogWarn(category: longint; fmt: PChar); cdecl; external;
procedure SDL_LogError(category: longint; fmt: PChar; args: array of const); cdecl; external;
procedure SDL_LogError(category: longint; fmt: PChar); cdecl; external;
procedure SDL_LogCritical(category: longint; fmt: PChar; args: array of const); cdecl; external;
procedure SDL_LogCritical(category: longint; fmt: PChar); cdecl; external;
procedure SDL_LogMessage(category: longint; priority: TSDL_LogPriority; fmt: PChar; args: array of const); cdecl; external;
procedure SDL_LogMessage(category: longint; priority: TSDL_LogPriority; fmt: PChar); cdecl; external;
procedure SDL_LogMessageV(category: longint; priority: TSDL_LogPriority; fmt: PChar); varargs; cdecl; external;

type
  TSDL_LogOutputFunction = procedure(userdata: pointer; category: longint; priority: TSDL_LogPriority; message: PChar); cdecl;
  PSDL_LogOutputFunction = ^TSDL_LogOutputFunction;

procedure SDL_LogGetOutputFunction(callback: PSDL_LogOutputFunction; userdata: Ppointer); cdecl; external;
procedure SDL_LogSetOutputFunction(callback: TSDL_LogOutputFunction; userdata: pointer); cdecl; external;

implementation

end.
