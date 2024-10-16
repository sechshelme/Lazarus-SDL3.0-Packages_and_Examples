unit SDL_process;

interface

uses
  ctypes, SDL_stdinc, SDL_properties, SDL_iostream;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  TSDL_Process = record
  end;
  PSDL_Process = ^TSDL_Process;

function SDL_CreateProcess(args: PPansichar; pipe_stdio: Tbool): PSDL_Process; cdecl; external libSDL3;

type
  PSDL_ProcessIO = ^TSDL_ProcessIO;
  TSDL_ProcessIO = longint;

const
  SDL_PROCESS_STDIO_INHERITED = 0;
  SDL_PROCESS_STDIO_NULL = 1;
  SDL_PROCESS_STDIO_APP = 2;
  SDL_PROCESS_STDIO_REDIRECT = 3;

function SDL_CreateProcessWithProperties(props: TSDL_PropertiesID): PSDL_Process; cdecl; external libSDL3;

const
  SDL_PROP_PROCESS_CREATE_ARGS_POINTER = 'SDL.process.create.args';
  SDL_PROP_PROCESS_CREATE_ENVIRONMENT_POINTER = 'SDL.process.create.environment';
  SDL_PROP_PROCESS_CREATE_STDIN_NUMBER = 'SDL.process.create.stdin_option';
  SDL_PROP_PROCESS_CREATE_STDIN_POINTER = 'SDL.process.create.stdin_source';
  SDL_PROP_PROCESS_CREATE_STDOUT_NUMBER = 'SDL.process.create.stdout_option';
  SDL_PROP_PROCESS_CREATE_STDOUT_POINTER = 'SDL.process.create.stdout_source';
  SDL_PROP_PROCESS_CREATE_STDERR_NUMBER = 'SDL.process.create.stderr_option';
  SDL_PROP_PROCESS_CREATE_STDERR_POINTER = 'SDL.process.create.stderr_source';
  SDL_PROP_PROCESS_CREATE_STDERR_TO_STDOUT_BOOLEAN = 'SDL.process.create.stderr_to_stdout';
  SDL_PROP_PROCESS_CREATE_BACKGROUND_BOOLEAN = 'SDL.process.create.background';

function SDL_GetProcessProperties(process: PSDL_Process): TSDL_PropertiesID; cdecl; external libSDL3;

const
  SDL_PROP_PROCESS_PID_NUMBER = 'SDL.process.pid';
  SDL_PROP_PROCESS_STDIN_POINTER = 'SDL.process.stdin';
  SDL_PROP_PROCESS_STDOUT_POINTER = 'SDL.process.stdout';
  SDL_PROP_PROCESS_STDERR_POINTER = 'SDL.process.stderr';
  SDL_PROP_PROCESS_BACKGROUND_BOOLEAN = 'SDL.process.background';

function SDL_ReadProcess(process: PSDL_Process; datasize: Psize_t; exitcode: Plongint): pointer; cdecl; external libSDL3;
function SDL_GetProcessInput(process: PSDL_Process): PSDL_IOStream; cdecl; external libSDL3;
function SDL_GetProcessOutput(process: PSDL_Process): PSDL_IOStream; cdecl; external libSDL3;
function SDL_KillProcess(process: PSDL_Process; force: Tbool): Tbool; cdecl; external libSDL3;
function SDL_WaitProcess(process: PSDL_Process; block: Tbool; exitcode: Plongint): Tbool; cdecl; external libSDL3;
procedure SDL_DestroyProcess(process: PSDL_Process); cdecl; external libSDL3;

implementation


end.
