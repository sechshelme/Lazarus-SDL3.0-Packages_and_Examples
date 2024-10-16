
unit SDL_log;
interface

{
  Automatically converted by H2Pas 0.99.16 from SDL_log.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_log.h
}

Type
PSDL_LogOutputFunction = ^TSDL_LogOutputFunction;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{
  Simple DirectMedia Layer
  Copyright (C) 1997-2024 Sam Lantinga <slouken@libsdl.org>

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
 }
{*
 * # CategoryLog
 *
 * Simple log messages with priorities and categories. A message's
 * SDL_LogPriority signifies how important the message is. A message's
 * SDL_LogCategory signifies from what domain it belongs to. Every category
 * has a minimum priority specified: when a message belongs to that category,
 * it will only be sent out if it has that minimum priority or higher.
 *
 * SDL's own logs are sent below the default priority threshold, so they are
 * quiet by default.
 *
 * You can change the log verbosity programmatically using
 * SDL_SetLogPriority() or with SDL_SetHint(SDL_HINT_LOGGING, ...), or with
 * the "SDL_LOGGING" environment variable. This variable is a comma separated
 * set of category=level tokens that define the default logging levels for SDL
 * applications.
 *
 * The category can be a numeric category, one of "app", "error", "assert",
 * "system", "audio", "video", "render", "input", "test", or `*` for any
 * unspecified category.
 *
 * The level can be a numeric level, one of "verbose", "debug", "info",
 * "warn", "error", "critical", or "quiet" to disable that category.
 *
 * You can omit the category if you want to set the logging level for all
 * categories.
 *
 * If this hint isn't set, the default log levels are equivalent to:
 *
 * `app=info,assert=warn,test=verbose,*=error`
 *
 * Here's where the messages go on different platforms:
 *
 * - Windows: debug output stream
 * - Android: log output
 * - Others: standard error output (stderr)
  }
{$ifndef SDL_log_h_}
{$define SDL_log_h_}
{$include <SDL3/SDL_stdinc.h>}
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{*
 * The predefined log categories
 *
 * By default the application and gpu categories are enabled at the INFO
 * level, the assert category is enabled at the WARN level, test is enabled at
 * the VERBOSE level and all other categories are enabled at the ERROR level.
 *
 * \since This enum is available since SDL 3.0.0.
  }
{ Reserved for future SDL library use  }
{ Beyond this point is reserved for application use, e.g.
       enum 
           MYAPP_CATEGORY_AWESOME1 = SDL_LOG_CATEGORY_CUSTOM,
           MYAPP_CATEGORY_AWESOME2,
           MYAPP_CATEGORY_AWESOME3,
           ...
       ;
      }
type
  PSDL_LogCategory = ^TSDL_LogCategory;
  TSDL_LogCategory =  Longint;
  Const
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
;
{*
 * The predefined log priorities
 *
 * \since This enum is available since SDL 3.0.0.
  }
type
  PSDL_LogPriority = ^TSDL_LogPriority;
  TSDL_LogPriority =  Longint;
  Const
    SDL_LOG_PRIORITY_INVALID = 0;
    SDL_LOG_PRIORITY_TRACE = 1;
    SDL_LOG_PRIORITY_VERBOSE = 2;
    SDL_LOG_PRIORITY_DEBUG = 3;
    SDL_LOG_PRIORITY_INFO = 4;
    SDL_LOG_PRIORITY_WARN = 5;
    SDL_LOG_PRIORITY_ERROR = 6;
    SDL_LOG_PRIORITY_CRITICAL = 7;
    SDL_LOG_PRIORITY_COUNT = 8;
;
{*
 * Set the priority of all log categories.
 *
 * \param priority the SDL_LogPriority to assign.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_ResetLogPriorities
 * \sa SDL_SetLogPriority
  }

procedure SDL_SetLogPriorities(priority:TSDL_LogPriority);cdecl;external;
{*
 * Set the priority of a particular log category.
 *
 * \param category the category to assign a priority to.
 * \param priority the SDL_LogPriority to assign.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetLogPriority
 * \sa SDL_ResetLogPriorities
 * \sa SDL_SetLogPriorities
  }
procedure SDL_SetLogPriority(category:longint; priority:TSDL_LogPriority);cdecl;external;
{*
 * Get the priority of a particular log category.
 *
 * \param category the category to query.
 * \returns the SDL_LogPriority for the requested category.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetLogPriority
  }
function SDL_GetLogPriority(category:longint):TSDL_LogPriority;cdecl;external;
{*
 * Reset all priorities to default.
 *
 * This is called by SDL_Quit().
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetLogPriorities
 * \sa SDL_SetLogPriority
  }
procedure SDL_ResetLogPriorities;cdecl;external;
{*
 * Set the text prepended to log messages of a given priority.
 *
 * By default SDL_LOG_PRIORITY_INFO and below have no prefix, and
 * SDL_LOG_PRIORITY_WARN and higher have a prefix showing their priority, e.g.
 * "WARNING: ".
 *
 * \param priority the SDL_LogPriority to modify.
 * \param prefix the prefix to use for that log priority, or NULL to use no
 *               prefix.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetLogPriorities
 * \sa SDL_SetLogPriority
  }
(* Const before declarator ignored *)
function SDL_SetLogPriorityPrefix(priority:TSDL_LogPriority; prefix:Pansichar):Tbool;cdecl;external;
{*
 * Log a message with SDL_LOG_CATEGORY_APPLICATION and SDL_LOG_PRIORITY_INFO.
 *
 * \param fmt a printf() style message format string.
 * \param ... additional parameters matching % tokens in the `fmt` string, if
 *            any.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_LogCritical
 * \sa SDL_LogDebug
 * \sa SDL_LogError
 * \sa SDL_LogInfo
 * \sa SDL_LogMessage
 * \sa SDL_LogMessageV
 * \sa SDL_LogTrace
 * \sa SDL_LogVerbose
 * \sa SDL_LogWarn
  }
(* Const before declarator ignored *)
procedure SDL_Log(fmt:Pansichar; args:array of const);cdecl;external;
procedure SDL_Log(fmt:Pansichar);cdecl;external;
{*
 * Log a message with SDL_LOG_PRIORITY_TRACE.
 *
 * \param category the category of the message.
 * \param fmt a printf() style message format string.
 * \param ... additional parameters matching % tokens in the **fmt** string,
 *            if any.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_Log
 * \sa SDL_LogCritical
 * \sa SDL_LogDebug
 * \sa SDL_LogError
 * \sa SDL_LogInfo
 * \sa SDL_LogMessage
 * \sa SDL_LogMessageV
 * \sa SDL_LogTrace
 * \sa SDL_LogVerbose
 * \sa SDL_LogWarn
  }
(* Const before declarator ignored *)
procedure SDL_LogTrace(category:longint; fmt:Pansichar; args:array of const);cdecl;external;
procedure SDL_LogTrace(category:longint; fmt:Pansichar);cdecl;external;
{*
 * Log a message with SDL_LOG_PRIORITY_VERBOSE.
 *
 * \param category the category of the message.
 * \param fmt a printf() style message format string.
 * \param ... additional parameters matching % tokens in the **fmt** string,
 *            if any.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_Log
 * \sa SDL_LogCritical
 * \sa SDL_LogDebug
 * \sa SDL_LogError
 * \sa SDL_LogInfo
 * \sa SDL_LogMessage
 * \sa SDL_LogMessageV
 * \sa SDL_LogWarn
  }
(* Const before declarator ignored *)
procedure SDL_LogVerbose(category:longint; fmt:Pansichar; args:array of const);cdecl;external;
procedure SDL_LogVerbose(category:longint; fmt:Pansichar);cdecl;external;
{*
 * Log a message with SDL_LOG_PRIORITY_DEBUG.
 *
 * \param category the category of the message.
 * \param fmt a printf() style message format string.
 * \param ... additional parameters matching % tokens in the **fmt** string,
 *            if any.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_Log
 * \sa SDL_LogCritical
 * \sa SDL_LogError
 * \sa SDL_LogInfo
 * \sa SDL_LogMessage
 * \sa SDL_LogMessageV
 * \sa SDL_LogTrace
 * \sa SDL_LogVerbose
 * \sa SDL_LogWarn
  }
(* Const before declarator ignored *)
procedure SDL_LogDebug(category:longint; fmt:Pansichar; args:array of const);cdecl;external;
procedure SDL_LogDebug(category:longint; fmt:Pansichar);cdecl;external;
{*
 * Log a message with SDL_LOG_PRIORITY_INFO.
 *
 * \param category the category of the message.
 * \param fmt a printf() style message format string.
 * \param ... additional parameters matching % tokens in the **fmt** string,
 *            if any.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_Log
 * \sa SDL_LogCritical
 * \sa SDL_LogDebug
 * \sa SDL_LogError
 * \sa SDL_LogMessage
 * \sa SDL_LogMessageV
 * \sa SDL_LogTrace
 * \sa SDL_LogVerbose
 * \sa SDL_LogWarn
  }
(* Const before declarator ignored *)
procedure SDL_LogInfo(category:longint; fmt:Pansichar; args:array of const);cdecl;external;
procedure SDL_LogInfo(category:longint; fmt:Pansichar);cdecl;external;
{*
 * Log a message with SDL_LOG_PRIORITY_WARN.
 *
 * \param category the category of the message.
 * \param fmt a printf() style message format string.
 * \param ... additional parameters matching % tokens in the **fmt** string,
 *            if any.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_Log
 * \sa SDL_LogCritical
 * \sa SDL_LogDebug
 * \sa SDL_LogError
 * \sa SDL_LogInfo
 * \sa SDL_LogMessage
 * \sa SDL_LogMessageV
 * \sa SDL_LogTrace
 * \sa SDL_LogVerbose
  }
(* Const before declarator ignored *)
procedure SDL_LogWarn(category:longint; fmt:Pansichar; args:array of const);cdecl;external;
procedure SDL_LogWarn(category:longint; fmt:Pansichar);cdecl;external;
{*
 * Log a message with SDL_LOG_PRIORITY_ERROR.
 *
 * \param category the category of the message.
 * \param fmt a printf() style message format string.
 * \param ... additional parameters matching % tokens in the **fmt** string,
 *            if any.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_Log
 * \sa SDL_LogCritical
 * \sa SDL_LogDebug
 * \sa SDL_LogInfo
 * \sa SDL_LogMessage
 * \sa SDL_LogMessageV
 * \sa SDL_LogTrace
 * \sa SDL_LogVerbose
 * \sa SDL_LogWarn
  }
(* Const before declarator ignored *)
procedure SDL_LogError(category:longint; fmt:Pansichar; args:array of const);cdecl;external;
procedure SDL_LogError(category:longint; fmt:Pansichar);cdecl;external;
{*
 * Log a message with SDL_LOG_PRIORITY_CRITICAL.
 *
 * \param category the category of the message.
 * \param fmt a printf() style message format string.
 * \param ... additional parameters matching % tokens in the **fmt** string,
 *            if any.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_Log
 * \sa SDL_LogDebug
 * \sa SDL_LogError
 * \sa SDL_LogInfo
 * \sa SDL_LogMessage
 * \sa SDL_LogMessageV
 * \sa SDL_LogTrace
 * \sa SDL_LogVerbose
 * \sa SDL_LogWarn
  }
(* Const before declarator ignored *)
procedure SDL_LogCritical(category:longint; fmt:Pansichar; args:array of const);cdecl;external;
procedure SDL_LogCritical(category:longint; fmt:Pansichar);cdecl;external;
{*
 * Log a message with the specified category and priority.
 *
 * \param category the category of the message.
 * \param priority the priority of the message.
 * \param fmt a printf() style message format string.
 * \param ... additional parameters matching % tokens in the **fmt** string,
 *            if any.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_Log
 * \sa SDL_LogCritical
 * \sa SDL_LogDebug
 * \sa SDL_LogError
 * \sa SDL_LogInfo
 * \sa SDL_LogMessageV
 * \sa SDL_LogTrace
 * \sa SDL_LogVerbose
 * \sa SDL_LogWarn
  }
(* Const before declarator ignored *)
procedure SDL_LogMessage(category:longint; priority:TSDL_LogPriority; fmt:Pansichar; args:array of const);cdecl;external;
procedure SDL_LogMessage(category:longint; priority:TSDL_LogPriority; fmt:Pansichar);cdecl;external;
{*
 * Log a message with the specified category and priority.
 *
 * \param category the category of the message.
 * \param priority the priority of the message.
 * \param fmt a printf() style message format string.
 * \param ap a variable argument list.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_Log
 * \sa SDL_LogCritical
 * \sa SDL_LogDebug
 * \sa SDL_LogError
 * \sa SDL_LogInfo
 * \sa SDL_LogMessage
 * \sa SDL_LogTrace
 * \sa SDL_LogVerbose
 * \sa SDL_LogWarn
  }
(* Const before declarator ignored *)
procedure SDL_LogMessageV(category:longint; priority:TSDL_LogPriority; fmt:Pansichar; ap:Tva_list);cdecl;external;
{*
 * The prototype for the log output callback function.
 *
 * This function is called by SDL when there is new text to be logged. A mutex
 * is held so that this function is never called by more than one thread at
 * once.
 *
 * \param userdata what was passed as `userdata` to
 *                 SDL_SetLogOutputFunction().
 * \param category the category of the message.
 * \param priority the priority of the message.
 * \param message the message being output.
 *
 * \since This datatype is available since SDL 3.0.0.
  }
(* Const before declarator ignored *)
type

  TSDL_LogOutputFunction = procedure (userdata:pointer; category:longint; priority:TSDL_LogPriority; message:Pansichar);cdecl;
{*
 * Get the current log output function.
 *
 * \param callback an SDL_LogOutputFunction filled in with the current log
 *                 callback.
 * \param userdata a pointer filled in with the pointer that is passed to
 *                 `callback`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetLogOutputFunction
  }

procedure SDL_GetLogOutputFunction(callback:PSDL_LogOutputFunction; userdata:Ppointer);cdecl;external;
{*
 * Replace the default log output function with one of your own.
 *
 * \param callback an SDL_LogOutputFunction to call instead of the default.
 * \param userdata a pointer that is passed to `callback`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetLogOutputFunction
  }
procedure SDL_SetLogOutputFunction(callback:TSDL_LogOutputFunction; userdata:pointer);cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_log_h_  }

implementation


end.
