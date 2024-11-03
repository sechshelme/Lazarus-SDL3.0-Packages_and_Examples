
unit SDL_test_common;
interface

{
  Automatically converted by H2Pas 0.99.16 from SDL_test_common.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_test_common.h
}

Type
PSDL_Event = ^TSDL_Event;
PSDL_Renderer = ^TSDL_Renderer;
PSDL_Texture = ^TSDL_Texture;
PSDL_Window = ^TSDL_Window;
Psingle = ^Tsingle;

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
 *  \file SDL_test_common.h
 *
 *  Common functions of SDL test framework.
 *
 *  This code is a part of the SDL test library, not the main SDL library.
  }
{ Ported from original test/common.h file.  }
{$ifndef SDL_test_common_h_}
{$define SDL_test_common_h_}
{$include <SDL3/SDL.h>}
{$ifdef SDL_PLATFORM_PSP}

const
  DEFAULT_WINDOW_WIDTH = 480;  
  DEFAULT_WINDOW_HEIGHT = 272;  
(*** was #elif ****){$else defined(SDL_PLATFORM_VITA)}
  DEFAULT_WINDOW_WIDTH = 960;  
  DEFAULT_WINDOW_HEIGHT = 544;  
{$else}
  DEFAULT_WINDOW_WIDTH = 640;  
  DEFAULT_WINDOW_HEIGHT = 480;  
{$endif}
type
  PSDLTest_VerboseFlags = ^TSDLTest_VerboseFlags;
  TSDLTest_VerboseFlags = TUint32;

const
  VERBOSE_VIDEO = $00000001;  
  VERBOSE_MODES = $00000002;  
  VERBOSE_RENDER = $00000004;  
  VERBOSE_EVENT = $00000008;  
  VERBOSE_AUDIO = $00000010;  
  VERBOSE_MOTION = $00000020;  
{ !< Function pointer parsing one argument at argv[index], returning the number of parsed arguments,
 *    or a negative value when the argument is invalid  }
type

  TSDLTest_ParseArgumentsFp = function (data:pointer; argv:PPansichar; index:longint):longint;cdecl;
{ !< Finalize the argument parser.  }

  TSDLTest_FinalizeArgumentParserFp = procedure (arg:pointer);cdecl;
{ !< Parse an argument.  }
{ !< Finalize this argument parser. Called once before parsing the first argument.  }
{ !< Null-terminated array of arguments. Printed when running with --help.  }
(* Const before declarator ignored *)
{ !< User data, passed to all callbacks.  }
{ !< Next argument parser.  }

  PSDLTest_ArgumentParser = ^TSDLTest_ArgumentParser;
  TSDLTest_ArgumentParser = record
      parse_arguments : TSDLTest_ParseArgumentsFp;
      finalize : TSDLTest_FinalizeArgumentParserFp;
      usage : ^Pansichar;
      data : pointer;
      next : PSDLTest_ArgumentParser;
    end;
{ SDL init flags  }
{ Video info  }
(* Const before declarator ignored *)
(* Const before declarator ignored *)
(* Const before declarator ignored *)
(* Const before declarator ignored *)
{ Renderer info  }
(* Const before declarator ignored *)
{ Audio info  }
(* Const before declarator ignored *)
{ GL settings  }
{ Mouse info  }
{ Options info  }

  PSDLTest_CommonState = ^TSDLTest_CommonState;
  TSDLTest_CommonState = record
      argv : ^Pansichar;
      flags : TSDL_InitFlags;
      verbose : TSDLTest_VerboseFlags;
      videodriver : Pansichar;
      display_index : longint;
      displayID : TSDL_DisplayID;
      window_title : Pansichar;
      window_icon : Pansichar;
      window_flags : TSDL_WindowFlags;
      flash_on_focus_loss : Tbool;
      window_x : longint;
      window_y : longint;
      window_w : longint;
      window_h : longint;
      window_minW : longint;
      window_minH : longint;
      window_maxW : longint;
      window_maxH : longint;
      window_min_aspect : single;
      window_max_aspect : single;
      logical_w : longint;
      logical_h : longint;
      auto_scale_content : Tbool;
      logical_presentation : TSDL_RendererLogicalPresentation;
      scale : single;
      depth : longint;
      refresh_rate : single;
      fill_usable_bounds : Tbool;
      fullscreen_exclusive : Tbool;
      fullscreen_mode : TSDL_DisplayMode;
      num_windows : longint;
      windows : ^PSDL_Window;
      gpudriver : Pansichar;
      renderdriver : Pansichar;
      render_vsync : longint;
      skip_renderer : Tbool;
      renderers : ^PSDL_Renderer;
      targets : ^PSDL_Texture;
      audiodriver : Pansichar;
      audio_format : TSDL_AudioFormat;
      audio_channels : longint;
      audio_freq : longint;
      audio_id : TSDL_AudioDeviceID;
      gl_red_size : longint;
      gl_green_size : longint;
      gl_blue_size : longint;
      gl_alpha_size : longint;
      gl_buffer_size : longint;
      gl_depth_size : longint;
      gl_stencil_size : longint;
      gl_double_buffer : longint;
      gl_accum_red_size : longint;
      gl_accum_green_size : longint;
      gl_accum_blue_size : longint;
      gl_accum_alpha_size : longint;
      gl_stereo : longint;
      gl_multisamplebuffers : longint;
      gl_multisamplesamples : longint;
      gl_retained_backing : longint;
      gl_accelerated : longint;
      gl_major_version : longint;
      gl_minor_version : longint;
      gl_debug : longint;
      gl_profile_mask : longint;
      confine : TSDL_Rect;
      hide_cursor : Tbool;
      common_argparser : TSDLTest_ArgumentParser;
      video_argparser : TSDLTest_ArgumentParser;
      audio_argparser : TSDLTest_ArgumentParser;
      argparser : PSDLTest_ArgumentParser;
    end;
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{ Function prototypes  }
{*
 * Parse command line parameters and create common state.
 *
 * \param argv Array of command line parameters
 * \param flags Flags indicating which subsystem to initialize (i.e. SDL_INIT_VIDEO | SDL_INIT_AUDIO)
 *
 * \returns a newly allocated common state object.
  }

function SDLTest_CommonCreateState(argv:PPansichar; flags:TSDL_InitFlags):PSDLTest_CommonState;cdecl;external;
{*
 * Free the common state object.
 *
 * You should call SDL_Quit() before calling this function.
 *
 * \param state The common state object to destroy
  }
procedure SDLTest_CommonDestroyState(state:PSDLTest_CommonState);cdecl;external;
{*
 * Process one common argument.
 *
 * \param state The common state describing the test window to create.
 * \param index The index of the argument to process in argv[].
 *
 * \returns the number of arguments processed (i.e. 1 for --fullscreen, 2 for --video [videodriver], or -1 on error.
  }
function SDLTest_CommonArg(state:PSDLTest_CommonState; index:longint):longint;cdecl;external;
{*
 * Logs command line usage info.
 *
 * This logs the appropriate command line options for the subsystems in use
 *  plus other common options, and then any application-specific options.
 *  This uses the SDL_Log() function and splits up output to be friendly to
 *  80-character-wide terminals.
 *
 * \param state The common state describing the test window for the app.
 * \param argv0 argv[0], as passed to main/SDL_main.
 * \param options an array of strings for application specific options. The last element of the array should be NULL.
  }
(* Const before declarator ignored *)
(* Const before declarator ignored *)
procedure SDLTest_CommonLogUsage(state:PSDLTest_CommonState; argv0:Pansichar; options:PPansichar);cdecl;external;
{*
 * Open test window.
 *
 * \param state The common state describing the test window to create.
 *
 * \returns true if initialization succeeded, false otherwise
  }
function SDLTest_CommonInit(state:PSDLTest_CommonState):Tbool;cdecl;external;
{*
 * Easy argument handling when test app doesn't need any custom args.
 *
 * \param state The common state describing the test window to create.
 * \param argc argc, as supplied to SDL_main
 * \param argv argv, as supplied to SDL_main
 *
 * \returns false if app should quit, true otherwise.
  }
function SDLTest_CommonDefaultArgs(state:PSDLTest_CommonState; argc:longint; argv:PPansichar):Tbool;cdecl;external;
{*
 * Print the details of an event.
 *
 * This is automatically called by SDLTest_CommonEvent() as needed.
 *
 * \param event The event to print.
  }
(* Const before declarator ignored *)
procedure SDLTest_PrintEvent(event:PSDL_Event);cdecl;external;
{*
 * Common event handler for test windows if you use a standard SDL_main.
 *
 * \param state The common state used to create test window.
 * \param event The event to handle.
 * \param done Flag indicating we are done.
  }
procedure SDLTest_CommonEvent(state:PSDLTest_CommonState; event:PSDL_Event; done:Plongint);cdecl;external;
{*
 * Common event handler for test windows if you use SDL_AppEvent.
 *
 * This does _not_ free anything in `event`.
 *
 * \param state The common state used to create test window.
 * \param event The event to handle.
 * \returns Value suitable for returning from SDL_AppEvent().
  }
(* Const before declarator ignored *)
function SDLTest_CommonEventMainCallbacks(state:PSDLTest_CommonState; event:PSDL_Event):TSDL_AppResult;cdecl;external;
{*
 * Close test window.
 *
 * \param state The common state used to create test window.
 *
  }
procedure SDLTest_CommonQuit(state:PSDLTest_CommonState);cdecl;external;
{*
 * Draws various window information (position, size, etc.) to the renderer.
 *
 * \param renderer The renderer to draw to.
 * \param window The window whose information should be displayed.
 * \param usedHeight Returns the height used, so the caller can draw more below.
 *
  }
procedure SDLTest_CommonDrawWindowInfo(renderer:PSDL_Renderer; window:PSDL_Window; usedHeight:Psingle);cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_test_common_h_  }

implementation


end.
