unit SDL_test_common;

interface

uses
  ctypes, SDL3;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

const
  DEFAULT_WINDOW_WIDTH = 640;
  DEFAULT_WINDOW_HEIGHT = 480;

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

type
  TSDLTest_ParseArgumentsFp = function(Data: pointer; argv: PPansichar; index: longint): longint; cdecl;
  TSDLTest_FinalizeArgumentParserFp = procedure(arg: pointer); cdecl;

  PSDLTest_ArgumentParser = ^TSDLTest_ArgumentParser;

  TSDLTest_ArgumentParser = record
    parse_arguments: TSDLTest_ParseArgumentsFp;
    finalize: TSDLTest_FinalizeArgumentParserFp;
    usage: ^pansichar;
    Data: pointer;
    Next: PSDLTest_ArgumentParser;
  end;

  PSDLTest_CommonState = ^TSDLTest_CommonState;

  TSDLTest_CommonState = record
    argv: ^pansichar;
    flags: TSDL_InitFlags;
    verbose: TSDLTest_VerboseFlags;
    videodriver: pansichar;
    display_index: longint;
    displayID: TSDL_DisplayID;
    window_title: pansichar;
    window_icon: pansichar;
    window_flags: TSDL_WindowFlags;
    flash_on_focus_loss: Tbool;
    window_x: longint;
    window_y: longint;
    window_w: longint;
    window_h: longint;
    window_minW: longint;
    window_minH: longint;
    window_maxW: longint;
    window_maxH: longint;
    window_min_aspect: single;
    window_max_aspect: single;
    logical_w: longint;
    logical_h: longint;
    auto_scale_content: Tbool;
    logical_presentation: TSDL_RendererLogicalPresentation;
    scale: single;
    depth: longint;
    refresh_rate: single;
    fill_usable_bounds: Tbool;
    fullscreen_exclusive: Tbool;
    fullscreen_mode: TSDL_DisplayMode;
    num_windows: longint;
    Windows: ^PSDL_Window;
    gpudriver: pansichar;
    renderdriver: pansichar;
    render_vsync: longint;
    skip_renderer: Tbool;
    renderers: ^PSDL_Renderer;
    targets: ^PSDL_Texture;
    audiodriver: pansichar;
    audio_format: TSDL_AudioFormat;
    audio_channels: longint;
    audio_freq: longint;
    audio_id: TSDL_AudioDeviceID;
    gl_red_size: longint;
    gl_green_size: longint;
    gl_blue_size: longint;
    gl_alpha_size: longint;
    gl_buffer_size: longint;
    gl_depth_size: longint;
    gl_stencil_size: longint;
    gl_double_buffer: longint;
    gl_accum_red_size: longint;
    gl_accum_green_size: longint;
    gl_accum_blue_size: longint;
    gl_accum_alpha_size: longint;
    gl_stereo: longint;
    gl_release_behavior : longint;
    gl_multisamplebuffers: longint;
    gl_multisamplesamples: longint;
    gl_retained_backing: longint;
    gl_accelerated: longint;
    gl_major_version: longint;
    gl_minor_version: longint;
    gl_debug: longint;
    gl_profile_mask: longint;
    confine: TSDL_Rect;
    hide_cursor: Tbool;
    common_argparser: TSDLTest_ArgumentParser;
    video_argparser: TSDLTest_ArgumentParser;
    audio_argparser: TSDLTest_ArgumentParser;
    argparser: PSDLTest_ArgumentParser;
  end;

function SDLTest_CommonCreateState(argv: PPansichar; flags: TSDL_InitFlags): PSDLTest_CommonState; cdecl; external;
procedure SDLTest_CommonDestroyState(state: PSDLTest_CommonState); cdecl; external;
function SDLTest_CommonArg(state: PSDLTest_CommonState; index: longint): longint; cdecl; external;
procedure SDLTest_CommonLogUsage(state: PSDLTest_CommonState; argv0: pansichar; options: PPansichar); cdecl; external;
function SDLTest_CommonInit(state: PSDLTest_CommonState): Tbool; cdecl; external;
function SDLTest_CommonDefaultArgs(state: PSDLTest_CommonState; argc: longint; argv: PPansichar): Tbool; cdecl; external;
procedure SDLTest_PrintEvent(event: PSDL_Event); cdecl; external;
procedure SDLTest_CommonEvent(state: PSDLTest_CommonState; event: PSDL_Event; done: Plongint); cdecl; external;
function SDLTest_CommonEventMainCallbacks(state: PSDLTest_CommonState; event: PSDL_Event): TSDL_AppResult; cdecl; external;
procedure SDLTest_CommonQuit(state: PSDLTest_CommonState); cdecl; external;
procedure SDLTest_CommonDrawWindowInfo(renderer: PSDL_Renderer; window: PSDL_Window; usedHeight: Psingle); cdecl; external;

implementation


end.
