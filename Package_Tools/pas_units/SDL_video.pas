unit SDL_video;

interface

uses
  SDL_stdinc, SDL_pixels, SDL_properties, SDL_rect, SDL_surface;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  PSDL_DisplayID = ^TSDL_DisplayID;
  TSDL_DisplayID = TUint32;

  PSDL_WindowID = ^TSDL_WindowID;
  TSDL_WindowID = TUint32;

const
  SDL_PROP_GLOBAL_VIDEO_WAYLAND_WL_DISPLAY_POINTER = 'SDL.video.wayland.wl_display';

type
  PSDL_SystemTheme = ^TSDL_SystemTheme;
  TSDL_SystemTheme = longint;

const
  SDL_SYSTEM_THEME_UNKNOWN = 0;
  SDL_SYSTEM_THEME_LIGHT = 1;
  SDL_SYSTEM_THEME_DARK = 2;

type
  TSDL_DisplayModeData = record
  end;
  PSDL_DisplayModeData = ^TSDL_DisplayModeData;

  TSDL_DisplayMode = record
    displayID: TSDL_DisplayID;
    format: TSDL_PixelFormat;
    w: longint;
    h: longint;
    pixel_density: single;
    refresh_rate: single;
    refresh_rate_numerator: longint;
    refresh_rate_denominator: longint;
    internal: PSDL_DisplayModeData;
  end;
  PSDL_DisplayMode = ^TSDL_DisplayMode;
  PPSDL_DisplayMode = ^PSDL_DisplayMode;

  PSDL_DisplayOrientation = ^TSDL_DisplayOrientation;
  TSDL_DisplayOrientation = longint;

const
  SDL_ORIENTATION_UNKNOWN = 0;
  SDL_ORIENTATION_LANDSCAPE = 1;
  SDL_ORIENTATION_LANDSCAPE_FLIPPED = 2;
  SDL_ORIENTATION_PORTRAIT = 3;
  SDL_ORIENTATION_PORTRAIT_FLIPPED = 4;

type
  PSDL_WindowFlags = ^TSDL_WindowFlags;
  TSDL_WindowFlags = TUint64;

const
  SDL_WINDOW_FULLSCREEN = $0000000000000001;
  SDL_WINDOW_OPENGL = $0000000000000002;
  SDL_WINDOW_OCCLUDED = $0000000000000004;
  SDL_WINDOW_HIDDEN = $0000000000000008;
  SDL_WINDOW_BORDERLESS = $0000000000000010;
  SDL_WINDOW_RESIZABLE = $0000000000000020;
  SDL_WINDOW_MINIMIZED = $0000000000000040;
  SDL_WINDOW_MAXIMIZED = $0000000000000080;
  SDL_WINDOW_MOUSE_GRABBED = $0000000000000100;
  SDL_WINDOW_INPUT_FOCUS = $0000000000000200;
  SDL_WINDOW_MOUSE_FOCUS = $0000000000000400;
  SDL_WINDOW_EXTERNAL = $0000000000000800;
  SDL_WINDOW_MODAL = $0000000000001000;
  SDL_WINDOW_HIGH_PIXEL_DENSITY = $0000000000002000;
  SDL_WINDOW_MOUSE_CAPTURE = $0000000000004000;
  SDL_WINDOW_MOUSE_RELATIVE_MODE = $0000000000008000;
  SDL_WINDOW_ALWAYS_ON_TOP = $0000000000010000;
  SDL_WINDOW_UTILITY = $0000000000020000;
  SDL_WINDOW_TOOLTIP = $0000000000040000;
  SDL_WINDOW_POPUP_MENU = $0000000000080000;
  SDL_WINDOW_KEYBOARD_GRABBED = $0000000000100000;
  SDL_WINDOW_VULKAN = $0000000010000000;
  SDL_WINDOW_METAL = $0000000020000000;
  SDL_WINDOW_TRANSPARENT = $0000000040000000;
  SDL_WINDOW_NOT_FOCUSABLE = $0000000080000000;

const
  SDL_WINDOWPOS_UNDEFINED_MASK = $1FFF0000;
  SDL_WINDOWPOS_CENTERED_MASK = $2FFF0000;

type
  PSDL_FlashOperation = ^TSDL_FlashOperation;
  TSDL_FlashOperation = longint;

const
  SDL_FLASH_CANCEL = 0;
  SDL_FLASH_BRIEFLY = 1;
  SDL_FLASH_UNTIL_FOCUSED = 2;

type
  PSDL_GLContext = ^TSDL_GLContext;
  TSDL_GLContext = PSDL_GLContextState;

  PSDL_EGLDisplay = ^TSDL_EGLDisplay;
  TSDL_EGLDisplay = pointer;

  PSDL_EGLConfig = ^TSDL_EGLConfig;
  TSDL_EGLConfig = pointer;

  PSDL_EGLSurface = ^TSDL_EGLSurface;
  TSDL_EGLSurface = pointer;

  PSDL_EGLAttrib = ^TSDL_EGLAttrib;
  TSDL_EGLAttrib = Tintptr_t;

  PSDL_EGLint = ^TSDL_EGLint;
  TSDL_EGLint = longint;

  PSDL_EGLAttribArrayCallback = ^TSDL_EGLAttribArrayCallback;
  TSDL_EGLAttribArrayCallback = function(userdata: pointer): PSDL_EGLAttrib; cdecl;

  PSDL_EGLIntArrayCallback = ^TSDL_EGLIntArrayCallback;
  TSDL_EGLIntArrayCallback = function(userdata: pointer; display: TSDL_EGLDisplay; config: TSDL_EGLConfig): PSDL_EGLint; cdecl;

  PSDL_GLattr = ^TSDL_GLattr;
  TSDL_GLattr = longint;

const
  SDL_GL_RED_SIZE = 0;
  SDL_GL_GREEN_SIZE = 1;
  SDL_GL_BLUE_SIZE = 2;
  SDL_GL_ALPHA_SIZE = 3;
  SDL_GL_BUFFER_SIZE = 4;
  SDL_GL_DOUBLEBUFFER = 5;
  SDL_GL_DEPTH_SIZE = 6;
  SDL_GL_STENCIL_SIZE = 7;
  SDL_GL_ACCUM_RED_SIZE = 8;
  SDL_GL_ACCUM_GREEN_SIZE = 9;
  SDL_GL_ACCUM_BLUE_SIZE = 10;
  SDL_GL_ACCUM_ALPHA_SIZE = 11;
  SDL_GL_STEREO = 12;
  SDL_GL_MULTISAMPLEBUFFERS = 13;
  SDL_GL_MULTISAMPLESAMPLES = 14;
  SDL_GL_ACCELERATED_VISUAL = 15;
  SDL_GL_RETAINED_BACKING = 16;
  SDL_GL_CONTEXT_MAJOR_VERSION = 17;
  SDL_GL_CONTEXT_MINOR_VERSION = 18;
  SDL_GL_CONTEXT_FLAGS = 19;
  SDL_GL_CONTEXT_PROFILE_MASK = 20;
  SDL_GL_SHARE_WITH_CURRENT_CONTEXT = 21;
  SDL_GL_FRAMEBUFFER_SRGB_CAPABLE = 22;
  SDL_GL_CONTEXT_RELEASE_BEHAVIOR = 23;
  SDL_GL_CONTEXT_RESET_NOTIFICATION = 24;
  SDL_GL_CONTEXT_NO_ERROR = 25;
  SDL_GL_FLOATBUFFERS = 26;
  SDL_GL_EGL_PLATFORM = 27;

type
  PSDL_GLprofile = ^TSDL_GLprofile;
  TSDL_GLprofile = longint;

const
  SDL_GL_CONTEXT_PROFILE_CORE = $0001;
  SDL_GL_CONTEXT_PROFILE_COMPATIBILITY = $0002;
  SDL_GL_CONTEXT_PROFILE_ES = $0004;

type
  PSDL_GLcontextFlag = ^TSDL_GLcontextFlag;
  TSDL_GLcontextFlag = longint;

const
  SDL_GL_CONTEXT_DEBUG_FLAG = $0001;
  SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG = $0002;
  SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG = $0004;
  SDL_GL_CONTEXT_RESET_ISOLATION_FLAG = $0008;

type
  PSDL_GLcontextReleaseFlag = ^TSDL_GLcontextReleaseFlag;
  TSDL_GLcontextReleaseFlag = longint;

const
  SDL_GL_CONTEXT_RELEASE_BEHAVIOR_NONE = $0000;
  SDL_GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH = $0001;

type
  PSDL_GLContextResetNotification = ^TSDL_GLContextResetNotification;
  TSDL_GLContextResetNotification = longint;

const
  SDL_GL_CONTEXT_RESET_NO_NOTIFICATION = $0000;
  SDL_GL_CONTEXT_RESET_LOSE_CONTEXT = $0001;

type
  TSDL_Window = record
  end;
  PSDL_Window = ^TSDL_Window;
  PPSDL_Window = ^PSDL_Window;

function SDL_GetNumVideoDrivers: longint; cdecl; external libSDL3;
function SDL_GetVideoDriver(index: longint): pansichar; cdecl; external libSDL3;
function SDL_GetCurrentVideoDriver: pansichar; cdecl; external libSDL3;
function SDL_GetSystemTheme: TSDL_SystemTheme; cdecl; external libSDL3;
function SDL_GetDisplays(Count: Plongint): PSDL_DisplayID; cdecl; external libSDL3;
function SDL_GetPrimaryDisplay: TSDL_DisplayID; cdecl; external libSDL3;
function SDL_GetDisplayProperties(displayID: TSDL_DisplayID): TSDL_PropertiesID; cdecl; external libSDL3;

const
  SDL_PROP_DISPLAY_HDR_ENABLED_BOOLEAN = 'SDL.display.HDR_enabled';
  SDL_PROP_DISPLAY_KMSDRM_PANEL_ORIENTATION_NUMBER = 'SDL.display.KMSDRM.panel_orientation';

function SDL_GetDisplayName(displayID: TSDL_DisplayID): pansichar; cdecl; external libSDL3;
function SDL_GetDisplayBounds(displayID: TSDL_DisplayID; rect: PSDL_Rect): Tbool; cdecl; external libSDL3;
function SDL_GetDisplayUsableBounds(displayID: TSDL_DisplayID; rect: PSDL_Rect): Tbool; cdecl; external libSDL3;
function SDL_GetNaturalDisplayOrientation(displayID: TSDL_DisplayID): TSDL_DisplayOrientation; cdecl; external libSDL3;
function SDL_GetCurrentDisplayOrientation(displayID: TSDL_DisplayID): TSDL_DisplayOrientation; cdecl; external libSDL3;
function SDL_GetDisplayContentScale(displayID: TSDL_DisplayID): single; cdecl; external libSDL3;
function SDL_GetFullscreenDisplayModes(displayID: TSDL_DisplayID; Count: Plongint): PPSDL_DisplayMode; cdecl; external libSDL3;
function SDL_GetClosestFullscreenDisplayMode(displayID: TSDL_DisplayID; w: longint; h: longint; refresh_rate: single; include_high_density_modes: Tbool;
  mode: PSDL_DisplayMode): Tbool; cdecl; external libSDL3;
function SDL_GetDesktopDisplayMode(displayID: TSDL_DisplayID): PSDL_DisplayMode; cdecl; external libSDL3;
function SDL_GetCurrentDisplayMode(displayID: TSDL_DisplayID): PSDL_DisplayMode; cdecl; external libSDL3;
function SDL_GetDisplayForPoint(point: PSDL_Point): TSDL_DisplayID; cdecl; external libSDL3;
function SDL_GetDisplayForRect(rect: PSDL_Rect): TSDL_DisplayID; cdecl; external libSDL3;
function SDL_GetDisplayForWindow(window: PSDL_Window): TSDL_DisplayID; cdecl; external libSDL3;
function SDL_GetWindowPixelDensity(window: PSDL_Window): single; cdecl; external libSDL3;
function SDL_GetWindowDisplayScale(window: PSDL_Window): single; cdecl; external libSDL3;
function SDL_SetWindowFullscreenMode(window: PSDL_Window; mode: PSDL_DisplayMode): Tbool; cdecl; external libSDL3;
function SDL_GetWindowFullscreenMode(window: PSDL_Window): PSDL_DisplayMode; cdecl; external libSDL3;
function SDL_GetWindowICCProfile(window: PSDL_Window; size: Psize_t): pointer; cdecl; external libSDL3;
function SDL_GetWindowPixelFormat(window: PSDL_Window): TSDL_PixelFormat; cdecl; external libSDL3;
function SDL_GetWindows(Count: Plongint): PPSDL_Window; cdecl; external libSDL3;
function SDL_CreateWindow(title: pansichar; w: longint; h: longint; flags: TSDL_WindowFlags): PSDL_Window; cdecl; external libSDL3;
function SDL_CreatePopupWindow(parent: PSDL_Window; offset_x: longint; offset_y: longint; w: longint; h: longint;
  flags: TSDL_WindowFlags): PSDL_Window; cdecl; external libSDL3;
function SDL_CreateWindowWithProperties(props: TSDL_PropertiesID): PSDL_Window; cdecl; external libSDL3;

const
  SDL_PROP_WINDOW_CREATE_ALWAYS_ON_TOP_BOOLEAN = 'SDL.window.create.always_on_top';
  SDL_PROP_WINDOW_CREATE_BORDERLESS_BOOLEAN = 'SDL.window.create.borderless';
  SDL_PROP_WINDOW_CREATE_FOCUSABLE_BOOLEAN = 'SDL.window.create.focusable';
  SDL_PROP_WINDOW_CREATE_EXTERNAL_GRAPHICS_CONTEXT_BOOLEAN = 'SDL.window.create.external_graphics_context';
  SDL_PROP_WINDOW_CREATE_FLAGS_NUMBER = 'SDL.window.create.flags';
  SDL_PROP_WINDOW_CREATE_FULLSCREEN_BOOLEAN = 'SDL.window.create.fullscreen';
  SDL_PROP_WINDOW_CREATE_HEIGHT_NUMBER = 'SDL.window.create.height';
  SDL_PROP_WINDOW_CREATE_HIDDEN_BOOLEAN = 'SDL.window.create.hidden';
  SDL_PROP_WINDOW_CREATE_HIGH_PIXEL_DENSITY_BOOLEAN = 'SDL.window.create.high_pixel_density';
  SDL_PROP_WINDOW_CREATE_MAXIMIZED_BOOLEAN = 'SDL.window.create.maximized';
  SDL_PROP_WINDOW_CREATE_MENU_BOOLEAN = 'SDL.window.create.menu';
  SDL_PROP_WINDOW_CREATE_METAL_BOOLEAN = 'SDL.window.create.metal';
  SDL_PROP_WINDOW_CREATE_MINIMIZED_BOOLEAN = 'SDL.window.create.minimized';
  SDL_PROP_WINDOW_CREATE_MODAL_BOOLEAN = 'SDL.window.create.modal';
  SDL_PROP_WINDOW_CREATE_MOUSE_GRABBED_BOOLEAN = 'SDL.window.create.mouse_grabbed';
  SDL_PROP_WINDOW_CREATE_OPENGL_BOOLEAN = 'SDL.window.create.opengl';
  SDL_PROP_WINDOW_CREATE_PARENT_POINTER = 'SDL.window.create.parent';
  SDL_PROP_WINDOW_CREATE_RESIZABLE_BOOLEAN = 'SDL.window.create.resizable';
  SDL_PROP_WINDOW_CREATE_TITLE_STRING = 'SDL.window.create.title';
  SDL_PROP_WINDOW_CREATE_TRANSPARENT_BOOLEAN = 'SDL.window.create.transparent';
  SDL_PROP_WINDOW_CREATE_TOOLTIP_BOOLEAN = 'SDL.window.create.tooltip';
  SDL_PROP_WINDOW_CREATE_UTILITY_BOOLEAN = 'SDL.window.create.utility';
  SDL_PROP_WINDOW_CREATE_VULKAN_BOOLEAN = 'SDL.window.create.vulkan';
  SDL_PROP_WINDOW_CREATE_WIDTH_NUMBER = 'SDL.window.create.width';
  SDL_PROP_WINDOW_CREATE_X_NUMBER = 'SDL.window.create.x';
  SDL_PROP_WINDOW_CREATE_Y_NUMBER = 'SDL.window.create.y';
  SDL_PROP_WINDOW_CREATE_COCOA_WINDOW_POINTER = 'SDL.window.create.cocoa.window';
  SDL_PROP_WINDOW_CREATE_COCOA_VIEW_POINTER = 'SDL.window.create.cocoa.view';
  SDL_PROP_WINDOW_CREATE_WAYLAND_SURFACE_ROLE_CUSTOM_BOOLEAN = 'SDL.window.create.wayland.surface_role_custom';
  SDL_PROP_WINDOW_CREATE_WAYLAND_CREATE_EGL_WINDOW_BOOLEAN = 'SDL.window.create.wayland.create_egl_window';
  SDL_PROP_WINDOW_CREATE_WAYLAND_WL_SURFACE_POINTER = 'SDL.window.create.wayland.wl_surface';
  SDL_PROP_WINDOW_CREATE_WIN32_HWND_POINTER = 'SDL.window.create.win32.hwnd';
  SDL_PROP_WINDOW_CREATE_WIN32_PIXEL_FORMAT_HWND_POINTER = 'SDL.window.create.win32.pixel_format_hwnd';
  SDL_PROP_WINDOW_CREATE_X11_WINDOW_NUMBER = 'SDL.window.create.x11.window';

function SDL_GetWindowID(window: PSDL_Window): TSDL_WindowID; cdecl; external libSDL3;
function SDL_GetWindowFromID(id: TSDL_WindowID): PSDL_Window; cdecl; external libSDL3;
function SDL_GetWindowParent(window: PSDL_Window): PSDL_Window; cdecl; external libSDL3;
function SDL_GetWindowProperties(window: PSDL_Window): TSDL_PropertiesID; cdecl; external libSDL3;

const
  SDL_PROP_WINDOW_SHAPE_POINTER = 'SDL.window.shape';
  SDL_PROP_WINDOW_HDR_ENABLED_BOOLEAN = 'SDL.window.HDR_enabled';
  SDL_PROP_WINDOW_SDR_WHITE_LEVEL_FLOAT = 'SDL.window.SDR_white_level';
  SDL_PROP_WINDOW_HDR_HEADROOM_FLOAT = 'SDL.window.HDR_headroom';
  SDL_PROP_WINDOW_ANDROID_WINDOW_POINTER = 'SDL.window.android.window';
  SDL_PROP_WINDOW_ANDROID_SURFACE_POINTER = 'SDL.window.android.surface';
  SDL_PROP_WINDOW_UIKIT_WINDOW_POINTER = 'SDL.window.uikit.window';
  SDL_PROP_WINDOW_UIKIT_METAL_VIEW_TAG_NUMBER = 'SDL.window.uikit.metal_view_tag';
  SDL_PROP_WINDOW_UIKIT_OPENGL_FRAMEBUFFER_NUMBER = 'SDL.window.uikit.opengl.framebuffer';
  SDL_PROP_WINDOW_UIKIT_OPENGL_RENDERBUFFER_NUMBER = 'SDL.window.uikit.opengl.renderbuffer';
  SDL_PROP_WINDOW_UIKIT_OPENGL_RESOLVE_FRAMEBUFFER_NUMBER = 'SDL.window.uikit.opengl.resolve_framebuffer';
  SDL_PROP_WINDOW_KMSDRM_DEVICE_INDEX_NUMBER = 'SDL.window.kmsdrm.dev_index';
  SDL_PROP_WINDOW_KMSDRM_DRM_FD_NUMBER = 'SDL.window.kmsdrm.drm_fd';
  SDL_PROP_WINDOW_KMSDRM_GBM_DEVICE_POINTER = 'SDL.window.kmsdrm.gbm_dev';
  SDL_PROP_WINDOW_COCOA_WINDOW_POINTER = 'SDL.window.cocoa.window';
  SDL_PROP_WINDOW_COCOA_METAL_VIEW_TAG_NUMBER = 'SDL.window.cocoa.metal_view_tag';
  SDL_PROP_WINDOW_VIVANTE_DISPLAY_POINTER = 'SDL.window.vivante.display';
  SDL_PROP_WINDOW_VIVANTE_WINDOW_POINTER = 'SDL.window.vivante.window';
  SDL_PROP_WINDOW_VIVANTE_SURFACE_POINTER = 'SDL.window.vivante.surface';
  SDL_PROP_WINDOW_WIN32_HWND_POINTER = 'SDL.window.win32.hwnd';
  SDL_PROP_WINDOW_WIN32_HDC_POINTER = 'SDL.window.win32.hdc';
  SDL_PROP_WINDOW_WIN32_INSTANCE_POINTER = 'SDL.window.win32.instance';
  SDL_PROP_WINDOW_WAYLAND_DISPLAY_POINTER = 'SDL.window.wayland.display';
  SDL_PROP_WINDOW_WAYLAND_SURFACE_POINTER = 'SDL.window.wayland.surface';
  SDL_PROP_WINDOW_WAYLAND_EGL_WINDOW_POINTER = 'SDL.window.wayland.egl_window';
  SDL_PROP_WINDOW_WAYLAND_XDG_SURFACE_POINTER = 'SDL.window.wayland.xdg_surface';
  SDL_PROP_WINDOW_WAYLAND_XDG_TOPLEVEL_POINTER = 'SDL.window.wayland.xdg_toplevel';
  SDL_PROP_WINDOW_WAYLAND_XDG_TOPLEVEL_EXPORT_HANDLE_STRING = 'SDL.window.wayland.xdg_toplevel_export_handle';
  SDL_PROP_WINDOW_WAYLAND_XDG_POPUP_POINTER = 'SDL.window.wayland.xdg_popup';
  SDL_PROP_WINDOW_WAYLAND_XDG_POSITIONER_POINTER = 'SDL.window.wayland.xdg_positioner';
  SDL_PROP_WINDOW_X11_DISPLAY_POINTER = 'SDL.window.x11.display';
  SDL_PROP_WINDOW_X11_SCREEN_NUMBER = 'SDL.window.x11.screen';
  SDL_PROP_WINDOW_X11_WINDOW_NUMBER = 'SDL.window.x11.window';

function SDL_GetWindowFlags(window: PSDL_Window): TSDL_WindowFlags; cdecl; external libSDL3;
function SDL_SetWindowTitle(window: PSDL_Window; title: pansichar): Tbool; cdecl; external libSDL3;
function SDL_GetWindowTitle(window: PSDL_Window): pansichar; cdecl; external libSDL3;
function SDL_SetWindowIcon(window: PSDL_Window; icon: PSDL_Surface): Tbool; cdecl; external libSDL3;
function SDL_SetWindowPosition(window: PSDL_Window; x: longint; y: longint): Tbool; cdecl; external libSDL3;
function SDL_GetWindowPosition(window: PSDL_Window; x: Plongint; y: Plongint): Tbool; cdecl; external libSDL3;
function SDL_SetWindowSize(window: PSDL_Window; w: longint; h: longint): Tbool; cdecl; external libSDL3;
function SDL_GetWindowSize(window: PSDL_Window; w: Plongint; h: Plongint): Tbool; cdecl; external libSDL3;
function SDL_GetWindowSafeArea(window: PSDL_Window; rect: PSDL_Rect): Tbool; cdecl; external libSDL3;
function SDL_SetWindowAspectRatio(window: PSDL_Window; min_aspect: single; max_aspect: single): Tbool; cdecl; external libSDL3;
function SDL_GetWindowAspectRatio(window: PSDL_Window; min_aspect: Psingle; max_aspect: Psingle): Tbool; cdecl; external libSDL3;
function SDL_GetWindowBordersSize(window: PSDL_Window; top: Plongint; left: Plongint; bottom: Plongint; right: Plongint): Tbool; cdecl; external libSDL3;
function SDL_GetWindowSizeInPixels(window: PSDL_Window; w: Plongint; h: Plongint): Tbool; cdecl; external libSDL3;
function SDL_SetWindowMinimumSize(window: PSDL_Window; min_w: longint; min_h: longint): Tbool; cdecl; external libSDL3;
function SDL_GetWindowMinimumSize(window: PSDL_Window; w: Plongint; h: Plongint): Tbool; cdecl; external libSDL3;
function SDL_SetWindowMaximumSize(window: PSDL_Window; max_w: longint; max_h: longint): Tbool; cdecl; external libSDL3;
function SDL_GetWindowMaximumSize(window: PSDL_Window; w: Plongint; h: Plongint): Tbool; cdecl; external libSDL3;
function SDL_SetWindowBordered(window: PSDL_Window; bordered: Tbool): Tbool; cdecl; external libSDL3;
function SDL_SetWindowResizable(window: PSDL_Window; resizable: Tbool): Tbool; cdecl; external libSDL3;
function SDL_SetWindowAlwaysOnTop(window: PSDL_Window; on_top: Tbool): Tbool; cdecl; external libSDL3;
function SDL_ShowWindow(window: PSDL_Window): Tbool; cdecl; external libSDL3;
function SDL_HideWindow(window: PSDL_Window): Tbool; cdecl; external libSDL3;
function SDL_RaiseWindow(window: PSDL_Window): Tbool; cdecl; external libSDL3;
function SDL_MaximizeWindow(window: PSDL_Window): Tbool; cdecl; external libSDL3;
function SDL_MinimizeWindow(window: PSDL_Window): Tbool; cdecl; external libSDL3;
function SDL_RestoreWindow(window: PSDL_Window): Tbool; cdecl; external libSDL3;
function SDL_SetWindowFullscreen(window: PSDL_Window; fullscreen: Tbool): Tbool; cdecl; external libSDL3;
function SDL_SyncWindow(window: PSDL_Window): Tbool; cdecl; external libSDL3;
function SDL_WindowHasSurface(window: PSDL_Window): Tbool; cdecl; external libSDL3;
function SDL_GetWindowSurface(window: PSDL_Window): PSDL_Surface; cdecl; external libSDL3;
function SDL_SetWindowSurfaceVSync(window: PSDL_Window; vsync: longint): Tbool; cdecl; external libSDL3;

const
  SDL_WINDOW_SURFACE_VSYNC_DISABLED = 0;
  SDL_WINDOW_SURFACE_VSYNC_ADAPTIVE = -(1);

function SDL_GetWindowSurfaceVSync(window: PSDL_Window; vsync: Plongint): Tbool; cdecl; external libSDL3;
function SDL_UpdateWindowSurface(window: PSDL_Window): Tbool; cdecl; external libSDL3;
function SDL_UpdateWindowSurfaceRects(window: PSDL_Window; rects: PSDL_Rect; numrects: longint): Tbool; cdecl; external libSDL3;
function SDL_DestroyWindowSurface(window: PSDL_Window): Tbool; cdecl; external libSDL3;
function SDL_SetWindowKeyboardGrab(window: PSDL_Window; grabbed: Tbool): Tbool; cdecl; external libSDL3;
function SDL_SetWindowMouseGrab(window: PSDL_Window; grabbed: Tbool): Tbool; cdecl; external libSDL3;
function SDL_GetWindowKeyboardGrab(window: PSDL_Window): Tbool; cdecl; external libSDL3;
function SDL_GetWindowMouseGrab(window: PSDL_Window): Tbool; cdecl; external libSDL3;
function SDL_GetGrabbedWindow: PSDL_Window; cdecl; external libSDL3;
function SDL_SetWindowMouseRect(window: PSDL_Window; rect: PSDL_Rect): Tbool; cdecl; external libSDL3;
function SDL_GetWindowMouseRect(window: PSDL_Window): PSDL_Rect; cdecl; external libSDL3;
function SDL_SetWindowOpacity(window: PSDL_Window; opacity: single): Tbool; cdecl; external libSDL3;
function SDL_GetWindowOpacity(window: PSDL_Window): single; cdecl; external libSDL3;
function SDL_SetWindowParent(window: PSDL_Window; parent: PSDL_Window): Tbool; cdecl; external libSDL3;
function SDL_SetWindowModal(window: PSDL_Window; modal: Tbool): Tbool; cdecl; external libSDL3;
function SDL_SetWindowFocusable(window: PSDL_Window; focusable: Tbool): Tbool; cdecl; external libSDL3;
function SDL_ShowWindowSystemMenu(window: PSDL_Window; x: longint; y: longint): Tbool; cdecl; external libSDL3;

type
  PSDL_HitTestResult = ^TSDL_HitTestResult;
  TSDL_HitTestResult = longint;

const
  SDL_HITTEST_NORMAL = 0;
  SDL_HITTEST_DRAGGABLE = 1;
  SDL_HITTEST_RESIZE_TOPLEFT = 2;
  SDL_HITTEST_RESIZE_TOP = 3;
  SDL_HITTEST_RESIZE_TOPRIGHT = 4;
  SDL_HITTEST_RESIZE_RIGHT = 5;
  SDL_HITTEST_RESIZE_BOTTOMRIGHT = 6;
  SDL_HITTEST_RESIZE_BOTTOM = 7;
  SDL_HITTEST_RESIZE_BOTTOMLEFT = 8;
  SDL_HITTEST_RESIZE_LEFT = 9;

type
  TSDL_HitTest = function(win: PSDL_Window; area: PSDL_Point; Data: pointer): TSDL_HitTestResult; cdecl;

function SDL_SetWindowHitTest(window: PSDL_Window; callback: TSDL_HitTest; callback_data: pointer): Tbool; cdecl; external libSDL3;
function SDL_SetWindowShape(window: PSDL_Window; shape: PSDL_Surface): Tbool; cdecl; external libSDL3;
function SDL_FlashWindow(window: PSDL_Window; operation: TSDL_FlashOperation): Tbool; cdecl; external libSDL3;
procedure SDL_DestroyWindow(window: PSDL_Window); cdecl; external libSDL3;
function SDL_ScreenSaverEnabled: Tbool; cdecl; external libSDL3;
function SDL_EnableScreenSaver: Tbool; cdecl; external libSDL3;
function SDL_DisableScreenSaver: Tbool; cdecl; external libSDL3;
function SDL_GL_LoadLibrary(path: pansichar): Tbool; cdecl; external libSDL3;
function SDL_GL_GetProcAddress(proc: pansichar): TSDL_FunctionPointer; cdecl; external libSDL3;
function SDL_EGL_GetProcAddress(proc: pansichar): TSDL_FunctionPointer; cdecl; external libSDL3;
procedure SDL_GL_UnloadLibrary; cdecl; external libSDL3;
function SDL_GL_ExtensionSupported(extension: pansichar): Tbool; cdecl; external libSDL3;
procedure SDL_GL_ResetAttributes; cdecl; external libSDL3;
function SDL_GL_SetAttribute(attr: TSDL_GLattr; Value: longint): Tbool; cdecl; external libSDL3;
function SDL_GL_GetAttribute(attr: TSDL_GLattr; Value: Plongint): Tbool; cdecl; external libSDL3;
function SDL_GL_CreateContext(window: PSDL_Window): TSDL_GLContext; cdecl; external libSDL3;
function SDL_GL_MakeCurrent(window: PSDL_Window; context: TSDL_GLContext): Tbool; cdecl; external libSDL3;
function SDL_GL_GetCurrentWindow: PSDL_Window; cdecl; external libSDL3;
function SDL_GL_GetCurrentContext: TSDL_GLContext; cdecl; external libSDL3;
function SDL_EGL_GetCurrentDisplay: TSDL_EGLDisplay; cdecl; external libSDL3;
function SDL_EGL_GetCurrentConfig: TSDL_EGLConfig; cdecl; external libSDL3;
function SDL_EGL_GetWindowSurface(window: PSDL_Window): TSDL_EGLSurface; cdecl; external libSDL3;
procedure SDL_EGL_SetAttributeCallbacks(platformAttribCallback: TSDL_EGLAttribArrayCallback; surfaceAttribCallback: TSDL_EGLIntArrayCallback; contextAttribCallback: TSDL_EGLIntArrayCallback; userdata: pointer); cdecl; external libSDL3;
function SDL_GL_SetSwapInterval(interval: longint): Tbool; cdecl; external libSDL3;
function SDL_GL_GetSwapInterval(interval: Plongint): Tbool; cdecl; external libSDL3;
function SDL_GL_SwapWindow(window: PSDL_Window): Tbool; cdecl; external libSDL3;

function SDL_WINDOWPOS_UNDEFINED_DISPLAY(X: longint): longint;
function SDL_WINDOWPOS_UNDEFINED: longint;
function SDL_WINDOWPOS_ISUNDEFINED(X: longint): boolean;
function SDL_WINDOWPOS_CENTERED_DISPLAY(X: longint): longint;
function SDL_WINDOWPOS_CENTERED: longint;
function SDL_WINDOWPOS_ISCENTERED(X: longint): boolean;

implementation

function SDL_WINDOWPOS_UNDEFINED_DISPLAY(X: longint): longint;
begin
  SDL_WINDOWPOS_UNDEFINED_DISPLAY := SDL_WINDOWPOS_UNDEFINED_MASK or X;
end;

function SDL_WINDOWPOS_UNDEFINED: longint;
begin
  SDL_WINDOWPOS_UNDEFINED := SDL_WINDOWPOS_UNDEFINED_DISPLAY(0);
end;

function SDL_WINDOWPOS_ISUNDEFINED(X: longint): boolean;
begin
  SDL_WINDOWPOS_ISUNDEFINED := (X and $FFFF0000) = SDL_WINDOWPOS_UNDEFINED_MASK;
end;

function SDL_WINDOWPOS_CENTERED_DISPLAY(X: longint): longint;
begin
  SDL_WINDOWPOS_CENTERED_DISPLAY := SDL_WINDOWPOS_CENTERED_MASK or X;
end;

function SDL_WINDOWPOS_CENTERED: longint;
begin
  SDL_WINDOWPOS_CENTERED := SDL_WINDOWPOS_CENTERED_DISPLAY(0);
end;

function SDL_WINDOWPOS_ISCENTERED(X: longint): boolean;
begin
  SDL_WINDOWPOS_ISCENTERED := (X and $FFFF0000) = SDL_WINDOWPOS_CENTERED_MASK;
end;


end.
