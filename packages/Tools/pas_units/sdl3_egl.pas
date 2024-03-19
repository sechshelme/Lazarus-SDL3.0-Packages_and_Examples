unit SDL3_egl;

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

type
  TeglMustCastToProperFunctionPointerType = Pointer;

const
  KHRONOS_STATIC = 1;

type
  Pkhronos_int32_t = ^Tkhronos_int32_t;
  Tkhronos_int32_t = int32;

  Pkhronos_uint32_t = ^Tkhronos_uint32_t;
  Tkhronos_uint32_t = uint32;

  Pkhronos_int64_t = ^Tkhronos_int64_t;
  Tkhronos_int64_t = int64;

  Pkhronos_uint64_t = ^Tkhronos_uint64_t;
  Tkhronos_uint64_t = uint64;

const
  KHRONOS_SUPPORT_INT64 = 1;
  KHRONOS_SUPPORT_FLOAT = 1;

type
  Pkhronos_intptr_t = ^Tkhronos_intptr_t;
  Tkhronos_intptr_t = IntPtr;

  Pkhronos_uintptr_t = ^Tkhronos_uintptr_t;
  Tkhronos_uintptr_t = UIntPtr;

type
  Pkhronos_ssize_t = ^Tkhronos_ssize_t;
  Tkhronos_ssize_t = SizeInt;

  Pkhronos_usize_t = ^Tkhronos_usize_t;
  Tkhronos_usize_t = SizeUInt;

type
  Pkhronos_float_t = ^Tkhronos_float_t;
  Tkhronos_float_t = single;

type
  Pkhronos_utime_nanoseconds_t = ^Tkhronos_utime_nanoseconds_t;
  Tkhronos_utime_nanoseconds_t = Tkhronos_uint64_t;

  Pkhronos_stime_nanoseconds_t = ^Tkhronos_stime_nanoseconds_t;
  Tkhronos_stime_nanoseconds_t = Tkhronos_int64_t;

const
  KHRONOS_MAX_ENUM = $7FFFFFFF;

type
  Pkhronos_boolean_enum_t = ^Tkhronos_boolean_enum_t;
  Tkhronos_boolean_enum_t = longint;

const
  KHRONOS_FALSE = 0;
  KHRONOS_TRUE = 1;
  KHRONOS_BOOLEAN_ENUM_FORCE_SIZE = KHRONOS_MAX_ENUM;

type
  PEGLNativeDisplayType = ^TEGLNativeDisplayType;
  TEGLNativeDisplayType = pointer;

  PEGLNativePixmapType = ^TEGLNativePixmapType;
  TEGLNativePixmapType = pointer;

  PEGLNativeWindowType = ^TEGLNativeWindowType;
  TEGLNativeWindowType = pointer;

const
  WIN32_LEAN_AND_MEAN = 1;

type
  PANativeWindow = ^TANativeWindow;
  TANativeWindow = record
    {undefined structure}
  end;

  Pegl_native_pixmap_t = ^Tegl_native_pixmap_t;
  Tegl_native_pixmap_t = record
    {undefined structure}
  end;

  PEGLint = ^TEGLint;
  TEGLint = Tkhronos_int32_t;

const
  __egl_h_ = 1;

const
  EGL_EGL_PROTOTYPES = 1;

const
  EGL_VERSION_1_0 = 1;

type
  PEGLBoolean = ^TEGLBoolean;
  TEGLBoolean = dword;

  PEGLDisplay = ^TEGLDisplay;
  TEGLDisplay = pointer;

  PEGLConfig = ^TEGLConfig;
  TEGLConfig = pointer;

  PEGLSurface = ^TEGLSurface;
  TEGLSurface = pointer;

  PEGLContext = ^TEGLContext;
  TEGLContext = pointer;

  T_eglMustCastToProperFunctionPointerType = procedure(para1: pointer); cdecl;

const
  EGL_ALPHA_SIZE = $3021;
  EGL_BAD_ACCESS = $3002;
  EGL_BAD_ALLOC = $3003;
  EGL_BAD_ATTRIBUTE = $3004;
  EGL_BAD_CONFIG = $3005;
  EGL_BAD_CONTEXT = $3006;
  EGL_BAD_CURRENT_SURFACE = $3007;
  EGL_BAD_DISPLAY = $3008;
  EGL_BAD_MATCH = $3009;
  EGL_BAD_NATIVE_PIXMAP = $300A;
  EGL_BAD_NATIVE_WINDOW = $300B;
  EGL_BAD_PARAMETER = $300C;
  EGL_BAD_SURFACE = $300D;
  EGL_BLUE_SIZE = $3022;
  EGL_BUFFER_SIZE = $3020;
  EGL_CONFIG_CAVEAT = $3027;
  EGL_CONFIG_ID = $3028;
  EGL_CORE_NATIVE_ENGINE = $305B;
  EGL_DEPTH_SIZE = $3025;


const
  EGL_DRAW = $3059;
  EGL_EXTENSIONS = $3055;
  EGL_FALSE = 0;
  EGL_GREEN_SIZE = $3023;
  EGL_HEIGHT = $3056;
  EGL_LARGEST_PBUFFER = $3058;
  EGL_LEVEL = $3029;
  EGL_MAX_PBUFFER_HEIGHT = $302A;
  EGL_MAX_PBUFFER_PIXELS = $302B;
  EGL_MAX_PBUFFER_WIDTH = $302C;
  EGL_NATIVE_RENDERABLE = $302D;
  EGL_NATIVE_VISUAL_ID = $302E;
  EGL_NATIVE_VISUAL_TYPE = $302F;
  EGL_NONE = $3038;
  EGL_NON_CONFORMANT_CONFIG = $3051;
  EGL_NOT_INITIALIZED = $3001;

const
  EGL_PBUFFER_BIT = $0001;
  EGL_PIXMAP_BIT = $0002;
  EGL_READ = $305A;
  EGL_RED_SIZE = $3024;
  EGL_SAMPLES = $3031;
  EGL_SAMPLE_BUFFERS = $3032;
  EGL_SLOW_CONFIG = $3050;
  EGL_STENCIL_SIZE = $3026;
  EGL_SUCCESS = $3000;
  EGL_SURFACE_TYPE = $3033;
  EGL_TRANSPARENT_BLUE_VALUE = $3035;
  EGL_TRANSPARENT_GREEN_VALUE = $3036;
  EGL_TRANSPARENT_RED_VALUE = $3037;
  EGL_TRANSPARENT_RGB = $3052;
  EGL_TRANSPARENT_TYPE = $3034;
  EGL_TRUE = 1;
  EGL_VENDOR = $3053;
  EGL_VERSION = $3054;
  EGL_WIDTH = $3057;
  EGL_WINDOW_BIT = $0004;

function eglChooseConfig(dpy: TEGLDisplay; attrib_list: PEGLint; configs: PEGLConfig; config_size: TEGLint; num_config: PEGLint): TEGLBoolean; cdecl; external;
function eglCopyBuffers(dpy: TEGLDisplay; surface: TEGLSurface; target: TEGLNativePixmapType): TEGLBoolean; cdecl; external;
function eglCreateContext(dpy: TEGLDisplay; config: TEGLConfig; share_context: TEGLContext; attrib_list: PEGLint): TEGLContext; cdecl; external;
function eglCreatePbufferSurface(dpy: TEGLDisplay; config: TEGLConfig; attrib_list: PEGLint): TEGLSurface; cdecl; external;
function eglCreatePixmapSurface(dpy: TEGLDisplay; config: TEGLConfig; pixmap: TEGLNativePixmapType; attrib_list: PEGLint): TEGLSurface; cdecl; external;
function eglCreateWindowSurface(dpy: TEGLDisplay; config: TEGLConfig; win: TEGLNativeWindowType; attrib_list: PEGLint): TEGLSurface; cdecl; external;
function eglDestroyContext(dpy: TEGLDisplay; ctx: TEGLContext): TEGLBoolean; cdecl; external;
function eglDestroySurface(dpy: TEGLDisplay; surface: TEGLSurface): TEGLBoolean; cdecl; external;
function eglGetConfigAttrib(dpy: TEGLDisplay; config: TEGLConfig; attribute: TEGLint; Value: PEGLint): TEGLBoolean; cdecl; external;
function eglGetConfigs(dpy: TEGLDisplay; configs: PEGLConfig; config_size: TEGLint; num_config: PEGLint): TEGLBoolean; cdecl; external;
function eglGetCurrentDisplay: TEGLDisplay; cdecl; external;
function eglGetCurrentSurface(readdraw: TEGLint): TEGLSurface; cdecl; external;
function eglGetDisplay(display_id: TEGLNativeDisplayType): TEGLDisplay; cdecl; external;
function eglGetError: TEGLint; cdecl; external;
function eglGetProcAddress(procname: PChar): TeglMustCastToProperFunctionPointerType; cdecl; external;
function eglInitialize(dpy: TEGLDisplay; major: PEGLint; minor: PEGLint): TEGLBoolean; cdecl; external;
function eglMakeCurrent(dpy: TEGLDisplay; draw: TEGLSurface; Read: TEGLSurface; ctx: TEGLContext): TEGLBoolean; cdecl; external;
function eglQueryContext(dpy: TEGLDisplay; ctx: TEGLContext; attribute: TEGLint; Value: PEGLint): TEGLBoolean; cdecl; external;
function eglQueryString(dpy: TEGLDisplay; Name: TEGLint): PChar; cdecl; external;
function eglQuerySurface(dpy: TEGLDisplay; surface: TEGLSurface; attribute: TEGLint; Value: PEGLint): TEGLBoolean; cdecl; external;
function eglSwapBuffers(dpy: TEGLDisplay; surface: TEGLSurface): TEGLBoolean; cdecl; external;
function eglTerminate(dpy: TEGLDisplay): TEGLBoolean; cdecl; external;
function eglWaitGL: TEGLBoolean; cdecl; external;
function eglWaitNative(engine: TEGLint): TEGLBoolean; cdecl; external;

const
  EGL_VERSION_1_1 = 1;
  EGL_BACK_BUFFER = $3084;
  EGL_BIND_TO_TEXTURE_RGB = $3039;
  EGL_BIND_TO_TEXTURE_RGBA = $303A;
  EGL_CONTEXT_LOST = $300E;
  EGL_MIN_SWAP_INTERVAL = $303B;
  EGL_MAX_SWAP_INTERVAL = $303C;
  EGL_MIPMAP_TEXTURE = $3082;
  EGL_MIPMAP_LEVEL = $3083;
  EGL_NO_TEXTURE = $305C;
  EGL_TEXTURE_2D = $305F;
  EGL_TEXTURE_FORMAT = $3080;
  EGL_TEXTURE_RGB = $305D;
  EGL_TEXTURE_RGBA = $305E;
  EGL_TEXTURE_TARGET = $3081;

function eglBindTexImage(dpy: TEGLDisplay; surface: TEGLSurface; buffer: TEGLint): TEGLBoolean; cdecl; external;
function eglReleaseTexImage(dpy: TEGLDisplay; surface: TEGLSurface; buffer: TEGLint): TEGLBoolean; cdecl; external;
function eglSurfaceAttrib(dpy: TEGLDisplay; surface: TEGLSurface; attribute: TEGLint; Value: TEGLint): TEGLBoolean; cdecl; external;
function eglSwapInterval(dpy: TEGLDisplay; interval: TEGLint): TEGLBoolean; cdecl; external;
{ EGL_VERSION_1_1  }

const
  EGL_VERSION_1_2 = 1;

type
  PEGLenum = ^TEGLenum;
  TEGLenum = dword;

  PEGLClientBuffer = ^TEGLClientBuffer;
  TEGLClientBuffer = pointer;

const
  EGL_ALPHA_FORMAT = $3088;
  EGL_ALPHA_FORMAT_NONPRE = $308B;
  EGL_ALPHA_FORMAT_PRE = $308C;
  EGL_ALPHA_MASK_SIZE = $303E;
  EGL_BUFFER_PRESERVED = $3094;
  EGL_BUFFER_DESTROYED = $3095;
  EGL_CLIENT_APIS = $308D;
  EGL_COLORSPACE = $3087;
  EGL_COLORSPACE_sRGB = $3089;
  EGL_COLORSPACE_LINEAR = $308A;
  EGL_COLOR_BUFFER_TYPE = $303F;
  EGL_CONTEXT_CLIENT_TYPE = $3097;
  EGL_DISPLAY_SCALING = 10000;
  EGL_HORIZONTAL_RESOLUTION = $3090;
  EGL_LUMINANCE_BUFFER = $308F;
  EGL_LUMINANCE_SIZE = $303D;
  EGL_OPENGL_ES_BIT = $0001;
  EGL_OPENVG_BIT = $0002;
  EGL_OPENGL_ES_API = $30A0;
  EGL_OPENVG_API = $30A1;
  EGL_OPENVG_IMAGE = $3096;
  EGL_PIXEL_ASPECT_RATIO = $3092;
  EGL_RENDERABLE_TYPE = $3040;
  EGL_RENDER_BUFFER = $3086;
  EGL_RGB_BUFFER = $308E;
  EGL_SINGLE_BUFFER = $3085;
  EGL_SWAP_BEHAVIOR = $3093;

const
  EGL_VERTICAL_RESOLUTION = $3091;

function eglBindAPI(api: TEGLenum): TEGLBoolean; cdecl; external;
function eglQueryAPI: TEGLenum; cdecl; external;
function eglCreatePbufferFromClientBuffer(dpy: TEGLDisplay; buftype: TEGLenum; buffer: TEGLClientBuffer; config: TEGLConfig; attrib_list: PEGLint): TEGLSurface; cdecl; external;
function eglReleaseThread: TEGLBoolean; cdecl; external;
function eglWaitClient: TEGLBoolean; cdecl; external;
{ EGL_VERSION_1_2  }

const
  EGL_VERSION_1_3 = 1;
  EGL_CONFORMANT = $3042;
  EGL_CONTEXT_CLIENT_VERSION = $3098;
  EGL_MATCH_NATIVE_PIXMAP = $3041;
  EGL_OPENGL_ES2_BIT = $0004;
  EGL_VG_ALPHA_FORMAT = $3088;
  EGL_VG_ALPHA_FORMAT_NONPRE = $308B;
  EGL_VG_ALPHA_FORMAT_PRE = $308C;
  EGL_VG_ALPHA_FORMAT_PRE_BIT = $0040;
  EGL_VG_COLORSPACE = $3087;
  EGL_VG_COLORSPACE_sRGB = $3089;
  EGL_VG_COLORSPACE_LINEAR = $308A;
  EGL_VG_COLORSPACE_LINEAR_BIT = $0020;
  { EGL_VERSION_1_3  }

const
  EGL_VERSION_1_4 = 1;

const
  EGL_MULTISAMPLE_RESOLVE_BOX_BIT = $0200;
  EGL_MULTISAMPLE_RESOLVE = $3099;
  EGL_MULTISAMPLE_RESOLVE_DEFAULT = $309A;
  EGL_MULTISAMPLE_RESOLVE_BOX = $309B;
  EGL_OPENGL_API = $30A2;
  EGL_OPENGL_BIT = $0008;
  EGL_SWAP_BEHAVIOR_PRESERVED_BIT = $0400;

function eglGetCurrentContext: TEGLContext; cdecl; external;
{ EGL_VERSION_1_4  }

const
  EGL_VERSION_1_5 = 1;

type
  PEGLSync = ^TEGLSync;
  TEGLSync = pointer;

  PEGLAttrib = ^TEGLAttrib;
  TEGLAttrib = IntPtr;

  PEGLTime = ^TEGLTime;
  TEGLTime = Tkhronos_utime_nanoseconds_t;

  PEGLImage = ^TEGLImage;
  TEGLImage = pointer;

const
  EGL_CONTEXT_MAJOR_VERSION = $3098;
  EGL_CONTEXT_MINOR_VERSION = $30FB;
  EGL_CONTEXT_OPENGL_PROFILE_MASK = $30FD;
  EGL_CONTEXT_OPENGL_RESET_NOTIFICATION_STRATEGY = $31BD;
  EGL_NO_RESET_NOTIFICATION = $31BE;
  EGL_LOSE_CONTEXT_ON_RESET = $31BF;
  EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT = $00000001;
  EGL_CONTEXT_OPENGL_COMPATIBILITY_PROFILE_BIT = $00000002;
  EGL_CONTEXT_OPENGL_DEBUG = $31B0;
  EGL_CONTEXT_OPENGL_FORWARD_COMPATIBLE = $31B1;
  EGL_CONTEXT_OPENGL_ROBUST_ACCESS = $31B2;
  EGL_OPENGL_ES3_BIT = $00000040;
  EGL_CL_EVENT_HANDLE = $309C;
  EGL_SYNC_CL_EVENT = $30FE;
  EGL_SYNC_CL_EVENT_COMPLETE = $30FF;
  EGL_SYNC_PRIOR_COMMANDS_COMPLETE = $30F0;
  EGL_SYNC_TYPE = $30F7;
  EGL_SYNC_STATUS = $30F1;
  EGL_SYNC_CONDITION = $30F8;
  EGL_SIGNALED = $30F2;
  EGL_UNSIGNALED = $30F3;
  EGL_SYNC_FLUSH_COMMANDS_BIT = $0001;
  EGL_FOREVER = $FFFFFFFFFFFFFFFF;
  EGL_TIMEOUT_EXPIRED = $30F5;
  EGL_CONDITION_SATISFIED = $30F6;

const
  EGL_SYNC_FENCE = $30F9;
  EGL_GL_COLORSPACE = $309D;
  EGL_GL_COLORSPACE_SRGB = $3089;
  EGL_GL_COLORSPACE_LINEAR = $308A;
  EGL_GL_RENDERBUFFER = $30B9;
  EGL_GL_TEXTURE_2D = $30B1;
  EGL_GL_TEXTURE_LEVEL = $30BC;
  EGL_GL_TEXTURE_3D = $30B2;
  EGL_GL_TEXTURE_ZOFFSET = $30BD;
  EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_X = $30B3;
  EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_X = $30B4;
  EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Y = $30B5;
  EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = $30B6;
  EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Z = $30B7;
  EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = $30B8;
  EGL_IMAGE_PRESERVED = $30D2;

function eglCreateSync(dpy: TEGLDisplay; _type: TEGLenum; attrib_list: PEGLAttrib): TEGLSync; cdecl; external;
function eglDestroySync(dpy: TEGLDisplay; sync: TEGLSync): TEGLBoolean; cdecl; external;
function eglClientWaitSync(dpy: TEGLDisplay; sync: TEGLSync; flags: TEGLint; timeout: TEGLTime): TEGLint; cdecl; external;
function eglGetSyncAttrib(dpy: TEGLDisplay; sync: TEGLSync; attribute: TEGLint; Value: PEGLAttrib): TEGLBoolean; cdecl; external;
function eglCreateImage(dpy: TEGLDisplay; ctx: TEGLContext; target: TEGLenum; buffer: TEGLClientBuffer; attrib_list: PEGLAttrib): TEGLImage; cdecl; external;
function eglDestroyImage(dpy: TEGLDisplay; image: TEGLImage): TEGLBoolean; cdecl; external;
function eglGetPlatformDisplay(platform: TEGLenum; native_display: pointer; attrib_list: PEGLAttrib): TEGLDisplay; cdecl; external;
function eglCreatePlatformWindowSurface(dpy: TEGLDisplay; config: TEGLConfig; native_window: pointer; attrib_list: PEGLAttrib): TEGLSurface; cdecl; external;
function eglCreatePlatformPixmapSurface(dpy: TEGLDisplay; config: TEGLConfig; native_pixmap: pointer; attrib_list: PEGLAttrib): TEGLSurface; cdecl; external;
function eglWaitSync(dpy: TEGLDisplay; sync: TEGLSync; flags: TEGLint): TEGLBoolean; cdecl; external;
{ EGL_VERSION_1_5  }

const
  __eglext_h_ = 1;
  EGL_EGLEXT_VERSION = 20220525;

const
  EGL_KHR_cl_event = 1;
  EGL_CL_EVENT_HANDLE_KHR = $309C;
  EGL_SYNC_CL_EVENT_KHR = $30FE;
  EGL_SYNC_CL_EVENT_COMPLETE_KHR = $30FF;

const
  EGL_KHR_cl_event2 = 1;

type
  PEGLSyncKHR = ^TEGLSyncKHR;
  TEGLSyncKHR = pointer;

  PEGLAttribKHR = ^TEGLAttribKHR;
  TEGLAttribKHR = IntPtr;

function eglCreateSync64KHR(dpy: TEGLDisplay; _type: TEGLenum; attrib_list: PEGLAttribKHR): TEGLSyncKHR; cdecl; external;

const
  EGL_KHR_client_get_all_proc_addresses = 1;

const
  EGL_KHR_config_attribs = 1;
  EGL_CONFORMANT_KHR = $3042;
  EGL_VG_COLORSPACE_LINEAR_BIT_KHR = $0020;
  EGL_VG_ALPHA_FORMAT_PRE_BIT_KHR = $0040;

const
  EGL_KHR_context_flush_control = 1;
  EGL_CONTEXT_RELEASE_BEHAVIOR_NONE_KHR = 0;
  EGL_CONTEXT_RELEASE_BEHAVIOR_KHR = $2097;
  EGL_CONTEXT_RELEASE_BEHAVIOR_FLUSH_KHR = $2098;

const
  EGL_KHR_create_context = 1;
  EGL_CONTEXT_MAJOR_VERSION_KHR = $3098;
  EGL_CONTEXT_MINOR_VERSION_KHR = $30FB;
  EGL_CONTEXT_FLAGS_KHR = $30FC;
  EGL_CONTEXT_OPENGL_PROFILE_MASK_KHR = $30FD;
  EGL_CONTEXT_OPENGL_RESET_NOTIFICATION_STRATEGY_KHR = $31BD;
  EGL_NO_RESET_NOTIFICATION_KHR = $31BE;
  EGL_LOSE_CONTEXT_ON_RESET_KHR = $31BF;
  EGL_CONTEXT_OPENGL_DEBUG_BIT_KHR = $00000001;
  EGL_CONTEXT_OPENGL_FORWARD_COMPATIBLE_BIT_KHR = $00000002;
  EGL_CONTEXT_OPENGL_ROBUST_ACCESS_BIT_KHR = $00000004;
  EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT_KHR = $00000001;
  EGL_CONTEXT_OPENGL_COMPATIBILITY_PROFILE_BIT_KHR = $00000002;
  EGL_OPENGL_ES3_BIT_KHR = $00000040;

const
  EGL_KHR_create_context_no_error = 1;
  EGL_CONTEXT_OPENGL_NO_ERROR_KHR = $31B3;

const
  EGL_KHR_debug = 1;

type
  PEGLLabelKHR = ^TEGLLabelKHR;
  TEGLLabelKHR = pointer;

  PEGLObjectKHR = ^TEGLObjectKHR;
  TEGLObjectKHR = pointer;

  TEGLDEBUGPROCKHR = procedure(error: TEGLenum; command: PChar; messageType: TEGLint; threadLabel: TEGLLabelKHR; objectLabel: TEGLLabelKHR;
    message: PChar); cdecl;

const
  EGL_OBJECT_THREAD_KHR = $33B0;
  EGL_OBJECT_DISPLAY_KHR = $33B1;
  EGL_OBJECT_CONTEXT_KHR = $33B2;
  EGL_OBJECT_SURFACE_KHR = $33B3;
  EGL_OBJECT_IMAGE_KHR = $33B4;
  EGL_OBJECT_SYNC_KHR = $33B5;
  EGL_OBJECT_STREAM_KHR = $33B6;
  EGL_DEBUG_MSG_CRITICAL_KHR = $33B9;
  EGL_DEBUG_MSG_ERROR_KHR = $33BA;
  EGL_DEBUG_MSG_WARN_KHR = $33BB;
  EGL_DEBUG_MSG_INFO_KHR = $33BC;
  EGL_DEBUG_CALLBACK_KHR = $33B8;

function eglDebugMessageControlKHR(callback: TEGLDEBUGPROCKHR; attrib_list: PEGLAttrib): TEGLint; cdecl; external;
function eglQueryDebugKHR(attribute: TEGLint; Value: PEGLAttrib): TEGLBoolean; cdecl; external;
function eglLabelObjectKHR(display: TEGLDisplay; objectType: TEGLenum; object_: TEGLObjectKHR; _label: TEGLLabelKHR): TEGLint; cdecl; external;

const
  EGL_KHR_display_reference = 1;
  EGL_TRACK_REFERENCES_KHR = $3352;

function eglQueryDisplayAttribKHR(dpy: TEGLDisplay; Name: TEGLint; Value: PEGLAttrib): TEGLBoolean; cdecl; external;

const
  EGL_KHR_fence_sync = 1;

type
  PEGLTimeKHR = ^TEGLTimeKHR;
  TEGLTimeKHR = Tkhronos_utime_nanoseconds_t;

const
  EGL_SYNC_PRIOR_COMMANDS_COMPLETE_KHR = $30F0;
  EGL_SYNC_CONDITION_KHR = $30F8;
  EGL_SYNC_FENCE_KHR = $30F9;

function eglCreateSyncKHR(dpy: TEGLDisplay; _type: TEGLenum; attrib_list: PEGLint): TEGLSyncKHR; cdecl; external;
function eglDestroySyncKHR(dpy: TEGLDisplay; sync: TEGLSyncKHR): TEGLBoolean; cdecl; external;
function eglClientWaitSyncKHR(dpy: TEGLDisplay; sync: TEGLSyncKHR; flags: TEGLint; timeout: TEGLTimeKHR): TEGLint; cdecl; external;
function eglGetSyncAttribKHR(dpy: TEGLDisplay; sync: TEGLSyncKHR; attribute: TEGLint; Value: PEGLint): TEGLBoolean; cdecl; external;

const
  EGL_KHR_get_all_proc_addresses = 1;

const
  EGL_KHR_gl_colorspace = 1;
  EGL_GL_COLORSPACE_KHR = $309D;
  EGL_GL_COLORSPACE_SRGB_KHR = $3089;
  EGL_GL_COLORSPACE_LINEAR_KHR = $308A;

const
  EGL_KHR_gl_renderbuffer_image = 1;
  EGL_GL_RENDERBUFFER_KHR = $30B9;

const
  EGL_KHR_gl_texture_2D_image = 1;
  EGL_GL_TEXTURE_2D_KHR = $30B1;
  EGL_GL_TEXTURE_LEVEL_KHR = $30BC;

const
  EGL_KHR_gl_texture_3D_image = 1;
  EGL_GL_TEXTURE_3D_KHR = $30B2;
  EGL_GL_TEXTURE_ZOFFSET_KHR = $30BD;

const
  EGL_KHR_gl_texture_cubemap_image = 1;
  EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_X_KHR = $30B3;
  EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_X_KHR = $30B4;
  EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Y_KHR = $30B5;
  EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_KHR = $30B6;
  EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Z_KHR = $30B7;
  EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_KHR = $30B8;

const
  EGL_KHR_image = 1;

type
  PEGLImageKHR = ^TEGLImageKHR;
  TEGLImageKHR = pointer;

const
  EGL_NATIVE_PIXMAP_KHR = $30B0;

function eglCreateImageKHR(dpy: TEGLDisplay; ctx: TEGLContext; target: TEGLenum; buffer: TEGLClientBuffer; attrib_list: PEGLint): TEGLImageKHR; cdecl; external;
function eglDestroyImageKHR(dpy: TEGLDisplay; image: TEGLImageKHR): TEGLBoolean; cdecl; external;

const
  EGL_KHR_image_base = 1;
  EGL_IMAGE_PRESERVED_KHR = $30D2;

const
  EGL_KHR_image_pixmap = 1;

const
  EGL_KHR_lock_surface = 1;
  EGL_READ_SURFACE_BIT_KHR = $0001;
  EGL_WRITE_SURFACE_BIT_KHR = $0002;
  EGL_LOCK_SURFACE_BIT_KHR = $0080;
  EGL_OPTIMAL_FORMAT_BIT_KHR = $0100;
  EGL_MATCH_FORMAT_KHR = $3043;
  EGL_FORMAT_RGB_565_EXACT_KHR = $30C0;
  EGL_FORMAT_RGB_565_KHR = $30C1;
  EGL_FORMAT_RGBA_8888_EXACT_KHR = $30C2;
  EGL_FORMAT_RGBA_8888_KHR = $30C3;
  EGL_MAP_PRESERVE_PIXELS_KHR = $30C4;
  EGL_LOCK_USAGE_HINT_KHR = $30C5;
  EGL_BITMAP_POINTER_KHR = $30C6;
  EGL_BITMAP_PITCH_KHR = $30C7;
  EGL_BITMAP_ORIGIN_KHR = $30C8;
  EGL_BITMAP_PIXEL_RED_OFFSET_KHR = $30C9;
  EGL_BITMAP_PIXEL_GREEN_OFFSET_KHR = $30CA;
  EGL_BITMAP_PIXEL_BLUE_OFFSET_KHR = $30CB;
  EGL_BITMAP_PIXEL_ALPHA_OFFSET_KHR = $30CC;
  EGL_BITMAP_PIXEL_LUMINANCE_OFFSET_KHR = $30CD;
  EGL_LOWER_LEFT_KHR = $30CE;
  EGL_UPPER_LEFT_KHR = $30CF;

function eglLockSurfaceKHR(dpy: TEGLDisplay; surface: TEGLSurface; attrib_list: PEGLint): TEGLBoolean; cdecl; external;
function eglUnlockSurfaceKHR(dpy: TEGLDisplay; surface: TEGLSurface): TEGLBoolean; cdecl; external;

const
  EGL_KHR_lock_surface2 = 1;
  EGL_BITMAP_PIXEL_SIZE_KHR = $3110;

const
  EGL_KHR_lock_surface3 = 1;

function eglQuerySurface64KHR(dpy: TEGLDisplay; surface: TEGLSurface; attribute: TEGLint; Value: PEGLAttribKHR): TEGLBoolean; cdecl; external;

const
  EGL_KHR_mutable_render_buffer = 1;
  EGL_MUTABLE_RENDER_BUFFER_BIT_KHR = $1000;

const
  EGL_KHR_no_config_context = 1;

const
  EGL_KHR_partial_update = 1;
  EGL_BUFFER_AGE_KHR = $313D;

function eglSetDamageRegionKHR(dpy: TEGLDisplay; surface: TEGLSurface; rects: PEGLint; n_rects: TEGLint): TEGLBoolean; cdecl; external;

const
  EGL_KHR_platform_android = 1;
  EGL_PLATFORM_ANDROID_KHR = $3141;

const
  EGL_KHR_platform_gbm = 1;
  EGL_PLATFORM_GBM_KHR = $31D7;

const
  EGL_KHR_platform_wayland = 1;
  EGL_PLATFORM_WAYLAND_KHR = $31D8;

const
  EGL_KHR_platform_x11 = 1;
  EGL_PLATFORM_X11_KHR = $31D5;
  EGL_PLATFORM_X11_SCREEN_KHR = $31D6;

const
  EGL_KHR_reusable_sync = 1;
  EGL_SYNC_STATUS_KHR = $30F1;
  EGL_SIGNALED_KHR = $30F2;
  EGL_UNSIGNALED_KHR = $30F3;
  EGL_TIMEOUT_EXPIRED_KHR = $30F5;
  EGL_CONDITION_SATISFIED_KHR = $30F6;
  EGL_SYNC_TYPE_KHR = $30F7;
  EGL_SYNC_REUSABLE_KHR = $30FA;
  EGL_SYNC_FLUSH_COMMANDS_BIT_KHR = $0001;
  EGL_FOREVER_KHR = $FFFFFFFFFFFFFFFF;

function eglSignalSyncKHR(dpy: TEGLDisplay; sync: TEGLSyncKHR; mode: TEGLenum): TEGLBoolean; cdecl; external;

const
  EGL_KHR_stream = 1;

type
  PEGLStreamKHR = ^TEGLStreamKHR;
  TEGLStreamKHR = pointer;

  PEGLuint64KHR = ^TEGLuint64KHR;
  TEGLuint64KHR = Tkhronos_uint64_t;

const
  EGL_CONSUMER_LATENCY_USEC_KHR = $3210;
  EGL_PRODUCER_FRAME_KHR = $3212;
  EGL_CONSUMER_FRAME_KHR = $3213;
  EGL_STREAM_STATE_KHR = $3214;
  EGL_STREAM_STATE_CREATED_KHR = $3215;
  EGL_STREAM_STATE_CONNECTING_KHR = $3216;
  EGL_STREAM_STATE_EMPTY_KHR = $3217;
  EGL_STREAM_STATE_NEW_FRAME_AVAILABLE_KHR = $3218;
  EGL_STREAM_STATE_OLD_FRAME_AVAILABLE_KHR = $3219;
  EGL_STREAM_STATE_DISCONNECTED_KHR = $321A;
  EGL_BAD_STREAM_KHR = $321B;
  EGL_BAD_STATE_KHR = $321C;

function eglCreateStreamKHR(dpy: TEGLDisplay; attrib_list: PEGLint): TEGLStreamKHR; cdecl; external;
function eglDestroyStreamKHR(dpy: TEGLDisplay; stream: TEGLStreamKHR): TEGLBoolean; cdecl; external;
function eglStreamAttribKHR(dpy: TEGLDisplay; stream: TEGLStreamKHR; attribute: TEGLenum; Value: TEGLint): TEGLBoolean; cdecl; external;
function eglQueryStreamKHR(dpy: TEGLDisplay; stream: TEGLStreamKHR; attribute: TEGLenum; Value: PEGLint): TEGLBoolean; cdecl; external;
function eglQueryStreamu64KHR(dpy: TEGLDisplay; stream: TEGLStreamKHR; attribute: TEGLenum; Value: PEGLuint64KHR): TEGLBoolean; cdecl; external;

const
  EGL_KHR_stream_attrib = 1;

function eglCreateStreamAttribKHR(dpy: TEGLDisplay; attrib_list: PEGLAttrib): TEGLStreamKHR; cdecl; external;
function eglSetStreamAttribKHR(dpy: TEGLDisplay; stream: TEGLStreamKHR; attribute: TEGLenum; Value: TEGLAttrib): TEGLBoolean; cdecl; external;
function eglQueryStreamAttribKHR(dpy: TEGLDisplay; stream: TEGLStreamKHR; attribute: TEGLenum; Value: PEGLAttrib): TEGLBoolean; cdecl; external;
function eglStreamConsumerAcquireAttribKHR(dpy: TEGLDisplay; stream: TEGLStreamKHR; attrib_list: PEGLAttrib): TEGLBoolean; cdecl; external;
function eglStreamConsumerReleaseAttribKHR(dpy: TEGLDisplay; stream: TEGLStreamKHR; attrib_list: PEGLAttrib): TEGLBoolean; cdecl; external;

const
  EGL_KHR_stream_consumer_gltexture = 1;
  EGL_CONSUMER_ACQUIRE_TIMEOUT_USEC_KHR = $321E;

function eglStreamConsumerGLTextureExternalKHR(dpy: TEGLDisplay; stream: TEGLStreamKHR): TEGLBoolean; cdecl; external;
function eglStreamConsumerAcquireKHR(dpy: TEGLDisplay; stream: TEGLStreamKHR): TEGLBoolean; cdecl; external;
function eglStreamConsumerReleaseKHR(dpy: TEGLDisplay; stream: TEGLStreamKHR): TEGLBoolean; cdecl; external;

const
  EGL_KHR_stream_cross_process_fd = 1;

type
  PEGLNativeFileDescriptorKHR = ^TEGLNativeFileDescriptorKHR;
  TEGLNativeFileDescriptorKHR = longint;

function eglGetStreamFileDescriptorKHR(dpy: TEGLDisplay; stream: TEGLStreamKHR): TEGLNativeFileDescriptorKHR; cdecl; external;
function eglCreateStreamFromFileDescriptorKHR(dpy: TEGLDisplay; file_descriptor: TEGLNativeFileDescriptorKHR): TEGLStreamKHR; cdecl; external;

const
  EGL_KHR_stream_fifo = 1;
  EGL_STREAM_FIFO_LENGTH_KHR = $31FC;
  EGL_STREAM_TIME_NOW_KHR = $31FD;
  EGL_STREAM_TIME_CONSUMER_KHR = $31FE;
  EGL_STREAM_TIME_PRODUCER_KHR = $31FF;

function eglQueryStreamTimeKHR(dpy: TEGLDisplay; stream: TEGLStreamKHR; attribute: TEGLenum; Value: PEGLTimeKHR): TEGLBoolean; cdecl; external;

const
  EGL_KHR_stream_producer_aldatalocator = 1;

const
  EGL_KHR_stream_producer_eglsurface = 1;
  EGL_STREAM_BIT_KHR = $0800;

function eglCreateStreamProducerSurfaceKHR(dpy: TEGLDisplay; config: TEGLConfig; stream: TEGLStreamKHR; attrib_list: PEGLint): TEGLSurface; cdecl; external;

const
  EGL_KHR_surfaceless_context = 1;

const
  EGL_KHR_swap_buffers_with_damage = 1;

function eglSwapBuffersWithDamageKHR(dpy: TEGLDisplay; surface: TEGLSurface; rects: PEGLint; n_rects: TEGLint): TEGLBoolean; cdecl; external;

const
  EGL_KHR_vg_parent_image = 1;
  EGL_VG_PARENT_IMAGE_KHR = $30BA;

const
  EGL_KHR_wait_sync = 1;

function eglWaitSyncKHR(dpy: TEGLDisplay; sync: TEGLSyncKHR; flags: TEGLint): TEGLint; cdecl; external;

const
  EGL_ANDROID_GLES_layers = 1;

const
  EGL_ANDROID_blob_cache = 1;

type
  PEGLsizeiANDROID = ^TEGLsizeiANDROID;
  TEGLsizeiANDROID = Tkhronos_ssize_t;

  TEGLSetBlobFuncANDROID = procedure(key: pointer; keySize: TEGLsizeiANDROID; Value: pointer; valueSize: TEGLsizeiANDROID); cdecl;

  TEGLGetBlobFuncANDROID = function(key: pointer; keySize: TEGLsizeiANDROID; Value: pointer; valueSize: TEGLsizeiANDROID): TEGLsizeiANDROID; cdecl;

const
  EGL_ANDROID_create_native_client_buffer = 1;
  EGL_NATIVE_BUFFER_USAGE_ANDROID = $3143;
  EGL_NATIVE_BUFFER_USAGE_PROTECTED_BIT_ANDROID = $00000001;
  EGL_NATIVE_BUFFER_USAGE_RENDERBUFFER_BIT_ANDROID = $00000002;
  EGL_NATIVE_BUFFER_USAGE_TEXTURE_BIT_ANDROID = $00000004;

function eglCreateNativeClientBufferANDROID(attrib_list: PEGLint): TEGLClientBuffer; cdecl; external;

const
  EGL_ANDROID_framebuffer_target = 1;
  EGL_FRAMEBUFFER_TARGET_ANDROID = $3147;

const
  EGL_ANDROID_front_buffer_auto_refresh = 1;
  EGL_FRONT_BUFFER_AUTO_REFRESH_ANDROID = $314C;

const
  EGL_ANDROID_get_frame_timestamps = 1;

type
  PEGLnsecsANDROID = ^TEGLnsecsANDROID;
  TEGLnsecsANDROID = Tkhronos_stime_nanoseconds_t;

const
  EGL_TIMESTAMPS_ANDROID = $3430;
  EGL_COMPOSITE_DEADLINE_ANDROID = $3431;
  EGL_COMPOSITE_INTERVAL_ANDROID = $3432;
  EGL_COMPOSITE_TO_PRESENT_LATENCY_ANDROID = $3433;
  EGL_REQUESTED_PRESENT_TIME_ANDROID = $3434;
  EGL_RENDERING_COMPLETE_TIME_ANDROID = $3435;
  EGL_COMPOSITION_LATCH_TIME_ANDROID = $3436;
  EGL_FIRST_COMPOSITION_START_TIME_ANDROID = $3437;
  EGL_LAST_COMPOSITION_START_TIME_ANDROID = $3438;
  EGL_FIRST_COMPOSITION_GPU_FINISHED_TIME_ANDROID = $3439;
  EGL_DISPLAY_PRESENT_TIME_ANDROID = $343A;
  EGL_DEQUEUE_READY_TIME_ANDROID = $343B;
  EGL_READS_DONE_TIME_ANDROID = $343C;

function eglGetCompositorTimingSupportedANDROID(dpy: TEGLDisplay; surface: TEGLSurface; Name: TEGLint): TEGLBoolean; cdecl; external;
function eglGetCompositorTimingANDROID(dpy: TEGLDisplay; surface: TEGLSurface; numTimestamps: TEGLint; names: PEGLint; values: PEGLnsecsANDROID): TEGLBoolean; cdecl; external;
function eglGetNextFrameIdANDROID(dpy: TEGLDisplay; surface: TEGLSurface; frameId: PEGLuint64KHR): TEGLBoolean; cdecl; external;
function eglGetFrameTimestampSupportedANDROID(dpy: TEGLDisplay; surface: TEGLSurface; timestamp: TEGLint): TEGLBoolean; cdecl; external;
function eglGetFrameTimestampsANDROID(dpy: TEGLDisplay; surface: TEGLSurface; frameId: TEGLuint64KHR; numTimestamps: TEGLint; timestamps: PEGLint;
  values: PEGLnsecsANDROID): TEGLBoolean; cdecl; external;

const
  EGL_ANDROID_get_native_client_buffer = 1;

type
  PAHardwareBuffer = ^TAHardwareBuffer;
  TAHardwareBuffer = record
    {undefined structure}
  end;


function eglGetNativeClientBufferANDROID(buffer: PAHardwareBuffer): TEGLClientBuffer; cdecl; external;

const
  EGL_ANDROID_image_native_buffer = 1;
  EGL_NATIVE_BUFFER_ANDROID = $3140;

const
  EGL_ANDROID_native_fence_sync = 1;
  EGL_SYNC_NATIVE_FENCE_ANDROID = $3144;
  EGL_SYNC_NATIVE_FENCE_FD_ANDROID = $3145;
  EGL_SYNC_NATIVE_FENCE_SIGNALED_ANDROID = $3146;
  EGL_NO_NATIVE_FENCE_FD_ANDROID = -(1);

function eglDupNativeFenceFDANDROID(dpy: TEGLDisplay; sync: TEGLSyncKHR): TEGLint; cdecl; external;

const
  EGL_ANDROID_presentation_time = 1;

function eglPresentationTimeANDROID(dpy: TEGLDisplay; surface: TEGLSurface; time: TEGLnsecsANDROID): TEGLBoolean; cdecl; external;

const
  EGL_ANDROID_recordable = 1;
  EGL_RECORDABLE_ANDROID = $3142;

const
  EGL_ANGLE_d3d_share_handle_client_buffer = 1;
  EGL_D3D_TEXTURE_2D_SHARE_HANDLE_ANGLE = $3200;

const
  EGL_ANGLE_device_d3d = 1;
  EGL_D3D9_DEVICE_ANGLE = $33A0;
  EGL_D3D11_DEVICE_ANGLE = $33A1;

const
  EGL_ANGLE_query_surface_pointer = 1;

function eglQuerySurfacePointerANGLE(dpy: TEGLDisplay; surface: TEGLSurface; attribute: TEGLint; Value: Ppointer): TEGLBoolean; cdecl; external;

const
  EGL_ANGLE_surface_d3d_texture_2d_share_handle = 1;

const
  EGL_ANGLE_sync_control_rate = 1;

function eglGetMscRateANGLE(dpy: TEGLDisplay; surface: TEGLSurface; numerator: PEGLint; denominator: PEGLint): TEGLBoolean; cdecl; external;

const
  EGL_ANGLE_window_fixed_size = 1;
  EGL_FIXED_SIZE_ANGLE = $3201;

const
  EGL_ARM_image_format = 1;
  EGL_COLOR_COMPONENT_TYPE_UNSIGNED_INTEGER_ARM = $3287;
  EGL_COLOR_COMPONENT_TYPE_INTEGER_ARM = $3288;

const
  EGL_ARM_implicit_external_sync = 1;
  EGL_SYNC_PRIOR_COMMANDS_IMPLICIT_EXTERNAL_ARM = $328A;

const
  EGL_ARM_pixmap_multisample_discard = 1;
  EGL_DISCARD_SAMPLES_ARM = $3286;

const
  EGL_EXT_bind_to_front = 1;
  EGL_FRONT_BUFFER_EXT = $3464;

const
  EGL_EXT_buffer_age = 1;
  EGL_BUFFER_AGE_EXT = $313D;

const
  EGL_EXT_client_extensions = 1;

const
  EGL_EXT_client_sync = 1;
  EGL_SYNC_CLIENT_EXT = $3364;
  EGL_SYNC_CLIENT_SIGNAL_EXT = $3365;

function eglClientSignalSyncEXT(dpy: TEGLDisplay; sync: TEGLSync; attrib_list: PEGLAttrib): TEGLBoolean; cdecl; external;

const
  EGL_EXT_compositor = 1;
  EGL_PRIMARY_COMPOSITOR_CONTEXT_EXT = $3460;
  EGL_EXTERNAL_REF_ID_EXT = $3461;
  EGL_COMPOSITOR_DROP_NEWEST_FRAME_EXT = $3462;
  EGL_COMPOSITOR_KEEP_NEWEST_FRAME_EXT = $3463;

function eglCompositorSetContextListEXT(external_ref_ids: PEGLint; num_entries: TEGLint): TEGLBoolean; cdecl; external;
function eglCompositorSetContextAttributesEXT(external_ref_id: TEGLint; context_attributes: PEGLint; num_entries: TEGLint): TEGLBoolean; cdecl; external;
function eglCompositorSetWindowListEXT(external_ref_id: TEGLint; external_win_ids: PEGLint; num_entries: TEGLint): TEGLBoolean; cdecl; external;
function eglCompositorSetWindowAttributesEXT(external_win_id: TEGLint; window_attributes: PEGLint; num_entries: TEGLint): TEGLBoolean; cdecl; external;
function eglCompositorBindTexWindowEXT(external_win_id: TEGLint): TEGLBoolean; cdecl; external;
function eglCompositorSetSizeEXT(external_win_id: TEGLint; Width: TEGLint; Height: TEGLint): TEGLBoolean; cdecl; external;
function eglCompositorSwapPolicyEXT(external_win_id: TEGLint; policy: TEGLint): TEGLBoolean; cdecl; external;

const
  EGL_EXT_config_select_group = 1;
  EGL_CONFIG_SELECT_GROUP_EXT = $34C0;

const
  EGL_EXT_create_context_robustness = 1;
  EGL_CONTEXT_OPENGL_ROBUST_ACCESS_EXT = $30BF;
  EGL_CONTEXT_OPENGL_RESET_NOTIFICATION_STRATEGY_EXT = $3138;
  EGL_NO_RESET_NOTIFICATION_EXT = $31BE;
  EGL_LOSE_CONTEXT_ON_RESET_EXT = $31BF;

const
  EGL_EXT_device_base = 1;

type
  PEGLDeviceEXT = ^TEGLDeviceEXT;
  TEGLDeviceEXT = pointer;

const
  EGL_BAD_DEVICE_EXT = $322B;
  EGL_DEVICE_EXT = $322C;

function eglQueryDeviceAttribEXT(device: TEGLDeviceEXT; attribute: TEGLint; Value: PEGLAttrib): TEGLBoolean; cdecl; external;
function eglQueryDeviceStringEXT(device: TEGLDeviceEXT; Name: TEGLint): PChar; cdecl; external;
function eglQueryDevicesEXT(max_devices: TEGLint; devices: PEGLDeviceEXT; num_devices: PEGLint): TEGLBoolean; cdecl; external;
function eglQueryDisplayAttribEXT(dpy: TEGLDisplay; attribute: TEGLint; Value: PEGLAttrib): TEGLBoolean; cdecl; external;

const
  EGL_EXT_device_drm = 1;
  EGL_DRM_DEVICE_FILE_EXT = $3233;
  EGL_DRM_MASTER_FD_EXT = $333C;

const
  EGL_EXT_device_drm_render_node = 1;
  EGL_DRM_RENDER_NODE_FILE_EXT = $3377;

const
  EGL_EXT_device_enumeration = 1;

const
  EGL_EXT_device_openwf = 1;
  EGL_OPENWF_DEVICE_ID_EXT = $3237;
  EGL_OPENWF_DEVICE_EXT = $333D;

const
  EGL_EXT_device_persistent_id = 1;
  EGL_DEVICE_UUID_EXT = $335C;
  EGL_DRIVER_UUID_EXT = $335D;
  EGL_DRIVER_NAME_EXT = $335E;

function eglQueryDeviceBinaryEXT(device: TEGLDeviceEXT; Name: TEGLint; max_size: TEGLint; Value: pointer; size: PEGLint): TEGLBoolean; cdecl; external;

const
  EGL_EXT_device_query = 1;

const
  EGL_EXT_device_query_name = 1;
  EGL_RENDERER_EXT = $335F;

const
  EGL_EXT_explicit_device = 1;

const
  EGL_EXT_gl_colorspace_bt2020_linear = 1;
  EGL_GL_COLORSPACE_BT2020_LINEAR_EXT = $333F;

const
  EGL_EXT_gl_colorspace_bt2020_pq = 1;
  EGL_GL_COLORSPACE_BT2020_PQ_EXT = $3340;

const
  EGL_EXT_gl_colorspace_display_p3 = 1;
  EGL_GL_COLORSPACE_DISPLAY_P3_EXT = $3363;

const
  EGL_EXT_gl_colorspace_display_p3_linear = 1;
  EGL_GL_COLORSPACE_DISPLAY_P3_LINEAR_EXT = $3362;

const
  EGL_EXT_gl_colorspace_display_p3_passthrough = 1;
  EGL_GL_COLORSPACE_DISPLAY_P3_PASSTHROUGH_EXT = $3490;

const
  EGL_EXT_gl_colorspace_scrgb = 1;
  EGL_GL_COLORSPACE_SCRGB_EXT = $3351;

const
  EGL_EXT_gl_colorspace_scrgb_linear = 1;
  EGL_GL_COLORSPACE_SCRGB_LINEAR_EXT = $3350;

const
  EGL_EXT_image_dma_buf_import = 1;
  EGL_LINUX_DMA_BUF_EXT = $3270;
  EGL_LINUX_DRM_FOURCC_EXT = $3271;
  EGL_DMA_BUF_PLANE0_FD_EXT = $3272;
  EGL_DMA_BUF_PLANE0_OFFSET_EXT = $3273;
  EGL_DMA_BUF_PLANE0_PITCH_EXT = $3274;
  EGL_DMA_BUF_PLANE1_FD_EXT = $3275;
  EGL_DMA_BUF_PLANE1_OFFSET_EXT = $3276;
  EGL_DMA_BUF_PLANE1_PITCH_EXT = $3277;
  EGL_DMA_BUF_PLANE2_FD_EXT = $3278;
  EGL_DMA_BUF_PLANE2_OFFSET_EXT = $3279;
  EGL_DMA_BUF_PLANE2_PITCH_EXT = $327A;
  EGL_YUV_COLOR_SPACE_HINT_EXT = $327B;
  EGL_SAMPLE_RANGE_HINT_EXT = $327C;
  EGL_YUV_CHROMA_HORIZONTAL_SITING_HINT_EXT = $327D;
  EGL_YUV_CHROMA_VERTICAL_SITING_HINT_EXT = $327E;
  EGL_ITU_REC601_EXT = $327F;
  EGL_ITU_REC709_EXT = $3280;
  EGL_ITU_REC2020_EXT = $3281;
  EGL_YUV_FULL_RANGE_EXT = $3282;
  EGL_YUV_NARROW_RANGE_EXT = $3283;
  EGL_YUV_CHROMA_SITING_0_EXT = $3284;
  EGL_YUV_CHROMA_SITING_0_5_EXT = $3285;

const
  EGL_EXT_image_dma_buf_import_modifiers = 1;
  EGL_DMA_BUF_PLANE3_FD_EXT = $3440;
  EGL_DMA_BUF_PLANE3_OFFSET_EXT = $3441;
  EGL_DMA_BUF_PLANE3_PITCH_EXT = $3442;
  EGL_DMA_BUF_PLANE0_MODIFIER_LO_EXT = $3443;
  EGL_DMA_BUF_PLANE0_MODIFIER_HI_EXT = $3444;
  EGL_DMA_BUF_PLANE1_MODIFIER_LO_EXT = $3445;
  EGL_DMA_BUF_PLANE1_MODIFIER_HI_EXT = $3446;
  EGL_DMA_BUF_PLANE2_MODIFIER_LO_EXT = $3447;
  EGL_DMA_BUF_PLANE2_MODIFIER_HI_EXT = $3448;
  EGL_DMA_BUF_PLANE3_MODIFIER_LO_EXT = $3449;
  EGL_DMA_BUF_PLANE3_MODIFIER_HI_EXT = $344A;

function eglQueryDmaBufFormatsEXT(dpy: TEGLDisplay; max_formats: TEGLint; formats: PEGLint; num_formats: PEGLint): TEGLBoolean; cdecl; external;
function eglQueryDmaBufModifiersEXT(dpy: TEGLDisplay; format: TEGLint; max_modifiers: TEGLint; modifiers: PEGLuint64KHR; external_only: PEGLBoolean;
  num_modifiers: PEGLint): TEGLBoolean; cdecl; external;

const
  EGL_EXT_image_gl_colorspace = 1;
  EGL_GL_COLORSPACE_DEFAULT_EXT = $314D;

const
  EGL_EXT_image_implicit_sync_control = 1;
  EGL_IMPORT_SYNC_TYPE_EXT = $3470;
  EGL_IMPORT_IMPLICIT_SYNC_EXT = $3471;
  EGL_IMPORT_EXPLICIT_SYNC_EXT = $3472;

const
  EGL_EXT_multiview_window = 1;
  EGL_MULTIVIEW_VIEW_COUNT_EXT = $3134;

const
  EGL_EXT_output_base = 1;

type
  PEGLOutputLayerEXT = ^TEGLOutputLayerEXT;
  TEGLOutputLayerEXT = pointer;

  PEGLOutputPortEXT = ^TEGLOutputPortEXT;
  TEGLOutputPortEXT = pointer;

const
  EGL_BAD_OUTPUT_LAYER_EXT = $322D;
  EGL_BAD_OUTPUT_PORT_EXT = $322E;
  EGL_SWAP_INTERVAL_EXT = $322F;

function eglGetOutputLayersEXT(dpy: TEGLDisplay; attrib_list: PEGLAttrib; layers: PEGLOutputLayerEXT; max_layers: TEGLint; num_layers: PEGLint): TEGLBoolean; cdecl; external;
function eglGetOutputPortsEXT(dpy: TEGLDisplay; attrib_list: PEGLAttrib; ports: PEGLOutputPortEXT; max_ports: TEGLint; num_ports: PEGLint): TEGLBoolean; cdecl; external;
function eglOutputLayerAttribEXT(dpy: TEGLDisplay; layer: TEGLOutputLayerEXT; attribute: TEGLint; Value: TEGLAttrib): TEGLBoolean; cdecl; external;
function eglQueryOutputLayerAttribEXT(dpy: TEGLDisplay; layer: TEGLOutputLayerEXT; attribute: TEGLint; Value: PEGLAttrib): TEGLBoolean; cdecl; external;
function eglQueryOutputLayerStringEXT(dpy: TEGLDisplay; layer: TEGLOutputLayerEXT; Name: TEGLint): PChar; cdecl; external;
function eglOutputPortAttribEXT(dpy: TEGLDisplay; port: TEGLOutputPortEXT; attribute: TEGLint; Value: TEGLAttrib): TEGLBoolean; cdecl; external;
function eglQueryOutputPortAttribEXT(dpy: TEGLDisplay; port: TEGLOutputPortEXT; attribute: TEGLint; Value: PEGLAttrib): TEGLBoolean; cdecl; external;
function eglQueryOutputPortStringEXT(dpy: TEGLDisplay; port: TEGLOutputPortEXT; Name: TEGLint): PChar; cdecl; external;

const
  EGL_EXT_output_drm = 1;
  EGL_DRM_CRTC_EXT = $3234;
  EGL_DRM_PLANE_EXT = $3235;
  EGL_DRM_CONNECTOR_EXT = $3236;

const
  EGL_EXT_output_openwf = 1;
  EGL_OPENWF_PIPELINE_ID_EXT = $3238;
  EGL_OPENWF_PORT_ID_EXT = $3239;

const
  EGL_EXT_pixel_format_float = 1;
  EGL_COLOR_COMPONENT_TYPE_EXT = $3339;
  EGL_COLOR_COMPONENT_TYPE_FIXED_EXT = $333A;
  EGL_COLOR_COMPONENT_TYPE_FLOAT_EXT = $333B;

const
  EGL_EXT_platform_base = 1;

function eglGetPlatformDisplayEXT(platform: TEGLenum; native_display: pointer; attrib_list: PEGLint): TEGLDisplay; cdecl; external;
function eglCreatePlatformWindowSurfaceEXT(dpy: TEGLDisplay; config: TEGLConfig; native_window: pointer; attrib_list: PEGLint): TEGLSurface; cdecl; external;
function eglCreatePlatformPixmapSurfaceEXT(dpy: TEGLDisplay; config: TEGLConfig; native_pixmap: pointer; attrib_list: PEGLint): TEGLSurface; cdecl; external;

const
  EGL_EXT_platform_device = 1;
  EGL_PLATFORM_DEVICE_EXT = $313F;

const
  EGL_EXT_platform_wayland = 1;
  EGL_PLATFORM_WAYLAND_EXT = $31D8;

const
  EGL_EXT_platform_x11 = 1;
  EGL_PLATFORM_X11_EXT = $31D5;
  EGL_PLATFORM_X11_SCREEN_EXT = $31D6;

const
  EGL_EXT_platform_xcb = 1;
  EGL_PLATFORM_XCB_EXT = $31DC;
  EGL_PLATFORM_XCB_SCREEN_EXT = $31DE;

const
  EGL_EXT_present_opaque = 1;
  EGL_PRESENT_OPAQUE_EXT = $31DF;

const
  EGL_EXT_protected_content = 1;
  EGL_PROTECTED_CONTENT_EXT = $32C0;

const
  EGL_EXT_protected_surface = 1;

const
  EGL_EXT_stream_consumer_egloutput = 1;

function eglStreamConsumerOutputEXT(dpy: TEGLDisplay; stream: TEGLStreamKHR; layer: TEGLOutputLayerEXT): TEGLBoolean; cdecl; external;

const
  EGL_EXT_surface_CTA861_3_metadata = 1;
  EGL_CTA861_3_MAX_CONTENT_LIGHT_LEVEL_EXT = $3360;
  EGL_CTA861_3_MAX_FRAME_AVERAGE_LEVEL_EXT = $3361;

const
  EGL_EXT_surface_SMPTE2086_metadata = 1;
  EGL_SMPTE2086_DISPLAY_PRIMARY_RX_EXT = $3341;
  EGL_SMPTE2086_DISPLAY_PRIMARY_RY_EXT = $3342;
  EGL_SMPTE2086_DISPLAY_PRIMARY_GX_EXT = $3343;
  EGL_SMPTE2086_DISPLAY_PRIMARY_GY_EXT = $3344;
  EGL_SMPTE2086_DISPLAY_PRIMARY_BX_EXT = $3345;
  EGL_SMPTE2086_DISPLAY_PRIMARY_BY_EXT = $3346;
  EGL_SMPTE2086_WHITE_POINT_X_EXT = $3347;
  EGL_SMPTE2086_WHITE_POINT_Y_EXT = $3348;
  EGL_SMPTE2086_MAX_LUMINANCE_EXT = $3349;
  EGL_SMPTE2086_MIN_LUMINANCE_EXT = $334A;
  EGL_METADATA_SCALING_EXT = 50000;

const
  EGL_EXT_surface_compression = 1;
  EGL_SURFACE_COMPRESSION_EXT = $34B0;
  EGL_SURFACE_COMPRESSION_PLANE1_EXT = $328E;
  EGL_SURFACE_COMPRESSION_PLANE2_EXT = $328F;
  EGL_SURFACE_COMPRESSION_FIXED_RATE_NONE_EXT = $34B1;
  EGL_SURFACE_COMPRESSION_FIXED_RATE_DEFAULT_EXT = $34B2;
  EGL_SURFACE_COMPRESSION_FIXED_RATE_1BPC_EXT = $34B4;
  EGL_SURFACE_COMPRESSION_FIXED_RATE_2BPC_EXT = $34B5;
  EGL_SURFACE_COMPRESSION_FIXED_RATE_3BPC_EXT = $34B6;
  EGL_SURFACE_COMPRESSION_FIXED_RATE_4BPC_EXT = $34B7;
  EGL_SURFACE_COMPRESSION_FIXED_RATE_5BPC_EXT = $34B8;
  EGL_SURFACE_COMPRESSION_FIXED_RATE_6BPC_EXT = $34B9;
  EGL_SURFACE_COMPRESSION_FIXED_RATE_7BPC_EXT = $34BA;
  EGL_SURFACE_COMPRESSION_FIXED_RATE_8BPC_EXT = $34BB;
  EGL_SURFACE_COMPRESSION_FIXED_RATE_9BPC_EXT = $34BC;
  EGL_SURFACE_COMPRESSION_FIXED_RATE_10BPC_EXT = $34BD;
  EGL_SURFACE_COMPRESSION_FIXED_RATE_11BPC_EXT = $34BE;
  EGL_SURFACE_COMPRESSION_FIXED_RATE_12BPC_EXT = $34BF;

function eglQuerySupportedCompressionRatesEXT(dpy: TEGLDisplay; config: TEGLConfig; attrib_list: PEGLAttrib; rates: PEGLint; rate_size: TEGLint;
  num_rates: PEGLint): TEGLBoolean; cdecl; external;

const
  EGL_EXT_swap_buffers_with_damage = 1;

function eglSwapBuffersWithDamageEXT(dpy: TEGLDisplay; surface: TEGLSurface; rects: PEGLint; n_rects: TEGLint): TEGLBoolean; cdecl; external;

const
  EGL_EXT_sync_reuse = 1;

function eglUnsignalSyncEXT(dpy: TEGLDisplay; sync: TEGLSync; attrib_list: PEGLAttrib): TEGLBoolean; cdecl; external;

const
  EGL_EXT_yuv_surface = 1;
  EGL_YUV_ORDER_EXT = $3301;
  EGL_YUV_NUMBER_OF_PLANES_EXT = $3311;
  EGL_YUV_SUBSAMPLE_EXT = $3312;
  EGL_YUV_DEPTH_RANGE_EXT = $3317;
  EGL_YUV_CSC_STANDARD_EXT = $330A;
  EGL_YUV_PLANE_BPP_EXT = $331A;
  EGL_YUV_BUFFER_EXT = $3300;
  EGL_YUV_ORDER_YUV_EXT = $3302;
  EGL_YUV_ORDER_YVU_EXT = $3303;
  EGL_YUV_ORDER_YUYV_EXT = $3304;
  EGL_YUV_ORDER_UYVY_EXT = $3305;
  EGL_YUV_ORDER_YVYU_EXT = $3306;
  EGL_YUV_ORDER_VYUY_EXT = $3307;
  EGL_YUV_ORDER_AYUV_EXT = $3308;
  EGL_YUV_SUBSAMPLE_4_2_0_EXT = $3313;
  EGL_YUV_SUBSAMPLE_4_2_2_EXT = $3314;
  EGL_YUV_SUBSAMPLE_4_4_4_EXT = $3315;
  EGL_YUV_DEPTH_RANGE_LIMITED_EXT = $3318;
  EGL_YUV_DEPTH_RANGE_FULL_EXT = $3319;
  EGL_YUV_CSC_STANDARD_601_EXT = $330B;
  EGL_YUV_CSC_STANDARD_709_EXT = $330C;
  EGL_YUV_CSC_STANDARD_2020_EXT = $330D;
  EGL_YUV_PLANE_BPP_0_EXT = $331B;
  EGL_YUV_PLANE_BPP_8_EXT = $331C;
  EGL_YUV_PLANE_BPP_10_EXT = $331D;

const
  EGL_HI_clientpixmap = 1;

type
  PEGLClientPixmapHI = ^TEGLClientPixmapHI;

  TEGLClientPixmapHI = record
    pData: pointer;
    iWidth: TEGLint;
    iHeight: TEGLint;
    iStride: TEGLint;
  end;


const
  EGL_CLIENT_PIXMAP_POINTER_HI = $8F74;

function eglCreatePixmapSurfaceHI(dpy: TEGLDisplay; config: TEGLConfig; pixmap: PEGLClientPixmapHI): TEGLSurface; cdecl; external;

const
  EGL_HI_colorformats = 1;
  EGL_COLOR_FORMAT_HI = $8F70;
  EGL_COLOR_RGB_HI = $8F71;
  EGL_COLOR_RGBA_HI = $8F72;
  EGL_COLOR_ARGB_HI = $8F73;

const
  EGL_IMG_context_priority = 1;
  EGL_CONTEXT_PRIORITY_LEVEL_IMG = $3100;
  EGL_CONTEXT_PRIORITY_HIGH_IMG = $3101;
  EGL_CONTEXT_PRIORITY_MEDIUM_IMG = $3102;
  EGL_CONTEXT_PRIORITY_LOW_IMG = $3103;

const
  EGL_IMG_image_plane_attribs = 1;
  EGL_NATIVE_BUFFER_MULTIPLANE_SEPARATE_IMG = $3105;
  EGL_NATIVE_BUFFER_PLANE_OFFSET_IMG = $3106;

const
  EGL_MESA_drm_image = 1;
  EGL_DRM_BUFFER_FORMAT_MESA = $31D0;
  EGL_DRM_BUFFER_USE_MESA = $31D1;
  EGL_DRM_BUFFER_FORMAT_ARGB32_MESA = $31D2;
  EGL_DRM_BUFFER_MESA = $31D3;
  EGL_DRM_BUFFER_STRIDE_MESA = $31D4;
  EGL_DRM_BUFFER_USE_SCANOUT_MESA = $00000001;
  EGL_DRM_BUFFER_USE_SHARE_MESA = $00000002;
  EGL_DRM_BUFFER_USE_CURSOR_MESA = $00000004;

function eglCreateDRMImageMESA(dpy: TEGLDisplay; attrib_list: PEGLint): TEGLImageKHR; cdecl; external;
function eglExportDRMImageMESA(dpy: TEGLDisplay; image: TEGLImageKHR; Name: PEGLint; handle: PEGLint; stride: PEGLint): TEGLBoolean; cdecl; external;

const
  EGL_MESA_image_dma_buf_export = 1;

function eglExportDMABUFImageQueryMESA(dpy: TEGLDisplay; image: TEGLImageKHR; fourcc: Plongint; num_planes: Plongint; modifiers: PEGLuint64KHR): TEGLBoolean; cdecl; external;
function eglExportDMABUFImageMESA(dpy: TEGLDisplay; image: TEGLImageKHR; fds: Plongint; strides: PEGLint; offsets: PEGLint): TEGLBoolean; cdecl; external;

const
  EGL_MESA_platform_gbm = 1;
  EGL_PLATFORM_GBM_MESA = $31D7;

const
  EGL_MESA_platform_surfaceless = 1;
  EGL_PLATFORM_SURFACELESS_MESA = $31DD;

const
  EGL_MESA_query_driver = 1;

function eglGetDisplayDriverConfig(dpy: TEGLDisplay): PChar; cdecl; external;
function eglGetDisplayDriverName(dpy: TEGLDisplay): PChar; cdecl; external;

const
  EGL_NOK_swap_region = 1;

function eglSwapBuffersRegionNOK(dpy: TEGLDisplay; surface: TEGLSurface; numRects: TEGLint; rects: PEGLint): TEGLBoolean; cdecl; external;

const
  EGL_NOK_swap_region2 = 1;

function eglSwapBuffersRegion2NOK(dpy: TEGLDisplay; surface: TEGLSurface; numRects: TEGLint; rects: PEGLint): TEGLBoolean; cdecl; external;

const
  EGL_NOK_texture_from_pixmap = 1;
  EGL_Y_INVERTED_NOK = $307F;

const
  EGL_NV_3dvision_surface = 1;
  EGL_AUTO_STEREO_NV = $3136;

const
  EGL_NV_context_priority_realtime = 1;
  EGL_CONTEXT_PRIORITY_REALTIME_NV = $3357;

const
  EGL_NV_coverage_sample = 1;
  EGL_COVERAGE_BUFFERS_NV = $30E0;
  EGL_COVERAGE_SAMPLES_NV = $30E1;

const
  EGL_NV_coverage_sample_resolve = 1;
  EGL_COVERAGE_SAMPLE_RESOLVE_NV = $3131;
  EGL_COVERAGE_SAMPLE_RESOLVE_DEFAULT_NV = $3132;
  EGL_COVERAGE_SAMPLE_RESOLVE_NONE_NV = $3133;

const
  EGL_NV_cuda_event = 1;
  EGL_CUDA_EVENT_HANDLE_NV = $323B;
  EGL_SYNC_CUDA_EVENT_NV = $323C;
  EGL_SYNC_CUDA_EVENT_COMPLETE_NV = $323D;

const
  EGL_NV_depth_nonlinear = 1;
  EGL_DEPTH_ENCODING_NV = $30E2;
  EGL_DEPTH_ENCODING_NONE_NV = 0;
  EGL_DEPTH_ENCODING_NONLINEAR_NV = $30E3;

const
  EGL_NV_device_cuda = 1;
  EGL_CUDA_DEVICE_NV = $323A;

const
  EGL_NV_native_query = 1;

function eglQueryNativeDisplayNV(dpy: TEGLDisplay; display_id: PEGLNativeDisplayType): TEGLBoolean; cdecl; external;
function eglQueryNativeWindowNV(dpy: TEGLDisplay; surf: TEGLSurface; window: PEGLNativeWindowType): TEGLBoolean; cdecl; external;
function eglQueryNativePixmapNV(dpy: TEGLDisplay; surf: TEGLSurface; pixmap: PEGLNativePixmapType): TEGLBoolean; cdecl; external;

const
  EGL_NV_post_convert_rounding = 1;

const
  EGL_NV_post_sub_buffer = 1;
  EGL_POST_SUB_BUFFER_SUPPORTED_NV = $30BE;

function eglPostSubBufferNV(dpy: TEGLDisplay; surface: TEGLSurface; x: TEGLint; y: TEGLint; Width: TEGLint;
  Height: TEGLint): TEGLBoolean; cdecl; external;

const
  EGL_NV_quadruple_buffer = 1;
  EGL_QUADRUPLE_BUFFER_NV = $3231;

const
  EGL_NV_robustness_video_memory_purge = 1;
  EGL_GENERATE_RESET_ON_VIDEO_MEMORY_PURGE_NV = $334C;

const
  EGL_NV_stream_consumer_eglimage = 1;
  EGL_STREAM_CONSUMER_IMAGE_NV = $3373;
  EGL_STREAM_IMAGE_ADD_NV = $3374;
  EGL_STREAM_IMAGE_REMOVE_NV = $3375;
  EGL_STREAM_IMAGE_AVAILABLE_NV = $3376;

function eglStreamImageConsumerConnectNV(dpy: TEGLDisplay; stream: TEGLStreamKHR; num_modifiers: TEGLint; modifiers: PEGLuint64KHR; attrib_list: PEGLAttrib): TEGLBoolean; cdecl; external;
function eglQueryStreamConsumerEventNV(dpy: TEGLDisplay; stream: TEGLStreamKHR; timeout: TEGLTime; event: PEGLenum; aux: PEGLAttrib): TEGLint; cdecl; external;
function eglStreamAcquireImageNV(dpy: TEGLDisplay; stream: TEGLStreamKHR; pImage: PEGLImage; sync: TEGLSync): TEGLBoolean; cdecl; external;
function eglStreamReleaseImageNV(dpy: TEGLDisplay; stream: TEGLStreamKHR; image: TEGLImage; sync: TEGLSync): TEGLBoolean; cdecl; external;

const
  EGL_NV_stream_consumer_gltexture_yuv = 1;
  EGL_YUV_PLANE0_TEXTURE_UNIT_NV = $332C;
  EGL_YUV_PLANE1_TEXTURE_UNIT_NV = $332D;
  EGL_YUV_PLANE2_TEXTURE_UNIT_NV = $332E;

function eglStreamConsumerGLTextureExternalAttribsNV(dpy: TEGLDisplay; stream: TEGLStreamKHR; attrib_list: PEGLAttrib): TEGLBoolean; cdecl; external;

const
  EGL_NV_stream_cross_display = 1;
  EGL_STREAM_CROSS_DISPLAY_NV = $334E;

const
  EGL_NV_stream_cross_object = 1;
  EGL_STREAM_CROSS_OBJECT_NV = $334D;

const
  EGL_NV_stream_cross_partition = 1;
  EGL_STREAM_CROSS_PARTITION_NV = $323F;

const
  EGL_NV_stream_cross_process = 1;
  EGL_STREAM_CROSS_PROCESS_NV = $3245;

const
  EGL_NV_stream_cross_system = 1;
  EGL_STREAM_CROSS_SYSTEM_NV = $334F;

const
  EGL_NV_stream_dma = 1;
  EGL_STREAM_DMA_NV = $3371;
  EGL_STREAM_DMA_SERVER_NV = $3372;

const
  EGL_NV_stream_fifo_next = 1;
  EGL_PENDING_FRAME_NV = $3329;
  EGL_STREAM_TIME_PENDING_NV = $332A;

const
  EGL_NV_stream_fifo_synchronous = 1;
  EGL_STREAM_FIFO_SYNCHRONOUS_NV = $3336;

const
  EGL_NV_stream_flush = 1;

function eglStreamFlushNV(dpy: TEGLDisplay; stream: TEGLStreamKHR): TEGLBoolean; cdecl; external;

const
  EGL_NV_stream_frame_limits = 1;
  EGL_PRODUCER_MAX_FRAME_HINT_NV = $3337;
  EGL_CONSUMER_MAX_FRAME_HINT_NV = $3338;

const
  EGL_NV_stream_metadata = 1;
  EGL_MAX_STREAM_METADATA_BLOCKS_NV = $3250;
  EGL_MAX_STREAM_METADATA_BLOCK_SIZE_NV = $3251;
  EGL_MAX_STREAM_METADATA_TOTAL_SIZE_NV = $3252;
  EGL_PRODUCER_METADATA_NV = $3253;
  EGL_CONSUMER_METADATA_NV = $3254;
  EGL_PENDING_METADATA_NV = $3328;
  EGL_METADATA0_SIZE_NV = $3255;
  EGL_METADATA1_SIZE_NV = $3256;
  EGL_METADATA2_SIZE_NV = $3257;
  EGL_METADATA3_SIZE_NV = $3258;
  EGL_METADATA0_TYPE_NV = $3259;
  EGL_METADATA1_TYPE_NV = $325A;
  EGL_METADATA2_TYPE_NV = $325B;
  EGL_METADATA3_TYPE_NV = $325C;

function eglQueryDisplayAttribNV(dpy: TEGLDisplay; attribute: TEGLint; Value: PEGLAttrib): TEGLBoolean; cdecl; external;
function eglSetStreamMetadataNV(dpy: TEGLDisplay; stream: TEGLStreamKHR; n: TEGLint; offset: TEGLint; size: TEGLint;
  Data: pointer): TEGLBoolean; cdecl; external;
function eglQueryStreamMetadataNV(dpy: TEGLDisplay; stream: TEGLStreamKHR; Name: TEGLenum; n: TEGLint; offset: TEGLint;
  size: TEGLint; Data: pointer): TEGLBoolean; cdecl; external;

const
  EGL_NV_stream_origin = 1;
  EGL_STREAM_FRAME_ORIGIN_X_NV = $3366;
  EGL_STREAM_FRAME_ORIGIN_Y_NV = $3367;
  EGL_STREAM_FRAME_MAJOR_AXIS_NV = $3368;
  EGL_CONSUMER_AUTO_ORIENTATION_NV = $3369;
  EGL_PRODUCER_AUTO_ORIENTATION_NV = $336A;
  EGL_LEFT_NV = $336B;
  EGL_RIGHT_NV = $336C;
  EGL_TOP_NV = $336D;
  EGL_BOTTOM_NV = $336E;
  EGL_X_AXIS_NV = $336F;
  EGL_Y_AXIS_NV = $3370;

const
  EGL_NV_stream_remote = 1;
  EGL_STREAM_STATE_INITIALIZING_NV = $3240;
  EGL_STREAM_TYPE_NV = $3241;
  EGL_STREAM_PROTOCOL_NV = $3242;
  EGL_STREAM_ENDPOINT_NV = $3243;
  EGL_STREAM_LOCAL_NV = $3244;
  EGL_STREAM_PRODUCER_NV = $3247;
  EGL_STREAM_CONSUMER_NV = $3248;
  EGL_STREAM_PROTOCOL_FD_NV = $3246;

const
  EGL_NV_stream_reset = 1;
  EGL_SUPPORT_RESET_NV = $3334;
  EGL_SUPPORT_REUSE_NV = $3335;

function eglResetStreamNV(dpy: TEGLDisplay; stream: TEGLStreamKHR): TEGLBoolean; cdecl; external;

const
  EGL_NV_stream_socket = 1;
  EGL_STREAM_PROTOCOL_SOCKET_NV = $324B;
  EGL_SOCKET_HANDLE_NV = $324C;
  EGL_SOCKET_TYPE_NV = $324D;

const
  EGL_NV_stream_socket_inet = 1;
  EGL_SOCKET_TYPE_INET_NV = $324F;

const
  EGL_NV_stream_socket_unix = 1;
  EGL_SOCKET_TYPE_UNIX_NV = $324E;

const
  EGL_NV_stream_sync = 1;
  EGL_SYNC_NEW_FRAME_NV = $321F;

function eglCreateStreamSyncNV(dpy: TEGLDisplay; stream: TEGLStreamKHR; _type: TEGLenum; attrib_list: PEGLint): TEGLSyncKHR; cdecl; external;

const
  EGL_NV_sync = 1;

type
  PEGLSyncNV = ^TEGLSyncNV;
  TEGLSyncNV = pointer;

  PEGLTimeNV = ^TEGLTimeNV;
  TEGLTimeNV = Tkhronos_utime_nanoseconds_t;

const
  EGL_SYNC_PRIOR_COMMANDS_COMPLETE_NV = $30E6;
  EGL_SYNC_STATUS_NV = $30E7;
  EGL_SIGNALED_NV = $30E8;
  EGL_UNSIGNALED_NV = $30E9;
  EGL_SYNC_FLUSH_COMMANDS_BIT_NV = $0001;
  EGL_FOREVER_NV = $FFFFFFFFFFFFFFFF;
  EGL_ALREADY_SIGNALED_NV = $30EA;
  EGL_TIMEOUT_EXPIRED_NV = $30EB;
  EGL_CONDITION_SATISFIED_NV = $30EC;
  EGL_SYNC_TYPE_NV = $30ED;
  EGL_SYNC_CONDITION_NV = $30EE;
  EGL_SYNC_FENCE_NV = $30EF;

function eglCreateFenceSyncNV(dpy: TEGLDisplay; condition: TEGLenum; attrib_list: PEGLint): TEGLSyncNV; cdecl; external;
function eglDestroySyncNV(sync: TEGLSyncNV): TEGLBoolean; cdecl; external;
function eglFenceNV(sync: TEGLSyncNV): TEGLBoolean; cdecl; external;
function eglClientWaitSyncNV(sync: TEGLSyncNV; flags: TEGLint; timeout: TEGLTimeNV): TEGLint; cdecl; external;
function eglSignalSyncNV(sync: TEGLSyncNV; mode: TEGLenum): TEGLBoolean; cdecl; external;
function eglGetSyncAttribNV(sync: TEGLSyncNV; attribute: TEGLint; Value: PEGLint): TEGLBoolean; cdecl; external;

const
  EGL_NV_system_time = 1;

type
  PEGLuint64NV = ^TEGLuint64NV;
  TEGLuint64NV = Tkhronos_utime_nanoseconds_t;

function eglGetSystemTimeFrequencyNV: TEGLuint64NV; cdecl; external;
function eglGetSystemTimeNV: TEGLuint64NV; cdecl; external;

const
  EGL_NV_triple_buffer = 1;
  EGL_TRIPLE_BUFFER_NV = $3230;

const
  EGL_TIZEN_image_native_buffer = 1;
  EGL_NATIVE_BUFFER_TIZEN = $32A0;

const
  EGL_TIZEN_image_native_surface = 1;
  EGL_NATIVE_SURFACE_TIZEN = $32A1;

const
  EGL_WL_bind_wayland_display = 1;
  //  PFNEGLBINDWAYLANDDISPLAYWL = PFNEGLBINDWAYLANDDISPLAYWLPROC;
  //  PFNEGLUNBINDWAYLANDDISPLAYWL = PFNEGLUNBINDWAYLANDDISPLAYWLPROC;
  //  PFNEGLQUERYWAYLANDBUFFERWL = PFNEGLQUERYWAYLANDBUFFERWLPROC;
type
  Pwl_display = ^Twl_display;
  Twl_display = record
    {undefined structure}
  end;

  Pwl_resource = ^Twl_resource;
  Twl_resource = record
    {undefined structure}
  end;


const
  EGL_WAYLAND_BUFFER_WL = $31D5;
  EGL_WAYLAND_PLANE_WL = $31D6;
  EGL_TEXTURE_Y_U_V_WL = $31D7;
  EGL_TEXTURE_Y_UV_WL = $31D8;
  EGL_TEXTURE_Y_XUXV_WL = $31D9;
  EGL_TEXTURE_EXTERNAL_WL = $31DA;
  EGL_WAYLAND_Y_INVERTED_WL = $31DB;

function eglBindWaylandDisplayWL(dpy: TEGLDisplay; display: Pwl_display): TEGLBoolean; cdecl; external;
function eglUnbindWaylandDisplayWL(dpy: TEGLDisplay; display: Pwl_display): TEGLBoolean; cdecl; external;
function eglQueryWaylandBufferWL(dpy: TEGLDisplay; buffer: Pwl_resource; attribute: TEGLint; Value: PEGLint): TEGLBoolean; cdecl; external;

const
  EGL_WL_create_wayland_buffer_from_image = 1;
  //  PFNEGLCREATEWAYLANDBUFFERFROMIMAGEWL = PFNEGLCREATEWAYLANDBUFFERFROMIMAGEWLPROC;
type
  Pwl_buffer = ^Twl_buffer;
  Twl_buffer = record
    {undefined structure}
  end;

implementation

end.
