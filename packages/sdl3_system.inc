/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  TMSG = Pointer;
  TtagMSG = TMSG;
  PMSG = ^TMSG;

  TSDL_WindowsMessageHook = function(userdata: pointer; msg: PMSG): TSDL_bool; cdecl;

procedure SDL_SetWindowsMessageHook(callback: TSDL_WindowsMessageHook; userdata: pointer); cdecl; external;
function SDL_Direct3D9GetAdapterIndex(displayID: TSDL_DisplayID): longint; cdecl; external;
function SDL_DXGIGetOutputInfo(displayID: TSDL_DisplayID; adapterIndex: Plongint; outputIndex: Plongint): TSDL_bool; cdecl; external;

type
  TXEvent = Pointer;
  PXEvent = ^TXEvent;

  TSDL_X11EventHook = function(userdata: pointer; xevent: PXEvent): TSDL_bool; cdecl;

procedure SDL_SetX11EventHook(callback: TSDL_X11EventHook; userdata: pointer); cdecl; external;
function SDL_LinuxSetThreadPriority(threadID: int64; priority: longint): longint; cdecl; external;
function SDL_LinuxSetThreadPriorityAndPolicy(threadID: int64; sdlPriority: longint; schedPolicy: longint): longint; cdecl; external;

type
  callback_func = procedure(para1: pointer);

function SDL_iOSSetAnimationCallback(window: PSDL_Window; interval: longint; callback: callback_func; callbackParam: Pointer): longint;
function SDL_iPhoneSetAnimationCallback(window: PSDL_Window; interval: longint; callback: callback_func; callbackParam: pointer): longint; cdecl; external;
function SDL_iOSSetEventPump(Enabled: TSDL_bool): longint;

procedure SDL_iPhoneSetEventPump(Enabled: TSDL_bool); cdecl; external;
function SDL_AndroidGetJNIEnv: pointer; cdecl; external;
function SDL_AndroidGetActivity: pointer; cdecl; external;
function SDL_GetAndroidSDKVersion: longint; cdecl; external;
function SDL_IsAndroidTV: TSDL_bool; cdecl; external;
function SDL_IsChromebook: TSDL_bool; cdecl; external;
function SDL_IsDeXMode: TSDL_bool; cdecl; external;
procedure SDL_AndroidBackButton; cdecl; external;

const
  SDL_ANDROID_EXTERNAL_STORAGE_READ = $01;
  SDL_ANDROID_EXTERNAL_STORAGE_WRITE = $02;

function SDL_AndroidGetInternalStoragePath: PChar; cdecl; external;
function SDL_AndroidGetExternalStorageState(state: PUint32): longint; cdecl; external;
function SDL_AndroidGetExternalStoragePath: PChar; cdecl; external;

type

  TSDL_AndroidRequestPermissionCallback = procedure(userdata: pointer; permission: PChar; granted: TSDL_bool); cdecl;

function SDL_AndroidRequestPermission(permission: PChar; cb: TSDL_AndroidRequestPermissionCallback; userdata: pointer): longint; cdecl; external;
function SDL_AndroidShowToast(message: PChar; duration: longint; gravity: longint; xoffset: longint; yoffset: longint): longint; cdecl; external;
function SDL_AndroidSendMessage(command: uint32; param: longint): longint; cdecl; external;

type
  PSDL_WinRT_Path = ^TSDL_WinRT_Path;
  TSDL_WinRT_Path = longint;

const
  SDL_WINRT_PATH_INSTALLED_LOCATION = 0;
  SDL_WINRT_PATH_LOCAL_FOLDER = 1;
  SDL_WINRT_PATH_ROAMING_FOLDER = 2;
  SDL_WINRT_PATH_TEMP_FOLDER = 3;

type
  PSDL_WinRT_DeviceFamily = ^TSDL_WinRT_DeviceFamily;
  TSDL_WinRT_DeviceFamily = longint;

const
  SDL_WINRT_DEVICEFAMILY_UNKNOWN = 0;
  SDL_WINRT_DEVICEFAMILY_DESKTOP = 1;
  SDL_WINRT_DEVICEFAMILY_MOBILE = 2;
  SDL_WINRT_DEVICEFAMILY_XBOX = 3;

function SDL_WinRTGetFSPathUNICODE(pathType: TSDL_WinRT_Path): Pwchar_t; cdecl; external;
function SDL_WinRTGetFSPathUTF8(pathType: TSDL_WinRT_Path): PChar; cdecl; external;
function SDL_WinRTGetDeviceFamily: TSDL_WinRT_DeviceFamily; cdecl; external;
function SDL_IsTablet: TSDL_bool; cdecl; external;
procedure SDL_OnApplicationWillTerminate; cdecl; external;
procedure SDL_OnApplicationDidReceiveMemoryWarning; cdecl; external;
procedure SDL_OnApplicationWillResignActive; cdecl; external;
procedure SDL_OnApplicationDidEnterBackground; cdecl; external;
procedure SDL_OnApplicationWillEnterForeground; cdecl; external;
procedure SDL_OnApplicationDidBecomeActive; cdecl; external;
procedure SDL_OnApplicationDidChangeStatusBarOrientation; cdecl; external;

type
  PXTaskQueueObject = Pointer;
  PXTaskQueueHandle = ^TXTaskQueueHandle;
  TXTaskQueueHandle = PXTaskQueueObject;

  PXUser = Pointer;
  PXUserHandle = ^TXUserHandle;
  TXUserHandle = PXUser;

function SDL_GDKGetTaskQueue(outTaskQueue: PXTaskQueueHandle): longint; cdecl; external;
function SDL_GDKGetDefaultUser(outUserHandle: PXUserHandle): longint; cdecl; external;

{$ENDIF read_interface}


{$IFDEF read_implementation}

function SDL_iOSSetAnimationCallback(window: PSDL_Window; interval: longint; callback: callback_func; callbackParam: Pointer): longint;
begin
//  SDL_iOSSetAnimationCallback := SDL_iPhoneSetAnimationCallback(window, interval, callback, callbackParam);
end;

function SDL_iOSSetEventPump(Enabled: TSDL_bool): longint;
begin
  //  SDL_iOSSetEventPump:=SDL_iPhoneSetEventPump(enabled);
end;

{$ENDIF read_implementation}
