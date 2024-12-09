
{$IFDEF read_interface}

{$IFDEF Windows}
type
  TtagMSG = TMSG;
  TSDL_WindowsMessageHook = function(userdata: pointer; msg: PMSG): Tbool; cdecl;

procedure SDL_SetWindowsMessageHook(callback: TSDL_WindowsMessageHook; userdata: pointer); cdecl; external libSDL3;
function SDL_GetDirect3D9AdapterIndex(displayID: TSDL_DisplayID): longint; cdecl; external libSDL3;
function SDL_GetDXGIOutputInfo(displayID: TSDL_DisplayID; adapterIndex: Plongint; outputIndex: Plongint): Tbool; cdecl; external libSDL3;
{$ENDIF}
{$IFDEF Linux}
type
  TSDL_X11EventHook = function(userdata: pointer; xevent: PXEvent): Tbool; cdecl;

procedure SDL_SetX11EventHook(callback: TSDL_X11EventHook; userdata: pointer); cdecl; external libSDL3;
function SDL_SetLinuxThreadPriority(threadID: TSint64; priority: longint): Tbool; cdecl; external libSDL3;
function SDL_SetLinuxThreadPriorityAndPolicy(threadID: TSint64; sdlPriority: longint; schedPolicy: longint): Tbool; cdecl; external libSDL3;
{$ENDIF}
{$ifdef darwin}
type
  TSDL_iOSAnimationCallback = procedure(userdata: pointer); cdecl;

function SDL_SetiOSAnimationCallback(window: PSDL_Window; interval: longint; callback: TSDL_iOSAnimationCallback; callbackParam: pointer): Tbool; cdecl; external libSDL3;
procedure SDL_SetiOSEventPump(Enabled: Tbool); cdecl; external libSDL3;
{$endif}
{$ifdef ANDROID}
function SDL_GetAndroidJNIEnv: pointer; cdecl; external libSDL3;
function SDL_GetAndroidActivity: pointer; cdecl; external libSDL3;
function SDL_GetAndroidSDKVersion: longint; cdecl; external libSDL3;
function SDL_IsChromebook: Tbool; cdecl; external libSDL3;
function SDL_IsDeXMode: Tbool; cdecl; external libSDL3;
procedure SDL_SendAndroidBackButton; cdecl; external libSDL3;

const
  SDL_ANDROID_EXTERNAL_STORAGE_READ = $01;
  SDL_ANDROID_EXTERNAL_STORAGE_WRITE = $02;

function SDL_GetAndroidInternalStoragePath: pansichar; cdecl; external libSDL3;
function SDL_GetAndroidExternalStorageState: TUint32; cdecl; external libSDL3;
function SDL_GetAndroidExternalStoragePath: pansichar; cdecl; external libSDL3;
function SDL_GetAndroidCachePath: pansichar; cdecl; external libSDL3;

type
  TSDL_RequestAndroidPermissionCallback = procedure(userdata: pointer; permission: pansichar; granted: Tbool); cdecl;

function SDL_RequestAndroidPermission(permission: pansichar; cb: TSDL_RequestAndroidPermissionCallback; userdata: pointer): Tbool; cdecl; external libSDL3;
function SDL_ShowAndroidToast(message: pansichar; duration: longint; gravity: longint; xoffset: longint; yoffset: longint): Tbool; cdecl; external libSDL3;
function SDL_SendAndroidMessage(command: TUint32; param: longint): Tbool; cdecl; external libSDL3;
{$endif}

function SDL_IsTablet: Tbool; cdecl; external libSDL3;
function SDL_IsTV: Tbool; cdecl; external libSDL3;

type
  PSDL_Sandbox = ^TSDL_Sandbox;
  TSDL_Sandbox = longint;

const
  SDL_SANDBOX_NONE = 0;
  SDL_SANDBOX_UNKNOWN_CONTAINER = 1;
  SDL_SANDBOX_FLATPAK = 2;
  SDL_SANDBOX_SNAP = 3;
  SDL_SANDBOX_MACOS = 4;

function SDL_GetSandbox:TSDL_Sandbox;cdecl;external libSDL3;
procedure SDL_OnApplicationWillTerminate; cdecl; external libSDL3;
procedure SDL_OnApplicationDidReceiveMemoryWarning; cdecl; external libSDL3;
procedure SDL_OnApplicationWillEnterBackground; cdecl; external libSDL3;
procedure SDL_OnApplicationDidEnterBackground; cdecl; external libSDL3;
procedure SDL_OnApplicationWillEnterForeground; cdecl; external libSDL3;
procedure SDL_OnApplicationDidEnterForeground; cdecl; external libSDL3;
{$ifdef darwin}
procedure SDL_OnApplicationDidChangeStatusBarOrientation; cdecl; external libSDL3;
{$endif}

{$ifdef SDL_PLATFORM_GDK}
type
  PXTaskQueueHandle = ^TXTaskQueueHandle;
  TXTaskQueueHandle = PXTaskQueueObject;

  PXUserHandle = ^TXUserHandle;
  TXUserHandle = PXUser;

function SDL_GetGDKTaskQueue(outTaskQueue: PXTaskQueueHandle): Tbool; cdecl; external libSDL3;
function SDL_GetGDKDefaultUser(outUserHandle: PXUserHandle): Tbool; cdecl; external libSDL3;
{$endif}

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}
