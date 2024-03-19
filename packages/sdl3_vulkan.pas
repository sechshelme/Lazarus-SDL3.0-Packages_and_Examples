unit SDL3_vulkan;

interface

uses
  SDL3_stdinc, SDL3_video;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  PVkAllocationCallbacks = ^TVkAllocationCallbacks;
  TVkAllocationCallbacks = record
    {undefined structure}
  end;

type
  TVkInstance = Pointer;

  PSDL_vulkanInstance = ^TSDL_vulkanInstance;
  TSDL_vulkanInstance = TVkInstance;

  PVkSurfaceKHR = ^TVkSurfaceKHR;

  TVkSurfaceKHR = record
    object_: uint64;
  end;

  PSDL_vulkanSurface = ^TSDL_vulkanSurface;
  TSDL_vulkanSurface = TVkSurfaceKHR;

function SDL_Vulkan_LoadLibrary(path: PChar): longint; cdecl; external;
function SDL_Vulkan_GetVkGetInstanceProcAddr: TSDL_FunctionPointer; cdecl; external;
procedure SDL_Vulkan_UnloadLibrary; cdecl; external;
function SDL_Vulkan_GetInstanceExtensions(pCount: PUint32): PPchar; cdecl; external;
function SDL_Vulkan_CreateSurface(window: PSDL_Window; instance: TVkInstance; allocator: PVkAllocationCallbacks; surface: PVkSurfaceKHR): TSDL_bool; cdecl; external;

implementation

end.
