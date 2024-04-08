program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  SDL3,
  ctypes;

var
  nDevices, pCount: longint;
  pDeviceList: PSDL_CameraDeviceID;
  i, j: integer;
  pName: PChar;
  pSpec: PSDL_CameraSpec;
begin
  SDL_Init(SDL_INIT_CAMERA);

  SDL_Log('Camera count: %i', SDL_GetNumCameraDrivers);

  pDeviceList := SDL_GetCameraDevices(@nDevices);
  SDL_Log('Camera count: %i', nDevices);
  for i := 0 to nDevices - 1 do begin
    pName := SDL_GetCameraDeviceName(pDeviceList[i]);
    WriteLn('  ',pName);

   pSpec:= SDL_GetCameraDeviceSupportedFormats(pDeviceList[i],@pCount);
   for j:=0 to pCount-1 do
       WriteLn('    spec w: ',pSpec[j].width, ' h: ',pSpec[j].height,  ' f: ',pSpec[j].format);
  end;

end.
