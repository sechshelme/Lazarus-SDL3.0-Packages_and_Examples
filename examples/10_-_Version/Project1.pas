program Project1;

uses
  SDL3,
  SDL_image,
  SDL_ttf,
  SDL_rtf;

var
  ver: TSDL_Version;

begin
  SDL_VERSION(@ver);
  SDL_Log('SDL3 Include Version: %i.%i.%i', ver.major, ver.minor, ver.patch);
  SDL_GetVersion(@ver);
  SDL_Log('SDL3 lib Version: %i.%i.%i', ver.major, ver.minor, ver.patch);

  SDL_IMAGE_VERSION(@ver);
  SDL_Log('SDL_Image Include Version: %i.%i.%i', ver.major, ver.minor, ver.patch);
  ver := IMG_Linked_Version^;
  SDL_Log('SDL_Image lib     Version: %i.%i.%i', ver.major, ver.minor, ver.patch);

  SDL_TTF_VERSION(@ver);
  SDL_Log('SDL_TTF Include Version: %i.%i.%i', ver.major, ver.minor, ver.patch);
  ver := TTF_Linked_Version^;
  SDL_Log('SDL_TTF lib     Version: %i.%i.%i', ver.major, ver.minor, ver.patch);

  SDL_RTF_VERSION(@ver);
  SDL_Log('SDL_RTF Include Version: %i.%i.%i', ver.major, ver.minor, ver.patch);
  ver := RTF_Linked_Version^;
  SDL_Log('SDL_RTF lib     Version: %i.%i.%i', ver.major, ver.minor, ver.patch);
end.
