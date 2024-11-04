program Project1;

uses
  SDL3;

  // https://wiki.libsdl.org/SDL3/SDL_GL_ExtensionSupported


  procedure ShowGLExt(const ext: string);
  begin
    SDL_Log(PChar(ext + ' %d: '), SDL_GL_ExtensionSupported(PChar(ext)));
  end;


begin
  SDL_Init(SDL_INIT_VIDEO);

  ShowGLExt('GLX_ARB_create_context');
  ShowGLExt('GL_3DFX_texture_compression_FXT1');

  SDL_Quit;
end.
