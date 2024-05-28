program Project1;

uses
  SysUtils,
  SDL3,
  oglglad_GLES32,
  oglVector,
  oglMatrix,
  oglShader;

const
  Screen_Widht = 320;
  Screen_Height = 240;

var
  // SDL
  glcontext: TSDL_GLContext;
  gWindow: PSDL_Window;

  quit: boolean = False;
  e: TSDL_Event;

  // OpenGL
  MyShader: TShader;
  RotMatrix, ModelMatrix: Tmat4x4;
  //  textur0, textur1: GLuint;

  Texture: array[(TexID1)] of GLuint;
  VAOs: array [(vaQuad)] of TGLuint;
  Mesh_Buffers: array [(mbVector, mbTexturCord)] of TGLuint;

const
  QuadVertex: array of TVector3f =
    ((-0.2, -0.2, 0.0), (0.2, 0.2, 0.0), (-0.2, 0.2, 0.0),
    (-0.2, -0.2, 0.0), (0.2, -0.2, 0.0), (0.2, 0.2, 0.0));

  TextureVertex: array of TVector2f =
    ((0.0, 0.0), (1.0, 1.0), (0.0, 1.0),
    (0.0, 0.0), (1.0, 0.0), (1.0, 1.0));

  vertex_shader_text: string =
    '#version 330 core' + #10 +
    '' + #10 +
    'layout (location = 0) in vec3 vPosition;' + #10 +
    'layout (location = 1) in vec2 inUV;' + #10 +    // Textur-Koordinaten
    '' + #10 +
    'uniform mat4x4 matrix;' + #10 +
    '' + #10 +
    'out vec2 UV0;' + #10 +
    '' + #10 +
    'void main()' + #10 +
    '{' + #10 +
    '  gl_Position = matrix * vec4(vPosition, 1);' + #10 +
    'UV0 = inUV;' + #10 +
    '}';

  fragment_shader_text =
    '#version 330 core' + #10 +
    '' + #10 +
    'in vec2 UV0;' + #10 +
    'uniform sampler2D Sampler;' + #10 +
    '' + #10 +
    'out vec4 fColor;' + #10 +
    '' + #10 +
    'void main()' + #10 +
    '{' + #10 +
    '  fColor = texture(Sampler, UV0);' + #10 +
    '}';

  procedure printPixels(sur: PSDL_Surface);
  begin
    WriteLn('Pixel: ', pbyte(sur^.pixels)[0], ' ', pbyte(sur^.pixels)[1], ' ', pbyte(sur^.pixels)[2], ' ', pbyte(sur^.pixels)[3]);
    WriteLn();
  end;

  procedure printFormat(sur: PSDL_Surface);
  begin
    //  WriteLn('format: ', IntToHex(sur^.format^.format, 8));
    WriteLn('format: ', sur^.format^.format);
    WriteLn('bit per pixel: ', sur^.format^.bits_per_pixel, '   bytes per pixel: ', sur^.format^.bytes_per_pixel);
    WriteLn('Rmask: ', IntToHex(sur^.format^.Rmask, 8), '  Gmask: ', IntToHex(sur^.format^.Gmask, 8), '  Bmask: ', IntToHex(sur^.format^.Bmask, 8), '  Amask: ', IntToHex(sur^.format^.Amask, 8));
    WriteLn('Rshift: ', sur^.format^.Rshift: 7, '  Gshift: ', sur^.format^.Gshift: 7, '  Bshift: ', sur^.format^.Bshift: 7, '  Ashift: ', sur^.format^.Ashift: 7);
    WriteLn('Rloss: ', sur^.format^.Rloss: 8, '  Gloss: ', sur^.format^.Gloss: 8, '  Bloss: ', sur^.format^.Bloss: 8, '  Aloss: ', sur^.format^.Aloss: 8);
    WriteLn();
    printPixels(sur);
  end;


  function CreateSurface: PSDL_Surface;
  const
    pixels: array of array[0..3] of byte = (
      ($FF, $00, $00, $00),
      ($00, $FF, $00, $00),
      ($00, $00, $FF, $00),
      ($FF, $FF, $00, $00));
  begin
    //Result := SDL_LoadBMP('mauer.bmp');

    Result := SDL_CreateSurface(2, 2, SDL_PIXELFORMAT_RGBA32);
    Result := SDL_CreateSurfaceFrom(Pointer(pixels), 2, 2, 8, SDL_PIXELFORMAT_RGBA32);
    if Result = nil then begin
      SDL_Log('Konnte BMP nicht laden!:  %s', SDL_GetError);
    end;
  end;

  procedure CreateSurfaceBMPTextur(var TexturID: GLuint);
  var
    surfaceBMP: PSDL_Surface = nil;
    surfaceTexture: PSDL_Surface = nil;
  begin
    //      surfaceBMP := SDL_LoadBMP('mauer.bmp');
    surfaceBMP := CreateSurface;
    printFormat(surfaceBMP);

    surfaceTexture := SDL_ConvertSurfaceFormat(surfaceBMP, SDL_PIXELFORMAT_RGB24);
    printFormat(surfaceTexture);
    surfaceTexture := SDL_ConvertSurfaceFormat(surfaceBMP, SDL_PIXELFORMAT_RGBA32);
    //    surfaceTexture := SDL_ConvertSurfaceFormat(surfaceBMP, SDL_PIXELFORMAT_ABGR32);
    printFormat(surfaceTexture);

    // Textur
    glBindTexture(GL_TEXTURE_2D, TexturID);
    // SDL_DestroySurface(surfaceBMP);
    //    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 2, 2, 0, GL_RGB, GL_UNSIGNED_BYTE, Pointer(Texure24));
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, surfaceTexture^.w, surfaceTexture^.h, 0, GL_RGBA, GL_UNSIGNED_BYTE, surfaceTexture^.pixels);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glBindTexture(GL_TEXTURE_2D, 0);

    SDL_DestroySurface(surfaceTexture);
  end;

  procedure Init_SDL_and_OpenGL;
  begin
    // --- SDL inizialisieren
    if SDL_Init(SDL_INIT_VIDEO) < 0 then begin
      WriteLn('SDL could not initialize! SDL_Error: ', SDL_GetError);
      Halt(1);
    end;

    // --- Context für OpenGL erzeugen
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);

    gwindow := SDL_CreateWindow('SDL3 Window', Screen_Widht, Screen_Height, SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE);
    glcontext := SDL_GL_CreateContext(gWindow);
    if glcontext = nil then begin
      Writeln('OpenGL context could not be created! SDL Error: ', SDL_GetError);
      Halt(1);
    end;

    if SDL_GL_SetSwapInterval(1) < 0 then begin
      WriteLn('Warning: Unable to set VSync! SDL Error: ', SDL_GetError);
    end;

    RotMatrix.Identity;
    Load_GLADE;
  end;

  procedure CreateScene;
  begin
    glGenVertexArrays(Length(VAOs), VAOs);
    glGenBuffers(Length(Mesh_Buffers), Mesh_Buffers);

    glBindVertexArray(VAOs[vaQuad]);

    // Vector
    glBindBuffer(GL_ARRAY_BUFFER, Mesh_Buffers[mbVector]);
    glBufferData(GL_ARRAY_BUFFER, Length(QuadVertex) * sizeof(TVector3f), PVector3f(QuadVertex), GL_STATIC_DRAW);

    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nil);
    glEnableVertexAttribArray(0);

    // TexturCoord
    glBindBuffer(GL_ARRAY_BUFFER, Mesh_Buffers[mbTexturCord]);
    glBufferData(GL_ARRAY_BUFFER, Length(TextureVertex) * sizeof(TVector2f), PVector2f(TextureVertex), GL_STATIC_DRAW);

    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 0, nil);
    glEnableVertexAttribArray(1);

    glBindVertexArray(0);

    // Textur
    glGenTextures(Length(Texture), Texture);

    CreateSurfaceBMPTextur(Texture[TexID1]);

    // Shader
    MyShader := TShader.Create;
    MyShader.LoadShaderObject(GL_VERTEX_SHADER, vertex_shader_text);
    MyShader.LoadShaderObject(GL_FRAGMENT_SHADER, fragment_shader_text);
    MyShader.LinkProgram;
    MyShader.UseProgram;
  end;


  procedure DrawScene;
  const
    black: TVector4f = (0.3, 0.0, 0.2, 1.0);
  var
    mat_id: GLint;
  begin
    glClearBufferfv(GL_COLOR, 0, black);
    mat_id := MyShader.UniformLocation('matrix');
    RotMatrix.RotateC(0.01);

    // === 1
    ModelMatrix.Identity;
    ModelMatrix.Scale(3);
    ModelMatrix *= RotMatrix;
    ModelMatrix.Uniform(mat_id);
    glBindTexture(GL_TEXTURE_2D, Texture[TexID1]);
    glBindVertexArray(VAOs[vaQuad]);
    glDrawArrays(GL_TRIANGLES, 0, Length(QuadVertex));


    SDL_GL_SwapWindow(gWindow);
  end;

  procedure Destroy_SDL_and_OpenGL;
  begin
    glDeleteTextures(Length(Texture), Texture);
    glDeleteVertexArrays(Length(VAOs), VAOs);
    glDeleteBuffers(Length(Mesh_Buffers), Mesh_Buffers);

    MyShader.Free;

    SDL_GL_DeleteContext(glcontext);
    SDL_DestroyWindow(gWindow);
    SDL_Quit();
  end;

  procedure RunScene;
  var
    w, h: int32;
  begin
    while not quit do begin
      while SDL_PollEvent(@e) do begin
        case e.type_ of
          SDL_EVENT_KEY_DOWN: begin
            case e.key.keysym.sym of
              SDLK_ESCAPE: begin
                quit := True;
              end;
            end;
          end;
          SDL_EVENT_WINDOW_RESIZED: begin
            w := e.window.data1;
            h := e.window.data2;
            glViewport(0, 0, w, h);
          end;
          SDL_EVENT_QUIT: begin
            quit := True;
          end;
        end;
      end;
      DrawScene;
    end;
  end;

begin
  Init_SDL_and_OpenGL;
  CreateScene;
  RunScene;
  Destroy_SDL_and_OpenGL;
end.
