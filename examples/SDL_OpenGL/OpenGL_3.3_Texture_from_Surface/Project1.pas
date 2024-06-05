program Project1;

uses
  SDL3,
  oglglad_gl,
  oglVector,
  oglMatrix,
  oglShader;

  function RGBA(R, G, B, A: byte): uint32; inline;
  begin
    Result := R or (G shl 8) or (B shl 16) or (A shl 24);
  end;

  function RGBA16(R, G, B, A: byte): uint16; inline;
  begin
    Result := R or (G shl 4) or (B shl 8) or (A shl 12);
  end;

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

  Texture: array[(TexID1, TexID2, TexID3, TexID4)] of GLuint;
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
    '  fColor = texture( Sampler, UV0);' + #10 +
    '}';

  procedure CreateSurfaceBMPTextur(var TexturID: GLuint);
  var
    surface: PSDL_Surface;
    mode: GLint;
  begin
    surface := SDL_LoadBMP('mauer.bmp');

    case surface^.format^.bytes_per_pixel of
      3: begin
        WriteLn(surface^.format^.Rmask);
        if surface^.format^.Rmask = $FF then  begin
          mode := GL_RGB;
        end else begin
          mode := GL_BGR;
        end;
      end;
      4: begin
        WriteLn(surface^.format^.Rmask);
        if surface^.format^.Rmask = $FF then  begin
          mode := GL_RGBA;
        end else begin
          mode := GL_BGRA;
        end;
      end;
    end;

    // Textur
    glBindTexture(GL_TEXTURE_2D, TexturID);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, surface^.w, surface^.h, 0, mode, GL_UNSIGNED_BYTE, surface^.pixels);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glBindTexture(GL_TEXTURE_2D, 0);

    SDL_DestroySurface(surface);
  end;

  procedure CreateTexture(var TexturID: GLuint);
  const
    Textur32_0: packed array[0..1, 0..1, 0..3] of byte = ((($FF, $00, $00, $FF), ($00, $FF, $00, $FF)), (($00, $00, $FF, $FF), ($FF, $00, $00, $FF)));
  begin
    glBindTexture(GL_TEXTURE_2D, TexturID);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 2, 2, 0, GL_RGBA, GL_UNSIGNED_BYTE, @Textur32_0);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glBindTexture(GL_TEXTURE_2D, 0);
  end;

  procedure CreateTexturSurface32(var TexturID: GLuint);
  const
    size = 32;
  var
    surface: PSDL_Surface;
    i: integer;
    r: TSDL_Rect;
  begin
    surface := SDL_CreateSurface(size, size, SDL_PIXELFORMAT_RGBA8888);

    for i := 0 to size div 4 do begin
      r.items := [i * 2, i * 2, size - i * 4, size - i * 4];
      SDL_FillSurfaceRect(surface, @r, RGBA(i * 25, i * 50, i * 75, $FF));
    end;

    // Textur
    glBindTexture(GL_TEXTURE_2D, TexturID);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, surface^.w, surface^.h, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV, surface^.pixels);
    //  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, surface^.w, surface^.h, 0, GL_RGBA, GL_UNSIGNED_BYTE, surface^.pixels);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glBindTexture(GL_TEXTURE_2D, 0);

    SDL_DestroySurface(surface);
  end;

  procedure CreateTexturSurface16(var TexturID: GLuint);
  const
    size = 32;
  var
    surface: PSDL_Surface;
    i: integer;
    r: TSDL_Rect;
  begin
    surface := SDL_CreateSurface(size, size, SDL_PIXELFORMAT_RGBA4444);

    for i := 0 to size div 4 do begin
      r.items := [i * 2, i * 2, size - i * 4, size - i * 4];
      SDL_FillSurfaceRect(surface, @r, RGBA16((i * 25) div 16, (i * 50) div 16, (i * 75) div 16, $FF));
    end;

    // Textur
    glBindTexture(GL_TEXTURE_2D, TexturID);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, surface^.w, surface^.h, 0, GL_RGBA, GL_UNSIGNED_SHORT_4_4_4_4_REV, surface^.pixels);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glBindTexture(GL_TEXTURE_2D, 0);

    SDL_DestroySurface(surface);
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
    CreateTexture(Texture[TexID2]);
    CreateTexturSurface32(Texture[TexID3]);
    CreateTexturSurface16(Texture[TexID4]);

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
    ModelMatrix.Translate([-0.5, 0.5, 0.0]);
    ModelMatrix *= RotMatrix;
    ModelMatrix.Uniform(mat_id);
    glBindTexture(GL_TEXTURE_2D, Texture[TexID1]);
    glBindVertexArray(VAOs[vaQuad]);
    glDrawArrays(GL_TRIANGLES, 0, Length(QuadVertex));

    // === 2
    ModelMatrix.Identity;
    ModelMatrix.Translate([-0.5, -0.5, 0.0]);
    ModelMatrix *= RotMatrix;
    ModelMatrix.Uniform(mat_id);
    glBindTexture(GL_TEXTURE_2D, Texture[TexID2]);
    glBindVertexArray(VAOs[vaQuad]);
    glDrawArrays(GL_TRIANGLES, 0, Length(QuadVertex));

    // === 3
    ModelMatrix.Identity;
    ModelMatrix.Translate([0.5, 0.5, 0.0]);
    ModelMatrix *= RotMatrix;
    ModelMatrix.Uniform(mat_id);
    glBindTexture(GL_TEXTURE_2D, Texture[TexID3]);
    glBindVertexArray(VAOs[vaQuad]);
    glDrawArrays(GL_TRIANGLES, 0, Length(QuadVertex));

    // === 4
    ModelMatrix.Identity;
    ModelMatrix.Translate([0.5, -0.5, 0.0]);
    ModelMatrix *= RotMatrix;
    ModelMatrix.Uniform(mat_id);
    glBindTexture(GL_TEXTURE_2D, Texture[TexID4]);
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
