/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_RendererFlags = ^TSDL_RendererFlags;
  TSDL_RendererFlags = longint;

const
  SDL_RENDERER_SOFTWARE = $00000001;
  SDL_RENDERER_ACCELERATED = $00000002;
  SDL_RENDERER_PRESENTVSYNC = $00000004;

type
  PSDL_RendererInfo = ^TSDL_RendererInfo;

  TSDL_RendererInfo = record
    Name: PChar;
    flags: uint32;
    num_texture_formats: uint32;
    texture_formats: array[0..15] of uint32;
    max_texture_width: longint;
    max_texture_height: longint;
  end;

  PSDL_Vertex = ^TSDL_Vertex;

  TSDL_Vertex = record
    position: TSDL_FPoint;
    color: TSDL_FColor;
    tex_coord: TSDL_FPoint;
  end;

  PSDL_TextureAccess = ^TSDL_TextureAccess;
  TSDL_TextureAccess = longint;

const
  SDL_TEXTUREACCESS_STATIC = 0;
  SDL_TEXTUREACCESS_STREAMING = 1;
  SDL_TEXTUREACCESS_TARGET = 2;

type
  PSDL_RendererLogicalPresentation = ^TSDL_RendererLogicalPresentation;
  TSDL_RendererLogicalPresentation = longint;

const
  SDL_LOGICAL_PRESENTATION_DISABLED = 0;
  SDL_LOGICAL_PRESENTATION_STRETCH = 1;
  SDL_LOGICAL_PRESENTATION_LETTERBOX = 2;
  SDL_LOGICAL_PRESENTATION_OVERSCAN = 3;
  SDL_LOGICAL_PRESENTATION_INTEGER_SCALE = 4;

type
  PPSDL_Renderer = ^PSDL_Renderer;
  PSDL_Renderer = ^TSDL_Renderer;
  TSDL_Renderer = Pointer;      {undefined structure}
  PSDL_Texture = ^TSDL_Texture;
  TSDL_Texture = Pointer;      {undefined structure}

function SDL_GetNumRenderDrivers: longint; cdecl; external;
function SDL_GetRenderDriver(index: longint): PChar; cdecl; external;
function SDL_CreateWindowAndRenderer(Width: longint; Height: longint; window_flags: uint32; window: PPSDL_Window; renderer: PPSDL_Renderer): longint; cdecl; external;
function SDL_CreateRenderer(window: PSDL_Window; Name: PChar; flags: uint32): PSDL_Renderer; cdecl; external;
function SDL_CreateRendererWithProperties(props: TSDL_PropertiesID): PSDL_Renderer; cdecl; external;

const
  SDL_PROP_RENDERER_CREATE_NAME_STRING = 'name';
  SDL_PROP_RENDERER_CREATE_WINDOW_POINTER = 'window';
  SDL_PROP_RENDERER_CREATE_SURFACE_POINTER = 'surface';
  SDL_PROP_RENDERER_CREATE_OUTPUT_COLORSPACE_NUMBER = 'output_colorspace';
  SDL_PROP_RENDERER_CREATE_PRESENT_VSYNC_BOOLEAN = 'present_vsync';
  SDL_PROP_RENDERER_CREATE_VULKAN_INSTANCE_POINTER = 'vulkan.instance';
  SDL_PROP_RENDERER_CREATE_VULKAN_SURFACE_NUMBER = 'vulkan.surface';
  SDL_PROP_RENDERER_CREATE_VULKAN_PHYSICAL_DEVICE_POINTER = 'vulkan.physical_device';
  SDL_PROP_RENDERER_CREATE_VULKAN_DEVICE_POINTER = 'vulkan.device';
  SDL_PROP_RENDERER_CREATE_VULKAN_GRAPHICS_QUEUE_FAMILY_INDEX_NUMBER = 'vulkan.graphics_queue_family_index';
  SDL_PROP_RENDERER_CREATE_VULKAN_PRESENT_QUEUE_FAMILY_INDEX_NUMBER = 'vulkan.present_queue_family_index';

function SDL_CreateSoftwareRenderer(surface: PSDL_Surface): PSDL_Renderer; cdecl; external;
function SDL_GetRenderer(window: PSDL_Window): PSDL_Renderer; cdecl; external;
function SDL_GetRenderWindow(renderer: PSDL_Renderer): PSDL_Window; cdecl; external;
function SDL_GetRendererInfo(renderer: PSDL_Renderer; info: PSDL_RendererInfo): longint; cdecl; external;
function SDL_GetRendererProperties(renderer: PSDL_Renderer): TSDL_PropertiesID; cdecl; external;

const
  SDL_PROP_RENDERER_NAME_STRING = 'SDL.renderer.name';
  SDL_PROP_RENDERER_WINDOW_POINTER = 'SDL.renderer.window';
  SDL_PROP_RENDERER_SURFACE_POINTER = 'SDL.renderer.surface';
  SDL_PROP_RENDERER_OUTPUT_COLORSPACE_NUMBER = 'SDL.renderer.output_colorspace';
  SDL_PROP_RENDERER_HDR_ENABLED_BOOLEAN = 'SDL.renderer.HDR_enabled';
  SDL_PROP_RENDERER_SDR_WHITE_POINT_FLOAT = 'SDL.renderer.SDR_white_point';
  SDL_PROP_RENDERER_HDR_HEADROOM_FLOAT = 'SDL.renderer.HDR_headroom';
  SDL_PROP_RENDERER_D3D9_DEVICE_POINTER = 'SDL.renderer.d3d9.device';
  SDL_PROP_RENDERER_D3D11_DEVICE_POINTER = 'SDL.renderer.d3d11.device';
  SDL_PROP_RENDERER_D3D12_DEVICE_POINTER = 'SDL.renderer.d3d12.device';
  SDL_PROP_RENDERER_D3D12_COMMAND_QUEUE_POINTER = 'SDL.renderer.d3d12.command_queue';
  SDL_PROP_RENDERER_VULKAN_INSTANCE_POINTER = 'SDL.renderer.vulkan.instance';
  SDL_PROP_RENDERER_VULKAN_SURFACE_NUMBER = 'SDL.renderer.vulkan.surface';
  SDL_PROP_RENDERER_VULKAN_PHYSICAL_DEVICE_POINTER = 'SDL.renderer.vulkan.physical_device';
  SDL_PROP_RENDERER_VULKAN_DEVICE_POINTER = 'SDL.renderer.vulkan.device';
  SDL_PROP_RENDERER_VULKAN_GRAPHICS_QUEUE_FAMILY_INDEX_NUMBER = 'SDL.renderer.vulkan.graphics_queue_family_index';
  SDL_PROP_RENDERER_VULKAN_PRESENT_QUEUE_FAMILY_INDEX_NUMBER = 'SDL.renderer.vulkan.present_queue_family_index';

function SDL_GetRenderOutputSize(renderer: PSDL_Renderer; w: Plongint; h: Plongint): longint; cdecl; external;
function SDL_GetCurrentRenderOutputSize(renderer: PSDL_Renderer; w: Plongint; h: Plongint): longint; cdecl; external;
function SDL_CreateTexture(renderer: PSDL_Renderer; format: uint32; access: longint; w: longint; h: longint): PSDL_Texture; cdecl; external;
function SDL_CreateTextureFromSurface(renderer: PSDL_Renderer; surface: PSDL_Surface): PSDL_Texture; cdecl; external;
function SDL_CreateTextureWithProperties(renderer: PSDL_Renderer; props: TSDL_PropertiesID): PSDL_Texture; cdecl; external;

const
  SDL_PROP_TEXTURE_CREATE_COLORSPACE_NUMBER = 'colorspace';
  SDL_PROP_TEXTURE_CREATE_FORMAT_NUMBER = 'format';
  SDL_PROP_TEXTURE_CREATE_ACCESS_NUMBER = 'access';
  SDL_PROP_TEXTURE_CREATE_WIDTH_NUMBER = 'width';
  SDL_PROP_TEXTURE_CREATE_HEIGHT_NUMBER = 'height';
  SDL_PROP_TEXTURE_CREATE_SDR_WHITE_POINT_FLOAT = 'SDR_white_point';
  SDL_PROP_TEXTURE_CREATE_HDR_HEADROOM_FLOAT = 'HDR_headroom';
  SDL_PROP_TEXTURE_CREATE_D3D11_TEXTURE_POINTER = 'd3d11.texture';
  SDL_PROP_TEXTURE_CREATE_D3D11_TEXTURE_U_POINTER = 'd3d11.texture_u';
  SDL_PROP_TEXTURE_CREATE_D3D11_TEXTURE_V_POINTER = 'd3d11.texture_v';
  SDL_PROP_TEXTURE_CREATE_D3D12_TEXTURE_POINTER = 'd3d12.texture';
  SDL_PROP_TEXTURE_CREATE_D3D12_TEXTURE_U_POINTER = 'd3d12.texture_u';
  SDL_PROP_TEXTURE_CREATE_D3D12_TEXTURE_V_POINTER = 'd3d12.texture_v';
  SDL_PROP_TEXTURE_CREATE_METAL_PIXELBUFFER_POINTER = 'metal.pixelbuffer';
  SDL_PROP_TEXTURE_CREATE_OPENGL_TEXTURE_NUMBER = 'opengl.texture';
  SDL_PROP_TEXTURE_CREATE_OPENGL_TEXTURE_UV_NUMBER = 'opengl.texture_uv';
  SDL_PROP_TEXTURE_CREATE_OPENGL_TEXTURE_U_NUMBER = 'opengl.texture_u';
  SDL_PROP_TEXTURE_CREATE_OPENGL_TEXTURE_V_NUMBER = 'opengl.texture_v';
  SDL_PROP_TEXTURE_CREATE_OPENGLES2_TEXTURE_NUMBER = 'opengles2.texture';
  //  SDL_PROP_TEXTURE_CREATE_OPENGLES2_TEXTURE_NUMBER = 'opengles2.texture';
  SDL_PROP_TEXTURE_CREATE_OPENGLES2_TEXTURE_UV_NUMBER = 'opengles2.texture_uv';
  SDL_PROP_TEXTURE_CREATE_OPENGLES2_TEXTURE_U_NUMBER = 'opengles2.texture_u';
  SDL_PROP_TEXTURE_CREATE_OPENGLES2_TEXTURE_V_NUMBER = 'opengles2.texture_v';
  SDL_PROP_TEXTURE_CREATE_VULKAN_TEXTURE_NUMBER = 'vulkan.texture';


function SDL_GetTextureProperties(texture: PSDL_Texture): TSDL_PropertiesID; cdecl; external;

const
  SDL_PROP_TEXTURE_COLORSPACE_NUMBER = 'SDL.texture.colorspace';
  SDL_PROP_TEXTURE_SDR_WHITE_POINT_FLOAT = 'SDL.texture.SDR_white_point';
  SDL_PROP_TEXTURE_HDR_HEADROOM_FLOAT = 'SDL.texture.HDR_headroom';
  SDL_PROP_TEXTURE_D3D11_TEXTURE_POINTER = 'SDL.texture.d3d11.texture';
  SDL_PROP_TEXTURE_D3D11_TEXTURE_U_POINTER = 'SDL.texture.d3d11.texture_u';
  SDL_PROP_TEXTURE_D3D11_TEXTURE_V_POINTER = 'SDL.texture.d3d11.texture_v';
  SDL_PROP_TEXTURE_D3D12_TEXTURE_POINTER = 'SDL.texture.d3d12.texture';
  SDL_PROP_TEXTURE_D3D12_TEXTURE_U_POINTER = 'SDL.texture.d3d12.texture_u';
  SDL_PROP_TEXTURE_D3D12_TEXTURE_V_POINTER = 'SDL.texture.d3d12.texture_v';
  SDL_PROP_TEXTURE_OPENGL_TEXTURE_NUMBER = 'SDL.texture.opengl.texture';
  SDL_PROP_TEXTURE_OPENGL_TEXTURE_UV_NUMBER = 'SDL.texture.opengl.texture_uv';
  SDL_PROP_TEXTURE_OPENGL_TEXTURE_U_NUMBER = 'SDL.texture.opengl.texture_u';
  SDL_PROP_TEXTURE_OPENGL_TEXTURE_V_NUMBER = 'SDL.texture.opengl.texture_v';
  SDL_PROP_TEXTURE_OPENGL_TEXTURE_TARGET_NUMBER = 'SDL.texture.opengl.target';
  SDL_PROP_TEXTURE_OPENGL_TEX_W_FLOAT = 'SDL.texture.opengl.tex_w';
  SDL_PROP_TEXTURE_OPENGL_TEX_H_FLOAT = 'SDL.texture.opengl.tex_h';
  SDL_PROP_TEXTURE_OPENGLES2_TEXTURE_NUMBER = 'SDL.texture.opengles2.texture';
  SDL_PROP_TEXTURE_OPENGLES2_TEXTURE_UV_NUMBER = 'SDL.texture.opengles2.texture_uv';
  SDL_PROP_TEXTURE_OPENGLES2_TEXTURE_U_NUMBER = 'SDL.texture.opengles2.texture_u';
  SDL_PROP_TEXTURE_OPENGLES2_TEXTURE_V_NUMBER = 'SDL.texture.opengles2.texture_v';
  SDL_PROP_TEXTURE_OPENGLES2_TEXTURE_TARGET_NUMBER = 'SDL.texture.opengles2.target';
  SDL_PROP_TEXTURE_VULKAN_TEXTURE_POINTER = 'SDL.texture.vulkan.texture';
  SDL_PROP_TEXTURE_VULKAN_TEXTURE_U_POINTER = 'SDL.texture.vulkan.texture_u';
  SDL_PROP_TEXTURE_VULKAN_TEXTURE_V_POINTER = 'SDL.texture.vulkan.texture_v';
  SDL_PROP_TEXTURE_VULKAN_TEXTURE_UV_POINTER = 'SDL.texture.vulkan.texture_uv';

function SDL_GetRendererFromTexture(texture: PSDL_Texture): PSDL_Renderer; cdecl; external;
function SDL_QueryTexture(texture: PSDL_Texture; format: PUint32; access: Plongint; w: Plongint; h: Plongint): longint; cdecl; external;
function SDL_SetTextureColorMod(texture: PSDL_Texture; r: uint8; g: uint8; b: uint8): longint; cdecl; external;
function SDL_SetTextureColorModFloat(texture: PSDL_Texture; r: single; g: single; b: single): longint; cdecl; external;
function SDL_GetTextureColorMod(texture: PSDL_Texture; r: PUint8; g: PUint8; b: PUint8): longint; cdecl; external;
function SDL_GetTextureColorModFloat(texture: PSDL_Texture; r: Psingle; g: Psingle; b: Psingle): longint; cdecl; external;
function SDL_SetTextureAlphaMod(texture: PSDL_Texture; alpha: uint8): longint; cdecl; external;
function SDL_SetTextureAlphaModFloat(texture: PSDL_Texture; alpha: single): longint; cdecl; external;
function SDL_GetTextureAlphaMod(texture: PSDL_Texture; alpha: PUint8): longint; cdecl; external;
function SDL_GetTextureAlphaModFloat(texture: PSDL_Texture; alpha: Psingle): longint; cdecl; external;
function SDL_SetTextureBlendMode(texture: PSDL_Texture; blendMode: TSDL_BlendMode): longint; cdecl; external;
function SDL_GetTextureBlendMode(texture: PSDL_Texture; blendMode: PSDL_BlendMode): longint; cdecl; external;
function SDL_SetTextureScaleMode(texture: PSDL_Texture; scaleMode: TSDL_ScaleMode): longint; cdecl; external;
function SDL_GetTextureScaleMode(texture: PSDL_Texture; scaleMode: PSDL_ScaleMode): longint; cdecl; external;
function SDL_UpdateTexture(texture: PSDL_Texture; rect: PSDL_Rect; pixels: pointer; pitch: longint): longint; cdecl; external;
function SDL_UpdateYUVTexture(texture: PSDL_Texture; rect: PSDL_Rect; Yplane: PUint8; Ypitch: longint; Uplane: PUint8;
  Upitch: longint; Vplane: PUint8; Vpitch: longint): longint; cdecl; external;
function SDL_UpdateNVTexture(texture: PSDL_Texture; rect: PSDL_Rect; Yplane: PUint8; Ypitch: longint; UVplane: PUint8;
  UVpitch: longint): longint; cdecl; external;
function SDL_LockTexture(texture: PSDL_Texture; rect: PSDL_Rect; pixels: Ppointer; pitch: Plongint): longint; cdecl; external;
function SDL_LockTextureToSurface(texture: PSDL_Texture; rect: PSDL_Rect; surface: PPSDL_Surface): longint; cdecl; external;
procedure SDL_UnlockTexture(texture: PSDL_Texture); cdecl; external;
function SDL_SetRenderTarget(renderer: PSDL_Renderer; texture: PSDL_Texture): longint; cdecl; external;
function SDL_GetRenderTarget(renderer: PSDL_Renderer): PSDL_Texture; cdecl; external;
function SDL_SetRenderLogicalPresentation(renderer: PSDL_Renderer; w: longint; h: longint; mode: TSDL_RendererLogicalPresentation; scale_mode: TSDL_ScaleMode): longint; cdecl; external;
function SDL_GetRenderLogicalPresentation(renderer: PSDL_Renderer; w: Plongint; h: Plongint; mode: PSDL_RendererLogicalPresentation; scale_mode: PSDL_ScaleMode): longint; cdecl; external;
function SDL_RenderCoordinatesFromWindow(renderer: PSDL_Renderer; window_x: single; window_y: single; x: Psingle; y: Psingle): longint; cdecl; external;
function SDL_RenderCoordinatesToWindow(renderer: PSDL_Renderer; x: single; y: single; window_x: Psingle; window_y: Psingle): longint; cdecl; external;
function SDL_ConvertEventToRenderCoordinates(renderer: PSDL_Renderer; event: PSDL_Event): longint; cdecl; external;
function SDL_SetRenderViewport(renderer: PSDL_Renderer; rect: PSDL_Rect): longint; cdecl; external;
function SDL_GetRenderViewport(renderer: PSDL_Renderer; rect: PSDL_Rect): longint; cdecl; external;
function SDL_RenderViewportSet(renderer: PSDL_Renderer): TSDL_bool; cdecl; external;
function SDL_SetRenderClipRect(renderer: PSDL_Renderer; rect: PSDL_Rect): longint; cdecl; external;
function SDL_GetRenderClipRect(renderer: PSDL_Renderer; rect: PSDL_Rect): longint; cdecl; external;
function SDL_RenderClipEnabled(renderer: PSDL_Renderer): TSDL_bool; cdecl; external;
function SDL_SetRenderScale(renderer: PSDL_Renderer; scaleX: single; scaleY: single): longint; cdecl; external;
function SDL_GetRenderScale(renderer: PSDL_Renderer; scaleX: Psingle; scaleY: Psingle): longint; cdecl; external;
function SDL_SetRenderDrawColor(renderer: PSDL_Renderer; r: uint8; g: uint8; b: uint8; a: uint8): longint; cdecl; external;
function SDL_SetRenderDrawColorFloat(renderer: PSDL_Renderer; r: single; g: single; b: single; a: single): longint; cdecl; external;
function SDL_GetRenderDrawColor(renderer: PSDL_Renderer; r: PUint8; g: PUint8; b: PUint8; a: PUint8): longint; cdecl; external;
function SDL_GetRenderDrawColorFloat(renderer: PSDL_Renderer; r: Psingle; g: Psingle; b: Psingle; a: Psingle): longint; cdecl; external;
function SDL_SetRenderColorScale(renderer: PSDL_Renderer; scale: single): longint; cdecl; external;
function SDL_GetRenderColorScale(renderer: PSDL_Renderer; scale: Psingle): longint; cdecl; external;
function SDL_SetRenderDrawBlendMode(renderer: PSDL_Renderer; blendMode: TSDL_BlendMode): longint; cdecl; external;
function SDL_GetRenderDrawBlendMode(renderer: PSDL_Renderer; blendMode: PSDL_BlendMode): longint; cdecl; external;
function SDL_RenderClear(renderer: PSDL_Renderer): longint; cdecl; external;
function SDL_RenderPoint(renderer: PSDL_Renderer; x: single; y: single): longint; cdecl; external;
function SDL_RenderPoints(renderer: PSDL_Renderer; points: PSDL_FPoint; Count: longint): longint; cdecl; external;
function SDL_RenderLine(renderer: PSDL_Renderer; x1: single; y1: single; x2: single; y2: single): longint; cdecl; external;
function SDL_RenderLines(renderer: PSDL_Renderer; points: PSDL_FPoint; Count: longint): longint; cdecl; external;
function SDL_RenderRect(renderer: PSDL_Renderer; rect: PSDL_FRect): longint; cdecl; external;
function SDL_RenderRects(renderer: PSDL_Renderer; rects: PSDL_FRect; Count: longint): longint; cdecl; external;
function SDL_RenderFillRect(renderer: PSDL_Renderer; rect: PSDL_FRect): longint; cdecl; external;
function SDL_RenderFillRects(renderer: PSDL_Renderer; rects: PSDL_FRect; Count: longint): longint; cdecl; external;
function SDL_RenderTexture(renderer: PSDL_Renderer; texture: PSDL_Texture; srcrect: PSDL_FRect; dstrect: PSDL_FRect): longint; cdecl; external;
function SDL_RenderTextureRotated(renderer: PSDL_Renderer; texture: PSDL_Texture; srcrect: PSDL_FRect; dstrect: PSDL_FRect; angle: cdouble;
  center: PSDL_FPoint; flip: TSDL_FlipMode): longint; cdecl; external;
function SDL_RenderGeometry(renderer: PSDL_Renderer; texture: PSDL_Texture; vertices: PSDL_Vertex; num_vertices: longint; indices: Plongint;
  num_indices: longint): longint; cdecl; external;
function SDL_RenderGeometryRaw(renderer: PSDL_Renderer; texture: PSDL_Texture; xy: Psingle; xy_stride: longint; color: PSDL_Color;
  color_stride: longint; uv: Psingle; uv_stride: longint; num_vertices: longint; indices: pointer;
  num_indices: longint; size_indices: longint): longint; cdecl; external;
function SDL_RenderGeometryRawFloat(renderer: PSDL_Renderer; texture: PSDL_Texture; xy: Psingle; xy_stride: longint; color: PSDL_FColor;
  color_stride: longint; uv: Psingle; uv_stride: longint; num_vertices: longint; indices: pointer;
  num_indices: longint; size_indices: longint): longint; cdecl; external;
function SDL_RenderReadPixels(renderer: PSDL_Renderer; rect: PSDL_Rect): PSDL_Surface; cdecl; external;
function SDL_RenderPresent(renderer: PSDL_Renderer): longint; cdecl; external;
procedure SDL_DestroyTexture(texture: PSDL_Texture); cdecl; external;
procedure SDL_DestroyRenderer(renderer: PSDL_Renderer); cdecl; external;
function SDL_FlushRenderer(renderer: PSDL_Renderer): longint; cdecl; external;
function SDL_GetRenderMetalLayer(renderer: PSDL_Renderer): pointer; cdecl; external;
function SDL_GetRenderMetalCommandEncoder(renderer: PSDL_Renderer): pointer; cdecl; external;
function SDL_SetRenderVSync(renderer: PSDL_Renderer; vsync: longint): longint; cdecl; external;
function SDL_GetRenderVSync(renderer: PSDL_Renderer; vsync: Plongint): longint; cdecl; external;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}
