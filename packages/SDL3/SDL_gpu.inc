/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   17:58                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_GPUPrimitiveType = ^TSDL_GPUPrimitiveType;
  TSDL_GPUPrimitiveType = longint;

const
  SDL_GPU_PRIMITIVETYPE_TRIANGLELIST = 0;
  SDL_GPU_PRIMITIVETYPE_TRIANGLESTRIP = 1;
  SDL_GPU_PRIMITIVETYPE_LINELIST = 2;
  SDL_GPU_PRIMITIVETYPE_LINESTRIP = 3;
  SDL_GPU_PRIMITIVETYPE_POINTLIST = 4;

type
  PSDL_GPULoadOp = ^TSDL_GPULoadOp;
  TSDL_GPULoadOp = longint;

const
  SDL_GPU_LOADOP_LOAD = 0;
  SDL_GPU_LOADOP_CLEAR = 1;
  SDL_GPU_LOADOP_DONT_CARE = 2;

type
  PSDL_GPUStoreOp = ^TSDL_GPUStoreOp;
  TSDL_GPUStoreOp = longint;

const
  SDL_GPU_STOREOP_STORE = 0;
  SDL_GPU_STOREOP_DONT_CARE = 1;
  SDL_GPU_STOREOP_RESOLVE = 2;
  SDL_GPU_STOREOP_RESOLVE_AND_STORE = 3;

type
  PSDL_GPUIndexElementSize = ^TSDL_GPUIndexElementSize;
  TSDL_GPUIndexElementSize = longint;

const
  SDL_GPU_INDEXELEMENTSIZE_16BIT = 0;
  SDL_GPU_INDEXELEMENTSIZE_32BIT = 1;

type
  PSDL_GPUTextureFormat = ^TSDL_GPUTextureFormat;
  TSDL_GPUTextureFormat = longint;

const
  SDL_GPU_TEXTUREFORMAT_INVALID = 0;
  SDL_GPU_TEXTUREFORMAT_A8_UNORM = 1;
  SDL_GPU_TEXTUREFORMAT_R8_UNORM = 2;
  SDL_GPU_TEXTUREFORMAT_R8G8_UNORM = 3;
  SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM = 4;
  SDL_GPU_TEXTUREFORMAT_R16_UNORM = 5;
  SDL_GPU_TEXTUREFORMAT_R16G16_UNORM = 6;
  SDL_GPU_TEXTUREFORMAT_R16G16B16A16_UNORM = 7;
  SDL_GPU_TEXTUREFORMAT_R10G10B10A2_UNORM = 8;
  SDL_GPU_TEXTUREFORMAT_B5G6R5_UNORM = 9;
  SDL_GPU_TEXTUREFORMAT_B5G5R5A1_UNORM = 10;
  SDL_GPU_TEXTUREFORMAT_B4G4R4A4_UNORM = 11;
  SDL_GPU_TEXTUREFORMAT_B8G8R8A8_UNORM = 12;
  SDL_GPU_TEXTUREFORMAT_BC1_RGBA_UNORM = 13;
  SDL_GPU_TEXTUREFORMAT_BC2_RGBA_UNORM = 14;
  SDL_GPU_TEXTUREFORMAT_BC3_RGBA_UNORM = 15;
  SDL_GPU_TEXTUREFORMAT_BC4_R_UNORM = 16;
  SDL_GPU_TEXTUREFORMAT_BC5_RG_UNORM = 17;
  SDL_GPU_TEXTUREFORMAT_BC7_RGBA_UNORM = 18;
  SDL_GPU_TEXTUREFORMAT_BC6H_RGB_FLOAT = 19;
  SDL_GPU_TEXTUREFORMAT_BC6H_RGB_UFLOAT = 20;
  SDL_GPU_TEXTUREFORMAT_R8_SNORM = 21;
  SDL_GPU_TEXTUREFORMAT_R8G8_SNORM = 22;
  SDL_GPU_TEXTUREFORMAT_R8G8B8A8_SNORM = 23;
  SDL_GPU_TEXTUREFORMAT_R16_SNORM = 24;
  SDL_GPU_TEXTUREFORMAT_R16G16_SNORM = 25;
  SDL_GPU_TEXTUREFORMAT_R16G16B16A16_SNORM = 26;
  SDL_GPU_TEXTUREFORMAT_R16_FLOAT = 27;
  SDL_GPU_TEXTUREFORMAT_R16G16_FLOAT = 28;
  SDL_GPU_TEXTUREFORMAT_R16G16B16A16_FLOAT = 29;
  SDL_GPU_TEXTUREFORMAT_R32_FLOAT = 30;
  SDL_GPU_TEXTUREFORMAT_R32G32_FLOAT = 31;
  SDL_GPU_TEXTUREFORMAT_R32G32B32A32_FLOAT = 32;
  SDL_GPU_TEXTUREFORMAT_R11G11B10_UFLOAT = 33;
  SDL_GPU_TEXTUREFORMAT_R8_UINT = 34;
  SDL_GPU_TEXTUREFORMAT_R8G8_UINT = 35;
  SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UINT = 36;
  SDL_GPU_TEXTUREFORMAT_R16_UINT = 37;
  SDL_GPU_TEXTUREFORMAT_R16G16_UINT = 38;
  SDL_GPU_TEXTUREFORMAT_R16G16B16A16_UINT = 39;
  SDL_GPU_TEXTUREFORMAT_R32_UINT = 40;
  SDL_GPU_TEXTUREFORMAT_R32G32_UINT = 41;
  SDL_GPU_TEXTUREFORMAT_R32G32B32A32_UINT = 42;
  SDL_GPU_TEXTUREFORMAT_R8_INT = 43;
  SDL_GPU_TEXTUREFORMAT_R8G8_INT = 44;
  SDL_GPU_TEXTUREFORMAT_R8G8B8A8_INT = 45;
  SDL_GPU_TEXTUREFORMAT_R16_INT = 46;
  SDL_GPU_TEXTUREFORMAT_R16G16_INT = 47;
  SDL_GPU_TEXTUREFORMAT_R16G16B16A16_INT = 48;
  SDL_GPU_TEXTUREFORMAT_R32_INT = 49;
  SDL_GPU_TEXTUREFORMAT_R32G32_INT = 50;
  SDL_GPU_TEXTUREFORMAT_R32G32B32A32_INT = 51;
  SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM_SRGB = 52;
  SDL_GPU_TEXTUREFORMAT_B8G8R8A8_UNORM_SRGB = 53;
  SDL_GPU_TEXTUREFORMAT_BC1_RGBA_UNORM_SRGB = 54;
  SDL_GPU_TEXTUREFORMAT_BC2_RGBA_UNORM_SRGB = 55;
  SDL_GPU_TEXTUREFORMAT_BC3_RGBA_UNORM_SRGB = 56;
  SDL_GPU_TEXTUREFORMAT_BC7_RGBA_UNORM_SRGB = 57;
  SDL_GPU_TEXTUREFORMAT_D16_UNORM = 58;
  SDL_GPU_TEXTUREFORMAT_D24_UNORM = 59;
  SDL_GPU_TEXTUREFORMAT_D32_FLOAT = 60;
  SDL_GPU_TEXTUREFORMAT_D24_UNORM_S8_UINT = 61;
  SDL_GPU_TEXTUREFORMAT_D32_FLOAT_S8_UINT = 62;

type
  PSDL_GPUTextureUsageFlags = ^TSDL_GPUTextureUsageFlags;
  TSDL_GPUTextureUsageFlags = TUint32;

const
  SDL_GPU_TEXTUREUSAGE_SAMPLER = 1 shl 0;
  SDL_GPU_TEXTUREUSAGE_COLOR_TARGET = 1 shl 1;
  SDL_GPU_TEXTUREUSAGE_DEPTH_STENCIL_TARGET = 1 shl 2;
  SDL_GPU_TEXTUREUSAGE_GRAPHICS_STORAGE_READ = 1 shl 3;
  SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_READ = 1 shl 4;
  SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_WRITE = 1 shl 5;
  SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_SIMULTANEOUS_READ_WRITE = 1 shl 6;

type
  PSDL_GPUTextureType = ^TSDL_GPUTextureType;
  TSDL_GPUTextureType = longint;

const
  SDL_GPU_TEXTURETYPE_2D = 0;
  SDL_GPU_TEXTURETYPE_2D_ARRAY = 1;
  SDL_GPU_TEXTURETYPE_3D = 2;
  SDL_GPU_TEXTURETYPE_CUBE = 3;
  SDL_GPU_TEXTURETYPE_CUBE_ARRAY = 4;

type
  PSDL_GPUSampleCount = ^TSDL_GPUSampleCount;
  TSDL_GPUSampleCount = longint;

const
  SDL_GPU_SAMPLECOUNT_1 = 0;
  SDL_GPU_SAMPLECOUNT_2 = 1;
  SDL_GPU_SAMPLECOUNT_4 = 2;
  SDL_GPU_SAMPLECOUNT_8 = 3;

type
  PSDL_GPUCubeMapFace = ^TSDL_GPUCubeMapFace;
  TSDL_GPUCubeMapFace = longint;

const
  SDL_GPU_CUBEMAPFACE_POSITIVEX = 0;
  SDL_GPU_CUBEMAPFACE_NEGATIVEX = 1;
  SDL_GPU_CUBEMAPFACE_POSITIVEY = 2;
  SDL_GPU_CUBEMAPFACE_NEGATIVEY = 3;
  SDL_GPU_CUBEMAPFACE_POSITIVEZ = 4;
  SDL_GPU_CUBEMAPFACE_NEGATIVEZ = 5;

type
  PSDL_GPUBufferUsageFlags = ^TSDL_GPUBufferUsageFlags;
  TSDL_GPUBufferUsageFlags = TUint32;

const
  SDL_GPU_BUFFERUSAGE_VERTEX = 1 shl 0;
  SDL_GPU_BUFFERUSAGE_INDEX = 1 shl 1;
  SDL_GPU_BUFFERUSAGE_INDIRECT = 1 shl 2;
  SDL_GPU_BUFFERUSAGE_GRAPHICS_STORAGE_READ = 1 shl 3;
  SDL_GPU_BUFFERUSAGE_COMPUTE_STORAGE_READ = 1 shl 4;
  SDL_GPU_BUFFERUSAGE_COMPUTE_STORAGE_WRITE = 1 shl 5;

type
  PSDL_GPUTransferBufferUsage = ^TSDL_GPUTransferBufferUsage;
  TSDL_GPUTransferBufferUsage = longint;

const
  SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD = 0;
  SDL_GPU_TRANSFERBUFFERUSAGE_DOWNLOAD = 1;

type
  PSDL_GPUShaderStage = ^TSDL_GPUShaderStage;
  TSDL_GPUShaderStage = longint;

const
  SDL_GPU_SHADERSTAGE_VERTEX = 0;
  SDL_GPU_SHADERSTAGE_FRAGMENT = 1;

type
  PSDL_GPUShaderFormat = ^TSDL_GPUShaderFormat;
  TSDL_GPUShaderFormat = TUint32;

const
  SDL_GPU_SHADERFORMAT_INVALID = 0;
  SDL_GPU_SHADERFORMAT_PRIVATE = 1 shl 0;
  SDL_GPU_SHADERFORMAT_SPIRV = 1 shl 1;
  SDL_GPU_SHADERFORMAT_DXBC = 1 shl 2;
  SDL_GPU_SHADERFORMAT_DXIL = 1 shl 3;
  SDL_GPU_SHADERFORMAT_MSL = 1 shl 4;
  SDL_GPU_SHADERFORMAT_METALLIB = 1 shl 5;

type
  PSDL_GPUVertexElementFormat = ^TSDL_GPUVertexElementFormat;
  TSDL_GPUVertexElementFormat = longint;

const
  SDL_GPU_VERTEXELEMENTFORMAT_INVALID = 0;
  SDL_GPU_VERTEXELEMENTFORMAT_INT = 1;
  SDL_GPU_VERTEXELEMENTFORMAT_INT2 = 2;
  SDL_GPU_VERTEXELEMENTFORMAT_INT3 = 3;
  SDL_GPU_VERTEXELEMENTFORMAT_INT4 = 4;
  SDL_GPU_VERTEXELEMENTFORMAT_UINT = 5;
  SDL_GPU_VERTEXELEMENTFORMAT_UINT2 = 6;
  SDL_GPU_VERTEXELEMENTFORMAT_UINT3 = 7;
  SDL_GPU_VERTEXELEMENTFORMAT_UINT4 = 8;
  SDL_GPU_VERTEXELEMENTFORMAT_FLOAT = 9;
  SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2 = 10;
  SDL_GPU_VERTEXELEMENTFORMAT_FLOAT3 = 11;
  SDL_GPU_VERTEXELEMENTFORMAT_FLOAT4 = 12;
  SDL_GPU_VERTEXELEMENTFORMAT_BYTE2 = 13;
  SDL_GPU_VERTEXELEMENTFORMAT_BYTE4 = 14;
  SDL_GPU_VERTEXELEMENTFORMAT_UBYTE2 = 15;
  SDL_GPU_VERTEXELEMENTFORMAT_UBYTE4 = 16;
  SDL_GPU_VERTEXELEMENTFORMAT_BYTE2_NORM = 17;
  SDL_GPU_VERTEXELEMENTFORMAT_BYTE4_NORM = 18;
  SDL_GPU_VERTEXELEMENTFORMAT_UBYTE2_NORM = 19;
  SDL_GPU_VERTEXELEMENTFORMAT_UBYTE4_NORM = 20;
  SDL_GPU_VERTEXELEMENTFORMAT_SHORT2 = 21;
  SDL_GPU_VERTEXELEMENTFORMAT_SHORT4 = 22;
  SDL_GPU_VERTEXELEMENTFORMAT_USHORT2 = 23;
  SDL_GPU_VERTEXELEMENTFORMAT_USHORT4 = 24;
  SDL_GPU_VERTEXELEMENTFORMAT_SHORT2_NORM = 25;
  SDL_GPU_VERTEXELEMENTFORMAT_SHORT4_NORM = 26;
  SDL_GPU_VERTEXELEMENTFORMAT_USHORT2_NORM = 27;
  SDL_GPU_VERTEXELEMENTFORMAT_USHORT4_NORM = 28;
  SDL_GPU_VERTEXELEMENTFORMAT_HALF2 = 29;
  SDL_GPU_VERTEXELEMENTFORMAT_HALF4 = 30;

type
  PSDL_GPUVertexInputRate = ^TSDL_GPUVertexInputRate;
  TSDL_GPUVertexInputRate = longint;

const
  SDL_GPU_VERTEXINPUTRATE_VERTEX = 0;
  SDL_GPU_VERTEXINPUTRATE_INSTANCE = 1;

type
  PSDL_GPUFillMode = ^TSDL_GPUFillMode;
  TSDL_GPUFillMode = longint;

const
  SDL_GPU_FILLMODE_FILL = 0;
  SDL_GPU_FILLMODE_LINE = 1;

type
  PSDL_GPUCullMode = ^TSDL_GPUCullMode;
  TSDL_GPUCullMode = longint;

const
  SDL_GPU_CULLMODE_NONE = 0;
  SDL_GPU_CULLMODE_FRONT = 1;
  SDL_GPU_CULLMODE_BACK = 2;

type
  PSDL_GPUFrontFace = ^TSDL_GPUFrontFace;
  TSDL_GPUFrontFace = longint;

const
  SDL_GPU_FRONTFACE_COUNTER_CLOCKWISE = 0;
  SDL_GPU_FRONTFACE_CLOCKWISE = 1;

type
  PSDL_GPUCompareOp = ^TSDL_GPUCompareOp;
  TSDL_GPUCompareOp = longint;

const
  SDL_GPU_COMPAREOP_INVALID = 0;
  SDL_GPU_COMPAREOP_NEVER = 1;
  SDL_GPU_COMPAREOP_LESS = 2;
  SDL_GPU_COMPAREOP_EQUAL = 3;
  SDL_GPU_COMPAREOP_LESS_OR_EQUAL = 4;
  SDL_GPU_COMPAREOP_GREATER = 5;
  SDL_GPU_COMPAREOP_NOT_EQUAL = 6;
  SDL_GPU_COMPAREOP_GREATER_OR_EQUAL = 7;
  SDL_GPU_COMPAREOP_ALWAYS = 8;

type
  PSDL_GPUStencilOp = ^TSDL_GPUStencilOp;
  TSDL_GPUStencilOp = longint;

const
  SDL_GPU_STENCILOP_INVALID = 0;
  SDL_GPU_STENCILOP_KEEP = 1;
  SDL_GPU_STENCILOP_ZERO = 2;
  SDL_GPU_STENCILOP_REPLACE = 3;
  SDL_GPU_STENCILOP_INCREMENT_AND_CLAMP = 4;
  SDL_GPU_STENCILOP_DECREMENT_AND_CLAMP = 5;
  SDL_GPU_STENCILOP_INVERT = 6;
  SDL_GPU_STENCILOP_INCREMENT_AND_WRAP = 7;
  SDL_GPU_STENCILOP_DECREMENT_AND_WRAP = 8;

type
  PSDL_GPUBlendOp = ^TSDL_GPUBlendOp;
  TSDL_GPUBlendOp = longint;

const
  SDL_GPU_BLENDOP_INVALID = 0;
  SDL_GPU_BLENDOP_ADD = 1;
  SDL_GPU_BLENDOP_SUBTRACT = 2;
  SDL_GPU_BLENDOP_REVERSE_SUBTRACT = 3;
  SDL_GPU_BLENDOP_MIN = 4;
  SDL_GPU_BLENDOP_MAX = 5;

type
  PSDL_GPUBlendFactor = ^TSDL_GPUBlendFactor;
  TSDL_GPUBlendFactor = longint;

const
  SDL_GPU_BLENDFACTOR_INVALID = 0;
  SDL_GPU_BLENDFACTOR_ZERO = 1;
  SDL_GPU_BLENDFACTOR_ONE = 2;
  SDL_GPU_BLENDFACTOR_SRC_COLOR = 3;
  SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_COLOR = 4;
  SDL_GPU_BLENDFACTOR_DST_COLOR = 5;
  SDL_GPU_BLENDFACTOR_ONE_MINUS_DST_COLOR = 6;
  SDL_GPU_BLENDFACTOR_SRC_ALPHA = 7;
  SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA = 8;
  SDL_GPU_BLENDFACTOR_DST_ALPHA = 9;
  SDL_GPU_BLENDFACTOR_ONE_MINUS_DST_ALPHA = 10;
  SDL_GPU_BLENDFACTOR_CONSTANT_COLOR = 11;
  SDL_GPU_BLENDFACTOR_ONE_MINUS_CONSTANT_COLOR = 12;
  SDL_GPU_BLENDFACTOR_SRC_ALPHA_SATURATE = 13;

type
  PSDL_GPUColorComponentFlags = ^TSDL_GPUColorComponentFlags;
  TSDL_GPUColorComponentFlags = TUint8;

const
  SDL_GPU_COLORCOMPONENT_R = 1 shl 0;
  SDL_GPU_COLORCOMPONENT_G = 1 shl 1;
  SDL_GPU_COLORCOMPONENT_B = 1 shl 2;
  SDL_GPU_COLORCOMPONENT_A = 1 shl 3;

type
  PSDL_GPUFilter = ^TSDL_GPUFilter;
  TSDL_GPUFilter = longint;

const
  SDL_GPU_FILTER_NEAREST = 0;
  SDL_GPU_FILTER_LINEAR = 1;

type
  PSDL_GPUSamplerMipmapMode = ^TSDL_GPUSamplerMipmapMode;
  TSDL_GPUSamplerMipmapMode = longint;

const
  SDL_GPU_SAMPLERMIPMAPMODE_NEAREST = 0;
  SDL_GPU_SAMPLERMIPMAPMODE_LINEAR = 1;

type
  PSDL_GPUSamplerAddressMode = ^TSDL_GPUSamplerAddressMode;
  TSDL_GPUSamplerAddressMode = longint;

const
  SDL_GPU_SAMPLERADDRESSMODE_REPEAT = 0;
  SDL_GPU_SAMPLERADDRESSMODE_MIRRORED_REPEAT = 1;
  SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE = 2;

type
  PSDL_GPUPresentMode = ^TSDL_GPUPresentMode;
  TSDL_GPUPresentMode = longint;

const
  SDL_GPU_PRESENTMODE_VSYNC = 0;
  SDL_GPU_PRESENTMODE_IMMEDIATE = 1;
  SDL_GPU_PRESENTMODE_MAILBOX = 2;

type
  PSDL_GPUSwapchainComposition = ^TSDL_GPUSwapchainComposition;
  TSDL_GPUSwapchainComposition = longint;

const
  SDL_GPU_SWAPCHAINCOMPOSITION_SDR = 0;
  SDL_GPU_SWAPCHAINCOMPOSITION_SDR_LINEAR = 1;
  SDL_GPU_SWAPCHAINCOMPOSITION_HDR_EXTENDED_LINEAR = 2;
  SDL_GPU_SWAPCHAINCOMPOSITION_HDR10_ST2048 = 3;

type
  TSDL_GPUViewport = record
    x: single;
    y: single;
    w: single;
    h: single;
    min_depth: single;
    max_depth: single;
  end;

  TSDL_GPUTransferBuffer = record
  end;
  PSDL_GPUTransferBuffer = ^TSDL_GPUTransferBuffer;

  PSDL_GPUViewport = ^TSDL_GPUViewport;

  TSDL_GPUTextureTransferInfo = record
    transfer_buffer: PSDL_GPUTransferBuffer;
    offset: TUint32;
    pixels_per_row: TUint32;
    rows_per_layer: TUint32;
  end;
  PSDL_GPUTextureTransferInfo = ^TSDL_GPUTextureTransferInfo;

  TSDL_GPUTransferBufferLocation = record
    transfer_buffer: PSDL_GPUTransferBuffer;
    offset: TUint32;
  end;
  PSDL_GPUTransferBufferLocation = ^TSDL_GPUTransferBufferLocation;

  TSDL_GPUTexture = record
  end;
  PSDL_GPUTexture = ^TSDL_GPUTexture;
  PPSDL_GPUTexture = ^PSDL_GPUTexture;

  TSDL_GPUTextureLocation = record
    texture: PSDL_GPUTexture;
    mip_level: TUint32;
    layer: TUint32;
    x: TUint32;
    y: TUint32;
    z: TUint32;
  end;
  PSDL_GPUTextureLocation = ^TSDL_GPUTextureLocation;

  TSDL_GPUTextureRegion = record
    texture: PSDL_GPUTexture;
    mip_level: TUint32;
    layer: TUint32;
    x: TUint32;
    y: TUint32;
    z: TUint32;
    w: TUint32;
    h: TUint32;
    d: TUint32;
  end;
  PSDL_GPUTextureRegion = ^TSDL_GPUTextureRegion;

  TSDL_GPUBlitRegion = record
    texture: PSDL_GPUTexture;
    mip_level: TUint32;
    layer_or_depth_plane: TUint32;
    x: TUint32;
    y: TUint32;
    w: TUint32;
    h: TUint32;
  end;
  PSDL_GPUBlitRegion = ^TSDL_GPUBlitRegion;

  TSDL_GPUBuffer = record
  end;
  PSDL_GPUBuffer = ^TSDL_GPUBuffer;
  PPSDL_GPUBuffer = ^PSDL_GPUBuffer;

  TSDL_GPUBufferLocation = record
    buffer: PSDL_GPUBuffer;
    offset: TUint32;
  end;
  PSDL_GPUBufferLocation = ^TSDL_GPUBufferLocation;

  TSDL_GPUBufferRegion = record
    buffer: PSDL_GPUBuffer;
    offset: TUint32;
    size: TUint32;
  end;
  PSDL_GPUBufferRegion = ^TSDL_GPUBufferRegion;

  TSDL_GPUIndirectDrawCommand = record
    num_vertices: TUint32;
    num_instances: TUint32;
    first_vertex: TUint32;
    first_instance: TUint32;
  end;
  PSDL_GPUIndirectDrawCommand = ^TSDL_GPUIndirectDrawCommand;

  TSDL_GPUIndexedIndirectDrawCommand = record
    num_indices: TUint32;
    num_instances: TUint32;
    first_index: TUint32;
    vertex_offset: TSint32;
    first_instance: TUint32;
  end;
  PSDL_GPUIndexedIndirectDrawCommand = ^TSDL_GPUIndexedIndirectDrawCommand;

  TSDL_GPUIndirectDispatchCommand = record
    groupcount_x: TUint32;
    groupcount_y: TUint32;
    groupcount_z: TUint32;
  end;
  PSDL_GPUIndirectDispatchCommand = ^TSDL_GPUIndirectDispatchCommand;

  TSDL_GPUSamplerCreateInfo = record
    min_filter: TSDL_GPUFilter;
    mag_filter: TSDL_GPUFilter;
    mipmap_mode: TSDL_GPUSamplerMipmapMode;
    address_mode_u: TSDL_GPUSamplerAddressMode;
    address_mode_v: TSDL_GPUSamplerAddressMode;
    address_mode_w: TSDL_GPUSamplerAddressMode;
    mip_lod_bias: single;
    max_anisotropy: single;
    compare_op: TSDL_GPUCompareOp;
    min_lod: single;
    max_lod: single;
    enable_anisotropy: Tbool;
    enable_compare: Tbool;
    padding1: TUint8;
    padding2: TUint8;
    props: TSDL_PropertiesID;
  end;
  PSDL_GPUSamplerCreateInfo = ^TSDL_GPUSamplerCreateInfo;

  TSDL_GPUVertexBufferDescription = record
    slot: TUint32;
    pitch: TUint32;
    input_rate: TSDL_GPUVertexInputRate;
    instance_step_rate: TUint32;
  end;
  PSDL_GPUVertexBufferDescription = ^TSDL_GPUVertexBufferDescription;

  TSDL_GPUVertexAttribute = record
    location: TUint32;
    buffer_slot: TUint32;
    format: TSDL_GPUVertexElementFormat;
    offset: TUint32;
  end;
  PSDL_GPUVertexAttribute = ^TSDL_GPUVertexAttribute;

  TSDL_GPUVertexInputState = record
    vertex_buffer_descriptions: PSDL_GPUVertexBufferDescription;
    num_vertex_buffers: TUint32;
    vertex_attributes: PSDL_GPUVertexAttribute;
    num_vertex_attributes: TUint32;
  end;
  PSDL_GPUVertexInputState = ^TSDL_GPUVertexInputState;

  TSDL_GPUStencilOpState = record
    fail_op: TSDL_GPUStencilOp;
    pass_op: TSDL_GPUStencilOp;
    depth_fail_op: TSDL_GPUStencilOp;
    compare_op: TSDL_GPUCompareOp;
  end;
  PSDL_GPUStencilOpState = ^TSDL_GPUStencilOpState;

  TSDL_GPUColorTargetBlendState = record
    src_color_blendfactor: TSDL_GPUBlendFactor;
    dst_color_blendfactor: TSDL_GPUBlendFactor;
    color_blend_op: TSDL_GPUBlendOp;
    src_alpha_blendfactor: TSDL_GPUBlendFactor;
    dst_alpha_blendfactor: TSDL_GPUBlendFactor;
    alpha_blend_op: TSDL_GPUBlendOp;
    color_write_mask: TSDL_GPUColorComponentFlags;
    enable_blend: Tbool;
    enable_color_write_mask: Tbool;
    padding1: TUint8;
    padding2: TUint8;
  end;
  PSDL_GPUColorTargetBlendState = ^TSDL_GPUColorTargetBlendState;

  TSDL_GPUShaderCreateInfo = record
    code_size: Tsize_t;
    code: PUint8;
    entrypoint: pansichar;
    format: TSDL_GPUShaderFormat;
    stage: TSDL_GPUShaderStage;
    num_samplers: TUint32;
    num_storage_textures: TUint32;
    num_storage_buffers: TUint32;
    num_uniform_buffers: TUint32;
    props: TSDL_PropertiesID;
  end;
  PSDL_GPUShaderCreateInfo = ^TSDL_GPUShaderCreateInfo;

  TSDL_GPUTextureCreateInfo = record
    _type: TSDL_GPUTextureType;
    format: TSDL_GPUTextureFormat;
    usage: TSDL_GPUTextureUsageFlags;
    Width: TUint32;
    Height: TUint32;
    layer_count_or_depth: TUint32;
    num_levels: TUint32;
    sample_count: TSDL_GPUSampleCount;
    props: TSDL_PropertiesID;
  end;
  PSDL_GPUTextureCreateInfo = ^TSDL_GPUTextureCreateInfo;

const
  SDL_PROP_GPU_CREATETEXTURE_D3D12_CLEAR_R_FLOAT = 'SDL.gpu.createtexture.d3d12.clear.r';
  SDL_PROP_GPU_CREATETEXTURE_D3D12_CLEAR_G_FLOAT = 'SDL.gpu.createtexture.d3d12.clear.g';
  SDL_PROP_GPU_CREATETEXTURE_D3D12_CLEAR_B_FLOAT = 'SDL.gpu.createtexture.d3d12.clear.b';
  SDL_PROP_GPU_CREATETEXTURE_D3D12_CLEAR_A_FLOAT = 'SDL.gpu.createtexture.d3d12.clear.a';
  SDL_PROP_GPU_CREATETEXTURE_D3D12_CLEAR_DEPTH_FLOAT = 'SDL.gpu.createtexture.d3d12.clear.depth';
  SDL_PROP_GPU_CREATETEXTURE_D3D12_CLEAR_STENCIL_UINT8 = 'SDL.gpu.createtexture.d3d12.clear.stencil';

type
  TSDL_GPUBufferCreateInfo = record
    usage: TSDL_GPUBufferUsageFlags;
    size: TUint32;
    props: TSDL_PropertiesID;
  end;
  PSDL_GPUBufferCreateInfo = ^TSDL_GPUBufferCreateInfo;

  TSDL_GPUTransferBufferCreateInfo = record
    usage: TSDL_GPUTransferBufferUsage;
    size: TUint32;
    props: TSDL_PropertiesID;
  end;
  PSDL_GPUTransferBufferCreateInfo = ^TSDL_GPUTransferBufferCreateInfo;

  TSDL_GPURasterizerState = record
    fill_mode: TSDL_GPUFillMode;
    cull_mode: TSDL_GPUCullMode;
    front_face: TSDL_GPUFrontFace;
    depth_bias_constant_factor: single;
    depth_bias_clamp: single;
    depth_bias_slope_factor: single;
    enable_depth_bias: Tbool;
    enable_depth_clip: Tbool;
    padding1: TUint8;
    padding2: TUint8;
  end;
  PSDL_GPURasterizerState = ^TSDL_GPURasterizerState;

  TSDL_GPUMultisampleState = record
    sample_count: TSDL_GPUSampleCount;
    sample_mask: TUint32;
    enable_mask: Tbool;
    padding1: TUint8;
    padding2: TUint8;
    padding3: TUint8;
  end;
  PSDL_GPUMultisampleState = ^TSDL_GPUMultisampleState;

  TSDL_GPUDepthStencilState = record
    compare_op: TSDL_GPUCompareOp;
    back_stencil_state: TSDL_GPUStencilOpState;
    front_stencil_state: TSDL_GPUStencilOpState;
    compare_mask: TUint8;
    write_mask: TUint8;
    enable_depth_test: Tbool;
    enable_depth_write: Tbool;
    enable_stencil_test: Tbool;
    padding1: TUint8;
    padding2: TUint8;
    padding3: TUint8;
  end;
  PSDL_GPUDepthStencilState = ^TSDL_GPUDepthStencilState;

  TSDL_GPUColorTargetDescription = record
    format: TSDL_GPUTextureFormat;
    blend_state: TSDL_GPUColorTargetBlendState;
  end;
  PSDL_GPUColorTargetDescription = ^TSDL_GPUColorTargetDescription;

  TSDL_GPUGraphicsPipelineTargetInfo = record
    color_target_descriptions: PSDL_GPUColorTargetDescription;
    num_color_targets: TUint32;
    depth_stencil_format: TSDL_GPUTextureFormat;
    has_depth_stencil_target: Tbool;
    padding1: TUint8;
    padding2: TUint8;
    padding3: TUint8;
  end;
  PSDL_GPUGraphicsPipelineTargetInfo = ^TSDL_GPUGraphicsPipelineTargetInfo;

  TSDL_GPUShader = record
  end;
  PSDL_GPUShader = ^TSDL_GPUShader;

  TSDL_GPUGraphicsPipelineCreateInfo = record
    vertex_shader: PSDL_GPUShader;
    fragment_shader: PSDL_GPUShader;
    vertex_input_state: TSDL_GPUVertexInputState;
    primitive_type: TSDL_GPUPrimitiveType;
    rasterizer_state: TSDL_GPURasterizerState;
    multisample_state: TSDL_GPUMultisampleState;
    depth_stencil_state: TSDL_GPUDepthStencilState;
    target_info: TSDL_GPUGraphicsPipelineTargetInfo;
    props: TSDL_PropertiesID;
  end;
  PSDL_GPUGraphicsPipelineCreateInfo = ^TSDL_GPUGraphicsPipelineCreateInfo;

  TSDL_GPUComputePipelineCreateInfo = record
    code_size: Tsize_t;
    code: PUint8;
    entrypoint: pansichar;
    format: TSDL_GPUShaderFormat;
    num_samplers: TUint32;
    num_readonly_storage_textures: TUint32;
    num_readonly_storage_buffers: TUint32;
    num_readwrite_storage_textures: TUint32;
    num_readwrite_storage_buffers: TUint32;
    num_uniform_buffers: TUint32;
    threadcount_x: TUint32;
    threadcount_y: TUint32;
    threadcount_z: TUint32;
    props: TSDL_PropertiesID;
  end;
  PSDL_GPUComputePipelineCreateInfo = ^TSDL_GPUComputePipelineCreateInfo;

  TSDL_GPUColorTargetInfo = record
    texture: PSDL_GPUTexture;
    mip_level: TUint32;
    layer_or_depth_plane: TUint32;
    clear_color: TSDL_FColor;
    load_op: TSDL_GPULoadOp;
    store_op: TSDL_GPUStoreOp;
    resolve_texture: PSDL_GPUTexture;
    resolve_mip_level: TUint32;
    resolve_layer: TUint32;
    cycle: Tbool;
    cycle_resolve_texture: Tbool;
    padding1: TUint8;
    padding2: TUint8;
  end;
  PSDL_GPUColorTargetInfo = ^TSDL_GPUColorTargetInfo;

  TSDL_GPUDepthStencilTargetInfo = record
    texture: PSDL_GPUTexture;
    clear_depth: single;
    load_op: TSDL_GPULoadOp;
    store_op: TSDL_GPUStoreOp;
    stencil_load_op: TSDL_GPULoadOp;
    stencil_store_op: TSDL_GPUStoreOp;
    cycle: Tbool;
    clear_stencil: TUint8;
    padding1: TUint8;
    padding2: TUint8;
  end;
  PSDL_GPUDepthStencilTargetInfo = ^TSDL_GPUDepthStencilTargetInfo;

  TSDL_GPUBlitInfo = record
    Source: TSDL_GPUBlitRegion;
    destination: TSDL_GPUBlitRegion;
    load_op: TSDL_GPULoadOp;
    clear_color: TSDL_FColor;
    flip_mode: TSDL_FlipMode;
    filter: TSDL_GPUFilter;
    cycle: Tbool;
    padding1: TUint8;
    padding2: TUint8;
    padding3: TUint8;
  end;
  PSDL_GPUBlitInfo = ^TSDL_GPUBlitInfo;

  TSDL_GPUBufferBinding = record
    buffer: PSDL_GPUBuffer;
    offset: TUint32;
  end;
  PSDL_GPUBufferBinding = ^TSDL_GPUBufferBinding;

  TSDL_GPUSampler = record
  end;
  PSDL_GPUSampler = ^TSDL_GPUSampler;

  TSDL_GPUTextureSamplerBinding = record
    texture: PSDL_GPUTexture;
    sampler: PSDL_GPUSampler;
  end;
  PSDL_GPUTextureSamplerBinding = ^TSDL_GPUTextureSamplerBinding;

  TSDL_GPUStorageBufferReadWriteBinding = record
    buffer: PSDL_GPUBuffer;
    cycle: Tbool;
    padding1: TUint8;
    padding2: TUint8;
    padding3: TUint8;
  end;
  PSDL_GPUStorageBufferReadWriteBinding = ^TSDL_GPUStorageBufferReadWriteBinding;

  TSDL_GPUStorageTextureReadWriteBinding = record
    texture: PSDL_GPUTexture;
    mip_level: TUint32;
    layer: TUint32;
    cycle: Tbool;
    padding1: TUint8;
    padding2: TUint8;
    padding3: TUint8;
  end;
  PSDL_GPUStorageTextureReadWriteBinding = ^TSDL_GPUStorageTextureReadWriteBinding;

  TSDL_GPUDevice = record
  end;
  PSDL_GPUDevice = ^TSDL_GPUDevice;

  TSDL_GPUComputePipeline = record
  end;
  PSDL_GPUComputePipeline = ^TSDL_GPUComputePipeline;

  TSDL_GPUGraphicsPipeline = record
  end;
  PSDL_GPUGraphicsPipeline = ^TSDL_GPUGraphicsPipeline;

  TSDL_GPUCommandBuffer = record
  end;
  PSDL_GPUCommandBuffer = ^TSDL_GPUCommandBuffer;

  TSDL_GPURenderPass = record
  end;
  PSDL_GPURenderPass = ^TSDL_GPURenderPass;

  TSDL_GPUComputePass = record
  end;
  PSDL_GPUComputePass = ^TSDL_GPUComputePass;

  TSDL_GPUCopyPass = record
  end;
  PSDL_GPUCopyPass = ^TSDL_GPUCopyPass;

  TSDL_GPUFence = record
  end;
  PSDL_GPUFence = ^TSDL_GPUFence;
  PPSDL_GPUFence = ^PSDL_GPUFence;

function SDL_GPUSupportsShaderFormats(format_flags: TSDL_GPUShaderFormat; Name: pansichar): Tbool; cdecl; external libSDL3;
function SDL_GPUSupportsProperties(props: TSDL_PropertiesID): Tbool; cdecl; external libSDL3;
function SDL_CreateGPUDevice(format_flags: TSDL_GPUShaderFormat; debug_mode: Tbool; Name: pansichar): PSDL_GPUDevice; cdecl; external libSDL3;
function SDL_CreateGPUDeviceWithProperties(props: TSDL_PropertiesID): PSDL_GPUDevice; cdecl; external libSDL3;

const
  SDL_PROP_GPU_DEVICE_CREATE_DEBUGMODE_BOOL = 'SDL.gpu.device.create.debugmode';
  SDL_PROP_GPU_DEVICE_CREATE_PREFERLOWPOWER_BOOL = 'SDL.gpu.device.create.preferlowpower';
  SDL_PROP_GPU_DEVICE_CREATE_NAME_STRING = 'SDL.gpu.device.create.name';
  SDL_PROP_GPU_DEVICE_CREATE_SHADERS_PRIVATE_BOOL = 'SDL.gpu.device.create.shaders.private';
  SDL_PROP_GPU_DEVICE_CREATE_SHADERS_SPIRV_BOOL = 'SDL.gpu.device.create.shaders.spirv';
  SDL_PROP_GPU_DEVICE_CREATE_SHADERS_DXBC_BOOL = 'SDL.gpu.device.create.shaders.dxbc';
  SDL_PROP_GPU_DEVICE_CREATE_SHADERS_DXIL_BOOL = 'SDL.gpu.device.create.shaders.dxil';
  SDL_PROP_GPU_DEVICE_CREATE_SHADERS_MSL_BOOL = 'SDL.gpu.device.create.shaders.msl';
  SDL_PROP_GPU_DEVICE_CREATE_SHADERS_METALLIB_BOOL = 'SDL.gpu.device.create.shaders.metallib';
  SDL_PROP_GPU_DEVICE_CREATE_D3D12_SEMANTIC_NAME_STRING = 'SDL.gpu.device.create.d3d12.semantic';

procedure SDL_DestroyGPUDevice(device: PSDL_GPUDevice); cdecl; external libSDL3;
function SDL_GetNumGPUDrivers: longint; cdecl; external libSDL3;
function SDL_GetGPUDriver(index: longint): pansichar; cdecl; external libSDL3;
function SDL_GetGPUDeviceDriver(device: PSDL_GPUDevice): pansichar; cdecl; external libSDL3;
function SDL_GetGPUShaderFormats(device: PSDL_GPUDevice): TSDL_GPUShaderFormat; cdecl; external libSDL3;
function SDL_CreateGPUComputePipeline(device: PSDL_GPUDevice; createinfo: PSDL_GPUComputePipelineCreateInfo): PSDL_GPUComputePipeline; cdecl; external libSDL3;
function SDL_CreateGPUGraphicsPipeline(device: PSDL_GPUDevice; createinfo: PSDL_GPUGraphicsPipelineCreateInfo): PSDL_GPUGraphicsPipeline; cdecl; external libSDL3;
function SDL_CreateGPUSampler(device: PSDL_GPUDevice; createinfo: PSDL_GPUSamplerCreateInfo): PSDL_GPUSampler; cdecl; external libSDL3;
function SDL_CreateGPUShader(device: PSDL_GPUDevice; createinfo: PSDL_GPUShaderCreateInfo): PSDL_GPUShader; cdecl; external libSDL3;
function SDL_CreateGPUTexture(device: PSDL_GPUDevice; createinfo: PSDL_GPUTextureCreateInfo): PSDL_GPUTexture; cdecl; external libSDL3;
function SDL_CreateGPUBuffer(device: PSDL_GPUDevice; createinfo: PSDL_GPUBufferCreateInfo): PSDL_GPUBuffer; cdecl; external libSDL3;
function SDL_CreateGPUTransferBuffer(device: PSDL_GPUDevice; createinfo: PSDL_GPUTransferBufferCreateInfo): PSDL_GPUTransferBuffer; cdecl; external libSDL3;
procedure SDL_SetGPUBufferName(device: PSDL_GPUDevice; buffer: PSDL_GPUBuffer; Text: pansichar); cdecl; external libSDL3;
procedure SDL_SetGPUTextureName(device: PSDL_GPUDevice; texture: PSDL_GPUTexture; Text: pansichar); cdecl; external libSDL3;
procedure SDL_InsertGPUDebugLabel(command_buffer: PSDL_GPUCommandBuffer; Text: pansichar); cdecl; external libSDL3;
procedure SDL_PushGPUDebugGroup(command_buffer: PSDL_GPUCommandBuffer; Name: pansichar); cdecl; external libSDL3;
procedure SDL_PopGPUDebugGroup(command_buffer: PSDL_GPUCommandBuffer); cdecl; external libSDL3;
procedure SDL_ReleaseGPUTexture(device: PSDL_GPUDevice; texture: PSDL_GPUTexture); cdecl; external libSDL3;
procedure SDL_ReleaseGPUSampler(device: PSDL_GPUDevice; sampler: PSDL_GPUSampler); cdecl; external libSDL3;
procedure SDL_ReleaseGPUBuffer(device: PSDL_GPUDevice; buffer: PSDL_GPUBuffer); cdecl; external libSDL3;
procedure SDL_ReleaseGPUTransferBuffer(device: PSDL_GPUDevice; transfer_buffer: PSDL_GPUTransferBuffer); cdecl; external libSDL3;
procedure SDL_ReleaseGPUComputePipeline(device: PSDL_GPUDevice; compute_pipeline: PSDL_GPUComputePipeline); cdecl; external libSDL3;
procedure SDL_ReleaseGPUShader(device: PSDL_GPUDevice; shader: PSDL_GPUShader); cdecl; external libSDL3;
procedure SDL_ReleaseGPUGraphicsPipeline(device: PSDL_GPUDevice; graphics_pipeline: PSDL_GPUGraphicsPipeline); cdecl; external libSDL3;
function SDL_AcquireGPUCommandBuffer(device: PSDL_GPUDevice): PSDL_GPUCommandBuffer; cdecl; external libSDL3;
procedure SDL_PushGPUVertexUniformData(command_buffer: PSDL_GPUCommandBuffer; slot_index: TUint32; Data: pointer; length: TUint32); cdecl; external libSDL3;
procedure SDL_PushGPUFragmentUniformData(command_buffer: PSDL_GPUCommandBuffer; slot_index: TUint32; Data: pointer; length: TUint32); cdecl; external libSDL3;
procedure SDL_PushGPUComputeUniformData(command_buffer: PSDL_GPUCommandBuffer; slot_index: TUint32; Data: pointer; length: TUint32); cdecl; external libSDL3;
function SDL_BeginGPURenderPass(command_buffer: PSDL_GPUCommandBuffer; color_target_infos: PSDL_GPUColorTargetInfo; num_color_targets: TUint32; depth_stencil_target_info: PSDL_GPUDepthStencilTargetInfo): PSDL_GPURenderPass; cdecl; external libSDL3;
procedure SDL_BindGPUGraphicsPipeline(render_pass: PSDL_GPURenderPass; graphics_pipeline: PSDL_GPUGraphicsPipeline); cdecl; external libSDL3;
procedure SDL_SetGPUViewport(render_pass: PSDL_GPURenderPass; viewport: PSDL_GPUViewport); cdecl; external libSDL3;
procedure SDL_SetGPUScissor(render_pass: PSDL_GPURenderPass; scissor: PSDL_Rect); cdecl; external libSDL3;
procedure SDL_SetGPUBlendConstants(render_pass: PSDL_GPURenderPass; blend_constants: TSDL_FColor); cdecl; external libSDL3;
procedure SDL_SetGPUStencilReference(render_pass: PSDL_GPURenderPass; reference: TUint8); cdecl; external libSDL3;
procedure SDL_BindGPUVertexBuffers(render_pass: PSDL_GPURenderPass; first_slot: TUint32; bindings: PSDL_GPUBufferBinding; num_bindings: TUint32); cdecl; external libSDL3;
procedure SDL_BindGPUIndexBuffer(render_pass: PSDL_GPURenderPass; binding: PSDL_GPUBufferBinding; index_element_size: TSDL_GPUIndexElementSize); cdecl; external libSDL3;
procedure SDL_BindGPUVertexSamplers(render_pass: PSDL_GPURenderPass; first_slot: TUint32; texture_sampler_bindings: PSDL_GPUTextureSamplerBinding; num_bindings: TUint32); cdecl; external libSDL3;
procedure SDL_BindGPUVertexStorageTextures(render_pass: PSDL_GPURenderPass; first_slot: TUint32; storage_textures: PPSDL_GPUTexture; num_bindings: TUint32); cdecl; external libSDL3;
procedure SDL_BindGPUVertexStorageBuffers(render_pass: PSDL_GPURenderPass; first_slot: TUint32; storage_buffers: PPSDL_GPUBuffer; num_bindings: TUint32); cdecl; external libSDL3;
procedure SDL_BindGPUFragmentSamplers(render_pass: PSDL_GPURenderPass; first_slot: TUint32; texture_sampler_bindings: PSDL_GPUTextureSamplerBinding; num_bindings: TUint32); cdecl; external libSDL3;
procedure SDL_BindGPUFragmentStorageTextures(render_pass: PSDL_GPURenderPass; first_slot: TUint32; storage_textures: PPSDL_GPUTexture; num_bindings: TUint32); cdecl; external libSDL3;
procedure SDL_BindGPUFragmentStorageBuffers(render_pass: PSDL_GPURenderPass; first_slot: TUint32; storage_buffers: PPSDL_GPUBuffer; num_bindings: TUint32); cdecl; external libSDL3;
procedure SDL_DrawGPUIndexedPrimitives(render_pass: PSDL_GPURenderPass; num_indices: TUint32; num_instances: TUint32; first_index: TUint32; vertex_offset: TSint32;
  first_instance: TUint32); cdecl; external libSDL3;
procedure SDL_DrawGPUPrimitives(render_pass: PSDL_GPURenderPass; num_vertices: TUint32; num_instances: TUint32; first_vertex: TUint32; first_instance: TUint32); cdecl; external libSDL3;
procedure SDL_DrawGPUPrimitivesIndirect(render_pass: PSDL_GPURenderPass; buffer: PSDL_GPUBuffer; offset: TUint32; draw_count: TUint32); cdecl; external libSDL3;
procedure SDL_DrawGPUIndexedPrimitivesIndirect(render_pass: PSDL_GPURenderPass; buffer: PSDL_GPUBuffer; offset: TUint32; draw_count: TUint32); cdecl; external libSDL3;
procedure SDL_EndGPURenderPass(render_pass: PSDL_GPURenderPass); cdecl; external libSDL3;
function SDL_BeginGPUComputePass(command_buffer: PSDL_GPUCommandBuffer; storage_texture_bindings: PSDL_GPUStorageTextureReadWriteBinding; num_storage_texture_bindings: TUint32; storage_buffer_bindings: PSDL_GPUStorageBufferReadWriteBinding;
  num_storage_buffer_bindings: TUint32): PSDL_GPUComputePass; cdecl; external libSDL3;
procedure SDL_BindGPUComputePipeline(compute_pass: PSDL_GPUComputePass; compute_pipeline: PSDL_GPUComputePipeline); cdecl; external libSDL3;
procedure SDL_BindGPUComputeSamplers(compute_pass: PSDL_GPUComputePass; first_slot: TUint32; texture_sampler_bindings: PSDL_GPUTextureSamplerBinding; num_bindings: TUint32); cdecl; external libSDL3;
procedure SDL_BindGPUComputeStorageTextures(compute_pass: PSDL_GPUComputePass; first_slot: TUint32; storage_textures: PPSDL_GPUTexture; num_bindings: TUint32); cdecl; external libSDL3;
procedure SDL_BindGPUComputeStorageBuffers(compute_pass: PSDL_GPUComputePass; first_slot: TUint32; storage_buffers: PPSDL_GPUBuffer; num_bindings: TUint32); cdecl; external libSDL3;
procedure SDL_DispatchGPUCompute(compute_pass: PSDL_GPUComputePass; groupcount_x: TUint32; groupcount_y: TUint32; groupcount_z: TUint32); cdecl; external libSDL3;
procedure SDL_DispatchGPUComputeIndirect(compute_pass: PSDL_GPUComputePass; buffer: PSDL_GPUBuffer; offset: TUint32); cdecl; external libSDL3;
procedure SDL_EndGPUComputePass(compute_pass: PSDL_GPUComputePass); cdecl; external libSDL3;
function SDL_MapGPUTransferBuffer(device: PSDL_GPUDevice; transfer_buffer: PSDL_GPUTransferBuffer; cycle: Tbool): pointer; cdecl; external libSDL3;
procedure SDL_UnmapGPUTransferBuffer(device: PSDL_GPUDevice; transfer_buffer: PSDL_GPUTransferBuffer); cdecl; external libSDL3;
function SDL_BeginGPUCopyPass(command_buffer: PSDL_GPUCommandBuffer): PSDL_GPUCopyPass; cdecl; external libSDL3;
procedure SDL_UploadToGPUTexture(copy_pass: PSDL_GPUCopyPass; Source: PSDL_GPUTextureTransferInfo; destination: PSDL_GPUTextureRegion; cycle: Tbool); cdecl; external libSDL3;
procedure SDL_UploadToGPUBuffer(copy_pass: PSDL_GPUCopyPass; Source: PSDL_GPUTransferBufferLocation; destination: PSDL_GPUBufferRegion; cycle: Tbool); cdecl; external libSDL3;
procedure SDL_CopyGPUTextureToTexture(copy_pass: PSDL_GPUCopyPass; Source: PSDL_GPUTextureLocation; destination: PSDL_GPUTextureLocation; w: TUint32; h: TUint32;
  d: TUint32; cycle: Tbool); cdecl; external libSDL3;
procedure SDL_CopyGPUBufferToBuffer(copy_pass: PSDL_GPUCopyPass; Source: PSDL_GPUBufferLocation; destination: PSDL_GPUBufferLocation; size: TUint32; cycle: Tbool); cdecl; external libSDL3;
procedure SDL_DownloadFromGPUTexture(copy_pass: PSDL_GPUCopyPass; Source: PSDL_GPUTextureRegion; destination: PSDL_GPUTextureTransferInfo); cdecl; external libSDL3;
procedure SDL_DownloadFromGPUBuffer(copy_pass: PSDL_GPUCopyPass; Source: PSDL_GPUBufferRegion; destination: PSDL_GPUTransferBufferLocation); cdecl; external libSDL3;
procedure SDL_EndGPUCopyPass(copy_pass: PSDL_GPUCopyPass); cdecl; external libSDL3;
procedure SDL_GenerateMipmapsForGPUTexture(command_buffer: PSDL_GPUCommandBuffer; texture: PSDL_GPUTexture); cdecl; external libSDL3;
procedure SDL_BlitGPUTexture(command_buffer: PSDL_GPUCommandBuffer; info: PSDL_GPUBlitInfo); cdecl; external libSDL3;
function SDL_WindowSupportsGPUSwapchainComposition(device: PSDL_GPUDevice; window: PSDL_Window; swapchain_composition: TSDL_GPUSwapchainComposition): Tbool; cdecl; external libSDL3;
function SDL_WindowSupportsGPUPresentMode(device: PSDL_GPUDevice; window: PSDL_Window; present_mode: TSDL_GPUPresentMode): Tbool; cdecl; external libSDL3;
function SDL_ClaimWindowForGPUDevice(device: PSDL_GPUDevice; window: PSDL_Window): Tbool; cdecl; external libSDL3;
procedure SDL_ReleaseWindowFromGPUDevice(device: PSDL_GPUDevice; window: PSDL_Window); cdecl; external libSDL3;
function SDL_SetGPUSwapchainParameters(device: PSDL_GPUDevice; window: PSDL_Window; swapchain_composition: TSDL_GPUSwapchainComposition; present_mode: TSDL_GPUPresentMode): Tbool; cdecl; external libSDL3;
function SDL_GetGPUSwapchainTextureFormat(device: PSDL_GPUDevice; window: PSDL_Window): TSDL_GPUTextureFormat; cdecl; external libSDL3;
function SDL_AcquireGPUSwapchainTexture(command_buffer: PSDL_GPUCommandBuffer; window: PSDL_Window; swapchain_texture: PPSDL_GPUTexture; swapchain_texture_width: PUint32; swapchain_texture_height: PUint32): Tbool; cdecl; external libSDL3;
function SDL_SubmitGPUCommandBuffer(command_buffer: PSDL_GPUCommandBuffer): Tbool; cdecl; external libSDL3;
function SDL_SubmitGPUCommandBufferAndAcquireFence(command_buffer: PSDL_GPUCommandBuffer): PSDL_GPUFence; cdecl; external libSDL3;
function SDL_WaitForGPUIdle(device: PSDL_GPUDevice): Tbool; cdecl; external libSDL3;
function SDL_WaitForGPUFences(device: PSDL_GPUDevice; wait_all: Tbool; fences: PPSDL_GPUFence; num_fences: TUint32): Tbool; cdecl; external libSDL3;
function SDL_QueryGPUFence(device: PSDL_GPUDevice; fence: PSDL_GPUFence): Tbool; cdecl; external libSDL3;
procedure SDL_ReleaseGPUFence(device: PSDL_GPUDevice; fence: PSDL_GPUFence); cdecl; external libSDL3;
function SDL_GPUTextureFormatTexelBlockSize(format: TSDL_GPUTextureFormat): TUint32; cdecl; external libSDL3;
function SDL_GPUTextureSupportsFormat(device: PSDL_GPUDevice; format: TSDL_GPUTextureFormat; _type: TSDL_GPUTextureType; usage: TSDL_GPUTextureUsageFlags): Tbool; cdecl; external libSDL3;
function SDL_GPUTextureSupportsSampleCount(device: PSDL_GPUDevice; format: TSDL_GPUTextureFormat; sample_count: TSDL_GPUSampleCount): Tbool; cdecl; external libSDL3;
{$ifdef SDL_PLATFORM_GDK}
procedure SDL_GDKSuspendGPU(device: PSDL_GPUDevice); cdecl; external libSDL3;
procedure SDL_GDKResumeGPU(device: PSDL_GPUDevice); cdecl; external libSDL3;
{$endif}

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}
