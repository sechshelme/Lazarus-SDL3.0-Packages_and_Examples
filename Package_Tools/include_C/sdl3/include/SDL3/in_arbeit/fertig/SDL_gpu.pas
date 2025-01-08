unit SDL_gpu;

interface

uses
  ctypes, SDL_stdinc;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{
  Simple DirectMedia Layer
  Copyright (C) 1997-2025 Sam Lantinga <slouken@libsdl.org>

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
 }
{ WIKI CATEGORY: GPU  }
{*
 * # CategoryGPU
 *
 * The GPU API offers a cross-platform way for apps to talk to modern graphics
 * hardware. It offers both 3D graphics and compute support, in the style of
 * Metal, Vulkan, and Direct3D 12.
 *
 * A basic workflow might be something like this:
 *
 * The app creates a GPU device with SDL_CreateGPUDevice(), and assigns it to
 * a window with SDL_ClaimWindowForGPUDevice()--although strictly speaking you
 * can render offscreen entirely, perhaps for image processing, and not use a
 * window at all.
 *
 * Next the app prepares static data (things that are created once and used
 * over and over). For example:
 *
 * - Shaders (programs that run on the GPU): use SDL_CreateGPUShader().
 * - Vertex buffers (arrays of geometry data) and other data rendering will
 *   need: use SDL_UploadToGPUBuffer().
 * - Textures (images): use SDL_UploadToGPUTexture().
 * - Samplers (how textures should be read from): use SDL_CreateGPUSampler().
 * - Render pipelines (precalculated rendering state): use
 *   SDL_CreateGPUGraphicsPipeline()
 *
 * To render, the app creates one or more command buffers, with
 * SDL_AcquireGPUCommandBuffer(). Command buffers collect rendering
 * instructions that will be submitted to the GPU in batch. Complex scenes can
 * use multiple command buffers, maybe configured across multiple threads in
 * parallel, as long as they are submitted in the correct order, but many apps
 * will just need one command buffer per frame.
 *
 * Rendering can happen to a texture (what other APIs call a "render target")
 * or it can happen to the swapchain texture (which is just a special texture
 * that represents a window's contents). The app can use
 * SDL_WaitAndAcquireGPUSwapchainTexture() to render to the window.
 *
 * Rendering actually happens in a Render Pass, which is encoded into a
 * command buffer. One can encode multiple render passes (or alternate between
 * render and compute passes) in a single command buffer, but many apps might
 * simply need a single render pass in a single command buffer. Render Passes
 * can render to up to four color textures and one depth texture
 * simultaneously. If the set of textures being rendered to needs to change,
 * the Render Pass must be ended and a new one must be begun.
 *
 * The app calls SDL_BeginGPURenderPass(). Then it sets states it needs for
 * each draw:
 *
 * - SDL_BindGPUGraphicsPipeline()
 * - SDL_SetGPUViewport()
 * - SDL_BindGPUVertexBuffers()
 * - SDL_BindGPUVertexSamplers()
 * - etc
 *
 * Then, make the actual draw commands with these states:
 *
 * - SDL_DrawGPUPrimitives()
 * - SDL_DrawGPUPrimitivesIndirect()
 * - SDL_DrawGPUIndexedPrimitivesIndirect()
 * - etc
 *
 * After all the drawing commands for a pass are complete, the app should call
 * SDL_EndGPURenderPass(). Once a render pass ends all render-related state is
 * reset.
 *
 * The app can begin new Render Passes and make new draws in the same command
 * buffer until the entire scene is rendered.
 *
 * Once all of the render commands for the scene are complete, the app calls
 * SDL_SubmitGPUCommandBuffer() to send it to the GPU for processing.
 *
 * If the app needs to read back data from texture or buffers, the API has an
 * efficient way of doing this, provided that the app is willing to tolerate
 * some latency. When the app uses SDL_DownloadFromGPUTexture() or
 * SDL_DownloadFromGPUBuffer(), submitting the command buffer with
 * SDL_SubmitGPUCommandBufferAndAcquireFence() will return a fence handle that
 * the app can poll or wait on in a thread. Once the fence indicates that the
 * command buffer is done processing, it is safe to read the downloaded data.
 * Make sure to call SDL_ReleaseGPUFence() when done with the fence.
 *
 * The API also has "compute" support. The app calls SDL_BeginGPUComputePass()
 * with compute-writeable textures and/or buffers, which can be written to in
 * a compute shader. Then it sets states it needs for the compute dispatches:
 *
 * - SDL_BindGPUComputePipeline()
 * - SDL_BindGPUComputeStorageBuffers()
 * - SDL_BindGPUComputeStorageTextures()
 *
 * Then, dispatch compute work:
 *
 * - SDL_DispatchGPUCompute()
 *
 * For advanced users, this opens up powerful GPU-driven workflows.
 *
 * Graphics and compute pipelines require the use of shaders, which as
 * mentioned above are small programs executed on the GPU. Each backend
 * (Vulkan, Metal, D3D12) requires a different shader format. When the app
 * creates the GPU device, the app lets the device know which shader formats
 * the app can provide. It will then select the appropriate backend depending
 * on the available shader formats and the backends available on the platform.
 * When creating shaders, the app must provide the correct shader format for
 * the selected backend. If you would like to learn more about why the API
 * works this way, there is a detailed
 * [blog post](https://moonside.games/posts/layers-all-the-way-down/)
 * explaining this situation.
 *
 * It is optimal for apps to pre-compile the shader formats they might use,
 * but for ease of use SDL provides a separate project,
 * [SDL_shadercross](https://github.com/libsdl-org/SDL_shadercross)
 * , for performing runtime shader cross-compilation.
 *
 * This is an extremely quick overview that leaves out several important
 * details. Already, though, one can see that GPU programming can be quite
 * complex! If you just need simple 2D graphics, the
 * [Render API](https://wiki.libsdl.org/SDL3/CategoryRender)
 * is much easier to use but still hardware-accelerated. That said, even for
 * 2D applications the performance benefits and expressiveness of the GPU API
 * are significant.
 *
 * The GPU API targets a feature set with a wide range of hardware support and
 * ease of portability. It is designed so that the app won't have to branch
 * itself by querying feature support. If you need cutting-edge features with
 * limited hardware support, this API is probably not for you.
 *
 * Examples demonstrating proper usage of this API can be found
 * [here](https://github.com/TheSpydog/SDL_gpu_examples)
 * .
 *
 * ## Performance considerations
 *
 * Here are some basic tips for maximizing your rendering performance.
 *
 * - Beginning a new render pass is relatively expensive. Use as few render
 *   passes as you can.
 * - Minimize the amount of state changes. For example, binding a pipeline is
 *   relatively cheap, but doing it hundreds of times when you don't need to
 *   will slow the performance significantly.
 * - Perform your data uploads as early as possible in the frame.
 * - Don't churn resources. Creating and releasing resources is expensive.
 *   It's better to create what you need up front and cache it.
 * - Don't use uniform buffers for large amounts of data (more than a matrix
 *   or so). Use a storage buffer instead.
 * - Use cycling correctly. There is a detailed explanation of cycling further
 *   below.
 * - Use culling techniques to minimize pixel writes. The less writing the GPU
 *   has to do the better. Culling can be a very advanced topic but even
 *   simple culling techniques can boost performance significantly.
 *
 * In general try to remember the golden rule of performance: doing things is
 * more expensive than not doing things. Don't Touch The Driver!
 *
 * ## FAQ
 *
 * **Question: When are you adding more advanced features, like ray tracing or
 * mesh shaders?**
 *
 * Answer: We don't have immediate plans to add more bleeding-edge features,
 * but we certainly might in the future, when these features prove worthwhile,
 * and reasonable to implement across several platforms and underlying APIs.
 * So while these things are not in the "never" category, they are definitely
 * not "near future" items either.
 *
 * **Question: Why is my shader not working?**
 *
 * Answer: A common oversight when using shaders is not properly laying out
 * the shader resources/registers correctly. The GPU API is very strict with
 * how it wants resources to be laid out and it's difficult for the API to
 * automatically validate shaders to see if they have a compatible layout. See
 * the documentation for SDL_CreateGPUShader() and
 * SDL_CreateGPUComputePipeline() for information on the expected layout.
 *
 * Another common issue is not setting the correct number of samplers,
 * textures, and buffers in SDL_GPUShaderCreateInfo. If possible use shader
 * reflection to extract the required information from the shader
 * automatically instead of manually filling in the struct's values.
 *
 * **Question: My application isn't performing very well. Is this the GPU
 * API's fault?**
 *
 * Answer: No. Long answer: The GPU API is a relatively thin layer over the
 * underlying graphics API. While it's possible that we have done something
 * inefficiently, it's very unlikely especially if you are relatively
 * inexperienced with GPU rendering. Please see the performance tips above and
 * make sure you are following them. Additionally, tools like RenderDoc can be
 * very helpful for diagnosing incorrect behavior and performance issues.
 *
 * ## System Requirements
 *
 * **Vulkan:** Supported on Windows, Linux, Nintendo Switch, and certain
 * Android devices. Requires Vulkan 1.0 with the following extensions and
 * device features:
 *
 * - `VK_KHR_swapchain`
 * - `VK_KHR_maintenance1`
 * - `independentBlend`
 * - `imageCubeArray`
 * - `depthClamp`
 * - `shaderClipDistance`
 * - `drawIndirectFirstInstance`
 *
 * **D3D12:** Supported on Windows 10 or newer, Xbox One (GDK), and Xbox
 * Series X|S (GDK). Requires a GPU that supports DirectX 12 Feature Level
 * 11_1.
 *
 * **Metal:** Supported on macOS 10.14+ and iOS/tvOS 13.0+. Hardware
 * requirements vary by operating system:
 *
 * - macOS requires an Apple Silicon or
 *   [Intel Mac2 family](https://developer.apple.com/documentation/metal/mtlfeatureset/mtlfeatureset_macos_gpufamily2_v1?language=objc)
 *   GPU
 * - iOS/tvOS requires an A9 GPU or newer
 * - iOS Simulator and tvOS Simulator are unsupported
 *
 * ## Uniform Data
 *
 * Uniforms are for passing data to shaders. The uniform data will be constant
 * across all executions of the shader.
 *
 * There are 4 available uniform slots per shader stage (where the stages are
 * vertex, fragment, and compute). Uniform data pushed to a slot on a stage
 * keeps its value throughout the command buffer until you call the relevant
 * Push function on that slot again.
 *
 * For example, you could write your vertex shaders to read a camera matrix
 * from uniform binding slot 0, push the camera matrix at the start of the
 * command buffer, and that data will be used for every subsequent draw call.
 *
 * It is valid to push uniform data during a render or compute pass.
 *
 * Uniforms are best for pushing small amounts of data. If you are pushing
 * more than a matrix or two per call you should consider using a storage
 * buffer instead.
 *
 * ## A Note On Cycling
 *
 * When using a command buffer, operations do not occur immediately - they
 * occur some time after the command buffer is submitted.
 *
 * When a resource is used in a pending or active command buffer, it is
 * considered to be "bound". When a resource is no longer used in any pending
 * or active command buffers, it is considered to be "unbound".
 *
 * If data resources are bound, it is unspecified when that data will be
 * unbound unless you acquire a fence when submitting the command buffer and
 * wait on it. However, this doesn't mean you need to track resource usage
 * manually.
 *
 * All of the functions and structs that involve writing to a resource have a
 * "cycle" bool. SDL_GPUTransferBuffer, SDL_GPUBuffer, and SDL_GPUTexture all
 * effectively function as ring buffers on internal resources. When cycle is
 * true, if the resource is bound, the cycle rotates to the next unbound
 * internal resource, or if none are available, a new one is created. This
 * means you don't have to worry about complex state tracking and
 * synchronization as long as cycling is correctly employed.
 *
 * For example: you can call SDL_MapGPUTransferBuffer(), write texture data,
 * SDL_UnmapGPUTransferBuffer(), and then SDL_UploadToGPUTexture(). The next
 * time you write texture data to the transfer buffer, if you set the cycle
 * param to true, you don't have to worry about overwriting any data that is
 * not yet uploaded.
 *
 * Another example: If you are using a texture in a render pass every frame,
 * this can cause a data dependency between frames. If you set cycle to true
 * in the SDL_GPUColorTargetInfo struct, you can prevent this data dependency.
 *
 * Cycling will never undefine already bound data. When cycling, all data in
 * the resource is considered to be undefined for subsequent commands until
 * that data is written again. You must take care not to read undefined data.
 *
 * Note that when cycling a texture, the entire texture will be cycled, even
 * if only part of the texture is used in the call, so you must consider the
 * entire texture to contain undefined data after cycling.
 *
 * You must also take care not to overwrite a section of data that has been
 * referenced in a command without cycling first. It is OK to overwrite
 * unreferenced data in a bound resource without cycling, but overwriting a
 * section of data that has already been referenced will produce unexpected
 * results.
  }
{$ifndef SDL_gpu_h_}
{$define SDL_gpu_h_}
{$include <SDL3/SDL_stdinc.h>}
{$include <SDL3/SDL_pixels.h>}
{$include <SDL3/SDL_properties.h>}
{$include <SDL3/SDL_rect.h>}
{$include <SDL3/SDL_surface.h>}
{$include <SDL3/SDL_video.h>}
{$include <SDL3/SDL_begin_code.h>}
{ C++ extern C conditionnal removed }
{ __cplusplus  }
{ Type Declarations  }
{*
 * An opaque handle representing the SDL_GPU context.
 *
 * \since This struct is available since SDL 3.1.3
  }
type
{*
 * An opaque handle representing a buffer.
 *
 * Used for vertices, indices, indirect draw commands, and general compute
 * data.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUBuffer
 * \sa SDL_SetGPUBufferName
 * \sa SDL_UploadToGPUBuffer
 * \sa SDL_DownloadFromGPUBuffer
 * \sa SDL_CopyGPUBufferToBuffer
 * \sa SDL_BindGPUVertexBuffers
 * \sa SDL_BindGPUIndexBuffer
 * \sa SDL_BindGPUVertexStorageBuffers
 * \sa SDL_BindGPUFragmentStorageBuffers
 * \sa SDL_DrawGPUPrimitivesIndirect
 * \sa SDL_DrawGPUIndexedPrimitivesIndirect
 * \sa SDL_BindGPUComputeStorageBuffers
 * \sa SDL_DispatchGPUComputeIndirect
 * \sa SDL_ReleaseGPUBuffer
  }
{*
 * An opaque handle representing a transfer buffer.
 *
 * Used for transferring data to and from the device.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUTransferBuffer
 * \sa SDL_MapGPUTransferBuffer
 * \sa SDL_UnmapGPUTransferBuffer
 * \sa SDL_UploadToGPUBuffer
 * \sa SDL_UploadToGPUTexture
 * \sa SDL_DownloadFromGPUBuffer
 * \sa SDL_DownloadFromGPUTexture
 * \sa SDL_ReleaseGPUTransferBuffer
  }
{*
 * An opaque handle representing a texture.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUTexture
 * \sa SDL_SetGPUTextureName
 * \sa SDL_UploadToGPUTexture
 * \sa SDL_DownloadFromGPUTexture
 * \sa SDL_CopyGPUTextureToTexture
 * \sa SDL_BindGPUVertexSamplers
 * \sa SDL_BindGPUVertexStorageTextures
 * \sa SDL_BindGPUFragmentSamplers
 * \sa SDL_BindGPUFragmentStorageTextures
 * \sa SDL_BindGPUComputeStorageTextures
 * \sa SDL_GenerateMipmapsForGPUTexture
 * \sa SDL_BlitGPUTexture
 * \sa SDL_ReleaseGPUTexture
  }
{*
 * An opaque handle representing a sampler.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUSampler
 * \sa SDL_BindGPUVertexSamplers
 * \sa SDL_BindGPUFragmentSamplers
 * \sa SDL_ReleaseGPUSampler
  }
{*
 * An opaque handle representing a compiled shader object.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUShader
 * \sa SDL_CreateGPUGraphicsPipeline
 * \sa SDL_ReleaseGPUShader
  }
{*
 * An opaque handle representing a compute pipeline.
 *
 * Used during compute passes.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUComputePipeline
 * \sa SDL_BindGPUComputePipeline
 * \sa SDL_ReleaseGPUComputePipeline
  }
{*
 * An opaque handle representing a graphics pipeline.
 *
 * Used during render passes.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUGraphicsPipeline
 * \sa SDL_BindGPUGraphicsPipeline
 * \sa SDL_ReleaseGPUGraphicsPipeline
  }
{*
 * An opaque handle representing a command buffer.
 *
 * Most state is managed via command buffers. When setting state using a
 * command buffer, that state is local to the command buffer.
 *
 * Commands only begin execution on the GPU once SDL_SubmitGPUCommandBuffer is
 * called. Once the command buffer is submitted, it is no longer valid to use
 * it.
 *
 * Command buffers are executed in submission order. If you submit command
 * buffer A and then command buffer B all commands in A will begin executing
 * before any command in B begins executing.
 *
 * In multi-threading scenarios, you should only access a command buffer on
 * the thread you acquired it from.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_AcquireGPUCommandBuffer
 * \sa SDL_SubmitGPUCommandBuffer
 * \sa SDL_SubmitGPUCommandBufferAndAcquireFence
  }
{*
 * An opaque handle representing a render pass.
 *
 * This handle is transient and should not be held or referenced after
 * SDL_EndGPURenderPass is called.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_BeginGPURenderPass
 * \sa SDL_EndGPURenderPass
  }
{*
 * An opaque handle representing a compute pass.
 *
 * This handle is transient and should not be held or referenced after
 * SDL_EndGPUComputePass is called.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_BeginGPUComputePass
 * \sa SDL_EndGPUComputePass
  }
{*
 * An opaque handle representing a copy pass.
 *
 * This handle is transient and should not be held or referenced after
 * SDL_EndGPUCopyPass is called.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_BeginGPUCopyPass
 * \sa SDL_EndGPUCopyPass
  }
{*
 * An opaque handle representing a fence.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_SubmitGPUCommandBufferAndAcquireFence
 * \sa SDL_QueryGPUFence
 * \sa SDL_WaitForGPUFences
 * \sa SDL_ReleaseGPUFence
  }
{*
 * Specifies the primitive topology of a graphics pipeline.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUGraphicsPipeline
  }
{*< A series of separate triangles.  }
{*< A series of connected triangles.  }
{*< A series of separate lines.  }
{*< A series of connected lines.  }
{*< A series of separate points.  }

  PSDL_GPUPrimitiveType = ^TSDL_GPUPrimitiveType;
  TSDL_GPUPrimitiveType =  Longint;
  Const
    SDL_GPU_PRIMITIVETYPE_TRIANGLELIST = 0;
    SDL_GPU_PRIMITIVETYPE_TRIANGLESTRIP = 1;
    SDL_GPU_PRIMITIVETYPE_LINELIST = 2;
    SDL_GPU_PRIMITIVETYPE_LINESTRIP = 3;
    SDL_GPU_PRIMITIVETYPE_POINTLIST = 4;
;
{*
 * Specifies how the contents of a texture attached to a render pass are
 * treated at the beginning of the render pass.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_BeginGPURenderPass
  }
{*< The previous contents of the texture will be preserved.  }
{*< The contents of the texture will be cleared to a color.  }
{*< The previous contents of the texture need not be preserved. The contents will be undefined.  }
type
  PSDL_GPULoadOp = ^TSDL_GPULoadOp;
  TSDL_GPULoadOp =  Longint;
  Const
    SDL_GPU_LOADOP_LOAD = 0;
    SDL_GPU_LOADOP_CLEAR = 1;
    SDL_GPU_LOADOP_DONT_CARE = 2;
;
{*
 * Specifies how the contents of a texture attached to a render pass are
 * treated at the end of the render pass.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_BeginGPURenderPass
  }
{*< The contents generated during the render pass will be written to memory.  }
{*< The contents generated during the render pass are not needed and may be discarded. The contents will be undefined.  }
{*< The multisample contents generated during the render pass will be resolved to a non-multisample texture. The contents in the multisample texture may then be discarded and will be undefined.  }
{*< The multisample contents generated during the render pass will be resolved to a non-multisample texture. The contents in the multisample texture will be written to memory.  }
type
  PSDL_GPUStoreOp = ^TSDL_GPUStoreOp;
  TSDL_GPUStoreOp =  Longint;
  Const
    SDL_GPU_STOREOP_STORE = 0;
    SDL_GPU_STOREOP_DONT_CARE = 1;
    SDL_GPU_STOREOP_RESOLVE = 2;
    SDL_GPU_STOREOP_RESOLVE_AND_STORE = 3;
;
{*
 * Specifies the size of elements in an index buffer.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUGraphicsPipeline
  }
{*< The index elements are 16-bit.  }
{*< The index elements are 32-bit.  }
type
  PSDL_GPUIndexElementSize = ^TSDL_GPUIndexElementSize;
  TSDL_GPUIndexElementSize =  Longint;
  Const
    SDL_GPU_INDEXELEMENTSIZE_16BIT = 0;
    SDL_GPU_INDEXELEMENTSIZE_32BIT = 1;
;
{*
 * Specifies the pixel format of a texture.
 *
 * Texture format support varies depending on driver, hardware, and usage
 * flags. In general, you should use SDL_GPUTextureSupportsFormat to query if
 * a format is supported before using it. However, there are a few guaranteed
 * formats.
 *
 * FIXME: Check universal support for 32-bit component formats FIXME: Check
 * universal support for SIMULTANEOUS_READ_WRITE
 *
 * For SAMPLER usage, the following formats are universally supported:
 *
 * - R8G8B8A8_UNORM
 * - B8G8R8A8_UNORM
 * - R8_UNORM
 * - R8_SNORM
 * - R8G8_UNORM
 * - R8G8_SNORM
 * - R8G8B8A8_SNORM
 * - R16_FLOAT
 * - R16G16_FLOAT
 * - R16G16B16A16_FLOAT
 * - R32_FLOAT
 * - R32G32_FLOAT
 * - R32G32B32A32_FLOAT
 * - R11G11B10_UFLOAT
 * - R8G8B8A8_UNORM_SRGB
 * - B8G8R8A8_UNORM_SRGB
 * - D16_UNORM
 *
 * For COLOR_TARGET usage, the following formats are universally supported:
 *
 * - R8G8B8A8_UNORM
 * - B8G8R8A8_UNORM
 * - R8_UNORM
 * - R16_FLOAT
 * - R16G16_FLOAT
 * - R16G16B16A16_FLOAT
 * - R32_FLOAT
 * - R32G32_FLOAT
 * - R32G32B32A32_FLOAT
 * - R8_UINT
 * - R8G8_UINT
 * - R8G8B8A8_UINT
 * - R16_UINT
 * - R16G16_UINT
 * - R16G16B16A16_UINT
 * - R8_INT
 * - R8G8_INT
 * - R8G8B8A8_INT
 * - R16_INT
 * - R16G16_INT
 * - R16G16B16A16_INT
 * - R8G8B8A8_UNORM_SRGB
 * - B8G8R8A8_UNORM_SRGB
 *
 * For STORAGE usages, the following formats are universally supported:
 *
 * - R8G8B8A8_UNORM
 * - R8G8B8A8_SNORM
 * - R16G16B16A16_FLOAT
 * - R32_FLOAT
 * - R32G32_FLOAT
 * - R32G32B32A32_FLOAT
 * - R8G8B8A8_UINT
 * - R16G16B16A16_UINT
 * - R8G8B8A8_INT
 * - R16G16B16A16_INT
 *
 * For DEPTH_STENCIL_TARGET usage, the following formats are universally
 * supported:
 *
 * - D16_UNORM
 * - Either (but not necessarily both!) D24_UNORM or D32_SFLOAT
 * - Either (but not necessarily both!) D24_UNORM_S8_UINT or
 *   D32_SFLOAT_S8_UINT
 *
 * Unless D16_UNORM is sufficient for your purposes, always check which of
 * D24/D32 is supported before creating a depth-stencil texture!
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUTexture
 * \sa SDL_GPUTextureSupportsFormat
  }
{ Unsigned Normalized Float Color Formats  }
{ Compressed Unsigned Normalized Float Color Formats  }
{ Compressed Signed Float Color Formats  }
{ Compressed Unsigned Float Color Formats  }
{ Signed Normalized Float Color Formats   }
{ Signed Float Color Formats  }
{ Unsigned Float Color Formats  }
{ Unsigned Integer Color Formats  }
{ Signed Integer Color Formats  }
{ SRGB Unsigned Normalized Color Formats  }
{ Compressed SRGB Unsigned Normalized Color Formats  }
{ Depth Formats  }
{ Compressed ASTC Normalized Float Color Formats }
{ Compressed SRGB ASTC Normalized Float Color Formats }
{ Compressed ASTC Signed Float Color Formats }
type
  PSDL_GPUTextureFormat = ^TSDL_GPUTextureFormat;
  TSDL_GPUTextureFormat =  Longint;
  Const
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
    SDL_GPU_TEXTUREFORMAT_ASTC_4x4_UNORM = 63;
    SDL_GPU_TEXTUREFORMAT_ASTC_5x4_UNORM = 64;
    SDL_GPU_TEXTUREFORMAT_ASTC_5x5_UNORM = 65;
    SDL_GPU_TEXTUREFORMAT_ASTC_6x5_UNORM = 66;
    SDL_GPU_TEXTUREFORMAT_ASTC_6x6_UNORM = 67;
    SDL_GPU_TEXTUREFORMAT_ASTC_8x5_UNORM = 68;
    SDL_GPU_TEXTUREFORMAT_ASTC_8x6_UNORM = 69;
    SDL_GPU_TEXTUREFORMAT_ASTC_8x8_UNORM = 70;
    SDL_GPU_TEXTUREFORMAT_ASTC_10x5_UNORM = 71;
    SDL_GPU_TEXTUREFORMAT_ASTC_10x6_UNORM = 72;
    SDL_GPU_TEXTUREFORMAT_ASTC_10x8_UNORM = 73;
    SDL_GPU_TEXTUREFORMAT_ASTC_10x10_UNORM = 74;
    SDL_GPU_TEXTUREFORMAT_ASTC_12x10_UNORM = 75;
    SDL_GPU_TEXTUREFORMAT_ASTC_12x12_UNORM = 76;
    SDL_GPU_TEXTUREFORMAT_ASTC_4x4_UNORM_SRGB = 77;
    SDL_GPU_TEXTUREFORMAT_ASTC_5x4_UNORM_SRGB = 78;
    SDL_GPU_TEXTUREFORMAT_ASTC_5x5_UNORM_SRGB = 79;
    SDL_GPU_TEXTUREFORMAT_ASTC_6x5_UNORM_SRGB = 80;
    SDL_GPU_TEXTUREFORMAT_ASTC_6x6_UNORM_SRGB = 81;
    SDL_GPU_TEXTUREFORMAT_ASTC_8x5_UNORM_SRGB = 82;
    SDL_GPU_TEXTUREFORMAT_ASTC_8x6_UNORM_SRGB = 83;
    SDL_GPU_TEXTUREFORMAT_ASTC_8x8_UNORM_SRGB = 84;
    SDL_GPU_TEXTUREFORMAT_ASTC_10x5_UNORM_SRGB = 85;
    SDL_GPU_TEXTUREFORMAT_ASTC_10x6_UNORM_SRGB = 86;
    SDL_GPU_TEXTUREFORMAT_ASTC_10x8_UNORM_SRGB = 87;
    SDL_GPU_TEXTUREFORMAT_ASTC_10x10_UNORM_SRGB = 88;
    SDL_GPU_TEXTUREFORMAT_ASTC_12x10_UNORM_SRGB = 89;
    SDL_GPU_TEXTUREFORMAT_ASTC_12x12_UNORM_SRGB = 90;
    SDL_GPU_TEXTUREFORMAT_ASTC_4x4_FLOAT = 91;
    SDL_GPU_TEXTUREFORMAT_ASTC_5x4_FLOAT = 92;
    SDL_GPU_TEXTUREFORMAT_ASTC_5x5_FLOAT = 93;
    SDL_GPU_TEXTUREFORMAT_ASTC_6x5_FLOAT = 94;
    SDL_GPU_TEXTUREFORMAT_ASTC_6x6_FLOAT = 95;
    SDL_GPU_TEXTUREFORMAT_ASTC_8x5_FLOAT = 96;
    SDL_GPU_TEXTUREFORMAT_ASTC_8x6_FLOAT = 97;
    SDL_GPU_TEXTUREFORMAT_ASTC_8x8_FLOAT = 98;
    SDL_GPU_TEXTUREFORMAT_ASTC_10x5_FLOAT = 99;
    SDL_GPU_TEXTUREFORMAT_ASTC_10x6_FLOAT = 100;
    SDL_GPU_TEXTUREFORMAT_ASTC_10x8_FLOAT = 101;
    SDL_GPU_TEXTUREFORMAT_ASTC_10x10_FLOAT = 102;
    SDL_GPU_TEXTUREFORMAT_ASTC_12x10_FLOAT = 103;
    SDL_GPU_TEXTUREFORMAT_ASTC_12x12_FLOAT = 104;
;
{*
 * Specifies how a texture is intended to be used by the client.
 *
 * A texture must have at least one usage flag. Note that some usage flag
 * combinations are invalid.
 *
 * With regards to compute storage usage, READ | WRITE means that you can have
 * shader A that only writes into the texture and shader B that only reads
 * from the texture and bind the same texture to either shader respectively.
 * SIMULTANEOUS means that you can do reads and writes within the same shader
 * or compute pass. It also implies that atomic ops can be used, since those
 * are read-modify-write operations. If you use SIMULTANEOUS, you are
 * responsible for avoiding data races, as there is no data synchronization
 * within a compute pass. Note that SIMULTANEOUS usage is only supported by a
 * limited number of texture formats.
 *
 * \since This datatype is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUTexture
  }
type
  PSDL_GPUTextureUsageFlags = ^TSDL_GPUTextureUsageFlags;
  TSDL_GPUTextureUsageFlags = TUint32;
{*< Texture supports sampling.  }

const
  SDL_GPU_TEXTUREUSAGE_SAMPLER = 1 shl 0;  
{*< Texture is a color render target.  }
  SDL_GPU_TEXTUREUSAGE_COLOR_TARGET = 1 shl 1;  
{*< Texture is a depth stencil target.  }
  SDL_GPU_TEXTUREUSAGE_DEPTH_STENCIL_TARGET = 1 shl 2;  
{*< Texture supports storage reads in graphics stages.  }
  SDL_GPU_TEXTUREUSAGE_GRAPHICS_STORAGE_READ = 1 shl 3;  
{*< Texture supports storage reads in the compute stage.  }
  SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_READ = 1 shl 4;  
{*< Texture supports storage writes in the compute stage.  }
  SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_WRITE = 1 shl 5;  
{*< Texture supports reads and writes in the same compute shader. This is NOT equivalent to READ | WRITE.  }
  SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_SIMULTANEOUS_READ_WRITE = 1 shl 6;  
{*
 * Specifies the type of a texture.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUTexture
  }
{*< The texture is a 2-dimensional image.  }
{*< The texture is a 2-dimensional array image.  }
{*< The texture is a 3-dimensional image.  }
{*< The texture is a cube image.  }
{*< The texture is a cube array image.  }
type
  PSDL_GPUTextureType = ^TSDL_GPUTextureType;
  TSDL_GPUTextureType =  Longint;
  Const
    SDL_GPU_TEXTURETYPE_2D = 0;
    SDL_GPU_TEXTURETYPE_2D_ARRAY = 1;
    SDL_GPU_TEXTURETYPE_3D = 2;
    SDL_GPU_TEXTURETYPE_CUBE = 3;
    SDL_GPU_TEXTURETYPE_CUBE_ARRAY = 4;
;
{*
 * Specifies the sample count of a texture.
 *
 * Used in multisampling. Note that this value only applies when the texture
 * is used as a render target.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUTexture
 * \sa SDL_GPUTextureSupportsSampleCount
  }
{*< No multisampling.  }
{*< MSAA 2x  }
{*< MSAA 4x  }
{*< MSAA 8x  }
type
  PSDL_GPUSampleCount = ^TSDL_GPUSampleCount;
  TSDL_GPUSampleCount =  Longint;
  Const
    SDL_GPU_SAMPLECOUNT_1 = 0;
    SDL_GPU_SAMPLECOUNT_2 = 1;
    SDL_GPU_SAMPLECOUNT_4 = 2;
    SDL_GPU_SAMPLECOUNT_8 = 3;
;
{*
 * Specifies the face of a cube map.
 *
 * Can be passed in as the layer field in texture-related structs.
 *
 * \since This enum is available since SDL 3.1.3
  }
type
  PSDL_GPUCubeMapFace = ^TSDL_GPUCubeMapFace;
  TSDL_GPUCubeMapFace =  Longint;
  Const
    SDL_GPU_CUBEMAPFACE_POSITIVEX = 0;
    SDL_GPU_CUBEMAPFACE_NEGATIVEX = 1;
    SDL_GPU_CUBEMAPFACE_POSITIVEY = 2;
    SDL_GPU_CUBEMAPFACE_NEGATIVEY = 3;
    SDL_GPU_CUBEMAPFACE_POSITIVEZ = 4;
    SDL_GPU_CUBEMAPFACE_NEGATIVEZ = 5;
;
{*
 * Specifies how a buffer is intended to be used by the client.
 *
 * A buffer must have at least one usage flag. Note that some usage flag
 * combinations are invalid.
 *
 * Unlike textures, READ | WRITE can be used for simultaneous read-write
 * usage. The same data synchronization concerns as textures apply.
 *
 * \since This datatype is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUBuffer
  }
type
  PSDL_GPUBufferUsageFlags = ^TSDL_GPUBufferUsageFlags;
  TSDL_GPUBufferUsageFlags = TUint32;
{*< Buffer is a vertex buffer.  }

const
  SDL_GPU_BUFFERUSAGE_VERTEX = 1 shl 0;  
{*< Buffer is an index buffer.  }
  SDL_GPU_BUFFERUSAGE_INDEX = 1 shl 1;  
{*< Buffer is an indirect buffer.  }
  SDL_GPU_BUFFERUSAGE_INDIRECT = 1 shl 2;  
{*< Buffer supports storage reads in graphics stages.  }
  SDL_GPU_BUFFERUSAGE_GRAPHICS_STORAGE_READ = 1 shl 3;  
{*< Buffer supports storage reads in the compute stage.  }
  SDL_GPU_BUFFERUSAGE_COMPUTE_STORAGE_READ = 1 shl 4;  
{*< Buffer supports storage writes in the compute stage.  }
  SDL_GPU_BUFFERUSAGE_COMPUTE_STORAGE_WRITE = 1 shl 5;  
{*
 * Specifies how a transfer buffer is intended to be used by the client.
 *
 * Note that mapping and copying FROM an upload transfer buffer or TO a
 * download transfer buffer is undefined behavior.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUTransferBuffer
  }
type
  PSDL_GPUTransferBufferUsage = ^TSDL_GPUTransferBufferUsage;
  TSDL_GPUTransferBufferUsage =  Longint;
  Const
    SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD = 0;
    SDL_GPU_TRANSFERBUFFERUSAGE_DOWNLOAD = 1;
;
{*
 * Specifies which stage a shader program corresponds to.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUShader
  }
type
  PSDL_GPUShaderStage = ^TSDL_GPUShaderStage;
  TSDL_GPUShaderStage =  Longint;
  Const
    SDL_GPU_SHADERSTAGE_VERTEX = 0;
    SDL_GPU_SHADERSTAGE_FRAGMENT = 1;
;
{*
 * Specifies the format of shader code.
 *
 * Each format corresponds to a specific backend that accepts it.
 *
 * \since This datatype is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUShader
  }
type
  PSDL_GPUShaderFormat = ^TSDL_GPUShaderFormat;
  TSDL_GPUShaderFormat = TUint32;

const
  SDL_GPU_SHADERFORMAT_INVALID = 0;  
{*< Shaders for NDA'd platforms.  }
  SDL_GPU_SHADERFORMAT_PRIVATE = 1 shl 0;  
{*< SPIR-V shaders for Vulkan.  }
  SDL_GPU_SHADERFORMAT_SPIRV = 1 shl 1;  
{*< DXBC SM5_1 shaders for D3D12.  }
  SDL_GPU_SHADERFORMAT_DXBC = 1 shl 2;  
{*< DXIL SM6_0 shaders for D3D12.  }
  SDL_GPU_SHADERFORMAT_DXIL = 1 shl 3;  
{*< MSL shaders for Metal.  }
  SDL_GPU_SHADERFORMAT_MSL = 1 shl 4;  
{*< Precompiled metallib shaders for Metal.  }
  SDL_GPU_SHADERFORMAT_METALLIB = 1 shl 5;  
{*
 * Specifies the format of a vertex attribute.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUGraphicsPipeline
  }
{ 32-bit Signed Integers  }
{ 32-bit Unsigned Integers  }
{ 32-bit Floats  }
{ 8-bit Signed Integers  }
{ 8-bit Unsigned Integers  }
{ 8-bit Signed Normalized  }
{ 8-bit Unsigned Normalized  }
{ 16-bit Signed Integers  }
{ 16-bit Unsigned Integers  }
{ 16-bit Signed Normalized  }
{ 16-bit Unsigned Normalized  }
{ 16-bit Floats  }
type
  PSDL_GPUVertexElementFormat = ^TSDL_GPUVertexElementFormat;
  TSDL_GPUVertexElementFormat =  Longint;
  Const
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
;
{*
 * Specifies the rate at which vertex attributes are pulled from buffers.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUGraphicsPipeline
  }
{*< Attribute addressing is a function of the vertex index.  }
{*< Attribute addressing is a function of the instance index.  }
type
  PSDL_GPUVertexInputRate = ^TSDL_GPUVertexInputRate;
  TSDL_GPUVertexInputRate =  Longint;
  Const
    SDL_GPU_VERTEXINPUTRATE_VERTEX = 0;
    SDL_GPU_VERTEXINPUTRATE_INSTANCE = 1;
;
{*
 * Specifies the fill mode of the graphics pipeline.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUGraphicsPipeline
  }
{*< Polygons will be rendered via rasterization.  }
{*< Polygon edges will be drawn as line segments.  }
type
  PSDL_GPUFillMode = ^TSDL_GPUFillMode;
  TSDL_GPUFillMode =  Longint;
  Const
    SDL_GPU_FILLMODE_FILL = 0;
    SDL_GPU_FILLMODE_LINE = 1;
;
{*
 * Specifies the facing direction in which triangle faces will be culled.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUGraphicsPipeline
  }
{*< No triangles are culled.  }
{*< Front-facing triangles are culled.  }
{*< Back-facing triangles are culled.  }
type
  PSDL_GPUCullMode = ^TSDL_GPUCullMode;
  TSDL_GPUCullMode =  Longint;
  Const
    SDL_GPU_CULLMODE_NONE = 0;
    SDL_GPU_CULLMODE_FRONT = 1;
    SDL_GPU_CULLMODE_BACK = 2;
;
{*
 * Specifies the vertex winding that will cause a triangle to be determined to
 * be front-facing.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUGraphicsPipeline
  }
{*< A triangle with counter-clockwise vertex winding will be considered front-facing.  }
{*< A triangle with clockwise vertex winding will be considered front-facing.  }
type
  PSDL_GPUFrontFace = ^TSDL_GPUFrontFace;
  TSDL_GPUFrontFace =  Longint;
  Const
    SDL_GPU_FRONTFACE_COUNTER_CLOCKWISE = 0;
    SDL_GPU_FRONTFACE_CLOCKWISE = 1;
;
{*
 * Specifies a comparison operator for depth, stencil and sampler operations.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUGraphicsPipeline
  }
{*< The comparison always evaluates false.  }
{*< The comparison evaluates reference < test.  }
{*< The comparison evaluates reference == test.  }
{*< The comparison evaluates reference <= test.  }
{*< The comparison evaluates reference > test.  }
{*< The comparison evaluates reference != test.  }
{*< The comparison evalutes reference >= test.  }
{*< The comparison always evaluates true.  }
type
  PSDL_GPUCompareOp = ^TSDL_GPUCompareOp;
  TSDL_GPUCompareOp =  Longint;
  Const
    SDL_GPU_COMPAREOP_INVALID = 0;
    SDL_GPU_COMPAREOP_NEVER = 1;
    SDL_GPU_COMPAREOP_LESS = 2;
    SDL_GPU_COMPAREOP_EQUAL = 3;
    SDL_GPU_COMPAREOP_LESS_OR_EQUAL = 4;
    SDL_GPU_COMPAREOP_GREATER = 5;
    SDL_GPU_COMPAREOP_NOT_EQUAL = 6;
    SDL_GPU_COMPAREOP_GREATER_OR_EQUAL = 7;
    SDL_GPU_COMPAREOP_ALWAYS = 8;
;
{*
 * Specifies what happens to a stored stencil value if stencil tests fail or
 * pass.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUGraphicsPipeline
  }
{*< Keeps the current value.  }
{*< Sets the value to 0.  }
{*< Sets the value to reference.  }
{*< Increments the current value and clamps to the maximum value.  }
{*< Decrements the current value and clamps to 0.  }
{*< Bitwise-inverts the current value.  }
{*< Increments the current value and wraps back to 0.  }
{*< Decrements the current value and wraps to the maximum value.  }
type
  PSDL_GPUStencilOp = ^TSDL_GPUStencilOp;
  TSDL_GPUStencilOp =  Longint;
  Const
    SDL_GPU_STENCILOP_INVALID = 0;
    SDL_GPU_STENCILOP_KEEP = 1;
    SDL_GPU_STENCILOP_ZERO = 2;
    SDL_GPU_STENCILOP_REPLACE = 3;
    SDL_GPU_STENCILOP_INCREMENT_AND_CLAMP = 4;
    SDL_GPU_STENCILOP_DECREMENT_AND_CLAMP = 5;
    SDL_GPU_STENCILOP_INVERT = 6;
    SDL_GPU_STENCILOP_INCREMENT_AND_WRAP = 7;
    SDL_GPU_STENCILOP_DECREMENT_AND_WRAP = 8;
;
{*
 * Specifies the operator to be used when pixels in a render target are
 * blended with existing pixels in the texture.
 *
 * The source color is the value written by the fragment shader. The
 * destination color is the value currently existing in the texture.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUGraphicsPipeline
  }
{*< (source * source_factor) + (destination * destination_factor)  }
{*< (source * source_factor) - (destination * destination_factor)  }
{*< (destination * destination_factor) - (source * source_factor)  }
{*< min(source, destination)  }
{*< max(source, destination)  }
type
  PSDL_GPUBlendOp = ^TSDL_GPUBlendOp;
  TSDL_GPUBlendOp =  Longint;
  Const
    SDL_GPU_BLENDOP_INVALID = 0;
    SDL_GPU_BLENDOP_ADD = 1;
    SDL_GPU_BLENDOP_SUBTRACT = 2;
    SDL_GPU_BLENDOP_REVERSE_SUBTRACT = 3;
    SDL_GPU_BLENDOP_MIN = 4;
    SDL_GPU_BLENDOP_MAX = 5;
;
{*
 * Specifies a blending factor to be used when pixels in a render target are
 * blended with existing pixels in the texture.
 *
 * The source color is the value written by the fragment shader. The
 * destination color is the value currently existing in the texture.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUGraphicsPipeline
  }
{*< 0  }
{*< 1  }
{*< source color  }
{*< 1 - source color  }
{*< destination color  }
{*< 1 - destination color  }
{*< source alpha  }
{*< 1 - source alpha  }
{*< destination alpha  }
{*< 1 - destination alpha  }
{*< blend constant  }
{*< 1 - blend constant  }
{*< min(source alpha, 1 - destination alpha)  }
type
  PSDL_GPUBlendFactor = ^TSDL_GPUBlendFactor;
  TSDL_GPUBlendFactor =  Longint;
  Const
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
;
{*
 * Specifies which color components are written in a graphics pipeline.
 *
 * \since This datatype is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUGraphicsPipeline
  }
type
  PSDL_GPUColorComponentFlags = ^TSDL_GPUColorComponentFlags;
  TSDL_GPUColorComponentFlags = TUint8;
{*< the red component  }

const
  SDL_GPU_COLORCOMPONENT_R = 1 shl 0;  
{*< the green component  }
  SDL_GPU_COLORCOMPONENT_G = 1 shl 1;  
{*< the blue component  }
  SDL_GPU_COLORCOMPONENT_B = 1 shl 2;  
{*< the alpha component  }
  SDL_GPU_COLORCOMPONENT_A = 1 shl 3;  
{*
 * Specifies a filter operation used by a sampler.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUSampler
  }
{*< Point filtering.  }
{*< Linear filtering.  }
type
  PSDL_GPUFilter = ^TSDL_GPUFilter;
  TSDL_GPUFilter =  Longint;
  Const
    SDL_GPU_FILTER_NEAREST = 0;
    SDL_GPU_FILTER_LINEAR = 1;
;
{*
 * Specifies a mipmap mode used by a sampler.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUSampler
  }
{*< Point filtering.  }
{*< Linear filtering.  }
type
  PSDL_GPUSamplerMipmapMode = ^TSDL_GPUSamplerMipmapMode;
  TSDL_GPUSamplerMipmapMode =  Longint;
  Const
    SDL_GPU_SAMPLERMIPMAPMODE_NEAREST = 0;
    SDL_GPU_SAMPLERMIPMAPMODE_LINEAR = 1;
;
{*
 * Specifies behavior of texture sampling when the coordinates exceed the 0-1
 * range.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUSampler
  }
{*< Specifies that the coordinates will wrap around.  }
{*< Specifies that the coordinates will wrap around mirrored.  }
{*< Specifies that the coordinates will clamp to the 0-1 range.  }
type
  PSDL_GPUSamplerAddressMode = ^TSDL_GPUSamplerAddressMode;
  TSDL_GPUSamplerAddressMode =  Longint;
  Const
    SDL_GPU_SAMPLERADDRESSMODE_REPEAT = 0;
    SDL_GPU_SAMPLERADDRESSMODE_MIRRORED_REPEAT = 1;
    SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE = 2;
;
{*
 * Specifies the timing that will be used to present swapchain textures to the
 * OS.
 *
 * VSYNC mode will always be supported. IMMEDIATE and MAILBOX modes may not be
 * supported on certain systems.
 *
 * It is recommended to query SDL_WindowSupportsGPUPresentMode after claiming
 * the window if you wish to change the present mode to IMMEDIATE or MAILBOX.
 *
 * - VSYNC: Waits for vblank before presenting. No tearing is possible. If
 *   there is a pending image to present, the new image is enqueued for
 *   presentation. Disallows tearing at the cost of visual latency.
 * - IMMEDIATE: Immediately presents. Lowest latency option, but tearing may
 *   occur.
 * - MAILBOX: Waits for vblank before presenting. No tearing is possible. If
 *   there is a pending image to present, the pending image is replaced by the
 *   new image. Similar to VSYNC, but with reduced visual latency.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_SetGPUSwapchainParameters
 * \sa SDL_WindowSupportsGPUPresentMode
 * \sa SDL_WaitAndAcquireGPUSwapchainTexture
  }
type
  PSDL_GPUPresentMode = ^TSDL_GPUPresentMode;
  TSDL_GPUPresentMode =  Longint;
  Const
    SDL_GPU_PRESENTMODE_VSYNC = 0;
    SDL_GPU_PRESENTMODE_IMMEDIATE = 1;
    SDL_GPU_PRESENTMODE_MAILBOX = 2;
;
{*
 * Specifies the texture format and colorspace of the swapchain textures.
 *
 * SDR will always be supported. Other compositions may not be supported on
 * certain systems.
 *
 * It is recommended to query SDL_WindowSupportsGPUSwapchainComposition after
 * claiming the window if you wish to change the swapchain composition from
 * SDR.
 *
 * - SDR: B8G8R8A8 or R8G8B8A8 swapchain. Pixel values are in sRGB encoding.
 * - SDR_LINEAR: B8G8R8A8_SRGB or R8G8B8A8_SRGB swapchain. Pixel values are
 *   stored in memory in sRGB encoding but accessed in shaders in "linear
 *   sRGB" encoding which is sRGB but with a linear transfer function.
 * - HDR_EXTENDED_LINEAR: R16G16B16A16_SFLOAT swapchain. Pixel values are in
 *   extended linear sRGB encoding and permits values outside of the [0, 1]
 *   range.
 * - HDR10_ST2084: A2R10G10B10 or A2B10G10R10 swapchain. Pixel values are in
 *   BT.2020 ST2084 (PQ) encoding.
 *
 * \since This enum is available since SDL 3.1.3
 *
 * \sa SDL_SetGPUSwapchainParameters
 * \sa SDL_WindowSupportsGPUSwapchainComposition
 * \sa SDL_WaitAndAcquireGPUSwapchainTexture
  }
type
  PSDL_GPUSwapchainComposition = ^TSDL_GPUSwapchainComposition;
  TSDL_GPUSwapchainComposition =  Longint;
  Const
    SDL_GPU_SWAPCHAINCOMPOSITION_SDR = 0;
    SDL_GPU_SWAPCHAINCOMPOSITION_SDR_LINEAR = 1;
    SDL_GPU_SWAPCHAINCOMPOSITION_HDR_EXTENDED_LINEAR = 2;
    SDL_GPU_SWAPCHAINCOMPOSITION_HDR10_ST2084 = 3;
;
{ Structures  }
{*
 * A structure specifying a viewport.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_SetGPUViewport
  }
{*< The left offset of the viewport.  }
{*< The top offset of the viewport.  }
{*< The width of the viewport.  }
{*< The height of the viewport.  }
{*< The minimum depth of the viewport.  }
{*< The maximum depth of the viewport.  }
type
  PSDL_GPUViewport = ^TSDL_GPUViewport;
  TSDL_GPUViewport = record
      x : single;
      y : single;
      w : single;
      h : single;
      min_depth : single;
      max_depth : single;
    end;
{*
 * A structure specifying parameters related to transferring data to or from a
 * texture.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_UploadToGPUTexture
 * \sa SDL_DownloadFromGPUTexture
  }
{*< The transfer buffer used in the transfer operation.  }
{*< The starting byte of the image data in the transfer buffer.  }
{*< The number of pixels from one row to the next.  }
{*< The number of rows from one layer/depth-slice to the next.  }

  PSDL_GPUTextureTransferInfo = ^TSDL_GPUTextureTransferInfo;
  TSDL_GPUTextureTransferInfo = record
      transfer_buffer : PSDL_GPUTransferBuffer;
      offset : TUint32;
      pixels_per_row : TUint32;
      rows_per_layer : TUint32;
    end;
{*
 * A structure specifying a location in a transfer buffer.
 *
 * Used when transferring buffer data to or from a transfer buffer.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_UploadToGPUBuffer
 * \sa SDL_DownloadFromGPUBuffer
  }
{*< The transfer buffer used in the transfer operation.  }
{*< The starting byte of the buffer data in the transfer buffer.  }

  PSDL_GPUTransferBufferLocation = ^TSDL_GPUTransferBufferLocation;
  TSDL_GPUTransferBufferLocation = record
      transfer_buffer : PSDL_GPUTransferBuffer;
      offset : TUint32;
    end;
{*
 * A structure specifying a location in a texture.
 *
 * Used when copying data from one texture to another.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_CopyGPUTextureToTexture
  }
{*< The texture used in the copy operation.  }
{*< The mip level index of the location.  }
{*< The layer index of the location.  }
{*< The left offset of the location.  }
{*< The top offset of the location.  }
{*< The front offset of the location.  }

  PSDL_GPUTextureLocation = ^TSDL_GPUTextureLocation;
  TSDL_GPUTextureLocation = record
      texture : PSDL_GPUTexture;
      mip_level : TUint32;
      layer : TUint32;
      x : TUint32;
      y : TUint32;
      z : TUint32;
    end;
{*
 * A structure specifying a region of a texture.
 *
 * Used when transferring data to or from a texture.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_UploadToGPUTexture
 * \sa SDL_DownloadFromGPUTexture
  }
{*< The texture used in the copy operation.  }
{*< The mip level index to transfer.  }
{*< The layer index to transfer.  }
{*< The left offset of the region.  }
{*< The top offset of the region.  }
{*< The front offset of the region.  }
{*< The width of the region.  }
{*< The height of the region.  }
{*< The depth of the region.  }

  PSDL_GPUTextureRegion = ^TSDL_GPUTextureRegion;
  TSDL_GPUTextureRegion = record
      texture : PSDL_GPUTexture;
      mip_level : TUint32;
      layer : TUint32;
      x : TUint32;
      y : TUint32;
      z : TUint32;
      w : TUint32;
      h : TUint32;
      d : TUint32;
    end;
{*
 * A structure specifying a region of a texture used in the blit operation.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_BlitGPUTexture
  }
{*< The texture.  }
{*< The mip level index of the region.  }
{*< The layer index or depth plane of the region. This value is treated as a layer index on 2D array and cube textures, and as a depth plane on 3D textures.  }
{*< The left offset of the region.  }
{*< The top offset of the region.   }
{*< The width of the region.  }
{*< The height of the region.  }

  PSDL_GPUBlitRegion = ^TSDL_GPUBlitRegion;
  TSDL_GPUBlitRegion = record
      texture : PSDL_GPUTexture;
      mip_level : TUint32;
      layer_or_depth_plane : TUint32;
      x : TUint32;
      y : TUint32;
      w : TUint32;
      h : TUint32;
    end;
{*
 * A structure specifying a location in a buffer.
 *
 * Used when copying data between buffers.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_CopyGPUBufferToBuffer
  }
{*< The buffer.  }
{*< The starting byte within the buffer.  }

  PSDL_GPUBufferLocation = ^TSDL_GPUBufferLocation;
  TSDL_GPUBufferLocation = record
      buffer : PSDL_GPUBuffer;
      offset : TUint32;
    end;
{*
 * A structure specifying a region of a buffer.
 *
 * Used when transferring data to or from buffers.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_UploadToGPUBuffer
 * \sa SDL_DownloadFromGPUBuffer
  }
{*< The buffer.  }
{*< The starting byte within the buffer.  }
{*< The size in bytes of the region.  }

  PSDL_GPUBufferRegion = ^TSDL_GPUBufferRegion;
  TSDL_GPUBufferRegion = record
      buffer : PSDL_GPUBuffer;
      offset : TUint32;
      size : TUint32;
    end;
{*
 * A structure specifying the parameters of an indirect draw command.
 *
 * Note that the `first_vertex` and `first_instance` parameters are NOT
 * compatible with built-in vertex/instance ID variables in shaders (for
 * example, SV_VertexID); GPU APIs and shader languages do not define these
 * built-in variables consistently, so if your shader depends on them, the
 * only way to keep behavior consistent and portable is to always pass 0 for
 * the correlating parameter in the draw calls.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_DrawGPUPrimitivesIndirect
  }
{*< The number of vertices to draw.  }
{*< The number of instances to draw.  }
{*< The index of the first vertex to draw.  }
{*< The ID of the first instance to draw.  }

  PSDL_GPUIndirectDrawCommand = ^TSDL_GPUIndirectDrawCommand;
  TSDL_GPUIndirectDrawCommand = record
      num_vertices : TUint32;
      num_instances : TUint32;
      first_vertex : TUint32;
      first_instance : TUint32;
    end;
{*
 * A structure specifying the parameters of an indexed indirect draw command.
 *
 * Note that the `first_vertex` and `first_instance` parameters are NOT
 * compatible with built-in vertex/instance ID variables in shaders (for
 * example, SV_VertexID); GPU APIs and shader languages do not define these
 * built-in variables consistently, so if your shader depends on them, the
 * only way to keep behavior consistent and portable is to always pass 0 for
 * the correlating parameter in the draw calls.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_DrawGPUIndexedPrimitivesIndirect
  }
{*< The number of indices to draw per instance.  }
{*< The number of instances to draw.  }
{*< The base index within the index buffer.  }
{*< The value added to the vertex index before indexing into the vertex buffer.  }
{*< The ID of the first instance to draw.  }

  PSDL_GPUIndexedIndirectDrawCommand = ^TSDL_GPUIndexedIndirectDrawCommand;
  TSDL_GPUIndexedIndirectDrawCommand = record
      num_indices : TUint32;
      num_instances : TUint32;
      first_index : TUint32;
      vertex_offset : TSint32;
      first_instance : TUint32;
    end;
{*
 * A structure specifying the parameters of an indexed dispatch command.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_DispatchGPUComputeIndirect
  }
{*< The number of local workgroups to dispatch in the X dimension.  }
{*< The number of local workgroups to dispatch in the Y dimension.  }
{*< The number of local workgroups to dispatch in the Z dimension.  }

  PSDL_GPUIndirectDispatchCommand = ^TSDL_GPUIndirectDispatchCommand;
  TSDL_GPUIndirectDispatchCommand = record
      groupcount_x : TUint32;
      groupcount_y : TUint32;
      groupcount_z : TUint32;
    end;
{ State structures  }
{*
 * A structure specifying the parameters of a sampler.
 *
 * \since This function is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUSampler
  }
{*< The minification filter to apply to lookups.  }
{*< The magnification filter to apply to lookups.  }
{*< The mipmap filter to apply to lookups.  }
{*< The addressing mode for U coordinates outside [0, 1).  }
{*< The addressing mode for V coordinates outside [0, 1).  }
{*< The addressing mode for W coordinates outside [0, 1).  }
{*< The bias to be added to mipmap LOD calculation.  }
{*< The anisotropy value clamp used by the sampler. If enable_anisotropy is false, this is ignored.  }
{*< The comparison operator to apply to fetched data before filtering.  }
{*< Clamps the minimum of the computed LOD value.  }
{*< Clamps the maximum of the computed LOD value.  }
{*< true to enable anisotropic filtering.  }
{*< true to enable comparison against a reference value during lookups.  }
{*< A properties ID for extensions. Should be 0 if no extensions are needed.  }

  PSDL_GPUSamplerCreateInfo = ^TSDL_GPUSamplerCreateInfo;
  TSDL_GPUSamplerCreateInfo = record
      min_filter : TSDL_GPUFilter;
      mag_filter : TSDL_GPUFilter;
      mipmap_mode : TSDL_GPUSamplerMipmapMode;
      address_mode_u : TSDL_GPUSamplerAddressMode;
      address_mode_v : TSDL_GPUSamplerAddressMode;
      address_mode_w : TSDL_GPUSamplerAddressMode;
      mip_lod_bias : single;
      max_anisotropy : single;
      compare_op : TSDL_GPUCompareOp;
      min_lod : single;
      max_lod : single;
      enable_anisotropy : Tbool;
      enable_compare : Tbool;
      padding1 : TUint8;
      padding2 : TUint8;
      props : TSDL_PropertiesID;
    end;
{*
 * A structure specifying the parameters of vertex buffers used in a graphics
 * pipeline.
 *
 * When you call SDL_BindGPUVertexBuffers, you specify the binding slots of
 * the vertex buffers. For example if you called SDL_BindGPUVertexBuffers with
 * a first_slot of 2 and num_bindings of 3, the binding slots 2, 3, 4 would be
 * used by the vertex buffers you pass in.
 *
 * Vertex attributes are linked to buffers via the buffer_slot field of
 * SDL_GPUVertexAttribute. For example, if an attribute has a buffer_slot of
 * 0, then that attribute belongs to the vertex buffer bound at slot 0.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_GPUVertexAttribute
 * \sa SDL_GPUVertexInputState
  }
{*< The binding slot of the vertex buffer.  }
{*< The byte pitch between consecutive elements of the vertex buffer.  }
{*< Whether attribute addressing is a function of the vertex index or instance index.  }
{*< The number of instances to draw using the same per-instance data before advancing in the instance buffer by one element. Ignored unless input_rate is SDL_GPU_VERTEXINPUTRATE_INSTANCE  }

  PSDL_GPUVertexBufferDescription = ^TSDL_GPUVertexBufferDescription;
  TSDL_GPUVertexBufferDescription = record
      slot : TUint32;
      pitch : TUint32;
      input_rate : TSDL_GPUVertexInputRate;
      instance_step_rate : TUint32;
    end;
{*
 * A structure specifying a vertex attribute.
 *
 * All vertex attribute locations provided to an SDL_GPUVertexInputState must
 * be unique.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_GPUVertexBufferDescription
 * \sa SDL_GPUVertexInputState
  }
{*< The shader input location index.  }
{*< The binding slot of the associated vertex buffer.  }
{*< The size and type of the attribute data.  }
{*< The byte offset of this attribute relative to the start of the vertex element.  }

  PSDL_GPUVertexAttribute = ^TSDL_GPUVertexAttribute;
  TSDL_GPUVertexAttribute = record
      location : TUint32;
      buffer_slot : TUint32;
      format : TSDL_GPUVertexElementFormat;
      offset : TUint32;
    end;
{*
 * A structure specifying the parameters of a graphics pipeline vertex input
 * state.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_GPUGraphicsPipelineCreateInfo
 * \sa SDL_GPUVertexBufferDescription
 * \sa SDL_GPUVertexAttribute
  }
{*< A pointer to an array of vertex buffer descriptions.  }
{*< The number of vertex buffer descriptions in the above array.  }
{*< A pointer to an array of vertex attribute descriptions.  }
{*< The number of vertex attribute descriptions in the above array.  }

  PSDL_GPUVertexInputState = ^TSDL_GPUVertexInputState;
  TSDL_GPUVertexInputState = record
      vertex_buffer_descriptions : PSDL_GPUVertexBufferDescription;
      num_vertex_buffers : TUint32;
      vertex_attributes : PSDL_GPUVertexAttribute;
      num_vertex_attributes : TUint32;
    end;
{*
 * A structure specifying the stencil operation state of a graphics pipeline.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_GPUDepthStencilState
  }
{*< The action performed on samples that fail the stencil test.  }
{*< The action performed on samples that pass the depth and stencil tests.  }
{*< The action performed on samples that pass the stencil test and fail the depth test.  }
{*< The comparison operator used in the stencil test.  }

  PSDL_GPUStencilOpState = ^TSDL_GPUStencilOpState;
  TSDL_GPUStencilOpState = record
      fail_op : TSDL_GPUStencilOp;
      pass_op : TSDL_GPUStencilOp;
      depth_fail_op : TSDL_GPUStencilOp;
      compare_op : TSDL_GPUCompareOp;
    end;
{*
 * A structure specifying the blend state of a color target.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_GPUColorTargetDescription
  }
{*< The value to be multiplied by the source RGB value.  }
{*< The value to be multiplied by the destination RGB value.  }
{*< The blend operation for the RGB components.  }
{*< The value to be multiplied by the source alpha.  }
{*< The value to be multiplied by the destination alpha.  }
{*< The blend operation for the alpha component.  }
{*< A bitmask specifying which of the RGBA components are enabled for writing. Writes to all channels if enable_color_write_mask is false.  }
{*< Whether blending is enabled for the color target.  }
{*< Whether the color write mask is enabled.  }

  PSDL_GPUColorTargetBlendState = ^TSDL_GPUColorTargetBlendState;
  TSDL_GPUColorTargetBlendState = record
      src_color_blendfactor : TSDL_GPUBlendFactor;
      dst_color_blendfactor : TSDL_GPUBlendFactor;
      color_blend_op : TSDL_GPUBlendOp;
      src_alpha_blendfactor : TSDL_GPUBlendFactor;
      dst_alpha_blendfactor : TSDL_GPUBlendFactor;
      alpha_blend_op : TSDL_GPUBlendOp;
      color_write_mask : TSDL_GPUColorComponentFlags;
      enable_blend : Tbool;
      enable_color_write_mask : Tbool;
      padding1 : TUint8;
      padding2 : TUint8;
    end;
{*
 * A structure specifying code and metadata for creating a shader object.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUShader
  }
{*< The size in bytes of the code pointed to.  }
{*< A pointer to shader code.  }
{*< A pointer to a null-terminated UTF-8 string specifying the entry point function name for the shader.  }
{*< The format of the shader code.  }
{*< The stage the shader program corresponds to.  }
{*< The number of samplers defined in the shader.  }
{*< The number of storage textures defined in the shader.  }
{*< The number of storage buffers defined in the shader.  }
{*< The number of uniform buffers defined in the shader.  }
{*< A properties ID for extensions. Should be 0 if no extensions are needed.  }

  PSDL_GPUShaderCreateInfo = ^TSDL_GPUShaderCreateInfo;
  TSDL_GPUShaderCreateInfo = record
      code_size : Tsize_t;
      code : PUint8;
      entrypoint : Pchar;
      format : TSDL_GPUShaderFormat;
      stage : TSDL_GPUShaderStage;
      num_samplers : TUint32;
      num_storage_textures : TUint32;
      num_storage_buffers : TUint32;
      num_uniform_buffers : TUint32;
      props : TSDL_PropertiesID;
    end;
{*
 * A structure specifying the parameters of a texture.
 *
 * Usage flags can be bitwise OR'd together for combinations of usages. Note
 * that certain usage combinations are invalid, for example SAMPLER and
 * GRAPHICS_STORAGE.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUTexture
 * \sa SDL_GPUTextureType
 * \sa SDL_GPUTextureFormat
 * \sa SDL_GPUTextureUsageFlags
 * \sa SDL_GPUSampleCount
  }
{*< The base dimensionality of the texture.  }
{*< The pixel format of the texture.  }
{*< How the texture is intended to be used by the client.  }
{*< The width of the texture.  }
{*< The height of the texture.  }
{*< The layer count or depth of the texture. This value is treated as a layer count on 2D array textures, and as a depth value on 3D textures.  }
{*< The number of mip levels in the texture.  }
{*< The number of samples per texel. Only applies if the texture is used as a render target.  }
{*< A properties ID for extensions. Should be 0 if no extensions are needed.  }

  PSDL_GPUTextureCreateInfo = ^TSDL_GPUTextureCreateInfo;
  TSDL_GPUTextureCreateInfo = record
      _type : TSDL_GPUTextureType;
      format : TSDL_GPUTextureFormat;
      usage : TSDL_GPUTextureUsageFlags;
      width : TUint32;
      height : TUint32;
      layer_count_or_depth : TUint32;
      num_levels : TUint32;
      sample_count : TSDL_GPUSampleCount;
      props : TSDL_PropertiesID;
    end;
{*
 * A structure specifying the parameters of a buffer.
 *
 * Usage flags can be bitwise OR'd together for combinations of usages. Note
 * that certain combinations are invalid, for example VERTEX and INDEX.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUBuffer
 * \sa SDL_GPUBufferUsageFlags
  }
{*< How the buffer is intended to be used by the client.  }
{*< The size in bytes of the buffer.  }
{*< A properties ID for extensions. Should be 0 if no extensions are needed.  }

  PSDL_GPUBufferCreateInfo = ^TSDL_GPUBufferCreateInfo;
  TSDL_GPUBufferCreateInfo = record
      usage : TSDL_GPUBufferUsageFlags;
      size : TUint32;
      props : TSDL_PropertiesID;
    end;
{*
 * A structure specifying the parameters of a transfer buffer.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUTransferBuffer
  }
{*< How the transfer buffer is intended to be used by the client.  }
{*< The size in bytes of the transfer buffer.  }
{*< A properties ID for extensions. Should be 0 if no extensions are needed.  }

  PSDL_GPUTransferBufferCreateInfo = ^TSDL_GPUTransferBufferCreateInfo;
  TSDL_GPUTransferBufferCreateInfo = record
      usage : TSDL_GPUTransferBufferUsage;
      size : TUint32;
      props : TSDL_PropertiesID;
    end;
{ Pipeline state structures  }
{*
 * A structure specifying the parameters of the graphics pipeline rasterizer
 * state.
 *
 * NOTE: Some backend APIs (D3D11/12) will enable depth clamping even if
 * enable_depth_clip is true. If you rely on this clamp+clip behavior,
 * consider enabling depth clip and then manually clamping depth in your
 * fragment shaders on Metal and Vulkan.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_GPUGraphicsPipelineCreateInfo
  }
{*< Whether polygons will be filled in or drawn as lines.  }
{*< The facing direction in which triangles will be culled.  }
{*< The vertex winding that will cause a triangle to be determined as front-facing.  }
{*< A scalar factor controlling the depth value added to each fragment.  }
{*< The maximum depth bias of a fragment.  }
{*< A scalar factor applied to a fragment's slope in depth calculations.  }
{*< true to bias fragment depth values.  }
{*< true to enable depth clip, false to enable depth clamp.  }

  PSDL_GPURasterizerState = ^TSDL_GPURasterizerState;
  TSDL_GPURasterizerState = record
      fill_mode : TSDL_GPUFillMode;
      cull_mode : TSDL_GPUCullMode;
      front_face : TSDL_GPUFrontFace;
      depth_bias_constant_factor : single;
      depth_bias_clamp : single;
      depth_bias_slope_factor : single;
      enable_depth_bias : Tbool;
      enable_depth_clip : Tbool;
      padding1 : TUint8;
      padding2 : TUint8;
    end;
{*
 * A structure specifying the parameters of the graphics pipeline multisample
 * state.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_GPUGraphicsPipelineCreateInfo
  }
{*< The number of samples to be used in rasterization.  }
{*< Determines which samples get updated in the render targets. Treated as 0xFFFFFFFF if enable_mask is false.  }
{*< Enables sample masking.  }

  PSDL_GPUMultisampleState = ^TSDL_GPUMultisampleState;
  TSDL_GPUMultisampleState = record
      sample_count : TSDL_GPUSampleCount;
      sample_mask : TUint32;
      enable_mask : Tbool;
      padding1 : TUint8;
      padding2 : TUint8;
      padding3 : TUint8;
    end;
{*
 * A structure specifying the parameters of the graphics pipeline depth
 * stencil state.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_GPUGraphicsPipelineCreateInfo
  }
{*< The comparison operator used for depth testing.  }
{*< The stencil op state for back-facing triangles.  }
{*< The stencil op state for front-facing triangles.  }
{*< Selects the bits of the stencil values participating in the stencil test.  }
{*< Selects the bits of the stencil values updated by the stencil test.  }
{*< true enables the depth test.  }
{*< true enables depth writes. Depth writes are always disabled when enable_depth_test is false.  }
{*< true enables the stencil test.  }

  PSDL_GPUDepthStencilState = ^TSDL_GPUDepthStencilState;
  TSDL_GPUDepthStencilState = record
      compare_op : TSDL_GPUCompareOp;
      back_stencil_state : TSDL_GPUStencilOpState;
      front_stencil_state : TSDL_GPUStencilOpState;
      compare_mask : TUint8;
      write_mask : TUint8;
      enable_depth_test : Tbool;
      enable_depth_write : Tbool;
      enable_stencil_test : Tbool;
      padding1 : TUint8;
      padding2 : TUint8;
      padding3 : TUint8;
    end;
{*
 * A structure specifying the parameters of color targets used in a graphics
 * pipeline.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_GPUGraphicsPipelineTargetInfo
  }
{*< The pixel format of the texture to be used as a color target.  }
{*< The blend state to be used for the color target.  }

  PSDL_GPUColorTargetDescription = ^TSDL_GPUColorTargetDescription;
  TSDL_GPUColorTargetDescription = record
      format : TSDL_GPUTextureFormat;
      blend_state : TSDL_GPUColorTargetBlendState;
    end;
{*
 * A structure specifying the descriptions of render targets used in a
 * graphics pipeline.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_GPUGraphicsPipelineCreateInfo
  }
{*< A pointer to an array of color target descriptions.  }
{*< The number of color target descriptions in the above array.  }
{*< The pixel format of the depth-stencil target. Ignored if has_depth_stencil_target is false.  }
{*< true specifies that the pipeline uses a depth-stencil target.  }

  PSDL_GPUGraphicsPipelineTargetInfo = ^TSDL_GPUGraphicsPipelineTargetInfo;
  TSDL_GPUGraphicsPipelineTargetInfo = record
      color_target_descriptions : PSDL_GPUColorTargetDescription;
      num_color_targets : TUint32;
      depth_stencil_format : TSDL_GPUTextureFormat;
      has_depth_stencil_target : Tbool;
      padding1 : TUint8;
      padding2 : TUint8;
      padding3 : TUint8;
    end;
{*
 * A structure specifying the parameters of a graphics pipeline state.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUGraphicsPipeline
 * \sa SDL_GPUVertexInputState
 * \sa SDL_GPUPrimitiveType
 * \sa SDL_GPURasterizerState
 * \sa SDL_GPUMultisampleState
 * \sa SDL_GPUDepthStencilState
 * \sa SDL_GPUGraphicsPipelineTargetInfo
  }
{*< The vertex shader used by the graphics pipeline.  }
{*< The fragment shader used by the graphics pipeline.  }
{*< The vertex layout of the graphics pipeline.  }
{*< The primitive topology of the graphics pipeline.  }
{*< The rasterizer state of the graphics pipeline.  }
{*< The multisample state of the graphics pipeline.  }
{*< The depth-stencil state of the graphics pipeline.  }
{*< Formats and blend modes for the render targets of the graphics pipeline.  }
{*< A properties ID for extensions. Should be 0 if no extensions are needed.  }

  PSDL_GPUGraphicsPipelineCreateInfo = ^TSDL_GPUGraphicsPipelineCreateInfo;
  TSDL_GPUGraphicsPipelineCreateInfo = record
      vertex_shader : PSDL_GPUShader;
      fragment_shader : PSDL_GPUShader;
      vertex_input_state : TSDL_GPUVertexInputState;
      primitive_type : TSDL_GPUPrimitiveType;
      rasterizer_state : TSDL_GPURasterizerState;
      multisample_state : TSDL_GPUMultisampleState;
      depth_stencil_state : TSDL_GPUDepthStencilState;
      target_info : TSDL_GPUGraphicsPipelineTargetInfo;
      props : TSDL_PropertiesID;
    end;
{*
 * A structure specifying the parameters of a compute pipeline state.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_CreateGPUComputePipeline
  }
{*< The size in bytes of the compute shader code pointed to.  }
{*< A pointer to compute shader code.  }
{*< A pointer to a null-terminated UTF-8 string specifying the entry point function name for the shader.  }
{*< The format of the compute shader code.  }
{*< The number of samplers defined in the shader.  }
{*< The number of readonly storage textures defined in the shader.  }
{*< The number of readonly storage buffers defined in the shader.  }
{*< The number of read-write storage textures defined in the shader.  }
{*< The number of read-write storage buffers defined in the shader.  }
{*< The number of uniform buffers defined in the shader.  }
{*< The number of threads in the X dimension. This should match the value in the shader.  }
{*< The number of threads in the Y dimension. This should match the value in the shader.  }
{*< The number of threads in the Z dimension. This should match the value in the shader.  }
{*< A properties ID for extensions. Should be 0 if no extensions are needed.  }

  PSDL_GPUComputePipelineCreateInfo = ^TSDL_GPUComputePipelineCreateInfo;
  TSDL_GPUComputePipelineCreateInfo = record
      code_size : Tsize_t;
      code : PUint8;
      entrypoint : Pchar;
      format : TSDL_GPUShaderFormat;
      num_samplers : TUint32;
      num_readonly_storage_textures : TUint32;
      num_readonly_storage_buffers : TUint32;
      num_readwrite_storage_textures : TUint32;
      num_readwrite_storage_buffers : TUint32;
      num_uniform_buffers : TUint32;
      threadcount_x : TUint32;
      threadcount_y : TUint32;
      threadcount_z : TUint32;
      props : TSDL_PropertiesID;
    end;
{*
 * A structure specifying the parameters of a color target used by a render
 * pass.
 *
 * The load_op field determines what is done with the texture at the beginning
 * of the render pass.
 *
 * - LOAD: Loads the data currently in the texture. Not recommended for
 *   multisample textures as it requires significant memory bandwidth.
 * - CLEAR: Clears the texture to a single color.
 * - DONT_CARE: The driver will do whatever it wants with the texture memory.
 *   This is a good option if you know that every single pixel will be touched
 *   in the render pass.
 *
 * The store_op field determines what is done with the color results of the
 * render pass.
 *
 * - STORE: Stores the results of the render pass in the texture. Not
 *   recommended for multisample textures as it requires significant memory
 *   bandwidth.
 * - DONT_CARE: The driver will do whatever it wants with the texture memory.
 *   This is often a good option for depth/stencil textures.
 * - RESOLVE: Resolves a multisample texture into resolve_texture, which must
 *   have a sample count of 1. Then the driver may discard the multisample
 *   texture memory. This is the most performant method of resolving a
 *   multisample target.
 * - RESOLVE_AND_STORE: Resolves a multisample texture into the
 *   resolve_texture, which must have a sample count of 1. Then the driver
 *   stores the multisample texture's contents. Not recommended as it requires
 *   significant memory bandwidth.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_BeginGPURenderPass
  }
{*< The texture that will be used as a color target by a render pass.  }
{*< The mip level to use as a color target.  }
{*< The layer index or depth plane to use as a color target. This value is treated as a layer index on 2D array and cube textures, and as a depth plane on 3D textures.  }
{*< The color to clear the color target to at the start of the render pass. Ignored if SDL_GPU_LOADOP_CLEAR is not used.  }
{*< What is done with the contents of the color target at the beginning of the render pass.  }
{*< What is done with the results of the render pass.  }
{*< The texture that will receive the results of a multisample resolve operation. Ignored if a RESOLVE* store_op is not used.  }
{*< The mip level of the resolve texture to use for the resolve operation. Ignored if a RESOLVE* store_op is not used.  }
{*< The layer index of the resolve texture to use for the resolve operation. Ignored if a RESOLVE* store_op is not used.  }
{*< true cycles the texture if the texture is bound and load_op is not LOAD  }
{*< true cycles the resolve texture if the resolve texture is bound. Ignored if a RESOLVE* store_op is not used.  }

  PSDL_GPUColorTargetInfo = ^TSDL_GPUColorTargetInfo;
  TSDL_GPUColorTargetInfo = record
      texture : PSDL_GPUTexture;
      mip_level : TUint32;
      layer_or_depth_plane : TUint32;
      clear_color : TSDL_FColor;
      load_op : TSDL_GPULoadOp;
      store_op : TSDL_GPUStoreOp;
      resolve_texture : PSDL_GPUTexture;
      resolve_mip_level : TUint32;
      resolve_layer : TUint32;
      cycle : Tbool;
      cycle_resolve_texture : Tbool;
      padding1 : TUint8;
      padding2 : TUint8;
    end;
{*
 * A structure specifying the parameters of a depth-stencil target used by a
 * render pass.
 *
 * The load_op field determines what is done with the depth contents of the
 * texture at the beginning of the render pass.
 *
 * - LOAD: Loads the depth values currently in the texture.
 * - CLEAR: Clears the texture to a single depth.
 * - DONT_CARE: The driver will do whatever it wants with the memory. This is
 *   a good option if you know that every single pixel will be touched in the
 *   render pass.
 *
 * The store_op field determines what is done with the depth results of the
 * render pass.
 *
 * - STORE: Stores the depth results in the texture.
 * - DONT_CARE: The driver will do whatever it wants with the depth results.
 *   This is often a good option for depth/stencil textures that don't need to
 *   be reused again.
 *
 * The stencil_load_op field determines what is done with the stencil contents
 * of the texture at the beginning of the render pass.
 *
 * - LOAD: Loads the stencil values currently in the texture.
 * - CLEAR: Clears the stencil values to a single value.
 * - DONT_CARE: The driver will do whatever it wants with the memory. This is
 *   a good option if you know that every single pixel will be touched in the
 *   render pass.
 *
 * The stencil_store_op field determines what is done with the stencil results
 * of the render pass.
 *
 * - STORE: Stores the stencil results in the texture.
 * - DONT_CARE: The driver will do whatever it wants with the stencil results.
 *   This is often a good option for depth/stencil textures that don't need to
 *   be reused again.
 *
 * Note that depth/stencil targets do not support multisample resolves.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_BeginGPURenderPass
  }
{*< The texture that will be used as the depth stencil target by the render pass.  }
{*< The value to clear the depth component to at the beginning of the render pass. Ignored if SDL_GPU_LOADOP_CLEAR is not used.  }
{*< What is done with the depth contents at the beginning of the render pass.  }
{*< What is done with the depth results of the render pass.  }
{*< What is done with the stencil contents at the beginning of the render pass.  }
{*< What is done with the stencil results of the render pass.  }
{*< true cycles the texture if the texture is bound and any load ops are not LOAD  }
{*< The value to clear the stencil component to at the beginning of the render pass. Ignored if SDL_GPU_LOADOP_CLEAR is not used.  }

  PSDL_GPUDepthStencilTargetInfo = ^TSDL_GPUDepthStencilTargetInfo;
  TSDL_GPUDepthStencilTargetInfo = record
      texture : PSDL_GPUTexture;
      clear_depth : single;
      load_op : TSDL_GPULoadOp;
      store_op : TSDL_GPUStoreOp;
      stencil_load_op : TSDL_GPULoadOp;
      stencil_store_op : TSDL_GPUStoreOp;
      cycle : Tbool;
      clear_stencil : TUint8;
      padding1 : TUint8;
      padding2 : TUint8;
    end;
{*
 * A structure containing parameters for a blit command.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_BlitGPUTexture
  }
{*< The source region for the blit.  }
{*< The destination region for the blit.  }
{*< What is done with the contents of the destination before the blit.  }
{*< The color to clear the destination region to before the blit. Ignored if load_op is not SDL_GPU_LOADOP_CLEAR.  }
{*< The flip mode for the source region.  }
{*< The filter mode used when blitting.  }
{*< true cycles the destination texture if it is already bound.  }

  PSDL_GPUBlitInfo = ^TSDL_GPUBlitInfo;
  TSDL_GPUBlitInfo = record
      source : TSDL_GPUBlitRegion;
      destination : TSDL_GPUBlitRegion;
      load_op : TSDL_GPULoadOp;
      clear_color : TSDL_FColor;
      flip_mode : TSDL_FlipMode;
      filter : TSDL_GPUFilter;
      cycle : Tbool;
      padding1 : TUint8;
      padding2 : TUint8;
      padding3 : TUint8;
    end;
{ Binding structs  }
{*
 * A structure specifying parameters in a buffer binding call.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_BindGPUVertexBuffers
 * \sa SDL_BindGPUIndexBuffer
  }
{*< The buffer to bind. Must have been created with SDL_GPU_BUFFERUSAGE_VERTEX for SDL_BindGPUVertexBuffers, or SDL_GPU_BUFFERUSAGE_INDEX for SDL_BindGPUIndexBuffer.  }
{*< The starting byte of the data to bind in the buffer.  }

  PSDL_GPUBufferBinding = ^TSDL_GPUBufferBinding;
  TSDL_GPUBufferBinding = record
      buffer : PSDL_GPUBuffer;
      offset : TUint32;
    end;
{*
 * A structure specifying parameters in a sampler binding call.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_BindGPUVertexSamplers
 * \sa SDL_BindGPUFragmentSamplers
  }
{*< The texture to bind. Must have been created with SDL_GPU_TEXTUREUSAGE_SAMPLER.  }
{*< The sampler to bind.  }

  PSDL_GPUTextureSamplerBinding = ^TSDL_GPUTextureSamplerBinding;
  TSDL_GPUTextureSamplerBinding = record
      texture : PSDL_GPUTexture;
      sampler : PSDL_GPUSampler;
    end;
{*
 * A structure specifying parameters related to binding buffers in a compute
 * pass.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_BeginGPUComputePass
  }
{*< The buffer to bind. Must have been created with SDL_GPU_BUFFERUSAGE_COMPUTE_STORAGE_WRITE.  }
{*< true cycles the buffer if it is already bound.  }

  PSDL_GPUStorageBufferReadWriteBinding = ^TSDL_GPUStorageBufferReadWriteBinding;
  TSDL_GPUStorageBufferReadWriteBinding = record
      buffer : PSDL_GPUBuffer;
      cycle : Tbool;
      padding1 : TUint8;
      padding2 : TUint8;
      padding3 : TUint8;
    end;
{*
 * A structure specifying parameters related to binding textures in a compute
 * pass.
 *
 * \since This struct is available since SDL 3.1.3
 *
 * \sa SDL_BeginGPUComputePass
  }
{*< The texture to bind. Must have been created with SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_WRITE or SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_SIMULTANEOUS_READ_WRITE.  }
{*< The mip level index to bind.  }
{*< The layer index to bind.  }
{*< true cycles the texture if it is already bound.  }

  PSDL_GPUStorageTextureReadWriteBinding = ^TSDL_GPUStorageTextureReadWriteBinding;
  TSDL_GPUStorageTextureReadWriteBinding = record
      texture : PSDL_GPUTexture;
      mip_level : TUint32;
      layer : TUint32;
      cycle : Tbool;
      padding1 : TUint8;
      padding2 : TUint8;
      padding3 : TUint8;
    end;
{ Functions  }
{ Device  }
{*
 * Checks for GPU runtime support.
 *
 * \param format_flags a bitflag indicating which shader formats the app is
 *                     able to provide.
 * \param name the preferred GPU driver, or NULL to let SDL pick the optimal
 *             driver.
 * \returns true if supported, false otherwise.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_CreateGPUDevice
  }

function SDL_GPUSupportsShaderFormats(format_flags:TSDL_GPUShaderFormat; name:Pchar):Tbool;cdecl;external libSDL3;
{*
 * Checks for GPU runtime support.
 *
 * \param props the properties to use.
 * \returns true if supported, false otherwise.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_CreateGPUDeviceWithProperties
  }
function SDL_GPUSupportsProperties(props:TSDL_PropertiesID):Tbool;cdecl;external libSDL3;
{*
 * Creates a GPU context.
 *
 * \param format_flags a bitflag indicating which shader formats the app is
 *                     able to provide.
 * \param debug_mode enable debug mode properties and validations.
 * \param name the preferred GPU driver, or NULL to let SDL pick the optimal
 *             driver.
 * \returns a GPU context on success or NULL on failure; call SDL_GetError()
 *          for more information.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_GetGPUShaderFormats
 * \sa SDL_GetGPUDeviceDriver
 * \sa SDL_DestroyGPUDevice
 * \sa SDL_GPUSupportsShaderFormats
  }
function SDL_CreateGPUDevice(format_flags:TSDL_GPUShaderFormat; debug_mode:Tbool; name:Pchar):PSDL_GPUDevice;cdecl;external libSDL3;
{*
 * Creates a GPU context.
 *
 * These are the supported properties:
 *
 * - `SDL_PROP_GPU_DEVICE_CREATE_DEBUGMODE_BOOLEAN`: enable debug mode
 *   properties and validations, defaults to true.
 * - `SDL_PROP_GPU_DEVICE_CREATE_PREFERLOWPOWER_BOOLEAN`: enable to prefer
 *   energy efficiency over maximum GPU performance, defaults to false.
 * - `SDL_PROP_GPU_DEVICE_CREATE_NAME_STRING`: the name of the GPU driver to
 *   use, if a specific one is desired.
 *
 * These are the current shader format properties:
 *
 * - `SDL_PROP_GPU_DEVICE_CREATE_SHADERS_PRIVATE_BOOLEAN`: The app is able to
 *   provide shaders for an NDA platform.
 * - `SDL_PROP_GPU_DEVICE_CREATE_SHADERS_SPIRV_BOOLEAN`: The app is able to
 *   provide SPIR-V shaders if applicable.
 * - `SDL_PROP_GPU_DEVICE_CREATE_SHADERS_DXBC_BOOLEAN`: The app is able to
 *   provide DXBC shaders if applicable
 *   `SDL_PROP_GPU_DEVICE_CREATE_SHADERS_DXIL_BOOLEAN`: The app is able to
 *   provide DXIL shaders if applicable.
 * - `SDL_PROP_GPU_DEVICE_CREATE_SHADERS_MSL_BOOLEAN`: The app is able to
 *   provide MSL shaders if applicable.
 * - `SDL_PROP_GPU_DEVICE_CREATE_SHADERS_METALLIB_BOOLEAN`: The app is able to
 *   provide Metal shader libraries if applicable.
 *
 * With the D3D12 renderer:
 *
 * - `SDL_PROP_GPU_DEVICE_CREATE_D3D12_SEMANTIC_NAME_STRING`: the prefix to
 *   use for all vertex semantics, default is "TEXCOORD".
 *
 * \param props the properties to use.
 * \returns a GPU context on success or NULL on failure; call SDL_GetError()
 *          for more information.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_GetGPUShaderFormats
 * \sa SDL_GetGPUDeviceDriver
 * \sa SDL_DestroyGPUDevice
 * \sa SDL_GPUSupportsProperties
  }
function SDL_CreateGPUDeviceWithProperties(props:TSDL_PropertiesID):PSDL_GPUDevice;cdecl;external libSDL3;
const
  SDL_PROP_GPU_DEVICE_CREATE_DEBUGMODE_BOOLEAN = 'SDL.gpu.device.create.debugmode';  
  SDL_PROP_GPU_DEVICE_CREATE_PREFERLOWPOWER_BOOLEAN = 'SDL.gpu.device.create.preferlowpower';  
  SDL_PROP_GPU_DEVICE_CREATE_NAME_STRING = 'SDL.gpu.device.create.name';  
  SDL_PROP_GPU_DEVICE_CREATE_SHADERS_PRIVATE_BOOLEAN = 'SDL.gpu.device.create.shaders.private';  
  SDL_PROP_GPU_DEVICE_CREATE_SHADERS_SPIRV_BOOLEAN = 'SDL.gpu.device.create.shaders.spirv';  
  SDL_PROP_GPU_DEVICE_CREATE_SHADERS_DXBC_BOOLEAN = 'SDL.gpu.device.create.shaders.dxbc';  
  SDL_PROP_GPU_DEVICE_CREATE_SHADERS_DXIL_BOOLEAN = 'SDL.gpu.device.create.shaders.dxil';  
  SDL_PROP_GPU_DEVICE_CREATE_SHADERS_MSL_BOOLEAN = 'SDL.gpu.device.create.shaders.msl';  
  SDL_PROP_GPU_DEVICE_CREATE_SHADERS_METALLIB_BOOLEAN = 'SDL.gpu.device.create.shaders.metallib';  
  SDL_PROP_GPU_DEVICE_CREATE_D3D12_SEMANTIC_NAME_STRING = 'SDL.gpu.device.create.d3d12.semantic';  
{*
 * Destroys a GPU context previously returned by SDL_CreateGPUDevice.
 *
 * \param device a GPU Context to destroy.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_CreateGPUDevice
  }

procedure SDL_DestroyGPUDevice(device:PSDL_GPUDevice);cdecl;external libSDL3;
{*
 * Get the number of GPU drivers compiled into SDL.
 *
 * \returns the number of built in GPU drivers.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_GetGPUDriver
  }
function SDL_GetNumGPUDrivers:longint;cdecl;external libSDL3;
{*
 * Get the name of a built in GPU driver.
 *
 * The GPU drivers are presented in the order in which they are normally
 * checked during initialization.
 *
 * The names of drivers are all simple, low-ASCII identifiers, like "vulkan",
 * "metal" or "direct3d12". These never have Unicode characters, and are not
 * meant to be proper names.
 *
 * \param index the index of a GPU driver.
 * \returns the name of the GPU driver with the given **index**.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_GetNumGPUDrivers
  }
function SDL_GetGPUDriver(index:longint):Pchar;cdecl;external libSDL3;
{*
 * Returns the name of the backend used to create this GPU context.
 *
 * \param device a GPU context to query.
 * \returns the name of the device's driver, or NULL on error.
 *
 * \since This function is available since SDL 3.1.3.
  }
function SDL_GetGPUDeviceDriver(device:PSDL_GPUDevice):Pchar;cdecl;external libSDL3;
{*
 * Returns the supported shader formats for this GPU context.
 *
 * \param device a GPU context to query.
 * \returns a bitflag indicating which shader formats the driver is able to
 *          consume.
 *
 * \since This function is available since SDL 3.1.3.
  }
function SDL_GetGPUShaderFormats(device:PSDL_GPUDevice):TSDL_GPUShaderFormat;cdecl;external libSDL3;
{ State Creation  }
{*
 * Creates a pipeline object to be used in a compute workflow.
 *
 * Shader resource bindings must be authored to follow a particular order
 * depending on the shader format.
 *
 * For SPIR-V shaders, use the following resource sets:
 *
 * - 0: Sampled textures, followed by read-only storage textures, followed by
 *   read-only storage buffers
 * - 1: Read-write storage textures, followed by read-write storage buffers
 * - 2: Uniform buffers
 *
 * For DXBC and DXIL shaders, use the following register order:
 *
 * - (t[n], space0): Sampled textures, followed by read-only storage textures,
 *   followed by read-only storage buffers
 * - (u[n], space1): Read-write storage textures, followed by read-write
 *   storage buffers
 * - (b[n], space2): Uniform buffers
 *
 * For MSL/metallib, use the following order:
 *
 * - [[buffer]]: Uniform buffers, followed by read-only storage buffers,
 *   followed by read-write storage buffers
 * - [[texture]]: Sampled textures, followed by read-only storage textures,
 *   followed by read-write storage textures
 *
 * \param device a GPU Context.
 * \param createinfo a struct describing the state of the compute pipeline to
 *                   create.
 * \returns a compute pipeline object on success, or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_BindGPUComputePipeline
 * \sa SDL_ReleaseGPUComputePipeline
  }
function SDL_CreateGPUComputePipeline(device:PSDL_GPUDevice; createinfo:PSDL_GPUComputePipelineCreateInfo):PSDL_GPUComputePipeline;cdecl;external libSDL3;
{*
 * Creates a pipeline object to be used in a graphics workflow.
 *
 * \param device a GPU Context.
 * \param createinfo a struct describing the state of the graphics pipeline to
 *                   create.
 * \returns a graphics pipeline object on success, or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_CreateGPUShader
 * \sa SDL_BindGPUGraphicsPipeline
 * \sa SDL_ReleaseGPUGraphicsPipeline
  }
function SDL_CreateGPUGraphicsPipeline(device:PSDL_GPUDevice; createinfo:PSDL_GPUGraphicsPipelineCreateInfo):PSDL_GPUGraphicsPipeline;cdecl;external libSDL3;
{*
 * Creates a sampler object to be used when binding textures in a graphics
 * workflow.
 *
 * \param device a GPU Context.
 * \param createinfo a struct describing the state of the sampler to create.
 * \returns a sampler object on success, or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_BindGPUVertexSamplers
 * \sa SDL_BindGPUFragmentSamplers
 * \sa SDL_ReleaseGPUSampler
  }
function SDL_CreateGPUSampler(device:PSDL_GPUDevice; createinfo:PSDL_GPUSamplerCreateInfo):PSDL_GPUSampler;cdecl;external libSDL3;
{*
 * Creates a shader to be used when creating a graphics pipeline.
 *
 * Shader resource bindings must be authored to follow a particular order
 * depending on the shader format.
 *
 * For SPIR-V shaders, use the following resource sets:
 *
 * For vertex shaders:
 *
 * - 0: Sampled textures, followed by storage textures, followed by storage
 *   buffers
 * - 1: Uniform buffers
 *
 * For fragment shaders:
 *
 * - 2: Sampled textures, followed by storage textures, followed by storage
 *   buffers
 * - 3: Uniform buffers
 *
 * For DXBC and DXIL shaders, use the following register order:
 *
 * For vertex shaders:
 *
 * - (t[n], space0): Sampled textures, followed by storage textures, followed
 *   by storage buffers
 * - (s[n], space0): Samplers with indices corresponding to the sampled
 *   textures
 * - (b[n], space1): Uniform buffers
 *
 * For pixel shaders:
 *
 * - (t[n], space2): Sampled textures, followed by storage textures, followed
 *   by storage buffers
 * - (s[n], space2): Samplers with indices corresponding to the sampled
 *   textures
 * - (b[n], space3): Uniform buffers
 *
 * For MSL/metallib, use the following order:
 *
 * - [[texture]]: Sampled textures, followed by storage textures
 * - [[sampler]]: Samplers with indices corresponding to the sampled textures
 * - [[buffer]]: Uniform buffers, followed by storage buffers. Vertex buffer 0
 *   is bound at [[buffer(14)]], vertex buffer 1 at [[buffer(15)]], and so on.
 *   Rather than manually authoring vertex buffer indices, use the
 *   [[stage_in]] attribute which will automatically use the vertex input
 *   information from the SDL_GPUGraphicsPipeline.
 *
 * Shader semantics other than system-value semantics do not matter in D3D12
 * and for ease of use the SDL implementation assumes that non system-value
 * semantics will all be TEXCOORD. If you are using HLSL as the shader source
 * language, your vertex semantics should start at TEXCOORD0 and increment
 * like so: TEXCOORD1, TEXCOORD2, etc. If you wish to change the semantic
 * prefix to something other than TEXCOORD you can use
 * SDL_PROP_GPU_DEVICE_CREATE_D3D12_SEMANTIC_NAME_STRING with
 * SDL_CreateGPUDeviceWithProperties().
 *
 * \param device a GPU Context.
 * \param createinfo a struct describing the state of the shader to create.
 * \returns a shader object on success, or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_CreateGPUGraphicsPipeline
 * \sa SDL_ReleaseGPUShader
  }
function SDL_CreateGPUShader(device:PSDL_GPUDevice; createinfo:PSDL_GPUShaderCreateInfo):PSDL_GPUShader;cdecl;external libSDL3;
{*
 * Creates a texture object to be used in graphics or compute workflows.
 *
 * The contents of this texture are undefined until data is written to the
 * texture.
 *
 * Note that certain combinations of usage flags are invalid. For example, a
 * texture cannot have both the SAMPLER and GRAPHICS_STORAGE_READ flags.
 *
 * If you request a sample count higher than the hardware supports, the
 * implementation will automatically fall back to the highest available sample
 * count.
 *
 * There are optional properties that can be provided through
 * SDL_GPUTextureCreateInfo's `props`. These are the supported properties:
 *
 * - `SDL_PROP_PROCESS_CREATE_ARGS_POINTER`: an array of strings containing
 *   the program to run, any arguments, and a NULL pointer, e.g. const char
 *   *args[] =  "myprogram", "argument", NULL . This is a required property.
 * - `SDL_PROP_GPU_CREATETEXTURE_D3D12_CLEAR_R_FLOAT`: (Direct3D 12 only) if
 *   the texture usage is SDL_GPU_TEXTUREUSAGE_COLOR_TARGET, clear the texture
 *   to a color with this red intensity. Defaults to zero.
 * - `SDL_PROP_GPU_CREATETEXTURE_D3D12_CLEAR_G_FLOAT`: (Direct3D 12 only) if
 *   the texture usage is SDL_GPU_TEXTUREUSAGE_COLOR_TARGET, clear the texture
 *   to a color with this green intensity. Defaults to zero.
 * - `SDL_PROP_GPU_CREATETEXTURE_D3D12_CLEAR_B_FLOAT`: (Direct3D 12 only) if
 *   the texture usage is SDL_GPU_TEXTUREUSAGE_COLOR_TARGET, clear the texture
 *   to a color with this blue intensity. Defaults to zero.
 * - `SDL_PROP_GPU_CREATETEXTURE_D3D12_CLEAR_A_FLOAT`: (Direct3D 12 only) if
 *   the texture usage is SDL_GPU_TEXTUREUSAGE_COLOR_TARGET, clear the texture
 *   to a color with this alpha intensity. Defaults to zero.
 * - `SDL_PROP_GPU_CREATETEXTURE_D3D12_CLEAR_DEPTH_FLOAT`: (Direct3D 12 only)
 *   if the texture usage is SDL_GPU_TEXTUREUSAGE_DEPTH_STENCIL_TARGET, clear
 *   the texture to a depth of this value. Defaults to zero.
 * - `SDL_PROP_GPU_CREATETEXTURE_D3D12_CLEAR_STENCIL_UINT8`: (Direct3D 12
 *   only) if the texture usage is SDL_GPU_TEXTUREUSAGE_DEPTH_STENCIL_TARGET,
 *   clear the texture to a stencil of this value. Defaults to zero.
 *
 * \param device a GPU Context.
 * \param createinfo a struct describing the state of the texture to create.
 * \returns a texture object on success, or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_UploadToGPUTexture
 * \sa SDL_DownloadFromGPUTexture
 * \sa SDL_BindGPUVertexSamplers
 * \sa SDL_BindGPUVertexStorageTextures
 * \sa SDL_BindGPUFragmentSamplers
 * \sa SDL_BindGPUFragmentStorageTextures
 * \sa SDL_BindGPUComputeStorageTextures
 * \sa SDL_BlitGPUTexture
 * \sa SDL_ReleaseGPUTexture
 * \sa SDL_GPUTextureSupportsFormat
  }
function SDL_CreateGPUTexture(device:PSDL_GPUDevice; createinfo:PSDL_GPUTextureCreateInfo):PSDL_GPUTexture;cdecl;external libSDL3;
const
  SDL_PROP_GPU_CREATETEXTURE_D3D12_CLEAR_R_FLOAT = 'SDL.gpu.createtexture.d3d12.clear.r';  
  SDL_PROP_GPU_CREATETEXTURE_D3D12_CLEAR_G_FLOAT = 'SDL.gpu.createtexture.d3d12.clear.g';  
  SDL_PROP_GPU_CREATETEXTURE_D3D12_CLEAR_B_FLOAT = 'SDL.gpu.createtexture.d3d12.clear.b';  
  SDL_PROP_GPU_CREATETEXTURE_D3D12_CLEAR_A_FLOAT = 'SDL.gpu.createtexture.d3d12.clear.a';  
  SDL_PROP_GPU_CREATETEXTURE_D3D12_CLEAR_DEPTH_FLOAT = 'SDL.gpu.createtexture.d3d12.clear.depth';  
  SDL_PROP_GPU_CREATETEXTURE_D3D12_CLEAR_STENCIL_UINT8 = 'SDL.gpu.createtexture.d3d12.clear.stencil';  
{*
 * Creates a buffer object to be used in graphics or compute workflows.
 *
 * The contents of this buffer are undefined until data is written to the
 * buffer.
 *
 * Note that certain combinations of usage flags are invalid. For example, a
 * buffer cannot have both the VERTEX and INDEX flags.
 *
 * For better understanding of underlying concepts and memory management with
 * SDL GPU API, you may refer
 * [this blog post](https://moonside.games/posts/sdl-gpu-concepts-cycling/)
 * .
 *
 * \param device a GPU Context.
 * \param createinfo a struct describing the state of the buffer to create.
 * \returns a buffer object on success, or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_SetGPUBufferName
 * \sa SDL_UploadToGPUBuffer
 * \sa SDL_DownloadFromGPUBuffer
 * \sa SDL_CopyGPUBufferToBuffer
 * \sa SDL_BindGPUVertexBuffers
 * \sa SDL_BindGPUIndexBuffer
 * \sa SDL_BindGPUVertexStorageBuffers
 * \sa SDL_BindGPUFragmentStorageBuffers
 * \sa SDL_DrawGPUPrimitivesIndirect
 * \sa SDL_DrawGPUIndexedPrimitivesIndirect
 * \sa SDL_BindGPUComputeStorageBuffers
 * \sa SDL_DispatchGPUComputeIndirect
 * \sa SDL_ReleaseGPUBuffer
  }

function SDL_CreateGPUBuffer(device:PSDL_GPUDevice; createinfo:PSDL_GPUBufferCreateInfo):PSDL_GPUBuffer;cdecl;external libSDL3;
{*
 * Creates a transfer buffer to be used when uploading to or downloading from
 * graphics resources.
 *
 * Download buffers can be particularly expensive to create, so it is good
 * practice to reuse them if data will be downloaded regularly.
 *
 * \param device a GPU Context.
 * \param createinfo a struct describing the state of the transfer buffer to
 *                   create.
 * \returns a transfer buffer on success, or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_UploadToGPUBuffer
 * \sa SDL_DownloadFromGPUBuffer
 * \sa SDL_UploadToGPUTexture
 * \sa SDL_DownloadFromGPUTexture
 * \sa SDL_ReleaseGPUTransferBuffer
  }
function SDL_CreateGPUTransferBuffer(device:PSDL_GPUDevice; createinfo:PSDL_GPUTransferBufferCreateInfo):PSDL_GPUTransferBuffer;cdecl;external libSDL3;
{ Debug Naming  }
{*
 * Sets an arbitrary string constant to label a buffer.
 *
 * Useful for debugging.
 *
 * \param device a GPU Context.
 * \param buffer a buffer to attach the name to.
 * \param text a UTF-8 string constant to mark as the name of the buffer.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_SetGPUBufferName(device:PSDL_GPUDevice; buffer:PSDL_GPUBuffer; text:Pchar);cdecl;external libSDL3;
{*
 * Sets an arbitrary string constant to label a texture.
 *
 * Useful for debugging.
 *
 * \param device a GPU Context.
 * \param texture a texture to attach the name to.
 * \param text a UTF-8 string constant to mark as the name of the texture.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_SetGPUTextureName(device:PSDL_GPUDevice; texture:PSDL_GPUTexture; text:Pchar);cdecl;external libSDL3;
{*
 * Inserts an arbitrary string label into the command buffer callstream.
 *
 * Useful for debugging.
 *
 * \param command_buffer a command buffer.
 * \param text a UTF-8 string constant to insert as the label.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_InsertGPUDebugLabel(command_buffer:PSDL_GPUCommandBuffer; text:Pchar);cdecl;external libSDL3;
{*
 * Begins a debug group with an arbitary name.
 *
 * Used for denoting groups of calls when viewing the command buffer
 * callstream in a graphics debugging tool.
 *
 * Each call to SDL_PushGPUDebugGroup must have a corresponding call to
 * SDL_PopGPUDebugGroup.
 *
 * On some backends (e.g. Metal), pushing a debug group during a
 * render/blit/compute pass will create a group that is scoped to the native
 * pass rather than the command buffer. For best results, if you push a debug
 * group during a pass, always pop it in the same pass.
 *
 * \param command_buffer a command buffer.
 * \param name a UTF-8 string constant that names the group.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_PopGPUDebugGroup
  }
procedure SDL_PushGPUDebugGroup(command_buffer:PSDL_GPUCommandBuffer; name:Pchar);cdecl;external libSDL3;
{*
 * Ends the most-recently pushed debug group.
 *
 * \param command_buffer a command buffer.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_PushGPUDebugGroup
  }
procedure SDL_PopGPUDebugGroup(command_buffer:PSDL_GPUCommandBuffer);cdecl;external libSDL3;
{ Disposal  }
{*
 * Frees the given texture as soon as it is safe to do so.
 *
 * You must not reference the texture after calling this function.
 *
 * \param device a GPU context.
 * \param texture a texture to be destroyed.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_ReleaseGPUTexture(device:PSDL_GPUDevice; texture:PSDL_GPUTexture);cdecl;external libSDL3;
{*
 * Frees the given sampler as soon as it is safe to do so.
 *
 * You must not reference the sampler after calling this function.
 *
 * \param device a GPU context.
 * \param sampler a sampler to be destroyed.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_ReleaseGPUSampler(device:PSDL_GPUDevice; sampler:PSDL_GPUSampler);cdecl;external libSDL3;
{*
 * Frees the given buffer as soon as it is safe to do so.
 *
 * You must not reference the buffer after calling this function.
 *
 * \param device a GPU context.
 * \param buffer a buffer to be destroyed.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_ReleaseGPUBuffer(device:PSDL_GPUDevice; buffer:PSDL_GPUBuffer);cdecl;external libSDL3;
{*
 * Frees the given transfer buffer as soon as it is safe to do so.
 *
 * You must not reference the transfer buffer after calling this function.
 *
 * \param device a GPU context.
 * \param transfer_buffer a transfer buffer to be destroyed.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_ReleaseGPUTransferBuffer(device:PSDL_GPUDevice; transfer_buffer:PSDL_GPUTransferBuffer);cdecl;external libSDL3;
{*
 * Frees the given compute pipeline as soon as it is safe to do so.
 *
 * You must not reference the compute pipeline after calling this function.
 *
 * \param device a GPU context.
 * \param compute_pipeline a compute pipeline to be destroyed.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_ReleaseGPUComputePipeline(device:PSDL_GPUDevice; compute_pipeline:PSDL_GPUComputePipeline);cdecl;external libSDL3;
{*
 * Frees the given shader as soon as it is safe to do so.
 *
 * You must not reference the shader after calling this function.
 *
 * \param device a GPU context.
 * \param shader a shader to be destroyed.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_ReleaseGPUShader(device:PSDL_GPUDevice; shader:PSDL_GPUShader);cdecl;external libSDL3;
{*
 * Frees the given graphics pipeline as soon as it is safe to do so.
 *
 * You must not reference the graphics pipeline after calling this function.
 *
 * \param device a GPU context.
 * \param graphics_pipeline a graphics pipeline to be destroyed.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_ReleaseGPUGraphicsPipeline(device:PSDL_GPUDevice; graphics_pipeline:PSDL_GPUGraphicsPipeline);cdecl;external libSDL3;
{*
 * Acquire a command buffer.
 *
 * This command buffer is managed by the implementation and should not be
 * freed by the user. The command buffer may only be used on the thread it was
 * acquired on. The command buffer should be submitted on the thread it was
 * acquired on.
 *
 * It is valid to acquire multiple command buffers on the same thread at once.
 * In fact a common design pattern is to acquire two command buffers per frame
 * where one is dedicated to render and compute passes and the other is
 * dedicated to copy passes and other preparatory work such as generating
 * mipmaps. Interleaving commands between the two command buffers reduces the
 * total amount of passes overall which improves rendering performance.
 *
 * \param device a GPU context.
 * \returns a command buffer, or NULL on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_SubmitGPUCommandBuffer
 * \sa SDL_SubmitGPUCommandBufferAndAcquireFence
  }
function SDL_AcquireGPUCommandBuffer(device:PSDL_GPUDevice):PSDL_GPUCommandBuffer;cdecl;external libSDL3;
{ Uniform Data  }
{*
 * Pushes data to a vertex uniform slot on the command buffer.
 *
 * Subsequent draw calls will use this uniform data.
 *
 * \param command_buffer a command buffer.
 * \param slot_index the vertex uniform slot to push data to.
 * \param data client data to write.
 * \param length the length of the data to write.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_PushGPUVertexUniformData(command_buffer:PSDL_GPUCommandBuffer; slot_index:TUint32; data:pointer; length:TUint32);cdecl;external libSDL3;
{*
 * Pushes data to a fragment uniform slot on the command buffer.
 *
 * Subsequent draw calls will use this uniform data.
 *
 * \param command_buffer a command buffer.
 * \param slot_index the fragment uniform slot to push data to.
 * \param data client data to write.
 * \param length the length of the data to write.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_PushGPUFragmentUniformData(command_buffer:PSDL_GPUCommandBuffer; slot_index:TUint32; data:pointer; length:TUint32);cdecl;external libSDL3;
{*
 * Pushes data to a uniform slot on the command buffer.
 *
 * Subsequent draw calls will use this uniform data.
 *
 * \param command_buffer a command buffer.
 * \param slot_index the uniform slot to push data to.
 * \param data client data to write.
 * \param length the length of the data to write.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_PushGPUComputeUniformData(command_buffer:PSDL_GPUCommandBuffer; slot_index:TUint32; data:pointer; length:TUint32);cdecl;external libSDL3;
{ Graphics State  }
{*
 * Begins a render pass on a command buffer.
 *
 * A render pass consists of a set of texture subresources (or depth slices in
 * the 3D texture case) which will be rendered to during the render pass,
 * along with corresponding clear values and load/store operations. All
 * operations related to graphics pipelines must take place inside of a render
 * pass. A default viewport and scissor state are automatically set when this
 * is called. You cannot begin another render pass, or begin a compute pass or
 * copy pass until you have ended the render pass.
 *
 * \param command_buffer a command buffer.
 * \param color_target_infos an array of texture subresources with
 *                           corresponding clear values and load/store ops.
 * \param num_color_targets the number of color targets in the
 *                          color_target_infos array.
 * \param depth_stencil_target_info a texture subresource with corresponding
 *                                  clear value and load/store ops, may be
 *                                  NULL.
 * \returns a render pass handle.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_EndGPURenderPass
  }
function SDL_BeginGPURenderPass(command_buffer:PSDL_GPUCommandBuffer; color_target_infos:PSDL_GPUColorTargetInfo; num_color_targets:TUint32; depth_stencil_target_info:PSDL_GPUDepthStencilTargetInfo):PSDL_GPURenderPass;cdecl;external libSDL3;
{*
 * Binds a graphics pipeline on a render pass to be used in rendering.
 *
 * A graphics pipeline must be bound before making any draw calls.
 *
 * \param render_pass a render pass handle.
 * \param graphics_pipeline the graphics pipeline to bind.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_BindGPUGraphicsPipeline(render_pass:PSDL_GPURenderPass; graphics_pipeline:PSDL_GPUGraphicsPipeline);cdecl;external libSDL3;
{*
 * Sets the current viewport state on a command buffer.
 *
 * \param render_pass a render pass handle.
 * \param viewport the viewport to set.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_SetGPUViewport(render_pass:PSDL_GPURenderPass; viewport:PSDL_GPUViewport);cdecl;external libSDL3;
{*
 * Sets the current scissor state on a command buffer.
 *
 * \param render_pass a render pass handle.
 * \param scissor the scissor area to set.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_SetGPUScissor(render_pass:PSDL_GPURenderPass; scissor:PSDL_Rect);cdecl;external libSDL3;
{*
 * Sets the current blend constants on a command buffer.
 *
 * \param render_pass a render pass handle.
 * \param blend_constants the blend constant color.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_GPU_BLENDFACTOR_CONSTANT_COLOR
 * \sa SDL_GPU_BLENDFACTOR_ONE_MINUS_CONSTANT_COLOR
  }
procedure SDL_SetGPUBlendConstants(render_pass:PSDL_GPURenderPass; blend_constants:TSDL_FColor);cdecl;external libSDL3;
{*
 * Sets the current stencil reference value on a command buffer.
 *
 * \param render_pass a render pass handle.
 * \param reference the stencil reference value to set.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_SetGPUStencilReference(render_pass:PSDL_GPURenderPass; reference:TUint8);cdecl;external libSDL3;
{*
 * Binds vertex buffers on a command buffer for use with subsequent draw
 * calls.
 *
 * \param render_pass a render pass handle.
 * \param first_slot the vertex buffer slot to begin binding from.
 * \param bindings an array of SDL_GPUBufferBinding structs containing vertex
 *                 buffers and offset values.
 * \param num_bindings the number of bindings in the bindings array.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_BindGPUVertexBuffers(render_pass:PSDL_GPURenderPass; first_slot:TUint32; bindings:PSDL_GPUBufferBinding; num_bindings:TUint32);cdecl;external libSDL3;
{*
 * Binds an index buffer on a command buffer for use with subsequent draw
 * calls.
 *
 * \param render_pass a render pass handle.
 * \param binding a pointer to a struct containing an index buffer and offset.
 * \param index_element_size whether the index values in the buffer are 16- or
 *                           32-bit.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_BindGPUIndexBuffer(render_pass:PSDL_GPURenderPass; binding:PSDL_GPUBufferBinding; index_element_size:TSDL_GPUIndexElementSize);cdecl;external libSDL3;
{*
 * Binds texture-sampler pairs for use on the vertex shader.
 *
 * The textures must have been created with SDL_GPU_TEXTUREUSAGE_SAMPLER.
 *
 * \param render_pass a render pass handle.
 * \param first_slot the vertex sampler slot to begin binding from.
 * \param texture_sampler_bindings an array of texture-sampler binding
 *                                 structs.
 * \param num_bindings the number of texture-sampler pairs to bind from the
 *                     array.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_BindGPUVertexSamplers(render_pass:PSDL_GPURenderPass; first_slot:TUint32; texture_sampler_bindings:PSDL_GPUTextureSamplerBinding; num_bindings:TUint32);cdecl;external libSDL3;
{*
 * Binds storage textures for use on the vertex shader.
 *
 * These textures must have been created with
 * SDL_GPU_TEXTUREUSAGE_GRAPHICS_STORAGE_READ.
 *
 * \param render_pass a render pass handle.
 * \param first_slot the vertex storage texture slot to begin binding from.
 * \param storage_textures an array of storage textures.
 * \param num_bindings the number of storage texture to bind from the array.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_BindGPUVertexStorageTextures(render_pass:PSDL_GPURenderPass; first_slot:TUint32; storage_textures:PPSDL_GPUTexture; num_bindings:TUint32);cdecl;external libSDL3;
{*
 * Binds storage buffers for use on the vertex shader.
 *
 * These buffers must have been created with
 * SDL_GPU_BUFFERUSAGE_GRAPHICS_STORAGE_READ.
 *
 * \param render_pass a render pass handle.
 * \param first_slot the vertex storage buffer slot to begin binding from.
 * \param storage_buffers an array of buffers.
 * \param num_bindings the number of buffers to bind from the array.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_BindGPUVertexStorageBuffers(render_pass:PSDL_GPURenderPass; first_slot:TUint32; storage_buffers:PPSDL_GPUBuffer; num_bindings:TUint32);cdecl;external libSDL3;
{*
 * Binds texture-sampler pairs for use on the fragment shader.
 *
 * The textures must have been created with SDL_GPU_TEXTUREUSAGE_SAMPLER.
 *
 * \param render_pass a render pass handle.
 * \param first_slot the fragment sampler slot to begin binding from.
 * \param texture_sampler_bindings an array of texture-sampler binding
 *                                 structs.
 * \param num_bindings the number of texture-sampler pairs to bind from the
 *                     array.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_BindGPUFragmentSamplers(render_pass:PSDL_GPURenderPass; first_slot:TUint32; texture_sampler_bindings:PSDL_GPUTextureSamplerBinding; num_bindings:TUint32);cdecl;external libSDL3;
{*
 * Binds storage textures for use on the fragment shader.
 *
 * These textures must have been created with
 * SDL_GPU_TEXTUREUSAGE_GRAPHICS_STORAGE_READ.
 *
 * \param render_pass a render pass handle.
 * \param first_slot the fragment storage texture slot to begin binding from.
 * \param storage_textures an array of storage textures.
 * \param num_bindings the number of storage textures to bind from the array.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_BindGPUFragmentStorageTextures(render_pass:PSDL_GPURenderPass; first_slot:TUint32; storage_textures:PPSDL_GPUTexture; num_bindings:TUint32);cdecl;external libSDL3;
{*
 * Binds storage buffers for use on the fragment shader.
 *
 * These buffers must have been created with
 * SDL_GPU_BUFFERUSAGE_GRAPHICS_STORAGE_READ.
 *
 * \param render_pass a render pass handle.
 * \param first_slot the fragment storage buffer slot to begin binding from.
 * \param storage_buffers an array of storage buffers.
 * \param num_bindings the number of storage buffers to bind from the array.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_BindGPUFragmentStorageBuffers(render_pass:PSDL_GPURenderPass; first_slot:TUint32; storage_buffers:PPSDL_GPUBuffer; num_bindings:TUint32);cdecl;external libSDL3;
{ Drawing  }
{*
 * Draws data using bound graphics state with an index buffer and instancing
 * enabled.
 *
 * You must not call this function before binding a graphics pipeline.
 *
 * Note that the `first_vertex` and `first_instance` parameters are NOT
 * compatible with built-in vertex/instance ID variables in shaders (for
 * example, SV_VertexID); GPU APIs and shader languages do not define these
 * built-in variables consistently, so if your shader depends on them, the
 * only way to keep behavior consistent and portable is to always pass 0 for
 * the correlating parameter in the draw calls.
 *
 * \param render_pass a render pass handle.
 * \param num_indices the number of indices to draw per instance.
 * \param num_instances the number of instances to draw.
 * \param first_index the starting index within the index buffer.
 * \param vertex_offset value added to vertex index before indexing into the
 *                      vertex buffer.
 * \param first_instance the ID of the first instance to draw.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_DrawGPUIndexedPrimitives(render_pass:PSDL_GPURenderPass; num_indices:TUint32; num_instances:TUint32; first_index:TUint32; vertex_offset:TSint32; 
            first_instance:TUint32);cdecl;external libSDL3;
{*
 * Draws data using bound graphics state.
 *
 * You must not call this function before binding a graphics pipeline.
 *
 * Note that the `first_vertex` and `first_instance` parameters are NOT
 * compatible with built-in vertex/instance ID variables in shaders (for
 * example, SV_VertexID); GPU APIs and shader languages do not define these
 * built-in variables consistently, so if your shader depends on them, the
 * only way to keep behavior consistent and portable is to always pass 0 for
 * the correlating parameter in the draw calls.
 *
 * \param render_pass a render pass handle.
 * \param num_vertices the number of vertices to draw.
 * \param num_instances the number of instances that will be drawn.
 * \param first_vertex the index of the first vertex to draw.
 * \param first_instance the ID of the first instance to draw.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_DrawGPUPrimitives(render_pass:PSDL_GPURenderPass; num_vertices:TUint32; num_instances:TUint32; first_vertex:TUint32; first_instance:TUint32);cdecl;external libSDL3;
{*
 * Draws data using bound graphics state and with draw parameters set from a
 * buffer.
 *
 * The buffer must consist of tightly-packed draw parameter sets that each
 * match the layout of SDL_GPUIndirectDrawCommand. You must not call this
 * function before binding a graphics pipeline.
 *
 * \param render_pass a render pass handle.
 * \param buffer a buffer containing draw parameters.
 * \param offset the offset to start reading from the draw buffer.
 * \param draw_count the number of draw parameter sets that should be read
 *                   from the draw buffer.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_DrawGPUPrimitivesIndirect(render_pass:PSDL_GPURenderPass; buffer:PSDL_GPUBuffer; offset:TUint32; draw_count:TUint32);cdecl;external libSDL3;
{*
 * Draws data using bound graphics state with an index buffer enabled and with
 * draw parameters set from a buffer.
 *
 * The buffer must consist of tightly-packed draw parameter sets that each
 * match the layout of SDL_GPUIndexedIndirectDrawCommand. You must not call
 * this function before binding a graphics pipeline.
 *
 * \param render_pass a render pass handle.
 * \param buffer a buffer containing draw parameters.
 * \param offset the offset to start reading from the draw buffer.
 * \param draw_count the number of draw parameter sets that should be read
 *                   from the draw buffer.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_DrawGPUIndexedPrimitivesIndirect(render_pass:PSDL_GPURenderPass; buffer:PSDL_GPUBuffer; offset:TUint32; draw_count:TUint32);cdecl;external libSDL3;
{*
 * Ends the given render pass.
 *
 * All bound graphics state on the render pass command buffer is unset. The
 * render pass handle is now invalid.
 *
 * \param render_pass a render pass handle.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_EndGPURenderPass(render_pass:PSDL_GPURenderPass);cdecl;external libSDL3;
{ Compute Pass  }
{*
 * Begins a compute pass on a command buffer.
 *
 * A compute pass is defined by a set of texture subresources and buffers that
 * may be written to by compute pipelines. These textures and buffers must
 * have been created with the COMPUTE_STORAGE_WRITE bit or the
 * COMPUTE_STORAGE_SIMULTANEOUS_READ_WRITE bit. If you do not create a texture
 * with COMPUTE_STORAGE_SIMULTANEOUS_READ_WRITE, you must not read from the
 * texture in the compute pass. All operations related to compute pipelines
 * must take place inside of a compute pass. You must not begin another
 * compute pass, or a render pass or copy pass before ending the compute pass.
 *
 * A VERY IMPORTANT NOTE - Reads and writes in compute passes are NOT
 * implicitly synchronized. This means you may cause data races by both
 * reading and writing a resource region in a compute pass, or by writing
 * multiple times to a resource region. If your compute work depends on
 * reading the completed output from a previous dispatch, you MUST end the
 * current compute pass and begin a new one before you can safely access the
 * data. Otherwise you will receive unexpected results. Reading and writing a
 * texture in the same compute pass is only supported by specific texture
 * formats. Make sure you check the format support!
 *
 * \param command_buffer a command buffer.
 * \param storage_texture_bindings an array of writeable storage texture
 *                                 binding structs.
 * \param num_storage_texture_bindings the number of storage textures to bind
 *                                     from the array.
 * \param storage_buffer_bindings an array of writeable storage buffer binding
 *                                structs.
 * \param num_storage_buffer_bindings the number of storage buffers to bind
 *                                    from the array.
 * \returns a compute pass handle.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_EndGPUComputePass
  }
function SDL_BeginGPUComputePass(command_buffer:PSDL_GPUCommandBuffer; storage_texture_bindings:PSDL_GPUStorageTextureReadWriteBinding; num_storage_texture_bindings:TUint32; storage_buffer_bindings:PSDL_GPUStorageBufferReadWriteBinding; num_storage_buffer_bindings:TUint32):PSDL_GPUComputePass;cdecl;external libSDL3;
{*
 * Binds a compute pipeline on a command buffer for use in compute dispatch.
 *
 * \param compute_pass a compute pass handle.
 * \param compute_pipeline a compute pipeline to bind.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_BindGPUComputePipeline(compute_pass:PSDL_GPUComputePass; compute_pipeline:PSDL_GPUComputePipeline);cdecl;external libSDL3;
{*
 * Binds texture-sampler pairs for use on the compute shader.
 *
 * The textures must have been created with SDL_GPU_TEXTUREUSAGE_SAMPLER.
 *
 * \param compute_pass a compute pass handle.
 * \param first_slot the compute sampler slot to begin binding from.
 * \param texture_sampler_bindings an array of texture-sampler binding
 *                                 structs.
 * \param num_bindings the number of texture-sampler bindings to bind from the
 *                     array.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_BindGPUComputeSamplers(compute_pass:PSDL_GPUComputePass; first_slot:TUint32; texture_sampler_bindings:PSDL_GPUTextureSamplerBinding; num_bindings:TUint32);cdecl;external libSDL3;
{*
 * Binds storage textures as readonly for use on the compute pipeline.
 *
 * These textures must have been created with
 * SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_READ.
 *
 * \param compute_pass a compute pass handle.
 * \param first_slot the compute storage texture slot to begin binding from.
 * \param storage_textures an array of storage textures.
 * \param num_bindings the number of storage textures to bind from the array.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_BindGPUComputeStorageTextures(compute_pass:PSDL_GPUComputePass; first_slot:TUint32; storage_textures:PPSDL_GPUTexture; num_bindings:TUint32);cdecl;external libSDL3;
{*
 * Binds storage buffers as readonly for use on the compute pipeline.
 *
 * These buffers must have been created with
 * SDL_GPU_BUFFERUSAGE_COMPUTE_STORAGE_READ.
 *
 * \param compute_pass a compute pass handle.
 * \param first_slot the compute storage buffer slot to begin binding from.
 * \param storage_buffers an array of storage buffer binding structs.
 * \param num_bindings the number of storage buffers to bind from the array.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_BindGPUComputeStorageBuffers(compute_pass:PSDL_GPUComputePass; first_slot:TUint32; storage_buffers:PPSDL_GPUBuffer; num_bindings:TUint32);cdecl;external libSDL3;
{*
 * Dispatches compute work.
 *
 * You must not call this function before binding a compute pipeline.
 *
 * A VERY IMPORTANT NOTE If you dispatch multiple times in a compute pass, and
 * the dispatches write to the same resource region as each other, there is no
 * guarantee of which order the writes will occur. If the write order matters,
 * you MUST end the compute pass and begin another one.
 *
 * \param compute_pass a compute pass handle.
 * \param groupcount_x number of local workgroups to dispatch in the X
 *                     dimension.
 * \param groupcount_y number of local workgroups to dispatch in the Y
 *                     dimension.
 * \param groupcount_z number of local workgroups to dispatch in the Z
 *                     dimension.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_DispatchGPUCompute(compute_pass:PSDL_GPUComputePass; groupcount_x:TUint32; groupcount_y:TUint32; groupcount_z:TUint32);cdecl;external libSDL3;
{*
 * Dispatches compute work with parameters set from a buffer.
 *
 * The buffer layout should match the layout of
 * SDL_GPUIndirectDispatchCommand. You must not call this function before
 * binding a compute pipeline.
 *
 * A VERY IMPORTANT NOTE If you dispatch multiple times in a compute pass, and
 * the dispatches write to the same resource region as each other, there is no
 * guarantee of which order the writes will occur. If the write order matters,
 * you MUST end the compute pass and begin another one.
 *
 * \param compute_pass a compute pass handle.
 * \param buffer a buffer containing dispatch parameters.
 * \param offset the offset to start reading from the dispatch buffer.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_DispatchGPUComputeIndirect(compute_pass:PSDL_GPUComputePass; buffer:PSDL_GPUBuffer; offset:TUint32);cdecl;external libSDL3;
{*
 * Ends the current compute pass.
 *
 * All bound compute state on the command buffer is unset. The compute pass
 * handle is now invalid.
 *
 * \param compute_pass a compute pass handle.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_EndGPUComputePass(compute_pass:PSDL_GPUComputePass);cdecl;external libSDL3;
{ TransferBuffer Data  }
{*
 * Maps a transfer buffer into application address space.
 *
 * You must unmap the transfer buffer before encoding upload commands.
 *
 * \param device a GPU context.
 * \param transfer_buffer a transfer buffer.
 * \param cycle if true, cycles the transfer buffer if it is already bound.
 * \returns the address of the mapped transfer buffer memory, or NULL on
 *          failure; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.1.3.
  }
function SDL_MapGPUTransferBuffer(device:PSDL_GPUDevice; transfer_buffer:PSDL_GPUTransferBuffer; cycle:Tbool):pointer;cdecl;external libSDL3;
{*
 * Unmaps a previously mapped transfer buffer.
 *
 * \param device a GPU context.
 * \param transfer_buffer a previously mapped transfer buffer.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_UnmapGPUTransferBuffer(device:PSDL_GPUDevice; transfer_buffer:PSDL_GPUTransferBuffer);cdecl;external libSDL3;
{ Copy Pass  }
{*
 * Begins a copy pass on a command buffer.
 *
 * All operations related to copying to or from buffers or textures take place
 * inside a copy pass. You must not begin another copy pass, or a render pass
 * or compute pass before ending the copy pass.
 *
 * \param command_buffer a command buffer.
 * \returns a copy pass handle.
 *
 * \since This function is available since SDL 3.1.3.
  }
function SDL_BeginGPUCopyPass(command_buffer:PSDL_GPUCommandBuffer):PSDL_GPUCopyPass;cdecl;external libSDL3;
{*
 * Uploads data from a transfer buffer to a texture.
 *
 * The upload occurs on the GPU timeline. You may assume that the upload has
 * finished in subsequent commands.
 *
 * You must align the data in the transfer buffer to a multiple of the texel
 * size of the texture format.
 *
 * \param copy_pass a copy pass handle.
 * \param source the source transfer buffer with image layout information.
 * \param destination the destination texture region.
 * \param cycle if true, cycles the texture if the texture is bound, otherwise
 *              overwrites the data.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_UploadToGPUTexture(copy_pass:PSDL_GPUCopyPass; source:PSDL_GPUTextureTransferInfo; destination:PSDL_GPUTextureRegion; cycle:Tbool);cdecl;external libSDL3;
{*
 * Uploads data from a transfer buffer to a buffer.
 *
 * The upload occurs on the GPU timeline. You may assume that the upload has
 * finished in subsequent commands.
 *
 * \param copy_pass a copy pass handle.
 * \param source the source transfer buffer with offset.
 * \param destination the destination buffer with offset and size.
 * \param cycle if true, cycles the buffer if it is already bound, otherwise
 *              overwrites the data.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_UploadToGPUBuffer(copy_pass:PSDL_GPUCopyPass; source:PSDL_GPUTransferBufferLocation; destination:PSDL_GPUBufferRegion; cycle:Tbool);cdecl;external libSDL3;
{*
 * Performs a texture-to-texture copy.
 *
 * This copy occurs on the GPU timeline. You may assume the copy has finished
 * in subsequent commands.
 *
 * \param copy_pass a copy pass handle.
 * \param source a source texture region.
 * \param destination a destination texture region.
 * \param w the width of the region to copy.
 * \param h the height of the region to copy.
 * \param d the depth of the region to copy.
 * \param cycle if true, cycles the destination texture if the destination
 *              texture is bound, otherwise overwrites the data.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_CopyGPUTextureToTexture(copy_pass:PSDL_GPUCopyPass; source:PSDL_GPUTextureLocation; destination:PSDL_GPUTextureLocation; w:TUint32; h:TUint32; 
            d:TUint32; cycle:Tbool);cdecl;external libSDL3;
{*
 * Performs a buffer-to-buffer copy.
 *
 * This copy occurs on the GPU timeline. You may assume the copy has finished
 * in subsequent commands.
 *
 * \param copy_pass a copy pass handle.
 * \param source the buffer and offset to copy from.
 * \param destination the buffer and offset to copy to.
 * \param size the length of the buffer to copy.
 * \param cycle if true, cycles the destination buffer if it is already bound,
 *              otherwise overwrites the data.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_CopyGPUBufferToBuffer(copy_pass:PSDL_GPUCopyPass; source:PSDL_GPUBufferLocation; destination:PSDL_GPUBufferLocation; size:TUint32; cycle:Tbool);cdecl;external libSDL3;
{*
 * Copies data from a texture to a transfer buffer on the GPU timeline.
 *
 * This data is not guaranteed to be copied until the command buffer fence is
 * signaled.
 *
 * \param copy_pass a copy pass handle.
 * \param source the source texture region.
 * \param destination the destination transfer buffer with image layout
 *                    information.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_DownloadFromGPUTexture(copy_pass:PSDL_GPUCopyPass; source:PSDL_GPUTextureRegion; destination:PSDL_GPUTextureTransferInfo);cdecl;external libSDL3;
{*
 * Copies data from a buffer to a transfer buffer on the GPU timeline.
 *
 * This data is not guaranteed to be copied until the command buffer fence is
 * signaled.
 *
 * \param copy_pass a copy pass handle.
 * \param source the source buffer with offset and size.
 * \param destination the destination transfer buffer with offset.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_DownloadFromGPUBuffer(copy_pass:PSDL_GPUCopyPass; source:PSDL_GPUBufferRegion; destination:PSDL_GPUTransferBufferLocation);cdecl;external libSDL3;
{*
 * Ends the current copy pass.
 *
 * \param copy_pass a copy pass handle.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_EndGPUCopyPass(copy_pass:PSDL_GPUCopyPass);cdecl;external libSDL3;
{*
 * Generates mipmaps for the given texture.
 *
 * This function must not be called inside of any pass.
 *
 * \param command_buffer a command_buffer.
 * \param texture a texture with more than 1 mip level.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_GenerateMipmapsForGPUTexture(command_buffer:PSDL_GPUCommandBuffer; texture:PSDL_GPUTexture);cdecl;external libSDL3;
{*
 * Blits from a source texture region to a destination texture region.
 *
 * This function must not be called inside of any pass.
 *
 * \param command_buffer a command buffer.
 * \param info the blit info struct containing the blit parameters.
 *
 * \since This function is available since SDL 3.1.3.
  }
procedure SDL_BlitGPUTexture(command_buffer:PSDL_GPUCommandBuffer; info:PSDL_GPUBlitInfo);cdecl;external libSDL3;
{ Submission/Presentation  }
{*
 * Determines whether a swapchain composition is supported by the window.
 *
 * The window must be claimed before calling this function.
 *
 * \param device a GPU context.
 * \param window an SDL_Window.
 * \param swapchain_composition the swapchain composition to check.
 * \returns true if supported, false if unsupported.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_ClaimWindowForGPUDevice
  }
function SDL_WindowSupportsGPUSwapchainComposition(device:PSDL_GPUDevice; window:PSDL_Window; swapchain_composition:TSDL_GPUSwapchainComposition):Tbool;cdecl;external libSDL3;
{*
 * Determines whether a presentation mode is supported by the window.
 *
 * The window must be claimed before calling this function.
 *
 * \param device a GPU context.
 * \param window an SDL_Window.
 * \param present_mode the presentation mode to check.
 * \returns true if supported, false if unsupported.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_ClaimWindowForGPUDevice
  }
function SDL_WindowSupportsGPUPresentMode(device:PSDL_GPUDevice; window:PSDL_Window; present_mode:TSDL_GPUPresentMode):Tbool;cdecl;external libSDL3;
{*
 * Claims a window, creating a swapchain structure for it.
 *
 * This must be called before SDL_AcquireGPUSwapchainTexture is called using
 * the window. You should only call this function from the thread that created
 * the window.
 *
 * The swapchain will be created with SDL_GPU_SWAPCHAINCOMPOSITION_SDR and
 * SDL_GPU_PRESENTMODE_VSYNC. If you want to have different swapchain
 * parameters, you must call SDL_SetGPUSwapchainParameters after claiming the
 * window.
 *
 * \param device a GPU context.
 * \param window an SDL_Window.
 * \returns true on success, or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should only be called from the thread that
 *               created the window.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_WaitAndAcquireGPUSwapchainTexture
 * \sa SDL_ReleaseWindowFromGPUDevice
 * \sa SDL_WindowSupportsGPUPresentMode
 * \sa SDL_WindowSupportsGPUSwapchainComposition
  }
function SDL_ClaimWindowForGPUDevice(device:PSDL_GPUDevice; window:PSDL_Window):Tbool;cdecl;external libSDL3;
{*
 * Unclaims a window, destroying its swapchain structure.
 *
 * \param device a GPU context.
 * \param window an SDL_Window that has been claimed.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_ClaimWindowForGPUDevice
  }
procedure SDL_ReleaseWindowFromGPUDevice(device:PSDL_GPUDevice; window:PSDL_Window);cdecl;external libSDL3;
{*
 * Changes the swapchain parameters for the given claimed window.
 *
 * This function will fail if the requested present mode or swapchain
 * composition are unsupported by the device. Check if the parameters are
 * supported via SDL_WindowSupportsGPUPresentMode /
 * SDL_WindowSupportsGPUSwapchainComposition prior to calling this function.
 *
 * SDL_GPU_PRESENTMODE_VSYNC and SDL_GPU_SWAPCHAINCOMPOSITION_SDR are always
 * supported.
 *
 * \param device a GPU context.
 * \param window an SDL_Window that has been claimed.
 * \param swapchain_composition the desired composition of the swapchain.
 * \param present_mode the desired present mode for the swapchain.
 * \returns true if successful, false on error; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_WindowSupportsGPUPresentMode
 * \sa SDL_WindowSupportsGPUSwapchainComposition
  }
function SDL_SetGPUSwapchainParameters(device:PSDL_GPUDevice; window:PSDL_Window; swapchain_composition:TSDL_GPUSwapchainComposition; present_mode:TSDL_GPUPresentMode):Tbool;cdecl;external libSDL3;
{*
 * Configures the maximum allowed number of frames in flight.
 *
 * The default value when the device is created is 2. This means that after
 * you have submitted 2 frames for presentation, if the GPU has not finished
 * working on the first frame, SDL_AcquireGPUSwapchainTexture() will fill the
 * swapchain texture pointer with NULL, and
 * SDL_WaitAndAcquireGPUSwapchainTexture() will block.
 *
 * Higher values increase throughput at the expense of visual latency. Lower
 * values decrease visual latency at the expense of throughput.
 *
 * Note that calling this function will stall and flush the command queue to
 * prevent synchronization issues.
 *
 * The minimum value of allowed frames in flight is 1, and the maximum is 3.
 *
 * \param device a GPU context.
 * \param allowed_frames_in_flight the maximum number of frames that can be
 *                                 pending on the GPU.
 * \returns true if successful, false on error; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.2.0.
  }
function SDL_SetGPUAllowedFramesInFlight(device:PSDL_GPUDevice; allowed_frames_in_flight:TUint32):Tbool;cdecl;external libSDL3;
{*
 * Obtains the texture format of the swapchain for the given window.
 *
 * Note that this format can change if the swapchain parameters change.
 *
 * \param device a GPU context.
 * \param window an SDL_Window that has been claimed.
 * \returns the texture format of the swapchain.
 *
 * \since This function is available since SDL 3.1.3.
  }
function SDL_GetGPUSwapchainTextureFormat(device:PSDL_GPUDevice; window:PSDL_Window):TSDL_GPUTextureFormat;cdecl;external libSDL3;
{*
 * Acquire a texture to use in presentation.
 *
 * When a swapchain texture is acquired on a command buffer, it will
 * automatically be submitted for presentation when the command buffer is
 * submitted. The swapchain texture should only be referenced by the command
 * buffer used to acquire it.
 *
 * This function will fill the swapchain texture handle with NULL if too many
 * frames are in flight. This is not an error.
 *
 * If you use this function, it is possible to create a situation where many
 * command buffers are allocated while the rendering context waits for the GPU
 * to catch up, which will cause memory usage to grow. You should use
 * SDL_WaitAndAcquireGPUSwapchainTexture() unless you know what you are doing
 * with timing.
 *
 * The swapchain texture is managed by the implementation and must not be
 * freed by the user. You MUST NOT call this function from any thread other
 * than the one that created the window.
 *
 * \param command_buffer a command buffer.
 * \param window a window that has been claimed.
 * \param swapchain_texture a pointer filled in with a swapchain texture
 *                          handle.
 * \param swapchain_texture_width a pointer filled in with the swapchain
 *                                texture width, may be NULL.
 * \param swapchain_texture_height a pointer filled in with the swapchain
 *                                 texture height, may be NULL.
 * \returns true on success, false on error; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should only be called from the thread that
 *               created the window.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_ClaimWindowForGPUDevice
 * \sa SDL_SubmitGPUCommandBuffer
 * \sa SDL_SubmitGPUCommandBufferAndAcquireFence
 * \sa SDL_CancelGPUCommandBuffer
 * \sa SDL_GetWindowSizeInPixels
 * \sa SDL_WaitForGPUSwapchain
 * \sa SDL_WaitAndAcquireGPUSwapchainTexture
 * \sa SDL_SetGPUAllowedFramesInFlight
  }
function SDL_AcquireGPUSwapchainTexture(command_buffer:PSDL_GPUCommandBuffer; window:PSDL_Window; swapchain_texture:PPSDL_GPUTexture; swapchain_texture_width:PUint32; swapchain_texture_height:PUint32):Tbool;cdecl;external libSDL3;
{*
 * Blocks the thread until a swapchain texture is available to be acquired.
 *
 * \param device a GPU context.
 * \param window a window that has been claimed.
 * \returns true on success, false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should only be called from the thread that
 *               created the window.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_AcquireGPUSwapchainTexture
 * \sa SDL_WaitAndAcquireGPUSwapchainTexture
 * \sa SDL_SetGPUAllowedFramesInFlight
  }
function SDL_WaitForGPUSwapchain(device:PSDL_GPUDevice; window:PSDL_Window):Tbool;cdecl;external libSDL3;
{*
 * Blocks the thread until a swapchain texture is available to be acquired,
 * and then acquires it.
 *
 * When a swapchain texture is acquired on a command buffer, it will
 * automatically be submitted for presentation when the command buffer is
 * submitted. The swapchain texture should only be referenced by the command
 * buffer used to acquire it. It is an error to call
 * SDL_CancelGPUCommandBuffer() after a swapchain texture is acquired.
 *
 * This function can fill the swapchain texture handle with NULL in certain
 * cases, for example if the window is minimized. This is not an error. You
 * should always make sure to check whether the pointer is NULL before
 * actually using it.
 *
 * The swapchain texture is managed by the implementation and must not be
 * freed by the user. You MUST NOT call this function from any thread other
 * than the one that created the window.
 *
 * \param command_buffer a command buffer.
 * \param window a window that has been claimed.
 * \param swapchain_texture a pointer filled in with a swapchain texture
 *                          handle.
 * \param swapchain_texture_width a pointer filled in with the swapchain
 *                                texture width, may be NULL.
 * \param swapchain_texture_height a pointer filled in with the swapchain
 *                                 texture height, may be NULL.
 * \returns true on success, false on error; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should only be called from the thread that
 *               created the window.
 *
 * \since This function is available since SDL 3.2.0.
 *
 * \sa SDL_SubmitGPUCommandBuffer
 * \sa SDL_SubmitGPUCommandBufferAndAcquireFence
  }
function SDL_WaitAndAcquireGPUSwapchainTexture(command_buffer:PSDL_GPUCommandBuffer; window:PSDL_Window; swapchain_texture:PPSDL_GPUTexture; swapchain_texture_width:PUint32; swapchain_texture_height:PUint32):Tbool;cdecl;external libSDL3;
{*
 * Submits a command buffer so its commands can be processed on the GPU.
 *
 * It is invalid to use the command buffer after this is called.
 *
 * This must be called from the thread the command buffer was acquired on.
 *
 * All commands in the submission are guaranteed to begin executing before any
 * command in a subsequent submission begins executing.
 *
 * \param command_buffer a command buffer.
 * \returns true on success, false on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_AcquireGPUCommandBuffer
 * \sa SDL_WaitAndAcquireGPUSwapchainTexture
 * \sa SDL_AcquireGPUSwapchainTexture
 * \sa SDL_SubmitGPUCommandBufferAndAcquireFence
  }
function SDL_SubmitGPUCommandBuffer(command_buffer:PSDL_GPUCommandBuffer):Tbool;cdecl;external libSDL3;
{*
 * Submits a command buffer so its commands can be processed on the GPU, and
 * acquires a fence associated with the command buffer.
 *
 * You must release this fence when it is no longer needed or it will cause a
 * leak. It is invalid to use the command buffer after this is called.
 *
 * This must be called from the thread the command buffer was acquired on.
 *
 * All commands in the submission are guaranteed to begin executing before any
 * command in a subsequent submission begins executing.
 *
 * \param command_buffer a command buffer.
 * \returns a fence associated with the command buffer, or NULL on failure;
 *          call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_AcquireGPUCommandBuffer
 * \sa SDL_WaitAndAcquireGPUSwapchainTexture
 * \sa SDL_AcquireGPUSwapchainTexture
 * \sa SDL_SubmitGPUCommandBuffer
 * \sa SDL_ReleaseGPUFence
  }
function SDL_SubmitGPUCommandBufferAndAcquireFence(command_buffer:PSDL_GPUCommandBuffer):PSDL_GPUFence;cdecl;external libSDL3;
{*
 * Cancels a command buffer.
 *
 * None of the enqueued commands are executed.
 *
 * It is an error to call this function after a swapchain texture has been
 * acquired.
 *
 * This must be called from the thread the command buffer was acquired on.
 *
 * You must not reference the command buffer after calling this function.
 *
 * \param command_buffer a command buffer.
 * \returns true on success, false on error; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.1.6.
 *
 * \sa SDL_WaitAndAcquireGPUSwapchainTexture
 * \sa SDL_AcquireGPUCommandBuffer
 * \sa SDL_AcquireGPUSwapchainTexture
  }
function SDL_CancelGPUCommandBuffer(command_buffer:PSDL_GPUCommandBuffer):Tbool;cdecl;external libSDL3;
{*
 * Blocks the thread until the GPU is completely idle.
 *
 * \param device a GPU context.
 * \returns true on success, false on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_WaitForGPUFences
  }
function SDL_WaitForGPUIdle(device:PSDL_GPUDevice):Tbool;cdecl;external libSDL3;
{*
 * Blocks the thread until the given fences are signaled.
 *
 * \param device a GPU context.
 * \param wait_all if 0, wait for any fence to be signaled, if 1, wait for all
 *                 fences to be signaled.
 * \param fences an array of fences to wait on.
 * \param num_fences the number of fences in the fences array.
 * \returns true on success, false on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_SubmitGPUCommandBufferAndAcquireFence
 * \sa SDL_WaitForGPUIdle
  }
function SDL_WaitForGPUFences(device:PSDL_GPUDevice; wait_all:Tbool; fences:PPSDL_GPUFence; num_fences:TUint32):Tbool;cdecl;external libSDL3;
{*
 * Checks the status of a fence.
 *
 * \param device a GPU context.
 * \param fence a fence.
 * \returns true if the fence is signaled, false if it is not.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_SubmitGPUCommandBufferAndAcquireFence
  }
function SDL_QueryGPUFence(device:PSDL_GPUDevice; fence:PSDL_GPUFence):Tbool;cdecl;external libSDL3;
{*
 * Releases a fence obtained from SDL_SubmitGPUCommandBufferAndAcquireFence.
 *
 * \param device a GPU context.
 * \param fence a fence.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_SubmitGPUCommandBufferAndAcquireFence
  }
procedure SDL_ReleaseGPUFence(device:PSDL_GPUDevice; fence:PSDL_GPUFence);cdecl;external libSDL3;
{ Format Info  }
{*
 * Obtains the texel block size for a texture format.
 *
 * \param format the texture format you want to know the texel size of.
 * \returns the texel block size of the texture format.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_UploadToGPUTexture
  }
function SDL_GPUTextureFormatTexelBlockSize(format:TSDL_GPUTextureFormat):TUint32;cdecl;external libSDL3;
{*
 * Determines whether a texture format is supported for a given type and
 * usage.
 *
 * \param device a GPU context.
 * \param format the texture format to check.
 * \param type the type of texture (2D, 3D, Cube).
 * \param usage a bitmask of all usage scenarios to check.
 * \returns whether the texture format is supported for this type and usage.
 *
 * \since This function is available since SDL 3.1.3.
  }
function SDL_GPUTextureSupportsFormat(device:PSDL_GPUDevice; format:TSDL_GPUTextureFormat; _type:TSDL_GPUTextureType; usage:TSDL_GPUTextureUsageFlags):Tbool;cdecl;external libSDL3;
{*
 * Determines if a sample count for a texture format is supported.
 *
 * \param device a GPU context.
 * \param format the texture format to check.
 * \param sample_count the sample count to check.
 * \returns a hardware-specific version of min(preferred, possible).
 *
 * \since This function is available since SDL 3.1.3.
  }
function SDL_GPUTextureSupportsSampleCount(device:PSDL_GPUDevice; format:TSDL_GPUTextureFormat; sample_count:TSDL_GPUSampleCount):Tbool;cdecl;external libSDL3;
{*
 * Calculate the size in bytes of a texture format with dimensions.
 *
 * \param format a texture format.
 * \param width width in pixels.
 * \param height height in pixels.
 * \param depth_or_layer_count depth for 3D textures or layer count otherwise.
 * \returns the size of a texture with this format and dimensions.
 *
 * \since This function is available since SDL 3.1.6.
  }
function SDL_CalculateGPUTextureFormatSize(format:TSDL_GPUTextureFormat; width:TUint32; height:TUint32; depth_or_layer_count:TUint32):TUint32;cdecl;external libSDL3;
{$ifdef SDL_PLATFORM_GDK}
{*
 * Call this to suspend GPU operation on Xbox when you receive the
 * SDL_EVENT_DID_ENTER_BACKGROUND event.
 *
 * Do NOT call any SDL_GPU functions after calling this function! This must
 * also be called before calling SDL_GDKSuspendComplete.
 *
 * \param device a GPU context.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_AddEventWatch
  }
procedure SDL_GDKSuspendGPU(device:PSDL_GPUDevice);cdecl;external libSDL3;
{*
 * Call this to resume GPU operation on Xbox when you receive the
 * SDL_EVENT_WILL_ENTER_FOREGROUND event.
 *
 * When resuming, this function MUST be called before calling any other
 * SDL_GPU functions.
 *
 * \param device a GPU context.
 *
 * \since This function is available since SDL 3.1.3.
 *
 * \sa SDL_AddEventWatch
  }
procedure SDL_GDKResumeGPU(device:PSDL_GPUDevice);cdecl;external libSDL3;
{$endif}
{ SDL_PLATFORM_GDK  }
{ C++ end of extern C conditionnal removed }
{ __cplusplus  }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_gpu_h_  }

implementation


end.
