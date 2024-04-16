unit SDL_image;

interface

uses
  SDL3;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

const
{$IFDEF Linux}
sdl3_image_lib = 'libSDL3_image.so';
{$ENDIF}

{$IFDEF Windows} //SDL3_image.dll
sdl3_image_lib =  'SDL3_image.dll';
//sdl3_lib = 'SDL3.dll';
{$ENDIF}

const
  SDL_IMAGE_MAJOR_VERSION = 3;  
  SDL_IMAGE_MINOR_VERSION = 0;  
  SDL_IMAGE_PATCHLEVEL = 0;  
{*
 * This macro can be used to fill a version structure with the compile-time
 * version of the SDL_image library.
  }
{#define SDL_IMAGE_VERSION(X)                        \ }
{                                                   \ }
{    (X)->major = SDL_IMAGE_MAJOR_VERSION;           \ }
{    (X)->minor = SDL_IMAGE_MINOR_VERSION;           \ }
{    (X)->patch = SDL_IMAGE_PATCHLEVEL;              \ }
{ }
//{$if SDL_IMAGE_MAJOR_VERSION < 3 && SDL_MAJOR_VERSION < 3}
{*
 * This is the version number macro for the current SDL_image version.
 *
 * In versions higher than 2.9.0, the minor version overflows into the
 * thousands digit: for example, 2.23.0 is encoded as 4300. This macro will
 * not be available in SDL 3.x or SDL_image 3.x.
 *
 * Deprecated, use SDL_IMAGE_VERSION_ATLEAST or SDL_IMAGE_VERSION instead.
  }

{ was #define dname def_expr }
function SDL_IMAGE_COMPILEDVERSION : longint; { return type might be wrong }

//{$endif}
{ SDL_IMAGE_MAJOR_VERSION < 3 && SDL_MAJOR_VERSION < 3  }
{*
 * This macro will evaluate to true if compiled with SDL_image at least X.Y.Z.
  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

function SDL_IMAGE_VERSION_ATLEAST(X,Y,Z : longint) : longint;

{*
 * This function gets the version of the dynamically linked SDL_image library.
 *
 * it should NOT be used to fill a version structure, instead you should use
 * the SDL_IMAGE_VERSION() macro.
 *
 * \returns SDL_image version
  }
(* Const before type ignored *)
function IMG_Linked_Version:PSDL_Version;cdecl;external sdl3_lib;
{*
 * Initialization flags
  }
type
  PIMG_InitFlags = ^TIMG_InitFlags;
  TIMG_InitFlags =  Longint;
  Const
    IMG_INIT_JPG = $00000001;
    IMG_INIT_PNG = $00000002;
    IMG_INIT_TIF = $00000004;
    IMG_INIT_WEBP = $00000008;
    IMG_INIT_JXL = $00000010;
    IMG_INIT_AVIF = $00000020;

{*
 * Initialize SDL_image.
 *
 * This function loads dynamic libraries that SDL_image needs, and prepares
 * them for use. This must be the first function you call in SDL_image, and if
 * it fails you should not continue with the library.
 *
 * Flags should be one or more flags from IMG_InitFlags OR'd together. It
 * returns the flags successfully initialized, or 0 on failure.
 *
 * Currently, these flags are:
 *
 * - `IMG_INIT_JPG`
 * - `IMG_INIT_PNG`
 * - `IMG_INIT_TIF`
 * - `IMG_INIT_WEBP`
 * - `IMG_INIT_JXL`
 * - `IMG_INIT_AVIF`
 *
 * More flags may be added in a future SDL_image release.
 *
 * This function may need to load external shared libraries to support various
 * codecs, which means this function can fail to initialize that support on an
 * otherwise-reasonable system if the library isn't available; this is not
 * just a question of exceptional circumstances like running out of memory at
 * startup!
 *
 * Note that you may call this function more than once to initialize with
 * additional flags. The return value will reflect both new flags that
 * successfully initialized, and also include flags that had previously been
 * initialized as well.
 *
 * As this will return previously-initialized flags, it's legal to call this
 * with zero (no flags set). This is a safe no-op that can be used to query
 * the current initialization state without changing it at all.
 *
 * Since this returns previously-initialized flags as well as new ones, and
 * you can call this with zero, you should not check for a zero return value
 * to determine an error condition. Instead, you should check to make sure all
 * the flags you require are set in the return value. If you have a game with
 * data in a specific format, this might be a fatal error. If you're a generic
 * image displaying app, perhaps you are fine with only having JPG and PNG
 * support and can live without WEBP, even if you request support for
 * everything.
 *
 * Unlike other SDL satellite libraries, calls to IMG_Init do not stack; a
 * single call to IMG_Quit() will deinitialize everything and does not have to
 * be paired with a matching IMG_Init call. For that reason, it's considered
 * best practices to have a single IMG_Init and IMG_Quit call in your program.
 * While this isn't required, be aware of the risks of deviating from that
 * behavior.
 *
 * After initializing SDL_image, the app may begin to load images into
 * SDL_Surfaces or SDL_Textures.
 *
 * \param flags initialization flags, OR'd together.
 * \returns all currently initialized flags.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_Quit
  }

function IMG_Init(flags:longint):longint;cdecl;external sdl3_lib;
{*
 * Deinitialize SDL_image.
 *
 * This should be the last function you call in SDL_image, after freeing all
 * other resources. This will unload any shared libraries it is using for
 * various codecs.
 *
 * After this call, a call to IMG_Init(0) will return 0 (no codecs loaded).
 *
 * You can safely call IMG_Init() to reload various codec support after this
 * call.
 *
 * Unlike other SDL satellite libraries, calls to IMG_Init do not stack; a
 * single call to IMG_Quit() will deinitialize everything and does not have to
 * be paired with a matching IMG_Init call. For that reason, it's considered
 * best practices to have a single IMG_Init and IMG_Quit call in your program.
 * While this isn't required, be aware of the risks of deviating from that
 * behavior.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_Init
  }
procedure IMG_Quit;cdecl;external sdl3_image_lib;
{*
 * Load an image from an SDL data source into a software surface.
 *
 * An SDL_Surface is a buffer of pixels in memory accessible by the CPU. Use
 * this if you plan to hand the data to something else or manipulate it
 * further in code.
 *
 * There are no guarantees about what format the new SDL_Surface data will be;
 * in many cases, SDL_image will attempt to supply a surface that exactly
 * matches the provided image, but in others it might have to convert (either
 * because the image is in a format that SDL doesn't directly support or
 * because it's compressed data that could reasonably uncompress to various
 * formats and SDL_image had to pick one). You can inspect an SDL_Surface for
 * its specifics, and use SDL_ConvertSurface to then migrate to any supported
 * format.
 *
 * If the image format supports a transparent pixel, SDL will set the colorkey
 * for the surface. You can enable RLE acceleration on the surface afterwards
 * by calling: SDL_SetSurfaceColorKey(image, SDL_RLEACCEL,
 * image->format->colorkey);
 *
 * If `closeio` is SDL_TRUE, `src` will be closed before returning, whether
 * this function succeeds or not. SDL_image reads everything it needs from
 * `src` during this call in any case.
 *
 * Even though this function accepts a file type, SDL_image may still try
 * other decoders that are capable of detecting file type from the contents of
 * the image data, but may rely on the caller-provided type string for formats
 * that it cannot autodetect. If `type` is NULL, SDL_image will rely solely on
 * its ability to guess the format.
 *
 * There is a separate function to read files from disk without having to deal
 * with SDL_IOStream: `IMG_Load("filename.jpg")` will call this function and
 * manage those details for you, determining the file type from the filename's
 * extension.
 *
 * There is also IMG_Load_IO(), which is equivalent to this function except
 * that it will rely on SDL_image to determine what type of data it is
 * loading, much like passing a NULL for type.
 *
 * If you are using SDL's 2D rendering API, there is an equivalent call to
 * load images directly into an SDL_Texture for use by the GPU without using a
 * software surface: call IMG_LoadTextureTyped_IO() instead.
 *
 * When done with the returned surface, the app should dispose of it with a
 * call to SDL_DestroySurface().
 *
 * \param src an SDL_IOStream that data will be read from.
 * \param closeio SDL_TRUE to close/free the SDL_IOStream before returning,
 *                SDL_FALSE to leave it open.
 * \param type a filename extension that represent this data ("BMP", "GIF",
 *             "PNG", etc).
 * \returns a new SDL surface, or NULL on error.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_Load
 * \sa IMG_Load_IO
 * \sa SDL_DestroySurface
  }
(* Const before type ignored *)
function IMG_LoadTyped_IO(src:PSDL_IOStream; closeio:TSDL_bool; _type:Pchar):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load an image from a filesystem path into a software surface.
 *
 * An SDL_Surface is a buffer of pixels in memory accessible by the CPU. Use
 * this if you plan to hand the data to something else or manipulate it
 * further in code.
 *
 * There are no guarantees about what format the new SDL_Surface data will be;
 * in many cases, SDL_image will attempt to supply a surface that exactly
 * matches the provided image, but in others it might have to convert (either
 * because the image is in a format that SDL doesn't directly support or
 * because it's compressed data that could reasonably uncompress to various
 * formats and SDL_image had to pick one). You can inspect an SDL_Surface for
 * its specifics, and use SDL_ConvertSurface to then migrate to any supported
 * format.
 *
 * If the image format supports a transparent pixel, SDL will set the colorkey
 * for the surface. You can enable RLE acceleration on the surface afterwards
 * by calling: SDL_SetSurfaceColorKey(image, SDL_RLEACCEL,
 * image->format->colorkey);
 *
 * There is a separate function to read files from an SDL_IOStream, if you
 * need an i/o abstraction to provide data from anywhere instead of a simple
 * filesystem read; that function is IMG_Load_IO().
 *
 * If you are using SDL's 2D rendering API, there is an equivalent call to
 * load images directly into an SDL_Texture for use by the GPU without using a
 * software surface: call IMG_LoadTexture() instead.
 *
 * When done with the returned surface, the app should dispose of it with a
 * call to SDL_DestroySurface().
 *
 * \param file a path on the filesystem to load an image from.
 * \returns a new SDL surface, or NULL on error.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadTyped_IO
 * \sa IMG_Load_IO
 * \sa SDL_DestroySurface
  }
(* Const before type ignored *)
function IMG_Load(file_:Pchar):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load an image from an SDL data source into a software surface.
 *
 * An SDL_Surface is a buffer of pixels in memory accessible by the CPU. Use
 * this if you plan to hand the data to something else or manipulate it
 * further in code.
 *
 * There are no guarantees about what format the new SDL_Surface data will be;
 * in many cases, SDL_image will attempt to supply a surface that exactly
 * matches the provided image, but in others it might have to convert (either
 * because the image is in a format that SDL doesn't directly support or
 * because it's compressed data that could reasonably uncompress to various
 * formats and SDL_image had to pick one). You can inspect an SDL_Surface for
 * its specifics, and use SDL_ConvertSurface to then migrate to any supported
 * format.
 *
 * If the image format supports a transparent pixel, SDL will set the colorkey
 * for the surface. You can enable RLE acceleration on the surface afterwards
 * by calling: SDL_SetSurfaceColorKey(image, SDL_RLEACCEL,
 * image->format->colorkey);
 *
 * If `closeio` is SDL_TRUE, `src` will be closed before returning, whether
 * this function succeeds or not. SDL_image reads everything it needs from
 * `src` during this call in any case.
 *
 * There is a separate function to read files from disk without having to deal
 * with SDL_IOStream: `IMG_Load("filename.jpg")` will call this function and
 * manage those details for you, determining the file type from the filename's
 * extension.
 *
 * There is also IMG_LoadTyped_IO(), which is equivalent to this function
 * except a file extension (like "BMP", "JPG", etc) can be specified, in case
 * SDL_image cannot autodetect the file format.
 *
 * If you are using SDL's 2D rendering API, there is an equivalent call to
 * load images directly into an SDL_Texture for use by the GPU without using a
 * software surface: call IMG_LoadTexture_IO() instead.
 *
 * When done with the returned surface, the app should dispose of it with a
 * call to SDL_DestroySurface().
 *
 * \param src an SDL_IOStream that data will be read from.
 * \param closeio SDL_TRUE to close/free the SDL_IOStream before returning,
 *                SDL_FALSE to leave it open.
 * \returns a new SDL surface, or NULL on error.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_Load
 * \sa IMG_LoadTyped_IO
 * \sa SDL_DestroySurface
  }
function IMG_Load_IO(src:PSDL_IOStream; closeio:TSDL_bool):PSDL_Surface;cdecl;external sdl3_image_lib;
//{$if SDL_VERSION_ATLEAST(2,0,0)}
{*
 * Load an image from a filesystem path into a GPU texture.
 *
 * An SDL_Texture represents an image in GPU memory, usable by SDL's 2D Render
 * API. This can be significantly more efficient than using a CPU-bound
 * SDL_Surface if you don't need to manipulate the image directly after
 * loading it.
 *
 * If the loaded image has transparency or a colorkey, a texture with an alpha
 * channel will be created. Otherwise, SDL_image will attempt to create an
 * SDL_Texture in the most format that most reasonably represents the image
 * data (but in many cases, this will just end up being 32-bit RGB or 32-bit
 * RGBA).
 *
 * There is a separate function to read files from an SDL_IOStream, if you
 * need an i/o abstraction to provide data from anywhere instead of a simple
 * filesystem read; that function is IMG_LoadTexture_IO().
 *
 * If you would rather decode an image to an SDL_Surface (a buffer of pixels
 * in CPU memory), call IMG_Load() instead.
 *
 * When done with the returned texture, the app should dispose of it with a
 * call to SDL_DestroyTexture().
 *
 * \param renderer the SDL_Renderer to use to create the GPU texture.
 * \param file a path on the filesystem to load an image from.
 * \returns a new texture, or NULL on error.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadTextureTyped_IO
 * \sa IMG_LoadTexture_IO
  }
(* Const before type ignored *)

function IMG_LoadTexture(renderer:PSDL_Renderer; file_:Pchar):PSDL_Texture;cdecl;external sdl3_image_lib;
{*
 * Load an image from an SDL data source into a GPU texture.
 *
 * An SDL_Texture represents an image in GPU memory, usable by SDL's 2D Render
 * API. This can be significantly more efficient than using a CPU-bound
 * SDL_Surface if you don't need to manipulate the image directly after
 * loading it.
 *
 * If the loaded image has transparency or a colorkey, a texture with an alpha
 * channel will be created. Otherwise, SDL_image will attempt to create an
 * SDL_Texture in the most format that most reasonably represents the image
 * data (but in many cases, this will just end up being 32-bit RGB or 32-bit
 * RGBA).
 *
 * If `closeio` is SDL_TRUE, `src` will be closed before returning, whether
 * this function succeeds or not. SDL_image reads everything it needs from
 * `src` during this call in any case.
 *
 * There is a separate function to read files from disk without having to deal
 * with SDL_IOStream: `IMG_LoadTexture(renderer, "filename.jpg")` will call
 * this function and manage those details for you, determining the file type
 * from the filename's extension.
 *
 * There is also IMG_LoadTextureTyped_IO(), which is equivalent to this
 * function except a file extension (like "BMP", "JPG", etc) can be specified,
 * in case SDL_image cannot autodetect the file format.
 *
 * If you would rather decode an image to an SDL_Surface (a buffer of pixels
 * in CPU memory), call IMG_Load() instead.
 *
 * When done with the returned texture, the app should dispose of it with a
 * call to SDL_DestroyTexture().
 *
 * \param renderer the SDL_Renderer to use to create the GPU texture.
 * \param src an SDL_IOStream that data will be read from.
 * \param closeio SDL_TRUE to close/free the SDL_IOStream before returning,
 *                SDL_FALSE to leave it open.
 * \returns a new texture, or NULL on error.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadTexture
 * \sa IMG_LoadTextureTyped_IO
 * \sa SDL_DestroyTexture
  }
function IMG_LoadTexture_IO(renderer:PSDL_Renderer; src:PSDL_IOStream; closeio:TSDL_bool):PSDL_Texture;cdecl;external sdl3_image_lib;
{*
 * Load an image from an SDL data source into a GPU texture.
 *
 * An SDL_Texture represents an image in GPU memory, usable by SDL's 2D Render
 * API. This can be significantly more efficient than using a CPU-bound
 * SDL_Surface if you don't need to manipulate the image directly after
 * loading it.
 *
 * If the loaded image has transparency or a colorkey, a texture with an alpha
 * channel will be created. Otherwise, SDL_image will attempt to create an
 * SDL_Texture in the most format that most reasonably represents the image
 * data (but in many cases, this will just end up being 32-bit RGB or 32-bit
 * RGBA).
 *
 * If `closeio` is SDL_TRUE, `src` will be closed before returning, whether
 * this function succeeds or not. SDL_image reads everything it needs from
 * `src` during this call in any case.
 *
 * Even though this function accepts a file type, SDL_image may still try
 * other decoders that are capable of detecting file type from the contents of
 * the image data, but may rely on the caller-provided type string for formats
 * that it cannot autodetect. If `type` is NULL, SDL_image will rely solely on
 * its ability to guess the format.
 *
 * There is a separate function to read files from disk without having to deal
 * with SDL_IOStream: `IMG_LoadTexture("filename.jpg")` will call this
 * function and manage those details for you, determining the file type from
 * the filename's extension.
 *
 * There is also IMG_LoadTexture_IO(), which is equivalent to this function
 * except that it will rely on SDL_image to determine what type of data it is
 * loading, much like passing a NULL for type.
 *
 * If you would rather decode an image to an SDL_Surface (a buffer of pixels
 * in CPU memory), call IMG_LoadTyped_IO() instead.
 *
 * When done with the returned texture, the app should dispose of it with a
 * call to SDL_DestroyTexture().
 *
 * \param renderer the SDL_Renderer to use to create the GPU texture.
 * \param src an SDL_IOStream that data will be read from.
 * \param closeio SDL_TRUE to close/free the SDL_IOStream before returning,
 *                SDL_FALSE to leave it open.
 * \param type a filename extension that represent this data ("BMP", "GIF",
 *             "PNG", etc).
 * \returns a new texture, or NULL on error.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadTexture
 * \sa IMG_LoadTexture_IO
 * \sa SDL_DestroyTexture
  }
(* Const before type ignored *)
function IMG_LoadTextureTyped_IO(renderer:PSDL_Renderer; src:PSDL_IOStream; closeio:TSDL_bool; _type:Pchar):PSDL_Texture;cdecl;external sdl3_image_lib;
//{$endif}
{ SDL 2.0  }
{*
 * Detect AVIF image data on a readable/seekable SDL_IOStream.
 *
 * This function attempts to determine if a file is a given filetype, reading
 * the least amount possible from the SDL_IOStream (usually a few bytes).
 *
 * There is no distinction made between "not the filetype in question" and
 * basic i/o errors.
 *
 * This function will always attempt to seek `src` back to where it started
 * when this function was called, but it will not report any errors in doing
 * so, but assuming seeking works, this means you can immediately use this
 * with a different IMG_isTYPE function, or load the image without further
 * seeking.
 *
 * You do not need to call this function to load data; SDL_image can work to
 * determine file type in many cases in its standard load functions.
 *
 * \param src a seekable/readable SDL_IOStream to provide image data.
 * \returns non-zero if this is AVIF data, zero otherwise.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_isAVIF
 * \sa IMG_isICO
 * \sa IMG_isCUR
 * \sa IMG_isBMP
 * \sa IMG_isGIF
 * \sa IMG_isJPG
 * \sa IMG_isJXL
 * \sa IMG_isLBM
 * \sa IMG_isPCX
 * \sa IMG_isPNG
 * \sa IMG_isPNM
 * \sa IMG_isSVG
 * \sa IMG_isQOI
 * \sa IMG_isTIF
 * \sa IMG_isXCF
 * \sa IMG_isXPM
 * \sa IMG_isXV
 * \sa IMG_isWEBP
  }

function IMG_isAVIF(src:PSDL_IOStream):longint;cdecl;external sdl3_image_lib;
{*
 * Detect ICO image data on a readable/seekable SDL_IOStream.
 *
 * This function attempts to determine if a file is a given filetype, reading
 * the least amount possible from the SDL_IOStream (usually a few bytes).
 *
 * There is no distinction made between "not the filetype in question" and
 * basic i/o errors.
 *
 * This function will always attempt to seek `src` back to where it started
 * when this function was called, but it will not report any errors in doing
 * so, but assuming seeking works, this means you can immediately use this
 * with a different IMG_isTYPE function, or load the image without further
 * seeking.
 *
 * You do not need to call this function to load data; SDL_image can work to
 * determine file type in many cases in its standard load functions.
 *
 * \param src a seekable/readable SDL_IOStream to provide image data.
 * \returns non-zero if this is ICO data, zero otherwise.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_isAVIF
 * \sa IMG_isCUR
 * \sa IMG_isBMP
 * \sa IMG_isGIF
 * \sa IMG_isJPG
 * \sa IMG_isJXL
 * \sa IMG_isLBM
 * \sa IMG_isPCX
 * \sa IMG_isPNG
 * \sa IMG_isPNM
 * \sa IMG_isSVG
 * \sa IMG_isQOI
 * \sa IMG_isTIF
 * \sa IMG_isXCF
 * \sa IMG_isXPM
 * \sa IMG_isXV
 * \sa IMG_isWEBP
  }
function IMG_isICO(src:PSDL_IOStream):longint;cdecl;external sdl3_image_lib;
{*
 * Detect CUR image data on a readable/seekable SDL_IOStream.
 *
 * This function attempts to determine if a file is a given filetype, reading
 * the least amount possible from the SDL_IOStream (usually a few bytes).
 *
 * There is no distinction made between "not the filetype in question" and
 * basic i/o errors.
 *
 * This function will always attempt to seek `src` back to where it started
 * when this function was called, but it will not report any errors in doing
 * so, but assuming seeking works, this means you can immediately use this
 * with a different IMG_isTYPE function, or load the image without further
 * seeking.
 *
 * You do not need to call this function to load data; SDL_image can work to
 * determine file type in many cases in its standard load functions.
 *
 * \param src a seekable/readable SDL_IOStream to provide image data.
 * \returns non-zero if this is CUR data, zero otherwise.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_isAVIF
 * \sa IMG_isICO
 * \sa IMG_isBMP
 * \sa IMG_isGIF
 * \sa IMG_isJPG
 * \sa IMG_isJXL
 * \sa IMG_isLBM
 * \sa IMG_isPCX
 * \sa IMG_isPNG
 * \sa IMG_isPNM
 * \sa IMG_isSVG
 * \sa IMG_isQOI
 * \sa IMG_isTIF
 * \sa IMG_isXCF
 * \sa IMG_isXPM
 * \sa IMG_isXV
 * \sa IMG_isWEBP
  }
function IMG_isCUR(src:PSDL_IOStream):longint;cdecl;external sdl3_image_lib;
{*
 * Detect BMP image data on a readable/seekable SDL_IOStream.
 *
 * This function attempts to determine if a file is a given filetype, reading
 * the least amount possible from the SDL_IOStream (usually a few bytes).
 *
 * There is no distinction made between "not the filetype in question" and
 * basic i/o errors.
 *
 * This function will always attempt to seek `src` back to where it started
 * when this function was called, but it will not report any errors in doing
 * so, but assuming seeking works, this means you can immediately use this
 * with a different IMG_isTYPE function, or load the image without further
 * seeking.
 *
 * You do not need to call this function to load data; SDL_image can work to
 * determine file type in many cases in its standard load functions.
 *
 * \param src a seekable/readable SDL_IOStream to provide image data.
 * \returns non-zero if this is BMP data, zero otherwise.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_isAVIF
 * \sa IMG_isICO
 * \sa IMG_isCUR
 * \sa IMG_isGIF
 * \sa IMG_isJPG
 * \sa IMG_isJXL
 * \sa IMG_isLBM
 * \sa IMG_isPCX
 * \sa IMG_isPNG
 * \sa IMG_isPNM
 * \sa IMG_isSVG
 * \sa IMG_isQOI
 * \sa IMG_isTIF
 * \sa IMG_isXCF
 * \sa IMG_isXPM
 * \sa IMG_isXV
 * \sa IMG_isWEBP
  }
function IMG_isBMP(src:PSDL_IOStream):longint;cdecl;external sdl3_image_lib;
{*
 * Detect GIF image data on a readable/seekable SDL_IOStream.
 *
 * This function attempts to determine if a file is a given filetype, reading
 * the least amount possible from the SDL_IOStream (usually a few bytes).
 *
 * There is no distinction made between "not the filetype in question" and
 * basic i/o errors.
 *
 * This function will always attempt to seek `src` back to where it started
 * when this function was called, but it will not report any errors in doing
 * so, but assuming seeking works, this means you can immediately use this
 * with a different IMG_isTYPE function, or load the image without further
 * seeking.
 *
 * You do not need to call this function to load data; SDL_image can work to
 * determine file type in many cases in its standard load functions.
 *
 * \param src a seekable/readable SDL_IOStream to provide image data.
 * \returns non-zero if this is GIF data, zero otherwise.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_isAVIF
 * \sa IMG_isICO
 * \sa IMG_isCUR
 * \sa IMG_isBMP
 * \sa IMG_isJPG
 * \sa IMG_isJXL
 * \sa IMG_isLBM
 * \sa IMG_isPCX
 * \sa IMG_isPNG
 * \sa IMG_isPNM
 * \sa IMG_isSVG
 * \sa IMG_isQOI
 * \sa IMG_isTIF
 * \sa IMG_isXCF
 * \sa IMG_isXPM
 * \sa IMG_isXV
 * \sa IMG_isWEBP
  }
function IMG_isGIF(src:PSDL_IOStream):longint;cdecl;external sdl3_image_lib;
{*
 * Detect JPG image data on a readable/seekable SDL_IOStream.
 *
 * This function attempts to determine if a file is a given filetype, reading
 * the least amount possible from the SDL_IOStream (usually a few bytes).
 *
 * There is no distinction made between "not the filetype in question" and
 * basic i/o errors.
 *
 * This function will always attempt to seek `src` back to where it started
 * when this function was called, but it will not report any errors in doing
 * so, but assuming seeking works, this means you can immediately use this
 * with a different IMG_isTYPE function, or load the image without further
 * seeking.
 *
 * You do not need to call this function to load data; SDL_image can work to
 * determine file type in many cases in its standard load functions.
 *
 * \param src a seekable/readable SDL_IOStream to provide image data.
 * \returns non-zero if this is JPG data, zero otherwise.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_isAVIF
 * \sa IMG_isICO
 * \sa IMG_isCUR
 * \sa IMG_isBMP
 * \sa IMG_isGIF
 * \sa IMG_isJXL
 * \sa IMG_isLBM
 * \sa IMG_isPCX
 * \sa IMG_isPNG
 * \sa IMG_isPNM
 * \sa IMG_isSVG
 * \sa IMG_isQOI
 * \sa IMG_isTIF
 * \sa IMG_isXCF
 * \sa IMG_isXPM
 * \sa IMG_isXV
 * \sa IMG_isWEBP
  }
function IMG_isJPG(src:PSDL_IOStream):longint;cdecl;external sdl3_image_lib;
{*
 * Detect JXL image data on a readable/seekable SDL_IOStream.
 *
 * This function attempts to determine if a file is a given filetype, reading
 * the least amount possible from the SDL_IOStream (usually a few bytes).
 *
 * There is no distinction made between "not the filetype in question" and
 * basic i/o errors.
 *
 * This function will always attempt to seek `src` back to where it started
 * when this function was called, but it will not report any errors in doing
 * so, but assuming seeking works, this means you can immediately use this
 * with a different IMG_isTYPE function, or load the image without further
 * seeking.
 *
 * You do not need to call this function to load data; SDL_image can work to
 * determine file type in many cases in its standard load functions.
 *
 * \param src a seekable/readable SDL_IOStream to provide image data.
 * \returns non-zero if this is JXL data, zero otherwise.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_isAVIF
 * \sa IMG_isICO
 * \sa IMG_isCUR
 * \sa IMG_isBMP
 * \sa IMG_isGIF
 * \sa IMG_isJPG
 * \sa IMG_isLBM
 * \sa IMG_isPCX
 * \sa IMG_isPNG
 * \sa IMG_isPNM
 * \sa IMG_isSVG
 * \sa IMG_isQOI
 * \sa IMG_isTIF
 * \sa IMG_isXCF
 * \sa IMG_isXPM
 * \sa IMG_isXV
 * \sa IMG_isWEBP
  }
function IMG_isJXL(src:PSDL_IOStream):longint;cdecl;external sdl3_image_lib;
{*
 * Detect LBM image data on a readable/seekable SDL_IOStream.
 *
 * This function attempts to determine if a file is a given filetype, reading
 * the least amount possible from the SDL_IOStream (usually a few bytes).
 *
 * There is no distinction made between "not the filetype in question" and
 * basic i/o errors.
 *
 * This function will always attempt to seek `src` back to where it started
 * when this function was called, but it will not report any errors in doing
 * so, but assuming seeking works, this means you can immediately use this
 * with a different IMG_isTYPE function, or load the image without further
 * seeking.
 *
 * You do not need to call this function to load data; SDL_image can work to
 * determine file type in many cases in its standard load functions.
 *
 * \param src a seekable/readable SDL_IOStream to provide image data.
 * \returns non-zero if this is LBM data, zero otherwise.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_isAVIF
 * \sa IMG_isICO
 * \sa IMG_isCUR
 * \sa IMG_isBMP
 * \sa IMG_isGIF
 * \sa IMG_isJPG
 * \sa IMG_isJXL
 * \sa IMG_isPCX
 * \sa IMG_isPNG
 * \sa IMG_isPNM
 * \sa IMG_isSVG
 * \sa IMG_isQOI
 * \sa IMG_isTIF
 * \sa IMG_isXCF
 * \sa IMG_isXPM
 * \sa IMG_isXV
 * \sa IMG_isWEBP
  }
function IMG_isLBM(src:PSDL_IOStream):longint;cdecl;external sdl3_image_lib;
{*
 * Detect PCX image data on a readable/seekable SDL_IOStream.
 *
 * This function attempts to determine if a file is a given filetype, reading
 * the least amount possible from the SDL_IOStream (usually a few bytes).
 *
 * There is no distinction made between "not the filetype in question" and
 * basic i/o errors.
 *
 * This function will always attempt to seek `src` back to where it started
 * when this function was called, but it will not report any errors in doing
 * so, but assuming seeking works, this means you can immediately use this
 * with a different IMG_isTYPE function, or load the image without further
 * seeking.
 *
 * You do not need to call this function to load data; SDL_image can work to
 * determine file type in many cases in its standard load functions.
 *
 * \param src a seekable/readable SDL_IOStream to provide image data.
 * \returns non-zero if this is PCX data, zero otherwise.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_isAVIF
 * \sa IMG_isICO
 * \sa IMG_isCUR
 * \sa IMG_isBMP
 * \sa IMG_isGIF
 * \sa IMG_isJPG
 * \sa IMG_isJXL
 * \sa IMG_isLBM
 * \sa IMG_isPNG
 * \sa IMG_isPNM
 * \sa IMG_isSVG
 * \sa IMG_isQOI
 * \sa IMG_isTIF
 * \sa IMG_isXCF
 * \sa IMG_isXPM
 * \sa IMG_isXV
 * \sa IMG_isWEBP
  }
function IMG_isPCX(src:PSDL_IOStream):longint;cdecl;external sdl3_image_lib;
{*
 * Detect PNG image data on a readable/seekable SDL_IOStream.
 *
 * This function attempts to determine if a file is a given filetype, reading
 * the least amount possible from the SDL_IOStream (usually a few bytes).
 *
 * There is no distinction made between "not the filetype in question" and
 * basic i/o errors.
 *
 * This function will always attempt to seek `src` back to where it started
 * when this function was called, but it will not report any errors in doing
 * so, but assuming seeking works, this means you can immediately use this
 * with a different IMG_isTYPE function, or load the image without further
 * seeking.
 *
 * You do not need to call this function to load data; SDL_image can work to
 * determine file type in many cases in its standard load functions.
 *
 * \param src a seekable/readable SDL_IOStream to provide image data.
 * \returns non-zero if this is PNG data, zero otherwise.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_isAVIF
 * \sa IMG_isICO
 * \sa IMG_isCUR
 * \sa IMG_isBMP
 * \sa IMG_isGIF
 * \sa IMG_isJPG
 * \sa IMG_isJXL
 * \sa IMG_isLBM
 * \sa IMG_isPCX
 * \sa IMG_isPNM
 * \sa IMG_isSVG
 * \sa IMG_isQOI
 * \sa IMG_isTIF
 * \sa IMG_isXCF
 * \sa IMG_isXPM
 * \sa IMG_isXV
 * \sa IMG_isWEBP
  }
function IMG_isPNG(src:PSDL_IOStream):longint;cdecl;external sdl3_image_lib;
{*
 * Detect PNM image data on a readable/seekable SDL_IOStream.
 *
 * This function attempts to determine if a file is a given filetype, reading
 * the least amount possible from the SDL_IOStream (usually a few bytes).
 *
 * There is no distinction made between "not the filetype in question" and
 * basic i/o errors.
 *
 * This function will always attempt to seek `src` back to where it started
 * when this function was called, but it will not report any errors in doing
 * so, but assuming seeking works, this means you can immediately use this
 * with a different IMG_isTYPE function, or load the image without further
 * seeking.
 *
 * You do not need to call this function to load data; SDL_image can work to
 * determine file type in many cases in its standard load functions.
 *
 * \param src a seekable/readable SDL_IOStream to provide image data.
 * \returns non-zero if this is PNM data, zero otherwise.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_isAVIF
 * \sa IMG_isICO
 * \sa IMG_isCUR
 * \sa IMG_isBMP
 * \sa IMG_isGIF
 * \sa IMG_isJPG
 * \sa IMG_isJXL
 * \sa IMG_isLBM
 * \sa IMG_isPCX
 * \sa IMG_isPNG
 * \sa IMG_isSVG
 * \sa IMG_isQOI
 * \sa IMG_isTIF
 * \sa IMG_isXCF
 * \sa IMG_isXPM
 * \sa IMG_isXV
 * \sa IMG_isWEBP
  }
function IMG_isPNM(src:PSDL_IOStream):longint;cdecl;external sdl3_image_lib;
{*
 * Detect SVG image data on a readable/seekable SDL_IOStream.
 *
 * This function attempts to determine if a file is a given filetype, reading
 * the least amount possible from the SDL_IOStream (usually a few bytes).
 *
 * There is no distinction made between "not the filetype in question" and
 * basic i/o errors.
 *
 * This function will always attempt to seek `src` back to where it started
 * when this function was called, but it will not report any errors in doing
 * so, but assuming seeking works, this means you can immediately use this
 * with a different IMG_isTYPE function, or load the image without further
 * seeking.
 *
 * You do not need to call this function to load data; SDL_image can work to
 * determine file type in many cases in its standard load functions.
 *
 * \param src a seekable/readable SDL_IOStream to provide image data.
 * \returns non-zero if this is SVG data, zero otherwise.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_isAVIF
 * \sa IMG_isICO
 * \sa IMG_isCUR
 * \sa IMG_isBMP
 * \sa IMG_isGIF
 * \sa IMG_isJPG
 * \sa IMG_isJXL
 * \sa IMG_isLBM
 * \sa IMG_isPCX
 * \sa IMG_isPNG
 * \sa IMG_isPNM
 * \sa IMG_isQOI
 * \sa IMG_isTIF
 * \sa IMG_isXCF
 * \sa IMG_isXPM
 * \sa IMG_isXV
 * \sa IMG_isWEBP
  }
function IMG_isSVG(src:PSDL_IOStream):longint;cdecl;external sdl3_image_lib;
{*
 * Detect QOI image data on a readable/seekable SDL_IOStream.
 *
 * This function attempts to determine if a file is a given filetype, reading
 * the least amount possible from the SDL_IOStream (usually a few bytes).
 *
 * There is no distinction made between "not the filetype in question" and
 * basic i/o errors.
 *
 * This function will always attempt to seek `src` back to where it started
 * when this function was called, but it will not report any errors in doing
 * so, but assuming seeking works, this means you can immediately use this
 * with a different IMG_isTYPE function, or load the image without further
 * seeking.
 *
 * You do not need to call this function to load data; SDL_image can work to
 * determine file type in many cases in its standard load functions.
 *
 * \param src a seekable/readable SDL_IOStream to provide image data.
 * \returns non-zero if this is QOI data, zero otherwise.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_isAVIF
 * \sa IMG_isICO
 * \sa IMG_isCUR
 * \sa IMG_isBMP
 * \sa IMG_isGIF
 * \sa IMG_isJPG
 * \sa IMG_isJXL
 * \sa IMG_isLBM
 * \sa IMG_isPCX
 * \sa IMG_isPNG
 * \sa IMG_isPNM
 * \sa IMG_isSVG
 * \sa IMG_isTIF
 * \sa IMG_isXCF
 * \sa IMG_isXPM
 * \sa IMG_isXV
 * \sa IMG_isWEBP
  }
function IMG_isQOI(src:PSDL_IOStream):longint;cdecl;external sdl3_image_lib;
{*
 * Detect TIFF image data on a readable/seekable SDL_IOStream.
 *
 * This function attempts to determine if a file is a given filetype, reading
 * the least amount possible from the SDL_IOStream (usually a few bytes).
 *
 * There is no distinction made between "not the filetype in question" and
 * basic i/o errors.
 *
 * This function will always attempt to seek `src` back to where it started
 * when this function was called, but it will not report any errors in doing
 * so, but assuming seeking works, this means you can immediately use this
 * with a different IMG_isTYPE function, or load the image without further
 * seeking.
 *
 * You do not need to call this function to load data; SDL_image can work to
 * determine file type in many cases in its standard load functions.
 *
 * \param src a seekable/readable SDL_IOStream to provide image data.
 * \returns non-zero if this is TIFF data, zero otherwise.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_isAVIF
 * \sa IMG_isICO
 * \sa IMG_isCUR
 * \sa IMG_isBMP
 * \sa IMG_isGIF
 * \sa IMG_isJPG
 * \sa IMG_isJXL
 * \sa IMG_isLBM
 * \sa IMG_isPCX
 * \sa IMG_isPNG
 * \sa IMG_isPNM
 * \sa IMG_isSVG
 * \sa IMG_isQOI
 * \sa IMG_isXCF
 * \sa IMG_isXPM
 * \sa IMG_isXV
 * \sa IMG_isWEBP
  }
function IMG_isTIF(src:PSDL_IOStream):longint;cdecl;external sdl3_image_lib;
{*
 * Detect XCF image data on a readable/seekable SDL_IOStream.
 *
 * This function attempts to determine if a file is a given filetype, reading
 * the least amount possible from the SDL_IOStream (usually a few bytes).
 *
 * There is no distinction made between "not the filetype in question" and
 * basic i/o errors.
 *
 * This function will always attempt to seek `src` back to where it started
 * when this function was called, but it will not report any errors in doing
 * so, but assuming seeking works, this means you can immediately use this
 * with a different IMG_isTYPE function, or load the image without further
 * seeking.
 *
 * You do not need to call this function to load data; SDL_image can work to
 * determine file type in many cases in its standard load functions.
 *
 * \param src a seekable/readable SDL_IOStream to provide image data.
 * \returns non-zero if this is XCF data, zero otherwise.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_isAVIF
 * \sa IMG_isICO
 * \sa IMG_isCUR
 * \sa IMG_isBMP
 * \sa IMG_isGIF
 * \sa IMG_isJPG
 * \sa IMG_isJXL
 * \sa IMG_isLBM
 * \sa IMG_isPCX
 * \sa IMG_isPNG
 * \sa IMG_isPNM
 * \sa IMG_isSVG
 * \sa IMG_isQOI
 * \sa IMG_isTIF
 * \sa IMG_isXPM
 * \sa IMG_isXV
 * \sa IMG_isWEBP
  }
function IMG_isXCF(src:PSDL_IOStream):longint;cdecl;external sdl3_image_lib;
{*
 * Detect XPM image data on a readable/seekable SDL_IOStream.
 *
 * This function attempts to determine if a file is a given filetype, reading
 * the least amount possible from the SDL_IOStream (usually a few bytes).
 *
 * There is no distinction made between "not the filetype in question" and
 * basic i/o errors.
 *
 * This function will always attempt to seek `src` back to where it started
 * when this function was called, but it will not report any errors in doing
 * so, but assuming seeking works, this means you can immediately use this
 * with a different IMG_isTYPE function, or load the image without further
 * seeking.
 *
 * You do not need to call this function to load data; SDL_image can work to
 * determine file type in many cases in its standard load functions.
 *
 * \param src a seekable/readable SDL_IOStream to provide image data.
 * \returns non-zero if this is XPM data, zero otherwise.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_isAVIF
 * \sa IMG_isICO
 * \sa IMG_isCUR
 * \sa IMG_isBMP
 * \sa IMG_isGIF
 * \sa IMG_isJPG
 * \sa IMG_isJXL
 * \sa IMG_isLBM
 * \sa IMG_isPCX
 * \sa IMG_isPNG
 * \sa IMG_isPNM
 * \sa IMG_isSVG
 * \sa IMG_isQOI
 * \sa IMG_isTIF
 * \sa IMG_isXCF
 * \sa IMG_isXV
 * \sa IMG_isWEBP
  }
function IMG_isXPM(src:PSDL_IOStream):longint;cdecl;external sdl3_image_lib;
{*
 * Detect XV image data on a readable/seekable SDL_IOStream.
 *
 * This function attempts to determine if a file is a given filetype, reading
 * the least amount possible from the SDL_IOStream (usually a few bytes).
 *
 * There is no distinction made between "not the filetype in question" and
 * basic i/o errors.
 *
 * This function will always attempt to seek `src` back to where it started
 * when this function was called, but it will not report any errors in doing
 * so, but assuming seeking works, this means you can immediately use this
 * with a different IMG_isTYPE function, or load the image without further
 * seeking.
 *
 * You do not need to call this function to load data; SDL_image can work to
 * determine file type in many cases in its standard load functions.
 *
 * \param src a seekable/readable SDL_IOStream to provide image data.
 * \returns non-zero if this is XV data, zero otherwise.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_isAVIF
 * \sa IMG_isICO
 * \sa IMG_isCUR
 * \sa IMG_isBMP
 * \sa IMG_isGIF
 * \sa IMG_isJPG
 * \sa IMG_isJXL
 * \sa IMG_isLBM
 * \sa IMG_isPCX
 * \sa IMG_isPNG
 * \sa IMG_isPNM
 * \sa IMG_isSVG
 * \sa IMG_isQOI
 * \sa IMG_isTIF
 * \sa IMG_isXCF
 * \sa IMG_isXPM
 * \sa IMG_isWEBP
  }
function IMG_isXV(src:PSDL_IOStream):longint;cdecl;external sdl3_image_lib;
{*
 * Detect WEBP image data on a readable/seekable SDL_IOStream.
 *
 * This function attempts to determine if a file is a given filetype, reading
 * the least amount possible from the SDL_IOStream (usually a few bytes).
 *
 * There is no distinction made between "not the filetype in question" and
 * basic i/o errors.
 *
 * This function will always attempt to seek `src` back to where it started
 * when this function was called, but it will not report any errors in doing
 * so, but assuming seeking works, this means you can immediately use this
 * with a different IMG_isTYPE function, or load the image without further
 * seeking.
 *
 * You do not need to call this function to load data; SDL_image can work to
 * determine file type in many cases in its standard load functions.
 *
 * \param src a seekable/readable SDL_IOStream to provide image data.
 * \returns non-zero if this is WEBP data, zero otherwise.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_isAVIF
 * \sa IMG_isICO
 * \sa IMG_isCUR
 * \sa IMG_isBMP
 * \sa IMG_isGIF
 * \sa IMG_isJPG
 * \sa IMG_isJXL
 * \sa IMG_isLBM
 * \sa IMG_isPCX
 * \sa IMG_isPNG
 * \sa IMG_isPNM
 * \sa IMG_isSVG
 * \sa IMG_isQOI
 * \sa IMG_isTIF
 * \sa IMG_isXCF
 * \sa IMG_isXPM
 * \sa IMG_isXV
  }
function IMG_isWEBP(src:PSDL_IOStream):longint;cdecl;external sdl3_image_lib;
{*
 * Load a AVIF image directly.
 *
 * If you know you definitely have a AVIF image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream to load image data from.
 * \returns SDL surface, or NULL on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadICO_IO
 * \sa IMG_LoadCUR_IO
 * \sa IMG_LoadBMP_IO
 * \sa IMG_LoadGIF_IO
 * \sa IMG_LoadJPG_IO
 * \sa IMG_LoadJXL_IO
 * \sa IMG_LoadLBM_IO
 * \sa IMG_LoadPCX_IO
 * \sa IMG_LoadPNG_IO
 * \sa IMG_LoadPNM_IO
 * \sa IMG_LoadSVG_IO
 * \sa IMG_LoadQOI_IO
 * \sa IMG_LoadTGA_IO
 * \sa IMG_LoadTIF_IO
 * \sa IMG_LoadXCF_IO
 * \sa IMG_LoadXPM_IO
 * \sa IMG_LoadXV_IO
 * \sa IMG_LoadWEBP_IO
  }
function IMG_LoadAVIF_IO(src:PSDL_IOStream):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load a ICO image directly.
 *
 * If you know you definitely have a ICO image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream to load image data from.
 * \returns SDL surface, or NULL on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAVIF_IO
 * \sa IMG_LoadCUR_IO
 * \sa IMG_LoadBMP_IO
 * \sa IMG_LoadGIF_IO
 * \sa IMG_LoadJPG_IO
 * \sa IMG_LoadJXL_IO
 * \sa IMG_LoadLBM_IO
 * \sa IMG_LoadPCX_IO
 * \sa IMG_LoadPNG_IO
 * \sa IMG_LoadPNM_IO
 * \sa IMG_LoadSVG_IO
 * \sa IMG_LoadQOI_IO
 * \sa IMG_LoadTGA_IO
 * \sa IMG_LoadTIF_IO
 * \sa IMG_LoadXCF_IO
 * \sa IMG_LoadXPM_IO
 * \sa IMG_LoadXV_IO
 * \sa IMG_LoadWEBP_IO
  }
function IMG_LoadICO_IO(src:PSDL_IOStream):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load a CUR image directly.
 *
 * If you know you definitely have a CUR image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream to load image data from.
 * \returns SDL surface, or NULL on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAVIF_IO
 * \sa IMG_LoadICO_IO
 * \sa IMG_LoadBMP_IO
 * \sa IMG_LoadGIF_IO
 * \sa IMG_LoadJPG_IO
 * \sa IMG_LoadJXL_IO
 * \sa IMG_LoadLBM_IO
 * \sa IMG_LoadPCX_IO
 * \sa IMG_LoadPNG_IO
 * \sa IMG_LoadPNM_IO
 * \sa IMG_LoadSVG_IO
 * \sa IMG_LoadQOI_IO
 * \sa IMG_LoadTGA_IO
 * \sa IMG_LoadTIF_IO
 * \sa IMG_LoadXCF_IO
 * \sa IMG_LoadXPM_IO
 * \sa IMG_LoadXV_IO
 * \sa IMG_LoadWEBP_IO
  }
function IMG_LoadCUR_IO(src:PSDL_IOStream):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load a BMP image directly.
 *
 * If you know you definitely have a BMP image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream to load image data from.
 * \returns SDL surface, or NULL on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAVIF_IO
 * \sa IMG_LoadICO_IO
 * \sa IMG_LoadCUR_IO
 * \sa IMG_LoadGIF_IO
 * \sa IMG_LoadJPG_IO
 * \sa IMG_LoadJXL_IO
 * \sa IMG_LoadLBM_IO
 * \sa IMG_LoadPCX_IO
 * \sa IMG_LoadPNG_IO
 * \sa IMG_LoadPNM_IO
 * \sa IMG_LoadSVG_IO
 * \sa IMG_LoadQOI_IO
 * \sa IMG_LoadTGA_IO
 * \sa IMG_LoadTIF_IO
 * \sa IMG_LoadXCF_IO
 * \sa IMG_LoadXPM_IO
 * \sa IMG_LoadXV_IO
 * \sa IMG_LoadWEBP_IO
  }
function IMG_LoadBMP_IO(src:PSDL_IOStream):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load a GIF image directly.
 *
 * If you know you definitely have a GIF image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream to load image data from.
 * \returns SDL surface, or NULL on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAVIF_IO
 * \sa IMG_LoadICO_IO
 * \sa IMG_LoadCUR_IO
 * \sa IMG_LoadBMP_IO
 * \sa IMG_LoadJPG_IO
 * \sa IMG_LoadJXL_IO
 * \sa IMG_LoadLBM_IO
 * \sa IMG_LoadPCX_IO
 * \sa IMG_LoadPNG_IO
 * \sa IMG_LoadPNM_IO
 * \sa IMG_LoadSVG_IO
 * \sa IMG_LoadQOI_IO
 * \sa IMG_LoadTGA_IO
 * \sa IMG_LoadTIF_IO
 * \sa IMG_LoadXCF_IO
 * \sa IMG_LoadXPM_IO
 * \sa IMG_LoadXV_IO
 * \sa IMG_LoadWEBP_IO
  }
function IMG_LoadGIF_IO(src:PSDL_IOStream):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load a JPG image directly.
 *
 * If you know you definitely have a JPG image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream to load image data from.
 * \returns SDL surface, or NULL on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAVIF_IO
 * \sa IMG_LoadICO_IO
 * \sa IMG_LoadCUR_IO
 * \sa IMG_LoadBMP_IO
 * \sa IMG_LoadGIF_IO
 * \sa IMG_LoadJXL_IO
 * \sa IMG_LoadLBM_IO
 * \sa IMG_LoadPCX_IO
 * \sa IMG_LoadPNG_IO
 * \sa IMG_LoadPNM_IO
 * \sa IMG_LoadSVG_IO
 * \sa IMG_LoadQOI_IO
 * \sa IMG_LoadTGA_IO
 * \sa IMG_LoadTIF_IO
 * \sa IMG_LoadXCF_IO
 * \sa IMG_LoadXPM_IO
 * \sa IMG_LoadXV_IO
 * \sa IMG_LoadWEBP_IO
  }
function IMG_LoadJPG_IO(src:PSDL_IOStream):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load a JXL image directly.
 *
 * If you know you definitely have a JXL image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream to load image data from.
 * \returns SDL surface, or NULL on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAVIF_IO
 * \sa IMG_LoadICO_IO
 * \sa IMG_LoadCUR_IO
 * \sa IMG_LoadBMP_IO
 * \sa IMG_LoadGIF_IO
 * \sa IMG_LoadJPG_IO
 * \sa IMG_LoadLBM_IO
 * \sa IMG_LoadPCX_IO
 * \sa IMG_LoadPNG_IO
 * \sa IMG_LoadPNM_IO
 * \sa IMG_LoadSVG_IO
 * \sa IMG_LoadQOI_IO
 * \sa IMG_LoadTGA_IO
 * \sa IMG_LoadTIF_IO
 * \sa IMG_LoadXCF_IO
 * \sa IMG_LoadXPM_IO
 * \sa IMG_LoadXV_IO
 * \sa IMG_LoadWEBP_IO
  }
function IMG_LoadJXL_IO(src:PSDL_IOStream):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load a LBM image directly.
 *
 * If you know you definitely have a LBM image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream to load image data from.
 * \returns SDL surface, or NULL on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAVIF_IO
 * \sa IMG_LoadICO_IO
 * \sa IMG_LoadCUR_IO
 * \sa IMG_LoadBMP_IO
 * \sa IMG_LoadGIF_IO
 * \sa IMG_LoadJPG_IO
 * \sa IMG_LoadJXL_IO
 * \sa IMG_LoadPCX_IO
 * \sa IMG_LoadPNG_IO
 * \sa IMG_LoadPNM_IO
 * \sa IMG_LoadSVG_IO
 * \sa IMG_LoadQOI_IO
 * \sa IMG_LoadTGA_IO
 * \sa IMG_LoadTIF_IO
 * \sa IMG_LoadXCF_IO
 * \sa IMG_LoadXPM_IO
 * \sa IMG_LoadXV_IO
 * \sa IMG_LoadWEBP_IO
  }
function IMG_LoadLBM_IO(src:PSDL_IOStream):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load a PCX image directly.
 *
 * If you know you definitely have a PCX image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream to load image data from.
 * \returns SDL surface, or NULL on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAVIF_IO
 * \sa IMG_LoadICO_IO
 * \sa IMG_LoadCUR_IO
 * \sa IMG_LoadBMP_IO
 * \sa IMG_LoadGIF_IO
 * \sa IMG_LoadJPG_IO
 * \sa IMG_LoadJXL_IO
 * \sa IMG_LoadLBM_IO
 * \sa IMG_LoadPNG_IO
 * \sa IMG_LoadPNM_IO
 * \sa IMG_LoadSVG_IO
 * \sa IMG_LoadQOI_IO
 * \sa IMG_LoadTGA_IO
 * \sa IMG_LoadTIF_IO
 * \sa IMG_LoadXCF_IO
 * \sa IMG_LoadXPM_IO
 * \sa IMG_LoadXV_IO
 * \sa IMG_LoadWEBP_IO
  }
function IMG_LoadPCX_IO(src:PSDL_IOStream):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load a PNG image directly.
 *
 * If you know you definitely have a PNG image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream to load image data from.
 * \returns SDL surface, or NULL on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAVIF_IO
 * \sa IMG_LoadICO_IO
 * \sa IMG_LoadCUR_IO
 * \sa IMG_LoadBMP_IO
 * \sa IMG_LoadGIF_IO
 * \sa IMG_LoadJPG_IO
 * \sa IMG_LoadJXL_IO
 * \sa IMG_LoadLBM_IO
 * \sa IMG_LoadPCX_IO
 * \sa IMG_LoadPNM_IO
 * \sa IMG_LoadSVG_IO
 * \sa IMG_LoadQOI_IO
 * \sa IMG_LoadTGA_IO
 * \sa IMG_LoadTIF_IO
 * \sa IMG_LoadXCF_IO
 * \sa IMG_LoadXPM_IO
 * \sa IMG_LoadXV_IO
 * \sa IMG_LoadWEBP_IO
  }
function IMG_LoadPNG_IO(src:PSDL_IOStream):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load a PNM image directly.
 *
 * If you know you definitely have a PNM image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream to load image data from.
 * \returns SDL surface, or NULL on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAVIF_IO
 * \sa IMG_LoadICO_IO
 * \sa IMG_LoadCUR_IO
 * \sa IMG_LoadBMP_IO
 * \sa IMG_LoadGIF_IO
 * \sa IMG_LoadJPG_IO
 * \sa IMG_LoadJXL_IO
 * \sa IMG_LoadLBM_IO
 * \sa IMG_LoadPCX_IO
 * \sa IMG_LoadPNG_IO
 * \sa IMG_LoadSVG_IO
 * \sa IMG_LoadQOI_IO
 * \sa IMG_LoadTGA_IO
 * \sa IMG_LoadTIF_IO
 * \sa IMG_LoadXCF_IO
 * \sa IMG_LoadXPM_IO
 * \sa IMG_LoadXV_IO
 * \sa IMG_LoadWEBP_IO
  }
function IMG_LoadPNM_IO(src:PSDL_IOStream):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load a SVG image directly.
 *
 * If you know you definitely have a SVG image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream to load image data from.
 * \returns SDL surface, or NULL on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAVIF_IO
 * \sa IMG_LoadICO_IO
 * \sa IMG_LoadCUR_IO
 * \sa IMG_LoadBMP_IO
 * \sa IMG_LoadGIF_IO
 * \sa IMG_LoadJPG_IO
 * \sa IMG_LoadJXL_IO
 * \sa IMG_LoadLBM_IO
 * \sa IMG_LoadPCX_IO
 * \sa IMG_LoadPNG_IO
 * \sa IMG_LoadPNM_IO
 * \sa IMG_LoadQOI_IO
 * \sa IMG_LoadTGA_IO
 * \sa IMG_LoadTIF_IO
 * \sa IMG_LoadXCF_IO
 * \sa IMG_LoadXPM_IO
 * \sa IMG_LoadXV_IO
 * \sa IMG_LoadWEBP_IO
  }
function IMG_LoadSVG_IO(src:PSDL_IOStream):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load a QOI image directly.
 *
 * If you know you definitely have a QOI image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream to load image data from.
 * \returns SDL surface, or NULL on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAVIF_IO
 * \sa IMG_LoadICO_IO
 * \sa IMG_LoadCUR_IO
 * \sa IMG_LoadBMP_IO
 * \sa IMG_LoadGIF_IO
 * \sa IMG_LoadJPG_IO
 * \sa IMG_LoadJXL_IO
 * \sa IMG_LoadLBM_IO
 * \sa IMG_LoadPCX_IO
 * \sa IMG_LoadPNG_IO
 * \sa IMG_LoadPNM_IO
 * \sa IMG_LoadSVG_IO
 * \sa IMG_LoadTGA_IO
 * \sa IMG_LoadTIF_IO
 * \sa IMG_LoadXCF_IO
 * \sa IMG_LoadXPM_IO
 * \sa IMG_LoadXV_IO
 * \sa IMG_LoadWEBP_IO
  }
function IMG_LoadQOI_IO(src:PSDL_IOStream):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load a TGA image directly.
 *
 * If you know you definitely have a TGA image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream to load image data from.
 * \returns SDL surface, or NULL on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAVIF_IO
 * \sa IMG_LoadICO_IO
 * \sa IMG_LoadCUR_IO
 * \sa IMG_LoadBMP_IO
 * \sa IMG_LoadGIF_IO
 * \sa IMG_LoadJPG_IO
 * \sa IMG_LoadJXL_IO
 * \sa IMG_LoadLBM_IO
 * \sa IMG_LoadPCX_IO
 * \sa IMG_LoadPNG_IO
 * \sa IMG_LoadPNM_IO
 * \sa IMG_LoadSVG_IO
 * \sa IMG_LoadQOI_IO
 * \sa IMG_LoadTIF_IO
 * \sa IMG_LoadXCF_IO
 * \sa IMG_LoadXPM_IO
 * \sa IMG_LoadXV_IO
 * \sa IMG_LoadWEBP_IO
  }
function IMG_LoadTGA_IO(src:PSDL_IOStream):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load a TIFF image directly.
 *
 * If you know you definitely have a TIFF image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream to load image data from.
 * \returns SDL surface, or NULL on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAVIF_IO
 * \sa IMG_LoadICO_IO
 * \sa IMG_LoadCUR_IO
 * \sa IMG_LoadBMP_IO
 * \sa IMG_LoadGIF_IO
 * \sa IMG_LoadJPG_IO
 * \sa IMG_LoadJXL_IO
 * \sa IMG_LoadLBM_IO
 * \sa IMG_LoadPCX_IO
 * \sa IMG_LoadPNG_IO
 * \sa IMG_LoadPNM_IO
 * \sa IMG_LoadSVG_IO
 * \sa IMG_LoadQOI_IO
 * \sa IMG_LoadTGA_IO
 * \sa IMG_LoadXCF_IO
 * \sa IMG_LoadXPM_IO
 * \sa IMG_LoadXV_IO
 * \sa IMG_LoadWEBP_IO
  }
function IMG_LoadTIF_IO(src:PSDL_IOStream):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load a XCF image directly.
 *
 * If you know you definitely have a XCF image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream to load image data from.
 * \returns SDL surface, or NULL on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAVIF_IO
 * \sa IMG_LoadICO_IO
 * \sa IMG_LoadCUR_IO
 * \sa IMG_LoadBMP_IO
 * \sa IMG_LoadGIF_IO
 * \sa IMG_LoadJPG_IO
 * \sa IMG_LoadJXL_IO
 * \sa IMG_LoadLBM_IO
 * \sa IMG_LoadPCX_IO
 * \sa IMG_LoadPNG_IO
 * \sa IMG_LoadPNM_IO
 * \sa IMG_LoadSVG_IO
 * \sa IMG_LoadQOI_IO
 * \sa IMG_LoadTGA_IO
 * \sa IMG_LoadTIF_IO
 * \sa IMG_LoadXPM_IO
 * \sa IMG_LoadXV_IO
 * \sa IMG_LoadWEBP_IO
  }
function IMG_LoadXCF_IO(src:PSDL_IOStream):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load a XPM image directly.
 *
 * If you know you definitely have a XPM image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream to load image data from.
 * \returns SDL surface, or NULL on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAVIF_IO
 * \sa IMG_LoadICO_IO
 * \sa IMG_LoadCUR_IO
 * \sa IMG_LoadBMP_IO
 * \sa IMG_LoadGIF_IO
 * \sa IMG_LoadJPG_IO
 * \sa IMG_LoadJXL_IO
 * \sa IMG_LoadLBM_IO
 * \sa IMG_LoadPCX_IO
 * \sa IMG_LoadPNG_IO
 * \sa IMG_LoadPNM_IO
 * \sa IMG_LoadSVG_IO
 * \sa IMG_LoadQOI_IO
 * \sa IMG_LoadTGA_IO
 * \sa IMG_LoadTIF_IO
 * \sa IMG_LoadXCF_IO
 * \sa IMG_LoadXV_IO
 * \sa IMG_LoadWEBP_IO
  }
function IMG_LoadXPM_IO(src:PSDL_IOStream):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load a XV image directly.
 *
 * If you know you definitely have a XV image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream to load image data from.
 * \returns SDL surface, or NULL on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAVIF_IO
 * \sa IMG_LoadICO_IO
 * \sa IMG_LoadCUR_IO
 * \sa IMG_LoadBMP_IO
 * \sa IMG_LoadGIF_IO
 * \sa IMG_LoadJPG_IO
 * \sa IMG_LoadJXL_IO
 * \sa IMG_LoadLBM_IO
 * \sa IMG_LoadPCX_IO
 * \sa IMG_LoadPNG_IO
 * \sa IMG_LoadPNM_IO
 * \sa IMG_LoadSVG_IO
 * \sa IMG_LoadQOI_IO
 * \sa IMG_LoadTGA_IO
 * \sa IMG_LoadTIF_IO
 * \sa IMG_LoadXCF_IO
 * \sa IMG_LoadXPM_IO
 * \sa IMG_LoadWEBP_IO
  }
function IMG_LoadXV_IO(src:PSDL_IOStream):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load a WEBP image directly.
 *
 * If you know you definitely have a WEBP image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream to load image data from.
 * \returns SDL surface, or NULL on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAVIF_IO
 * \sa IMG_LoadICO_IO
 * \sa IMG_LoadCUR_IO
 * \sa IMG_LoadBMP_IO
 * \sa IMG_LoadGIF_IO
 * \sa IMG_LoadJPG_IO
 * \sa IMG_LoadJXL_IO
 * \sa IMG_LoadLBM_IO
 * \sa IMG_LoadPCX_IO
 * \sa IMG_LoadPNG_IO
 * \sa IMG_LoadPNM_IO
 * \sa IMG_LoadSVG_IO
 * \sa IMG_LoadQOI_IO
 * \sa IMG_LoadTGA_IO
 * \sa IMG_LoadTIF_IO
 * \sa IMG_LoadXCF_IO
 * \sa IMG_LoadXPM_IO
 * \sa IMG_LoadXV_IO
  }
function IMG_LoadWEBP_IO(src:PSDL_IOStream):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load an SVG image, scaled to a specific size.
 *
 * Since SVG files are resolution-independent, you specify the size you would
 * like the output image to be and it will be generated at those dimensions.
 *
 * Either width or height may be 0 and the image will be auto-sized to
 * preserve aspect ratio.
 *
 * When done with the returned surface, the app should dispose of it with a
 * call to SDL_DestroySurface().
 *
 * \param src an SDL_IOStream to load SVG data from.
 * \param width desired width of the generated surface, in pixels.
 * \param height desired height of the generated surface, in pixels.
 * \returns a new SDL surface, or NULL on error.
 *
 * \since This function is available since SDL_image 3.0.0.
  }
function IMG_LoadSizedSVG_IO(src:PSDL_IOStream; width:longint; height:longint):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load an XPM image from a memory array.
 *
 * The returned surface will be an 8bpp indexed surface, if possible,
 * otherwise it will be 32bpp. If you always want 32-bit data, use
 * IMG_ReadXPMFromArrayToRGB888() instead.
 *
 * When done with the returned surface, the app should dispose of it with a
 * call to SDL_DestroySurface().
 *
 * \param xpm a null-terminated array of strings that comprise XPM data.
 * \returns a new SDL surface, or NULL on error.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_ReadXPMFromArrayToRGB888
  }
function IMG_ReadXPMFromArray(xpm:PPchar):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Load an XPM image from a memory array.
 *
 * The returned surface will always be a 32-bit RGB surface. If you want 8-bit
 * indexed colors (and the XPM data allows it), use IMG_ReadXPMFromArray()
 * instead.
 *
 * When done with the returned surface, the app should dispose of it with a
 * call to SDL_DestroySurface().
 *
 * \param xpm a null-terminated array of strings that comprise XPM data.
 * \returns a new SDL surface, or NULL on error.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_ReadXPMFromArray
  }
function IMG_ReadXPMFromArrayToRGB888(xpm:PPchar):PSDL_Surface;cdecl;external sdl3_image_lib;
{*
 * Save an SDL_Surface into a AVIF image file.
 *
 * If the file already exists, it will be overwritten.
 *
 * \param surface the SDL surface to save
 * \param file path on the filesystem to write new file to.
 * \param quality the desired quality, ranging between 0 (lowest) and 100
 *                (highest)
 * \returns 0 if successful, -1 on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_SaveAVIF_IO
  }
(* Const before type ignored *)
function IMG_SaveAVIF(surface:PSDL_Surface; file_:Pchar; quality:longint):longint;cdecl;external sdl3_image_lib;
{*
 * Save an SDL_Surface into AVIF image data, via an SDL_IOStream.
 *
 * If you just want to save to a filename, you can use IMG_SaveAVIF() instead.
 *
 * If `closeio` is SDL_TRUE, `dst` will be closed before returning, whether
 * this function succeeds or not.
 *
 * \param surface the SDL surface to save
 * \param dst the SDL_IOStream to save the image data to.
 * \param closeio SDL_TRUE to close/free the SDL_IOStream before returning,
 *                SDL_FALSE to leave it open.
 * \param quality the desired quality, ranging between 0 (lowest) and 100
 *                (highest)
 * \returns 0 if successful, -1 on error.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_SaveAVIF
  }
function IMG_SaveAVIF_IO(surface:PSDL_Surface; dst:PSDL_IOStream; closeio:longint; quality:longint):longint;cdecl;external sdl3_image_lib;
{*
 * Save an SDL_Surface into a PNG image file.
 *
 * If the file already exists, it will be overwritten.
 *
 * \param surface the SDL surface to save
 * \param file path on the filesystem to write new file to.
 * \returns 0 if successful, -1 on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_SavePNG_IO
  }
(* Const before type ignored *)
function IMG_SavePNG(surface:PSDL_Surface; file_:Pchar):longint;cdecl;external sdl3_image_lib;
{*
 * Save an SDL_Surface into PNG image data, via an SDL_IOStream.
 *
 * If you just want to save to a filename, you can use IMG_SavePNG() instead.
 *
 * If `closeio` is SDL_TRUE, `dst` will be closed before returning, whether
 * this function succeeds or not.
 *
 * \param surface the SDL surface to save
 * \param dst the SDL_IOStream to save the image data to.
 * \param closeio SDL_TRUE to close/free the SDL_IOStream before returning,
 *                SDL_FALSE to leave it open.
 * \returns 0 if successful, -1 on error.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_SavePNG
  }
function IMG_SavePNG_IO(surface:PSDL_Surface; dst:PSDL_IOStream; closeio:longint):longint;cdecl;external sdl3_image_lib;
{*
 * Save an SDL_Surface into a JPEG image file.
 *
 * If the file already exists, it will be overwritten.
 *
 * \param surface the SDL surface to save
 * \param file path on the filesystem to write new file to.
 * \param quality [0; 33] is Lowest quality, [34; 66] is Middle quality, [67;
 *                100] is Highest quality
 * \returns 0 if successful, -1 on error
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_SaveJPG_IO
  }
(* Const before type ignored *)
function IMG_SaveJPG(surface:PSDL_Surface; file_:Pchar; quality:longint):longint;cdecl;external sdl3_image_lib;
{*
 * Save an SDL_Surface into JPEG image data, via an SDL_IOStream.
 *
 * If you just want to save to a filename, you can use IMG_SaveJPG() instead.
 *
 * If `closeio` is SDL_TRUE, `dst` will be closed before returning, whether
 * this function succeeds or not.
 *
 * \param surface the SDL surface to save
 * \param dst the SDL_IOStream to save the image data to.
 * \param closeio SDL_TRUE to close/free the SDL_IOStream before returning,
 *                SDL_FALSE to leave it open.
 * \param quality [0; 33] is Lowest quality, [34; 66] is Middle quality, [67;
 *                100] is Highest quality
 * \returns 0 if successful, -1 on error.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_SaveJPG
  }
function IMG_SaveJPG_IO(surface:PSDL_Surface; dst:PSDL_IOStream; closeio:longint; quality:longint):longint;cdecl;external sdl3_image_lib;
{*
 * Animated image support Currently only animated GIFs are supported.
  }
type
  PIMG_Animation = ^TIMG_Animation;
  TIMG_Animation = record
      w : longint;
      h : longint;
      count : longint;
      frames : ^PSDL_Surface;
      delays : Plongint;
    end;
{*
 * Load an animation from a file.
 *
 * When done with the returned animation, the app should dispose of it with a
 * call to IMG_FreeAnimation().
 *
 * \param file path on the filesystem containing an animated image.
 * \returns a new IMG_Animation, or NULL on error.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_FreeAnimation
  }
(* Const before type ignored *)

function IMG_LoadAnimation(file_:Pchar):PIMG_Animation;cdecl;external sdl3_image_lib;
{*
 * Load an animation from an SDL_IOStream.
 *
 * If `closeio` is SDL_TRUE, `src` will be closed before returning, whether
 * this function succeeds or not. SDL_image reads everything it needs from
 * `src` during this call in any case.
 *
 * When done with the returned animation, the app should dispose of it with a
 * call to IMG_FreeAnimation().
 *
 * \param src an SDL_IOStream that data will be read from.
 * \param closeio SDL_TRUE to close/free the SDL_IOStream before returning,
 *                SDL_FALSE to leave it open.
 * \returns a new IMG_Animation, or NULL on error.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_FreeAnimation
  }
function IMG_LoadAnimation_IO(src:PSDL_IOStream; closeio:TSDL_bool):PIMG_Animation;cdecl;external sdl3_image_lib;
{*
 * Load an animation from an SDL datasource
 *
 * Even though this function accepts a file type, SDL_image may still try
 * other decoders that are capable of detecting file type from the contents of
 * the image data, but may rely on the caller-provided type string for formats
 * that it cannot autodetect. If `type` is NULL, SDL_image will rely solely on
 * its ability to guess the format.
 *
 * If `closeio` is SDL_TRUE, `src` will be closed before returning, whether
 * this function succeeds or not. SDL_image reads everything it needs from
 * `src` during this call in any case.
 *
 * When done with the returned animation, the app should dispose of it with a
 * call to IMG_FreeAnimation().
 *
 * \param src an SDL_IOStream that data will be read from.
 * \param closeio SDL_TRUE to close/free the SDL_IOStream before returning,
 *                SDL_FALSE to leave it open.
 * \param type a filename extension that represent this data ("GIF", etc).
 * \returns a new IMG_Animation, or NULL on error.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAnimation
 * \sa IMG_LoadAnimation_IO
 * \sa IMG_FreeAnimation
  }
(* Const before type ignored *)
function IMG_LoadAnimationTyped_IO(src:PSDL_IOStream; closeio:TSDL_bool; _type:Pchar):PIMG_Animation;cdecl;external sdl3_image_lib;
{*
 * Dispose of an IMG_Animation and free its resources.
 *
 * The provided `anim` pointer is not valid once this call returns.
 *
 * \param anim IMG_Animation to dispose of.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAnimation
 * \sa IMG_LoadAnimation_IO
 * \sa IMG_LoadAnimationTyped_IO
  }
procedure IMG_FreeAnimation(anim:PIMG_Animation);cdecl;external sdl3_image_lib;
{*
 * Load a GIF animation directly.
 *
 * If you know you definitely have a GIF image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream that data will be read from.
 * \returns a new IMG_Animation, or NULL on error.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAnimation
 * \sa IMG_LoadAnimation_IO
 * \sa IMG_LoadAnimationTyped_IO
 * \sa IMG_FreeAnimation
  }
function IMG_LoadGIFAnimation_IO(src:PSDL_IOStream):PIMG_Animation;cdecl;external sdl3_image_lib;
{*
 * Load a WEBP animation directly.
 *
 * If you know you definitely have a WEBP image, you can call this function,
 * which will skip SDL_image's file format detection routines. Generally it's
 * better to use the abstract interfaces; also, there is only an SDL_IOStream
 * interface available here.
 *
 * \param src an SDL_IOStream that data will be read from.
 * \returns a new IMG_Animation, or NULL on error.
 *
 * \since This function is available since SDL_image 3.0.0.
 *
 * \sa IMG_LoadAnimation
 * \sa IMG_LoadAnimation_IO
 * \sa IMG_LoadAnimationTyped_IO
 * \sa IMG_FreeAnimation
  }
function IMG_LoadWEBPAnimation_IO(src:PSDL_IOStream):PIMG_Animation;cdecl;external sdl3_image_lib;
{*
 * Report SDL_image errors
 *
 * \sa IMG_GetError
  }
//const
//  IMG_SetError = SDL_SetError;  
//IMG_GetError = SDL_GetError;
  function IMG_SetError(fmt: PChar): longint; varargs;  cdecl; external sdl3_image_lib name 'SDL_SetError';
  function IMG_GetError: PChar; cdecl; external sdl3_image_lib name 'SDL_GetError';
{*
 * Get last SDL_image error
 *
 * \sa IMG_SetError
  }
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{//$include <SDL3/SDL_close_code.h>}
//{$endif}
{ SDL_IMAGE_H_  }

implementation

{ was #define dname def_expr }
function SDL_IMAGE_COMPILEDVERSION : longint; { return type might be wrong }
  begin
    SDL_IMAGE_COMPILEDVERSION:=SDL_VERSIONNUM(SDL_IMAGE_MAJOR_VERSION,SDL_IMAGE_MINOR_VERSION,SDL_IMAGE_PATCHLEVEL);
  end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_IMAGE_VERSION_ATLEAST(X,Y,Z : longint) : longint;
begin
//  SDL_IMAGE_VERSION_ATLEAST:=((SDL_IMAGE_MAJOR_VERSION>=X) and ((SDL_IMAGE_MAJOR_VERSION>(X or SDL_IMAGE_MINOR_VERSION))>=Y)) and (((SDL_IMAGE_MAJOR_VERSION>(X or SDL_IMAGE_MINOR_VERSION))>(Y or SDL_IMAGE_PATCHLEVEL))>=Z);
end;


end.
