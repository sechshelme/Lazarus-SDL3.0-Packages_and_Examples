unit SDL_stdinc;

interface

uses
  crt;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

const
  SDL_SIZE_MAX = SIZE_MAX;  
{$else}

{ was #define dname def_expr }
function SDL_SIZE_MAX : Tsize_t;  

{$endif}
{*
 * Check if the compiler supports a given builtin.
 * Supported by virtually all clang versions and recent gcc. Use this
 * instead of checking the clang version if possible.
  }
{$ifdef __has_builtin}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_HAS_BUILTIN(x : longint) : longint;

{$else}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_HAS_BUILTIN(x : longint) : longint;

{$endif}
{*
 * The number of elements in an array.
 *
 * This macro looks like it double-evaluates the argument, but it does so
 * inside of `sizeof`, so there are no side-effects here, as expressions do
 * not actually run any code in these cases.
 *
 * \since This macro is available since SDL 3.0.0.
  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_arraysize(array : longint) : longint;

{*
 * Macro useful for building other macros with strings in them.
 *
 * For example:
 *
 * ```c
 * #define LOG_ERROR(X) OutputDebugString(SDL_STRINGIFY_ARG(__FUNCTION__) ": " X "\n")`
 * ```
 *
 * \since This macro is available since SDL 3.0.0.
  }
{#define SDL_STRINGIFY_ARG(arg)  #arg }
{*
 *  \name Cast operators
 *
 *  Use proper C++ casts when compiled as C++ to be compatible with the option
 *  -Wold-style-cast of GCC (and -Werror=old-style-cast in GCC 4.2 and above).
  }
{ @  }
{$ifdef SDL_WIKI_DOCUMENTATION_SECTION}
{*
 * Handle a Reinterpret Cast properly whether using C or C++.
 *
 * If compiled as C++, this macro offers a proper C++ reinterpret_cast<>.
 *
 * If compiled as C, this macro does a normal C-style cast.
 *
 * This is helpful to avoid compiler warnings in C++.
 *
 * \param type the type to cast the expression to.
 * \param expression the expression to cast to a different type.
 * \returns `expression`, cast to `type`.
 *
 * \threadsafety It is safe to call this macro from any thread.
 *
 * \since This macro is available since SDL 3.0.0.
 *
 * \sa SDL_static_cast
 * \sa SDL_const_cast
  }
{#define SDL_reinterpret_cast(type, expression) reinterpret_cast<type>(expression)  /* or `((type)(expression))` in C */ }
{*
 * Handle a Static Cast properly whether using C or C++.
 *
 * If compiled as C++, this macro offers a proper C++ static_cast<>.
 *
 * If compiled as C, this macro does a normal C-style cast.
 *
 * This is helpful to avoid compiler warnings in C++.
 *
 * \param type the type to cast the expression to.
 * \param expression the expression to cast to a different type.
 * \returns `expression`, cast to `type`.
 *
 * \threadsafety It is safe to call this macro from any thread.
 *
 * \since This macro is available since SDL 3.0.0.
 *
 * \sa SDL_reinterpret_cast
 * \sa SDL_const_cast
  }
{#define SDL_static_cast(type, expression) static_cast<type>(expression)  /* or `((type)(expression))` in C */ }
{*
 * Handle a Const Cast properly whether using C or C++.
 *
 * If compiled as C++, this macro offers a proper C++ const_cast<>.
 *
 * If compiled as C, this macro does a normal C-style cast.
 *
 * This is helpful to avoid compiler warnings in C++.
 *
 * \param type the type to cast the expression to.
 * \param expression the expression to cast to a different type.
 * \returns `expression`, cast to `type`.
 *
 * \threadsafety It is safe to call this macro from any thread.
 *
 * \since This macro is available since SDL 3.0.0.
 *
 * \sa SDL_reinterpret_cast
 * \sa SDL_static_cast
  }
{ @  }{ Cast operators  }
{*
 * Define a four character code as a Uint32.
 *
 * \param A the first ASCII character.
 * \param B the second ASCII character.
 * \param C the third ASCII character.
 * \param D the fourth ASCII character.
 * \returns the four characters converted into a Uint32, one character
 *          per-byte.
 *
 * \threadsafety It is safe to call this macro from any thread.
 *
 * \since This macro is available since SDL 3.0.0.
  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_FOURCC(A,B,C,D : longint) : longint;

{$ifdef SDL_WIKI_DOCUMENTATION_SECTION}
{*
 * Append the 64 bit integer suffix to a signed integer literal.
 *
 * This helps compilers that might believe a integer literal larger than
 * 0xFFFFFFFF is overflowing a 32-bit value. Use `SDL_SINT64_C(0xFFFFFFFF1)`
 * instead of `0xFFFFFFFF1` by itself.
 *
 * \since This macro is available since SDL 3.0.0.
 *
 * \sa SDL_UINT64_C
  }
{*
 *  \name Basic data types
  }
{ @  }
{*
 * A signed 8-bit integer type.
 *
 * \since This macro is available since SDL 3.0.0.
  }
type
  PSint8 = ^TSint8;
  TSint8 = Tint8_t;
{ 127  }

{ was #define dname def_expr }
function SDL_MAX_SINT8 : TSint8;  

{ -128  }
{ was #define dname def_expr }
function SDL_MIN_SINT8 : TSint8;  

{*
 * An unsigned 8-bit integer type.
 *
 * \since This macro is available since SDL 3.0.0.
  }
type
  PUint8 = ^TUint8;
  TUint8 = Tuint8_t;
{ 255  }

{ was #define dname def_expr }
function SDL_MAX_UINT8 : TUint8;  

{ 0  }
{ was #define dname def_expr }
function SDL_MIN_UINT8 : TUint8;  

{*
 * A signed 16-bit integer type.
 *
 * \since This macro is available since SDL 3.0.0.
  }
type
  PSint16 = ^TSint16;
  TSint16 = Tint16_t;
{ 32767  }

{ was #define dname def_expr }
function SDL_MAX_SINT16 : TSint16;  

{ -32768  }
{ was #define dname def_expr }
function SDL_MIN_SINT16 : TSint16;  

{*
 * An unsigned 16-bit integer type.
 *
 * \since This macro is available since SDL 3.0.0.
  }
type
  PUint16 = ^TUint16;
  TUint16 = Tuint16_t;
{ 65535  }

{ was #define dname def_expr }
function SDL_MAX_UINT16 : TUint16;  

{ 0  }
{ was #define dname def_expr }
function SDL_MIN_UINT16 : TUint16;  

{*
 * A signed 32-bit integer type.
 *
 * \since This macro is available since SDL 3.0.0.
  }
type
  PSint32 = ^TSint32;
  TSint32 = Tint32_t;
{ 2147483647  }

{ was #define dname def_expr }
function SDL_MAX_SINT32 : TSint32;  

{ -2147483648  }
{ was #define dname def_expr }
function SDL_MIN_SINT32 : TSint32;  

{*
 * An unsigned 32-bit integer type.
 *
 * \since This macro is available since SDL 3.0.0.
  }
type
  PUint32 = ^TUint32;
  TUint32 = Tuint32_t;
{ 4294967295  }

{ was #define dname def_expr }
function SDL_MAX_UINT32 : TUint32;  

{ 0  }
{ was #define dname def_expr }
function SDL_MIN_UINT32 : TUint32;  

{*
 * A signed 64-bit integer type.
 *
 * \since This macro is available since SDL 3.0.0.
 *
 * \sa SDL_SINT64_C
  }
type
  PSint64 = ^TSint64;
  TSint64 = Tint64_t;
{ 9223372036854775807  }

{ was #define dname def_expr }
function SDL_MAX_SINT64 : longint; { return type might be wrong }

{ -9223372036854775808  }
{ was #define dname def_expr }
function SDL_MIN_SINT64 : longint; { return type might be wrong }

{*
 * An unsigned 64-bit integer type.
 *
 * \since This macro is available since SDL 3.0.0.
 *
 * \sa SDL_UINT64_C
  }
type
  PUint64 = ^TUint64;
  TUint64 = Tuint64_t;
{ 18446744073709551615  }

{ was #define dname def_expr }
function SDL_MAX_UINT64 : longint; { return type might be wrong }

{ 0  }
{ was #define dname def_expr }
function SDL_MIN_UINT64 : longint; { return type might be wrong }

{*
 * SDL times are signed, 64-bit integers representing nanoseconds since the
 * Unix epoch (Jan 1, 1970).
 *
 * They can be converted between POSIX time_t values with SDL_NS_TO_SECONDS()
 * and SDL_SECONDS_TO_NS(), and between Windows FILETIME values with
 * SDL_TimeToWindows() and SDL_TimeFromWindows().
 *
 * \since This macro is available since SDL 3.0.0.
 *
 * \sa SDL_MAX_SINT64
 * \sa SDL_MIN_SINT64
  }
type
  PSDL_Time = ^TSDL_Time;
  TSDL_Time = TSint64;

const
  SDL_MAX_TIME = SDL_MAX_SINT64;  
  SDL_MIN_TIME = SDL_MIN_SINT64;  
{ @  }{ Basic data types  }
{*
 *  \name Floating-point constants
  }
{ @  }
{$ifdef FLT_EPSILON}
  SDL_FLT_EPSILON = FLT_EPSILON;  
{$else}
{*
 * Epsilon constant, used for comparing floating-point numbers.
 *
 * Equals by default to platform-defined `FLT_EPSILON`, or
 * `1.1920928955078125e-07F` if that's not available.
 *
 * \since This macro is available since SDL 3.0.0.
  }
{ 0x0.000002p0  }

{ was #define dname def_expr }
function SDL_FLT_EPSILON : single;  

{$endif}
{ @  }{ Floating-point constants  }
{ Make sure we have macros for printing width-based integers.
 * <inttypes.h> should define these but this is not true all platforms.
 * (for example win32)  }
{$ifndef SDL_PRIs64}
{$if defined(SDL_PLATFORM_WINDOWS)}
const
  SDL_PRIs64 = 'I64d';  
(*** was #elif ****){$else defined(PRIs64)}
  SDL_PRIs64 = PRIs64;  
(*** was #elif ****){$else defined(__LP64__) && !defined(SDL_PLATFORM_APPLE)}
  SDL_PRIs64 = 'ld';  
{$else}
  SDL_PRIs64 = 'lld';  
{$endif}
{$endif}
{$ifndef SDL_PRIu64}
{$if defined(SDL_PLATFORM_WINDOWS)}
  SDL_PRIu64 = 'I64u';  
(*** was #elif ****){$else defined(PRIu64)}
  SDL_PRIu64 = PRIu64;  
(*** was #elif ****){$else defined(__LP64__) && !defined(SDL_PLATFORM_APPLE)}
  SDL_PRIu64 = 'lu';  
{$else}
  SDL_PRIu64 = 'llu';  
{$endif}
{$endif}
{$ifndef SDL_PRIx64}
{$if defined(SDL_PLATFORM_WINDOWS)}
  SDL_PRIx64 = 'I64x';  
(*** was #elif ****){$else defined(PRIx64)}
  SDL_PRIx64 = PRIx64;  
(*** was #elif ****){$else defined(__LP64__) && !defined(SDL_PLATFORM_APPLE)}
  SDL_PRIx64 = 'lx';  
{$else}
  SDL_PRIx64 = 'llx';  
{$endif}
{$endif}
{$ifndef SDL_PRIX64}
{$if defined(SDL_PLATFORM_WINDOWS)}
  SDL_PRIX64 = 'I64X';  
(*** was #elif ****){$else defined(PRIX64)}
  SDL_PRIX64 = PRIX64;  
(*** was #elif ****){$else defined(__LP64__) && !defined(SDL_PLATFORM_APPLE)}
  SDL_PRIX64 = 'lX';  
{$else}
  SDL_PRIX64 = 'llX';  
{$endif}
{$endif}
{$ifndef SDL_PRIs32}
{$ifdef PRId32}
  SDL_PRIs32 = PRId32;  
{$else}
  SDL_PRIs32 = 'd';  
{$endif}
{$endif}
{$ifndef SDL_PRIu32}
{$ifdef PRIu32}
  SDL_PRIu32 = PRIu32;  
{$else}
  SDL_PRIu32 = 'u';  
{$endif}
{$endif}
{$ifndef SDL_PRIx32}
{$ifdef PRIx32}
  SDL_PRIx32 = PRIx32;  
{$else}
  SDL_PRIx32 = 'x';  
{$endif}
{$endif}
{$ifndef SDL_PRIX32}
{$ifdef PRIX32}
  SDL_PRIX32 = PRIX32;  
{$else}
  SDL_PRIX32 = 'X';  
{$endif}
{$endif}
{* \endcond  }
{ Check to make sure enums are the size of ints, for structure packing.
   For both Watcom C/C++ and Borland C/C++ the compiler option that makes
   enums having the size of an int must be enabled.
   This is "-b" for Borland C/C++ and "-ei" for Watcom C/C++ (v11).
 }
{* \cond  }
type
  PSDL_DUMMY_ENUM = ^TSDL_DUMMY_ENUM;
  TSDL_DUMMY_ENUM =  Longint;
  Const
    DUMMY_ENUM_VALUE = 0;
;
{ C++ extern C conditionnal removed }
{*
 * A macro to initialize an SDL interface.
 *
 * This macro will initialize an SDL interface structure and should be called
 * before you fill out the fields with your implementation.
 *
 * You can use it like this:
 *
 * ```c
 * SDL_IOStreamInterface iface;
 *
 * SDL_INIT_INTERFACE(&iface);
 *
 * // Fill in the interface function pointers with your implementation
 * iface.seek = ...
 *
 * stream = SDL_OpenIO(&iface, NULL);
 * ```
 *
 * If you are using designated initializers, you can use the size of the
 * interface as the version, e.g.
 *
 * ```c
 * SDL_IOStreamInterface iface = 
 *     .version = sizeof(iface),
 *     .seek = ...
 * ;
 * stream = SDL_OpenIO(&iface, NULL);
 * ```
 *
 * \threadsafety It is safe to call this macro from any thread.
 *
 * \since This macro is available since SDL 3.0.0.
 *
 * \sa SDL_IOStreamInterface
 * \sa SDL_StorageInterface
 * \sa SDL_VirtualJoystickDesc
  }
{*
 * Allocate uninitialized memory.
 *
 * The allocated memory returned by this function must be freed with
 * SDL_free().
 *
 * If `size` is 0, it will be set to 1.
 *
 * If you want to allocate memory aligned to a specific alignment, consider
 * using SDL_aligned_alloc().
 *
 * \param size the size to allocate.
 * \returns a pointer to the allocated memory, or NULL if allocation failed.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_free
 * \sa SDL_calloc
 * \sa SDL_realloc
 * \sa SDL_aligned_alloc
  }

function SDL_malloc(size:Tsize_t):pointer;cdecl;external libSDL3;
{*
 * Allocate a zero-initialized array.
 *
 * The memory returned by this function must be freed with SDL_free().
 *
 * If either of `nmemb` or `size` is 0, they will both be set to 1.
 *
 * \param nmemb the number of elements in the array.
 * \param size the size of each element of the array.
 * \returns a pointer to the allocated array, or NULL if allocation failed.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_free
 * \sa SDL_malloc
 * \sa SDL_realloc
  }
function SDL_calloc(nmemb:Tsize_t; size:Tsize_t):pointer;cdecl;external libSDL3;
{*
 * Change the size of allocated memory.
 *
 * The memory returned by this function must be freed with SDL_free().
 *
 * If `size` is 0, it will be set to 1. Note that this is unlike some other C
 * runtime `realloc` implementations, which may treat `realloc(mem, 0)` the
 * same way as `free(mem)`.
 *
 * If `mem` is NULL, the behavior of this function is equivalent to
 * SDL_malloc(). Otherwise, the function can have one of three possible
 * outcomes:
 *
 * - If it returns the same pointer as `mem`, it means that `mem` was resized
 *   in place without freeing.
 * - If it returns a different non-NULL pointer, it means that `mem` was freed
 *   and cannot be dereferenced anymore.
 * - If it returns NULL (indicating failure), then `mem` will remain valid and
 *   must still be freed with SDL_free().
 *
 * \param mem a pointer to allocated memory to reallocate, or NULL.
 * \param size the new size of the memory.
 * \returns a pointer to the newly allocated memory, or NULL if allocation
 *          failed.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_free
 * \sa SDL_malloc
 * \sa SDL_calloc
  }
function SDL_realloc(mem:pointer; size:Tsize_t):pointer;cdecl;external libSDL3;
{*
 * Free allocated memory.
 *
 * The pointer is no longer valid after this call and cannot be dereferenced
 * anymore.
 *
 * If `mem` is NULL, this function does nothing.
 *
 * \param mem a pointer to allocated memory, or NULL.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_malloc
 * \sa SDL_calloc
 * \sa SDL_realloc
  }
procedure SDL_free(mem:pointer);cdecl;external libSDL3;
{*
 * A callback used to implement SDL_malloc().
 *
 * SDL will always ensure that the passed `size` is greater than 0.
 *
 * \param size the size to allocate.
 * \returns a pointer to the allocated memory, or NULL if allocation failed.
 *
 * \threadsafety It should be safe to call this callback from any thread.
 *
 * \since This datatype is available since SDL 3.0.0.
 *
 * \sa SDL_malloc
 * \sa SDL_GetOriginalMemoryFunctions
 * \sa SDL_GetMemoryFunctions
 * \sa SDL_SetMemoryFunctions
  }
type
  PSDL_malloc_func = ^TSDL_malloc_func;
  TSDL_malloc_func = function (size:Tsize_t):pointer;cdecl;
{*
 * A callback used to implement SDL_calloc().
 *
 * SDL will always ensure that the passed `nmemb` and `size` are both greater
 * than 0.
 *
 * \param nmemb the number of elements in the array.
 * \param size the size of each element of the array.
 * \returns a pointer to the allocated array, or NULL if allocation failed.
 *
 * \threadsafety It should be safe to call this callback from any thread.
 *
 * \since This datatype is available since SDL 3.0.0.
 *
 * \sa SDL_calloc
 * \sa SDL_GetOriginalMemoryFunctions
 * \sa SDL_GetMemoryFunctions
 * \sa SDL_SetMemoryFunctions
  }

  PSDL_calloc_func = ^TSDL_calloc_func;
  TSDL_calloc_func = function (nmemb:Tsize_t; size:Tsize_t):pointer;cdecl;
{*
 * A callback used to implement SDL_realloc().
 *
 * SDL will always ensure that the passed `size` is greater than 0.
 *
 * \param mem a pointer to allocated memory to reallocate, or NULL.
 * \param size the new size of the memory.
 * \returns a pointer to the newly allocated memory, or NULL if allocation
 *          failed.
 *
 * \threadsafety It should be safe to call this callback from any thread.
 *
 * \since This datatype is available since SDL 3.0.0.
 *
 * \sa SDL_realloc
 * \sa SDL_GetOriginalMemoryFunctions
 * \sa SDL_GetMemoryFunctions
 * \sa SDL_SetMemoryFunctions
  }

  PSDL_realloc_func = ^TSDL_realloc_func;
  TSDL_realloc_func = function (mem:pointer; size:Tsize_t):pointer;cdecl;
{*
 * A callback used to implement SDL_free().
 *
 * SDL will always ensure that the passed `mem` is a non-NULL pointer.
 *
 * \param mem a pointer to allocated memory.
 *
 * \threadsafety It should be safe to call this callback from any thread.
 *
 * \since This datatype is available since SDL 3.0.0.
 *
 * \sa SDL_free
 * \sa SDL_GetOriginalMemoryFunctions
 * \sa SDL_GetMemoryFunctions
 * \sa SDL_SetMemoryFunctions
  }

  TSDL_free_func = procedure (mem:pointer);cdecl;
{*
 * Get the original set of SDL memory functions.
 *
 * This is what SDL_malloc and friends will use by default, if there has been
 * no call to SDL_SetMemoryFunctions. This is not necessarily using the C
 * runtime's `malloc` functions behind the scenes! Different platforms and
 * build configurations might do any number of unexpected things.
 *
 * \param malloc_func filled with malloc function.
 * \param calloc_func filled with calloc function.
 * \param realloc_func filled with realloc function.
 * \param free_func filled with free function.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }

procedure SDL_GetOriginalMemoryFunctions(malloc_func:PSDL_malloc_func; calloc_func:PSDL_calloc_func; realloc_func:PSDL_realloc_func; free_func:PSDL_free_func);cdecl;external libSDL3;
{*
 * Get the current set of SDL memory functions.
 *
 * \param malloc_func filled with malloc function.
 * \param calloc_func filled with calloc function.
 * \param realloc_func filled with realloc function.
 * \param free_func filled with free function.
 *
 * \threadsafety This does not hold a lock, so do not call this in the
 *               unlikely event of a background thread calling
 *               SDL_SetMemoryFunctions simultaneously.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetMemoryFunctions
 * \sa SDL_GetOriginalMemoryFunctions
  }
procedure SDL_GetMemoryFunctions(malloc_func:PSDL_malloc_func; calloc_func:PSDL_calloc_func; realloc_func:PSDL_realloc_func; free_func:PSDL_free_func);cdecl;external libSDL3;
{*
 * Replace SDL's memory allocation functions with a custom set.
 *
 * It is not safe to call this function once any allocations have been made,
 * as future calls to SDL_free will use the new allocator, even if they came
 * from an SDL_malloc made with the old one!
 *
 * If used, usually this needs to be the first call made into the SDL library,
 * if not the very first thing done at program startup time.
 *
 * \param malloc_func custom malloc function.
 * \param calloc_func custom calloc function.
 * \param realloc_func custom realloc function.
 * \param free_func custom free function.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety It is safe to call this function from any thread, but one
 *               should not replace the memory functions once any allocations
 *               are made!
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetMemoryFunctions
 * \sa SDL_GetOriginalMemoryFunctions
  }
function SDL_SetMemoryFunctions(malloc_func:TSDL_malloc_func; calloc_func:TSDL_calloc_func; realloc_func:TSDL_realloc_func; free_func:TSDL_free_func):Tbool;cdecl;external libSDL3;
{*
 * Allocate memory aligned to a specific alignment.
 *
 * The memory returned by this function must be freed with SDL_aligned_free(),
 * _not_ SDL_free().
 *
 * If `alignment` is less than the size of `void *`, it will be increased to
 * match that.
 *
 * The returned memory address will be a multiple of the alignment value, and
 * the size of the memory allocated will be a multiple of the alignment value.
 *
 * \param alignment the alignment of the memory.
 * \param size the size to allocate.
 * \returns a pointer to the aligned memory, or NULL if allocation failed.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_aligned_free
  }
function SDL_aligned_alloc(alignment:Tsize_t; size:Tsize_t):pointer;cdecl;external libSDL3;
{*
 * Free memory allocated by SDL_aligned_alloc().
 *
 * The pointer is no longer valid after this call and cannot be dereferenced
 * anymore.
 *
 * If `mem` is NULL, this function does nothing.
 *
 * \param mem a pointer previously returned by SDL_aligned_alloc(), or NULL.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_aligned_alloc
  }
procedure SDL_aligned_free(mem:pointer);cdecl;external libSDL3;
{*
 * Get the number of outstanding (unfreed) allocations.
 *
 * \returns the number of allocations.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetNumAllocations:longint;cdecl;external libSDL3;
{*
 * A thread-safe set of environment variables
 *
 * \since This struct is available since SDL 3.0.0.
 *
 * \sa SDL_GetEnvironment
 * \sa SDL_CreateEnvironment
 * \sa SDL_GetEnvironmentVariable
 * \sa SDL_GetEnvironmentVariables
 * \sa SDL_SetEnvironmentVariable
 * \sa SDL_UnsetEnvironmentVariable
 * \sa SDL_DestroyEnvironment
  }
type
{*
 * Get the process environment.
 *
 * This is initialized at application start and is not affected by setenv()
 * and unsetenv() calls after that point. Use SDL_SetEnvironmentVariable() and
 * SDL_UnsetEnvironmentVariable() if you want to modify this environment, or
 * SDL_setenv_unsafe() or SDL_unsetenv_unsafe() if you want changes to persist
 * in the C runtime environment after SDL_Quit().
 *
 * \returns a pointer to the environment for the process or NULL on failure;
 *          call SDL_GetError() for more information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetEnvironmentVariable
 * \sa SDL_GetEnvironmentVariables
 * \sa SDL_SetEnvironmentVariable
 * \sa SDL_UnsetEnvironmentVariable
  }

function SDL_GetEnvironment:PSDL_Environment;cdecl;external libSDL3;
{*
 * Create a set of environment variables
 *
 * \param populated true to initialize it from the C runtime environment,
 *                  false to create an empty environment.
 * \returns a pointer to the new environment or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \threadsafety If `populated` is false, it is safe to call this function
 *               from any thread, otherwise it is safe if no other threads are
 *               calling setenv() or unsetenv()
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetEnvironmentVariable
 * \sa SDL_GetEnvironmentVariables
 * \sa SDL_SetEnvironmentVariable
 * \sa SDL_UnsetEnvironmentVariable
 * \sa SDL_DestroyEnvironment
  }
function SDL_CreateEnvironment(populated:Tbool):PSDL_Environment;cdecl;external libSDL3;
{*
 * Get the value of a variable in the environment.
 *
 * \param env the environment to query.
 * \param name the name of the variable to get.
 * \returns a pointer to the value of the variable or NULL if it can't be
 *          found.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetEnvironment
 * \sa SDL_CreateEnvironment
 * \sa SDL_GetEnvironmentVariables
 * \sa SDL_SetEnvironmentVariable
 * \sa SDL_UnsetEnvironmentVariable
  }
function SDL_GetEnvironmentVariable(env:PSDL_Environment; name:Pansichar):Pansichar;cdecl;external libSDL3;
{*
 * Get all variables in the environment.
 *
 * \param env the environment to query.
 * \returns a NULL terminated array of pointers to environment variables in
 *          the form "variable=value" or NULL on failure; call SDL_GetError()
 *          for more information. This is a single allocation that should be
 *          freed with SDL_free() when it is no longer needed.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetEnvironment
 * \sa SDL_CreateEnvironment
 * \sa SDL_GetEnvironmentVariables
 * \sa SDL_SetEnvironmentVariable
 * \sa SDL_UnsetEnvironmentVariable
  }
function SDL_GetEnvironmentVariables(env:PSDL_Environment):^Pansichar;cdecl;external libSDL3;
{*
 * Set the value of a variable in the environment.
 *
 * \param env the environment to modify.
 * \param name the name of the variable to set.
 * \param value the value of the variable to set.
 * \param overwrite true to overwrite the variable if it exists, false to
 *                  return success without setting the variable if it already
 *                  exists.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetEnvironment
 * \sa SDL_CreateEnvironment
 * \sa SDL_GetEnvironmentVariable
 * \sa SDL_GetEnvironmentVariables
 * \sa SDL_UnsetEnvironmentVariable
  }
function SDL_SetEnvironmentVariable(env:PSDL_Environment; name:Pansichar; value:Pansichar; overwrite:Tbool):Tbool;cdecl;external libSDL3;
{*
 * Clear a variable from the environment.
 *
 * \param env the environment to modify.
 * \param name the name of the variable to unset.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetEnvironment
 * \sa SDL_CreateEnvironment
 * \sa SDL_GetEnvironmentVariable
 * \sa SDL_GetEnvironmentVariables
 * \sa SDL_SetEnvironmentVariable
 * \sa SDL_UnsetEnvironmentVariable
  }
function SDL_UnsetEnvironmentVariable(env:PSDL_Environment; name:Pansichar):Tbool;cdecl;external libSDL3;
{*
 * Destroy a set of environment variables.
 *
 * \param env the environment to destroy.
 *
 * \threadsafety It is safe to call this function from any thread, as long as
 *               the environment is no longer in use.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CreateEnvironment
  }
procedure SDL_DestroyEnvironment(env:PSDL_Environment);cdecl;external libSDL3;
{*
 * Get the value of a variable in the environment.
 *
 * This function uses SDL's cached copy of the environment and is thread-safe.
 *
 * \param name the name of the variable to get.
 * \returns a pointer to the value of the variable or NULL if it can't be
 *          found.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_getenv(name:Pansichar):Pansichar;cdecl;external libSDL3;
{*
 * Get the value of a variable in the environment.
 *
 * This function bypasses SDL's cached copy of the environment and is not
 * thread-safe.
 *
 * \param name the name of the variable to get.
 * \returns a pointer to the value of the variable or NULL if it can't be
 *          found.
 *
 * \threadsafety This function is not thread safe, consider using SDL_getenv()
 *               instead.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_getenv
  }
function SDL_getenv_unsafe(name:Pansichar):Pansichar;cdecl;external libSDL3;
{*
 * Set the value of a variable in the environment.
 *
 * \param name the name of the variable to set.
 * \param value the value of the variable to set.
 * \param overwrite 1 to overwrite the variable if it exists, 0 to return
 *                  success without setting the variable if it already exists.
 * \returns 0 on success, -1 on error.
 *
 * \threadsafety This function is not thread safe, consider using
 *               SDL_SetEnvironmentVariable() instead.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetEnvironmentVariable
  }
function SDL_setenv_unsafe(name:Pansichar; value:Pansichar; overwrite:longint):longint;cdecl;external libSDL3;
{*
 * Clear a variable from the environment.
 *
 * \param name the name of the variable to unset.
 * \returns 0 on success, -1 on error.
 *
 * \threadsafety This function is not thread safe, consider using
 *               SDL_UnsetEnvironmentVariable() instead.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_UnsetEnvironmentVariable
  }
function SDL_unsetenv_unsafe(name:Pansichar):longint;cdecl;external libSDL3;
{*
 * A callback used with SDL sorting and binary search functions.
 *
 * \param a a pointer to the first element being compared.
 * \param b a pointer to the second element being compared.
 * \returns -1 if `a` should be sorted before `b`, 1 if `b` should be sorted
 *          before `a`, 0 if they are equal. If two elements are equal, their
 *          order in the sorted array is undefined.
 *
 * \since This callback is available since SDL 3.0.0.
 *
 * \sa SDL_bsearch
 * \sa SDL_qsort
  }
type

  TSDL_CompareCallback = function (a:pointer; b:pointer):longint;cdecl;
{*
 * Sort an array.
 *
 * For example:
 *
 * ```c
 * typedef struct 
 *     int key;
 *     const char *string;
 *  data;
 *
 * int  compare(const void *a, const void *b)
 * 
 *     const data *A = (const data *)a;
 *     const data *B = (const data *)b;
 *
 *     if (A->n < B->n) 
 *         return -1;
 *      else if (B->n < A->n) 
 *         return 1;
 *      else 
 *         return 0;
 *     
 * 
 *
 * data values[] = 
 *      3, "third" ,  1, "first" ,  2, "second" 
 * ;
 *
 * SDL_qsort(values, SDL_arraysize(values), sizeof(values[0]), compare);
 * ```
 *
 * \param base a pointer to the start of the array.
 * \param nmemb the number of elements in the array.
 * \param size the size of the elements in the array.
 * \param compare a function used to compare elements in the array.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_bsearch
 * \sa SDL_qsort_r
  }

procedure SDL_qsort(base:pointer; nmemb:Tsize_t; size:Tsize_t; compare:TSDL_CompareCallback);cdecl;external libSDL3;
{*
 * Perform a binary search on a previously sorted array.
 *
 * For example:
 *
 * ```c
 * typedef struct 
 *     int key;
 *     const char *string;
 *  data;
 *
 * int  compare(const void *a, const void *b)
 * 
 *     const data *A = (const data *)a;
 *     const data *B = (const data *)b;
 *
 *     if (A->n < B->n) 
 *         return -1;
 *      else if (B->n < A->n) 
 *         return 1;
 *      else 
 *         return 0;
 *     
 * 
 *
 * data values[] = 
 *      1, "first" ,  2, "second" ,  3, "third" 
 * ;
 * data key =  2, NULL ;
 *
 * data *result = SDL_bsearch(&key, values, SDL_arraysize(values), sizeof(values[0]), compare);
 * ```
 *
 * \param key a pointer to a key equal to the element being searched for.
 * \param base a pointer to the start of the array.
 * \param nmemb the number of elements in the array.
 * \param size the size of the elements in the array.
 * \param compare a function used to compare elements in the array.
 * \returns a pointer to the matching element in the array, or NULL if not
 *          found.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_bsearch_r
 * \sa SDL_qsort
  }
function SDL_bsearch(key:pointer; base:pointer; nmemb:Tsize_t; size:Tsize_t; compare:TSDL_CompareCallback):pointer;cdecl;external libSDL3;
{*
 * A callback used with SDL sorting and binary search functions.
 *
 * \param userdata the `userdata` pointer passed to the sort function.
 * \param a a pointer to the first element being compared.
 * \param b a pointer to the second element being compared.
 * \returns -1 if `a` should be sorted before `b`, 1 if `b` should be sorted
 *          before `a`, 0 if they are equal. If two elements are equal, their
 *          order in the sorted array is undefined.
 *
 * \since This callback is available since SDL 3.0.0.
 *
 * \sa SDL_qsort_r
 * \sa SDL_bsearch_r
  }
type

  TSDL_CompareCallback_r = function (userdata:pointer; a:pointer; b:pointer):longint;cdecl;
{*
 * Sort an array, passing a userdata pointer to the compare function.
 *
 * For example:
 *
 * ```c
 * typedef enum 
 *     sort_increasing,
 *     sort_decreasing,
 *  sort_method;
 *
 * typedef struct 
 *     int key;
 *     const char *string;
 *  data;
 *
 * int  compare(const void *userdata, const void *a, const void *b)
 * 
 *     sort_method method = (sort_method)(uintptr_t)userdata;
 *     const data *A = (const data *)a;
 *     const data *B = (const data *)b;
 *
 *     if (A->n < B->n) 
 *         return (method == sort_increasing) ? -1 : 1;
 *      else if (B->n < A->n) 
 *         return (method == sort_increasing) ? 1 : -1;
 *      else 
 *         return 0;
 *     
 * 
 *
 * data values[] = 
 *      3, "third" ,  1, "first" ,  2, "second" 
 * ;
 *
 * SDL_qsort_r(values, SDL_arraysize(values), sizeof(values[0]), compare, (const void *)(uintptr_t)sort_increasing);
 * ```
 *
 * \param base a pointer to the start of the array.
 * \param nmemb the number of elements in the array.
 * \param size the size of the elements in the array.
 * \param compare a function used to compare elements in the array.
 * \param userdata a pointer to pass to the compare function.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_bsearch_r
 * \sa SDL_qsort
  }

procedure SDL_qsort_r(base:pointer; nmemb:Tsize_t; size:Tsize_t; compare:TSDL_CompareCallback_r; userdata:pointer);cdecl;external libSDL3;
{*
 * Perform a binary search on a previously sorted array, passing a userdata
 * pointer to the compare function.
 *
 * For example:
 *
 * ```c
 * typedef enum 
 *     sort_increasing,
 *     sort_decreasing,
 *  sort_method;
 *
 * typedef struct 
 *     int key;
 *     const char *string;
 *  data;
 *
 * int  compare(const void *userdata, const void *a, const void *b)
 * 
 *     sort_method method = (sort_method)(uintptr_t)userdata;
 *     const data *A = (const data *)a;
 *     const data *B = (const data *)b;
 *
 *     if (A->n < B->n) 
 *         return (method == sort_increasing) ? -1 : 1;
 *      else if (B->n < A->n) 
 *         return (method == sort_increasing) ? 1 : -1;
 *      else 
 *         return 0;
 *     
 * 
 *
 * data values[] = 
 *      1, "first" ,  2, "second" ,  3, "third" 
 * ;
 * data key =  2, NULL ;
 *
 * data *result = SDL_bsearch_r(&key, values, SDL_arraysize(values), sizeof(values[0]), compare, (const void *)(uintptr_t)sort_increasing);
 * ```
 *
 * \param key a pointer to a key equal to the element being searched for.
 * \param base a pointer to the start of the array.
 * \param nmemb the number of elements in the array.
 * \param size the size of the elements in the array.
 * \param compare a function used to compare elements in the array.
 * \param userdata a pointer to pass to the compare function.
 * \returns a pointer to the matching element in the array, or NULL if not
 *          found.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_bsearch
 * \sa SDL_qsort_r
  }
function SDL_bsearch_r(key:pointer; base:pointer; nmemb:Tsize_t; size:Tsize_t; compare:TSDL_CompareCallback_r; 
           userdata:pointer):pointer;cdecl;external libSDL3;
function SDL_abs(x:longint):longint;cdecl;external libSDL3;
{ NOTE: these double-evaluate their arguments, so you should never have side effects in the parameters  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_min(x,y : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_max(x,y : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_clamp(x,a,b : longint) : longint;

{*
 * Query if a character is alphabetic (a letter).
 *
 * **WARNING**: Regardless of system locale, this will only treat ASCII values
 * for English 'a-z' and 'A-Z' as true.
 *
 * \param x character value to check.
 * \returns non-zero if x falls within the character class, zero otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_isalpha(x:longint):longint;cdecl;external libSDL3;
{*
 * Query if a character is alphabetic (a letter) or a number.
 *
 * **WARNING**: Regardless of system locale, this will only treat ASCII values
 * for English 'a-z', 'A-Z', and '0-9' as true.
 *
 * \param x character value to check.
 * \returns non-zero if x falls within the character class, zero otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_isalnum(x:longint):longint;cdecl;external libSDL3;
{*
 * Report if a character is blank (a space or tab).
 *
 * **WARNING**: Regardless of system locale, this will only treat ASCII values
 * 0x20 (space) or 0x9 (tab) as true.
 *
 * \param x character value to check.
 * \returns non-zero if x falls within the character class, zero otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_isblank(x:longint):longint;cdecl;external libSDL3;
{*
 * Report if a character is a control character.
 *
 * **WARNING**: Regardless of system locale, this will only treat ASCII values
 * 0 through 0x1F, and 0x7F, as true.
 *
 * \param x character value to check.
 * \returns non-zero if x falls within the character class, zero otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_iscntrl(x:longint):longint;cdecl;external libSDL3;
{*
 * Report if a character is a numeric digit.
 *
 * **WARNING**: Regardless of system locale, this will only treat ASCII values
 * '0' (0x30) through '9' (0x39), as true.
 *
 * \param x character value to check.
 * \returns non-zero if x falls within the character class, zero otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_isdigit(x:longint):longint;cdecl;external libSDL3;
{*
 * Report if a character is a hexadecimal digit.
 *
 * **WARNING**: Regardless of system locale, this will only treat ASCII values
 * 'A' through 'F', 'a' through 'f', and '0' through '9', as true.
 *
 * \param x character value to check.
 * \returns non-zero if x falls within the character class, zero otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_isxdigit(x:longint):longint;cdecl;external libSDL3;
{*
 * Report if a character is a punctuation mark.
 *
 * **WARNING**: Regardless of system locale, this is equivalent to
 * `((SDL_isgraph(x)) && (!SDL_isalnum(x)))`.
 *
 * \param x character value to check.
 * \returns non-zero if x falls within the character class, zero otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_isgraph
 * \sa SDL_isalnum
  }
function SDL_ispunct(x:longint):longint;cdecl;external libSDL3;
{*
 * Report if a character is whitespace.
 *
 * **WARNING**: Regardless of system locale, this will only treat the
 * following ASCII values as true:
 *
 * - space (0x20)
 * - tab (0x09)
 * - newline (0x0A)
 * - vertical tab (0x0B)
 * - form feed (0x0C)
 * - return (0x0D)
 *
 * \param x character value to check.
 * \returns non-zero if x falls within the character class, zero otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_isspace(x:longint):longint;cdecl;external libSDL3;
{*
 * Report if a character is upper case.
 *
 * **WARNING**: Regardless of system locale, this will only treat ASCII values
 * 'A' through 'Z' as true.
 *
 * \param x character value to check.
 * \returns non-zero if x falls within the character class, zero otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_isupper(x:longint):longint;cdecl;external libSDL3;
{*
 * Report if a character is lower case.
 *
 * **WARNING**: Regardless of system locale, this will only treat ASCII values
 * 'a' through 'z' as true.
 *
 * \param x character value to check.
 * \returns non-zero if x falls within the character class, zero otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_islower(x:longint):longint;cdecl;external libSDL3;
{*
 * Report if a character is "printable".
 *
 * Be advised that "printable" has a definition that goes back to text
 * terminals from the dawn of computing, making this a sort of special case
 * function that is not suitable for Unicode (or most any) text management.
 *
 * **WARNING**: Regardless of system locale, this will only treat ASCII values
 * ' ' (0x20) through '~' (0x7E) as true.
 *
 * \param x character value to check.
 * \returns non-zero if x falls within the character class, zero otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_isprint(x:longint):longint;cdecl;external libSDL3;
{*
 * Report if a character is any "printable" except space.
 *
 * Be advised that "printable" has a definition that goes back to text
 * terminals from the dawn of computing, making this a sort of special case
 * function that is not suitable for Unicode (or most any) text management.
 *
 * **WARNING**: Regardless of system locale, this is equivalent to
 * `(SDL_isprint(x)) && ((x) != ' ')`.
 *
 * \param x character value to check.
 * \returns non-zero if x falls within the character class, zero otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_isprint
  }
function SDL_isgraph(x:longint):longint;cdecl;external libSDL3;
{*
 * Convert low-ASCII English letters to uppercase.
 *
 * **WARNING**: Regardless of system locale, this will only convert ASCII
 * values 'a' through 'z' to uppercase.
 *
 * This function returns the uppercase equivalent of `x`. If a character
 * cannot be converted, or is already uppercase, this function returns `x`.
 *
 * \param x character value to check.
 * \returns capitalized version of x, or x if no conversion available.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_toupper(x:longint):longint;cdecl;external libSDL3;
{*
 * Convert low-ASCII English letters to lowercase.
 *
 * **WARNING**: Regardless of system locale, this will only convert ASCII
 * values 'A' through 'Z' to lowercase.
 *
 * This function returns the lowercase equivalent of `x`. If a character
 * cannot be converted, or is already lowercase, this function returns `x`.
 *
 * \param x character value to check.
 * \returns lowercase version of x, or x if no conversion available.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_tolower(x:longint):longint;cdecl;external libSDL3;
function SDL_crc16(crc:TUint16; data:pointer; len:Tsize_t):TUint16;cdecl;external libSDL3;
function SDL_crc32(crc:TUint32; data:pointer; len:Tsize_t):TUint32;cdecl;external libSDL3;
function SDL_murmur3_32(data:pointer; len:Tsize_t; seed:TUint32):TUint32;cdecl;external libSDL3;
{*
 * Copy non-overlapping memory.
 *
 * The memory regions must not overlap. If they do, use SDL_memmove() instead.
 *
 * \param dst The destination memory region. Must not be NULL, and must not
 *            overlap with `src`.
 * \param src The source memory region. Must not be NULL, and must not overlap
 *            with `dst`.
 * \param len The length in bytes of both `dst` and `src`.
 * \returns `dst`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_memmove
  }
function SDL_memcpy(dst:pointer; src:pointer; len:Tsize_t):pointer;cdecl;external libSDL3;
{ Take advantage of compiler optimizations for memcpy  }
{$ifndef SDL_SLOW_MEMCPY}
{$ifdef SDL_memcpy}
{$undef SDL_memcpy}
{$endif}
const
  SDL_memcpy = memcpy;  
{$endif}
{*
 * Copy memory.
 *
 * It is okay for the memory regions to overlap. If you are confident that the
 * regions never overlap, using SDL_memcpy() may improve performance.
 *
 * \param dst The destination memory region. Must not be NULL.
 * \param src The source memory region. Must not be NULL.
 * \param len The length in bytes of both `dst` and `src`.
 * \returns `dst`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_memcpy
  }

function SDL_memmove(dst:pointer; src:pointer; len:Tsize_t):pointer;cdecl;external libSDL3;
{ Take advantage of compiler optimizations for memmove  }
{$ifndef SDL_SLOW_MEMMOVE}
{$ifdef SDL_memmove}
{$undef SDL_memmove}
{$endif}
const
  SDL_memmove = memmove;  
{$endif}

function SDL_memset(dst:pointer; c:longint; len:Tsize_t):pointer;cdecl;external libSDL3;
function SDL_memset4(dst:pointer; val:TUint32; dwords:Tsize_t):pointer;cdecl;external libSDL3;
{ Take advantage of compiler optimizations for memset  }
{$ifndef SDL_SLOW_MEMSET}
{$ifdef SDL_memset}
{$undef SDL_memset}
{$endif}
const
  SDL_memset = memset;  
{$endif}

function SDL_memcmp(s1:pointer; s2:pointer; len:Tsize_t):longint;cdecl;external libSDL3;
function SDL_wcslen(wstr:Pwchar_t):Tsize_t;cdecl;external libSDL3;
function SDL_wcsnlen(wstr:Pwchar_t; maxlen:Tsize_t):Tsize_t;cdecl;external libSDL3;
{*
 * Copy a wide string.
 *
 * This function copies `maxlen` - 1 wide characters from `src` to `dst`, then
 * appends a null terminator.
 *
 * `src` and `dst` must not overlap.
 *
 * If `maxlen` is 0, no wide characters are copied and no null terminator is
 * written.
 *
 * \param dst The destination buffer. Must not be NULL, and must not overlap
 *            with `src`.
 * \param src The null-terminated wide string to copy. Must not be NULL, and
 *            must not overlap with `dst`.
 * \param maxlen The length (in wide characters) of the destination buffer.
 * \returns The length (in wide characters, excluding the null terminator) of
 *          `src`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_wcslcat
  }
function SDL_wcslcpy(dst:Pwchar_t; src:Pwchar_t; maxlen:Tsize_t):Tsize_t;cdecl;external libSDL3;
{*
 * Concatenate wide strings.
 *
 * This function appends up to `maxlen` - SDL_wcslen(dst) - 1 wide characters
 * from `src` to the end of the wide string in `dst`, then appends a null
 * terminator.
 *
 * `src` and `dst` must not overlap.
 *
 * If `maxlen` - SDL_wcslen(dst) - 1 is less than or equal to 0, then `dst` is
 * unmodified.
 *
 * \param dst The destination buffer already containing the first
 *            null-terminated wide string. Must not be NULL and must not
 *            overlap with `src`.
 * \param src The second null-terminated wide string. Must not be NULL, and
 *            must not overlap with `dst`.
 * \param maxlen The length (in wide characters) of the destination buffer.
 * \returns The length (in wide characters, excluding the null terminator) of
 *          the string in `dst` plus the length of `src`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_wcslcpy
  }
function SDL_wcslcat(dst:Pwchar_t; src:Pwchar_t; maxlen:Tsize_t):Tsize_t;cdecl;external libSDL3;
function SDL_wcsdup(wstr:Pwchar_t):Pwchar_t;cdecl;external libSDL3;
function SDL_wcsstr(haystack:Pwchar_t; needle:Pwchar_t):Pwchar_t;cdecl;external libSDL3;
function SDL_wcsnstr(haystack:Pwchar_t; needle:Pwchar_t; maxlen:Tsize_t):Pwchar_t;cdecl;external libSDL3;
{*
 * Compare two null-terminated wide strings.
 *
 * This only compares wchar_t values until it hits a null-terminating
 * character; it does not care if the string is well-formed UTF-16 (or UTF-32,
 * depending on your platform's wchar_t size), or uses valid Unicode values.
 *
 * \param str1 the first string to compare. NULL is not permitted!
 * \param str2 the second string to compare. NULL is not permitted!
 * \returns less than zero if str1 is "less than" str2, greater than zero if
 *          str1 is "greater than" str2, and zero if the strings match
 *          exactly.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_wcscmp(str1:Pwchar_t; str2:Pwchar_t):longint;cdecl;external libSDL3;
{*
 * Compare two wide strings up to a number of wchar_t values.
 *
 * This only compares wchar_t values; it does not care if the string is
 * well-formed UTF-16 (or UTF-32, depending on your platform's wchar_t size),
 * or uses valid Unicode values.
 *
 * Note that while this function is intended to be used with UTF-16 (or
 * UTF-32, depending on your platform's definition of wchar_t), it is
 * comparing raw wchar_t values and not Unicode codepoints: `maxlen` specifies
 * a wchar_t limit! If the limit lands in the middle of a multi-wchar UTF-16
 * sequence, it will only compare a portion of the final character.
 *
 * `maxlen` specifies a maximum number of wchar_t to compare; if the strings
 * match to this number of wide chars (or both have matched to a
 * null-terminator character before this count), they will be considered
 * equal.
 *
 * \param str1 the first string to compare. NULL is not permitted!
 * \param str2 the second string to compare. NULL is not permitted!
 * \param maxlen the maximum number of wchar_t to compare.
 * \returns less than zero if str1 is "less than" str2, greater than zero if
 *          str1 is "greater than" str2, and zero if the strings match
 *          exactly.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_wcsncmp(str1:Pwchar_t; str2:Pwchar_t; maxlen:Tsize_t):longint;cdecl;external libSDL3;
{*
 * Compare two null-terminated wide strings, case-insensitively.
 *
 * This will work with Unicode strings, using a technique called
 * "case-folding" to handle the vast majority of case-sensitive human
 * languages regardless of system locale. It can deal with expanding values: a
 * German Eszett character can compare against two ASCII 's' chars and be
 * considered a match, for example. A notable exception: it does not handle
 * the Turkish 'i' character; human language is complicated!
 *
 * Depending on your platform, "wchar_t" might be 2 bytes, and expected to be
 * UTF-16 encoded (like Windows), or 4 bytes in UTF-32 format. Since this
 * handles Unicode, it expects the string to be well-formed and not a
 * null-terminated string of arbitrary bytes. Characters that are not valid
 * UTF-16 (or UTF-32) are treated as Unicode character U+FFFD (REPLACEMENT
 * CHARACTER), which is to say two strings of random bits may turn out to
 * match if they convert to the same amount of replacement characters.
 *
 * \param str1 the first string to compare. NULL is not permitted!
 * \param str2 the second string to compare. NULL is not permitted!
 * \returns less than zero if str1 is "less than" str2, greater than zero if
 *          str1 is "greater than" str2, and zero if the strings match
 *          exactly.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_wcscasecmp(str1:Pwchar_t; str2:Pwchar_t):longint;cdecl;external libSDL3;
{*
 * Compare two wide strings, case-insensitively, up to a number of wchar_t.
 *
 * This will work with Unicode strings, using a technique called
 * "case-folding" to handle the vast majority of case-sensitive human
 * languages regardless of system locale. It can deal with expanding values: a
 * German Eszett character can compare against two ASCII 's' chars and be
 * considered a match, for example. A notable exception: it does not handle
 * the Turkish 'i' character; human language is complicated!
 *
 * Depending on your platform, "wchar_t" might be 2 bytes, and expected to be
 * UTF-16 encoded (like Windows), or 4 bytes in UTF-32 format. Since this
 * handles Unicode, it expects the string to be well-formed and not a
 * null-terminated string of arbitrary bytes. Characters that are not valid
 * UTF-16 (or UTF-32) are treated as Unicode character U+FFFD (REPLACEMENT
 * CHARACTER), which is to say two strings of random bits may turn out to
 * match if they convert to the same amount of replacement characters.
 *
 * Note that while this function might deal with variable-sized characters,
 * `maxlen` specifies a _wchar_ limit! If the limit lands in the middle of a
 * multi-byte UTF-16 sequence, it may convert a portion of the final character
 * to one or more Unicode character U+FFFD (REPLACEMENT CHARACTER) so as not
 * to overflow a buffer.
 *
 * `maxlen` specifies a maximum number of wchar_t values to compare; if the
 * strings match to this number of wchar_t (or both have matched to a
 * null-terminator character before this number of bytes), they will be
 * considered equal.
 *
 * \param str1 the first string to compare. NULL is not permitted!
 * \param str2 the second string to compare. NULL is not permitted!
 * \param maxlen the maximum number of wchar_t values to compare.
 * \returns less than zero if str1 is "less than" str2, greater than zero if
 *          str1 is "greater than" str2, and zero if the strings match
 *          exactly.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_wcsncasecmp(str1:Pwchar_t; str2:Pwchar_t; maxlen:Tsize_t):longint;cdecl;external libSDL3;
{*
 * Parse a `long` from a wide string.
 *
 * If `str` starts with whitespace, then those whitespace characters are
 * skipped before attempting to parse the number.
 *
 * If the parsed number does not fit inside a `long`, the result is clamped to
 * the minimum and maximum representable `long` values.
 *
 * \param str The null-terminated wide string to read. Must not be NULL.
 * \param endp If not NULL, the address of the first invalid wide character
 *             (i.e. the next character after the parsed number) will be
 *             written to this pointer.
 * \param base The base of the integer to read. Supported values are 0 and 2
 *             to 36 inclusive. If 0, the base will be inferred from the
 *             number's prefix (0x for hexadecimal, 0 for octal, decimal
 *             otherwise).
 * \returns The parsed `long`, or 0 if no number could be parsed.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_strtol
  }
function SDL_wcstol(str:Pwchar_t; endp:PPwchar_t; base:longint):longint;cdecl;external libSDL3;
function SDL_strlen(str:Pansichar):Tsize_t;cdecl;external libSDL3;
function SDL_strnlen(str:Pansichar; maxlen:Tsize_t):Tsize_t;cdecl;external libSDL3;
{*
 * Copy a string.
 *
 * This function copies up to `maxlen` - 1 characters from `src` to `dst`,
 * then appends a null terminator.
 *
 * If `maxlen` is 0, no characters are copied and no null terminator is
 * written.
 *
 * If you want to copy an UTF-8 string but need to ensure that multi-byte
 * sequences are not truncated, consider using SDL_utf8strlcpy().
 *
 * \param dst The destination buffer. Must not be NULL, and must not overlap
 *            with `src`.
 * \param src The null-terminated string to copy. Must not be NULL, and must
 *            not overlap with `dst`.
 * \param maxlen The length (in characters) of the destination buffer.
 * \returns The length (in characters, excluding the null terminator) of
 *          `src`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_strlcat
 * \sa SDL_utf8strlcpy
  }
function SDL_strlcpy(dst:Pansichar; src:Pansichar; maxlen:Tsize_t):Tsize_t;cdecl;external libSDL3;
{*
 * Copy an UTF-8 string.
 *
 * This function copies up to `dst_bytes` - 1 bytes from `src` to `dst` while
 * also ensuring that the string written to `dst` does not end in a truncated
 * multi-byte sequence. Finally, it appends a null terminator.
 *
 * `src` and `dst` must not overlap.
 *
 * Note that unlike SDL_strlcpy(), this function returns the number of bytes
 * written, not the length of `src`.
 *
 * \param dst The destination buffer. Must not be NULL, and must not overlap
 *            with `src`.
 * \param src The null-terminated UTF-8 string to copy. Must not be NULL, and
 *            must not overlap with `dst`.
 * \param dst_bytes The length (in bytes) of the destination buffer. Must not
 *                  be 0.
 * \returns The number of bytes written, excluding the null terminator.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_strlcpy
  }
function SDL_utf8strlcpy(dst:Pansichar; src:Pansichar; dst_bytes:Tsize_t):Tsize_t;cdecl;external libSDL3;
{*
 * Concatenate strings.
 *
 * This function appends up to `maxlen` - SDL_strlen(dst) - 1 characters from
 * `src` to the end of the string in `dst`, then appends a null terminator.
 *
 * `src` and `dst` must not overlap.
 *
 * If `maxlen` - SDL_strlen(dst) - 1 is less than or equal to 0, then `dst` is
 * unmodified.
 *
 * \param dst The destination buffer already containing the first
 *            null-terminated string. Must not be NULL and must not overlap
 *            with `src`.
 * \param src The second null-terminated string. Must not be NULL, and must
 *            not overlap with `dst`.
 * \param maxlen The length (in characters) of the destination buffer.
 * \returns The length (in characters, excluding the null terminator) of the
 *          string in `dst` plus the length of `src`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_strlcpy
  }
function SDL_strlcat(dst:Pansichar; src:Pansichar; maxlen:Tsize_t):Tsize_t;cdecl;external libSDL3;
function SDL_strdup(str:Pansichar):Pansichar;cdecl;external libSDL3;
function SDL_strndup(str:Pansichar; maxlen:Tsize_t):Pansichar;cdecl;external libSDL3;
function SDL_strrev(str:Pansichar):Pansichar;cdecl;external libSDL3;
{*
 * Convert a string to uppercase.
 *
 * **WARNING**: Regardless of system locale, this will only convert ASCII
 * values 'A' through 'Z' to uppercase.
 *
 * This function operates on a null-terminated string of bytes--even if it is
 * malformed UTF-8!--and converts ASCII characters 'a' through 'z' to their
 * uppercase equivalents in-place, returning the original `str` pointer.
 *
 * \param str the string to convert in-place. Can not be NULL.
 * \returns the `str` pointer passed into this function.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_strlwr
  }
function SDL_strupr(str:Pansichar):Pansichar;cdecl;external libSDL3;
{*
 * Convert a string to lowercase.
 *
 * **WARNING**: Regardless of system locale, this will only convert ASCII
 * values 'A' through 'Z' to lowercase.
 *
 * This function operates on a null-terminated string of bytes--even if it is
 * malformed UTF-8!--and converts ASCII characters 'A' through 'Z' to their
 * lowercase equivalents in-place, returning the original `str` pointer.
 *
 * \param str the string to convert in-place. Can not be NULL.
 * \returns the `str` pointer passed into this function.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_strupr
  }
function SDL_strlwr(str:Pansichar):Pansichar;cdecl;external libSDL3;
function SDL_strchr(str:Pansichar; c:longint):Pansichar;cdecl;external libSDL3;
function SDL_strrchr(str:Pansichar; c:longint):Pansichar;cdecl;external libSDL3;
function SDL_strstr(haystack:Pansichar; needle:Pansichar):Pansichar;cdecl;external libSDL3;
function SDL_strnstr(haystack:Pansichar; needle:Pansichar; maxlen:Tsize_t):Pansichar;cdecl;external libSDL3;
function SDL_strcasestr(haystack:Pansichar; needle:Pansichar):Pansichar;cdecl;external libSDL3;
function SDL_strtok_r(s1:Pansichar; s2:Pansichar; saveptr:PPansichar):Pansichar;cdecl;external libSDL3;
function SDL_utf8strlen(str:Pansichar):Tsize_t;cdecl;external libSDL3;
function SDL_utf8strnlen(str:Pansichar; bytes:Tsize_t):Tsize_t;cdecl;external libSDL3;
function SDL_itoa(value:longint; str:Pansichar; radix:longint):Pansichar;cdecl;external libSDL3;
function SDL_uitoa(value:dword; str:Pansichar; radix:longint):Pansichar;cdecl;external libSDL3;
function SDL_ltoa(value:longint; str:Pansichar; radix:longint):Pansichar;cdecl;external libSDL3;
function SDL_ultoa(value:dword; str:Pansichar; radix:longint):Pansichar;cdecl;external libSDL3;
function SDL_lltoa(value:int64; str:Pansichar; radix:longint):Pansichar;cdecl;external libSDL3;
function SDL_ulltoa(value:qword; str:Pansichar; radix:longint):Pansichar;cdecl;external libSDL3;
{*
 * Parse an `int` from a string.
 *
 * The result of calling `SDL_atoi(str)` is equivalent to
 * `(int)SDL_strtol(str, NULL, 10)`.
 *
 * \param str The null-terminated string to read. Must not be NULL.
 * \returns The parsed `int`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_atof
 * \sa SDL_strtol
 * \sa SDL_strtoul
 * \sa SDL_strtoll
 * \sa SDL_strtoull
 * \sa SDL_strtod
 * \sa SDL_itoa
  }
function SDL_atoi(str:Pansichar):longint;cdecl;external libSDL3;
{*
 * Parse a `double` from a string.
 *
 * The result of calling `SDL_atof(str)` is equivalent to `SDL_strtod(str,
 * NULL)`.
 *
 * \param str The null-terminated string to read. Must not be NULL.
 * \returns The parsed `double`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_atoi
 * \sa SDL_strtol
 * \sa SDL_strtoul
 * \sa SDL_strtoll
 * \sa SDL_strtoull
 * \sa SDL_strtod
  }
function SDL_atof(str:Pansichar):Tdouble;cdecl;external libSDL3;
{*
 * Parse a `long` from a string.
 *
 * If `str` starts with whitespace, then those whitespace characters are
 * skipped before attempting to parse the number.
 *
 * If the parsed number does not fit inside a `long`, the result is clamped to
 * the minimum and maximum representable `long` values.
 *
 * \param str The null-terminated string to read. Must not be NULL.
 * \param endp If not NULL, the address of the first invalid character (i.e.
 *             the next character after the parsed number) will be written to
 *             this pointer.
 * \param base The base of the integer to read. Supported values are 0 and 2
 *             to 36 inclusive. If 0, the base will be inferred from the
 *             number's prefix (0x for hexadecimal, 0 for octal, decimal
 *             otherwise).
 * \returns The parsed `long`, or 0 if no number could be parsed.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_atoi
 * \sa SDL_atof
 * \sa SDL_strtoul
 * \sa SDL_strtoll
 * \sa SDL_strtoull
 * \sa SDL_strtod
 * \sa SDL_ltoa
 * \sa SDL_wcstol
  }
function SDL_strtol(str:Pansichar; endp:PPansichar; base:longint):longint;cdecl;external libSDL3;
{*
 * Parse an `unsigned long` from a string.
 *
 * If `str` starts with whitespace, then those whitespace characters are
 * skipped before attempting to parse the number.
 *
 * If the parsed number does not fit inside an `unsigned long`, the result is
 * clamped to the maximum representable `unsigned long` value.
 *
 * \param str The null-terminated string to read. Must not be NULL.
 * \param endp If not NULL, the address of the first invalid character (i.e.
 *             the next character after the parsed number) will be written to
 *             this pointer.
 * \param base The base of the integer to read. Supported values are 0 and 2
 *             to 36 inclusive. If 0, the base will be inferred from the
 *             number's prefix (0x for hexadecimal, 0 for octal, decimal
 *             otherwise).
 * \returns The parsed `unsigned long`, or 0 if no number could be parsed.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_atoi
 * \sa SDL_atof
 * \sa SDL_strtol
 * \sa SDL_strtoll
 * \sa SDL_strtoull
 * \sa SDL_strtod
 * \sa SDL_ultoa
  }
function SDL_strtoul(str:Pansichar; endp:PPansichar; base:longint):dword;cdecl;external libSDL3;
{*
 * Parse a `long long` from a string.
 *
 * If `str` starts with whitespace, then those whitespace characters are
 * skipped before attempting to parse the number.
 *
 * If the parsed number does not fit inside a `long long`, the result is
 * clamped to the minimum and maximum representable `long long` values.
 *
 * \param str The null-terminated string to read. Must not be NULL.
 * \param endp If not NULL, the address of the first invalid character (i.e.
 *             the next character after the parsed number) will be written to
 *             this pointer.
 * \param base The base of the integer to read. Supported values are 0 and 2
 *             to 36 inclusive. If 0, the base will be inferred from the
 *             number's prefix (0x for hexadecimal, 0 for octal, decimal
 *             otherwise).
 * \returns The parsed `long long`, or 0 if no number could be parsed.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_atoi
 * \sa SDL_atof
 * \sa SDL_strtol
 * \sa SDL_strtoul
 * \sa SDL_strtoull
 * \sa SDL_strtod
 * \sa SDL_lltoa
  }
function SDL_strtoll(str:Pansichar; endp:PPansichar; base:longint):int64;cdecl;external libSDL3;
{*
 * Parse an `unsigned long long` from a string.
 *
 * If `str` starts with whitespace, then those whitespace characters are
 * skipped before attempting to parse the number.
 *
 * If the parsed number does not fit inside an `unsigned long long`, the
 * result is clamped to the maximum representable `unsigned long long` value.
 *
 * \param str The null-terminated string to read. Must not be NULL.
 * \param endp If not NULL, the address of the first invalid character (i.e.
 *             the next character after the parsed number) will be written to
 *             this pointer.
 * \param base The base of the integer to read. Supported values are 0 and 2
 *             to 36 inclusive. If 0, the base will be inferred from the
 *             number's prefix (0x for hexadecimal, 0 for octal, decimal
 *             otherwise).
 * \returns The parsed `unsigned long long`, or 0 if no number could be
 *          parsed.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_atoi
 * \sa SDL_atof
 * \sa SDL_strtol
 * \sa SDL_strtoll
 * \sa SDL_strtoul
 * \sa SDL_strtod
 * \sa SDL_ulltoa
  }
function SDL_strtoull(str:Pansichar; endp:PPansichar; base:longint):qword;cdecl;external libSDL3;
{*
 * Parse a `double` from a string.
 *
 * This function makes fewer guarantees than the C runtime `strtod`:
 *
 * - Only decimal notation is guaranteed to be supported. The handling of
 *   scientific and hexadecimal notation is unspecified.
 * - Whether or not INF and NAN can be parsed is unspecified.
 * - The precision of the result is unspecified.
 *
 * \param str The null-terminated string to read. Must not be NULL.
 * \param endp If not NULL, the address of the first invalid character (i.e.
 *             the next character after the parsed number) will be written to
 *             this pointer.
 * \returns The parsed `double`, or 0 if no number could be parsed.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_atoi
 * \sa SDL_atof
 * \sa SDL_strtol
 * \sa SDL_strtoll
 * \sa SDL_strtoul
 * \sa SDL_strtoull
  }
function SDL_strtod(str:Pansichar; endp:PPansichar):Tdouble;cdecl;external libSDL3;
{*
 * Compare two null-terminated UTF-8 strings.
 *
 * Due to the nature of UTF-8 encoding, this will work with Unicode strings,
 * since effectively this function just compares bytes until it hits a
 * null-terminating character. Also due to the nature of UTF-8, this can be
 * used with SDL_qsort() to put strings in (roughly) alphabetical order.
 *
 * \param str1 the first string to compare. NULL is not permitted!
 * \param str2 the second string to compare. NULL is not permitted!
 * \returns less than zero if str1 is "less than" str2, greater than zero if
 *          str1 is "greater than" str2, and zero if the strings match
 *          exactly.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_strcmp(str1:Pansichar; str2:Pansichar):longint;cdecl;external libSDL3;
{*
 * Compare two UTF-8 strings up to a number of bytes.
 *
 * Due to the nature of UTF-8 encoding, this will work with Unicode strings,
 * since effectively this function just compares bytes until it hits a
 * null-terminating character. Also due to the nature of UTF-8, this can be
 * used with SDL_qsort() to put strings in (roughly) alphabetical order.
 *
 * Note that while this function is intended to be used with UTF-8, it is
 * doing a bytewise comparison, and `maxlen` specifies a _byte_ limit! If the
 * limit lands in the middle of a multi-byte UTF-8 sequence, it will only
 * compare a portion of the final character.
 *
 * `maxlen` specifies a maximum number of bytes to compare; if the strings
 * match to this number of bytes (or both have matched to a null-terminator
 * character before this number of bytes), they will be considered equal.
 *
 * \param str1 the first string to compare. NULL is not permitted!
 * \param str2 the second string to compare. NULL is not permitted!
 * \param maxlen the maximum number of _bytes_ to compare.
 * \returns less than zero if str1 is "less than" str2, greater than zero if
 *          str1 is "greater than" str2, and zero if the strings match
 *          exactly.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_strncmp(str1:Pansichar; str2:Pansichar; maxlen:Tsize_t):longint;cdecl;external libSDL3;
{*
 * Compare two null-terminated UTF-8 strings, case-insensitively.
 *
 * This will work with Unicode strings, using a technique called
 * "case-folding" to handle the vast majority of case-sensitive human
 * languages regardless of system locale. It can deal with expanding values: a
 * German Eszett character can compare against two ASCII 's' chars and be
 * considered a match, for example. A notable exception: it does not handle
 * the Turkish 'i' character; human language is complicated!
 *
 * Since this handles Unicode, it expects the string to be well-formed UTF-8
 * and not a null-terminated string of arbitrary bytes. Bytes that are not
 * valid UTF-8 are treated as Unicode character U+FFFD (REPLACEMENT
 * CHARACTER), which is to say two strings of random bits may turn out to
 * match if they convert to the same amount of replacement characters.
 *
 * \param str1 the first string to compare. NULL is not permitted!
 * \param str2 the second string to compare. NULL is not permitted!
 * \returns less than zero if str1 is "less than" str2, greater than zero if
 *          str1 is "greater than" str2, and zero if the strings match
 *          exactly.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_strcasecmp(str1:Pansichar; str2:Pansichar):longint;cdecl;external libSDL3;
{*
 * Compare two UTF-8 strings, case-insensitively, up to a number of bytes.
 *
 * This will work with Unicode strings, using a technique called
 * "case-folding" to handle the vast majority of case-sensitive human
 * languages regardless of system locale. It can deal with expanding values: a
 * German Eszett character can compare against two ASCII 's' chars and be
 * considered a match, for example. A notable exception: it does not handle
 * the Turkish 'i' character; human language is complicated!
 *
 * Since this handles Unicode, it expects the string to be well-formed UTF-8
 * and not a null-terminated string of arbitrary bytes. Bytes that are not
 * valid UTF-8 are treated as Unicode character U+FFFD (REPLACEMENT
 * CHARACTER), which is to say two strings of random bits may turn out to
 * match if they convert to the same amount of replacement characters.
 *
 * Note that while this function is intended to be used with UTF-8, `maxlen`
 * specifies a _byte_ limit! If the limit lands in the middle of a multi-byte
 * UTF-8 sequence, it may convert a portion of the final character to one or
 * more Unicode character U+FFFD (REPLACEMENT CHARACTER) so as not to overflow
 * a buffer.
 *
 * `maxlen` specifies a maximum number of bytes to compare; if the strings
 * match to this number of bytes (or both have matched to a null-terminator
 * character before this number of bytes), they will be considered equal.
 *
 * \param str1 the first string to compare. NULL is not permitted!
 * \param str2 the second string to compare. NULL is not permitted!
 * \param maxlen the maximum number of bytes to compare.
 * \returns less than zero if str1 is "less than" str2, greater than zero if
 *          str1 is "greater than" str2, and zero if the strings match
 *          exactly.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_strncasecmp(str1:Pansichar; str2:Pansichar; maxlen:Tsize_t):longint;cdecl;external libSDL3;
{*
 * Searches a string for the first occurence of any character contained in a
 * breakset, and returns a pointer from the string to that character.
 *
 * \param str The null-terminated string to be searched. Must not be NULL, and
 *            must not overlap with `breakset`.
 * \param breakset A null-terminated string containing the list of characters
 *                 to look for. Must not be NULL, and must not overlap with
 *                 `str`.
 * \returns A pointer to the location, in str, of the first occurence of a
 *          character present in the breakset, or NULL if none is found.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_strpbrk(str:Pansichar; breakset:Pansichar):Pansichar;cdecl;external libSDL3;
{*
 * The Unicode REPLACEMENT CHARACTER codepoint.
 *
 * SDL_StepUTF8() reports this codepoint when it encounters a UTF-8 string
 * with encoding errors.
 *
 * This tends to render as something like a question mark in most places.
 *
 * \since This macro is available since SDL 3.0.0.
 *
 * \sa SDL_StepUTF8
  }
const
  SDL_INVALID_UNICODE_CODEPOINT = $FFFD;  
{*
 * Decode a UTF-8 string, one Unicode codepoint at a time.
 *
 * This will return the first Unicode codepoint in the UTF-8 encoded string in
 * `*pstr`, and then advance `*pstr` past any consumed bytes before returning.
 *
 * It will not access more than `*pslen` bytes from the string. `*pslen` will
 * be adjusted, as well, subtracting the number of bytes consumed.
 *
 * `pslen` is allowed to be NULL, in which case the string _must_ be
 * NULL-terminated, as the function will blindly read until it sees the NULL
 * char.
 *
 * if `*pslen` is zero, it assumes the end of string is reached and returns a
 * zero codepoint regardless of the contents of the string buffer.
 *
 * If the resulting codepoint is zero (a NULL terminator), or `*pslen` is
 * zero, it will not advance `*pstr` or `*pslen` at all.
 *
 * Generally this function is called in a loop until it returns zero,
 * adjusting its parameters each iteration.
 *
 * If an invalid UTF-8 sequence is encountered, this function returns
 * SDL_INVALID_UNICODE_CODEPOINT and advances the string/length by one byte
 * (which is to say, a multibyte sequence might produce several
 * SDL_INVALID_UNICODE_CODEPOINT returns before it syncs to the next valid
 * UTF-8 sequence).
 *
 * Several things can generate invalid UTF-8 sequences, including overlong
 * encodings, the use of UTF-16 surrogate values, and truncated data. Please
 * refer to
 * [RFC3629](https://www.ietf.org/rfc/rfc3629.txt)
 * for details.
 *
 * \param pstr a pointer to a UTF-8 string pointer to be read and adjusted.
 * \param pslen a pointer to the number of bytes in the string, to be read and
 *              adjusted. NULL is allowed.
 * \returns the first Unicode codepoint in the string.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }

function SDL_StepUTF8(pstr:PPansichar; pslen:Psize_t):TUint32;cdecl;external libSDL3;
{*
 * Convert a single Unicode codepoint to UTF-8.
 *
 * The buffer pointed to by `dst` must be at least 4 bytes long, as this
 * function may generate between 1 and 4 bytes of output.
 *
 * This function returns the first byte _after_ the newly-written UTF-8
 * sequence, which is useful for encoding multiple codepoints in a loop, or
 * knowing where to write a NULL-terminator character to end the string (in
 * either case, plan to have a buffer of _more_ than 4 bytes!).
 *
 * If `codepoint` is an invalid value (outside the Unicode range, or a UTF-16
 * surrogate value, etc), this will use U+FFFD (REPLACEMENT CHARACTER) for the
 * codepoint instead, and not set an error.
 *
 * If `dst` is NULL, this returns NULL immediately without writing to the
 * pointer and without setting an error.
 *
 * \param codepoint a Unicode codepoint to convert to UTF-8.
 * \param dst the location to write the encoded UTF-8. Must point to at least
 *            4 bytes!
 * \returns the first byte past the newly-written UTF-8 sequence.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_UCS4ToUTF8(codepoint:TUint32; dst:Pansichar):Pansichar;cdecl;external libSDL3;
function SDL_sscanf(text:Pansichar; fmt:Pansichar; args:array of const):longint;cdecl;external libSDL3;
function SDL_sscanf(text:Pansichar; fmt:Pansichar):longint;cdecl;external libSDL3;
function SDL_vsscanf(text:Pansichar; fmt:Pansichar; ap:Tva_list):longint;cdecl;external libSDL3;
function SDL_snprintf(text:Pansichar; maxlen:Tsize_t; fmt:Pansichar; args:array of const):longint;cdecl;external libSDL3;
function SDL_snprintf(text:Pansichar; maxlen:Tsize_t; fmt:Pansichar):longint;cdecl;external libSDL3;
function SDL_swprintf(text:Pwchar_t; maxlen:Tsize_t; fmt:Pwchar_t; args:array of const):longint;cdecl;external libSDL3;
function SDL_swprintf(text:Pwchar_t; maxlen:Tsize_t; fmt:Pwchar_t):longint;cdecl;external libSDL3;
function SDL_vsnprintf(text:Pansichar; maxlen:Tsize_t; fmt:Pansichar; ap:Tva_list):longint;cdecl;external libSDL3;
function SDL_vswprintf(text:Pwchar_t; maxlen:Tsize_t; fmt:Pwchar_t; ap:Tva_list):longint;cdecl;external libSDL3;
function SDL_asprintf(strp:PPansichar; fmt:Pansichar; args:array of const):longint;cdecl;external libSDL3;
function SDL_asprintf(strp:PPansichar; fmt:Pansichar):longint;cdecl;external libSDL3;
function SDL_vasprintf(strp:PPansichar; fmt:Pansichar; ap:Tva_list):longint;cdecl;external libSDL3;
{*
 * Seeds the pseudo-random number generator.
 *
 * Reusing the seed number will cause SDL_rand_*() to repeat the same stream
 * of 'random' numbers.
 *
 * \param seed the value to use as a random number seed, or 0 to use
 *             SDL_GetPerformanceCounter().
 *
 * \threadsafety This should be called on the same thread that calls
 *               SDL_rand*()
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_rand
 * \sa SDL_rand_bits
 * \sa SDL_randf
  }
procedure SDL_srand(seed:TUint64);cdecl;external libSDL3;
{*
 * Generate a pseudo-random number less than n for positive n
 *
 * The method used is faster and of better quality than `rand() % n`. Odds are
 * roughly 99.9% even for n = 1 million. Evenness is better for smaller n, and
 * much worse as n gets bigger.
 *
 * Example: to simulate a d6 use `SDL_rand(6) + 1` The +1 converts 0..5 to
 * 1..6
 *
 * If you want to generate a pseudo-random number in the full range of Sint32,
 * you should use: (Sint32)SDL_rand_bits()
 *
 * If you want reproducible output, be sure to initialize with SDL_srand()
 * first.
 *
 * There are no guarantees as to the quality of the random sequence produced,
 * and this should not be used for security (cryptography, passwords) or where
 * money is on the line (loot-boxes, casinos). There are many random number
 * libraries available with different characteristics and you should pick one
 * of those to meet any serious needs.
 *
 * \param n the number of possible outcomes. n must be positive.
 * \returns a random value in the range of [0 .. n-1].
 *
 * \threadsafety All calls should be made from a single thread
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_srand
 * \sa SDL_randf
  }
function SDL_rand(n:TSint32):TSint32;cdecl;external libSDL3;
{*
 * Generate a uniform pseudo-random floating point number less than 1.0
 *
 * If you want reproducible output, be sure to initialize with SDL_srand()
 * first.
 *
 * There are no guarantees as to the quality of the random sequence produced,
 * and this should not be used for security (cryptography, passwords) or where
 * money is on the line (loot-boxes, casinos). There are many random number
 * libraries available with different characteristics and you should pick one
 * of those to meet any serious needs.
 *
 * \returns a random value in the range of [0.0, 1.0).
 *
 * \threadsafety All calls should be made from a single thread
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_srand
 * \sa SDL_rand
  }
function SDL_randf:single;cdecl;external libSDL3;
{*
 * Generate 32 pseudo-random bits.
 *
 * You likely want to use SDL_rand() to get a psuedo-random number instead.
 *
 * There are no guarantees as to the quality of the random sequence produced,
 * and this should not be used for security (cryptography, passwords) or where
 * money is on the line (loot-boxes, casinos). There are many random number
 * libraries available with different characteristics and you should pick one
 * of those to meet any serious needs.
 *
 * \returns a random value in the range of [0-SDL_MAX_UINT32].
 *
 * \threadsafety All calls should be made from a single thread
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_rand
 * \sa SDL_randf
 * \sa SDL_srand
  }
function SDL_rand_bits:TUint32;cdecl;external libSDL3;
{*
 * Generate a pseudo-random number less than n for positive n
 *
 * The method used is faster and of better quality than `rand() % n`. Odds are
 * roughly 99.9% even for n = 1 million. Evenness is better for smaller n, and
 * much worse as n gets bigger.
 *
 * Example: to simulate a d6 use `SDL_rand_r(state, 6) + 1` The +1 converts
 * 0..5 to 1..6
 *
 * If you want to generate a pseudo-random number in the full range of Sint32,
 * you should use: (Sint32)SDL_rand_bits_r(state)
 *
 * There are no guarantees as to the quality of the random sequence produced,
 * and this should not be used for security (cryptography, passwords) or where
 * money is on the line (loot-boxes, casinos). There are many random number
 * libraries available with different characteristics and you should pick one
 * of those to meet any serious needs.
 *
 * \param state a pointer to the current random number state, this may not be
 *              NULL.
 * \param n the number of possible outcomes. n must be positive.
 * \returns a random value in the range of [0 .. n-1].
 *
 * \threadsafety This function is thread-safe, as long as the state pointer
 *               isn't shared between threads.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_rand
 * \sa SDL_rand_bits_r
 * \sa SDL_randf_r
  }
function SDL_rand_r(state:PUint64; n:TSint32):TSint32;cdecl;external libSDL3;
{*
 * Generate a uniform pseudo-random floating point number less than 1.0
 *
 * If you want reproducible output, be sure to initialize with SDL_srand()
 * first.
 *
 * There are no guarantees as to the quality of the random sequence produced,
 * and this should not be used for security (cryptography, passwords) or where
 * money is on the line (loot-boxes, casinos). There are many random number
 * libraries available with different characteristics and you should pick one
 * of those to meet any serious needs.
 *
 * \param state a pointer to the current random number state, this may not be
 *              NULL.
 * \returns a random value in the range of [0.0, 1.0).
 *
 * \threadsafety This function is thread-safe, as long as the state pointer
 *               isn't shared between threads.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_rand_bits_r
 * \sa SDL_rand_r
 * \sa SDL_randf
  }
function SDL_randf_r(state:PUint64):single;cdecl;external libSDL3;
{*
 * Generate 32 pseudo-random bits.
 *
 * You likely want to use SDL_rand_r() to get a psuedo-random number instead.
 *
 * There are no guarantees as to the quality of the random sequence produced,
 * and this should not be used for security (cryptography, passwords) or where
 * money is on the line (loot-boxes, casinos). There are many random number
 * libraries available with different characteristics and you should pick one
 * of those to meet any serious needs.
 *
 * \param state a pointer to the current random number state, this may not be
 *              NULL.
 * \returns a random value in the range of [0-SDL_MAX_UINT32].
 *
 * \threadsafety This function is thread-safe, as long as the state pointer
 *               isn't shared between threads.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_rand_r
 * \sa SDL_randf_r
  }
function SDL_rand_bits_r(state:PUint64):TUint32;cdecl;external libSDL3;
{$ifndef SDL_PI_D}
{*< pi (double)  }
{ was #define dname def_expr }
function SDL_PI_D : Tdouble;  

{$endif}
{$ifndef SDL_PI_F}
{*< pi (float)  }
{ was #define dname def_expr }
function SDL_PI_F : single;  

{$endif}
{*
 * Compute the arc cosine of `x`.
 *
 * The definition of `y = acos(x)` is `x = cos(y)`.
 *
 * Domain: `-1 <= x <= 1`
 *
 * Range: `0 <= y <= Pi`
 *
 * This function operates on double-precision floating point values, use
 * SDL_acosf for single-precision floats.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x floating point value.
 * \returns arc cosine of `x`, in radians.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_acosf
 * \sa SDL_asin
 * \sa SDL_cos
  }
function SDL_acos(x:Tdouble):Tdouble;cdecl;external libSDL3;
{*
 * Compute the arc cosine of `x`.
 *
 * The definition of `y = acos(x)` is `x = cos(y)`.
 *
 * Domain: `-1 <= x <= 1`
 *
 * Range: `0 <= y <= Pi`
 *
 * This function operates on single-precision floating point values, use
 * SDL_acos for double-precision floats.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x floating point value.
 * \returns arc cosine of `x`, in radians.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_acos
 * \sa SDL_asinf
 * \sa SDL_cosf
  }
function SDL_acosf(x:single):single;cdecl;external libSDL3;
{*
 * Compute the arc sine of `x`.
 *
 * The definition of `y = asin(x)` is `x = sin(y)`.
 *
 * Domain: `-1 <= x <= 1`
 *
 * Range: `-Pi/2 <= y <= Pi/2`
 *
 * This function operates on double-precision floating point values, use
 * SDL_asinf for single-precision floats.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x floating point value.
 * \returns arc sine of `x`, in radians.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_asinf
 * \sa SDL_acos
 * \sa SDL_sin
  }
function SDL_asin(x:Tdouble):Tdouble;cdecl;external libSDL3;
{*
 * Compute the arc sine of `x`.
 *
 * The definition of `y = asin(x)` is `x = sin(y)`.
 *
 * Domain: `-1 <= x <= 1`
 *
 * Range: `-Pi/2 <= y <= Pi/2`
 *
 * This function operates on single-precision floating point values, use
 * SDL_asin for double-precision floats.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x floating point value.
 * \returns arc sine of `x`, in radians.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_asin
 * \sa SDL_acosf
 * \sa SDL_sinf
  }
function SDL_asinf(x:single):single;cdecl;external libSDL3;
{*
 * Compute the arc tangent of `x`.
 *
 * The definition of `y = atan(x)` is `x = tan(y)`.
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `-Pi/2 <= y <= Pi/2`
 *
 * This function operates on double-precision floating point values, use
 * SDL_atanf for single-precision floats.
 *
 * To calculate the arc tangent of y / x, use SDL_atan2.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x floating point value.
 * \returns arc tangent of of `x` in radians, or 0 if `x = 0`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_atanf
 * \sa SDL_atan2
 * \sa SDL_tan
  }
function SDL_atan(x:Tdouble):Tdouble;cdecl;external libSDL3;
{*
 * Compute the arc tangent of `x`.
 *
 * The definition of `y = atan(x)` is `x = tan(y)`.
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `-Pi/2 <= y <= Pi/2`
 *
 * This function operates on single-precision floating point values, use
 * SDL_atan for dboule-precision floats.
 *
 * To calculate the arc tangent of y / x, use SDL_atan2f.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x floating point value.
 * \returns arc tangent of of `x` in radians, or 0 if `x = 0`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_atan
 * \sa SDL_atan2f
 * \sa SDL_tanf
  }
function SDL_atanf(x:single):single;cdecl;external libSDL3;
{*
 * Compute the arc tangent of `y / x`, using the signs of x and y to adjust
 * the result's quadrant.
 *
 * The definition of `z = atan2(x, y)` is `y = x tan(z)`, where the quadrant
 * of z is determined based on the signs of x and y.
 *
 * Domain: `-INF <= x <= INF`, `-INF <= y <= INF`
 *
 * Range: `-Pi/2 <= y <= Pi/2`
 *
 * This function operates on double-precision floating point values, use
 * SDL_atan2f for single-precision floats.
 *
 * To calculate the arc tangent of a single value, use SDL_atan.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param y floating point value of the numerator (y coordinate).
 * \param x floating point value of the denominator (x coordinate).
 * \returns arc tangent of of `y / x` in radians, or, if `x = 0`, either
 *          `-Pi/2`, `0`, or `Pi/2`, depending on the value of `y`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_atan2f
 * \sa SDL_atan
 * \sa SDL_tan
  }
function SDL_atan2(y:Tdouble; x:Tdouble):Tdouble;cdecl;external libSDL3;
{*
 * Compute the arc tangent of `y / x`, using the signs of x and y to adjust
 * the result's quadrant.
 *
 * The definition of `z = atan2(x, y)` is `y = x tan(z)`, where the quadrant
 * of z is determined based on the signs of x and y.
 *
 * Domain: `-INF <= x <= INF`, `-INF <= y <= INF`
 *
 * Range: `-Pi/2 <= y <= Pi/2`
 *
 * This function operates on single-precision floating point values, use
 * SDL_atan2 for double-precision floats.
 *
 * To calculate the arc tangent of a single value, use SDL_atanf.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param y floating point value of the numerator (y coordinate).
 * \param x floating point value of the denominator (x coordinate).
 * \returns arc tangent of of `y / x` in radians, or, if `x = 0`, either
 *          `-Pi/2`, `0`, or `Pi/2`, depending on the value of `y`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_atan2f
 * \sa SDL_atan
 * \sa SDL_tan
  }
function SDL_atan2f(y:single; x:single):single;cdecl;external libSDL3;
{*
 * Compute the ceiling of `x`.
 *
 * The ceiling of `x` is the smallest integer `y` such that `y > x`, i.e `x`
 * rounded up to the nearest integer.
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `-INF <= y <= INF`, y integer
 *
 * This function operates on double-precision floating point values, use
 * SDL_ceilf for single-precision floats.
 *
 * \param x floating point value.
 * \returns the ceiling of `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_ceilf
 * \sa SDL_floor
 * \sa SDL_trunc
 * \sa SDL_round
 * \sa SDL_lround
  }
function SDL_ceil(x:Tdouble):Tdouble;cdecl;external libSDL3;
{*
 * Compute the ceiling of `x`.
 *
 * The ceiling of `x` is the smallest integer `y` such that `y > x`, i.e `x`
 * rounded up to the nearest integer.
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `-INF <= y <= INF`, y integer
 *
 * This function operates on single-precision floating point values, use
 * SDL_ceil for double-precision floats.
 *
 * \param x floating point value.
 * \returns the ceiling of `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_ceil
 * \sa SDL_floorf
 * \sa SDL_truncf
 * \sa SDL_roundf
 * \sa SDL_lroundf
  }
function SDL_ceilf(x:single):single;cdecl;external libSDL3;
{*
 * Copy the sign of one floating-point value to another.
 *
 * The definition of copysign is that ``copysign(x, y) = abs(x) * sign(y)``.
 *
 * Domain: `-INF <= x <= INF`, ``-INF <= y <= f``
 *
 * Range: `-INF <= z <= INF`
 *
 * This function operates on double-precision floating point values, use
 * SDL_copysignf for single-precision floats.
 *
 * \param x floating point value to use as the magnitude.
 * \param y floating point value to use as the sign.
 * \returns the floating point value with the sign of y and the magnitude of
 *          x.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_copysignf
 * \sa SDL_fabs
  }
function SDL_copysign(x:Tdouble; y:Tdouble):Tdouble;cdecl;external libSDL3;
{*
 * Copy the sign of one floating-point value to another.
 *
 * The definition of copysign is that ``copysign(x, y) = abs(x) * sign(y)``.
 *
 * Domain: `-INF <= x <= INF`, ``-INF <= y <= f``
 *
 * Range: `-INF <= z <= INF`
 *
 * This function operates on single-precision floating point values, use
 * SDL_copysign for double-precision floats.
 *
 * \param x floating point value to use as the magnitude.
 * \param y floating point value to use as the sign.
 * \returns the floating point value with the sign of y and the magnitude of
 *          x.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_copysignf
 * \sa SDL_fabsf
  }
function SDL_copysignf(x:single; y:single):single;cdecl;external libSDL3;
{*
 * Compute the cosine of `x`.
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `-1 <= y <= 1`
 *
 * This function operates on double-precision floating point values, use
 * SDL_cosf for single-precision floats.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x floating point value, in radians.
 * \returns cosine of `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_cosf
 * \sa SDL_acos
 * \sa SDL_sin
  }
function SDL_cos(x:Tdouble):Tdouble;cdecl;external libSDL3;
{*
 * Compute the cosine of `x`.
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `-1 <= y <= 1`
 *
 * This function operates on single-precision floating point values, use
 * SDL_cos for double-precision floats.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x floating point value, in radians.
 * \returns cosine of `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_cos
 * \sa SDL_acosf
 * \sa SDL_sinf
  }
function SDL_cosf(x:single):single;cdecl;external libSDL3;
{*
 * Compute the exponential of `x`.
 *
 * The definition of `y = exp(x)` is `y = e^x`, where `e` is the base of the
 * natural logarithm. The inverse is the natural logarithm, SDL_log.
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `0 <= y <= INF`
 *
 * The output will overflow if `exp(x)` is too large to be represented.
 *
 * This function operates on double-precision floating point values, use
 * SDL_expf for single-precision floats.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x floating point value.
 * \returns value of `e^x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_expf
 * \sa SDL_log
  }
function SDL_exp(x:Tdouble):Tdouble;cdecl;external libSDL3;
{*
 * Compute the exponential of `x`.
 *
 * The definition of `y = exp(x)` is `y = e^x`, where `e` is the base of the
 * natural logarithm. The inverse is the natural logarithm, SDL_logf.
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `0 <= y <= INF`
 *
 * The output will overflow if `exp(x)` is too large to be represented.
 *
 * This function operates on single-precision floating point values, use
 * SDL_exp for double-precision floats.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x floating point value.
 * \returns value of `e^x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_exp
 * \sa SDL_logf
  }
function SDL_expf(x:single):single;cdecl;external libSDL3;
{*
 * Compute the absolute value of `x`
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `0 <= y <= INF`
 *
 * This function operates on double-precision floating point values, use
 * SDL_copysignf for single-precision floats.
 *
 * \param x floating point value to use as the magnitude.
 * \returns the absolute value of `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_fabsf
  }
function SDL_fabs(x:Tdouble):Tdouble;cdecl;external libSDL3;
{*
 * Compute the absolute value of `x`
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `0 <= y <= INF`
 *
 * This function operates on single-precision floating point values, use
 * SDL_copysignf for double-precision floats.
 *
 * \param x floating point value to use as the magnitude.
 * \returns the absolute value of `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_fabs
  }
function SDL_fabsf(x:single):single;cdecl;external libSDL3;
{*
 * Compute the floor of `x`.
 *
 * The floor of `x` is the largest integer `y` such that `y > x`, i.e `x`
 * rounded down to the nearest integer.
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `-INF <= y <= INF`, y integer
 *
 * This function operates on double-precision floating point values, use
 * SDL_floorf for single-precision floats.
 *
 * \param x floating point value.
 * \returns the floor of `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_floorf
 * \sa SDL_ceil
 * \sa SDL_trunc
 * \sa SDL_round
 * \sa SDL_lround
  }
function SDL_floor(x:Tdouble):Tdouble;cdecl;external libSDL3;
{*
 * Compute the floor of `x`.
 *
 * The floor of `x` is the largest integer `y` such that `y > x`, i.e `x`
 * rounded down to the nearest integer.
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `-INF <= y <= INF`, y integer
 *
 * This function operates on single-precision floating point values, use
 * SDL_floorf for double-precision floats.
 *
 * \param x floating point value.
 * \returns the floor of `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_floor
 * \sa SDL_ceilf
 * \sa SDL_truncf
 * \sa SDL_roundf
 * \sa SDL_lroundf
  }
function SDL_floorf(x:single):single;cdecl;external libSDL3;
{*
 * Truncate `x` to an integer.
 *
 * Rounds `x` to the next closest integer to 0. This is equivalent to removing
 * the fractional part of `x`, leaving only the integer part.
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `-INF <= y <= INF`, y integer
 *
 * This function operates on double-precision floating point values, use
 * SDL_truncf for single-precision floats.
 *
 * \param x floating point value.
 * \returns `x` truncated to an integer.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_truncf
 * \sa SDL_fmod
 * \sa SDL_ceil
 * \sa SDL_floor
 * \sa SDL_round
 * \sa SDL_lround
  }
function SDL_trunc(x:Tdouble):Tdouble;cdecl;external libSDL3;
{*
 * Truncate `x` to an integer.
 *
 * Rounds `x` to the next closest integer to 0. This is equivalent to removing
 * the fractional part of `x`, leaving only the integer part.
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `-INF <= y <= INF`, y integer
 *
 * This function operates on single-precision floating point values, use
 * SDL_truncf for double-precision floats.
 *
 * \param x floating point value.
 * \returns `x` truncated to an integer.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_trunc
 * \sa SDL_fmodf
 * \sa SDL_ceilf
 * \sa SDL_floorf
 * \sa SDL_roundf
 * \sa SDL_lroundf
  }
function SDL_truncf(x:single):single;cdecl;external libSDL3;
{*
 * Return the floating-point remainder of `x / y`
 *
 * Divides `x` by `y`, and returns the remainder.
 *
 * Domain: `-INF <= x <= INF`, `-INF <= y <= INF`, `y != 0`
 *
 * Range: `-y <= z <= y`
 *
 * This function operates on double-precision floating point values, use
 * SDL_fmodf for single-precision floats.
 *
 * \param x the numerator.
 * \param y the denominator. Must not be 0.
 * \returns the remainder of `x / y`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_fmodf
 * \sa SDL_modf
 * \sa SDL_trunc
 * \sa SDL_ceil
 * \sa SDL_floor
 * \sa SDL_round
 * \sa SDL_lround
  }
function SDL_fmod(x:Tdouble; y:Tdouble):Tdouble;cdecl;external libSDL3;
{*
 * Return the floating-point remainder of `x / y`
 *
 * Divides `x` by `y`, and returns the remainder.
 *
 * Domain: `-INF <= x <= INF`, `-INF <= y <= INF`, `y != 0`
 *
 * Range: `-y <= z <= y`
 *
 * This function operates on single-precision floating point values, use
 * SDL_fmod for single-precision floats.
 *
 * \param x the numerator.
 * \param y the denominator. Must not be 0.
 * \returns the remainder of `x / y`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_fmod
 * \sa SDL_truncf
 * \sa SDL_modff
 * \sa SDL_ceilf
 * \sa SDL_floorf
 * \sa SDL_roundf
 * \sa SDL_lroundf
  }
function SDL_fmodf(x:single; y:single):single;cdecl;external libSDL3;
{*
 * Return whether the value is infinity.
 *
 * \param x double-precision floating point value.
 * \returns non-zero if the value is infinity, 0 otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_isinff
  }
function SDL_isinf(x:Tdouble):longint;cdecl;external libSDL3;
{*
 * Return whether the value is infinity.
 *
 * \param x floating point value.
 * \returns non-zero if the value is infinity, 0 otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_isinf
  }
function SDL_isinff(x:single):longint;cdecl;external libSDL3;
{*
 * Return whether the value is NaN.
 *
 * \param x double-precision floating point value.
 * \returns non-zero if the value is NaN, 0 otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_isnanf
  }
function SDL_isnan(x:Tdouble):longint;cdecl;external libSDL3;
{*
 * Return whether the value is NaN.
 *
 * \param x floating point value.
 * \returns non-zero if the value is NaN, 0 otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_isnan
  }
function SDL_isnanf(x:single):longint;cdecl;external libSDL3;
{*
 * Compute the natural logarithm of `x`.
 *
 * Domain: `0 < x <= INF`
 *
 * Range: `-INF <= y <= INF`
 *
 * It is an error for `x` to be less than or equal to 0.
 *
 * This function operates on double-precision floating point values, use
 * SDL_logf for single-precision floats.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x floating point value. Must be greater than 0.
 * \returns the natural logarithm of `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_logf
 * \sa SDL_log10
 * \sa SDL_exp
  }
function SDL_log(x:Tdouble):Tdouble;cdecl;external libSDL3;
{*
 * Compute the natural logarithm of `x`.
 *
 * Domain: `0 < x <= INF`
 *
 * Range: `-INF <= y <= INF`
 *
 * It is an error for `x` to be less than or equal to 0.
 *
 * This function operates on single-precision floating point values, use
 * SDL_log for double-precision floats.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x floating point value. Must be greater than 0.
 * \returns the natural logarithm of `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_log
 * \sa SDL_expf
  }
function SDL_logf(x:single):single;cdecl;external libSDL3;
{*
 * Compute the base-10 logarithm of `x`.
 *
 * Domain: `0 < x <= INF`
 *
 * Range: `-INF <= y <= INF`
 *
 * It is an error for `x` to be less than or equal to 0.
 *
 * This function operates on double-precision floating point values, use
 * SDL_log10f for single-precision floats.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x floating point value. Must be greater than 0.
 * \returns the logarithm of `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_log10f
 * \sa SDL_log
 * \sa SDL_pow
  }
function SDL_log10(x:Tdouble):Tdouble;cdecl;external libSDL3;
{*
 * Compute the base-10 logarithm of `x`.
 *
 * Domain: `0 < x <= INF`
 *
 * Range: `-INF <= y <= INF`
 *
 * It is an error for `x` to be less than or equal to 0.
 *
 * This function operates on single-precision floating point values, use
 * SDL_log10 for double-precision floats.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x floating point value. Must be greater than 0.
 * \returns the logarithm of `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_log10
 * \sa SDL_logf
 * \sa SDL_powf
  }
function SDL_log10f(x:single):single;cdecl;external libSDL3;
{*
 * Split `x` into integer and fractional parts
 *
 * This function operates on double-precision floating point values, use
 * SDL_modff for single-precision floats.
 *
 * \param x floating point value.
 * \param y output pointer to store the integer part of `x`.
 * \returns the fractional part of `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_modff
 * \sa SDL_trunc
 * \sa SDL_fmod
  }
function SDL_modf(x:Tdouble; y:Pdouble):Tdouble;cdecl;external libSDL3;
{*
 * Split `x` into integer and fractional parts
 *
 * This function operates on single-precision floating point values, use
 * SDL_modf for double-precision floats.
 *
 * \param x floating point value.
 * \param y output pointer to store the integer part of `x`.
 * \returns the fractional part of `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_modf
 * \sa SDL_truncf
 * \sa SDL_fmodf
  }
function SDL_modff(x:single; y:Psingle):single;cdecl;external libSDL3;
{*
 * Raise `x` to the power `y`
 *
 * Domain: `-INF <= x <= INF`, `-INF <= y <= INF`
 *
 * Range: `-INF <= z <= INF`
 *
 * If `y` is the base of the natural logarithm (e), consider using SDL_exp
 * instead.
 *
 * This function operates on double-precision floating point values, use
 * SDL_powf for single-precision floats.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x the base.
 * \param y the exponent.
 * \returns `x` raised to the power `y`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_powf
 * \sa SDL_exp
 * \sa SDL_log
  }
function SDL_pow(x:Tdouble; y:Tdouble):Tdouble;cdecl;external libSDL3;
{*
 * Raise `x` to the power `y`
 *
 * Domain: `-INF <= x <= INF`, `-INF <= y <= INF`
 *
 * Range: `-INF <= z <= INF`
 *
 * If `y` is the base of the natural logarithm (e), consider using SDL_exp
 * instead.
 *
 * This function operates on single-precision floating point values, use
 * SDL_powf for double-precision floats.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x the base.
 * \param y the exponent.
 * \returns `x` raised to the power `y`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_pow
 * \sa SDL_expf
 * \sa SDL_logf
  }
function SDL_powf(x:single; y:single):single;cdecl;external libSDL3;
{*
 * Round `x` to the nearest integer.
 *
 * Rounds `x` to the nearest integer. Values halfway between integers will be
 * rounded away from zero.
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `-INF <= y <= INF`, y integer
 *
 * This function operates on double-precision floating point values, use
 * SDL_roundf for single-precision floats. To get the result as an integer
 * type, use SDL_lround.
 *
 * \param x floating point value.
 * \returns the nearest integer to `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_roundf
 * \sa SDL_lround
 * \sa SDL_floor
 * \sa SDL_ceil
 * \sa SDL_trunc
  }
function SDL_round(x:Tdouble):Tdouble;cdecl;external libSDL3;
{*
 * Round `x` to the nearest integer.
 *
 * Rounds `x` to the nearest integer. Values halfway between integers will be
 * rounded away from zero.
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `-INF <= y <= INF`, y integer
 *
 * This function operates on double-precision floating point values, use
 * SDL_roundf for single-precision floats. To get the result as an integer
 * type, use SDL_lroundf.
 *
 * \param x floating point value.
 * \returns the nearest integer to `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_round
 * \sa SDL_lroundf
 * \sa SDL_floorf
 * \sa SDL_ceilf
 * \sa SDL_truncf
  }
function SDL_roundf(x:single):single;cdecl;external libSDL3;
{*
 * Round `x` to the nearest integer representable as a long
 *
 * Rounds `x` to the nearest integer. Values halfway between integers will be
 * rounded away from zero.
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `MIN_LONG <= y <= MAX_LONG`
 *
 * This function operates on double-precision floating point values, use
 * SDL_lround for single-precision floats. To get the result as a
 * floating-point type, use SDL_round.
 *
 * \param x floating point value.
 * \returns the nearest integer to `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_lroundf
 * \sa SDL_round
 * \sa SDL_floor
 * \sa SDL_ceil
 * \sa SDL_trunc
  }
function SDL_lround(x:Tdouble):longint;cdecl;external libSDL3;
{*
 * Round `x` to the nearest integer representable as a long
 *
 * Rounds `x` to the nearest integer. Values halfway between integers will be
 * rounded away from zero.
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `MIN_LONG <= y <= MAX_LONG`
 *
 * This function operates on single-precision floating point values, use
 * SDL_lroundf for double-precision floats. To get the result as a
 * floating-point type, use SDL_roundf,
 *
 * \param x floating point value.
 * \returns the nearest integer to `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_lround
 * \sa SDL_roundf
 * \sa SDL_floorf
 * \sa SDL_ceilf
 * \sa SDL_truncf
  }
function SDL_lroundf(x:single):longint;cdecl;external libSDL3;
{*
 * Scale `x` by an integer power of two.
 *
 * Multiplies `x` by the `n`th power of the floating point radix (always 2).
 *
 * Domain: `-INF <= x <= INF`, `n` integer
 *
 * Range: `-INF <= y <= INF`
 *
 * This function operates on double-precision floating point values, use
 * SDL_scalbnf for single-precision floats.
 *
 * \param x floating point value to be scaled.
 * \param n integer exponent.
 * \returns `x * 2^n`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_scalbnf
 * \sa SDL_pow
  }
function SDL_scalbn(x:Tdouble; n:longint):Tdouble;cdecl;external libSDL3;
{*
 * Scale `x` by an integer power of two.
 *
 * Multiplies `x` by the `n`th power of the floating point radix (always 2).
 *
 * Domain: `-INF <= x <= INF`, `n` integer
 *
 * Range: `-INF <= y <= INF`
 *
 * This function operates on single-precision floating point values, use
 * SDL_scalbn for double-precision floats.
 *
 * \param x floating point value to be scaled.
 * \param n integer exponent.
 * \returns `x * 2^n`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_scalbn
 * \sa SDL_powf
  }
function SDL_scalbnf(x:single; n:longint):single;cdecl;external libSDL3;
{*
 * Compute the sine of `x`.
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `-1 <= y <= 1`
 *
 * This function operates on double-precision floating point values, use
 * SDL_sinf for single-precision floats.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x floating point value, in radians.
 * \returns sine of `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_sinf
 * \sa SDL_asin
 * \sa SDL_cos
  }
function SDL_sin(x:Tdouble):Tdouble;cdecl;external libSDL3;
{*
 * Compute the sine of `x`.
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `-1 <= y <= 1`
 *
 * This function operates on single-precision floating point values, use
 * SDL_sinf for double-precision floats.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x floating point value, in radians.
 * \returns sine of `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_sin
 * \sa SDL_asinf
 * \sa SDL_cosf
  }
function SDL_sinf(x:single):single;cdecl;external libSDL3;
{*
 * Compute the square root of `x`.
 *
 * Domain: `0 <= x <= INF`
 *
 * Range: `0 <= y <= INF`
 *
 * This function operates on double-precision floating point values, use
 * SDL_sqrtf for single-precision floats.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x floating point value. Must be greater than or equal to 0.
 * \returns square root of `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_sqrtf
  }
function SDL_sqrt(x:Tdouble):Tdouble;cdecl;external libSDL3;
{*
 * Compute the square root of `x`.
 *
 * Domain: `0 <= x <= INF`
 *
 * Range: `0 <= y <= INF`
 *
 * This function operates on single-precision floating point values, use
 * SDL_sqrt for double-precision floats.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x floating point value. Must be greater than or equal to 0.
 * \returns square root of `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_sqrt
  }
function SDL_sqrtf(x:single):single;cdecl;external libSDL3;
{*
 * Compute the tangent of `x`.
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `-INF <= y <= INF`
 *
 * This function operates on double-precision floating point values, use
 * SDL_tanf for single-precision floats.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x floating point value, in radians.
 * \returns tangent of `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_tanf
 * \sa SDL_sin
 * \sa SDL_cos
 * \sa SDL_atan
 * \sa SDL_atan2
  }
function SDL_tan(x:Tdouble):Tdouble;cdecl;external libSDL3;
{*
 * Compute the tangent of `x`.
 *
 * Domain: `-INF <= x <= INF`
 *
 * Range: `-INF <= y <= INF`
 *
 * This function operates on single-precision floating point values, use
 * SDL_tanf for double-precision floats.
 *
 * This function may use a different approximation across different versions,
 * platforms and configurations. i.e, it can return a different value given
 * the same input on different machines or operating systems, or if SDL is
 * updated.
 *
 * \param x floating point value, in radians.
 * \returns tangent of `x`.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_tan
 * \sa SDL_sinf
 * \sa SDL_cosf
 * \sa SDL_atanf
 * \sa SDL_atan2f
  }
function SDL_tanf(x:single):single;cdecl;external libSDL3;
{ The SDL implementation of iconv() returns these error codes  }
{ was #define dname def_expr }
function SDL_ICONV_ERROR : Tsize_t;  

{ was #define dname def_expr }
function SDL_ICONV_E2BIG : Tsize_t;  

{ was #define dname def_expr }
function SDL_ICONV_EILSEQ : Tsize_t;  

{ was #define dname def_expr }
function SDL_ICONV_EINVAL : Tsize_t;  

type
  PSDL_iconv_t = ^TSDL_iconv_t;
  TSDL_iconv_t = PSDL_iconv_data_t;
{*
 * This function allocates a context for the specified character set
 * conversion.
 *
 * \param tocode The target character encoding, must not be NULL.
 * \param fromcode The source character encoding, must not be NULL.
 * \returns a handle that must be freed with SDL_iconv_close, or
 *          SDL_ICONV_ERROR on failure.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_iconv
 * \sa SDL_iconv_close
 * \sa SDL_iconv_string
  }

function SDL_iconv_open(tocode:Pansichar; fromcode:Pansichar):TSDL_iconv_t;cdecl;external libSDL3;
{*
 * This function frees a context used for character set conversion.
 *
 * \param cd The character set conversion handle.
 * \returns 0 on success, or -1 on failure.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_iconv
 * \sa SDL_iconv_open
 * \sa SDL_iconv_string
  }
function SDL_iconv_close(cd:TSDL_iconv_t):longint;cdecl;external libSDL3;
{*
 * This function converts text between encodings, reading from and writing to
 * a buffer.
 *
 * It returns the number of succesful conversions.
 *
 * \param cd The character set conversion context, created in
 *           SDL_iconv_open().
 * \param inbuf Address of variable that points to the first character of the
 *              input sequence.
 * \param inbytesleft The number of bytes in the input buffer.
 * \param outbuf Address of variable that points to the output buffer.
 * \param outbytesleft The number of bytes in the output buffer.
 * \returns the number of conversions on success, else SDL_ICONV_E2BIG is
 *          returned when the output buffer is too small, or SDL_ICONV_EILSEQ
 *          is returned when an invalid input sequence is encountered, or
 *          SDL_ICONV_EINVAL is returned when an incomplete input sequence is
 *          encountered.
 *
 *          On exit:
 *
 *          - inbuf will point to the beginning of the next multibyte
 *            sequence. On error, this is the location of the problematic
 *            input sequence. On success, this is the end of the input
 *            sequence. - inbytesleft will be set to the number of bytes left
 *            to convert, which will be 0 on success. - outbuf will point to
 *            the location where to store the next output byte. - outbytesleft
 *            will be set to the number of bytes left in the output buffer.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_iconv_open
 * \sa SDL_iconv_close
 * \sa SDL_iconv_string
  }
function SDL_iconv(cd:TSDL_iconv_t; inbuf:PPansichar; inbytesleft:Psize_t; outbuf:PPansichar; outbytesleft:Psize_t):Tsize_t;cdecl;external libSDL3;
{*
 * Helper function to convert a string's encoding in one call.
 *
 * This function converts a buffer or string between encodings in one pass.
 *
 * The string does not need to be NULL-terminated; this function operates on
 * the number of bytes specified in `inbytesleft` whether there is a NULL
 * character anywhere in the buffer.
 *
 * The returned string is owned by the caller, and should be passed to
 * SDL_free when no longer needed.
 *
 * \param tocode the character encoding of the output string. Examples are
 *               "UTF-8", "UCS-4", etc.
 * \param fromcode the character encoding of data in `inbuf`.
 * \param inbuf the string to convert to a different encoding.
 * \param inbytesleft the size of the input string _in bytes_.
 * \returns a new string, converted to the new encoding, or NULL on error.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_iconv_open
 * \sa SDL_iconv_close
 * \sa SDL_iconv
  }
function SDL_iconv_string(tocode:Pansichar; fromcode:Pansichar; inbuf:Pansichar; inbytesleft:Tsize_t):Pansichar;cdecl;external libSDL3;
{ Some helper macros for common cases...  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_iconv_utf8_locale(S : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function SDL_iconv_utf8_ucs2(S : longint) : PUint16;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function SDL_iconv_utf8_ucs4(S : longint) : PUint32;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_iconv_wchar_utf8(S : longint) : longint;

{ force builds using Clang's static analysis tools to use literal C runtime
   here, since there are possibly tests that are ineffective otherwise.  }
{$if defined(__clang_analyzer__) && !defined(SDL_DISABLE_ANALYZE_MACROS)}
{ The analyzer knows about strlcpy even when the system doesn't provide it  }
{$if !defined(HAVE_STRLCPY) && !defined(strlcpy)}
function strlcpy(dst:Pansichar; src:Pansichar; size:Tsize_t):Tsize_t;cdecl;external libSDL3;
{$endif}
{ The analyzer knows about strlcat even when the system doesn't provide it  }
{$if !defined(HAVE_STRLCAT) && !defined(strlcat)}
function strlcat(dst:Pansichar; src:Pansichar; size:Tsize_t):Tsize_t;cdecl;external libSDL3;
{$endif}
{$if !defined(HAVE_WCSLCPY) && !defined(wcslcpy)}
function wcslcpy(dst:Pwchar_t; src:Pwchar_t; size:Tsize_t):Tsize_t;cdecl;external libSDL3;
{$endif}
{$if !defined(HAVE_WCSLCAT) && !defined(wcslcat)}
function wcslcat(dst:Pwchar_t; src:Pwchar_t; size:Tsize_t):Tsize_t;cdecl;external libSDL3;
{$endif}
{ Starting LLVM 16, the analyser errors out if these functions do not have
   their prototype defined (clang-diagnostic-implicit-function-declaration)  }
{$include <stdio.h>}
{$include <stdlib.h>}
{$include <strings.h>}
const
  SDL_malloc = malloc;  
  SDL_calloc = calloc;  
  SDL_realloc = realloc;  
  SDL_free = free;  
{$ifndef SDL_memcpy}
  SDL_memcpy = memcpy;  
{$endif}
{$ifndef SDL_memmove}
  SDL_memmove = memmove;  
{$endif}
{$ifndef SDL_memset}
  SDL_memset = memset;  
{$endif}
  SDL_memcmp = memcmp;  
  SDL_strlcpy = strlcpy;  
  SDL_strlcat = strlcat;  
  SDL_strlen = strlen;  
  SDL_wcslen = wcslen;  
  SDL_wcslcpy = wcslcpy;  
  SDL_wcslcat = wcslcat;  
  SDL_strdup = strdup;  
  SDL_wcsdup = wcsdup;  
  SDL_strchr = strchr;  
  SDL_strrchr = strrchr;  
  SDL_strstr = strstr;  
  SDL_wcsstr = wcsstr;  
  SDL_strtok_r = strtok_r;  
  SDL_strcmp = strcmp;  
  SDL_wcscmp = wcscmp;  
  SDL_strncmp = strncmp;  
  SDL_wcsncmp = wcsncmp;  
  SDL_strcasecmp = strcasecmp;  
  SDL_strncasecmp = strncasecmp;  
  SDL_strpbrk = strpbrk;  
  SDL_sscanf = sscanf;  
  SDL_vsscanf = vsscanf;  
  SDL_snprintf = snprintf;  
  SDL_vsnprintf = vsnprintf;  
{$endif}
{*
 * Multiply two integers, checking for overflow.
 *
 * If `a * b` would overflow, return false.
 *
 * Otherwise store `a * b` via ret and return true.
 *
 * \param a the multiplicand.
 * \param b the multiplier.
 * \param ret on non-overflow output, stores the multiplication result, may
 *            not be NULL.
 * \returns false on overflow, true if result is multiplied without overflow.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
{ xxxxxxxxxxxxxxxx }
{
SDL_FORCE_INLINE bool SDL_size_mul_check_overflow(size_t a, size_t b, size_t *ret)

    if (a != 0 && b > SDL_SIZE_MAX / a) 
        return false;
    
    *ret = a * b;
    return true;

 }
{$ifndef SDL_WIKI_DOCUMENTATION_SECTION}
{$if SDL_HAS_BUILTIN(__builtin_mul_overflow)}
{ This needs to be wrapped in an inline rather than being a direct #define,
 * because __builtin_mul_overflow() is type-generic, but we want to be
 * consistent about interpreting a and b as size_t.  }
{ xxxxxxxxxxxxxxxxx }
{
SDL_FORCE_INLINE bool SDL_size_mul_check_overflow_builtin(size_t a, size_t b, size_t *ret)

    return (__builtin_mul_overflow(a, b, ret) == 0);

 }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

function SDL_size_mul_check_overflow(a,b,ret : longint) : longint;

{$endif}
{$endif}
{*
 * Add two integers, checking for overflow.
 *
 * If `a + b` would overflow, return -1.
 *
 * Otherwise store `a + b` via ret and return 0.
 *
 * \param a the first addend.
 * \param b the second addend.
 * \param ret on non-overflow output, stores the addition result, may not be
 *            NULL.
 * \returns false on overflow, true if result is added without overflow.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
{
// xxxxxxxxxxxxxxxxx
SDL_FORCE_INLINE bool SDL_size_add_check_overflow(size_t a, size_t b, size_t *ret)

    if (b > SDL_SIZE_MAX - a) 
        return false;
    
    *ret = a + b;
    return true;

 }
{$ifndef SDL_WIKI_DOCUMENTATION_SECTION}
{$if SDL_HAS_BUILTIN(__builtin_add_overflow)}
{ This needs to be wrapped in an inline rather than being a direct #define,
 * the same as the call to __builtin_mul_overflow() above.  }
{ xxxxxxxxxxxxxxxxx }
{
SDL_FORCE_INLINE bool SDL_size_add_check_overflow_builtin(size_t a, size_t b, size_t *ret)

    return (__builtin_add_overflow(a, b, ret) == 0);

 }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_size_add_check_overflow(a,b,ret : longint) : longint;

{$endif}
{$endif}
{ This is a generic function pointer which should be cast to the type you expect  }
{$ifdef SDL_WIKI_DOCUMENTATION_SECTION}
{*
 * A generic function pointer.
 *
 * In theory, generic function pointers should use this, instead of `void *`,
 * since some platforms could treat code addresses differently than data
 * addresses. Although in current times no popular platforms make this
 * distinction, it is more correct and portable to use the correct type for a
 * generic pointer.
 *
 * If for some reason you need to force this typedef to be an actual `void *`,
 * perhaps to work around a compiler or existing code, you can define
 * `SDL_FUNCTION_POINTER_IS_VOID_POINTER` before including any SDL headers.
 *
 * \since This datatype is available since SDL 3.0.0.
  }
type

  TSDL_FunctionPointer = procedure (para1:pointer);cdecl;
(*** was #elif ****){$else defined(SDL_FUNCTION_POINTER_IS_VOID_POINTER)}

  PSDL_FunctionPointer = ^TSDL_FunctionPointer;
  TSDL_FunctionPointer = pointer;
{$else}

  TSDL_FunctionPointer = procedure (para1:pointer);cdecl;
{$endif}
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_stdinc_h_  }

implementation

{ was #define dname def_expr }
function SDL_SIZE_MAX : Tsize_t;
  begin
    SDL_SIZE_MAX:=Tsize_t(-(1));
  end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_HAS_BUILTIN(x : longint) : longint;
begin
  SDL_HAS_BUILTIN:=__has_builtin(x);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_HAS_BUILTIN(x : longint) : longint;
begin
  SDL_HAS_BUILTIN:=&;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_arraysize(array : longint) : longint;
begin
  SDL_arraysize:=(sizeof(array))/(sizeof(array[&]));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_FOURCC(A,B,C,D : longint) : longint;
begin
  SDL_FOURCC:=((((SDL_static_cast(Uint32,SDL_static_cast(Uint8,A))) shl &) or ((SDL_static_cast(Uint32,SDL_static_cast(Uint8,B))) shl 8)) or ((SDL_static_cast(Uint32,SDL_static_cast(Uint8,C))) shl 16)) or ((SDL_static_cast(Uint32,SDL_static_cast(Uint8,D))) shl 24);
end;

{ was #define dname def_expr }
function SDL_MAX_SINT8 : TSint8;
  begin
    SDL_MAX_SINT8:=TSint8($7F);
  end;

{ was #define dname def_expr }
function SDL_MIN_SINT8 : TSint8;
  begin
    SDL_MIN_SINT8:=TSint8( not ($7F));
  end;

{ was #define dname def_expr }
function SDL_MAX_UINT8 : TUint8;
  begin
    SDL_MAX_UINT8:=TUint8($FF);
  end;

{ was #define dname def_expr }
function SDL_MIN_UINT8 : TUint8;
  begin
    SDL_MIN_UINT8:=TUint8($00);
  end;

{ was #define dname def_expr }
function SDL_MAX_SINT16 : TSint16;
  begin
    SDL_MAX_SINT16:=TSint16($7FFF);
  end;

{ was #define dname def_expr }
function SDL_MIN_SINT16 : TSint16;
  begin
    SDL_MIN_SINT16:=TSint16( not ($7FFF));
  end;

{ was #define dname def_expr }
function SDL_MAX_UINT16 : TUint16;
  begin
    SDL_MAX_UINT16:=TUint16($FFFF);
  end;

{ was #define dname def_expr }
function SDL_MIN_UINT16 : TUint16;
  begin
    SDL_MIN_UINT16:=TUint16($0000);
  end;

{ was #define dname def_expr }
function SDL_MAX_SINT32 : TSint32;
  begin
    SDL_MAX_SINT32:=TSint32($7FFFFFFF);
  end;

{ was #define dname def_expr }
function SDL_MIN_SINT32 : TSint32;
  begin
    SDL_MIN_SINT32:=TSint32( not ($7FFFFFFF));
  end;

{ was #define dname def_expr }
function SDL_MAX_UINT32 : TUint32;
  begin
    SDL_MAX_UINT32:=TUint32($FFFFFFFF);
  end;

{ was #define dname def_expr }
function SDL_MIN_UINT32 : TUint32;
  begin
    SDL_MIN_UINT32:=TUint32($00000000);
  end;

{ was #define dname def_expr }
function SDL_MAX_SINT64 : longint; { return type might be wrong }
  begin
    SDL_MAX_SINT64:=SDL_SINT64_C($7FFFFFFFFFFFFFFF);
  end;

{ was #define dname def_expr }
function SDL_MIN_SINT64 : longint; { return type might be wrong }
  begin
    SDL_MIN_SINT64:= not (SDL_SINT64_C($7FFFFFFFFFFFFFFF));
  end;

{ was #define dname def_expr }
function SDL_MAX_UINT64 : longint; { return type might be wrong }
  begin
    SDL_MAX_UINT64:=SDL_UINT64_C($FFFFFFFFFFFFFFFF);
  end;

{ was #define dname def_expr }
function SDL_MIN_UINT64 : longint; { return type might be wrong }
  begin
    SDL_MIN_UINT64:=SDL_UINT64_C($0000000000000000);
  end;

{ was #define dname def_expr }
function SDL_FLT_EPSILON : single;
  begin
    SDL_FLT_EPSILON:=single(1.1920928955078125e-07);
  end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_min(x,y : longint) : longint;
var
   if_local1 : longint;
(* result types are not known *)
begin
  if x<y then
    if_local1:=x
  else
    if_local1:=y;
  SDL_min:=if_local1;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_max(x,y : longint) : longint;
var
   if_local1 : longint;
(* result types are not known *)
begin
  if x>y then
    if_local1:=x
  else
    if_local1:=y;
  SDL_max:=if_local1;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_clamp(x,a,b : longint) : longint;
var
   if_local1, if_local2 : longint;
(* result types are not known *)
begin
  if x>b then
    if_local1:=b
  else
    if_local1:=x;
  if x<a then
    if_local2:=a
  else
    if_local2:=if_local1;
  SDL_clamp:=if_local2;
end;

{ was #define dname def_expr }
function SDL_PI_D : Tdouble;
  begin
    SDL_PI_D:=Tdouble(3.141592653589793238462643383279502884);
  end;

{ was #define dname def_expr }
function SDL_PI_F : single;
  begin
    SDL_PI_F:=single(3.141592653589793238462643383279502884);
  end;

{ was #define dname def_expr }
function SDL_ICONV_ERROR : Tsize_t;
  begin
    SDL_ICONV_ERROR:=Tsize_t(-(1));
  end;

{ was #define dname def_expr }
function SDL_ICONV_E2BIG : Tsize_t;
  begin
    SDL_ICONV_E2BIG:=Tsize_t(-(2));
  end;

{ was #define dname def_expr }
function SDL_ICONV_EILSEQ : Tsize_t;
  begin
    SDL_ICONV_EILSEQ:=Tsize_t(-(3));
  end;

{ was #define dname def_expr }
function SDL_ICONV_EINVAL : Tsize_t;
  begin
    SDL_ICONV_EINVAL:=Tsize_t(-(4));
  end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_iconv_utf8_locale(S : longint) : longint;
begin
  SDL_iconv_utf8_locale:=SDL_iconv_string('','UTF-8',S,(SDL_strlen(S))+1);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function SDL_iconv_utf8_ucs2(S : longint) : PUint16;
begin
  SDL_iconv_utf8_ucs2:=PUint16(SDL_iconv_string('UCS-2','UTF-8',S,(SDL_strlen(S))+1));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function SDL_iconv_utf8_ucs4(S : longint) : PUint32;
begin
  SDL_iconv_utf8_ucs4:=PUint32(SDL_iconv_string('UCS-4','UTF-8',S,(SDL_strlen(S))+1));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_iconv_wchar_utf8(S : longint) : longint;
begin
  SDL_iconv_wchar_utf8:=SDL_iconv_string('UTF-8','WCHAR_T',Pansichar(S),((SDL_wcslen(S))+1)*(sizeof(wchar_t)));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_size_mul_check_overflow(a,b,ret : longint) : longint;
begin
  SDL_size_mul_check_overflow:=SDL_size_mul_check_overflow_builtin(a,b,ret);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_size_add_check_overflow(a,b,ret : longint) : longint;
begin
  SDL_size_add_check_overflow:=SDL_size_add_check_overflow_builtin(a,b,ret);
end;


end.
