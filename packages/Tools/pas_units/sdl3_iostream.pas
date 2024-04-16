
unit SDL3_iostream;

interface

uses
  SDL3;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

type
  PSDL_IOStatus = ^TSDL_IOStatus;
  TSDL_IOStatus =  Longint;
  Const
    SDL_IO_STATUS_READY = 0;
    SDL_IO_STATUS_ERROR = 1;
    SDL_IO_STATUS_EOF = 2;
    SDL_IO_STATUS_NOT_READY = 3;
    SDL_IO_STATUS_READONLY = 4;
    SDL_IO_STATUS_WRITEONLY = 5;

{*
 * The function pointers that drive an SDL_IOStream.
 *
 * Applications can provide this struct to SDL_OpenIO() to create their own
 * implementation of SDL_IOStream. This is not necessarily required, as SDL
 * already offers several common types of I/O streams, via functions like
 * SDL_IOFromFile() and SDL_IOFromMem().
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*
     *  Return the number of bytes in this SDL_IOStream
     *
     *  \return the total size of the data stream, or -1 on error.
      }
{*
     *  Seek to \c offset relative to \c whence, one of stdio's whence values:
     *  SDL_IO_SEEK_SET, SDL_IO_SEEK_CUR, SDL_IO_SEEK_END
     *
     *  \return the final offset in the data stream, or -1 on error.
      }
{*
     *  Read up to \c size bytes from the data stream to the area pointed
     *  at by \c ptr.
     *
     *  On an incomplete read, you should set `*status` to a value from the
     *  SDL_IOStatus enum. You do not have to explicitly set this on
     *  a complete, successful read.
     *
     *  \return the number of bytes read
      }
{*
     *  Write exactly \c size bytes from the area pointed at by \c ptr
     *  to data stream.
     *
     *  On an incomplete write, you should set `*status` to a value from the
     *  SDL_IOStatus enum. You do not have to explicitly set this on
     *  a complete, successful write.
     *
     *  \return the number of bytes written
      }
(* Const before type ignored *)
{*
     *  Close and free any allocated resources.
     *
     *  The SDL_IOStream is still destroyed even if this fails, so clean up anything
     *  even if flushing to disk returns an error.
     *
     *  \return 0 if successful or -1 on write error when flushing data.
      }
type
  PSDL_IOStreamInterface = ^TSDL_IOStreamInterface;
  TSDL_IOStreamInterface = record
      size : function (userdata:pointer):TSint64;cdecl;
      seek : function (userdata:pointer; offset:TSint64; whence:longint):TSint64;cdecl;
      read : function (userdata:pointer; ptr:pointer; size:Tsize_t; status:PSDL_IOStatus):Tsize_t;cdecl;
      write : function (userdata:pointer; ptr:pointer; size:Tsize_t; status:PSDL_IOStatus):Tsize_t;cdecl;
      close : function (userdata:pointer):longint;cdecl;
    end;
{*
 * The read/write operation structure.
 *
 * This operates as an opaque handle. There are several APIs to create various
 * types of I/O streams, or an app can supply an SDL_IOStreamInterface to
 * SDL_OpenIO() to provide their own stream implementation behind this
 * struct's abstract interface.
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*
 *  \name IOFrom functions
 *
 *  Functions to create SDL_IOStream structures from various data streams.
  }
{ @  }
{*
 * Use this function to create a new SDL_IOStream structure for reading from
 * and/or writing to a named file.
 *
 * The `mode` string is treated roughly the same as in a call to the C
 * library's fopen(), even if SDL doesn't happen to use fopen() behind the
 * scenes.
 *
 * Available `mode` strings:
 *
 * - "r": Open a file for reading. The file must exist.
 * - "w": Create an empty file for writing. If a file with the same name
 *   already exists its content is erased and the file is treated as a new
 *   empty file.
 * - "a": Append to a file. Writing operations append data at the end of the
 *   file. The file is created if it does not exist.
 * - "r+": Open a file for update both reading and writing. The file must
 *   exist.
 * - "w+": Create an empty file for both reading and writing. If a file with
 *   the same name already exists its content is erased and the file is
 *   treated as a new empty file.
 * - "a+": Open a file for reading and appending. All writing operations are
 *   performed at the end of the file, protecting the previous content to be
 *   overwritten. You can reposition (fseek, rewind) the internal pointer to
 *   anywhere in the file for reading, but writing operations will move it
 *   back to the end of file. The file is created if it does not exist.
 *
 * **NOTE**: In order to open a file as a binary file, a "b" character has to
 * be included in the `mode` string. This additional "b" character can either
 * be appended at the end of the string (thus making the following compound
 * modes: "rb", "wb", "ab", "r+b", "w+b", "a+b") or be inserted between the
 * letter and the "+" sign for the mixed modes ("rb+", "wb+", "ab+").
 * Additional characters may follow the sequence, although they should have no
 * effect. For example, "t" is sometimes appended to make explicit the file is
 * a text file.
 *
 * This function supports Unicode filenames, but they must be encoded in UTF-8
 * format, regardless of the underlying operating system.
 *
 * As a fallback, SDL_IOFromFile() will transparently open a matching filename
 * in an Android app's `assets`.
 *
 * Closing the SDL_IOStream will close SDL's internal file handle.
 *
 * The following properties may be set at creation time by SDL:
 *
 * - `SDL_PROP_IOSTREAM_WINDOWS_HANDLE_POINTER`: a pointer, that can be cast
 *   to a win32 `HANDLE`, that this SDL_IOStream is using to access the
 *   filesystem. If the program isn't running on Windows, or SDL used some
 *   other method to access the filesystem, this property will not be set.
 * - `SDL_PROP_IOSTREAM_STDIO_FILE_POINTER`: a pointer, that can be cast to a
 *   stdio `FILE *`, that this SDL_IOStream is using to access the filesystem.
 *   If SDL used some other method to access the filesystem, this property
 *   will not be set. PLEASE NOTE that if SDL is using a different C runtime
 *   than your app, trying to use this pointer will almost certainly result in
 *   a crash! This is mostly a problem on Windows; make sure you build SDL and
 *   your app with the same compiler and settings to avoid it.
 * - `SDL_PROP_IOSTREAM_ANDROID_AASSET_POINTER`: a pointer, that can be cast
 *   to an Android NDK `AAsset *`, that this SDL_IOStream is using to access
 *   the filesystem. If SDL used some other method to access the filesystem,
 *   this property will not be set.
 *
 * \param file a UTF-8 string representing the filename to open
 * \param mode an ASCII string representing the mode to be used for opening
 *             the file.
 * \returns a pointer to the SDL_IOStream structure that is created, or NULL
 *          on failure; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CloseIO
 * \sa SDL_ReadIO
 * \sa SDL_SeekIO
 * \sa SDL_TellIO
 * \sa SDL_WriteIO
  }
(* Const before type ignored *)
(* Const before type ignored *)

type TSDL_IOStream=Pointer;
  PSDL_IOStream=^TSDL_IOStream;

function SDL_IOFromFile(file_:Pchar; mode:Pchar):PSDL_IOStream;cdecl;external sdl3_lib;
const
  SDL_PROP_IOSTREAM_WINDOWS_HANDLE_POINTER = 'SDL.iostream.windows.handle';  
  SDL_PROP_IOSTREAM_STDIO_FILE_POINTER = 'SDL.iostream.stdio.file';  
  SDL_PROP_IOSTREAM_ANDROID_AASSET_POINTER = 'SDL.iostream.android.aasset';  
{*
 * Use this function to prepare a read-write memory buffer for use with
 * SDL_IOStream.
 *
 * This function sets up an SDL_IOStream struct based on a memory area of a
 * certain size, for both read and write access.
 *
 * This memory buffer is not copied by the SDL_IOStream; the pointer you
 * provide must remain valid until you close the stream. Closing the stream
 * will not free the original buffer.
 *
 * If you need to make sure the SDL_IOStream never writes to the memory
 * buffer, you should use SDL_IOFromConstMem() with a read-only buffer of
 * memory instead.
 *
 * \param mem a pointer to a buffer to feed an SDL_IOStream stream
 * \param size the buffer size, in bytes
 * \returns a pointer to a new SDL_IOStream structure, or NULL if it fails;
 *          call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_IOFromConstMem
 * \sa SDL_CloseIO
 * \sa SDL_ReadIO
 * \sa SDL_SeekIO
 * \sa SDL_TellIO
 * \sa SDL_WriteIO
  }

function SDL_IOFromMem(mem:pointer; size:Tsize_t):PSDL_IOStream;cdecl;external sdl3_lib;
{*
 * Use this function to prepare a read-only memory buffer for use with
 * SDL_IOStream.
 *
 * This function sets up an SDL_IOStream struct based on a memory area of a
 * certain size. It assumes the memory area is not writable.
 *
 * Attempting to write to this SDL_IOStream stream will report an error
 * without writing to the memory buffer.
 *
 * This memory buffer is not copied by the SDL_IOStream; the pointer you
 * provide must remain valid until you close the stream. Closing the stream
 * will not free the original buffer.
 *
 * If you need to write to a memory buffer, you should use SDL_IOFromMem()
 * with a writable buffer of memory instead.
 *
 * \param mem a pointer to a read-only buffer to feed an SDL_IOStream stream
 * \param size the buffer size, in bytes
 * \returns a pointer to a new SDL_IOStream structure, or NULL if it fails;
 *          call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_IOFromMem
 * \sa SDL_CloseIO
 * \sa SDL_ReadIO
 * \sa SDL_SeekIO
 * \sa SDL_TellIO
  }
(* Const before type ignored *)
function SDL_IOFromConstMem(mem:pointer; size:Tsize_t):PSDL_IOStream;cdecl;external sdl3_lib;
{*
 * Use this function to create an SDL_IOStream that is backed by dynamically
 * allocated memory.
 *
 * This supports the following properties to provide access to the memory and
 * control over allocations: - `SDL_PROP_IOSTREAM_DYNAMIC_MEMORY_POINTER`: a
 * pointer to the internal memory of the stream. This can be set to NULL to
 * transfer ownership of the memory to the application, which should free the
 * memory with SDL_free(). If this is done, the next operation on the stream
 * must be SDL_CloseIO(). - `SDL_PROP_IOSTREAM_DYNAMIC_CHUNKSIZE_NUMBER`:
 * memory will be allocated in multiples of this size, defaulting to 1024.
 *
 * \returns a pointer to a new SDL_IOStream structure, or NULL if it fails;
 *          call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CloseIO
 * \sa SDL_ReadIO
 * \sa SDL_SeekIO
 * \sa SDL_TellIO
 * \sa SDL_WriteIO
  }
function SDL_IOFromDynamicMem:PSDL_IOStream;cdecl;external sdl3_lib;
const
  SDL_PROP_IOSTREAM_DYNAMIC_MEMORY_POINTER = 'SDL.iostream.dynamic.memory';  
  SDL_PROP_IOSTREAM_DYNAMIC_CHUNKSIZE_NUMBER = 'SDL.iostream.dynamic.chunksize';  
{ @  }{ IOFrom functions  }
{*
 * Create a custom SDL_IOStream.
 *
 * Applications do not need to use this function unless they are providing
 * their own SDL_IOStream implementation. If you just need an SDL_IOStream to
 * read/write a common data source, you should use the built-in
 * implementations in SDL, like SDL_IOFromFile() or SDL_IOFromMem(), etc.
 *
 * You must free the returned pointer with SDL_CloseIO().
 *
 * This function makes a copy of `iface` and the caller does not need to keep
 * this data around after this call.
 *
 * \param iface The function pointers that implement this SDL_IOStream.
 * \param userdata The app-controlled pointer that is passed to iface's
 *                 functions when called.
 * \returns a pointer to the allocated memory on success, or NULL on failure;
 *          call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CloseIO
 * \sa SDL_IOFromConstMem
 * \sa SDL_IOFromFile
 * \sa SDL_IOFromMem
  }
(* Const before type ignored *)

function SDL_OpenIO(iface:PSDL_IOStreamInterface; userdata:pointer):PSDL_IOStream;cdecl;external sdl3_lib;
{*
 * Close and free an allocated SDL_IOStream structure.
 *
 * SDL_CloseIO() closes and cleans up the SDL_IOStream stream. It releases any
 * resources used by the stream and frees the SDL_IOStream itself. This
 * returns 0 on success, or -1 if the stream failed to flush to its output
 * (e.g. to disk).
 *
 * Note that if this fails to flush the stream to disk, this function reports
 * an error, but the SDL_IOStream is still invalid once this function returns.
 *
 * \param context SDL_IOStream structure to close
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_OpenIO
  }
function SDL_CloseIO(context:PSDL_IOStream):longint;cdecl;external sdl3_lib;
{*
 * Get the properties associated with an SDL_IOStream.
 *
 * \param context a pointer to an SDL_IOStream structure
 * \returns a valid property ID on success or 0 on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetProperty
 * \sa SDL_SetProperty
  }
function SDL_GetIOProperties(context:PSDL_IOStream):TSDL_PropertiesID;cdecl;external sdl3_lib;
{*< Seek from the beginning of data  }
const
  SDL_IO_SEEK_SET = 0;  
{*< Seek relative to current read point  }
  SDL_IO_SEEK_CUR = 1;  
{*< Seek relative to the end of data  }
  SDL_IO_SEEK_END = 2;  
{*
 * Query the stream status of an SDL_IOStream.
 *
 * This information can be useful to decide if a short read or write was due
 * to an error, an EOF, or a non-blocking operation that isn't yet ready to
 * complete.
 *
 * An SDL_IOStream's status is only expected to change after a SDL_ReadIO or
 * SDL_WriteIO call; don't expect it to change if you just call this query
 * function in a tight loop.
 *
 * \param context the SDL_IOStream to query.
 * \returns an SDL_IOStatus enum with the current state.
 *
 * \threadsafety This function should not be called at the same time that
 *               another thread is operating on the same SDL_IOStream.
 *
 * \since This function is available since SDL 3.0.0.
  }

function SDL_GetIOStatus(context:PSDL_IOStream):TSDL_IOStatus;cdecl;external sdl3_lib;
{*
 * Use this function to get the size of the data stream in an SDL_IOStream.
 *
 * \param context the SDL_IOStream to get the size of the data stream from
 * \returns the size of the data stream in the SDL_IOStream on success or a
 *          negative error code on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetIOSize(context:PSDL_IOStream):TSint64;cdecl;external sdl3_lib;
{*
 * Seek within an SDL_IOStream data stream.
 *
 * This function seeks to byte `offset`, relative to `whence`.
 *
 * `whence` may be any of the following values:
 *
 * - `SDL_IO_SEEK_SET`: seek from the beginning of data
 * - `SDL_IO_SEEK_CUR`: seek relative to current read point
 * - `SDL_IO_SEEK_END`: seek relative to the end of data
 *
 * If this stream can not seek, it will return -1.
 *
 * \param context a pointer to an SDL_IOStream structure
 * \param offset an offset in bytes, relative to **whence** location; can be
 *               negative
 * \param whence any of `SDL_IO_SEEK_SET`, `SDL_IO_SEEK_CUR`,
 *               `SDL_IO_SEEK_END`
 * \returns the final offset in the data stream after the seek or a negative
 *          error code on failure; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_TellIO
  }
function SDL_SeekIO(context:PSDL_IOStream; offset:TSint64; whence:longint):TSint64;cdecl;external sdl3_lib;
{*
 * Determine the current read/write offset in an SDL_IOStream data stream.
 *
 * SDL_TellIO is actually a wrapper function that calls the SDL_IOStream's
 * `seek` method, with an offset of 0 bytes from `SDL_IO_SEEK_CUR`, to
 * simplify application development.
 *
 * \param context an SDL_IOStream data stream object from which to get the
 *                current offset
 * \returns the current offset in the stream, or -1 if the information can not
 *          be determined.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SeekIO
  }
function SDL_TellIO(context:PSDL_IOStream):TSint64;cdecl;external sdl3_lib;
{*
 * Read from a data source.
 *
 * This function reads up `size` bytes from the data source to the area
 * pointed at by `ptr`. This function may read less bytes than requested. It
 * will return zero when the data stream is completely read, or -1 on error.
 * For streams that support non-blocking operation, if nothing was read
 * because it would require blocking, this function returns -2 to distinguish
 * that this is not an error or end-of-file, and the caller can try again
 * later.
 *
 * \param context a pointer to an SDL_IOStream structure
 * \param ptr a pointer to a buffer to read data into
 * \param size the number of bytes to read from the data source.
 * \returns the number of bytes read, or 0 on end of file or other error.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SeekIO
 * \sa SDL_WriteIO
  }
function SDL_ReadIO(context:PSDL_IOStream; ptr:pointer; size:Tsize_t):Tsize_t;cdecl;external sdl3_lib;
{*
 * Write to an SDL_IOStream data stream.
 *
 * This function writes exactly `size` bytes from the area pointed at by `ptr`
 * to the stream. If this fails for any reason, it'll return less than `size`
 * to demonstrate how far the write progressed. On success, it returns `num`.
 *
 * On error, this function still attempts to write as much as possible, so it
 * might return a positive value less than the requested write size. If the
 * function failed to write anything and there was an actual error, it will
 * return -1. For streams that support non-blocking operation, if nothing was
 * written because it would require blocking, this function returns -2 to
 * distinguish that this is not an error and the caller can try again later.
 *
 * It is an error to specify a negative `size`, but this parameter is signed
 * so you definitely cannot overflow the return value on a successful run with
 * enormous amounts of data.
 *
 * \param context a pointer to an SDL_IOStream structure
 * \param ptr a pointer to a buffer containing data to write
 * \param size the number of bytes to write
 * \returns the number of bytes written, which will be less than `num` on
 *          error; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_IOprintf
 * \sa SDL_ReadIO
 * \sa SDL_SeekIO
  }
(* Const before type ignored *)
function SDL_WriteIO(context:PSDL_IOStream; ptr:pointer; size:Tsize_t):Tsize_t;cdecl;external sdl3_lib;
{*
 * Print to an SDL_IOStream data stream.
 *
 * This function does formatted printing to the stream.
 *
 * \param context a pointer to an SDL_IOStream structure
 * \param fmt a printf() style format string
 * \param ... additional parameters matching % tokens in the `fmt` string, if
 *            any
 * \returns the number of bytes written, or 0 on error; call SDL_GetError()
 *          for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_IOvprintf
 * \sa SDL_WriteIO
  }
(* Const before type ignored *)
function SDL_IOprintf(context:PSDL_IOStream; fmt:Pchar):Tsize_t;varargs;cdecl;external sdl3_lib;
//function SDL_IOprintf(context:PSDL_IOStream; fmt:Pchar):Tsize_t;cdecl;external sdl3_lib;
{*
 * Print to an SDL_IOStream data stream.
 *
 * This function does formatted printing to the stream.
 *
 * \param context a pointer to an SDL_IOStream structure
 * \param fmt a printf() style format string
 * \param ap a variable argument list
 * \returns the number of bytes written, or 0 on error; call SDL_GetError()
 *          for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_IOprintf
 * \sa SDL_WriteIO
  }
(* Const before type ignored *)
function SDL_IOvprintf(context:PSDL_IOStream; fmt:Pchar):Tsize_t;varargs; cdecl;external sdl3_lib;
{*
 * Load all the data from an SDL data stream.
 *
 * The data is allocated with a zero byte at the end (null terminated) for
 * convenience. This extra byte is not included in the value reported via
 * `datasize`.
 *
 * The data should be freed with SDL_free().
 *
 * \param src the SDL_IOStream to read all available data from
 * \param datasize if not NULL, will store the number of bytes read
 * \param closeio if SDL_TRUE, calls SDL_CloseIO() on `src` before returning,
 *                even in the case of an error
 * \returns the data, or NULL if there was an error.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_LoadFile
  }
function SDL_LoadFile_IO(src:PSDL_IOStream; datasize:Psize_t; closeio:TSDL_bool):pointer;cdecl;external sdl3_lib;
{*
 * Load all the data from a file path.
 *
 * The data is allocated with a zero byte at the end (null terminated) for
 * convenience. This extra byte is not included in the value reported via
 * `datasize`.
 *
 * The data should be freed with SDL_free().
 *
 * \param file the path to read all available data from
 * \param datasize if not NULL, will store the number of bytes read
 * \returns the data, or NULL if there was an error.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_LoadFile_IO
  }
(* Const before type ignored *)
function SDL_LoadFile(file_:Pchar; datasize:Psize_t):pointer;cdecl;external sdl3_lib;
{*
 *  \name Read endian functions
 *
 *  Read an item of the specified endianness and return in native format.
  }
{ @  }
{*
 * Use this function to read a byte from an SDL_IOStream.
 *
 * \param src the SDL_IOStream to read from
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on success or SDL_FALSE on failure; call SDL_GetError()
 *          for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadU8(src:PSDL_IOStream; value:PUint8):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to read 16 bits of little-endian data from an
 * SDL_IOStream and return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadU16LE(src:PSDL_IOStream; value:PUint16):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to read 16 bits of little-endian data from an
 * SDL_IOStream and return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadS16LE(src:PSDL_IOStream; value:PSint16):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to read 16 bits of big-endian data from an SDL_IOStream
 * and return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadU16BE(src:PSDL_IOStream; value:PUint16):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to read 16 bits of big-endian data from an SDL_IOStream
 * and return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadS16BE(src:PSDL_IOStream; value:PSint16):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to read 32 bits of little-endian data from an
 * SDL_IOStream and return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadU32LE(src:PSDL_IOStream; value:PUint32):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to read 32 bits of little-endian data from an
 * SDL_IOStream and return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadS32LE(src:PSDL_IOStream; value:PSint32):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to read 32 bits of big-endian data from an SDL_IOStream
 * and return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadU32BE(src:PSDL_IOStream; value:PUint32):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to read 32 bits of big-endian data from an SDL_IOStream
 * and return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadS32BE(src:PSDL_IOStream; value:PSint32):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to read 64 bits of little-endian data from an
 * SDL_IOStream and return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadU64LE(src:PSDL_IOStream; value:PUint64):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to read 64 bits of little-endian data from an
 * SDL_IOStream and return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadS64LE(src:PSDL_IOStream; value:PSint64):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to read 64 bits of big-endian data from an SDL_IOStream
 * and return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadU64BE(src:PSDL_IOStream; value:PUint64):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to read 64 bits of big-endian data from an SDL_IOStream
 * and return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadS64BE(src:PSDL_IOStream; value:PSint64):TSDL_bool;cdecl;external sdl3_lib;
{ @  }{ Read endian functions  }
{*
 *  \name Write endian functions
 *
 *  Write an item of native format to the specified endianness.
  }
{ @  }
{*
 * Use this function to write a byte to an SDL_IOStream.
 *
 * \param dst the SDL_IOStream to write to
 * \param value the byte value to write
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteU8(dst:PSDL_IOStream; value:TUint8):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to write 16 bits in native format to an SDL_IOStream as
 * little-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in little-endian
 * format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteU16LE(dst:PSDL_IOStream; value:TUint16):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to write 16 bits in native format to an SDL_IOStream as
 * little-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in little-endian
 * format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteS16LE(dst:PSDL_IOStream; value:TSint16):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to write 16 bits in native format to an SDL_IOStream as
 * big-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in big-endian format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteU16BE(dst:PSDL_IOStream; value:TUint16):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to write 16 bits in native format to an SDL_IOStream as
 * big-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in big-endian format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteS16BE(dst:PSDL_IOStream; value:TSint16):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to write 32 bits in native format to an SDL_IOStream as
 * little-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in little-endian
 * format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteU32LE(dst:PSDL_IOStream; value:TUint32):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to write 32 bits in native format to an SDL_IOStream as
 * little-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in little-endian
 * format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteS32LE(dst:PSDL_IOStream; value:TSint32):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to write 32 bits in native format to an SDL_IOStream as
 * big-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in big-endian format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteU32BE(dst:PSDL_IOStream; value:TUint32):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to write 32 bits in native format to an SDL_IOStream as
 * big-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in big-endian format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteS32BE(dst:PSDL_IOStream; value:TSint32):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to write 64 bits in native format to an SDL_IOStream as
 * little-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in little-endian
 * format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteU64LE(dst:PSDL_IOStream; value:TUint64):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to write 64 bits in native format to an SDL_IOStream as
 * little-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in little-endian
 * format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteS64LE(dst:PSDL_IOStream; value:TSint64):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to write 64 bits in native format to an SDL_IOStream as
 * big-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in big-endian format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteU64BE(dst:PSDL_IOStream; value:TUint64):TSDL_bool;cdecl;external sdl3_lib;
{*
 * Use this function to write 64 bits in native format to an SDL_IOStream as
 * big-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in big-endian format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteS64BE(dst:PSDL_IOStream; value:TSint64):TSDL_bool;cdecl;external sdl3_lib;
{ @  }{ Write endian functions  }
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }


implementation

end.
