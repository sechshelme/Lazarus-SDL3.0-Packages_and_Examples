
unit SDL_storage;
interface

{
  Automatically converted by H2Pas 0.99.16 from SDL_storage.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_storage.h
}

Type
PSDL_PathInfo = ^TSDL_PathInfo;
PSDL_Storage = ^TSDL_Storage;
PUint64 = ^TUint64;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{
  Simple DirectMedia Layer
  Copyright (C) 1997-2024 Sam Lantinga <slouken@libsdl.org>

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
{*
 * # CategoryStorage
 *
 * SDL storage container management.
  }
{$ifndef SDL_storage_h_}
{$define SDL_storage_h_}
{$include <SDL3/SDL_stdinc.h>}
{$include <SDL3/SDL_error.h>}
{$include <SDL3/SDL_filesystem.h>}
{$include <SDL3/SDL_properties.h>}
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{ !!! FIXME: Don't let this ship without async R/W support!!!  }
{*
 * Function interface for SDL_Storage.
 *
 * Apps that want to supply a custom implementation of SDL_Storage will fill
 * in all the functions in this struct, and then pass it to SDL_OpenStorage to
 * create a custom SDL_Storage object.
 *
 * It is not usually necessary to do this; SDL provides standard
 * implementations for many things you might expect to do with an SDL_Storage.
 *
 * This structure should be initialized using SDL_INIT_INTERFACE()
 *
 * \since This struct is available since SDL 3.0.0.
 *
 * \sa SDL_INIT_INTERFACE
  }
{ The version of this interface  }
{ Called when the storage is closed  }
{ Optional, returns whether the storage is currently ready for access  }
{ Enumerate a directory, optional for write-only storage  }
(* Const before declarator ignored *)
{ Get path information, optional for write-only storage  }
(* Const before declarator ignored *)
{ Read a file from storage, optional for write-only storage  }
(* Const before declarator ignored *)
{ Write a file to storage, optional for read-only storage  }
(* Const before declarator ignored *)
(* Const before declarator ignored *)
{ Create a directory, optional for read-only storage  }
(* Const before declarator ignored *)
{ Remove a file or empty directory, optional for read-only storage  }
(* Const before declarator ignored *)
{ Rename a path, optional for read-only storage  }
(* Const before declarator ignored *)
(* Const before declarator ignored *)
{ Copy a file, optional for read-only storage  }
(* Const before declarator ignored *)
(* Const before declarator ignored *)
{ Get the space remaining, optional for read-only storage  }
type
  PSDL_StorageInterface = ^TSDL_StorageInterface;
  TSDL_StorageInterface = record
      version : TUint32;
      close : function (userdata:pointer):Tbool;cdecl;
      ready : function (userdata:pointer):Tbool;cdecl;
      enumerate : function (userdata:pointer; path:Pansichar; callback:TSDL_EnumerateDirectoryCallback; callback_userdata:pointer):Tbool;cdecl;
      info : function (userdata:pointer; path:Pansichar; info:PSDL_PathInfo):Tbool;cdecl;
      read_file : function (userdata:pointer; path:Pansichar; destination:pointer; length:TUint64):Tbool;cdecl;
      write_file : function (userdata:pointer; path:Pansichar; source:pointer; length:TUint64):Tbool;cdecl;
      mkdir : function (userdata:pointer; path:Pansichar):Tbool;cdecl;
      remove : function (userdata:pointer; path:Pansichar):Tbool;cdecl;
      rename : function (userdata:pointer; oldpath:Pansichar; newpath:Pansichar):Tbool;cdecl;
      copy : function (userdata:pointer; oldpath:Pansichar; newpath:Pansichar):Tbool;cdecl;
      space_remaining : function (userdata:pointer):TUint64;cdecl;
    end;
{ Check the size of SDL_StorageInterface
 *
 * If this assert fails, either the compiler is padding to an unexpected size,
 * or the interface has been updated and this should be updated to match and
 * the code using this interface should be updated to handle the old version.
  }
{*
 * An abstract interface for filesystem access.
 *
 * This is an opaque datatype. One can create this object using standard SDL
 * functions like SDL_OpenTitleStorage or SDL_OpenUserStorage, etc, or create
 * an object with a custom implementation using SDL_OpenStorage.
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*
 * Opens up a read-only container for the application's filesystem.
 *
 * \param override a path to override the backend's default title root.
 * \param props a property list that may contain backend-specific information.
 * \returns a title storage container on success or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CloseStorage
 * \sa SDL_GetStorageFileSize
 * \sa SDL_OpenUserStorage
 * \sa SDL_ReadStorageFile
  }
(* Const before declarator ignored *)

function SDL_OpenTitleStorage(override:Pansichar; props:TSDL_PropertiesID):PSDL_Storage;cdecl;external;
{*
 * Opens up a container for a user's unique read/write filesystem.
 *
 * While title storage can generally be kept open throughout runtime, user
 * storage should only be opened when the client is ready to read/write files.
 * This allows the backend to properly batch file operations and flush them
 * when the container has been closed; ensuring safe and optimal save I/O.
 *
 * \param org the name of your organization.
 * \param app the name of your application.
 * \param props a property list that may contain backend-specific information.
 * \returns a user storage container on success or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CloseStorage
 * \sa SDL_GetStorageFileSize
 * \sa SDL_GetStorageSpaceRemaining
 * \sa SDL_OpenTitleStorage
 * \sa SDL_ReadStorageFile
 * \sa SDL_StorageReady
 * \sa SDL_WriteStorageFile
  }
(* Const before declarator ignored *)
(* Const before declarator ignored *)
function SDL_OpenUserStorage(org:Pansichar; app:Pansichar; props:TSDL_PropertiesID):PSDL_Storage;cdecl;external;
{*
 * Opens up a container for local filesystem storage.
 *
 * This is provided for development and tools. Portable applications should
 * use SDL_OpenTitleStorage() for access to game data and
 * SDL_OpenUserStorage() for access to user data.
 *
 * \param path the base path prepended to all storage paths, or NULL for no
 *             base path.
 * \returns a filesystem storage container on success or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CloseStorage
 * \sa SDL_GetStorageFileSize
 * \sa SDL_GetStorageSpaceRemaining
 * \sa SDL_OpenTitleStorage
 * \sa SDL_OpenUserStorage
 * \sa SDL_ReadStorageFile
 * \sa SDL_WriteStorageFile
  }
(* Const before declarator ignored *)
function SDL_OpenFileStorage(path:Pansichar):PSDL_Storage;cdecl;external;
{*
 * Opens up a container using a client-provided storage interface.
 *
 * Applications do not need to use this function unless they are providing
 * their own SDL_Storage implementation. If you just need an SDL_Storage, you
 * should use the built-in implementations in SDL, like SDL_OpenTitleStorage()
 * or SDL_OpenUserStorage().
 *
 * This function makes a copy of `iface` and the caller does not need to keep
 * it around after this call.
 *
 * \param iface the interface that implements this storage, initialized using
 *              SDL_INIT_INTERFACE().
 * \param userdata the pointer that will be passed to the interface functions.
 * \returns a storage container on success or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CloseStorage
 * \sa SDL_GetStorageFileSize
 * \sa SDL_GetStorageSpaceRemaining
 * \sa SDL_INIT_INTERFACE
 * \sa SDL_ReadStorageFile
 * \sa SDL_StorageReady
 * \sa SDL_WriteStorageFile
  }
(* Const before declarator ignored *)
function SDL_OpenStorage(iface:PSDL_StorageInterface; userdata:pointer):PSDL_Storage;cdecl;external;
{*
 * Closes and frees a storage container.
 *
 * \param storage a storage container to close.
 * \returns true if the container was freed with no errors, false otherwise;
 *          call SDL_GetError() for more information. Even if the function
 *          returns an error, the container data will be freed; the error is
 *          only for informational purposes.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_OpenFileStorage
 * \sa SDL_OpenStorage
 * \sa SDL_OpenTitleStorage
 * \sa SDL_OpenUserStorage
  }
function SDL_CloseStorage(storage:PSDL_Storage):Tbool;cdecl;external;
{*
 * Checks if the storage container is ready to use.
 *
 * This function should be called in regular intervals until it returns true -
 * however, it is not recommended to spinwait on this call, as the backend may
 * depend on a synchronous message loop.
 *
 * \param storage a storage container to query.
 * \returns true if the container is ready, false otherwise.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_StorageReady(storage:PSDL_Storage):Tbool;cdecl;external;
{*
 * Query the size of a file within a storage container.
 *
 * \param storage a storage container to query.
 * \param path the relative path of the file to query.
 * \param length a pointer to be filled with the file's length.
 * \returns true if the file could be queried or false on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_ReadStorageFile
 * \sa SDL_StorageReady
  }
(* Const before declarator ignored *)
function SDL_GetStorageFileSize(storage:PSDL_Storage; path:Pansichar; length:PUint64):Tbool;cdecl;external;
{*
 * Synchronously read a file from a storage container into a client-provided
 * buffer.
 *
 * \param storage a storage container to read from.
 * \param path the relative path of the file to read.
 * \param destination a client-provided buffer to read the file into.
 * \param length the length of the destination buffer.
 * \returns true if the file was read or false on failure; call SDL_GetError()
 *          for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetStorageFileSize
 * \sa SDL_StorageReady
 * \sa SDL_WriteStorageFile
  }
(* Const before declarator ignored *)
function SDL_ReadStorageFile(storage:PSDL_Storage; path:Pansichar; destination:pointer; length:TUint64):Tbool;cdecl;external;
{*
 * Synchronously write a file from client memory into a storage container.
 *
 * \param storage a storage container to write to.
 * \param path the relative path of the file to write.
 * \param source a client-provided buffer to write from.
 * \param length the length of the source buffer.
 * \returns true if the file was written or false on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetStorageSpaceRemaining
 * \sa SDL_ReadStorageFile
 * \sa SDL_StorageReady
  }
(* Const before declarator ignored *)
(* Const before declarator ignored *)
function SDL_WriteStorageFile(storage:PSDL_Storage; path:Pansichar; source:pointer; length:TUint64):Tbool;cdecl;external;
{*
 * Create a directory in a writable storage container.
 *
 * \param storage a storage container.
 * \param path the path of the directory to create.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_StorageReady
  }
(* Const before declarator ignored *)
function SDL_CreateStorageDirectory(storage:PSDL_Storage; path:Pansichar):Tbool;cdecl;external;
{*
 * Enumerate a directory in a storage container through a callback function.
 *
 * This function provides every directory entry through an app-provided
 * callback, called once for each directory entry, until all results have been
 * provided or the callback returns <= 0.
 *
 * This will return false if there was a system problem in general, or if a
 * callback returns -1. A successful return means a callback returned 1 to
 * halt enumeration, or all directory entries were enumerated.
 *
 * \param storage a storage container.
 * \param path the path of the directory to enumerate.
 * \param callback a function that is called for each entry in the directory.
 * \param userdata a pointer that is passed to `callback`.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_StorageReady
  }
(* Const before declarator ignored *)
function SDL_EnumerateStorageDirectory(storage:PSDL_Storage; path:Pansichar; callback:TSDL_EnumerateDirectoryCallback; userdata:pointer):Tbool;cdecl;external;
{*
 * Remove a file or an empty directory in a writable storage container.
 *
 * \param storage a storage container.
 * \param path the path of the directory to enumerate.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_StorageReady
  }
(* Const before declarator ignored *)
function SDL_RemoveStoragePath(storage:PSDL_Storage; path:Pansichar):Tbool;cdecl;external;
{*
 * Rename a file or directory in a writable storage container.
 *
 * \param storage a storage container.
 * \param oldpath the old path.
 * \param newpath the new path.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_StorageReady
  }
(* Const before declarator ignored *)
(* Const before declarator ignored *)
function SDL_RenameStoragePath(storage:PSDL_Storage; oldpath:Pansichar; newpath:Pansichar):Tbool;cdecl;external;
{*
 * Copy a file in a writable storage container.
 *
 * \param storage a storage container.
 * \param oldpath the old path.
 * \param newpath the new path.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_StorageReady
  }
(* Const before declarator ignored *)
(* Const before declarator ignored *)
function SDL_CopyStorageFile(storage:PSDL_Storage; oldpath:Pansichar; newpath:Pansichar):Tbool;cdecl;external;
{*
 * Get information about a filesystem path in a storage container.
 *
 * \param storage a storage container.
 * \param path the path to query.
 * \param info a pointer filled in with information about the path, or NULL to
 *             check for the existence of a file.
 * \returns true on success or false if the file doesn't exist, or another
 *          failure; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_StorageReady
  }
(* Const before declarator ignored *)
function SDL_GetStoragePathInfo(storage:PSDL_Storage; path:Pansichar; info:PSDL_PathInfo):Tbool;cdecl;external;
{*
 * Queries the remaining space in a storage container.
 *
 * \param storage a storage container to query.
 * \returns the amount of remaining space, in bytes.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_StorageReady
 * \sa SDL_WriteStorageFile
  }
function SDL_GetStorageSpaceRemaining(storage:PSDL_Storage):TUint64;cdecl;external;
{*
 * Enumerate a directory tree, filtered by pattern, and return a list.
 *
 * Files are filtered out if they don't match the string in `pattern`, which
 * may contain wildcard characters '*' (match everything) and '?' (match one
 * character). If pattern is NULL, no filtering is done and all results are
 * returned. Subdirectories are permitted, and are specified with a path
 * separator of '/'. Wildcard characters '*' and '?' never match a path
 * separator.
 *
 * `flags` may be set to SDL_GLOB_CASEINSENSITIVE to make the pattern matching
 * case-insensitive.
 *
 * The returned array is always NULL-terminated, for your iterating
 * convenience, but if `count` is non-NULL, on return it will contain the
 * number of items in the array, not counting the NULL terminator.
 *
 * \param storage a storage container.
 * \param path the path of the directory to enumerate.
 * \param pattern the pattern that files in the directory must match. Can be
 *                NULL.
 * \param flags `SDL_GLOB_*` bitflags that affect this search.
 * \param count on return, will be set to the number of items in the returned
 *              array. Can be NULL.
 * \returns an array of strings on success or NULL on failure; call
 *          SDL_GetError() for more information. The caller should pass the
 *          returned pointer to SDL_free when done with it. This is a single
 *          allocation that should be freed with SDL_free() when it is no
 *          longer needed.
 *
 * \threadsafety It is safe to call this function from any thread, assuming
 *               the `storage` object is thread-safe.
 *
 * \since This function is available since SDL 3.0.0.
  }
(* Const before declarator ignored *)
(* Const before declarator ignored *)
function SDL_GlobStorageDirectory(storage:PSDL_Storage; path:Pansichar; pattern:Pansichar; flags:TSDL_GlobFlags; count:Plongint):^Pansichar;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_storage_h_  }

implementation


end.
