
unit SDL_properties;
interface

{
  Automatically converted by H2Pas 0.99.16 from SDL_properties.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_properties.h
}


{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{
  Simple DiretMedia Layer
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
 * # CategoryProperties
 *
 * A property is a variable that can be created and retrieved by name at
 * runtime.
 *
 * All properties are part of a property group (SDL_PropertiesID). A property
 * group can be created with the SDL_CreateProperties function and destroyed
 * with the SDL_DestroyProperties function.
 *
 * Properties can be added to and retrieved from a property group through the
 * following functions:
 *
 * - SDL_SetPointerProperty and SDL_GetPointerProperty operate on `void*`
 *   pointer types.
 * - SDL_SetStringProperty and SDL_GetStringProperty operate on string types.
 * - SDL_SetNumberProperty and SDL_GetNumberProperty operate on signed 64-bit
 *   integer types.
 * - SDL_SetFloatProperty and SDL_GetFloatProperty operate on floating point
 *   types.
 * - SDL_SetBooleanProperty and SDL_GetBooleanProperty operate on boolean
 *   types.
 *
 * Properties can be removed from a group by using SDL_ClearProperty.
  }
{$ifndef SDL_properties_h_}
{$define SDL_properties_h_}
{$include <SDL3/SDL_stdinc.h>}
{$include <SDL3/SDL_error.h>}
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{*
 * SDL properties ID
 *
 * \since This datatype is available since SDL 3.0.0.
  }
type
  PSDL_PropertiesID = ^TSDL_PropertiesID;
  TSDL_PropertiesID = TUint32;
{*
 * SDL property type
 *
 * \since This enum is available since SDL 3.0.0.
  }

  PSDL_PropertyType = ^TSDL_PropertyType;
  TSDL_PropertyType =  Longint;
  Const
    SDL_PROPERTY_TYPE_INVALID = 0;
    SDL_PROPERTY_TYPE_POINTER = 1;
    SDL_PROPERTY_TYPE_STRING = 2;
    SDL_PROPERTY_TYPE_NUMBER = 3;
    SDL_PROPERTY_TYPE_FLOAT = 4;
    SDL_PROPERTY_TYPE_BOOLEAN = 5;
;
{*
 * Get the global SDL properties.
 *
 * \returns a valid property ID on success or 0 on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }

function SDL_GetGlobalProperties:TSDL_PropertiesID;cdecl;external;
{*
 * Create a group of properties.
 *
 * All properties are automatically destroyed when SDL_Quit() is called.
 *
 * \returns an ID for a new group of properties, or 0 on failure; call
 *          SDL_GetError() for more information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_DestroyProperties
  }
function SDL_CreateProperties:TSDL_PropertiesID;cdecl;external;
{*
 * Copy a group of properties.
 *
 * Copy all the properties from one group of properties to another, with the
 * exception of properties requiring cleanup (set using
 * SDL_SetPointerPropertyWithCleanup()), which will not be copied. Any
 * property that already exists on `dst` will be overwritten.
 *
 * \param src the properties to copy.
 * \param dst the destination properties.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_CopyProperties(src:TSDL_PropertiesID; dst:TSDL_PropertiesID):Tbool;cdecl;external;
{*
 * Lock a group of properties.
 *
 * Obtain a multi-threaded lock for these properties. Other threads will wait
 * while trying to lock these properties until they are unlocked. Properties
 * must be unlocked before they are destroyed.
 *
 * The lock is automatically taken when setting individual properties, this
 * function is only needed when you want to set several properties atomically
 * or want to guarantee that properties being queried aren't freed in another
 * thread.
 *
 * \param props the properties to lock.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_UnlockProperties
  }
function SDL_LockProperties(props:TSDL_PropertiesID):Tbool;cdecl;external;
{*
 * Unlock a group of properties.
 *
 * \param props the properties to unlock.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_LockProperties
  }
procedure SDL_UnlockProperties(props:TSDL_PropertiesID);cdecl;external;
{*
 * A callback used to free resources when a property is deleted.
 *
 * This should release any resources associated with `value` that are no
 * longer needed.
 *
 * This callback is set per-property. Different properties in the same group
 * can have different cleanup callbacks.
 *
 * This callback will be called _during_ SDL_SetPointerPropertyWithCleanup if
 * the function fails for any reason.
 *
 * \param userdata an app-defined pointer passed to the callback.
 * \param value the pointer assigned to the property to clean up.
 *
 * \threadsafety This callback may fire without any locks held; if this is a
 *               concern, the app should provide its own locking.
 *
 * \since This datatype is available since SDL 3.0.0.
 *
 * \sa SDL_SetPointerPropertyWithCleanup
  }
type

  TSDL_CleanupPropertyCallback = procedure (userdata:pointer; value:pointer);cdecl;
{*
 * Set a pointer property in a group of properties with a cleanup function
 * that is called when the property is deleted.
 *
 * The cleanup function is also called if setting the property fails for any
 * reason.
 *
 * For simply setting basic data types, like numbers, bools, or strings, use
 * SDL_SetNumberProperty, SDL_SetBooleanProperty, or SDL_SetStringProperty
 * instead, as those functions will handle cleanup on your behalf. This
 * function is only for more complex, custom data.
 *
 * \param props the properties to modify.
 * \param name the name of the property to modify.
 * \param value the new value of the property, or NULL to delete the property.
 * \param cleanup the function to call when this property is deleted, or NULL
 *                if no cleanup is necessary.
 * \param userdata a pointer that is passed to the cleanup function.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetPointerProperty
 * \sa SDL_SetPointerProperty
 * \sa SDL_CleanupPropertyCallback
  }
(* Const before declarator ignored *)

function SDL_SetPointerPropertyWithCleanup(props:TSDL_PropertiesID; name:Pansichar; value:pointer; cleanup:TSDL_CleanupPropertyCallback; userdata:pointer):Tbool;cdecl;external;
{*
 * Set a pointer property in a group of properties.
 *
 * \param props the properties to modify.
 * \param name the name of the property to modify.
 * \param value the new value of the property, or NULL to delete the property.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetPointerProperty
 * \sa SDL_HasProperty
 * \sa SDL_SetBooleanProperty
 * \sa SDL_SetFloatProperty
 * \sa SDL_SetNumberProperty
 * \sa SDL_SetPointerPropertyWithCleanup
 * \sa SDL_SetStringProperty
  }
(* Const before declarator ignored *)
function SDL_SetPointerProperty(props:TSDL_PropertiesID; name:Pansichar; value:pointer):Tbool;cdecl;external;
{*
 * Set a string property in a group of properties.
 *
 * This function makes a copy of the string; the caller does not have to
 * preserve the data after this call completes.
 *
 * \param props the properties to modify.
 * \param name the name of the property to modify.
 * \param value the new value of the property, or NULL to delete the property.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetStringProperty
  }
(* Const before declarator ignored *)
(* Const before declarator ignored *)
function SDL_SetStringProperty(props:TSDL_PropertiesID; name:Pansichar; value:Pansichar):Tbool;cdecl;external;
{*
 * Set an integer property in a group of properties.
 *
 * \param props the properties to modify.
 * \param name the name of the property to modify.
 * \param value the new value of the property.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetNumberProperty
  }
(* Const before declarator ignored *)
function SDL_SetNumberProperty(props:TSDL_PropertiesID; name:Pansichar; value:TSint64):Tbool;cdecl;external;
{*
 * Set a floating point property in a group of properties.
 *
 * \param props the properties to modify.
 * \param name the name of the property to modify.
 * \param value the new value of the property.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetFloatProperty
  }
(* Const before declarator ignored *)
function SDL_SetFloatProperty(props:TSDL_PropertiesID; name:Pansichar; value:single):Tbool;cdecl;external;
{*
 * Set a boolean property in a group of properties.
 *
 * \param props the properties to modify.
 * \param name the name of the property to modify.
 * \param value the new value of the property.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetBooleanProperty
  }
(* Const before declarator ignored *)
function SDL_SetBooleanProperty(props:TSDL_PropertiesID; name:Pansichar; value:Tbool):Tbool;cdecl;external;
{*
 * Return whether a property exists in a group of properties.
 *
 * \param props the properties to query.
 * \param name the name of the property to query.
 * \returns true if the property exists, or false if it doesn't.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetPropertyType
  }
(* Const before declarator ignored *)
function SDL_HasProperty(props:TSDL_PropertiesID; name:Pansichar):Tbool;cdecl;external;
{*
 * Get the type of a property in a group of properties.
 *
 * \param props the properties to query.
 * \param name the name of the property to query.
 * \returns the type of the property, or SDL_PROPERTY_TYPE_INVALID if it is
 *          not set.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_HasProperty
  }
(* Const before declarator ignored *)
function SDL_GetPropertyType(props:TSDL_PropertiesID; name:Pansichar):TSDL_PropertyType;cdecl;external;
{*
 * Get a pointer property from a group of properties.
 *
 * By convention, the names of properties that SDL exposes on objects will
 * start with "SDL.", and properties that SDL uses internally will start with
 * "SDL.internal.". These should be considered read-only and should not be
 * modified by applications.
 *
 * \param props the properties to query.
 * \param name the name of the property to query.
 * \param default_value the default value of the property.
 * \returns the value of the property, or `default_value` if it is not set or
 *          not a pointer property.
 *
 * \threadsafety It is safe to call this function from any thread, although
 *               the data returned is not protected and could potentially be
 *               freed if you call SDL_SetPointerProperty() or
 *               SDL_ClearProperty() on these properties from another thread.
 *               If you need to avoid this, use SDL_LockProperties() and
 *               SDL_UnlockProperties().
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetBooleanProperty
 * \sa SDL_GetFloatProperty
 * \sa SDL_GetNumberProperty
 * \sa SDL_GetPropertyType
 * \sa SDL_GetStringProperty
 * \sa SDL_HasProperty
 * \sa SDL_SetPointerProperty
  }
(* Const before declarator ignored *)
function SDL_GetPointerProperty(props:TSDL_PropertiesID; name:Pansichar; default_value:pointer):pointer;cdecl;external;
{*
 * Get a string property from a group of properties.
 *
 * \param props the properties to query.
 * \param name the name of the property to query.
 * \param default_value the default value of the property.
 * \returns the value of the property, or `default_value` if it is not set or
 *          not a string property.
 *
 * \threadsafety It is safe to call this function from any thread, although
 *               the data returned is not protected and could potentially be
 *               freed if you call SDL_SetStringProperty() or
 *               SDL_ClearProperty() on these properties from another thread.
 *               If you need to avoid this, use SDL_LockProperties() and
 *               SDL_UnlockProperties().
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetPropertyType
 * \sa SDL_HasProperty
 * \sa SDL_SetStringProperty
  }
(* Const before declarator ignored *)
(* Const before declarator ignored *)
(* Const before declarator ignored *)
function SDL_GetStringProperty(props:TSDL_PropertiesID; name:Pansichar; default_value:Pansichar):Pansichar;cdecl;external;
{*
 * Get a number property from a group of properties.
 *
 * You can use SDL_GetPropertyType() to query whether the property exists and
 * is a number property.
 *
 * \param props the properties to query.
 * \param name the name of the property to query.
 * \param default_value the default value of the property.
 * \returns the value of the property, or `default_value` if it is not set or
 *          not a number property.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetPropertyType
 * \sa SDL_HasProperty
 * \sa SDL_SetNumberProperty
  }
(* Const before declarator ignored *)
function SDL_GetNumberProperty(props:TSDL_PropertiesID; name:Pansichar; default_value:TSint64):TSint64;cdecl;external;
{*
 * Get a floating point property from a group of properties.
 *
 * You can use SDL_GetPropertyType() to query whether the property exists and
 * is a floating point property.
 *
 * \param props the properties to query.
 * \param name the name of the property to query.
 * \param default_value the default value of the property.
 * \returns the value of the property, or `default_value` if it is not set or
 *          not a float property.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetPropertyType
 * \sa SDL_HasProperty
 * \sa SDL_SetFloatProperty
  }
(* Const before declarator ignored *)
function SDL_GetFloatProperty(props:TSDL_PropertiesID; name:Pansichar; default_value:single):single;cdecl;external;
{*
 * Get a boolean property from a group of properties.
 *
 * You can use SDL_GetPropertyType() to query whether the property exists and
 * is a boolean property.
 *
 * \param props the properties to query.
 * \param name the name of the property to query.
 * \param default_value the default value of the property.
 * \returns the value of the property, or `default_value` if it is not set or
 *          not a boolean property.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetPropertyType
 * \sa SDL_HasProperty
 * \sa SDL_SetBooleanProperty
  }
(* Const before declarator ignored *)
function SDL_GetBooleanProperty(props:TSDL_PropertiesID; name:Pansichar; default_value:Tbool):Tbool;cdecl;external;
{*
 * Clear a property from a group of properties.
 *
 * \param props the properties to modify.
 * \param name the name of the property to clear.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
(* Const before declarator ignored *)
function SDL_ClearProperty(props:TSDL_PropertiesID; name:Pansichar):Tbool;cdecl;external;
{*
 * A callback used to enumerate all the properties in a group of properties.
 *
 * This callback is called from SDL_EnumerateProperties(), and is called once
 * per property in the set.
 *
 * \param userdata an app-defined pointer passed to the callback.
 * \param props the SDL_PropertiesID that is being enumerated.
 * \param name the next property name in the enumeration.
 *
 * \threadsafety SDL_EnumerateProperties holds a lock on `props` during this
 *               callback.
 *
 * \since This datatype is available since SDL 3.0.0.
 *
 * \sa SDL_EnumerateProperties
  }
(* Const before declarator ignored *)
type

  TSDL_EnumeratePropertiesCallback = procedure (userdata:pointer; props:TSDL_PropertiesID; name:Pansichar);cdecl;
{*
 * Enumerate the properties contained in a group of properties.
 *
 * The callback function is called for each property in the group of
 * properties. The properties are locked during enumeration.
 *
 * \param props the properties to query.
 * \param callback the function to call for each property.
 * \param userdata a pointer that is passed to `callback`.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }

function SDL_EnumerateProperties(props:TSDL_PropertiesID; callback:TSDL_EnumeratePropertiesCallback; userdata:pointer):Tbool;cdecl;external;
{*
 * Destroy a group of properties.
 *
 * All properties are deleted and their cleanup functions will be called, if
 * any.
 *
 * \param props the properties to destroy.
 *
 * \threadsafety This function should not be called while these properties are
 *               locked or other threads might be setting or getting values
 *               from these properties.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CreateProperties
  }
procedure SDL_DestroyProperties(props:TSDL_PropertiesID);cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_properties_h_  }

implementation


end.
