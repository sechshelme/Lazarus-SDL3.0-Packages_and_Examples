//
// ===========================  ftcache.h  ===========================
//
/****************************************************************************
 *
 * ftcache.h
 *
 *   FreeType Cache subsystem (specification).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTCACHE_H_
#define FTCACHE_H_


#include <freetype/ftglyph.h>





  /**************************************************************************
   *
   * @section:
   *   cache_subsystem
   *
   * @title:
   *   Cache Sub-System
   *
   * @abstract:
   *   How to cache face, size, and glyph data with FreeType~2.
   *
   * @description:
   *   This section describes the FreeType~2 cache sub-system, which is used
   *   to limit the number of concurrently opened @FT_Face and @FT_Size
   *   objects, as well as caching information like character maps and glyph
   *   images while limiting their maximum memory usage.
   *
   *   Note that all types and functions begin with the `FTC_` prefix rather
   *   than the usual `FT_` prefix in the rest of FreeType.
   *
   *   The cache is highly portable and, thus, doesn't know anything about
   *   the fonts installed on your system, or how to access them.  Therefore,
   *   it requires the following.
   *
   *   * @FTC_FaceID, an arbitrary non-zero value that uniquely identifies
   *     available or installed font faces, has to be provided to the
   *     cache by the client.  Note that the cache only stores and compares
   *     these values and doesn't try to interpret them in any way, but they
   *     have to be persistent on the client side.
   *
   *   * @FTC_Face_Requester, a method to convert an @FTC_FaceID into a new
   *     @FT_Face object when necessary, has to be provided to the cache by
   *     the client.  The @FT_Face object is completely managed by the cache,
   *     including its termination through @FT_Done_Face.  To monitor
   *     termination of face objects, the finalizer callback in the `generic`
   *     field of the @FT_Face object can be used, which might also be used
   *     to store the @FTC_FaceID of the face.
   *
   *   Clients are free to map face IDs to anything useful.  The most simple
   *   usage is, for example, to associate them to a `{pathname,face_index}`
   *   pair that is then used by @FTC_Face_Requester to call @FT_New_Face.
   *   However, more complex schemes are also possible.
   *
   *   Note that for the cache to work correctly, the face ID values must be
   *   **persistent**, which means that the contents they point to should not
   *   change at runtime, or that their value should not become invalid.
   *   If this is unavoidable (e.g., when a font is uninstalled at runtime),
   *   you should call @FTC_Manager_RemoveFaceID as soon as possible to let
   *   the cache get rid of any references to the old @FTC_FaceID it may keep
   *   internally.  Failure to do so will lead to incorrect behaviour or even
   *   crashes in @FTC_Face_Requester.
   *
   *   To use the cache, start with calling @FTC_Manager_New to create a new
   *   @FTC_Manager object, which models a single cache instance.  You can
   *   then look up @FT_Face and @FT_Size objects with
   *   @FTC_Manager_LookupFace and @FTC_Manager_LookupSize, respectively, and
   *   use them in any FreeType work stream.  You can also cache other
   *   FreeType objects as follows.
   *
   *   * If you want to use the charmap caching, call @FTC_CMapCache_New,
   *     then later use @FTC_CMapCache_Lookup to perform the equivalent of
   *     @FT_Get_Char_Index, only much faster.
   *
   *   * If you want to use the @FT_Glyph caching, call @FTC_ImageCache_New,
   *     then later use @FTC_ImageCache_Lookup to retrieve the corresponding
   *     @FT_Glyph objects from the cache.
   *
   *   * If you need lots of small bitmaps, it is much more memory-efficient
   *     to call @FTC_SBitCache_New followed by @FTC_SBitCache_Lookup.  This
   *     returns @FTC_SBitRec structures, which are used to store small
   *     bitmaps directly.  (A small bitmap is one whose metrics and
   *     dimensions all fit into 8-bit integers).
   *
   * @order:
   *   FTC_Manager
   *   FTC_FaceID
   *   FTC_Face_Requester
   *
   *   FTC_Manager_New
   *   FTC_Manager_Reset
   *   FTC_Manager_Done
   *   FTC_Manager_LookupFace
   *   FTC_Manager_LookupSize
   *   FTC_Manager_RemoveFaceID
   *
   *   FTC_Node
   *   FTC_Node_Unref
   *
   *   FTC_ImageCache
   *   FTC_ImageCache_New
   *   FTC_ImageCache_Lookup
   *
   *   FTC_SBit
   *   FTC_SBitCache
   *   FTC_SBitCache_New
   *   FTC_SBitCache_Lookup
   *
   *   FTC_CMapCache
   *   FTC_CMapCache_New
   *   FTC_CMapCache_Lookup
   *
   *************************************************************************/


  /*************************************************************************/
  /*************************************************************************/
  /*************************************************************************/
  /*****                                                               *****/
  /*****                    BASIC TYPE DEFINITIONS                     *****/
  /*****                                                               *****/
  /*************************************************************************/
  /*************************************************************************/
  /*************************************************************************/


  /**************************************************************************
   *
   * @type:
   *   FTC_FaceID
   *
   * @description:
   *   An opaque pointer type that is used to identity face objects.  The
   *   contents of such objects is application-dependent.
   *
   *   These pointers are typically used to point to a user-defined structure
   *   containing a font file path, and face index.
   *
   * @note:
   *   Never use `NULL` as a valid @FTC_FaceID.
   *
   *   Face IDs are passed by the client to the cache manager that calls,
   *   when needed, the @FTC_Face_Requester to translate them into new
   *   @FT_Face objects.
   *
   *   If the content of a given face ID changes at runtime, or if the value
   *   becomes invalid (e.g., when uninstalling a font), you should
   *   immediately call @FTC_Manager_RemoveFaceID before any other cache
   *   function.
   *
   *   Failure to do so will result in incorrect behaviour or even memory
   *   leaks and crashes.
   */
  typedef FT_Pointer  FTC_FaceID;


  /**************************************************************************
   *
   * @functype:
   *   FTC_Face_Requester
   *
   * @description:
   *   A callback function provided by client applications.  It is used by
   *   the cache manager to translate a given @FTC_FaceID into a new valid
   *   @FT_Face object, on demand.
   *
   * @input:
   *   face_id ::
   *     The face ID to resolve.
   *
   *   library ::
   *     A handle to a FreeType library object.
   *
   *   req_data ::
   *     Application-provided request data (see note below).
   *
   * @output:
   *   aface ::
   *     A new @FT_Face handle.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The third parameter `req_data` is the same as the one passed by the
   *   client when @FTC_Manager_New is called.
   *
   *   The face requester should not perform funny things on the returned
   *   face object, like creating a new @FT_Size for it, or setting a
   *   transformation through @FT_Set_Transform!
   */
  typedef FT_Error
  (*FTC_Face_Requester)( FTC_FaceID  face_id,
                         FT_Library  library,
                         FT_Pointer  req_data,
                         FT_Face*    aface );

  /* */


  /*************************************************************************/
  /*************************************************************************/
  /*************************************************************************/
  /*****                                                               *****/
  /*****                      CACHE MANAGER OBJECT                     *****/
  /*****                                                               *****/
  /*************************************************************************/
  /*************************************************************************/
  /*************************************************************************/


  /**************************************************************************
   *
   * @type:
   *   FTC_Manager
   *
   * @description:
   *   This object corresponds to one instance of the cache-subsystem.  It is
   *   used to cache one or more @FT_Face objects, along with corresponding
   *   @FT_Size objects.
   *
   *   The manager intentionally limits the total number of opened @FT_Face
   *   and @FT_Size objects to control memory usage.  See the `max_faces` and
   *   `max_sizes` parameters of @FTC_Manager_New.
   *
   *   The manager is also used to cache 'nodes' of various types while
   *   limiting their total memory usage.
   *
   *   All limitations are enforced by keeping lists of managed objects in
   *   most-recently-used order, and flushing old nodes to make room for new
   *   ones.
   */
  typedef struct FTC_ManagerRec_*  FTC_Manager;


  /**************************************************************************
   *
   * @type:
   *   FTC_Node
   *
   * @description:
   *   An opaque handle to a cache node object.  Each cache node is
   *   reference-counted.  A node with a count of~0 might be flushed out of a
   *   full cache whenever a lookup request is performed.
   *
   *   If you look up nodes, you have the ability to 'acquire' them, i.e., to
   *   increment their reference count.  This will prevent the node from
   *   being flushed out of the cache until you explicitly 'release' it (see
   *   @FTC_Node_Unref).
   *
   *   See also @FTC_SBitCache_Lookup and @FTC_ImageCache_Lookup.
   */
  typedef struct FTC_NodeRec_*  FTC_Node;


  /**************************************************************************
   *
   * @function:
   *   FTC_Manager_New
   *
   * @description:
   *   Create a new cache manager.
   *
   * @input:
   *   library ::
   *     The parent FreeType library handle to use.
   *
   *   max_faces ::
   *     Maximum number of opened @FT_Face objects managed by this cache
   *     instance.  Use~0 for defaults.
   *
   *   max_sizes ::
   *     Maximum number of opened @FT_Size objects managed by this cache
   *     instance.  Use~0 for defaults.
   *
   *   max_bytes ::
   *     Maximum number of bytes to use for cached data nodes.  Use~0 for
   *     defaults.  Note that this value does not account for managed
   *     @FT_Face and @FT_Size objects.
   *
   *   requester ::
   *     An application-provided callback used to translate face IDs into
   *     real @FT_Face objects.
   *
   *   req_data ::
   *     A generic pointer that is passed to the requester each time it is
   *     called (see @FTC_Face_Requester).
   *
   * @output:
   *   amanager ::
   *     A handle to a new manager object.  0~in case of failure.
   *
   * @return:
   *   FreeType error code.  0~means success.
   */
   FT_Error 
  FTC_Manager_New( FT_Library          library,
                   FT_UInt             max_faces,
                   FT_UInt             max_sizes,
                   FT_ULong            max_bytes,
                   FTC_Face_Requester  requester,
                   FT_Pointer          req_data,
                   FTC_Manager        *amanager );


  /**************************************************************************
   *
   * @function:
   *   FTC_Manager_Reset
   *
   * @description:
   *   Empty a given cache manager.  This simply gets rid of all the
   *   currently cached @FT_Face and @FT_Size objects within the manager.
   *
   * @inout:
   *   manager ::
   *     A handle to the manager.
   */
   void 
  FTC_Manager_Reset( FTC_Manager  manager );


  /**************************************************************************
   *
   * @function:
   *   FTC_Manager_Done
   *
   * @description:
   *   Destroy a given manager after emptying it.
   *
   * @input:
   *   manager ::
   *     A handle to the target cache manager object.
   */
   void 
  FTC_Manager_Done( FTC_Manager  manager );


  /**************************************************************************
   *
   * @function:
   *   FTC_Manager_LookupFace
   *
   * @description:
   *   Retrieve the @FT_Face object that corresponds to a given face ID
   *   through a cache manager.
   *
   * @input:
   *   manager ::
   *     A handle to the cache manager.
   *
   *   face_id ::
   *     The ID of the face object.
   *
   * @output:
   *   aface ::
   *     A handle to the face object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The returned @FT_Face object is always owned by the manager.  You
   *   should never try to discard it yourself.
   *
   *   The @FT_Face object doesn't necessarily have a current size object
   *   (i.e., face->size can be~0).  If you need a specific 'font size', use
   *   @FTC_Manager_LookupSize instead.
   *
   *   Never change the face's transformation matrix (i.e., never call the
   *   @FT_Set_Transform function) on a returned face!  If you need to
   *   transform glyphs, do it yourself after glyph loading.
   *
   *   When you perform a lookup, out-of-memory errors are detected _within_
   *   the lookup and force incremental flushes of the cache until enough
   *   memory is released for the lookup to succeed.
   *
   *   If a lookup fails with `FT_Err_Out_Of_Memory` the cache has already
   *   been completely flushed, and still no memory was available for the
   *   operation.
   */
   FT_Error 
  FTC_Manager_LookupFace( FTC_Manager  manager,
                          FTC_FaceID   face_id,
                          FT_Face     *aface );


  /**************************************************************************
   *
   * @struct:
   *   FTC_ScalerRec
   *
   * @description:
   *   A structure used to describe a given character size in either pixels
   *   or points to the cache manager.  See @FTC_Manager_LookupSize.
   *
   * @fields:
   *   face_id ::
   *     The source face ID.
   *
   *   width ::
   *     The character width.
   *
   *   height ::
   *     The character height.
   *
   *   pixel ::
   *     A Boolean.  If 1, the `width` and `height` fields are interpreted as
   *     integer pixel character sizes.  Otherwise, they are expressed as
   *     1/64 of points.
   *
   *   x_res ::
   *     Only used when `pixel` is value~0 to indicate the horizontal
   *     resolution in dpi.
   *
   *   y_res ::
   *     Only used when `pixel` is value~0 to indicate the vertical
   *     resolution in dpi.
   *
   * @note:
   *   This type is mainly used to retrieve @FT_Size objects through the
   *   cache manager.
   */
  typedef struct  FTC_ScalerRec_
  {
    FTC_FaceID  face_id;
    FT_UInt     width;
    FT_UInt     height;
    FT_Int      pixel;
    FT_UInt     x_res;
    FT_UInt     y_res;

  } FTC_ScalerRec;


  /**************************************************************************
   *
   * @struct:
   *   FTC_Scaler
   *
   * @description:
   *   A handle to an @FTC_ScalerRec structure.
   */
  typedef struct FTC_ScalerRec_*  FTC_Scaler;


  /**************************************************************************
   *
   * @function:
   *   FTC_Manager_LookupSize
   *
   * @description:
   *   Retrieve the @FT_Size object that corresponds to a given
   *   @FTC_ScalerRec pointer through a cache manager.
   *
   * @input:
   *   manager ::
   *     A handle to the cache manager.
   *
   *   scaler ::
   *     A scaler handle.
   *
   * @output:
   *   asize ::
   *     A handle to the size object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The returned @FT_Size object is always owned by the manager.  You
   *   should never try to discard it by yourself.
   *
   *   You can access the parent @FT_Face object simply as `size->face` if
   *   you need it.  Note that this object is also owned by the manager.
   *
   * @note:
   *   When you perform a lookup, out-of-memory errors are detected _within_
   *   the lookup and force incremental flushes of the cache until enough
   *   memory is released for the lookup to succeed.
   *
   *   If a lookup fails with `FT_Err_Out_Of_Memory` the cache has already
   *   been completely flushed, and still no memory is available for the
   *   operation.
   */
   FT_Error 
  FTC_Manager_LookupSize( FTC_Manager  manager,
                          FTC_Scaler   scaler,
                          FT_Size     *asize );


  /**************************************************************************
   *
   * @function:
   *   FTC_Node_Unref
   *
   * @description:
   *   Decrement a cache node's internal reference count.  When the count
   *   reaches 0, it is not destroyed but becomes eligible for subsequent
   *   cache flushes.
   *
   * @input:
   *   node ::
   *     The cache node handle.
   *
   *   manager ::
   *     The cache manager handle.
   */
   void 
  FTC_Node_Unref( FTC_Node     node,
                  FTC_Manager  manager );


  /**************************************************************************
   *
   * @function:
   *   FTC_Manager_RemoveFaceID
   *
   * @description:
   *   A special function used to indicate to the cache manager that a given
   *   @FTC_FaceID is no longer valid, either because its content changed, or
   *   because it was deallocated or uninstalled.
   *
   * @input:
   *   manager ::
   *     The cache manager handle.
   *
   *   face_id ::
   *     The @FTC_FaceID to be removed.
   *
   * @note:
   *   This function flushes all nodes from the cache corresponding to this
   *   `face_id`, with the exception of nodes with a non-null reference
   *   count.
   *
   *   Such nodes are however modified internally so as to never appear in
   *   later lookups with the same `face_id` value, and to be immediately
   *   destroyed when released by all their users.
   *
   */
   void 
  FTC_Manager_RemoveFaceID( FTC_Manager  manager,
                            FTC_FaceID   face_id );


  /**************************************************************************
   *
   * @type:
   *   FTC_CMapCache
   *
   * @description:
   *   An opaque handle used to model a charmap cache.  This cache is to hold
   *   character codes -> glyph indices mappings.
   *
   */
  typedef struct FTC_CMapCacheRec_*  FTC_CMapCache;


  /**************************************************************************
   *
   * @function:
   *   FTC_CMapCache_New
   *
   * @description:
   *   Create a new charmap cache.
   *
   * @input:
   *   manager ::
   *     A handle to the cache manager.
   *
   * @output:
   *   acache ::
   *     A new cache handle.  `NULL` in case of error.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   Like all other caches, this one will be destroyed with the cache
   *   manager.
   *
   */
   FT_Error 
  FTC_CMapCache_New( FTC_Manager     manager,
                     FTC_CMapCache  *acache );


  /**************************************************************************
   *
   * @function:
   *   FTC_CMapCache_Lookup
   *
   * @description:
   *   Translate a character code into a glyph index, using the charmap
   *   cache.
   *
   * @input:
   *   cache ::
   *     A charmap cache handle.
   *
   *   face_id ::
   *     The source face ID.
   *
   *   cmap_index ::
   *     The index of the charmap in the source face.  Any negative value
   *     means to use the cache @FT_Face's default charmap.
   *
   *   char_code ::
   *     The character code (in the corresponding charmap).
   *
   * @return:
   *    Glyph index.  0~means 'no glyph'.
   *
   */
   FT_UInt 
  FTC_CMapCache_Lookup( FTC_CMapCache  cache,
                        FTC_FaceID     face_id,
                        FT_Int         cmap_index,
                        FT_UInt32      char_code );


  /*************************************************************************/
  /*************************************************************************/
  /*************************************************************************/
  /*****                                                               *****/
  /*****                       IMAGE CACHE OBJECT                      *****/
  /*****                                                               *****/
  /*************************************************************************/
  /*************************************************************************/
  /*************************************************************************/


  /**************************************************************************
   *
   * @struct:
   *   FTC_ImageTypeRec
   *
   * @description:
   *   A structure used to model the type of images in a glyph cache.
   *
   * @fields:
   *   face_id ::
   *     The face ID.
   *
   *   width ::
   *     The width in pixels.
   *
   *   height ::
   *     The height in pixels.
   *
   *   flags ::
   *     The load flags, as in @FT_Load_Glyph.
   *
   */
  typedef struct  FTC_ImageTypeRec_
  {
    FTC_FaceID  face_id;
    FT_UInt     width;
    FT_UInt     height;
    FT_Int32    flags;

  } FTC_ImageTypeRec;


  /**************************************************************************
   *
   * @type:
   *   FTC_ImageType
   *
   * @description:
   *   A handle to an @FTC_ImageTypeRec structure.
   *
   */
  typedef struct FTC_ImageTypeRec_*  FTC_ImageType;


  /* */


#define FTC_IMAGE_TYPE_COMPARE( d1, d2 )      \
          ( (d1)->face_id == (d2)->face_id && \
            (d1)->width   == (d2)->width   && \
            (d1)->flags   == (d2)->flags   )


  /**************************************************************************
   *
   * @type:
   *   FTC_ImageCache
   *
   * @description:
   *   A handle to a glyph image cache object.  They are designed to hold
   *   many distinct glyph images while not exceeding a certain memory
   *   threshold.
   */
  typedef struct FTC_ImageCacheRec_*  FTC_ImageCache;


  /**************************************************************************
   *
   * @function:
   *   FTC_ImageCache_New
   *
   * @description:
   *   Create a new glyph image cache.
   *
   * @input:
   *   manager ::
   *     The parent manager for the image cache.
   *
   * @output:
   *   acache ::
   *     A handle to the new glyph image cache object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   */
   FT_Error 
  FTC_ImageCache_New( FTC_Manager      manager,
                      FTC_ImageCache  *acache );


  /**************************************************************************
   *
   * @function:
   *   FTC_ImageCache_Lookup
   *
   * @description:
   *   Retrieve a given glyph image from a glyph image cache.
   *
   * @input:
   *   cache ::
   *     A handle to the source glyph image cache.
   *
   *   type ::
   *     A pointer to a glyph image type descriptor.
   *
   *   gindex ::
   *     The glyph index to retrieve.
   *
   * @output:
   *   aglyph ::
   *     The corresponding @FT_Glyph object.  0~in case of failure.
   *
   *   anode ::
   *     Used to return the address of the corresponding cache node after
   *     incrementing its reference count (see note below).
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The returned glyph is owned and managed by the glyph image cache.
   *   Never try to transform or discard it manually!  You can however create
   *   a copy with @FT_Glyph_Copy and modify the new one.
   *
   *   If `anode` is _not_ `NULL`, it receives the address of the cache node
   *   containing the glyph image, after increasing its reference count.
   *   This ensures that the node (as well as the @FT_Glyph) will always be
   *   kept in the cache until you call @FTC_Node_Unref to 'release' it.
   *
   *   If `anode` is `NULL`, the cache node is left unchanged, which means
   *   that the @FT_Glyph could be flushed out of the cache on the next call
   *   to one of the caching sub-system APIs.  Don't assume that it is
   *   persistent!
   */
   FT_Error 
  FTC_ImageCache_Lookup( FTC_ImageCache  cache,
                         FTC_ImageType   type,
                         FT_UInt         gindex,
                         FT_Glyph       *aglyph,
                         FTC_Node       *anode );


  /**************************************************************************
   *
   * @function:
   *   FTC_ImageCache_LookupScaler
   *
   * @description:
   *   A variant of @FTC_ImageCache_Lookup that uses an @FTC_ScalerRec to
   *   specify the face ID and its size.
   *
   * @input:
   *   cache ::
   *     A handle to the source glyph image cache.
   *
   *   scaler ::
   *     A pointer to a scaler descriptor.
   *
   *   load_flags ::
   *     The corresponding load flags.
   *
   *   gindex ::
   *     The glyph index to retrieve.
   *
   * @output:
   *   aglyph ::
   *     The corresponding @FT_Glyph object.  0~in case of failure.
   *
   *   anode ::
   *     Used to return the address of the corresponding cache node after
   *     incrementing its reference count (see note below).
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The returned glyph is owned and managed by the glyph image cache.
   *   Never try to transform or discard it manually!  You can however create
   *   a copy with @FT_Glyph_Copy and modify the new one.
   *
   *   If `anode` is _not_ `NULL`, it receives the address of the cache node
   *   containing the glyph image, after increasing its reference count.
   *   This ensures that the node (as well as the @FT_Glyph) will always be
   *   kept in the cache until you call @FTC_Node_Unref to 'release' it.
   *
   *   If `anode` is `NULL`, the cache node is left unchanged, which means
   *   that the @FT_Glyph could be flushed out of the cache on the next call
   *   to one of the caching sub-system APIs.  Don't assume that it is
   *   persistent!
   *
   *   Calls to @FT_Set_Char_Size and friends have no effect on cached
   *   glyphs; you should always use the FreeType cache API instead.
   */
   FT_Error 
  FTC_ImageCache_LookupScaler( FTC_ImageCache  cache,
                               FTC_Scaler      scaler,
                               FT_ULong        load_flags,
                               FT_UInt         gindex,
                               FT_Glyph       *aglyph,
                               FTC_Node       *anode );


  /**************************************************************************
   *
   * @type:
   *   FTC_SBit
   *
   * @description:
   *   A handle to a small bitmap descriptor.  See the @FTC_SBitRec structure
   *   for details.
   */
  typedef struct FTC_SBitRec_*  FTC_SBit;


  /**************************************************************************
   *
   * @struct:
   *   FTC_SBitRec
   *
   * @description:
   *   A very compact structure used to describe a small glyph bitmap.
   *
   * @fields:
   *   width ::
   *     The bitmap width in pixels.
   *
   *   height ::
   *     The bitmap height in pixels.
   *
   *   left ::
   *     The horizontal distance from the pen position to the left bitmap
   *     border (a.k.a. 'left side bearing', or 'lsb').
   *
   *   top ::
   *     The vertical distance from the pen position (on the baseline) to the
   *     upper bitmap border (a.k.a. 'top side bearing').  The distance is
   *     positive for upwards y~coordinates.
   *
   *   format ::
   *     The format of the glyph bitmap (monochrome or gray).
   *
   *   max_grays ::
   *     Maximum gray level value (in the range 1 to~255).
   *
   *   pitch ::
   *     The number of bytes per bitmap line.  May be positive or negative.
   *
   *   xadvance ::
   *     The horizontal advance width in pixels.
   *
   *   yadvance ::
   *     The vertical advance height in pixels.
   *
   *   buffer ::
   *     A pointer to the bitmap pixels.
   */
  typedef struct  FTC_SBitRec_
  {
    FT_Byte   width;
    FT_Byte   height;
    FT_Char   left;
    FT_Char   top;

    FT_Byte   format;
    FT_Byte   max_grays;
    FT_Short  pitch;
    FT_Char   xadvance;
    FT_Char   yadvance;

    FT_Byte*  buffer;

  } FTC_SBitRec;


  /**************************************************************************
   *
   * @type:
   *   FTC_SBitCache
   *
   * @description:
   *   A handle to a small bitmap cache.  These are special cache objects
   *   used to store small glyph bitmaps (and anti-aliased pixmaps) in a much
   *   more efficient way than the traditional glyph image cache implemented
   *   by @FTC_ImageCache.
   */
  typedef struct FTC_SBitCacheRec_*  FTC_SBitCache;


  /**************************************************************************
   *
   * @function:
   *   FTC_SBitCache_New
   *
   * @description:
   *   Create a new cache to store small glyph bitmaps.
   *
   * @input:
   *   manager ::
   *     A handle to the source cache manager.
   *
   * @output:
   *   acache ::
   *     A handle to the new sbit cache.  `NULL` in case of error.
   *
   * @return:
   *   FreeType error code.  0~means success.
   */
   FT_Error 
  FTC_SBitCache_New( FTC_Manager     manager,
                     FTC_SBitCache  *acache );


  /**************************************************************************
   *
   * @function:
   *   FTC_SBitCache_Lookup
   *
   * @description:
   *   Look up a given small glyph bitmap in a given sbit cache and 'lock' it
   *   to prevent its flushing from the cache until needed.
   *
   * @input:
   *   cache ::
   *     A handle to the source sbit cache.
   *
   *   type ::
   *     A pointer to the glyph image type descriptor.
   *
   *   gindex ::
   *     The glyph index.
   *
   * @output:
   *   sbit ::
   *     A handle to a small bitmap descriptor.
   *
   *   anode ::
   *     Used to return the address of the corresponding cache node after
   *     incrementing its reference count (see note below).
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The small bitmap descriptor and its bit buffer are owned by the cache
   *   and should never be freed by the application.  They might as well
   *   disappear from memory on the next cache lookup, so don't treat them as
   *   persistent data.
   *
   *   The descriptor's `buffer` field is set to~0 to indicate a missing
   *   glyph bitmap.
   *
   *   If `anode` is _not_ `NULL`, it receives the address of the cache node
   *   containing the bitmap, after increasing its reference count.  This
   *   ensures that the node (as well as the image) will always be kept in
   *   the cache until you call @FTC_Node_Unref to 'release' it.
   *
   *   If `anode` is `NULL`, the cache node is left unchanged, which means
   *   that the bitmap could be flushed out of the cache on the next call to
   *   one of the caching sub-system APIs.  Don't assume that it is
   *   persistent!
   */
   FT_Error 
  FTC_SBitCache_Lookup( FTC_SBitCache    cache,
                        FTC_ImageType    type,
                        FT_UInt          gindex,
                        FTC_SBit        *sbit,
                        FTC_Node        *anode );


  /**************************************************************************
   *
   * @function:
   *   FTC_SBitCache_LookupScaler
   *
   * @description:
   *   A variant of @FTC_SBitCache_Lookup that uses an @FTC_ScalerRec to
   *   specify the face ID and its size.
   *
   * @input:
   *   cache ::
   *     A handle to the source sbit cache.
   *
   *   scaler ::
   *     A pointer to the scaler descriptor.
   *
   *   load_flags ::
   *     The corresponding load flags.
   *
   *   gindex ::
   *     The glyph index.
   *
   * @output:
   *   sbit ::
   *     A handle to a small bitmap descriptor.
   *
   *   anode ::
   *     Used to return the address of the corresponding cache node after
   *     incrementing its reference count (see note below).
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The small bitmap descriptor and its bit buffer are owned by the cache
   *   and should never be freed by the application.  They might as well
   *   disappear from memory on the next cache lookup, so don't treat them as
   *   persistent data.
   *
   *   The descriptor's `buffer` field is set to~0 to indicate a missing
   *   glyph bitmap.
   *
   *   If `anode` is _not_ `NULL`, it receives the address of the cache node
   *   containing the bitmap, after increasing its reference count.  This
   *   ensures that the node (as well as the image) will always be kept in
   *   the cache until you call @FTC_Node_Unref to 'release' it.
   *
   *   If `anode` is `NULL`, the cache node is left unchanged, which means
   *   that the bitmap could be flushed out of the cache on the next call to
   *   one of the caching sub-system APIs.  Don't assume that it is
   *   persistent!
   */
   FT_Error 
  FTC_SBitCache_LookupScaler( FTC_SBitCache  cache,
                              FTC_Scaler     scaler,
                              FT_ULong       load_flags,
                              FT_UInt        gindex,
                              FTC_SBit      *sbit,
                              FTC_Node      *anode );

  /* */




#endif /* FTCACHE_H_ */


/* END */
//
// ===========================  ftpfr.h  ===========================
//
/****************************************************************************
 *
 * ftpfr.h
 *
 *   FreeType API for accessing PFR-specific data (specification only).
 *
 * Copyright (C) 2002-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTPFR_H_
#define FTPFR_H_

#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   pfr_fonts
   *
   * @title:
   *   PFR Fonts
   *
   * @abstract:
   *   PFR/TrueDoc-specific API.
   *
   * @description:
   *   This section contains the declaration of PFR-specific functions.
   *
   */


  /**************************************************************************
   *
   * @function:
   *    FT_Get_PFR_Metrics
   *
   * @description:
   *    Return the outline and metrics resolutions of a given PFR face.
   *
   * @input:
   *    face ::
   *      Handle to the input face.  It can be a non-PFR face.
   *
   * @output:
   *    aoutline_resolution ::
   *      Outline resolution.  This is equivalent to `face->units_per_EM` for
   *      non-PFR fonts.  Optional (parameter can be `NULL`).
   *
   *    ametrics_resolution ::
   *      Metrics resolution.  This is equivalent to `outline_resolution` for
   *      non-PFR fonts.  Optional (parameter can be `NULL`).
   *
   *    ametrics_x_scale ::
   *      A 16.16 fixed-point number used to scale distance expressed in
   *      metrics units to device subpixels.  This is equivalent to
   *      `face->size->x_scale`, but for metrics only.  Optional (parameter
   *      can be `NULL`).
   *
   *    ametrics_y_scale ::
   *      Same as `ametrics_x_scale` but for the vertical direction.
   *      optional (parameter can be `NULL`).
   *
   * @return:
   *    FreeType error code.  0~means success.
   *
   * @note:
   *   If the input face is not a PFR, this function will return an error.
   *   However, in all cases, it will return valid values.
   */
   FT_Error 
  FT_Get_PFR_Metrics( FT_Face    face,
                      FT_UInt   *aoutline_resolution,
                      FT_UInt   *ametrics_resolution,
                      FT_Fixed  *ametrics_x_scale,
                      FT_Fixed  *ametrics_y_scale );


  /**************************************************************************
   *
   * @function:
   *    FT_Get_PFR_Kerning
   *
   * @description:
   *    Return the kerning pair corresponding to two glyphs in a PFR face.
   *    The distance is expressed in metrics units, unlike the result of
   *    @FT_Get_Kerning.
   *
   * @input:
   *    face ::
   *      A handle to the input face.
   *
   *    left ::
   *      Index of the left glyph.
   *
   *    right ::
   *      Index of the right glyph.
   *
   * @output:
   *    avector ::
   *      A kerning vector.
   *
   * @return:
   *    FreeType error code.  0~means success.
   *
   * @note:
   *    This function always return distances in original PFR metrics units.
   *    This is unlike @FT_Get_Kerning with the @FT_KERNING_UNSCALED mode,
   *    which always returns distances converted to outline units.
   *
   *    You can use the value of the `x_scale` and `y_scale` parameters
   *    returned by @FT_Get_PFR_Metrics to scale these to device subpixels.
   */
   FT_Error 
  FT_Get_PFR_Kerning( FT_Face     face,
                      FT_UInt     left,
                      FT_UInt     right,
                      FT_Vector  *avector );


  /**************************************************************************
   *
   * @function:
   *    FT_Get_PFR_Advance
   *
   * @description:
   *    Return a given glyph advance, expressed in original metrics units,
   *    from a PFR font.
   *
   * @input:
   *    face ::
   *      A handle to the input face.
   *
   *    gindex ::
   *      The glyph index.
   *
   * @output:
   *    aadvance ::
   *      The glyph advance in metrics units.
   *
   * @return:
   *    FreeType error code.  0~means success.
   *
   * @note:
   *    You can use the `x_scale` or `y_scale` results of @FT_Get_PFR_Metrics
   *    to convert the advance to device subpixels (i.e., 1/64 of pixels).
   */
   FT_Error 
  FT_Get_PFR_Advance( FT_Face   face,
                      FT_UInt   gindex,
                      FT_Pos   *aadvance );

  /* */




#endif /* FTPFR_H_ */


/* END */
//
// ===========================  ftsystem.h  ===========================
//
/****************************************************************************
 *
 * ftsystem.h
 *
 *   FreeType low-level system interface definition (specification).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTSYSTEM_H_
#define FTSYSTEM_H_







  /**************************************************************************
   *
   * @section:
   *  system_interface
   *
   * @title:
   *  System Interface
   *
   * @abstract:
   *  How FreeType manages memory and i/o.
   *
   * @description:
   *  This section contains various definitions related to memory management
   *  and i/o access.  You need to understand this information if you want to
   *  use a custom memory manager or you own i/o streams.
   *
   */


  /**************************************************************************
   *
   *                 M E M O R Y   M A N A G E M E N T
   *
   */


  /**************************************************************************
   *
   * @type:
   *   FT_Memory
   *
   * @description:
   *   A handle to a given memory manager object, defined with an
   *   @FT_MemoryRec structure.
   *
   */
  typedef struct FT_MemoryRec_*  FT_Memory;


  /**************************************************************************
   *
   * @functype:
   *   FT_Alloc_Func
   *
   * @description:
   *   A function used to allocate `size` bytes from `memory`.
   *
   * @input:
   *   memory ::
   *     A handle to the source memory manager.
   *
   *   size ::
   *     The size in bytes to allocate.
   *
   * @return:
   *   Address of new memory block.  0~in case of failure.
   *
   */
  typedef void*
  (*FT_Alloc_Func)( FT_Memory  memory,
                    long       size );


  /**************************************************************************
   *
   * @functype:
   *   FT_Free_Func
   *
   * @description:
   *   A function used to release a given block of memory.
   *
   * @input:
   *   memory ::
   *     A handle to the source memory manager.
   *
   *   block ::
   *     The address of the target memory block.
   *
   */
  typedef void
  (*FT_Free_Func)( FT_Memory  memory,
                   void*      block );


  /**************************************************************************
   *
   * @functype:
   *   FT_Realloc_Func
   *
   * @description:
   *   A function used to re-allocate a given block of memory.
   *
   * @input:
   *   memory ::
   *     A handle to the source memory manager.
   *
   *   cur_size ::
   *     The block's current size in bytes.
   *
   *   new_size ::
   *     The block's requested new size.
   *
   *   block ::
   *     The block's current address.
   *
   * @return:
   *   New block address.  0~in case of memory shortage.
   *
   * @note:
   *   In case of error, the old block must still be available.
   *
   */
  typedef void*
  (*FT_Realloc_Func)( FT_Memory  memory,
                      long       cur_size,
                      long       new_size,
                      void*      block );


  /**************************************************************************
   *
   * @struct:
   *   FT_MemoryRec
   *
   * @description:
   *   A structure used to describe a given memory manager to FreeType~2.
   *
   * @fields:
   *   user ::
   *     A generic typeless pointer for user data.
   *
   *   alloc ::
   *     A pointer type to an allocation function.
   *
   *   free ::
   *     A pointer type to an memory freeing function.
   *
   *   realloc ::
   *     A pointer type to a reallocation function.
   *
   */
  struct  FT_MemoryRec_
  {
    void*            user;
    FT_Alloc_Func    alloc;
    FT_Free_Func     free;
    FT_Realloc_Func  realloc;
  };


  /**************************************************************************
   *
   *                      I / O   M A N A G E M E N T
   *
   */


  /**************************************************************************
   *
   * @type:
   *   FT_Stream
   *
   * @description:
   *   A handle to an input stream.
   *
   * @also:
   *   See @FT_StreamRec for the publicly accessible fields of a given stream
   *   object.
   *
   */
  typedef struct FT_StreamRec_*  FT_Stream;


  /**************************************************************************
   *
   * @struct:
   *   FT_StreamDesc
   *
   * @description:
   *   A union type used to store either a long or a pointer.  This is used
   *   to store a file descriptor or a `FILE*` in an input stream.
   *
   */
  typedef union  FT_StreamDesc_
  {
    long   value;
    void*  pointer;

  } FT_StreamDesc;


  /**************************************************************************
   *
   * @functype:
   *   FT_Stream_IoFunc
   *
   * @description:
   *   A function used to seek and read data from a given input stream.
   *
   * @input:
   *   stream ::
   *     A handle to the source stream.
   *
   *   offset ::
   *     The offset from the start of the stream to seek to.
   *
   *   buffer ::
   *     The address of the read buffer.
   *
   *   count ::
   *     The number of bytes to read from the stream.
   *
   * @return:
   *   If count >~0, return the number of bytes effectively read by the
   *   stream (after seeking to `offset`).  If count ==~0, return the status
   *   of the seek operation (non-zero indicates an error).
   *
   */
  typedef unsigned long
  (*FT_Stream_IoFunc)( FT_Stream       stream,
                       unsigned long   offset,
                       unsigned char*  buffer,
                       unsigned long   count );


  /**************************************************************************
   *
   * @functype:
   *   FT_Stream_CloseFunc
   *
   * @description:
   *   A function used to close a given input stream.
   *
   * @input:
   *  stream ::
   *    A handle to the target stream.
   *
   */
  typedef void
  (*FT_Stream_CloseFunc)( FT_Stream  stream );


  /**************************************************************************
   *
   * @struct:
   *   FT_StreamRec
   *
   * @description:
   *   A structure used to describe an input stream.
   *
   * @input:
   *   base ::
   *     For memory-based streams, this is the address of the first stream
   *     byte in memory.  This field should always be set to `NULL` for
   *     disk-based streams.
   *
   *   size ::
   *     The stream size in bytes.
   *
   *     In case of compressed streams where the size is unknown before
   *     actually doing the decompression, the value is set to 0x7FFFFFFF.
   *     (Note that this size value can occur for normal streams also; it is
   *     thus just a hint.)
   *
   *   pos ::
   *     The current position within the stream.
   *
   *   descriptor ::
   *     This field is a union that can hold an integer or a pointer.  It is
   *     used by stream implementations to store file descriptors or `FILE*`
   *     pointers.
   *
   *   pathname ::
   *     This field is completely ignored by FreeType.  However, it is often
   *     useful during debugging to use it to store the stream's filename
   *     (where available).
   *
   *   read ::
   *     The stream's input function.
   *
   *   close ::
   *     The stream's close function.
   *
   *   memory ::
   *     The memory manager to use to preload frames.  This is set internally
   *     by FreeType and shouldn't be touched by stream implementations.
   *
   *   cursor ::
   *     This field is set and used internally by FreeType when parsing
   *     frames.  In particular, the `FT_GET_XXX` macros use this instead of
   *     the `pos` field.
   *
   *   limit ::
   *     This field is set and used internally by FreeType when parsing
   *     frames.
   *
   */
  typedef struct  FT_StreamRec_
  {
    unsigned char*       base;
    unsigned long        size;
    unsigned long        pos;

    FT_StreamDesc        descriptor;
    FT_StreamDesc        pathname;
    FT_Stream_IoFunc     read;
    FT_Stream_CloseFunc  close;

    FT_Memory            memory;
    unsigned char*       cursor;
    unsigned char*       limit;

  } FT_StreamRec;

  /* */




#endif /* FTSYSTEM_H_ */


/* END */
//
// ===========================  ftsnames.h  ===========================
//
/****************************************************************************
 *
 * ftsnames.h
 *
 *   Simple interface to access SFNT 'name' tables (which are used
 *   to hold font names, copyright info, notices, etc.) (specification).
 *
 *   This is _not_ used to retrieve glyph names!
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTSNAMES_H_
#define FTSNAMES_H_


#include <freetype/freetype.h>
#include <freetype/ftparams.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   sfnt_names
   *
   * @title:
   *   SFNT Names
   *
   * @abstract:
   *   Access the names embedded in TrueType and OpenType files.
   *
   * @description:
   *   The TrueType and OpenType specifications allow the inclusion of a
   *   special names table ('name') in font files.  This table contains
   *   textual (and internationalized) information regarding the font, like
   *   family name, copyright, version, etc.
   *
   *   The definitions below are used to access them if available.
   *
   *   Note that this has nothing to do with glyph names!
   *
   */


  /**************************************************************************
   *
   * @struct:
   *   FT_SfntName
   *
   * @description:
   *   A structure used to model an SFNT 'name' table entry.
   *
   * @fields:
   *   platform_id ::
   *     The platform ID for `string`.  See @TT_PLATFORM_XXX for possible
   *     values.
   *
   *   encoding_id ::
   *     The encoding ID for `string`.  See @TT_APPLE_ID_XXX, @TT_MAC_ID_XXX,
   *     @TT_ISO_ID_XXX, @TT_MS_ID_XXX, and @TT_ADOBE_ID_XXX for possible
   *     values.
   *
   *   language_id ::
   *     The language ID for `string`.  See @TT_MAC_LANGID_XXX and
   *     @TT_MS_LANGID_XXX for possible values.
   *
   *     Registered OpenType values for `language_id` are always smaller than
   *     0x8000; values equal or larger than 0x8000 usually indicate a
   *     language tag string (introduced in OpenType version 1.6).  Use
   *     function @FT_Get_Sfnt_LangTag with `language_id` as its argument to
   *     retrieve the associated language tag.
   *
   *   name_id ::
   *     An identifier for `string`.  See @TT_NAME_ID_XXX for possible
   *     values.
   *
   *   string ::
   *     The 'name' string.  Note that its format differs depending on the
   *     (platform,encoding) pair, being either a string of bytes (without a
   *     terminating `NULL` byte) or containing UTF-16BE entities.
   *
   *   string_len ::
   *     The length of `string` in bytes.
   *
   * @note:
   *   Please refer to the TrueType or OpenType specification for more
   *   details.
   */
  typedef struct  FT_SfntName_
  {
    FT_UShort  platform_id;
    FT_UShort  encoding_id;
    FT_UShort  language_id;
    FT_UShort  name_id;

    FT_Byte*   string;      /* this string is *not* null-terminated! */
    FT_UInt    string_len;  /* in bytes                              */

  } FT_SfntName;


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Sfnt_Name_Count
   *
   * @description:
   *   Retrieve the number of name strings in the SFNT 'name' table.
   *
   * @input:
   *   face ::
   *     A handle to the source face.
   *
   * @return:
   *   The number of strings in the 'name' table.
   *
   * @note:
   *   This function always returns an error if the config macro
   *   `TT_CONFIG_OPTION_SFNT_NAMES` is not defined in `ftoption.h`.
   */
   FT_UInt 
  FT_Get_Sfnt_Name_Count( FT_Face  face );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Sfnt_Name
   *
   * @description:
   *   Retrieve a string of the SFNT 'name' table for a given index.
   *
   * @input:
   *   face ::
   *     A handle to the source face.
   *
   *   idx ::
   *     The index of the 'name' string.
   *
   * @output:
   *   aname ::
   *     The indexed @FT_SfntName structure.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The `string` array returned in the `aname` structure is not
   *   null-terminated.  Note that you don't have to deallocate `string` by
   *   yourself; FreeType takes care of it if you call @FT_Done_Face.
   *
   *   Use @FT_Get_Sfnt_Name_Count to get the total number of available
   *   'name' table entries, then do a loop until you get the right platform,
   *   encoding, and name ID.
   *
   *   'name' table format~1 entries can use language tags also, see
   *   @FT_Get_Sfnt_LangTag.
   *
   *   This function always returns an error if the config macro
   *   `TT_CONFIG_OPTION_SFNT_NAMES` is not defined in `ftoption.h`.
   */
   FT_Error 
  FT_Get_Sfnt_Name( FT_Face       face,
                    FT_UInt       idx,
                    FT_SfntName  *aname );


  /**************************************************************************
   *
   * @struct:
   *   FT_SfntLangTag
   *
   * @description:
   *   A structure to model a language tag entry from an SFNT 'name' table.
   *
   * @fields:
   *   string ::
   *     The language tag string, encoded in UTF-16BE (without trailing
   *     `NULL` bytes).
   *
   *   string_len ::
   *     The length of `string` in **bytes**.
   *
   * @note:
   *   Please refer to the TrueType or OpenType specification for more
   *   details.
   *
   * @since:
   *   2.8
   */
  typedef struct  FT_SfntLangTag_
  {
    FT_Byte*  string;      /* this string is *not* null-terminated! */
    FT_UInt   string_len;  /* in bytes                              */

  } FT_SfntLangTag;


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Sfnt_LangTag
   *
   * @description:
   *   Retrieve the language tag associated with a language ID of an SFNT
   *   'name' table entry.
   *
   * @input:
   *   face ::
   *     A handle to the source face.
   *
   *   langID ::
   *     The language ID, as returned by @FT_Get_Sfnt_Name.  This is always a
   *     value larger than 0x8000.
   *
   * @output:
   *   alangTag ::
   *     The language tag associated with the 'name' table entry's language
   *     ID.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The `string` array returned in the `alangTag` structure is not
   *   null-terminated.  Note that you don't have to deallocate `string` by
   *   yourself; FreeType takes care of it if you call @FT_Done_Face.
   *
   *   Only 'name' table format~1 supports language tags.  For format~0
   *   tables, this function always returns FT_Err_Invalid_Table.  For
   *   invalid format~1 language ID values, FT_Err_Invalid_Argument is
   *   returned.
   *
   *   This function always returns an error if the config macro
   *   `TT_CONFIG_OPTION_SFNT_NAMES` is not defined in `ftoption.h`.
   *
   * @since:
   *   2.8
   */
   FT_Error 
  FT_Get_Sfnt_LangTag( FT_Face          face,
                       FT_UInt          langID,
                       FT_SfntLangTag  *alangTag );


  /* */




#endif /* FTSNAMES_H_ */


/* END */
//
// ===========================  otsvg.h  ===========================
//
/****************************************************************************
 *
 * otsvg.h
 *
 *   Interface for OT-SVG support related things (specification).
 *
 * Copyright (C) 2022-2024 by
 * David Turner, Robert Wilhelm, Werner Lemberg, and Moazin Khatti.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef OTSVG_H_
#define OTSVG_H_

#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   svg_fonts
   *
   * @title:
   *   OpenType SVG Fonts
   *
   * @abstract:
   *   OT-SVG API between FreeType and an external SVG rendering library.
   *
   * @description:
   *   This section describes the four hooks necessary to render SVG
   *   'documents' that are contained in an OpenType font's 'SVG~' table.
   *
   *   For more information on the implementation, see our standard hooks
   *   based on 'librsvg' in the [FreeType Demo
   *   Programs](https://gitlab.freedesktop.org/freetype/freetype-demos)
   *   repository.
   *
   */


  /**************************************************************************
   *
   * @functype:
   *   SVG_Lib_Init_Func
   *
   * @description:
   *   A callback that is called when the first OT-SVG glyph is rendered in
   *   the lifetime of an @FT_Library object.  In a typical implementation,
   *   one would want to allocate a structure and point the `data_pointer`
   *   to it and perform any library initializations that might be needed.
   *
   * @inout:
   *   data_pointer ::
   *     The SVG rendering module stores a pointer variable that can be used
   *     by clients to store any data that needs to be shared across
   *     different hooks.  `data_pointer` is essentially a pointer to that
   *     pointer such that it can be written to as well as read from.
   *
   * @return:
   *   FreeType error code.  0 means success.
   *
   * @since:
   *   2.12
   */
  typedef FT_Error
  (*SVG_Lib_Init_Func)( FT_Pointer  *data_pointer );


  /**************************************************************************
   *
   * @functype:
   *   SVG_Lib_Free_Func
   *
   * @description:
   *   A callback that is called when the `ot-svg` module is being freed.
   *   It is only called if the init hook was called earlier.  This means
   *   that neither the init nor the free hook is called if no OT-SVG glyph
   *   is rendered.
   *
   *   In a typical implementation, one would want to free any state
   *   structure that was allocated in the init hook and perform any
   *   library-related closure that might be needed.
   *
   * @inout:
   *   data_pointer ::
   *     The SVG rendering module stores a pointer variable that can be used
   *     by clients to store any data that needs to be shared across
   *     different hooks.  `data_pointer` is essentially a pointer to that
   *     pointer such that it can be written to as well as read from.
   *
   * @since:
   *   2.12
   */
  typedef void
  (*SVG_Lib_Free_Func)( FT_Pointer  *data_pointer );


  /**************************************************************************
   *
   * @functype:
   *   SVG_Lib_Render_Func
   *
   * @description:
   *   A callback that is called to render an OT-SVG glyph.  This callback
   *   hook is called right after the preset hook @SVG_Lib_Preset_Slot_Func
   *   has been called with `cache` set to `TRUE`.  The data necessary to
   *   render is available through the handle @FT_SVG_Document, which is set
   *   in the `other` field of @FT_GlyphSlotRec.
   *
   *   The render hook is expected to render the SVG glyph to the bitmap
   *   buffer that is allocated already at `slot->bitmap.buffer`.  It also
   *   sets the `num_grays` value as well as `slot->format`.
   *
   * @input:
   *   slot ::
   *     The slot to render.
   *
   * @inout:
   *   data_pointer ::
   *     The SVG rendering module stores a pointer variable that can be used
   *     by clients to store any data that needs to be shared across
   *     different hooks.  `data_pointer` is essentially a pointer to that
   *     pointer such that it can be written to as well as read from.
   *
   * @return:
   *   FreeType error code.  0 means success.
   *
   * @since:
   *   2.12
   */
  typedef FT_Error
  (*SVG_Lib_Render_Func)( FT_GlyphSlot  slot,
                          FT_Pointer   *data_pointer );


  /**************************************************************************
   *
   * @functype:
   *   SVG_Lib_Preset_Slot_Func
   *
   * @description:
   *   A callback that is called to preset the glyph slot.  It is called from
   *   two places.
   *
   *   1. When `FT_Load_Glyph` needs to preset the glyph slot.
   *
   *   2. Right before the `svg` module calls the render callback hook.
   *
   *   When it is the former, the argument `cache` is set to `FALSE`.  When
   *   it is the latter, the argument `cache` is set to `TRUE`.  This
   *   distinction has been made because many calculations that are necessary
   *   for presetting a glyph slot are the same needed later for the render
   *   callback hook.  Thus, if `cache` is `TRUE`, the hook can _cache_ those
   *   calculations in a memory block referenced by the state pointer.
   *
   *   This hook is expected to preset the slot by setting parameters such as
   *   `bitmap_left`, `bitmap_top`, `width`, `rows`, `pitch`, and
   *   `pixel_mode`.  It is also expected to set all the metrics for the slot
   *   including the vertical advance if it is not already set.  Typically,
   *   fonts have horizontal advances but not vertical ones.  If those are
   *   available, they had already been set, otherwise they have to be
   *   estimated and set manually.  The hook must take into account the
   *   transformations that have been set, and translate the transformation
   *   matrices into the SVG coordinate system, as the original matrix is
   *   intended for the TTF/CFF coordinate system.
   *
   * @input:
   *   slot ::
   *     The glyph slot that has the SVG document loaded.
   *
   *   cache ::
   *     See description.
   *
   * @inout:
   *   data_pointer ::
   *     The SVG rendering module stores a pointer variable that can be used
   *     by clients to store any data that needs to be shared across
   *     different hooks.  `data_pointer` is essentially a pointer to that
   *     pointer such that it can be written to as well as read from.
   *
   * @return:
   *   FreeType error code.  0 means success.
   *
   * @since:
   *   2.12
   */
  typedef FT_Error
  (*SVG_Lib_Preset_Slot_Func)( FT_GlyphSlot  slot,
                               FT_Bool       cache,
                               FT_Pointer   *state );


  /**************************************************************************
   *
   * @struct:
   *   SVG_RendererHooks
   *
   * @description:
   *   A structure that stores the four hooks needed to render OT-SVG glyphs
   *   properly.  The structure is publicly used to set the hooks via the
   *   @svg-hooks driver property.
   *
   *   The behavior of each hook is described in its documentation.  One
   *   thing to note is that the preset hook and the render hook often need
   *   to do the same operations; therefore, it's better to cache the
   *   intermediate data in a state structure to avoid calculating it twice.
   *   For example, in the preset hook one can draw the glyph on a recorder
   *   surface and later create a bitmap surface from it in the render hook.
   *
   *   All four hooks must be non-NULL.
   *
   * @fields:
   *   init_svg ::
   *     The initialization hook.
   *
   *   free_svg ::
   *     The cleanup hook.
   *
   *   render_hook ::
   *     The render hook.
   *
   *   preset_slot ::
   *     The preset hook.
   *
   * @since:
   *   2.12
   */
  typedef struct SVG_RendererHooks_
  {
    SVG_Lib_Init_Func    init_svg;
    SVG_Lib_Free_Func    free_svg;
    SVG_Lib_Render_Func  render_svg;

    SVG_Lib_Preset_Slot_Func  preset_slot;

  } SVG_RendererHooks;


  /**************************************************************************
   *
   * @struct:
   *   FT_SVG_DocumentRec
   *
   * @description:
   *   A structure that models one SVG document.
   *
   * @fields:
   *   svg_document ::
   *     A pointer to the SVG document.
   *
   *   svg_document_length ::
   *     The length of `svg_document`.
   *
   *   metrics ::
   *     A metrics object storing the size information.
   *
   *   units_per_EM ::
   *     The size of the EM square.
   *
   *   start_glyph_id ::
   *     The first glyph ID in the glyph range covered by this document.
   *
   *   end_glyph_id ::
   *     The last glyph ID in the glyph range covered by this document.
   *
   *   transform ::
   *     A 2x2 transformation matrix to apply to the glyph while rendering
   *     it.
   *
   *   delta ::
   *     The translation to apply to the glyph while rendering.
   *
   * @note:
   *   When an @FT_GlyphSlot object `slot` is passed down to a renderer, the
   *   renderer can only access the `metrics` and `units_per_EM` fields via
   *   `slot->face`.  However, when @FT_Glyph_To_Bitmap sets up a dummy
   *   object, it has no way to set a `face` object.  Thus, metrics
   *   information and `units_per_EM` (which is necessary for OT-SVG) has to
   *   be stored separately.
   *
   * @since:
   *   2.12
   */
  typedef struct  FT_SVG_DocumentRec_
  {
    FT_Byte*  svg_document;
    FT_ULong  svg_document_length;

    FT_Size_Metrics  metrics;
    FT_UShort        units_per_EM;

    FT_UShort  start_glyph_id;
    FT_UShort  end_glyph_id;

    FT_Matrix  transform;
    FT_Vector  delta;

  } FT_SVG_DocumentRec;


  /**************************************************************************
   *
   * @type:
   *   FT_SVG_Document
   *
   * @description:
   *   A handle to an @FT_SVG_DocumentRec object.
   *
   * @since:
   *   2.12
   */
  typedef struct FT_SVG_DocumentRec_*  FT_SVG_Document;




#endif /* OTSVG_H_ */


/* END */
//
// ===========================  ftparams.h  ===========================
//
/****************************************************************************
 *
 * ftparams.h
 *
 *   FreeType API for possible FT_Parameter tags (specification only).
 *
 * Copyright (C) 2017-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTPARAMS_H_
#define FTPARAMS_H_

#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   parameter_tags
   *
   * @title:
   *   Parameter Tags
   *
   * @abstract:
   *   Macros for driver property and font loading parameter tags.
   *
   * @description:
   *   This section contains macros for the @FT_Parameter structure that are
   *   used with various functions to activate some special functionality or
   *   different behaviour of various components of FreeType.
   *
   */


  /**************************************************************************
   *
   * @enum:
   *   FT_PARAM_TAG_IGNORE_TYPOGRAPHIC_FAMILY
   *
   * @description:
   *   A tag for @FT_Parameter to make @FT_Open_Face ignore typographic
   *   family names in the 'name' table (introduced in OpenType version 1.4).
   *   Use this for backward compatibility with legacy systems that have a
   *   four-faces-per-family restriction.
   *
   * @since:
   *   2.8
   *
   */
#define FT_PARAM_TAG_IGNORE_TYPOGRAPHIC_FAMILY \
          FT_MAKE_TAG( 'i', 'g', 'p', 'f' )


  /* this constant is deprecated */
#define FT_PARAM_TAG_IGNORE_PREFERRED_FAMILY \
          FT_PARAM_TAG_IGNORE_TYPOGRAPHIC_FAMILY


  /**************************************************************************
   *
   * @enum:
   *   FT_PARAM_TAG_IGNORE_TYPOGRAPHIC_SUBFAMILY
   *
   * @description:
   *   A tag for @FT_Parameter to make @FT_Open_Face ignore typographic
   *   subfamily names in the 'name' table (introduced in OpenType version
   *   1.4).  Use this for backward compatibility with legacy systems that
   *   have a four-faces-per-family restriction.
   *
   * @since:
   *   2.8
   *
   */
#define FT_PARAM_TAG_IGNORE_TYPOGRAPHIC_SUBFAMILY \
          FT_MAKE_TAG( 'i', 'g', 'p', 's' )


  /* this constant is deprecated */
#define FT_PARAM_TAG_IGNORE_PREFERRED_SUBFAMILY \
          FT_PARAM_TAG_IGNORE_TYPOGRAPHIC_SUBFAMILY


  /**************************************************************************
   *
   * @enum:
   *   FT_PARAM_TAG_INCREMENTAL
   *
   * @description:
   *   An @FT_Parameter tag to be used with @FT_Open_Face to indicate
   *   incremental glyph loading.
   *
   */
#define FT_PARAM_TAG_INCREMENTAL \
          FT_MAKE_TAG( 'i', 'n', 'c', 'r' )


  /**************************************************************************
   *
   * @enum:
   *   FT_PARAM_TAG_IGNORE_SBIX
   *
   * @description:
   *   A tag for @FT_Parameter to make @FT_Open_Face ignore an 'sbix' table
   *   while loading a font.  Use this if @FT_FACE_FLAG_SBIX is set and you
   *   want to access the outline glyphs in the font.
   *
   */
#define FT_PARAM_TAG_IGNORE_SBIX \
          FT_MAKE_TAG( 'i', 's', 'b', 'x' )


  /**************************************************************************
   *
   * @enum:
   *   FT_PARAM_TAG_LCD_FILTER_WEIGHTS
   *
   * @description:
   *   An @FT_Parameter tag to be used with @FT_Face_Properties.  The
   *   corresponding argument specifies the five LCD filter weights for a
   *   given face (if using @FT_LOAD_TARGET_LCD, for example), overriding the
   *   global default values or the values set up with
   *   @FT_Library_SetLcdFilterWeights.
   *
   * @since:
   *   2.8
   *
   */
#define FT_PARAM_TAG_LCD_FILTER_WEIGHTS \
          FT_MAKE_TAG( 'l', 'c', 'd', 'f' )


  /**************************************************************************
   *
   * @enum:
   *   FT_PARAM_TAG_RANDOM_SEED
   *
   * @description:
   *   An @FT_Parameter tag to be used with @FT_Face_Properties.  The
   *   corresponding 32bit signed integer argument overrides the font
   *   driver's random seed value with a face-specific one; see @random-seed.
   *
   * @since:
   *   2.8
   *
   */
#define FT_PARAM_TAG_RANDOM_SEED \
          FT_MAKE_TAG( 's', 'e', 'e', 'd' )


  /**************************************************************************
   *
   * @enum:
   *   FT_PARAM_TAG_STEM_DARKENING
   *
   * @description:
   *   An @FT_Parameter tag to be used with @FT_Face_Properties.  The
   *   corresponding Boolean argument specifies whether to apply stem
   *   darkening, overriding the global default values or the values set up
   *   with @FT_Property_Set (see @no-stem-darkening).
   *
   *   This is a passive setting that only takes effect if the font driver or
   *   autohinter honors it, which the CFF, Type~1, and CID drivers always
   *   do, but the autohinter only in 'light' hinting mode (as of version
   *   2.9).
   *
   * @since:
   *   2.8
   *
   */
#define FT_PARAM_TAG_STEM_DARKENING \
          FT_MAKE_TAG( 'd', 'a', 'r', 'k' )


  /**************************************************************************
   *
   * @enum:
   *   FT_PARAM_TAG_UNPATENTED_HINTING
   *
   * @description:
   *   Deprecated, no effect.
   *
   *   Previously: A constant used as the tag of an @FT_Parameter structure
   *   to indicate that unpatented methods only should be used by the
   *   TrueType bytecode interpreter for a typeface opened by @FT_Open_Face.
   *
   */
#define FT_PARAM_TAG_UNPATENTED_HINTING \
          FT_MAKE_TAG( 'u', 'n', 'p', 'a' )


  /* */





#endif /* FTPARAMS_H_ */


/* END */
//
// ===========================  ftlcdfil.h  ===========================
//
/****************************************************************************
 *
 * ftlcdfil.h
 *
 *   FreeType API for color filtering of subpixel bitmap glyphs
 *   (specification).
 *
 * Copyright (C) 2006-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTLCDFIL_H_
#define FTLCDFIL_H_

#include <freetype/freetype.h>
#include <freetype/ftparams.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif




  /**************************************************************************
   *
   * @section:
   *   lcd_rendering
   *
   * @title:
   *   Subpixel Rendering
   *
   * @abstract:
   *   API to control subpixel rendering.
   *
   * @description:
   *   FreeType provides two alternative subpixel rendering technologies.
   *   Should you define `FT_CONFIG_OPTION_SUBPIXEL_RENDERING` in your
   *   `ftoption.h` file, this enables ClearType-style rendering.
   *   Otherwise, Harmony LCD rendering is enabled.  These technologies are
   *   controlled differently and API described below, although always
   *   available, performs its function when appropriate method is enabled
   *   and does nothing otherwise.
   *
   *   ClearType-style LCD rendering exploits the color-striped structure of
   *   LCD pixels, increasing the available resolution in the direction of
   *   the stripe (usually horizontal RGB) by a factor of~3.  Using the
   *   subpixel coverages unfiltered can create severe color fringes
   *   especially when rendering thin features.  Indeed, to produce
   *   black-on-white text, the nearby color subpixels must be dimmed
   *   evenly.  Therefore, an equalizing 5-tap FIR filter should be applied
   *   to subpixel coverages regardless of pixel boundaries and should have
   *   these properties:
   *
   *   1. It should be symmetrical, like {~a, b, c, b, a~}, to avoid
   *      any shifts in appearance.
   *
   *   2. It should be color-balanced, meaning a~+ b~=~c, to reduce color
   *      fringes by distributing the computed coverage for one subpixel to
   *      all subpixels equally.
   *
   *   3. It should be normalized, meaning 2a~+ 2b~+ c~=~1.0 to maintain
   *      overall brightness.
   *
   *   Boxy 3-tap filter {0, 1/3, 1/3, 1/3, 0} is sharper but is less
   *   forgiving of non-ideal gamma curves of a screen (and viewing angles),
   *   beveled filters are fuzzier but more tolerant.
   *
   *   Use the @FT_Library_SetLcdFilter or @FT_Library_SetLcdFilterWeights
   *   API to specify a low-pass filter, which is then applied to
   *   subpixel-rendered bitmaps generated through @FT_Render_Glyph.
   *
   *   Harmony LCD rendering is suitable to panels with any regular subpixel
   *   structure, not just monitors with 3 color striped subpixels, as long
   *   as the color subpixels have fixed positions relative to the pixel
   *   center.  In this case, each color channel can be rendered separately
   *   after shifting the outline opposite to the subpixel shift so that the
   *   coverage maps are aligned.  This method is immune to color fringes
   *   because the shifts do not change integral coverage.
   *
   *   The subpixel geometry must be specified by xy-coordinates for each
   *   subpixel. By convention they may come in the RGB order: {{-1/3, 0},
   *   {0, 0}, {1/3, 0}} for standard RGB striped panel or {{-1/6, 1/4},
   *   {-1/6, -1/4}, {1/3, 0}} for a certain PenTile panel.
   *
   *   Use the @FT_Library_SetLcdGeometry API to specify subpixel positions.
   *   If one follows the RGB order convention, the same order applies to the
   *   resulting @FT_PIXEL_MODE_LCD and @FT_PIXEL_MODE_LCD_V bitmaps.  Note,
   *   however, that the coordinate frame for the latter must be rotated
   *   clockwise.  Harmony with default LCD geometry is equivalent to
   *   ClearType with light filter.
   *
   *   As a result of ClearType filtering or Harmony shifts, the resulting
   *   dimensions of LCD bitmaps can be slightly wider or taller than the
   *   dimensions the original outline with regard to the pixel grid.
   *   For example, for @FT_RENDER_MODE_LCD, the filter adds 2~subpixels to
   *   the left, and 2~subpixels to the right.  The bitmap offset values are
   *   adjusted accordingly, so clients shouldn't need to modify their layout
   *   and glyph positioning code when enabling the filter.
   *
   *   The ClearType and Harmony rendering is applicable to glyph bitmaps
   *   rendered through @FT_Render_Glyph, @FT_Load_Glyph, @FT_Load_Char, and
   *   @FT_Glyph_To_Bitmap, when @FT_RENDER_MODE_LCD or @FT_RENDER_MODE_LCD_V
   *   is specified.  This API does not control @FT_Outline_Render and
   *   @FT_Outline_Get_Bitmap.
   *
   *   The described algorithms can completely remove color artefacts when
   *   combined with gamma-corrected alpha blending in linear space.  Each of
   *   the 3~alpha values (subpixels) must by independently used to blend one
   *   color channel.  That is, red alpha blends the red channel of the text
   *   color with the red channel of the background pixel.
   */


  /**************************************************************************
   *
   * @enum:
   *   FT_LcdFilter
   *
   * @description:
   *   A list of values to identify various types of LCD filters.
   *
   * @values:
   *   FT_LCD_FILTER_NONE ::
   *     Do not perform filtering.  When used with subpixel rendering, this
   *     results in sometimes severe color fringes.
   *
   *   FT_LCD_FILTER_DEFAULT ::
   *     This is a beveled, normalized, and color-balanced five-tap filter
   *     with weights of [0x08 0x4D 0x56 0x4D 0x08] in 1/256 units.
   *
   *   FT_LCD_FILTER_LIGHT ::
   *     this is a boxy, normalized, and color-balanced three-tap filter with
   *     weights of [0x00 0x55 0x56 0x55 0x00] in 1/256 units.
   *
   *   FT_LCD_FILTER_LEGACY ::
   *   FT_LCD_FILTER_LEGACY1 ::
   *     This filter corresponds to the original libXft color filter.  It
   *     provides high contrast output but can exhibit really bad color
   *     fringes if glyphs are not extremely well hinted to the pixel grid.
   *     This filter is only provided for comparison purposes, and might be
   *     disabled or stay unsupported in the future. The second value is
   *     provided for compatibility with FontConfig, which historically used
   *     different enumeration, sometimes incorrectly forwarded to FreeType.
   *
   * @since:
   *   2.3.0 (`FT_LCD_FILTER_LEGACY1` since 2.6.2)
   */
  typedef enum  FT_LcdFilter_
  {
    FT_LCD_FILTER_NONE    = 0,
    FT_LCD_FILTER_DEFAULT = 1,
    FT_LCD_FILTER_LIGHT   = 2,
    FT_LCD_FILTER_LEGACY1 = 3,
    FT_LCD_FILTER_LEGACY  = 16,

    FT_LCD_FILTER_MAX   /* do not remove */

  } FT_LcdFilter;


  /**************************************************************************
   *
   * @function:
   *   FT_Library_SetLcdFilter
   *
   * @description:
   *   This function is used to change filter applied to LCD decimated
   *   bitmaps, like the ones used when calling @FT_Render_Glyph with
   *   @FT_RENDER_MODE_LCD or @FT_RENDER_MODE_LCD_V.
   *
   * @input:
   *   library ::
   *     A handle to the target library instance.
   *
   *   filter ::
   *     The filter type.
   *
   *     You can use @FT_LCD_FILTER_NONE here to disable this feature, or
   *     @FT_LCD_FILTER_DEFAULT to use a default filter that should work well
   *     on most LCD screens.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   Since 2.10.3 the LCD filtering is enabled with @FT_LCD_FILTER_DEFAULT.
   *   It is no longer necessary to call this function explicitly except
   *   to choose a different filter or disable filtering altogether with
   *   @FT_LCD_FILTER_NONE.
   *
   *   This function does nothing but returns `FT_Err_Unimplemented_Feature`
   *   if the configuration macro `FT_CONFIG_OPTION_SUBPIXEL_RENDERING` is
   *   not defined in your build of the library.
   *
   * @since:
   *   2.3.0
   */
   FT_Error 
  FT_Library_SetLcdFilter( FT_Library    library,
                           FT_LcdFilter  filter );


  /**************************************************************************
   *
   * @function:
   *   FT_Library_SetLcdFilterWeights
   *
   * @description:
   *   This function can be used to enable LCD filter with custom weights,
   *   instead of using presets in @FT_Library_SetLcdFilter.
   *
   * @input:
   *   library ::
   *     A handle to the target library instance.
   *
   *   weights ::
   *     A pointer to an array; the function copies the first five bytes and
   *     uses them to specify the filter weights in 1/256 units.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function does nothing but returns `FT_Err_Unimplemented_Feature`
   *   if the configuration macro `FT_CONFIG_OPTION_SUBPIXEL_RENDERING` is
   *   not defined in your build of the library.
   *
   *   LCD filter weights can also be set per face using @FT_Face_Properties
   *   with @FT_PARAM_TAG_LCD_FILTER_WEIGHTS.
   *
   * @since:
   *   2.4.0
   */
   FT_Error 
  FT_Library_SetLcdFilterWeights( FT_Library      library,
                                  unsigned char  *weights );


  /**************************************************************************
   *
   * @type:
   *   FT_LcdFiveTapFilter
   *
   * @description:
   *   A typedef for passing the five LCD filter weights to
   *   @FT_Face_Properties within an @FT_Parameter structure.
   *
   * @since:
   *   2.8
   *
   */
#define FT_LCD_FILTER_FIVE_TAPS  5

  typedef FT_Byte  FT_LcdFiveTapFilter[FT_LCD_FILTER_FIVE_TAPS];


  /**************************************************************************
   *
   * @function:
   *   FT_Library_SetLcdGeometry
   *
   * @description:
   *   This function can be used to modify default positions of color
   *   subpixels, which controls Harmony LCD rendering.
   *
   * @input:
   *   library ::
   *     A handle to the target library instance.
   *
   *   sub ::
   *     A pointer to an array of 3 vectors in 26.6 fractional pixel format;
   *     the function modifies the default values, see the note below.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   Subpixel geometry examples:
   *
   *   - {{-21, 0}, {0, 0}, {21, 0}} is the default, corresponding to 3 color
   *   stripes shifted by a third of a pixel. This could be an RGB panel.
   *
   *   - {{21, 0}, {0, 0}, {-21, 0}} looks the same as the default but can
   *   specify a BGR panel instead, while keeping the bitmap in the same
   *   RGB888 format.
   *
   *   - {{0, 21}, {0, 0}, {0, -21}} is the vertical RGB, but the bitmap
   *   stays RGB888 as a result.
   *
   *   - {{-11, 16}, {-11, -16}, {22, 0}} is a certain PenTile arrangement.
   *
   *   This function does nothing and returns `FT_Err_Unimplemented_Feature`
   *   in the context of ClearType-style subpixel rendering when
   *   `FT_CONFIG_OPTION_SUBPIXEL_RENDERING` is defined in your build of the
   *   library.
   *
   * @since:
   *   2.10.0
   */
   FT_Error 
  FT_Library_SetLcdGeometry( FT_Library  library,
                             FT_Vector   sub[3] );

  /* */




#endif /* FTLCDFIL_H_ */


/* END */
//
// ===========================  ttnameid.h  ===========================
//
/****************************************************************************
 *
 * ttnameid.h
 *
 *   TrueType name ID definitions (specification only).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef TTNAMEID_H_
#define TTNAMEID_H_







  /**************************************************************************
   *
   * @section:
   *   truetype_tables
   */


  /**************************************************************************
   *
   * Possible values for the 'platform' identifier code in the name records
   * of an SFNT 'name' table.
   *
   */


  /**************************************************************************
   *
   * @enum:
   *   TT_PLATFORM_XXX
   *
   * @description:
   *   A list of valid values for the `platform_id` identifier code in
   *   @FT_CharMapRec and @FT_SfntName structures.
   *
   * @values:
   *   TT_PLATFORM_APPLE_UNICODE ::
   *     Used by Apple to indicate a Unicode character map and/or name entry.
   *     See @TT_APPLE_ID_XXX for corresponding `encoding_id` values.  Note
   *     that name entries in this format are coded as big-endian UCS-2
   *     character codes _only_.
   *
   *   TT_PLATFORM_MACINTOSH ::
   *     Used by Apple to indicate a MacOS-specific charmap and/or name
   *     entry.  See @TT_MAC_ID_XXX for corresponding `encoding_id` values.
   *     Note that most TrueType fonts contain an Apple roman charmap to be
   *     usable on MacOS systems (even if they contain a Microsoft charmap as
   *     well).
   *
   *   TT_PLATFORM_ISO ::
   *     This value was used to specify ISO/IEC 10646 charmaps.  It is
   *     however now deprecated.  See @TT_ISO_ID_XXX for a list of
   *     corresponding `encoding_id` values.
   *
   *   TT_PLATFORM_MICROSOFT ::
   *     Used by Microsoft to indicate Windows-specific charmaps.  See
   *     @TT_MS_ID_XXX for a list of corresponding `encoding_id` values.
   *     Note that most fonts contain a Unicode charmap using
   *     (`TT_PLATFORM_MICROSOFT`, @TT_MS_ID_UNICODE_CS).
   *
   *   TT_PLATFORM_CUSTOM ::
   *     Used to indicate application-specific charmaps.
   *
   *   TT_PLATFORM_ADOBE ::
   *     This value isn't part of any font format specification, but is used
   *     by FreeType to report Adobe-specific charmaps in an @FT_CharMapRec
   *     structure.  See @TT_ADOBE_ID_XXX.
   */

#define TT_PLATFORM_APPLE_UNICODE  0
#define TT_PLATFORM_MACINTOSH      1
#define TT_PLATFORM_ISO            2 /* deprecated */
#define TT_PLATFORM_MICROSOFT      3
#define TT_PLATFORM_CUSTOM         4
#define TT_PLATFORM_ADOBE          7 /* artificial */


  /**************************************************************************
   *
   * @enum:
   *   TT_APPLE_ID_XXX
   *
   * @description:
   *   A list of valid values for the `encoding_id` for
   *   @TT_PLATFORM_APPLE_UNICODE charmaps and name entries.
   *
   * @values:
   *   TT_APPLE_ID_DEFAULT ::
   *     Unicode version 1.0.
   *
   *   TT_APPLE_ID_UNICODE_1_1 ::
   *     Unicode 1.1; specifies Hangul characters starting at U+34xx.
   *
   *   TT_APPLE_ID_ISO_10646 ::
   *     Deprecated (identical to preceding).
   *
   *   TT_APPLE_ID_UNICODE_2_0 ::
   *     Unicode 2.0 and beyond (UTF-16 BMP only).
   *
   *   TT_APPLE_ID_UNICODE_32 ::
   *     Unicode 3.1 and beyond, using UTF-32.
   *
   *   TT_APPLE_ID_VARIANT_SELECTOR ::
   *     From Adobe, not Apple.  Not a normal cmap.  Specifies variations on
   *     a real cmap.
   *
   *   TT_APPLE_ID_FULL_UNICODE ::
   *     Used for fallback fonts that provide complete Unicode coverage with
   *     a type~13 cmap.
   */

#define TT_APPLE_ID_DEFAULT           0 /* Unicode 1.0                   */
#define TT_APPLE_ID_UNICODE_1_1       1 /* specify Hangul at U+34xx      */
#define TT_APPLE_ID_ISO_10646         2 /* deprecated                    */
#define TT_APPLE_ID_UNICODE_2_0       3 /* or later                      */
#define TT_APPLE_ID_UNICODE_32        4 /* 2.0 or later, full repertoire */
#define TT_APPLE_ID_VARIANT_SELECTOR  5 /* variation selector data       */
#define TT_APPLE_ID_FULL_UNICODE      6 /* used with type 13 cmaps       */


  /**************************************************************************
   *
   * @enum:
   *   TT_MAC_ID_XXX
   *
   * @description:
   *   A list of valid values for the `encoding_id` for
   *   @TT_PLATFORM_MACINTOSH charmaps and name entries.
   */

#define TT_MAC_ID_ROMAN                 0
#define TT_MAC_ID_JAPANESE              1
#define TT_MAC_ID_TRADITIONAL_CHINESE   2
#define TT_MAC_ID_KOREAN                3
#define TT_MAC_ID_ARABIC                4
#define TT_MAC_ID_HEBREW                5
#define TT_MAC_ID_GREEK                 6
#define TT_MAC_ID_RUSSIAN               7
#define TT_MAC_ID_RSYMBOL               8
#define TT_MAC_ID_DEVANAGARI            9
#define TT_MAC_ID_GURMUKHI             10
#define TT_MAC_ID_GUJARATI             11
#define TT_MAC_ID_ORIYA                12
#define TT_MAC_ID_BENGALI              13
#define TT_MAC_ID_TAMIL                14
#define TT_MAC_ID_TELUGU               15
#define TT_MAC_ID_KANNADA              16
#define TT_MAC_ID_MALAYALAM            17
#define TT_MAC_ID_SINHALESE            18
#define TT_MAC_ID_BURMESE              19
#define TT_MAC_ID_KHMER                20
#define TT_MAC_ID_THAI                 21
#define TT_MAC_ID_LAOTIAN              22
#define TT_MAC_ID_GEORGIAN             23
#define TT_MAC_ID_ARMENIAN             24
#define TT_MAC_ID_MALDIVIAN            25
#define TT_MAC_ID_SIMPLIFIED_CHINESE   25
#define TT_MAC_ID_TIBETAN              26
#define TT_MAC_ID_MONGOLIAN            27
#define TT_MAC_ID_GEEZ                 28
#define TT_MAC_ID_SLAVIC               29
#define TT_MAC_ID_VIETNAMESE           30
#define TT_MAC_ID_SINDHI               31
#define TT_MAC_ID_UNINTERP             32


  /**************************************************************************
   *
   * @enum:
   *   TT_ISO_ID_XXX
   *
   * @description:
   *   A list of valid values for the `encoding_id` for @TT_PLATFORM_ISO
   *   charmaps and name entries.
   *
   *   Their use is now deprecated.
   *
   * @values:
   *   TT_ISO_ID_7BIT_ASCII ::
   *     ASCII.
   *   TT_ISO_ID_10646 ::
   *     ISO/10646.
   *   TT_ISO_ID_8859_1 ::
   *     Also known as Latin-1.
   */

#define TT_ISO_ID_7BIT_ASCII  0
#define TT_ISO_ID_10646       1
#define TT_ISO_ID_8859_1      2


  /**************************************************************************
   *
   * @enum:
   *   TT_MS_ID_XXX
   *
   * @description:
   *   A list of valid values for the `encoding_id` for
   *   @TT_PLATFORM_MICROSOFT charmaps and name entries.
   *
   * @values:
   *   TT_MS_ID_SYMBOL_CS ::
   *     Microsoft symbol encoding.  See @FT_ENCODING_MS_SYMBOL.
   *
   *   TT_MS_ID_UNICODE_CS ::
   *     Microsoft WGL4 charmap, matching Unicode.  See @FT_ENCODING_UNICODE.
   *
   *   TT_MS_ID_SJIS ::
   *     Shift JIS Japanese encoding.  See @FT_ENCODING_SJIS.
   *
   *   TT_MS_ID_PRC ::
   *     Chinese encodings as used in the People's Republic of China (PRC).
   *     This means the encodings GB~2312 and its supersets GBK and GB~18030.
   *     See @FT_ENCODING_PRC.
   *
   *   TT_MS_ID_BIG_5 ::
   *     Traditional Chinese as used in Taiwan and Hong Kong.  See
   *     @FT_ENCODING_BIG5.
   *
   *   TT_MS_ID_WANSUNG ::
   *     Korean Extended Wansung encoding.  See @FT_ENCODING_WANSUNG.
   *
   *   TT_MS_ID_JOHAB ::
   *     Korean Johab encoding.  See @FT_ENCODING_JOHAB.
   *
   *   TT_MS_ID_UCS_4 ::
   *     UCS-4 or UTF-32 charmaps.  This has been added to the OpenType
   *     specification version 1.4 (mid-2001).
   */

#define TT_MS_ID_SYMBOL_CS    0
#define TT_MS_ID_UNICODE_CS   1
#define TT_MS_ID_SJIS         2
#define TT_MS_ID_PRC          3
#define TT_MS_ID_BIG_5        4
#define TT_MS_ID_WANSUNG      5
#define TT_MS_ID_JOHAB        6
#define TT_MS_ID_UCS_4       10

  /* this value is deprecated */
#define TT_MS_ID_GB2312  TT_MS_ID_PRC


  /**************************************************************************
   *
   * @enum:
   *   TT_ADOBE_ID_XXX
   *
   * @description:
   *   A list of valid values for the `encoding_id` for @TT_PLATFORM_ADOBE
   *   charmaps.  This is a FreeType-specific extension!
   *
   * @values:
   *   TT_ADOBE_ID_STANDARD ::
   *     Adobe standard encoding.
   *   TT_ADOBE_ID_EXPERT ::
   *     Adobe expert encoding.
   *   TT_ADOBE_ID_CUSTOM ::
   *     Adobe custom encoding.
   *   TT_ADOBE_ID_LATIN_1 ::
   *     Adobe Latin~1 encoding.
   */

#define TT_ADOBE_ID_STANDARD  0
#define TT_ADOBE_ID_EXPERT    1
#define TT_ADOBE_ID_CUSTOM    2
#define TT_ADOBE_ID_LATIN_1   3


  /**************************************************************************
   *
   * @enum:
   *   TT_MAC_LANGID_XXX
   *
   * @description:
   *   Possible values of the language identifier field in the name records
   *   of the SFNT 'name' table if the 'platform' identifier code is
   *   @TT_PLATFORM_MACINTOSH.  These values are also used as return values
   *   for function @FT_Get_CMap_Language_ID.
   *
   *   The canonical source for Apple's IDs is
   *
   *     https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6name.html
   */

#define TT_MAC_LANGID_ENGLISH                       0
#define TT_MAC_LANGID_FRENCH                        1
#define TT_MAC_LANGID_GERMAN                        2
#define TT_MAC_LANGID_ITALIAN                       3
#define TT_MAC_LANGID_DUTCH                         4
#define TT_MAC_LANGID_SWEDISH                       5
#define TT_MAC_LANGID_SPANISH                       6
#define TT_MAC_LANGID_DANISH                        7
#define TT_MAC_LANGID_PORTUGUESE                    8
#define TT_MAC_LANGID_NORWEGIAN                     9
#define TT_MAC_LANGID_HEBREW                       10
#define TT_MAC_LANGID_JAPANESE                     11
#define TT_MAC_LANGID_ARABIC                       12
#define TT_MAC_LANGID_FINNISH                      13
#define TT_MAC_LANGID_GREEK                        14
#define TT_MAC_LANGID_ICELANDIC                    15
#define TT_MAC_LANGID_MALTESE                      16
#define TT_MAC_LANGID_TURKISH                      17
#define TT_MAC_LANGID_CROATIAN                     18
#define TT_MAC_LANGID_CHINESE_TRADITIONAL          19
#define TT_MAC_LANGID_URDU                         20
#define TT_MAC_LANGID_HINDI                        21
#define TT_MAC_LANGID_THAI                         22
#define TT_MAC_LANGID_KOREAN                       23
#define TT_MAC_LANGID_LITHUANIAN                   24
#define TT_MAC_LANGID_POLISH                       25
#define TT_MAC_LANGID_HUNGARIAN                    26
#define TT_MAC_LANGID_ESTONIAN                     27
#define TT_MAC_LANGID_LETTISH                      28
#define TT_MAC_LANGID_SAAMISK                      29
#define TT_MAC_LANGID_FAEROESE                     30
#define TT_MAC_LANGID_FARSI                        31
#define TT_MAC_LANGID_RUSSIAN                      32
#define TT_MAC_LANGID_CHINESE_SIMPLIFIED           33
#define TT_MAC_LANGID_FLEMISH                      34
#define TT_MAC_LANGID_IRISH                        35
#define TT_MAC_LANGID_ALBANIAN                     36
#define TT_MAC_LANGID_ROMANIAN                     37
#define TT_MAC_LANGID_CZECH                        38
#define TT_MAC_LANGID_SLOVAK                       39
#define TT_MAC_LANGID_SLOVENIAN                    40
#define TT_MAC_LANGID_YIDDISH                      41
#define TT_MAC_LANGID_SERBIAN                      42
#define TT_MAC_LANGID_MACEDONIAN                   43
#define TT_MAC_LANGID_BULGARIAN                    44
#define TT_MAC_LANGID_UKRAINIAN                    45
#define TT_MAC_LANGID_BYELORUSSIAN                 46
#define TT_MAC_LANGID_UZBEK                        47
#define TT_MAC_LANGID_KAZAKH                       48
#define TT_MAC_LANGID_AZERBAIJANI                  49
#define TT_MAC_LANGID_AZERBAIJANI_CYRILLIC_SCRIPT  49
#define TT_MAC_LANGID_AZERBAIJANI_ARABIC_SCRIPT    50
#define TT_MAC_LANGID_ARMENIAN                     51
#define TT_MAC_LANGID_GEORGIAN                     52
#define TT_MAC_LANGID_MOLDAVIAN                    53
#define TT_MAC_LANGID_KIRGHIZ                      54
#define TT_MAC_LANGID_TAJIKI                       55
#define TT_MAC_LANGID_TURKMEN                      56
#define TT_MAC_LANGID_MONGOLIAN                    57
#define TT_MAC_LANGID_MONGOLIAN_MONGOLIAN_SCRIPT   57
#define TT_MAC_LANGID_MONGOLIAN_CYRILLIC_SCRIPT    58
#define TT_MAC_LANGID_PASHTO                       59
#define TT_MAC_LANGID_KURDISH                      60
#define TT_MAC_LANGID_KASHMIRI                     61
#define TT_MAC_LANGID_SINDHI                       62
#define TT_MAC_LANGID_TIBETAN                      63
#define TT_MAC_LANGID_NEPALI                       64
#define TT_MAC_LANGID_SANSKRIT                     65
#define TT_MAC_LANGID_MARATHI                      66
#define TT_MAC_LANGID_BENGALI                      67
#define TT_MAC_LANGID_ASSAMESE                     68
#define TT_MAC_LANGID_GUJARATI                     69
#define TT_MAC_LANGID_PUNJABI                      70
#define TT_MAC_LANGID_ORIYA                        71
#define TT_MAC_LANGID_MALAYALAM                    72
#define TT_MAC_LANGID_KANNADA                      73
#define TT_MAC_LANGID_TAMIL                        74
#define TT_MAC_LANGID_TELUGU                       75
#define TT_MAC_LANGID_SINHALESE                    76
#define TT_MAC_LANGID_BURMESE                      77
#define TT_MAC_LANGID_KHMER                        78
#define TT_MAC_LANGID_LAO                          79
#define TT_MAC_LANGID_VIETNAMESE                   80
#define TT_MAC_LANGID_INDONESIAN                   81
#define TT_MAC_LANGID_TAGALOG                      82
#define TT_MAC_LANGID_MALAY_ROMAN_SCRIPT           83
#define TT_MAC_LANGID_MALAY_ARABIC_SCRIPT          84
#define TT_MAC_LANGID_AMHARIC                      85
#define TT_MAC_LANGID_TIGRINYA                     86
#define TT_MAC_LANGID_GALLA                        87
#define TT_MAC_LANGID_SOMALI                       88
#define TT_MAC_LANGID_SWAHILI                      89
#define TT_MAC_LANGID_RUANDA                       90
#define TT_MAC_LANGID_RUNDI                        91
#define TT_MAC_LANGID_CHEWA                        92
#define TT_MAC_LANGID_MALAGASY                     93
#define TT_MAC_LANGID_ESPERANTO                    94
#define TT_MAC_LANGID_WELSH                       128
#define TT_MAC_LANGID_BASQUE                      129
#define TT_MAC_LANGID_CATALAN                     130
#define TT_MAC_LANGID_LATIN                       131
#define TT_MAC_LANGID_QUECHUA                     132
#define TT_MAC_LANGID_GUARANI                     133
#define TT_MAC_LANGID_AYMARA                      134
#define TT_MAC_LANGID_TATAR                       135
#define TT_MAC_LANGID_UIGHUR                      136
#define TT_MAC_LANGID_DZONGKHA                    137
#define TT_MAC_LANGID_JAVANESE                    138
#define TT_MAC_LANGID_SUNDANESE                   139

  /* The following codes are new as of 2000-03-10 */
#define TT_MAC_LANGID_GALICIAN                    140
#define TT_MAC_LANGID_AFRIKAANS                   141
#define TT_MAC_LANGID_BRETON                      142
#define TT_MAC_LANGID_INUKTITUT                   143
#define TT_MAC_LANGID_SCOTTISH_GAELIC             144
#define TT_MAC_LANGID_MANX_GAELIC                 145
#define TT_MAC_LANGID_IRISH_GAELIC                146
#define TT_MAC_LANGID_TONGAN                      147
#define TT_MAC_LANGID_GREEK_POLYTONIC             148
#define TT_MAC_LANGID_GREELANDIC                  149
#define TT_MAC_LANGID_AZERBAIJANI_ROMAN_SCRIPT    150


  /**************************************************************************
   *
   * @enum:
   *   TT_MS_LANGID_XXX
   *
   * @description:
   *   Possible values of the language identifier field in the name records
   *   of the SFNT 'name' table if the 'platform' identifier code is
   *   @TT_PLATFORM_MICROSOFT.  These values are also used as return values
   *   for function @FT_Get_CMap_Language_ID.
   *
   *   The canonical source for Microsoft's IDs is
   *
   *     https://docs.microsoft.com/en-us/windows/desktop/Intl/language-identifier-constants-and-strings ,
   *
   *   however, we only provide macros for language identifiers present in
   *   the OpenType specification: Microsoft has abandoned the concept of
   *   LCIDs (language code identifiers), and format~1 of the 'name' table
   *   provides a better mechanism for languages not covered here.
   *
   *   More legacy values not listed in the reference can be found in the
   *   @FT_TRUETYPE_IDS_H header file.
   */

#define TT_MS_LANGID_ARABIC_SAUDI_ARABIA               0x0401
#define TT_MS_LANGID_ARABIC_IRAQ                       0x0801
#define TT_MS_LANGID_ARABIC_EGYPT                      0x0C01
#define TT_MS_LANGID_ARABIC_LIBYA                      0x1001
#define TT_MS_LANGID_ARABIC_ALGERIA                    0x1401
#define TT_MS_LANGID_ARABIC_MOROCCO                    0x1801
#define TT_MS_LANGID_ARABIC_TUNISIA                    0x1C01
#define TT_MS_LANGID_ARABIC_OMAN                       0x2001
#define TT_MS_LANGID_ARABIC_YEMEN                      0x2401
#define TT_MS_LANGID_ARABIC_SYRIA                      0x2801
#define TT_MS_LANGID_ARABIC_JORDAN                     0x2C01
#define TT_MS_LANGID_ARABIC_LEBANON                    0x3001
#define TT_MS_LANGID_ARABIC_KUWAIT                     0x3401
#define TT_MS_LANGID_ARABIC_UAE                        0x3801
#define TT_MS_LANGID_ARABIC_BAHRAIN                    0x3C01
#define TT_MS_LANGID_ARABIC_QATAR                      0x4001
#define TT_MS_LANGID_BULGARIAN_BULGARIA                0x0402
#define TT_MS_LANGID_CATALAN_CATALAN                   0x0403
#define TT_MS_LANGID_CHINESE_TAIWAN                    0x0404
#define TT_MS_LANGID_CHINESE_PRC                       0x0804
#define TT_MS_LANGID_CHINESE_HONG_KONG                 0x0C04
#define TT_MS_LANGID_CHINESE_SINGAPORE                 0x1004
#define TT_MS_LANGID_CHINESE_MACAO                     0x1404
#define TT_MS_LANGID_CZECH_CZECH_REPUBLIC              0x0405
#define TT_MS_LANGID_DANISH_DENMARK                    0x0406
#define TT_MS_LANGID_GERMAN_GERMANY                    0x0407
#define TT_MS_LANGID_GERMAN_SWITZERLAND                0x0807
#define TT_MS_LANGID_GERMAN_AUSTRIA                    0x0C07
#define TT_MS_LANGID_GERMAN_LUXEMBOURG                 0x1007
#define TT_MS_LANGID_GERMAN_LIECHTENSTEIN              0x1407
#define TT_MS_LANGID_GREEK_GREECE                      0x0408
#define TT_MS_LANGID_ENGLISH_UNITED_STATES             0x0409
#define TT_MS_LANGID_ENGLISH_UNITED_KINGDOM            0x0809
#define TT_MS_LANGID_ENGLISH_AUSTRALIA                 0x0C09
#define TT_MS_LANGID_ENGLISH_CANADA                    0x1009
#define TT_MS_LANGID_ENGLISH_NEW_ZEALAND               0x1409
#define TT_MS_LANGID_ENGLISH_IRELAND                   0x1809
#define TT_MS_LANGID_ENGLISH_SOUTH_AFRICA              0x1C09
#define TT_MS_LANGID_ENGLISH_JAMAICA                   0x2009
#define TT_MS_LANGID_ENGLISH_CARIBBEAN                 0x2409
#define TT_MS_LANGID_ENGLISH_BELIZE                    0x2809
#define TT_MS_LANGID_ENGLISH_TRINIDAD                  0x2C09
#define TT_MS_LANGID_ENGLISH_ZIMBABWE                  0x3009
#define TT_MS_LANGID_ENGLISH_PHILIPPINES               0x3409
#define TT_MS_LANGID_ENGLISH_INDIA                     0x4009
#define TT_MS_LANGID_ENGLISH_MALAYSIA                  0x4409
#define TT_MS_LANGID_ENGLISH_SINGAPORE                 0x4809
#define TT_MS_LANGID_SPANISH_SPAIN_TRADITIONAL_SORT    0x040A
#define TT_MS_LANGID_SPANISH_MEXICO                    0x080A
#define TT_MS_LANGID_SPANISH_SPAIN_MODERN_SORT         0x0C0A
#define TT_MS_LANGID_SPANISH_GUATEMALA                 0x100A
#define TT_MS_LANGID_SPANISH_COSTA_RICA                0x140A
#define TT_MS_LANGID_SPANISH_PANAMA                    0x180A
#define TT_MS_LANGID_SPANISH_DOMINICAN_REPUBLIC        0x1C0A
#define TT_MS_LANGID_SPANISH_VENEZUELA                 0x200A
#define TT_MS_LANGID_SPANISH_COLOMBIA                  0x240A
#define TT_MS_LANGID_SPANISH_PERU                      0x280A
#define TT_MS_LANGID_SPANISH_ARGENTINA                 0x2C0A
#define TT_MS_LANGID_SPANISH_ECUADOR                   0x300A
#define TT_MS_LANGID_SPANISH_CHILE                     0x340A
#define TT_MS_LANGID_SPANISH_URUGUAY                   0x380A
#define TT_MS_LANGID_SPANISH_PARAGUAY                  0x3C0A
#define TT_MS_LANGID_SPANISH_BOLIVIA                   0x400A
#define TT_MS_LANGID_SPANISH_EL_SALVADOR               0x440A
#define TT_MS_LANGID_SPANISH_HONDURAS                  0x480A
#define TT_MS_LANGID_SPANISH_NICARAGUA                 0x4C0A
#define TT_MS_LANGID_SPANISH_PUERTO_RICO               0x500A
#define TT_MS_LANGID_SPANISH_UNITED_STATES             0x540A
#define TT_MS_LANGID_FINNISH_FINLAND                   0x040B
#define TT_MS_LANGID_FRENCH_FRANCE                     0x040C
#define TT_MS_LANGID_FRENCH_BELGIUM                    0x080C
#define TT_MS_LANGID_FRENCH_CANADA                     0x0C0C
#define TT_MS_LANGID_FRENCH_SWITZERLAND                0x100C
#define TT_MS_LANGID_FRENCH_LUXEMBOURG                 0x140C
#define TT_MS_LANGID_FRENCH_MONACO                     0x180C
#define TT_MS_LANGID_HEBREW_ISRAEL                     0x040D
#define TT_MS_LANGID_HUNGARIAN_HUNGARY                 0x040E
#define TT_MS_LANGID_ICELANDIC_ICELAND                 0x040F
#define TT_MS_LANGID_ITALIAN_ITALY                     0x0410
#define TT_MS_LANGID_ITALIAN_SWITZERLAND               0x0810
#define TT_MS_LANGID_JAPANESE_JAPAN                    0x0411
#define TT_MS_LANGID_KOREAN_KOREA                      0x0412
#define TT_MS_LANGID_DUTCH_NETHERLANDS                 0x0413
#define TT_MS_LANGID_DUTCH_BELGIUM                     0x0813
#define TT_MS_LANGID_NORWEGIAN_NORWAY_BOKMAL           0x0414
#define TT_MS_LANGID_NORWEGIAN_NORWAY_NYNORSK          0x0814
#define TT_MS_LANGID_POLISH_POLAND                     0x0415
#define TT_MS_LANGID_PORTUGUESE_BRAZIL                 0x0416
#define TT_MS_LANGID_PORTUGUESE_PORTUGAL               0x0816
#define TT_MS_LANGID_ROMANSH_SWITZERLAND               0x0417
#define TT_MS_LANGID_ROMANIAN_ROMANIA                  0x0418
#define TT_MS_LANGID_RUSSIAN_RUSSIA                    0x0419
#define TT_MS_LANGID_CROATIAN_CROATIA                  0x041A
#define TT_MS_LANGID_SERBIAN_SERBIA_LATIN              0x081A
#define TT_MS_LANGID_SERBIAN_SERBIA_CYRILLIC           0x0C1A
#define TT_MS_LANGID_CROATIAN_BOSNIA_HERZEGOVINA       0x101A
#define TT_MS_LANGID_BOSNIAN_BOSNIA_HERZEGOVINA        0x141A
#define TT_MS_LANGID_SERBIAN_BOSNIA_HERZ_LATIN         0x181A
#define TT_MS_LANGID_SERBIAN_BOSNIA_HERZ_CYRILLIC      0x1C1A
#define TT_MS_LANGID_BOSNIAN_BOSNIA_HERZ_CYRILLIC      0x201A
#define TT_MS_LANGID_SLOVAK_SLOVAKIA                   0x041B
#define TT_MS_LANGID_ALBANIAN_ALBANIA                  0x041C
#define TT_MS_LANGID_SWEDISH_SWEDEN                    0x041D
#define TT_MS_LANGID_SWEDISH_FINLAND                   0x081D
#define TT_MS_LANGID_THAI_THAILAND                     0x041E
#define TT_MS_LANGID_TURKISH_TURKEY                    0x041F
#define TT_MS_LANGID_URDU_PAKISTAN                     0x0420
#define TT_MS_LANGID_INDONESIAN_INDONESIA              0x0421
#define TT_MS_LANGID_UKRAINIAN_UKRAINE                 0x0422
#define TT_MS_LANGID_BELARUSIAN_BELARUS                0x0423
#define TT_MS_LANGID_SLOVENIAN_SLOVENIA                0x0424
#define TT_MS_LANGID_ESTONIAN_ESTONIA                  0x0425
#define TT_MS_LANGID_LATVIAN_LATVIA                    0x0426
#define TT_MS_LANGID_LITHUANIAN_LITHUANIA              0x0427
#define TT_MS_LANGID_TAJIK_TAJIKISTAN                  0x0428
#define TT_MS_LANGID_VIETNAMESE_VIET_NAM               0x042A
#define TT_MS_LANGID_ARMENIAN_ARMENIA                  0x042B
#define TT_MS_LANGID_AZERI_AZERBAIJAN_LATIN            0x042C
#define TT_MS_LANGID_AZERI_AZERBAIJAN_CYRILLIC         0x082C
#define TT_MS_LANGID_BASQUE_BASQUE                     0x042D
#define TT_MS_LANGID_UPPER_SORBIAN_GERMANY             0x042E
#define TT_MS_LANGID_LOWER_SORBIAN_GERMANY             0x082E
#define TT_MS_LANGID_MACEDONIAN_MACEDONIA              0x042F
#define TT_MS_LANGID_SETSWANA_SOUTH_AFRICA             0x0432
#define TT_MS_LANGID_ISIXHOSA_SOUTH_AFRICA             0x0434
#define TT_MS_LANGID_ISIZULU_SOUTH_AFRICA              0x0435
#define TT_MS_LANGID_AFRIKAANS_SOUTH_AFRICA            0x0436
#define TT_MS_LANGID_GEORGIAN_GEORGIA                  0x0437
#define TT_MS_LANGID_FAEROESE_FAEROE_ISLANDS           0x0438
#define TT_MS_LANGID_HINDI_INDIA                       0x0439
#define TT_MS_LANGID_MALTESE_MALTA                     0x043A
#define TT_MS_LANGID_SAMI_NORTHERN_NORWAY              0x043B
#define TT_MS_LANGID_SAMI_NORTHERN_SWEDEN              0x083B
#define TT_MS_LANGID_SAMI_NORTHERN_FINLAND             0x0C3B
#define TT_MS_LANGID_SAMI_LULE_NORWAY                  0x103B
#define TT_MS_LANGID_SAMI_LULE_SWEDEN                  0x143B
#define TT_MS_LANGID_SAMI_SOUTHERN_NORWAY              0x183B
#define TT_MS_LANGID_SAMI_SOUTHERN_SWEDEN              0x1C3B
#define TT_MS_LANGID_SAMI_SKOLT_FINLAND                0x203B
#define TT_MS_LANGID_SAMI_INARI_FINLAND                0x243B
#define TT_MS_LANGID_IRISH_IRELAND                     0x083C
#define TT_MS_LANGID_MALAY_MALAYSIA                    0x043E
#define TT_MS_LANGID_MALAY_BRUNEI_DARUSSALAM           0x083E
#define TT_MS_LANGID_KAZAKH_KAZAKHSTAN                 0x043F
#define TT_MS_LANGID_KYRGYZ_KYRGYZSTAN /* Cyrillic */  0x0440
#define TT_MS_LANGID_KISWAHILI_KENYA                   0x0441
#define TT_MS_LANGID_TURKMEN_TURKMENISTAN              0x0442
#define TT_MS_LANGID_UZBEK_UZBEKISTAN_LATIN            0x0443
#define TT_MS_LANGID_UZBEK_UZBEKISTAN_CYRILLIC         0x0843
#define TT_MS_LANGID_TATAR_RUSSIA                      0x0444
#define TT_MS_LANGID_BENGALI_INDIA                     0x0445
#define TT_MS_LANGID_BENGALI_BANGLADESH                0x0845
#define TT_MS_LANGID_PUNJABI_INDIA                     0x0446
#define TT_MS_LANGID_GUJARATI_INDIA                    0x0447
#define TT_MS_LANGID_ODIA_INDIA                        0x0448
#define TT_MS_LANGID_TAMIL_INDIA                       0x0449
#define TT_MS_LANGID_TELUGU_INDIA                      0x044A
#define TT_MS_LANGID_KANNADA_INDIA                     0x044B
#define TT_MS_LANGID_MALAYALAM_INDIA                   0x044C
#define TT_MS_LANGID_ASSAMESE_INDIA                    0x044D
#define TT_MS_LANGID_MARATHI_INDIA                     0x044E
#define TT_MS_LANGID_SANSKRIT_INDIA                    0x044F
#define TT_MS_LANGID_MONGOLIAN_MONGOLIA /* Cyrillic */ 0x0450
#define TT_MS_LANGID_MONGOLIAN_PRC                     0x0850
#define TT_MS_LANGID_TIBETAN_PRC                       0x0451
#define TT_MS_LANGID_WELSH_UNITED_KINGDOM              0x0452
#define TT_MS_LANGID_KHMER_CAMBODIA                    0x0453
#define TT_MS_LANGID_LAO_LAOS                          0x0454
#define TT_MS_LANGID_GALICIAN_GALICIAN                 0x0456
#define TT_MS_LANGID_KONKANI_INDIA                     0x0457
#define TT_MS_LANGID_SYRIAC_SYRIA                      0x045A
#define TT_MS_LANGID_SINHALA_SRI_LANKA                 0x045B
#define TT_MS_LANGID_INUKTITUT_CANADA                  0x045D
#define TT_MS_LANGID_INUKTITUT_CANADA_LATIN            0x085D
#define TT_MS_LANGID_AMHARIC_ETHIOPIA                  0x045E
#define TT_MS_LANGID_TAMAZIGHT_ALGERIA                 0x085F
#define TT_MS_LANGID_NEPALI_NEPAL                      0x0461
#define TT_MS_LANGID_FRISIAN_NETHERLANDS               0x0462
#define TT_MS_LANGID_PASHTO_AFGHANISTAN                0x0463
#define TT_MS_LANGID_FILIPINO_PHILIPPINES              0x0464
#define TT_MS_LANGID_DHIVEHI_MALDIVES                  0x0465
#define TT_MS_LANGID_HAUSA_NIGERIA                     0x0468
#define TT_MS_LANGID_YORUBA_NIGERIA                    0x046A
#define TT_MS_LANGID_QUECHUA_BOLIVIA                   0x046B
#define TT_MS_LANGID_QUECHUA_ECUADOR                   0x086B
#define TT_MS_LANGID_QUECHUA_PERU                      0x0C6B
#define TT_MS_LANGID_SESOTHO_SA_LEBOA_SOUTH_AFRICA     0x046C
#define TT_MS_LANGID_BASHKIR_RUSSIA                    0x046D
#define TT_MS_LANGID_LUXEMBOURGISH_LUXEMBOURG          0x046E
#define TT_MS_LANGID_GREENLANDIC_GREENLAND             0x046F
#define TT_MS_LANGID_IGBO_NIGERIA                      0x0470
#define TT_MS_LANGID_YI_PRC                            0x0478
#define TT_MS_LANGID_MAPUDUNGUN_CHILE                  0x047A
#define TT_MS_LANGID_MOHAWK_MOHAWK                     0x047C
#define TT_MS_LANGID_BRETON_FRANCE                     0x047E
#define TT_MS_LANGID_UIGHUR_PRC                        0x0480
#define TT_MS_LANGID_MAORI_NEW_ZEALAND                 0x0481
#define TT_MS_LANGID_OCCITAN_FRANCE                    0x0482
#define TT_MS_LANGID_CORSICAN_FRANCE                   0x0483
#define TT_MS_LANGID_ALSATIAN_FRANCE                   0x0484
#define TT_MS_LANGID_YAKUT_RUSSIA                      0x0485
#define TT_MS_LANGID_KICHE_GUATEMALA                   0x0486
#define TT_MS_LANGID_KINYARWANDA_RWANDA                0x0487
#define TT_MS_LANGID_WOLOF_SENEGAL                     0x0488
#define TT_MS_LANGID_DARI_AFGHANISTAN                  0x048C

  /* */


  /* legacy macro definitions not present in OpenType 1.8.1 */
#define TT_MS_LANGID_ARABIC_GENERAL                    0x0001
#define TT_MS_LANGID_CATALAN_SPAIN \
          TT_MS_LANGID_CATALAN_CATALAN
#define TT_MS_LANGID_CHINESE_GENERAL                   0x0004
#define TT_MS_LANGID_CHINESE_MACAU \
          TT_MS_LANGID_CHINESE_MACAO
#define TT_MS_LANGID_GERMAN_LIECHTENSTEI \
          TT_MS_LANGID_GERMAN_LIECHTENSTEIN
#define TT_MS_LANGID_ENGLISH_GENERAL                   0x0009
#define TT_MS_LANGID_ENGLISH_INDONESIA                 0x3809
#define TT_MS_LANGID_ENGLISH_HONG_KONG                 0x3C09
#define TT_MS_LANGID_SPANISH_SPAIN_INTERNATIONAL_SORT \
          TT_MS_LANGID_SPANISH_SPAIN_MODERN_SORT
#define TT_MS_LANGID_SPANISH_LATIN_AMERICA             0xE40AU
#define TT_MS_LANGID_FRENCH_WEST_INDIES                0x1C0C
#define TT_MS_LANGID_FRENCH_REUNION                    0x200C
#define TT_MS_LANGID_FRENCH_CONGO                      0x240C
  /* which was formerly: */
#define TT_MS_LANGID_FRENCH_ZAIRE \
          TT_MS_LANGID_FRENCH_CONGO
#define TT_MS_LANGID_FRENCH_SENEGAL                    0x280C
#define TT_MS_LANGID_FRENCH_CAMEROON                   0x2C0C
#define TT_MS_LANGID_FRENCH_COTE_D_IVOIRE              0x300C
#define TT_MS_LANGID_FRENCH_MALI                       0x340C
#define TT_MS_LANGID_FRENCH_MOROCCO                    0x380C
#define TT_MS_LANGID_FRENCH_HAITI                      0x3C0C
#define TT_MS_LANGID_FRENCH_NORTH_AFRICA               0xE40CU
#define TT_MS_LANGID_KOREAN_EXTENDED_WANSUNG_KOREA \
          TT_MS_LANGID_KOREAN_KOREA
#define TT_MS_LANGID_KOREAN_JOHAB_KOREA                0x0812
#define TT_MS_LANGID_RHAETO_ROMANIC_SWITZERLAND \
          TT_MS_LANGID_ROMANSH_SWITZERLAND
#define TT_MS_LANGID_MOLDAVIAN_MOLDAVIA                0x0818
#define TT_MS_LANGID_RUSSIAN_MOLDAVIA                  0x0819
#define TT_MS_LANGID_URDU_INDIA                        0x0820
#define TT_MS_LANGID_CLASSIC_LITHUANIAN_LITHUANIA      0x0827
#define TT_MS_LANGID_SLOVENE_SLOVENIA \
          TT_MS_LANGID_SLOVENIAN_SLOVENIA
#define TT_MS_LANGID_FARSI_IRAN                        0x0429
#define TT_MS_LANGID_BASQUE_SPAIN \
          TT_MS_LANGID_BASQUE_BASQUE
#define TT_MS_LANGID_SORBIAN_GERMANY \
          TT_MS_LANGID_UPPER_SORBIAN_GERMANY
#define TT_MS_LANGID_SUTU_SOUTH_AFRICA                 0x0430
#define TT_MS_LANGID_TSONGA_SOUTH_AFRICA               0x0431
#define TT_MS_LANGID_TSWANA_SOUTH_AFRICA \
          TT_MS_LANGID_SETSWANA_SOUTH_AFRICA
#define TT_MS_LANGID_VENDA_SOUTH_AFRICA                0x0433
#define TT_MS_LANGID_XHOSA_SOUTH_AFRICA \
          TT_MS_LANGID_ISIXHOSA_SOUTH_AFRICA
#define TT_MS_LANGID_ZULU_SOUTH_AFRICA \
          TT_MS_LANGID_ISIZULU_SOUTH_AFRICA
#define TT_MS_LANGID_SAAMI_LAPONIA                     0x043B
  /* the next two values are incorrectly inverted */
#define TT_MS_LANGID_IRISH_GAELIC_IRELAND              0x043C
#define TT_MS_LANGID_SCOTTISH_GAELIC_UNITED_KINGDOM    0x083C
#define TT_MS_LANGID_YIDDISH_GERMANY                   0x043D
#define TT_MS_LANGID_KAZAK_KAZAKSTAN \
          TT_MS_LANGID_KAZAKH_KAZAKHSTAN
#define TT_MS_LANGID_KIRGHIZ_KIRGHIZ_REPUBLIC \
          TT_MS_LANGID_KYRGYZ_KYRGYZSTAN
#define TT_MS_LANGID_KIRGHIZ_KIRGHIZSTAN \
          TT_MS_LANGID_KYRGYZ_KYRGYZSTAN
#define TT_MS_LANGID_SWAHILI_KENYA \
          TT_MS_LANGID_KISWAHILI_KENYA
#define TT_MS_LANGID_TATAR_TATARSTAN \
          TT_MS_LANGID_TATAR_RUSSIA
#define TT_MS_LANGID_PUNJABI_ARABIC_PAKISTAN           0x0846
#define TT_MS_LANGID_ORIYA_INDIA \
          TT_MS_LANGID_ODIA_INDIA
#define TT_MS_LANGID_MONGOLIAN_MONGOLIA_MONGOLIAN \
          TT_MS_LANGID_MONGOLIAN_PRC
#define TT_MS_LANGID_TIBETAN_CHINA \
          TT_MS_LANGID_TIBETAN_PRC
#define TT_MS_LANGID_DZONGHKA_BHUTAN                   0x0851
#define TT_MS_LANGID_TIBETAN_BHUTAN \
          TT_MS_LANGID_DZONGHKA_BHUTAN
#define TT_MS_LANGID_WELSH_WALES \
          TT_MS_LANGID_WELSH_UNITED_KINGDOM
#define TT_MS_LANGID_BURMESE_MYANMAR                   0x0455
#define TT_MS_LANGID_GALICIAN_SPAIN \
          TT_MS_LANGID_GALICIAN_GALICIAN
#define TT_MS_LANGID_MANIPURI_INDIA  /* Bengali */     0x0458
#define TT_MS_LANGID_SINDHI_INDIA /* Arabic */         0x0459
#define TT_MS_LANGID_SINDHI_PAKISTAN                   0x0859
#define TT_MS_LANGID_SINHALESE_SRI_LANKA \
          TT_MS_LANGID_SINHALA_SRI_LANKA
#define TT_MS_LANGID_CHEROKEE_UNITED_STATES            0x045C
#define TT_MS_LANGID_TAMAZIGHT_MOROCCO /* Arabic */    0x045F
#define TT_MS_LANGID_TAMAZIGHT_MOROCCO_LATIN \
          TT_MS_LANGID_TAMAZIGHT_ALGERIA
#define TT_MS_LANGID_KASHMIRI_PAKISTAN /* Arabic */    0x0460
#define TT_MS_LANGID_KASHMIRI_SASIA                    0x0860
#define TT_MS_LANGID_KASHMIRI_INDIA \
          TT_MS_LANGID_KASHMIRI_SASIA
#define TT_MS_LANGID_NEPALI_INDIA                      0x0861
#define TT_MS_LANGID_DIVEHI_MALDIVES \
          TT_MS_LANGID_DHIVEHI_MALDIVES
#define TT_MS_LANGID_EDO_NIGERIA                       0x0466
#define TT_MS_LANGID_FULFULDE_NIGERIA                  0x0467
#define TT_MS_LANGID_IBIBIO_NIGERIA                    0x0469
#define TT_MS_LANGID_SEPEDI_SOUTH_AFRICA \
          TT_MS_LANGID_SESOTHO_SA_LEBOA_SOUTH_AFRICA
#define TT_MS_LANGID_SOTHO_SOUTHERN_SOUTH_AFRICA \
          TT_MS_LANGID_SESOTHO_SA_LEBOA_SOUTH_AFRICA
#define TT_MS_LANGID_KANURI_NIGERIA                    0x0471
#define TT_MS_LANGID_OROMO_ETHIOPIA                    0x0472
#define TT_MS_LANGID_TIGRIGNA_ETHIOPIA                 0x0473
#define TT_MS_LANGID_TIGRIGNA_ERYTHREA                 0x0873
#define TT_MS_LANGID_TIGRIGNA_ERYTREA \
          TT_MS_LANGID_TIGRIGNA_ERYTHREA
#define TT_MS_LANGID_GUARANI_PARAGUAY                  0x0474
#define TT_MS_LANGID_HAWAIIAN_UNITED_STATES            0x0475
#define TT_MS_LANGID_LATIN                             0x0476
#define TT_MS_LANGID_SOMALI_SOMALIA                    0x0477
#define TT_MS_LANGID_YI_CHINA \
          TT_MS_LANGID_YI_PRC
#define TT_MS_LANGID_PAPIAMENTU_NETHERLANDS_ANTILLES   0x0479
#define TT_MS_LANGID_UIGHUR_CHINA \
          TT_MS_LANGID_UIGHUR_PRC


  /**************************************************************************
   *
   * @enum:
   *   TT_NAME_ID_XXX
   *
   * @description:
   *   Possible values of the 'name' identifier field in the name records of
   *   an SFNT 'name' table.  These values are platform independent.
   */

#define TT_NAME_ID_COPYRIGHT              0
#define TT_NAME_ID_FONT_FAMILY            1
#define TT_NAME_ID_FONT_SUBFAMILY         2
#define TT_NAME_ID_UNIQUE_ID              3
#define TT_NAME_ID_FULL_NAME              4
#define TT_NAME_ID_VERSION_STRING         5
#define TT_NAME_ID_PS_NAME                6
#define TT_NAME_ID_TRADEMARK              7

  /* the following values are from the OpenType spec */
#define TT_NAME_ID_MANUFACTURER           8
#define TT_NAME_ID_DESIGNER               9
#define TT_NAME_ID_DESCRIPTION            10
#define TT_NAME_ID_VENDOR_URL             11
#define TT_NAME_ID_DESIGNER_URL           12
#define TT_NAME_ID_LICENSE                13
#define TT_NAME_ID_LICENSE_URL            14
  /* number 15 is reserved */
#define TT_NAME_ID_TYPOGRAPHIC_FAMILY     16
#define TT_NAME_ID_TYPOGRAPHIC_SUBFAMILY  17
#define TT_NAME_ID_MAC_FULL_NAME          18

  /* The following code is new as of 2000-01-21 */
#define TT_NAME_ID_SAMPLE_TEXT            19

  /* This is new in OpenType 1.3 */
#define TT_NAME_ID_CID_FINDFONT_NAME      20

  /* This is new in OpenType 1.5 */
#define TT_NAME_ID_WWS_FAMILY             21
#define TT_NAME_ID_WWS_SUBFAMILY          22

  /* This is new in OpenType 1.7 */
#define TT_NAME_ID_LIGHT_BACKGROUND       23
#define TT_NAME_ID_DARK_BACKGROUND        24

  /* This is new in OpenType 1.8 */
#define TT_NAME_ID_VARIATIONS_PREFIX      25

  /* these two values are deprecated */
#define TT_NAME_ID_PREFERRED_FAMILY     TT_NAME_ID_TYPOGRAPHIC_FAMILY
#define TT_NAME_ID_PREFERRED_SUBFAMILY  TT_NAME_ID_TYPOGRAPHIC_SUBFAMILY


  /**************************************************************************
   *
   * @enum:
   *   TT_UCR_XXX
   *
   * @description:
   *   Possible bit mask values for the `ulUnicodeRangeX` fields in an SFNT
   *   'OS/2' table.
   */

  /* ulUnicodeRange1 */
  /* --------------- */

  /* Bit  0   Basic Latin */
#define TT_UCR_BASIC_LATIN                     (1L <<  0) /* U+0020-U+007E */
  /* Bit  1   C1 Controls and Latin-1 Supplement */
#define TT_UCR_LATIN1_SUPPLEMENT               (1L <<  1) /* U+0080-U+00FF */
  /* Bit  2   Latin Extended-A */
#define TT_UCR_LATIN_EXTENDED_A                (1L <<  2) /* U+0100-U+017F */
  /* Bit  3   Latin Extended-B */
#define TT_UCR_LATIN_EXTENDED_B                (1L <<  3) /* U+0180-U+024F */
  /* Bit  4   IPA Extensions                 */
  /*          Phonetic Extensions            */
  /*          Phonetic Extensions Supplement */
#define TT_UCR_IPA_EXTENSIONS                  (1L <<  4) /* U+0250-U+02AF */
                                                          /* U+1D00-U+1D7F */
                                                          /* U+1D80-U+1DBF */
  /* Bit  5   Spacing Modifier Letters */
  /*          Modifier Tone Letters    */
#define TT_UCR_SPACING_MODIFIER                (1L <<  5) /* U+02B0-U+02FF */
                                                          /* U+A700-U+A71F */
  /* Bit  6   Combining Diacritical Marks            */
  /*          Combining Diacritical Marks Supplement */
#define TT_UCR_COMBINING_DIACRITICAL_MARKS     (1L <<  6) /* U+0300-U+036F */
                                                          /* U+1DC0-U+1DFF */
  /* Bit  7   Greek and Coptic */
#define TT_UCR_GREEK                           (1L <<  7) /* U+0370-U+03FF */
  /* Bit  8   Coptic */
#define TT_UCR_COPTIC                          (1L <<  8) /* U+2C80-U+2CFF */
  /* Bit  9   Cyrillic            */
  /*          Cyrillic Supplement */
  /*          Cyrillic Extended-A */
  /*          Cyrillic Extended-B */
#define TT_UCR_CYRILLIC                        (1L <<  9) /* U+0400-U+04FF */
                                                          /* U+0500-U+052F */
                                                          /* U+2DE0-U+2DFF */
                                                          /* U+A640-U+A69F */
  /* Bit 10   Armenian */
#define TT_UCR_ARMENIAN                        (1L << 10) /* U+0530-U+058F */
  /* Bit 11   Hebrew */
#define TT_UCR_HEBREW                          (1L << 11) /* U+0590-U+05FF */
  /* Bit 12   Vai */
#define TT_UCR_VAI                             (1L << 12) /* U+A500-U+A63F */
  /* Bit 13   Arabic            */
  /*          Arabic Supplement */
#define TT_UCR_ARABIC                          (1L << 13) /* U+0600-U+06FF */
                                                          /* U+0750-U+077F */
  /* Bit 14   NKo */
#define TT_UCR_NKO                             (1L << 14) /* U+07C0-U+07FF */
  /* Bit 15   Devanagari */
#define TT_UCR_DEVANAGARI                      (1L << 15) /* U+0900-U+097F */
  /* Bit 16   Bengali */
#define TT_UCR_BENGALI                         (1L << 16) /* U+0980-U+09FF */
  /* Bit 17   Gurmukhi */
#define TT_UCR_GURMUKHI                        (1L << 17) /* U+0A00-U+0A7F */
  /* Bit 18   Gujarati */
#define TT_UCR_GUJARATI                        (1L << 18) /* U+0A80-U+0AFF */
  /* Bit 19   Oriya */
#define TT_UCR_ORIYA                           (1L << 19) /* U+0B00-U+0B7F */
  /* Bit 20   Tamil */
#define TT_UCR_TAMIL                           (1L << 20) /* U+0B80-U+0BFF */
  /* Bit 21   Telugu */
#define TT_UCR_TELUGU                          (1L << 21) /* U+0C00-U+0C7F */
  /* Bit 22   Kannada */
#define TT_UCR_KANNADA                         (1L << 22) /* U+0C80-U+0CFF */
  /* Bit 23   Malayalam */
#define TT_UCR_MALAYALAM                       (1L << 23) /* U+0D00-U+0D7F */
  /* Bit 24   Thai */
#define TT_UCR_THAI                            (1L << 24) /* U+0E00-U+0E7F */
  /* Bit 25   Lao */
#define TT_UCR_LAO                             (1L << 25) /* U+0E80-U+0EFF */
  /* Bit 26   Georgian            */
  /*          Georgian Supplement */
#define TT_UCR_GEORGIAN                        (1L << 26) /* U+10A0-U+10FF */
                                                          /* U+2D00-U+2D2F */
  /* Bit 27   Balinese */
#define TT_UCR_BALINESE                        (1L << 27) /* U+1B00-U+1B7F */
  /* Bit 28   Hangul Jamo */
#define TT_UCR_HANGUL_JAMO                     (1L << 28) /* U+1100-U+11FF */
  /* Bit 29   Latin Extended Additional */
  /*          Latin Extended-C          */
  /*          Latin Extended-D          */
#define TT_UCR_LATIN_EXTENDED_ADDITIONAL       (1L << 29) /* U+1E00-U+1EFF */
                                                          /* U+2C60-U+2C7F */
                                                          /* U+A720-U+A7FF */
  /* Bit 30   Greek Extended */
#define TT_UCR_GREEK_EXTENDED                  (1L << 30) /* U+1F00-U+1FFF */
  /* Bit 31   General Punctuation      */
  /*          Supplemental Punctuation */
#define TT_UCR_GENERAL_PUNCTUATION             (1L << 31) /* U+2000-U+206F */
                                                          /* U+2E00-U+2E7F */

  /* ulUnicodeRange2 */
  /* --------------- */

  /* Bit 32   Superscripts And Subscripts */
#define TT_UCR_SUPERSCRIPTS_SUBSCRIPTS         (1L <<  0) /* U+2070-U+209F */
  /* Bit 33   Currency Symbols */
#define TT_UCR_CURRENCY_SYMBOLS                (1L <<  1) /* U+20A0-U+20CF */
  /* Bit 34   Combining Diacritical Marks For Symbols */
#define TT_UCR_COMBINING_DIACRITICAL_MARKS_SYMB \
                                               (1L <<  2) /* U+20D0-U+20FF */
  /* Bit 35   Letterlike Symbols */
#define TT_UCR_LETTERLIKE_SYMBOLS              (1L <<  3) /* U+2100-U+214F */
  /* Bit 36   Number Forms */
#define TT_UCR_NUMBER_FORMS                    (1L <<  4) /* U+2150-U+218F */
  /* Bit 37   Arrows                           */
  /*          Supplemental Arrows-A            */
  /*          Supplemental Arrows-B            */
  /*          Miscellaneous Symbols and Arrows */
#define TT_UCR_ARROWS                          (1L <<  5) /* U+2190-U+21FF */
                                                          /* U+27F0-U+27FF */
                                                          /* U+2900-U+297F */
                                                          /* U+2B00-U+2BFF */
  /* Bit 38   Mathematical Operators               */
  /*          Supplemental Mathematical Operators  */
  /*          Miscellaneous Mathematical Symbols-A */
  /*          Miscellaneous Mathematical Symbols-B */
#define TT_UCR_MATHEMATICAL_OPERATORS          (1L <<  6) /* U+2200-U+22FF */
                                                          /* U+2A00-U+2AFF */
                                                          /* U+27C0-U+27EF */
                                                          /* U+2980-U+29FF */
  /* Bit 39 Miscellaneous Technical */
#define TT_UCR_MISCELLANEOUS_TECHNICAL         (1L <<  7) /* U+2300-U+23FF */
  /* Bit 40   Control Pictures */
#define TT_UCR_CONTROL_PICTURES                (1L <<  8) /* U+2400-U+243F */
  /* Bit 41   Optical Character Recognition */
#define TT_UCR_OCR                             (1L <<  9) /* U+2440-U+245F */
  /* Bit 42   Enclosed Alphanumerics */
#define TT_UCR_ENCLOSED_ALPHANUMERICS          (1L << 10) /* U+2460-U+24FF */
  /* Bit 43   Box Drawing */
#define TT_UCR_BOX_DRAWING                     (1L << 11) /* U+2500-U+257F */
  /* Bit 44   Block Elements */
#define TT_UCR_BLOCK_ELEMENTS                  (1L << 12) /* U+2580-U+259F */
  /* Bit 45   Geometric Shapes */
#define TT_UCR_GEOMETRIC_SHAPES                (1L << 13) /* U+25A0-U+25FF */
  /* Bit 46   Miscellaneous Symbols */
#define TT_UCR_MISCELLANEOUS_SYMBOLS           (1L << 14) /* U+2600-U+26FF */
  /* Bit 47   Dingbats */
#define TT_UCR_DINGBATS                        (1L << 15) /* U+2700-U+27BF */
  /* Bit 48   CJK Symbols and Punctuation */
#define TT_UCR_CJK_SYMBOLS                     (1L << 16) /* U+3000-U+303F */
  /* Bit 49   Hiragana */
#define TT_UCR_HIRAGANA                        (1L << 17) /* U+3040-U+309F */
  /* Bit 50   Katakana                     */
  /*          Katakana Phonetic Extensions */
#define TT_UCR_KATAKANA                        (1L << 18) /* U+30A0-U+30FF */
                                                          /* U+31F0-U+31FF */
  /* Bit 51   Bopomofo          */
  /*          Bopomofo Extended */
#define TT_UCR_BOPOMOFO                        (1L << 19) /* U+3100-U+312F */
                                                          /* U+31A0-U+31BF */
  /* Bit 52   Hangul Compatibility Jamo */
#define TT_UCR_HANGUL_COMPATIBILITY_JAMO       (1L << 20) /* U+3130-U+318F */
  /* Bit 53   Phags-Pa */
#define TT_UCR_CJK_MISC                        (1L << 21) /* U+A840-U+A87F */
#define TT_UCR_KANBUN  TT_UCR_CJK_MISC /* deprecated */
#define TT_UCR_PHAGSPA
  /* Bit 54   Enclosed CJK Letters and Months */
#define TT_UCR_ENCLOSED_CJK_LETTERS_MONTHS     (1L << 22) /* U+3200-U+32FF */
  /* Bit 55   CJK Compatibility */
#define TT_UCR_CJK_COMPATIBILITY               (1L << 23) /* U+3300-U+33FF */
  /* Bit 56   Hangul Syllables */
#define TT_UCR_HANGUL                          (1L << 24) /* U+AC00-U+D7A3 */
  /* Bit 57   High Surrogates              */
  /*          High Private Use Surrogates  */
  /*          Low Surrogates               */

  /* According to OpenType specs v.1.3+,   */
  /* setting bit 57 implies that there is  */
  /* at least one codepoint beyond the     */
  /* Basic Multilingual Plane that is      */
  /* supported by this font.  So it really */
  /* means >= U+10000.                     */
#define TT_UCR_SURROGATES                      (1L << 25) /* U+D800-U+DB7F */
                                                          /* U+DB80-U+DBFF */
                                                          /* U+DC00-U+DFFF */
#define TT_UCR_NON_PLANE_0  TT_UCR_SURROGATES
  /* Bit 58  Phoenician */
#define TT_UCR_PHOENICIAN                      (1L << 26) /*U+10900-U+1091F*/
  /* Bit 59   CJK Unified Ideographs             */
  /*          CJK Radicals Supplement            */
  /*          Kangxi Radicals                    */
  /*          Ideographic Description Characters */
  /*          CJK Unified Ideographs Extension A */
  /*          CJK Unified Ideographs Extension B */
  /*          Kanbun                             */
#define TT_UCR_CJK_UNIFIED_IDEOGRAPHS          (1L << 27) /* U+4E00-U+9FFF */
                                                          /* U+2E80-U+2EFF */
                                                          /* U+2F00-U+2FDF */
                                                          /* U+2FF0-U+2FFF */
                                                          /* U+3400-U+4DB5 */
                                                          /*U+20000-U+2A6DF*/
                                                          /* U+3190-U+319F */
  /* Bit 60   Private Use */
#define TT_UCR_PRIVATE_USE                     (1L << 28) /* U+E000-U+F8FF */
  /* Bit 61   CJK Strokes                             */
  /*          CJK Compatibility Ideographs            */
  /*          CJK Compatibility Ideographs Supplement */
#define TT_UCR_CJK_COMPATIBILITY_IDEOGRAPHS    (1L << 29) /* U+31C0-U+31EF */
                                                          /* U+F900-U+FAFF */
                                                          /*U+2F800-U+2FA1F*/
  /* Bit 62   Alphabetic Presentation Forms */
#define TT_UCR_ALPHABETIC_PRESENTATION_FORMS   (1L << 30) /* U+FB00-U+FB4F */
  /* Bit 63   Arabic Presentation Forms-A */
#define TT_UCR_ARABIC_PRESENTATION_FORMS_A     (1L << 31) /* U+FB50-U+FDFF */

  /* ulUnicodeRange3 */
  /* --------------- */

  /* Bit 64   Combining Half Marks */
#define TT_UCR_COMBINING_HALF_MARKS            (1L <<  0) /* U+FE20-U+FE2F */
  /* Bit 65   Vertical forms          */
  /*          CJK Compatibility Forms */
#define TT_UCR_CJK_COMPATIBILITY_FORMS         (1L <<  1) /* U+FE10-U+FE1F */
                                                          /* U+FE30-U+FE4F */
  /* Bit 66   Small Form Variants */
#define TT_UCR_SMALL_FORM_VARIANTS             (1L <<  2) /* U+FE50-U+FE6F */
  /* Bit 67   Arabic Presentation Forms-B */
#define TT_UCR_ARABIC_PRESENTATION_FORMS_B     (1L <<  3) /* U+FE70-U+FEFE */
  /* Bit 68   Halfwidth and Fullwidth Forms */
#define TT_UCR_HALFWIDTH_FULLWIDTH_FORMS       (1L <<  4) /* U+FF00-U+FFEF */
  /* Bit 69   Specials */
#define TT_UCR_SPECIALS                        (1L <<  5) /* U+FFF0-U+FFFD */
  /* Bit 70   Tibetan */
#define TT_UCR_TIBETAN                         (1L <<  6) /* U+0F00-U+0FFF */
  /* Bit 71   Syriac */
#define TT_UCR_SYRIAC                          (1L <<  7) /* U+0700-U+074F */
  /* Bit 72   Thaana */
#define TT_UCR_THAANA                          (1L <<  8) /* U+0780-U+07BF */
  /* Bit 73   Sinhala */
#define TT_UCR_SINHALA                         (1L <<  9) /* U+0D80-U+0DFF */
  /* Bit 74   Myanmar */
#define TT_UCR_MYANMAR                         (1L << 10) /* U+1000-U+109F */
  /* Bit 75   Ethiopic            */
  /*          Ethiopic Supplement */
  /*          Ethiopic Extended   */
#define TT_UCR_ETHIOPIC                        (1L << 11) /* U+1200-U+137F */
                                                          /* U+1380-U+139F */
                                                          /* U+2D80-U+2DDF */
  /* Bit 76   Cherokee */
#define TT_UCR_CHEROKEE                        (1L << 12) /* U+13A0-U+13FF */
  /* Bit 77   Unified Canadian Aboriginal Syllabics */
#define TT_UCR_CANADIAN_ABORIGINAL_SYLLABICS   (1L << 13) /* U+1400-U+167F */
  /* Bit 78   Ogham */
#define TT_UCR_OGHAM                           (1L << 14) /* U+1680-U+169F */
  /* Bit 79   Runic */
#define TT_UCR_RUNIC                           (1L << 15) /* U+16A0-U+16FF */
  /* Bit 80   Khmer         */
  /*          Khmer Symbols */
#define TT_UCR_KHMER                           (1L << 16) /* U+1780-U+17FF */
                                                          /* U+19E0-U+19FF */
  /* Bit 81   Mongolian */
#define TT_UCR_MONGOLIAN                       (1L << 17) /* U+1800-U+18AF */
  /* Bit 82   Braille Patterns */
#define TT_UCR_BRAILLE                         (1L << 18) /* U+2800-U+28FF */
  /* Bit 83   Yi Syllables */
  /*          Yi Radicals  */
#define TT_UCR_YI                              (1L << 19) /* U+A000-U+A48F */
                                                          /* U+A490-U+A4CF */
  /* Bit 84   Tagalog  */
  /*          Hanunoo  */
  /*          Buhid    */
  /*          Tagbanwa */
#define TT_UCR_PHILIPPINE                      (1L << 20) /* U+1700-U+171F */
                                                          /* U+1720-U+173F */
                                                          /* U+1740-U+175F */
                                                          /* U+1760-U+177F */
  /* Bit 85   Old Italic */
#define TT_UCR_OLD_ITALIC                      (1L << 21) /*U+10300-U+1032F*/
  /* Bit 86   Gothic */
#define TT_UCR_GOTHIC                          (1L << 22) /*U+10330-U+1034F*/
  /* Bit 87   Deseret */
#define TT_UCR_DESERET                         (1L << 23) /*U+10400-U+1044F*/
  /* Bit 88   Byzantine Musical Symbols      */
  /*          Musical Symbols                */
  /*          Ancient Greek Musical Notation */
#define TT_UCR_MUSICAL_SYMBOLS                 (1L << 24) /*U+1D000-U+1D0FF*/
                                                          /*U+1D100-U+1D1FF*/
                                                          /*U+1D200-U+1D24F*/
  /* Bit 89   Mathematical Alphanumeric Symbols */
#define TT_UCR_MATH_ALPHANUMERIC_SYMBOLS       (1L << 25) /*U+1D400-U+1D7FF*/
  /* Bit 90   Private Use (plane 15) */
  /*          Private Use (plane 16) */
#define TT_UCR_PRIVATE_USE_SUPPLEMENTARY       (1L << 26) /*U+F0000-U+FFFFD*/
                                                        /*U+100000-U+10FFFD*/
  /* Bit 91   Variation Selectors            */
  /*          Variation Selectors Supplement */
#define TT_UCR_VARIATION_SELECTORS             (1L << 27) /* U+FE00-U+FE0F */
                                                          /*U+E0100-U+E01EF*/
  /* Bit 92   Tags */
#define TT_UCR_TAGS                            (1L << 28) /*U+E0000-U+E007F*/
  /* Bit 93   Limbu */
#define TT_UCR_LIMBU                           (1L << 29) /* U+1900-U+194F */
  /* Bit 94   Tai Le */
#define TT_UCR_TAI_LE                          (1L << 30) /* U+1950-U+197F */
  /* Bit 95   New Tai Lue */
#define TT_UCR_NEW_TAI_LUE                     (1L << 31) /* U+1980-U+19DF */

  /* ulUnicodeRange4 */
  /* --------------- */

  /* Bit 96   Buginese */
#define TT_UCR_BUGINESE                        (1L <<  0) /* U+1A00-U+1A1F */
  /* Bit 97   Glagolitic */
#define TT_UCR_GLAGOLITIC                      (1L <<  1) /* U+2C00-U+2C5F */
  /* Bit 98   Tifinagh */
#define TT_UCR_TIFINAGH                        (1L <<  2) /* U+2D30-U+2D7F */
  /* Bit 99   Yijing Hexagram Symbols */
#define TT_UCR_YIJING                          (1L <<  3) /* U+4DC0-U+4DFF */
  /* Bit 100  Syloti Nagri */
#define TT_UCR_SYLOTI_NAGRI                    (1L <<  4) /* U+A800-U+A82F */
  /* Bit 101  Linear B Syllabary */
  /*          Linear B Ideograms */
  /*          Aegean Numbers     */
#define TT_UCR_LINEAR_B                        (1L <<  5) /*U+10000-U+1007F*/
                                                          /*U+10080-U+100FF*/
                                                          /*U+10100-U+1013F*/
  /* Bit 102  Ancient Greek Numbers */
#define TT_UCR_ANCIENT_GREEK_NUMBERS           (1L <<  6) /*U+10140-U+1018F*/
  /* Bit 103  Ugaritic */
#define TT_UCR_UGARITIC                        (1L <<  7) /*U+10380-U+1039F*/
  /* Bit 104  Old Persian */
#define TT_UCR_OLD_PERSIAN                     (1L <<  8) /*U+103A0-U+103DF*/
  /* Bit 105  Shavian */
#define TT_UCR_SHAVIAN                         (1L <<  9) /*U+10450-U+1047F*/
  /* Bit 106  Osmanya */
#define TT_UCR_OSMANYA                         (1L << 10) /*U+10480-U+104AF*/
  /* Bit 107  Cypriot Syllabary */
#define TT_UCR_CYPRIOT_SYLLABARY               (1L << 11) /*U+10800-U+1083F*/
  /* Bit 108  Kharoshthi */
#define TT_UCR_KHAROSHTHI                      (1L << 12) /*U+10A00-U+10A5F*/
  /* Bit 109  Tai Xuan Jing Symbols */
#define TT_UCR_TAI_XUAN_JING                   (1L << 13) /*U+1D300-U+1D35F*/
  /* Bit 110  Cuneiform                         */
  /*          Cuneiform Numbers and Punctuation */
#define TT_UCR_CUNEIFORM                       (1L << 14) /*U+12000-U+123FF*/
                                                          /*U+12400-U+1247F*/
  /* Bit 111  Counting Rod Numerals */
#define TT_UCR_COUNTING_ROD_NUMERALS           (1L << 15) /*U+1D360-U+1D37F*/
  /* Bit 112  Sundanese */
#define TT_UCR_SUNDANESE                       (1L << 16) /* U+1B80-U+1BBF */
  /* Bit 113  Lepcha */
#define TT_UCR_LEPCHA                          (1L << 17) /* U+1C00-U+1C4F */
  /* Bit 114  Ol Chiki */
#define TT_UCR_OL_CHIKI                        (1L << 18) /* U+1C50-U+1C7F */
  /* Bit 115  Saurashtra */
#define TT_UCR_SAURASHTRA                      (1L << 19) /* U+A880-U+A8DF */
  /* Bit 116  Kayah Li */
#define TT_UCR_KAYAH_LI                        (1L << 20) /* U+A900-U+A92F */
  /* Bit 117  Rejang */
#define TT_UCR_REJANG                          (1L << 21) /* U+A930-U+A95F */
  /* Bit 118  Cham */
#define TT_UCR_CHAM                            (1L << 22) /* U+AA00-U+AA5F */
  /* Bit 119  Ancient Symbols */
#define TT_UCR_ANCIENT_SYMBOLS                 (1L << 23) /*U+10190-U+101CF*/
  /* Bit 120  Phaistos Disc */
#define TT_UCR_PHAISTOS_DISC                   (1L << 24) /*U+101D0-U+101FF*/
  /* Bit 121  Carian */
  /*          Lycian */
  /*          Lydian */
#define TT_UCR_OLD_ANATOLIAN                   (1L << 25) /*U+102A0-U+102DF*/
                                                          /*U+10280-U+1029F*/
                                                          /*U+10920-U+1093F*/
  /* Bit 122  Domino Tiles  */
  /*          Mahjong Tiles */
#define TT_UCR_GAME_TILES                      (1L << 26) /*U+1F030-U+1F09F*/
                                                          /*U+1F000-U+1F02F*/
  /* Bit 123-127 Reserved for process-internal usage */

  /* */

  /* for backward compatibility with older FreeType versions */
#define TT_UCR_ARABIC_PRESENTATION_A         \
          TT_UCR_ARABIC_PRESENTATION_FORMS_A
#define TT_UCR_ARABIC_PRESENTATION_B         \
          TT_UCR_ARABIC_PRESENTATION_FORMS_B

#define TT_UCR_COMBINING_DIACRITICS          \
          TT_UCR_COMBINING_DIACRITICAL_MARKS
#define TT_UCR_COMBINING_DIACRITICS_SYMB          \
          TT_UCR_COMBINING_DIACRITICAL_MARKS_SYMB




#endif /* TTNAMEID_H_ */


/* END */
//
// ===========================  ftbdf.h  ===========================
//
/****************************************************************************
 *
 * ftbdf.h
 *
 *   FreeType API for accessing BDF-specific strings (specification).
 *
 * Copyright (C) 2002-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTBDF_H_
#define FTBDF_H_

#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   bdf_fonts
   *
   * @title:
   *   BDF and PCF Files
   *
   * @abstract:
   *   BDF and PCF specific API.
   *
   * @description:
   *   This section contains the declaration of functions specific to BDF and
   *   PCF fonts.
   *
   */


  /**************************************************************************
   *
   * @enum:
   *    BDF_PropertyType
   *
   * @description:
   *    A list of BDF property types.
   *
   * @values:
   *    BDF_PROPERTY_TYPE_NONE ::
   *      Value~0 is used to indicate a missing property.
   *
   *    BDF_PROPERTY_TYPE_ATOM ::
   *      Property is a string atom.
   *
   *    BDF_PROPERTY_TYPE_INTEGER ::
   *      Property is a 32-bit signed integer.
   *
   *    BDF_PROPERTY_TYPE_CARDINAL ::
   *      Property is a 32-bit unsigned integer.
   */
  typedef enum  BDF_PropertyType_
  {
    BDF_PROPERTY_TYPE_NONE     = 0,
    BDF_PROPERTY_TYPE_ATOM     = 1,
    BDF_PROPERTY_TYPE_INTEGER  = 2,
    BDF_PROPERTY_TYPE_CARDINAL = 3

  } BDF_PropertyType;


  /**************************************************************************
   *
   * @type:
   *    BDF_Property
   *
   * @description:
   *    A handle to a @BDF_PropertyRec structure to model a given BDF/PCF
   *    property.
   */
  typedef struct BDF_PropertyRec_*  BDF_Property;


  /**************************************************************************
   *
   * @struct:
   *    BDF_PropertyRec
   *
   * @description:
   *    This structure models a given BDF/PCF property.
   *
   * @fields:
   *    type ::
   *      The property type.
   *
   *    u.atom ::
   *      The atom string, if type is @BDF_PROPERTY_TYPE_ATOM.  May be
   *      `NULL`, indicating an empty string.
   *
   *    u.integer ::
   *      A signed integer, if type is @BDF_PROPERTY_TYPE_INTEGER.
   *
   *    u.cardinal ::
   *      An unsigned integer, if type is @BDF_PROPERTY_TYPE_CARDINAL.
   */
  typedef struct  BDF_PropertyRec_
  {
    BDF_PropertyType  type;
    union {
      const char*     atom;
      FT_Int32        integer;
      FT_UInt32       cardinal;

    } u;

  } BDF_PropertyRec;


  /**************************************************************************
   *
   * @function:
   *    FT_Get_BDF_Charset_ID
   *
   * @description:
   *    Retrieve a BDF font character set identity, according to the BDF
   *    specification.
   *
   * @input:
   *    face ::
   *      A handle to the input face.
   *
   * @output:
   *    acharset_encoding ::
   *      Charset encoding, as a C~string, owned by the face.
   *
   *    acharset_registry ::
   *      Charset registry, as a C~string, owned by the face.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function only works with BDF faces, returning an error otherwise.
   */
   FT_Error 
  FT_Get_BDF_Charset_ID( FT_Face       face,
                         const char*  *acharset_encoding,
                         const char*  *acharset_registry );


  /**************************************************************************
   *
   * @function:
   *    FT_Get_BDF_Property
   *
   * @description:
   *    Retrieve a BDF property from a BDF or PCF font file.
   *
   * @input:
   *    face ::
   *      A handle to the input face.
   *
   *    name ::
   *      The property name.
   *
   * @output:
   *    aproperty ::
   *      The property.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function works with BDF _and_ PCF fonts.  It returns an error
   *   otherwise.  It also returns an error if the property is not in the
   *   font.
   *
   *   A 'property' is a either key-value pair within the STARTPROPERTIES
   *   ... ENDPROPERTIES block of a BDF font or a key-value pair from the
   *   `info->props` array within a `FontRec` structure of a PCF font.
   *
   *   Integer properties are always stored as 'signed' within PCF fonts;
   *   consequently, @BDF_PROPERTY_TYPE_CARDINAL is a possible return value
   *   for BDF fonts only.
   *
   *   In case of error, `aproperty->type` is always set to
   *   @BDF_PROPERTY_TYPE_NONE.
   */
   FT_Error 
  FT_Get_BDF_Property( FT_Face           face,
                       const char*       prop_name,
                       BDF_PropertyRec  *aproperty );

  /* */



#endif /* FTBDF_H_ */


/* END */
//
// ===========================  ftlzw.h  ===========================
//
/****************************************************************************
 *
 * ftlzw.h
 *
 *   LZW-compressed stream support.
 *
 * Copyright (C) 2004-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTLZW_H_
#define FTLZW_H_

#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif




  /**************************************************************************
   *
   * @section:
   *   lzw
   *
   * @title:
   *   LZW Streams
   *
   * @abstract:
   *   Using LZW-compressed font files.
   *
   * @description:
   *   In certain builds of the library, LZW compression recognition is
   *   automatically handled when calling @FT_New_Face or @FT_Open_Face.
   *   This means that if no font driver is capable of handling the raw
   *   compressed file, the library will try to open a LZW stream from it and
   *   re-open the face with it.
   *
   *   The stream implementation is very basic and resets the decompression
   *   process each time seeking backwards is needed within the stream,
   *   which significantly undermines the performance.
   *
   *   This section contains the declaration of LZW-specific functions.
   *
   */

  /**************************************************************************
   *
   * @function:
   *   FT_Stream_OpenLZW
   *
   * @description:
   *   Open a new stream to parse LZW-compressed font files.  This is mainly
   *   used to support the compressed `*.pcf.Z` fonts that come with XFree86.
   *
   * @input:
   *   stream ::
   *     The target embedding stream.
   *
   *   source ::
   *     The source stream.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The source stream must be opened _before_ calling this function.
   *
   *   Calling the internal function `FT_Stream_Close` on the new stream will
   *   **not** call `FT_Stream_Close` on the source stream.  None of the
   *   stream objects will be released to the heap.
   *
   *   This function may return `FT_Err_Unimplemented_Feature` if your build
   *   of FreeType was not compiled with LZW support.
   */
   FT_Error 
  FT_Stream_OpenLZW( FT_Stream  stream,
                     FT_Stream  source );

  /* */




#endif /* FTLZW_H_ */


/* END */
//
// ===========================  ftcid.h  ===========================
//
/****************************************************************************
 *
 * ftcid.h
 *
 *   FreeType API for accessing CID font information (specification).
 *
 * Copyright (C) 2007-2024 by
 * Dereg Clegg and Michael Toftdal.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTCID_H_
#define FTCID_H_

#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   cid_fonts
   *
   * @title:
   *   CID Fonts
   *
   * @abstract:
   *   CID-keyed font-specific API.
   *
   * @description:
   *   This section contains the declaration of CID-keyed font-specific
   *   functions.
   *
   */


  /**************************************************************************
   *
   * @function:
   *    FT_Get_CID_Registry_Ordering_Supplement
   *
   * @description:
   *    Retrieve the Registry/Ordering/Supplement triple (also known as the
   *    "R/O/S") from a CID-keyed font.
   *
   * @input:
   *    face ::
   *      A handle to the input face.
   *
   * @output:
   *    registry ::
   *      The registry, as a C~string, owned by the face.
   *
   *    ordering ::
   *      The ordering, as a C~string, owned by the face.
   *
   *    supplement ::
   *      The supplement.
   *
   * @return:
   *    FreeType error code.  0~means success.
   *
   * @note:
   *    This function only works with CID faces, returning an error
   *    otherwise.
   *
   * @since:
   *    2.3.6
   */
   FT_Error 
  FT_Get_CID_Registry_Ordering_Supplement( FT_Face       face,
                                           const char*  *registry,
                                           const char*  *ordering,
                                           FT_Int       *supplement );


  /**************************************************************************
   *
   * @function:
   *    FT_Get_CID_Is_Internally_CID_Keyed
   *
   * @description:
   *    Retrieve the type of the input face, CID keyed or not.  In contrast
   *    to the @FT_IS_CID_KEYED macro this function returns successfully also
   *    for CID-keyed fonts in an SFNT wrapper.
   *
   * @input:
   *    face ::
   *      A handle to the input face.
   *
   * @output:
   *    is_cid ::
   *      The type of the face as an @FT_Bool.
   *
   * @return:
   *    FreeType error code.  0~means success.
   *
   * @note:
   *    This function only works with CID faces and OpenType fonts, returning
   *    an error otherwise.
   *
   * @since:
   *    2.3.9
   */
   FT_Error 
  FT_Get_CID_Is_Internally_CID_Keyed( FT_Face   face,
                                      FT_Bool  *is_cid );


  /**************************************************************************
   *
   * @function:
   *    FT_Get_CID_From_Glyph_Index
   *
   * @description:
   *    Retrieve the CID of the input glyph index.
   *
   * @input:
   *    face ::
   *      A handle to the input face.
   *
   *    glyph_index ::
   *      The input glyph index.
   *
   * @output:
   *    cid ::
   *      The CID as an @FT_UInt.
   *
   * @return:
   *    FreeType error code.  0~means success.
   *
   * @note:
   *    This function only works with CID faces and OpenType fonts, returning
   *    an error otherwise.
   *
   * @since:
   *    2.3.9
   */
   FT_Error 
  FT_Get_CID_From_Glyph_Index( FT_Face   face,
                               FT_UInt   glyph_index,
                               FT_UInt  *cid );

  /* */




#endif /* FTCID_H_ */


/* END */
//
// ===========================  tttables.h  ===========================
//
/****************************************************************************
 *
 * tttables.h
 *
 *   Basic SFNT/TrueType tables definitions and interface
 *   (specification only).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef TTTABLES_H_
#define TTTABLES_H_


#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif




  /**************************************************************************
   *
   * @section:
   *   truetype_tables
   *
   * @title:
   *   TrueType Tables
   *
   * @abstract:
   *   TrueType-specific table types and functions.
   *
   * @description:
   *   This section contains definitions of some basic tables specific to
   *   TrueType and OpenType as well as some routines used to access and
   *   process them.
   *
   * @order:
   *   TT_Header
   *   TT_HoriHeader
   *   TT_VertHeader
   *   TT_OS2
   *   TT_Postscript
   *   TT_PCLT
   *   TT_MaxProfile
   *
   *   FT_Sfnt_Tag
   *   FT_Get_Sfnt_Table
   *   FT_Load_Sfnt_Table
   *   FT_Sfnt_Table_Info
   *
   *   FT_Get_CMap_Language_ID
   *   FT_Get_CMap_Format
   *
   *   FT_PARAM_TAG_UNPATENTED_HINTING
   *
   */


  /**************************************************************************
   *
   * @struct:
   *   TT_Header
   *
   * @description:
   *   A structure to model a TrueType font header table.  All fields follow
   *   the OpenType specification.  The 64-bit timestamps are stored in
   *   two-element arrays `Created` and `Modified`, first the upper then
   *   the lower 32~bits.
   */
  typedef struct  TT_Header_
  {
    FT_Fixed   Table_Version;
    FT_Fixed   Font_Revision;

    FT_Long    CheckSum_Adjust;
    FT_Long    Magic_Number;

    FT_UShort  Flags;
    FT_UShort  Units_Per_EM;

    FT_ULong   Created [2];
    FT_ULong   Modified[2];

    FT_Short   xMin;
    FT_Short   yMin;
    FT_Short   xMax;
    FT_Short   yMax;

    FT_UShort  Mac_Style;
    FT_UShort  Lowest_Rec_PPEM;

    FT_Short   Font_Direction;
    FT_Short   Index_To_Loc_Format;
    FT_Short   Glyph_Data_Format;

  } TT_Header;


  /**************************************************************************
   *
   * @struct:
   *   TT_HoriHeader
   *
   * @description:
   *   A structure to model a TrueType horizontal header, the 'hhea' table,
   *   as well as the corresponding horizontal metrics table, 'hmtx'.
   *
   * @fields:
   *   Version ::
   *     The table version.
   *
   *   Ascender ::
   *     The font's ascender, i.e., the distance from the baseline to the
   *     top-most of all glyph points found in the font.
   *
   *     This value is invalid in many fonts, as it is usually set by the
   *     font designer, and often reflects only a portion of the glyphs found
   *     in the font (maybe ASCII).
   *
   *     You should use the `sTypoAscender` field of the 'OS/2' table instead
   *     if you want the correct one.
   *
   *   Descender ::
   *     The font's descender, i.e., the distance from the baseline to the
   *     bottom-most of all glyph points found in the font.  It is negative.
   *
   *     This value is invalid in many fonts, as it is usually set by the
   *     font designer, and often reflects only a portion of the glyphs found
   *     in the font (maybe ASCII).
   *
   *     You should use the `sTypoDescender` field of the 'OS/2' table
   *     instead if you want the correct one.
   *
   *   Line_Gap ::
   *     The font's line gap, i.e., the distance to add to the ascender and
   *     descender to get the BTB, i.e., the baseline-to-baseline distance
   *     for the font.
   *
   *   advance_Width_Max ::
   *     This field is the maximum of all advance widths found in the font.
   *     It can be used to compute the maximum width of an arbitrary string
   *     of text.
   *
   *   min_Left_Side_Bearing ::
   *     The minimum left side bearing of all glyphs within the font.
   *
   *   min_Right_Side_Bearing ::
   *     The minimum right side bearing of all glyphs within the font.
   *
   *   xMax_Extent ::
   *     The maximum horizontal extent (i.e., the 'width' of a glyph's
   *     bounding box) for all glyphs in the font.
   *
   *   caret_Slope_Rise ::
   *     The rise coefficient of the cursor's slope of the cursor
   *     (slope=rise/run).
   *
   *   caret_Slope_Run ::
   *     The run coefficient of the cursor's slope.
   *
   *   caret_Offset ::
   *     The cursor's offset for slanted fonts.
   *
   *   Reserved ::
   *     8~reserved bytes.
   *
   *   metric_Data_Format ::
   *     Always~0.
   *
   *   number_Of_HMetrics ::
   *     Number of HMetrics entries in the 'hmtx' table -- this value can be
   *     smaller than the total number of glyphs in the font.
   *
   *   long_metrics ::
   *     A pointer into the 'hmtx' table.
   *
   *   short_metrics ::
   *     A pointer into the 'hmtx' table.
   *
   * @note:
   *   For an OpenType variation font, the values of the following fields can
   *   change after a call to @FT_Set_Var_Design_Coordinates (and friends) if
   *   the font contains an 'MVAR' table: `caret_Slope_Rise`,
   *   `caret_Slope_Run`, and `caret_Offset`.
   */
  typedef struct  TT_HoriHeader_
  {
    FT_Fixed   Version;
    FT_Short   Ascender;
    FT_Short   Descender;
    FT_Short   Line_Gap;

    FT_UShort  advance_Width_Max;      /* advance width maximum */

    FT_Short   min_Left_Side_Bearing;  /* minimum left-sb       */
    FT_Short   min_Right_Side_Bearing; /* minimum right-sb      */
    FT_Short   xMax_Extent;            /* xmax extents          */
    FT_Short   caret_Slope_Rise;
    FT_Short   caret_Slope_Run;
    FT_Short   caret_Offset;

    FT_Short   Reserved[4];

    FT_Short   metric_Data_Format;
    FT_UShort  number_Of_HMetrics;

    /* The following fields are not defined by the OpenType specification */
    /* but they are used to connect the metrics header to the relevant    */
    /* 'hmtx' table.                                                      */

    void*      long_metrics;
    void*      short_metrics;

  } TT_HoriHeader;


  /**************************************************************************
   *
   * @struct:
   *   TT_VertHeader
   *
   * @description:
   *   A structure used to model a TrueType vertical header, the 'vhea'
   *   table, as well as the corresponding vertical metrics table, 'vmtx'.
   *
   * @fields:
   *   Version ::
   *     The table version.
   *
   *   Ascender ::
   *     The font's ascender, i.e., the distance from the baseline to the
   *     top-most of all glyph points found in the font.
   *
   *     This value is invalid in many fonts, as it is usually set by the
   *     font designer, and often reflects only a portion of the glyphs found
   *     in the font (maybe ASCII).
   *
   *     You should use the `sTypoAscender` field of the 'OS/2' table instead
   *     if you want the correct one.
   *
   *   Descender ::
   *     The font's descender, i.e., the distance from the baseline to the
   *     bottom-most of all glyph points found in the font.  It is negative.
   *
   *     This value is invalid in many fonts, as it is usually set by the
   *     font designer, and often reflects only a portion of the glyphs found
   *     in the font (maybe ASCII).
   *
   *     You should use the `sTypoDescender` field of the 'OS/2' table
   *     instead if you want the correct one.
   *
   *   Line_Gap ::
   *     The font's line gap, i.e., the distance to add to the ascender and
   *     descender to get the BTB, i.e., the baseline-to-baseline distance
   *     for the font.
   *
   *   advance_Height_Max ::
   *     This field is the maximum of all advance heights found in the font.
   *     It can be used to compute the maximum height of an arbitrary string
   *     of text.
   *
   *   min_Top_Side_Bearing ::
   *     The minimum top side bearing of all glyphs within the font.
   *
   *   min_Bottom_Side_Bearing ::
   *     The minimum bottom side bearing of all glyphs within the font.
   *
   *   yMax_Extent ::
   *     The maximum vertical extent (i.e., the 'height' of a glyph's
   *     bounding box) for all glyphs in the font.
   *
   *   caret_Slope_Rise ::
   *     The rise coefficient of the cursor's slope of the cursor
   *     (slope=rise/run).
   *
   *   caret_Slope_Run ::
   *     The run coefficient of the cursor's slope.
   *
   *   caret_Offset ::
   *     The cursor's offset for slanted fonts.
   *
   *   Reserved ::
   *     8~reserved bytes.
   *
   *   metric_Data_Format ::
   *     Always~0.
   *
   *   number_Of_VMetrics ::
   *     Number of VMetrics entries in the 'vmtx' table -- this value can be
   *     smaller than the total number of glyphs in the font.
   *
   *   long_metrics ::
   *     A pointer into the 'vmtx' table.
   *
   *   short_metrics ::
   *     A pointer into the 'vmtx' table.
   *
   * @note:
   *   For an OpenType variation font, the values of the following fields can
   *   change after a call to @FT_Set_Var_Design_Coordinates (and friends) if
   *   the font contains an 'MVAR' table: `Ascender`, `Descender`,
   *   `Line_Gap`, `caret_Slope_Rise`, `caret_Slope_Run`, and `caret_Offset`.
   */
  typedef struct  TT_VertHeader_
  {
    FT_Fixed   Version;
    FT_Short   Ascender;
    FT_Short   Descender;
    FT_Short   Line_Gap;

    FT_UShort  advance_Height_Max;      /* advance height maximum */

    FT_Short   min_Top_Side_Bearing;    /* minimum top-sb          */
    FT_Short   min_Bottom_Side_Bearing; /* minimum bottom-sb       */
    FT_Short   yMax_Extent;             /* ymax extents            */
    FT_Short   caret_Slope_Rise;
    FT_Short   caret_Slope_Run;
    FT_Short   caret_Offset;

    FT_Short   Reserved[4];

    FT_Short   metric_Data_Format;
    FT_UShort  number_Of_VMetrics;

    /* The following fields are not defined by the OpenType specification */
    /* but they are used to connect the metrics header to the relevant    */
    /* 'vmtx' table.                                                      */

    void*      long_metrics;
    void*      short_metrics;

  } TT_VertHeader;


  /**************************************************************************
   *
   * @struct:
   *   TT_OS2
   *
   * @description:
   *   A structure to model a TrueType 'OS/2' table.  All fields comply to
   *   the OpenType specification.
   *
   *   Note that we now support old Mac fonts that do not include an 'OS/2'
   *   table.  In this case, the `version` field is always set to 0xFFFF.
   *
   * @note:
   *   For an OpenType variation font, the values of the following fields can
   *   change after a call to @FT_Set_Var_Design_Coordinates (and friends) if
   *   the font contains an 'MVAR' table: `sCapHeight`, `sTypoAscender`,
   *   `sTypoDescender`, `sTypoLineGap`, `sxHeight`, `usWinAscent`,
   *   `usWinDescent`, `yStrikeoutPosition`, `yStrikeoutSize`,
   *   `ySubscriptXOffset`, `ySubScriptXSize`, `ySubscriptYOffset`,
   *   `ySubscriptYSize`, `ySuperscriptXOffset`, `ySuperscriptXSize`,
   *   `ySuperscriptYOffset`, and `ySuperscriptYSize`.
   *
   *   Possible values for bits in the `ulUnicodeRangeX` fields are given by
   *   the @TT_UCR_XXX macros.
   */

  typedef struct  TT_OS2_
  {
    FT_UShort  version;                /* 0x0001 - more or 0xFFFF */
    FT_Short   xAvgCharWidth;
    FT_UShort  usWeightClass;
    FT_UShort  usWidthClass;
    FT_UShort  fsType;
    FT_Short   ySubscriptXSize;
    FT_Short   ySubscriptYSize;
    FT_Short   ySubscriptXOffset;
    FT_Short   ySubscriptYOffset;
    FT_Short   ySuperscriptXSize;
    FT_Short   ySuperscriptYSize;
    FT_Short   ySuperscriptXOffset;
    FT_Short   ySuperscriptYOffset;
    FT_Short   yStrikeoutSize;
    FT_Short   yStrikeoutPosition;
    FT_Short   sFamilyClass;

    FT_Byte    panose[10];

    FT_ULong   ulUnicodeRange1;        /* Bits 0-31   */
    FT_ULong   ulUnicodeRange2;        /* Bits 32-63  */
    FT_ULong   ulUnicodeRange3;        /* Bits 64-95  */
    FT_ULong   ulUnicodeRange4;        /* Bits 96-127 */

    FT_Char    achVendID[4];

    FT_UShort  fsSelection;
    FT_UShort  usFirstCharIndex;
    FT_UShort  usLastCharIndex;
    FT_Short   sTypoAscender;
    FT_Short   sTypoDescender;
    FT_Short   sTypoLineGap;
    FT_UShort  usWinAscent;
    FT_UShort  usWinDescent;

    /* only version 1 and higher: */

    FT_ULong   ulCodePageRange1;       /* Bits 0-31   */
    FT_ULong   ulCodePageRange2;       /* Bits 32-63  */

    /* only version 2 and higher: */

    FT_Short   sxHeight;
    FT_Short   sCapHeight;
    FT_UShort  usDefaultChar;
    FT_UShort  usBreakChar;
    FT_UShort  usMaxContext;

    /* only version 5 and higher: */

    FT_UShort  usLowerOpticalPointSize;       /* in twips (1/20 points) */
    FT_UShort  usUpperOpticalPointSize;       /* in twips (1/20 points) */

  } TT_OS2;


  /**************************************************************************
   *
   * @struct:
   *   TT_Postscript
   *
   * @description:
   *   A structure to model a TrueType 'post' table.  All fields comply to
   *   the OpenType specification.  This structure does not reference a
   *   font's PostScript glyph names; use @FT_Get_Glyph_Name to retrieve
   *   them.
   *
   * @note:
   *   For an OpenType variation font, the values of the following fields can
   *   change after a call to @FT_Set_Var_Design_Coordinates (and friends) if
   *   the font contains an 'MVAR' table: `underlinePosition` and
   *   `underlineThickness`.
   */
  typedef struct  TT_Postscript_
  {
    FT_Fixed  FormatType;
    FT_Fixed  italicAngle;
    FT_Short  underlinePosition;
    FT_Short  underlineThickness;
    FT_ULong  isFixedPitch;
    FT_ULong  minMemType42;
    FT_ULong  maxMemType42;
    FT_ULong  minMemType1;
    FT_ULong  maxMemType1;

    /* Glyph names follow in the 'post' table, but we don't */
    /* load them by default.                                */

  } TT_Postscript;


  /**************************************************************************
   *
   * @struct:
   *   TT_PCLT
   *
   * @description:
   *   A structure to model a TrueType 'PCLT' table.  All fields comply to
   *   the OpenType specification.
   */
  typedef struct  TT_PCLT_
  {
    FT_Fixed   Version;
    FT_ULong   FontNumber;
    FT_UShort  Pitch;
    FT_UShort  xHeight;
    FT_UShort  Style;
    FT_UShort  TypeFamily;
    FT_UShort  CapHeight;
    FT_UShort  SymbolSet;
    FT_Char    TypeFace[16];
    FT_Char    CharacterComplement[8];
    FT_Char    FileName[6];
    FT_Char    StrokeWeight;
    FT_Char    WidthType;
    FT_Byte    SerifStyle;
    FT_Byte    Reserved;

  } TT_PCLT;


  /**************************************************************************
   *
   * @struct:
   *   TT_MaxProfile
   *
   * @description:
   *   The maximum profile ('maxp') table contains many max values, which can
   *   be used to pre-allocate arrays for speeding up glyph loading and
   *   hinting.
   *
   * @fields:
   *   version ::
   *     The version number.
   *
   *   numGlyphs ::
   *     The number of glyphs in this TrueType font.
   *
   *   maxPoints ::
   *     The maximum number of points in a non-composite TrueType glyph.  See
   *     also `maxCompositePoints`.
   *
   *   maxContours ::
   *     The maximum number of contours in a non-composite TrueType glyph.
   *     See also `maxCompositeContours`.
   *
   *   maxCompositePoints ::
   *     The maximum number of points in a composite TrueType glyph.  See
   *     also `maxPoints`.
   *
   *   maxCompositeContours ::
   *     The maximum number of contours in a composite TrueType glyph.  See
   *     also `maxContours`.
   *
   *   maxZones ::
   *     The maximum number of zones used for glyph hinting.
   *
   *   maxTwilightPoints ::
   *     The maximum number of points in the twilight zone used for glyph
   *     hinting.
   *
   *   maxStorage ::
   *     The maximum number of elements in the storage area used for glyph
   *     hinting.
   *
   *   maxFunctionDefs ::
   *     The maximum number of function definitions in the TrueType bytecode
   *     for this font.
   *
   *   maxInstructionDefs ::
   *     The maximum number of instruction definitions in the TrueType
   *     bytecode for this font.
   *
   *   maxStackElements ::
   *     The maximum number of stack elements used during bytecode
   *     interpretation.
   *
   *   maxSizeOfInstructions ::
   *     The maximum number of TrueType opcodes used for glyph hinting.
   *
   *   maxComponentElements ::
   *     The maximum number of simple (i.e., non-composite) glyphs in a
   *     composite glyph.
   *
   *   maxComponentDepth ::
   *     The maximum nesting depth of composite glyphs.
   *
   * @note:
   *   This structure is only used during font loading.
   */
  typedef struct  TT_MaxProfile_
  {
    FT_Fixed   version;
    FT_UShort  numGlyphs;
    FT_UShort  maxPoints;
    FT_UShort  maxContours;
    FT_UShort  maxCompositePoints;
    FT_UShort  maxCompositeContours;
    FT_UShort  maxZones;
    FT_UShort  maxTwilightPoints;
    FT_UShort  maxStorage;
    FT_UShort  maxFunctionDefs;
    FT_UShort  maxInstructionDefs;
    FT_UShort  maxStackElements;
    FT_UShort  maxSizeOfInstructions;
    FT_UShort  maxComponentElements;
    FT_UShort  maxComponentDepth;

  } TT_MaxProfile;


  /**************************************************************************
   *
   * @enum:
   *   FT_Sfnt_Tag
   *
   * @description:
   *   An enumeration to specify indices of SFNT tables loaded and parsed by
   *   FreeType during initialization of an SFNT font.  Used in the
   *   @FT_Get_Sfnt_Table API function.
   *
   * @values:
   *   FT_SFNT_HEAD ::
   *     To access the font's @TT_Header structure.
   *
   *   FT_SFNT_MAXP ::
   *     To access the font's @TT_MaxProfile structure.
   *
   *   FT_SFNT_OS2 ::
   *     To access the font's @TT_OS2 structure.
   *
   *   FT_SFNT_HHEA ::
   *     To access the font's @TT_HoriHeader structure.
   *
   *   FT_SFNT_VHEA ::
   *     To access the font's @TT_VertHeader structure.
   *
   *   FT_SFNT_POST ::
   *     To access the font's @TT_Postscript structure.
   *
   *   FT_SFNT_PCLT ::
   *     To access the font's @TT_PCLT structure.
   */
  typedef enum  FT_Sfnt_Tag_
  {
    FT_SFNT_HEAD,
    FT_SFNT_MAXP,
    FT_SFNT_OS2,
    FT_SFNT_HHEA,
    FT_SFNT_VHEA,
    FT_SFNT_POST,
    FT_SFNT_PCLT,

    FT_SFNT_MAX

  } FT_Sfnt_Tag;

  /* these constants are deprecated; use the corresponding `FT_Sfnt_Tag` */
  /* values instead                                                      */
#define ft_sfnt_head  FT_SFNT_HEAD
#define ft_sfnt_maxp  FT_SFNT_MAXP
#define ft_sfnt_os2   FT_SFNT_OS2
#define ft_sfnt_hhea  FT_SFNT_HHEA
#define ft_sfnt_vhea  FT_SFNT_VHEA
#define ft_sfnt_post  FT_SFNT_POST
#define ft_sfnt_pclt  FT_SFNT_PCLT


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Sfnt_Table
   *
   * @description:
   *   Return a pointer to a given SFNT table stored within a face.
   *
   * @input:
   *   face ::
   *     A handle to the source.
   *
   *   tag ::
   *     The index of the SFNT table.
   *
   * @return:
   *   A type-less pointer to the table.  This will be `NULL` in case of
   *   error, or if the corresponding table was not found **OR** loaded from
   *   the file.
   *
   *   Use a typecast according to `tag` to access the structure elements.
   *
   * @note:
   *   The table is owned by the face object and disappears with it.
   *
   *   This function is only useful to access SFNT tables that are loaded by
   *   the sfnt, truetype, and opentype drivers.  See @FT_Sfnt_Tag for a
   *   list.
   *
   * @example:
   *   Here is an example demonstrating access to the 'vhea' table.
   *
   *   ```
   *     TT_VertHeader*  vert_header;
   *
   *
   *     vert_header =
   *       (TT_VertHeader*)FT_Get_Sfnt_Table( face, FT_SFNT_VHEA );
   *   ```
   */
   void* 
  FT_Get_Sfnt_Table( FT_Face      face,
                     FT_Sfnt_Tag  tag );


  /**************************************************************************
   *
   * @function:
   *   FT_Load_Sfnt_Table
   *
   * @description:
   *   Load any SFNT font table into client memory.
   *
   * @input:
   *   face ::
   *     A handle to the source face.
   *
   *   tag ::
   *     The four-byte tag of the table to load.  Use value~0 if you want to
   *     access the whole font file.  Otherwise, you can use one of the
   *     definitions found in the @FT_TRUETYPE_TAGS_H file, or forge a new
   *     one with @FT_MAKE_TAG.
   *
   *   offset ::
   *     The starting offset in the table (or file if tag~==~0).
   *
   * @output:
   *   buffer ::
   *     The target buffer address.  The client must ensure that the memory
   *     array is big enough to hold the data.
   *
   * @inout:
   *   length ::
   *     If the `length` parameter is `NULL`, try to load the whole table.
   *     Return an error code if it fails.
   *
   *     Else, if `*length` is~0, exit immediately while returning the
   *     table's (or file) full size in it.
   *
   *     Else the number of bytes to read from the table or file, from the
   *     starting offset.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   If you need to determine the table's length you should first call this
   *   function with `*length` set to~0, as in the following example:
   *
   *   ```
   *     FT_ULong  length = 0;
   *
   *
   *     error = FT_Load_Sfnt_Table( face, tag, 0, NULL, &length );
   *     if ( error ) { ... table does not exist ... }
   *
   *     buffer = malloc( length );
   *     if ( buffer == NULL ) { ... not enough memory ... }
   *
   *     error = FT_Load_Sfnt_Table( face, tag, 0, buffer, &length );
   *     if ( error ) { ... could not load table ... }
   *   ```
   *
   *   Note that structures like @TT_Header or @TT_OS2 can't be used with
   *   this function; they are limited to @FT_Get_Sfnt_Table.  Reason is that
   *   those structures depend on the processor architecture, with varying
   *   size (e.g. 32bit vs. 64bit) or order (big endian vs. little endian).
   *
   */
   FT_Error 
  FT_Load_Sfnt_Table( FT_Face    face,
                      FT_ULong   tag,
                      FT_Long    offset,
                      FT_Byte*   buffer,
                      FT_ULong*  length );


  /**************************************************************************
   *
   * @function:
   *   FT_Sfnt_Table_Info
   *
   * @description:
   *   Return information on an SFNT table.
   *
   * @input:
   *   face ::
   *     A handle to the source face.
   *
   *   table_index ::
   *     The index of an SFNT table.  The function returns
   *     FT_Err_Table_Missing for an invalid value.
   *
   * @inout:
   *   tag ::
   *     The name tag of the SFNT table.  If the value is `NULL`,
   *     `table_index` is ignored, and `length` returns the number of SFNT
   *     tables in the font.
   *
   * @output:
   *   length ::
   *     The length of the SFNT table (or the number of SFNT tables,
   *     depending on `tag`).
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   While parsing fonts, FreeType handles SFNT tables with length zero as
   *   missing.
   *
   */
   FT_Error 
  FT_Sfnt_Table_Info( FT_Face    face,
                      FT_UInt    table_index,
                      FT_ULong  *tag,
                      FT_ULong  *length );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_CMap_Language_ID
   *
   * @description:
   *   Return cmap language ID as specified in the OpenType standard.
   *   Definitions of language ID values are in file @FT_TRUETYPE_IDS_H.
   *
   * @input:
   *   charmap ::
   *     The target charmap.
   *
   * @return:
   *   The language ID of `charmap`.  If `charmap` doesn't belong to an SFNT
   *   face, just return~0 as the default value.
   *
   *   For a format~14 cmap (to access Unicode IVS), the return value is
   *   0xFFFFFFFF.
   */
   FT_ULong 
  FT_Get_CMap_Language_ID( FT_CharMap  charmap );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_CMap_Format
   *
   * @description:
   *   Return the format of an SFNT 'cmap' table.
   *
   * @input:
   *   charmap ::
   *     The target charmap.
   *
   * @return:
   *   The format of `charmap`.  If `charmap` doesn't belong to an SFNT face,
   *   return -1.
   */
   FT_Long 
  FT_Get_CMap_Format( FT_CharMap  charmap );

  /* */




#endif /* TTTABLES_H_ */


/* END */
//
// ===========================  ftbzip2.h  ===========================
//
/****************************************************************************
 *
 * ftbzip2.h
 *
 *   Bzip2-compressed stream support.
 *
 * Copyright (C) 2010-2024 by
 * Joel Klinghed.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTBZIP2_H_
#define FTBZIP2_H_

#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif




  /**************************************************************************
   *
   * @section:
   *   bzip2
   *
   * @title:
   *   BZIP2 Streams
   *
   * @abstract:
   *   Using bzip2-compressed font files.
   *
   * @description:
   *   In certain builds of the library, bzip2 compression recognition is
   *   automatically handled when calling @FT_New_Face or @FT_Open_Face.
   *   This means that if no font driver is capable of handling the raw
   *   compressed file, the library will try to open a bzip2 compressed
   *   stream from it and re-open the face with it.
   *
   *   The stream implementation is very basic and resets the decompression
   *   process each time seeking backwards is needed within the stream,
   *   which significantly undermines the performance.
   *
   *   This section contains the declaration of Bzip2-specific functions.
   *
   */


  /**************************************************************************
   *
   * @function:
   *   FT_Stream_OpenBzip2
   *
   * @description:
   *   Open a new stream to parse bzip2-compressed font files.  This is
   *   mainly used to support the compressed `*.pcf.bz2` fonts that come with
   *   XFree86.
   *
   * @input:
   *   stream ::
   *     The target embedding stream.
   *
   *   source ::
   *     The source stream.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The source stream must be opened _before_ calling this function.
   *
   *   Calling the internal function `FT_Stream_Close` on the new stream will
   *   **not** call `FT_Stream_Close` on the source stream.  None of the
   *   stream objects will be released to the heap.
   *
   *   This function may return `FT_Err_Unimplemented_Feature` if your build
   *   of FreeType was not compiled with bzip2 support.
   */
   FT_Error 
  FT_Stream_OpenBzip2( FT_Stream  stream,
                       FT_Stream  source );

  /* */




#endif /* FTBZIP2_H_ */


/* END */
//
// ===========================  ftbitmap.h  ===========================
//
/****************************************************************************
 *
 * ftbitmap.h
 *
 *   FreeType utility functions for bitmaps (specification).
 *
 * Copyright (C) 2004-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTBITMAP_H_
#define FTBITMAP_H_


#include <freetype/freetype.h>
#include <freetype/ftcolor.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   bitmap_handling
   *
   * @title:
   *   Bitmap Handling
   *
   * @abstract:
   *   Handling FT_Bitmap objects.
   *
   * @description:
   *   This section contains functions for handling @FT_Bitmap objects,
   *   automatically adjusting the target's bitmap buffer size as needed.
   *
   *   Note that none of the functions changes the bitmap's 'flow' (as
   *   indicated by the sign of the `pitch` field in @FT_Bitmap).
   *
   *   To set the flow, assign an appropriate positive or negative value to
   *   the `pitch` field of the target @FT_Bitmap object after calling
   *   @FT_Bitmap_Init but before calling any of the other functions
   *   described here.
   */


  /**************************************************************************
   *
   * @function:
   *   FT_Bitmap_Init
   *
   * @description:
   *   Initialize a pointer to an @FT_Bitmap structure.
   *
   * @inout:
   *   abitmap ::
   *     A pointer to the bitmap structure.
   *
   * @note:
   *   A deprecated name for the same function is `FT_Bitmap_New`.
   */
   void 
  FT_Bitmap_Init( FT_Bitmap  *abitmap );


  /* deprecated */
   void 
  FT_Bitmap_New( FT_Bitmap  *abitmap );


  /**************************************************************************
   *
   * @function:
   *   FT_Bitmap_Copy
   *
   * @description:
   *   Copy a bitmap into another one.
   *
   * @input:
   *   library ::
   *     A handle to a library object.
   *
   *   source ::
   *     A handle to the source bitmap.
   *
   * @output:
   *   target ::
   *     A handle to the target bitmap.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   `source->buffer` and `target->buffer` must neither be equal nor
   *   overlap.
   */
   FT_Error 
  FT_Bitmap_Copy( FT_Library        library,
                  const FT_Bitmap  *source,
                  FT_Bitmap        *target );


  /**************************************************************************
   *
   * @function:
   *   FT_Bitmap_Embolden
   *
   * @description:
   *   Embolden a bitmap.  The new bitmap will be about `xStrength` pixels
   *   wider and `yStrength` pixels higher.  The left and bottom borders are
   *   kept unchanged.
   *
   * @input:
   *   library ::
   *     A handle to a library object.
   *
   *   xStrength ::
   *     How strong the glyph is emboldened horizontally.  Expressed in 26.6
   *     pixel format.
   *
   *   yStrength ::
   *     How strong the glyph is emboldened vertically.  Expressed in 26.6
   *     pixel format.
   *
   * @inout:
   *   bitmap ::
   *     A handle to the target bitmap.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The current implementation restricts `xStrength` to be less than or
   *   equal to~8 if bitmap is of pixel_mode @FT_PIXEL_MODE_MONO.
   *
   *   If you want to embolden the bitmap owned by a @FT_GlyphSlotRec, you
   *   should call @FT_GlyphSlot_Own_Bitmap on the slot first.
   *
   *   Bitmaps in @FT_PIXEL_MODE_GRAY2 and @FT_PIXEL_MODE_GRAY@ format are
   *   converted to @FT_PIXEL_MODE_GRAY format (i.e., 8bpp).
   */
   FT_Error 
  FT_Bitmap_Embolden( FT_Library  library,
                      FT_Bitmap*  bitmap,
                      FT_Pos      xStrength,
                      FT_Pos      yStrength );


  /**************************************************************************
   *
   * @function:
   *   FT_Bitmap_Convert
   *
   * @description:
   *   Convert a bitmap object with depth 1bpp, 2bpp, 4bpp, 8bpp or 32bpp to
   *   a bitmap object with depth 8bpp, making the number of used bytes per
   *   line (a.k.a. the 'pitch') a multiple of `alignment`.
   *
   * @input:
   *   library ::
   *     A handle to a library object.
   *
   *   source ::
   *     The source bitmap.
   *
   *   alignment ::
   *     The pitch of the bitmap is a multiple of this argument.  Common
   *     values are 1, 2, or 4.
   *
   * @output:
   *   target ::
   *     The target bitmap.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   It is possible to call @FT_Bitmap_Convert multiple times without
   *   calling @FT_Bitmap_Done (the memory is simply reallocated).
   *
   *   Use @FT_Bitmap_Done to finally remove the bitmap object.
   *
   *   The `library` argument is taken to have access to FreeType's memory
   *   handling functions.
   *
   *   `source->buffer` and `target->buffer` must neither be equal nor
   *   overlap.
   */
   FT_Error 
  FT_Bitmap_Convert( FT_Library        library,
                     const FT_Bitmap  *source,
                     FT_Bitmap        *target,
                     FT_Int            alignment );


  /**************************************************************************
   *
   * @function:
   *   FT_Bitmap_Blend
   *
   * @description:
   *   Blend a bitmap onto another bitmap, using a given color.
   *
   * @input:
   *   library ::
   *     A handle to a library object.
   *
   *   source ::
   *     The source bitmap, which can have any @FT_Pixel_Mode format.
   *
   *   source_offset ::
   *     The offset vector to the upper left corner of the source bitmap in
   *     26.6 pixel format.  It should represent an integer offset; the
   *     function will set the lowest six bits to zero to enforce that.
   *
   *   color ::
   *     The color used to draw `source` onto `target`.
   *
   * @inout:
   *   target ::
   *     A handle to an `FT_Bitmap` object.  It should be either initialized
   *     as empty with a call to @FT_Bitmap_Init, or it should be of type
   *     @FT_PIXEL_MODE_BGRA.
   *
   *   atarget_offset ::
   *     The offset vector to the upper left corner of the target bitmap in
   *     26.6 pixel format.  It should represent an integer offset; the
   *     function will set the lowest six bits to zero to enforce that.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function doesn't perform clipping.
   *
   *   The bitmap in `target` gets allocated or reallocated as needed; the
   *   vector `atarget_offset` is updated accordingly.
   *
   *   In case of allocation or reallocation, the bitmap's pitch is set to
   *   `4 * width`.  Both `source` and `target` must have the same bitmap
   *   flow (as indicated by the sign of the `pitch` field).
   *
   *   `source->buffer` and `target->buffer` must neither be equal nor
   *   overlap.
   *
   * @since:
   *   2.10
   */
   FT_Error 
  FT_Bitmap_Blend( FT_Library         library,
                   const FT_Bitmap*   source,
                   const FT_Vector    source_offset,
                   FT_Bitmap*         target,
                   FT_Vector         *atarget_offset,
                   FT_Color           color );


  /**************************************************************************
   *
   * @function:
   *   FT_GlyphSlot_Own_Bitmap
   *
   * @description:
   *   Make sure that a glyph slot owns `slot->bitmap`.
   *
   * @input:
   *   slot ::
   *     The glyph slot.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function is to be used in combination with @FT_Bitmap_Embolden.
   */
   FT_Error 
  FT_GlyphSlot_Own_Bitmap( FT_GlyphSlot  slot );


  /**************************************************************************
   *
   * @function:
   *   FT_Bitmap_Done
   *
   * @description:
   *   Destroy a bitmap object initialized with @FT_Bitmap_Init.
   *
   * @input:
   *   library ::
   *     A handle to a library object.
   *
   *   bitmap ::
   *     The bitmap object to be freed.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The `library` argument is taken to have access to FreeType's memory
   *   handling functions.
   */
   FT_Error 
  FT_Bitmap_Done( FT_Library  library,
                  FT_Bitmap  *bitmap );


  /* */




#endif /* FTBITMAP_H_ */


/* END */
//
// ===========================  ftmodapi.h  ===========================
//
/****************************************************************************
 *
 * ftmodapi.h
 *
 *   FreeType modules public interface (specification).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTMODAPI_H_
#define FTMODAPI_H_


#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   module_management
   *
   * @title:
   *   Module Management
   *
   * @abstract:
   *   How to add, upgrade, remove, and control modules from FreeType.
   *
   * @description:
   *   The definitions below are used to manage modules within FreeType.
   *   Internal and external modules can be added, upgraded, and removed at
   *   runtime.  For example, an alternative renderer or proprietary font
   *   driver can be registered and prioritized.  Additionally, some module
   *   properties can also be controlled.
   *
   *   Here is a list of existing values of the `module_name` field in the
   *   @FT_Module_Class structure.
   *
   *   ```
   *     autofitter
   *     bdf
   *     cff
   *     gxvalid
   *     otvalid
   *     pcf
   *     pfr
   *     psaux
   *     pshinter
   *     psnames
   *     raster1
   *     sfnt
   *     smooth
   *     truetype
   *     type1
   *     type42
   *     t1cid
   *     winfonts
   *   ```
   *
   *   Note that the FreeType Cache sub-system is not a FreeType module.
   *
   * @order:
   *   FT_Module
   *   FT_Module_Constructor
   *   FT_Module_Destructor
   *   FT_Module_Requester
   *   FT_Module_Class
   *
   *   FT_Add_Module
   *   FT_Get_Module
   *   FT_Remove_Module
   *   FT_Add_Default_Modules
   *
   *   FT_FACE_DRIVER_NAME
   *   FT_Property_Set
   *   FT_Property_Get
   *   FT_Set_Default_Properties
   *
   *   FT_New_Library
   *   FT_Done_Library
   *   FT_Reference_Library
   *
   *   FT_Renderer
   *   FT_Renderer_Class
   *
   *   FT_Get_Renderer
   *   FT_Set_Renderer
   *
   *   FT_Set_Debug_Hook
   *
   */


  /* module bit flags */
#define FT_MODULE_FONT_DRIVER         1  /* this module is a font driver  */
#define FT_MODULE_RENDERER            2  /* this module is a renderer     */
#define FT_MODULE_HINTER              4  /* this module is a glyph hinter */
#define FT_MODULE_STYLER              8  /* this module is a styler       */

#define FT_MODULE_DRIVER_SCALABLE      0x100  /* the driver supports      */
                                              /* scalable fonts           */
#define FT_MODULE_DRIVER_NO_OUTLINES   0x200  /* the driver does not      */
                                              /* support vector outlines  */
#define FT_MODULE_DRIVER_HAS_HINTER    0x400  /* the driver provides its  */
                                              /* own hinter               */
#define FT_MODULE_DRIVER_HINTS_LIGHTLY 0x800  /* the driver's hinter      */
                                              /* produces LIGHT hints     */


  /* deprecated values */
#define ft_module_font_driver         FT_MODULE_FONT_DRIVER
#define ft_module_renderer            FT_MODULE_RENDERER
#define ft_module_hinter              FT_MODULE_HINTER
#define ft_module_styler              FT_MODULE_STYLER

#define ft_module_driver_scalable       FT_MODULE_DRIVER_SCALABLE
#define ft_module_driver_no_outlines    FT_MODULE_DRIVER_NO_OUTLINES
#define ft_module_driver_has_hinter     FT_MODULE_DRIVER_HAS_HINTER
#define ft_module_driver_hints_lightly  FT_MODULE_DRIVER_HINTS_LIGHTLY


  typedef FT_Pointer  FT_Module_Interface;


  /**************************************************************************
   *
   * @functype:
   *   FT_Module_Constructor
   *
   * @description:
   *   A function used to initialize (not create) a new module object.
   *
   * @input:
   *   module ::
   *     The module to initialize.
   */
  typedef FT_Error
  (*FT_Module_Constructor)( FT_Module  module );


  /**************************************************************************
   *
   * @functype:
   *   FT_Module_Destructor
   *
   * @description:
   *   A function used to finalize (not destroy) a given module object.
   *
   * @input:
   *   module ::
   *     The module to finalize.
   */
  typedef void
  (*FT_Module_Destructor)( FT_Module  module );


  /**************************************************************************
   *
   * @functype:
   *   FT_Module_Requester
   *
   * @description:
   *   A function used to query a given module for a specific interface.
   *
   * @input:
   *   module ::
   *     The module to be searched.
   *
   *   name ::
   *     The name of the interface in the module.
   */
  typedef FT_Module_Interface
  (*FT_Module_Requester)( FT_Module    module,
                          const char*  name );


  /**************************************************************************
   *
   * @struct:
   *   FT_Module_Class
   *
   * @description:
   *   The module class descriptor.  While being a public structure necessary
   *   for FreeType's module bookkeeping, most of the fields are essentially
   *   internal, not to be used directly by an application.
   *
   * @fields:
   *   module_flags ::
   *     Bit flags describing the module.
   *
   *   module_size ::
   *     The size of one module object/instance in bytes.
   *
   *   module_name ::
   *     The name of the module.
   *
   *   module_version ::
   *     The version, as a 16.16 fixed number (major.minor).
   *
   *   module_requires ::
   *     The version of FreeType this module requires, as a 16.16 fixed
   *     number (major.minor).  Starts at version 2.0, i.e., 0x20000.
   *
   *   module_interface ::
   *     A typeless pointer to a structure (which varies between different
   *     modules) that holds the module's interface functions.  This is
   *     essentially what `get_interface` returns.
   *
   *   module_init ::
   *     The initializing function.
   *
   *   module_done ::
   *     The finalizing function.
   *
   *   get_interface ::
   *     The interface requesting function.
   */
  typedef struct  FT_Module_Class_
  {
    FT_ULong               module_flags;
    FT_Long                module_size;
    const FT_String*       module_name;
    FT_Fixed               module_version;
    FT_Fixed               module_requires;

    const void*            module_interface;

    FT_Module_Constructor  module_init;
    FT_Module_Destructor   module_done;
    FT_Module_Requester    get_interface;

  } FT_Module_Class;


  /**************************************************************************
   *
   * @function:
   *   FT_Add_Module
   *
   * @description:
   *   Add a new module to a given library instance.
   *
   * @inout:
   *   library ::
   *     A handle to the library object.
   *
   * @input:
   *   clazz ::
   *     A pointer to class descriptor for the module.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   An error will be returned if a module already exists by that name, or
   *   if the module requires a version of FreeType that is too great.
   */
   FT_Error 
  FT_Add_Module( FT_Library              library,
                 const FT_Module_Class*  clazz );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Module
   *
   * @description:
   *   Find a module by its name.
   *
   * @input:
   *   library ::
   *     A handle to the library object.
   *
   *   module_name ::
   *     The module's name (as an ASCII string).
   *
   * @return:
   *   A module handle.  0~if none was found.
   *
   * @note:
   *   FreeType's internal modules aren't documented very well, and you
   *   should look up the source code for details.
   */
   FT_Module 
  FT_Get_Module( FT_Library   library,
                 const char*  module_name );


  /**************************************************************************
   *
   * @function:
   *   FT_Remove_Module
   *
   * @description:
   *   Remove a given module from a library instance.
   *
   * @inout:
   *   library ::
   *     A handle to a library object.
   *
   * @input:
   *   module ::
   *     A handle to a module object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The module object is destroyed by the function in case of success.
   */
   FT_Error 
  FT_Remove_Module( FT_Library  library,
                    FT_Module   module );


  /**************************************************************************
   *
   * @macro:
   *   FT_FACE_DRIVER_NAME
   *
   * @description:
   *   A macro that retrieves the name of a font driver from a face object.
   *
   * @note:
   *   The font driver name is a valid `module_name` for @FT_Property_Set
   *   and @FT_Property_Get.  This is not the same as @FT_Get_Font_Format.
   *
   * @since:
   *   2.11
   *
   */
//#define FT_FACE_DRIVER_NAME( face )                                     \
//          ( ( *FT_REINTERPRET_CAST( FT_Module_Class**,                  \
//                                    ( face )->driver ) )->module_name )


  /**************************************************************************
   *
   * @function:
   *    FT_Property_Set
   *
   * @description:
   *    Set a property for a given module.
   *
   * @input:
   *    library ::
   *      A handle to the library the module is part of.
   *
   *    module_name ::
   *      The module name.
   *
   *    property_name ::
   *      The property name.  Properties are described in section
   *      @properties.
   *
   *      Note that only a few modules have properties.
   *
   *    value ::
   *      A generic pointer to a variable or structure that gives the new
   *      value of the property.  The exact definition of `value` is
   *      dependent on the property; see section @properties.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *    If `module_name` isn't a valid module name, or `property_name`
   *    doesn't specify a valid property, or if `value` doesn't represent a
   *    valid value for the given property, an error is returned.
   *
   *    The following example sets property 'bar' (a simple integer) in
   *    module 'foo' to value~1.
   *
   *    ```
   *      FT_UInt  bar;
   *
   *
   *      bar = 1;
   *      FT_Property_Set( library, "foo", "bar", &bar );
   *    ```
   *
   *    Note that the FreeType Cache sub-system doesn't recognize module
   *    property changes.  To avoid glyph lookup confusion within the cache
   *    you should call @FTC_Manager_Reset to completely flush the cache if a
   *    module property gets changed after @FTC_Manager_New has been called.
   *
   *    It is not possible to set properties of the FreeType Cache sub-system
   *    itself with FT_Property_Set; use @FTC_Property_Set instead.
   *
   * @since:
   *   2.4.11
   *
   */
   FT_Error 
  FT_Property_Set( FT_Library        library,
                   const FT_String*  module_name,
                   const FT_String*  property_name,
                   const void*       value );


  /**************************************************************************
   *
   * @function:
   *    FT_Property_Get
   *
   * @description:
   *    Get a module's property value.
   *
   * @input:
   *    library ::
   *      A handle to the library the module is part of.
   *
   *    module_name ::
   *      The module name.
   *
   *    property_name ::
   *      The property name.  Properties are described in section
   *      @properties.
   *
   * @inout:
   *    value ::
   *      A generic pointer to a variable or structure that gives the value
   *      of the property.  The exact definition of `value` is dependent on
   *      the property; see section @properties.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *    If `module_name` isn't a valid module name, or `property_name`
   *    doesn't specify a valid property, or if `value` doesn't represent a
   *    valid value for the given property, an error is returned.
   *
   *    The following example gets property 'baz' (a range) in module 'foo'.
   *
   *    ```
   *      typedef  range_
   *      {
   *        FT_Int32  min;
   *        FT_Int32  max;
   *
   *      } range;
   *
   *      range  baz;
   *
   *
   *      FT_Property_Get( library, "foo", "baz", &baz );
   *    ```
   *
   *    It is not possible to retrieve properties of the FreeType Cache
   *    sub-system with FT_Property_Get; use @FTC_Property_Get instead.
   *
   * @since:
   *   2.4.11
   *
   */
   FT_Error 
  FT_Property_Get( FT_Library        library,
                   const FT_String*  module_name,
                   const FT_String*  property_name,
                   void*             value );


  /**************************************************************************
   *
   * @function:
   *   FT_Set_Default_Properties
   *
   * @description:
   *   If compilation option `FT_CONFIG_OPTION_ENVIRONMENT_PROPERTIES` is
   *   set, this function reads the `FREETYPE_PROPERTIES` environment
   *   variable to control driver properties.  See section @properties for
   *   more.
   *
   *   If the compilation option is not set, this function does nothing.
   *
   *   `FREETYPE_PROPERTIES` has the following syntax form (broken here into
   *   multiple lines for better readability).
   *
   *   ```
   *     <optional whitespace>
   *     <module-name1> ':'
   *     <property-name1> '=' <property-value1>
   *     <whitespace>
   *     <module-name2> ':'
   *     <property-name2> '=' <property-value2>
   *     ...
   *   ```
   *
   *   Example:
   *
   *   ```
   *     FREETYPE_PROPERTIES=truetype:interpreter-version=35 \
   *                         cff:no-stem-darkening=0
   *   ```
   *
   * @inout:
   *   library ::
   *     A handle to a new library object.
   *
   * @since:
   *   2.8
   */
   void 
  FT_Set_Default_Properties( FT_Library  library );


  /**************************************************************************
   *
   * @function:
   *   FT_Reference_Library
   *
   * @description:
   *   A counter gets initialized to~1 at the time an @FT_Library structure
   *   is created.  This function increments the counter.  @FT_Done_Library
   *   then only destroys a library if the counter is~1, otherwise it simply
   *   decrements the counter.
   *
   *   This function helps in managing life-cycles of structures that
   *   reference @FT_Library objects.
   *
   * @input:
   *   library ::
   *     A handle to a target library object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @since:
   *   2.4.2
   */
   FT_Error 
  FT_Reference_Library( FT_Library  library );


  /**************************************************************************
   *
   * @function:
   *   FT_New_Library
   *
   * @description:
   *   This function is used to create a new FreeType library instance from a
   *   given memory object.  It is thus possible to use libraries with
   *   distinct memory allocators within the same program.  Note, however,
   *   that the used @FT_Memory structure is expected to remain valid for the
   *   life of the @FT_Library object.
   *
   *   Normally, you would call this function (followed by a call to
   *   @FT_Add_Default_Modules or a series of calls to @FT_Add_Module, and a
   *   call to @FT_Set_Default_Properties) instead of @FT_Init_FreeType to
   *   initialize the FreeType library.
   *
   *   Don't use @FT_Done_FreeType but @FT_Done_Library to destroy a library
   *   instance.
   *
   * @input:
   *   memory ::
   *     A handle to the original memory object.
   *
   * @output:
   *   alibrary ::
   *     A pointer to handle of a new library object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   See the discussion of reference counters in the description of
   *   @FT_Reference_Library.
   */
   FT_Error 
  FT_New_Library( FT_Memory    memory,
                  FT_Library  *alibrary );


  /**************************************************************************
   *
   * @function:
   *   FT_Done_Library
   *
   * @description:
   *   Discard a given library object.  This closes all drivers and discards
   *   all resource objects.
   *
   * @input:
   *   library ::
   *     A handle to the target library.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   See the discussion of reference counters in the description of
   *   @FT_Reference_Library.
   */
   FT_Error 
  FT_Done_Library( FT_Library  library );


  /**************************************************************************
   *
   * @functype:
   *   FT_DebugHook_Func
   *
   * @description:
   *   A drop-in replacement (or rather a wrapper) for the bytecode or
   *   charstring interpreter's main loop function.
   *
   *   Its job is essentially
   *
   *   - to activate debug mode to enforce single-stepping,
   *
   *   - to call the main loop function to interpret the next opcode, and
   *
   *   - to show the changed context to the user.
   *
   *   An example for such a main loop function is `TT_RunIns` (declared in
   *   FreeType's internal header file `src/truetype/ttinterp.h`).
   *
   *   Have a look at the source code of the `ttdebug` FreeType demo program
   *   for an example of a drop-in replacement.
   *
   * @inout:
   *   arg ::
   *     A typeless pointer, to be cast to the main loop function's data
   *     structure (which depends on the font module).  For TrueType fonts
   *     it is bytecode interpreter's execution context, `TT_ExecContext`,
   *     which is declared in FreeType's internal header file `tttypes.h`.
   */
  typedef FT_Error
  (*FT_DebugHook_Func)( void*  arg );


  /**************************************************************************
   *
   * @enum:
   *   FT_DEBUG_HOOK_XXX
   *
   * @description:
   *   A list of named debug hook indices.
   *
   * @values:
   *   FT_DEBUG_HOOK_TRUETYPE::
   *     This hook index identifies the TrueType bytecode debugger.
   */
#define FT_DEBUG_HOOK_TRUETYPE  0


  /**************************************************************************
   *
   * @function:
   *   FT_Set_Debug_Hook
   *
   * @description:
   *   Set a debug hook function for debugging the interpreter of a font
   *   format.
   *
   *   While this is a public API function, an application needs access to
   *   FreeType's internal header files to do something useful.
   *
   *   Have a look at the source code of the `ttdebug` FreeType demo program
   *   for an example of its usage.
   *
   * @inout:
   *   library ::
   *     A handle to the library object.
   *
   * @input:
   *   hook_index ::
   *     The index of the debug hook.  You should use defined enumeration
   *     macros like @FT_DEBUG_HOOK_TRUETYPE.
   *
   *   debug_hook ::
   *     The function used to debug the interpreter.
   *
   * @note:
   *   Currently, four debug hook slots are available, but only one (for the
   *   TrueType interpreter) is defined.
   */
   void 
  FT_Set_Debug_Hook( FT_Library         library,
                     FT_UInt            hook_index,
                     FT_DebugHook_Func  debug_hook );


  /**************************************************************************
   *
   * @function:
   *   FT_Add_Default_Modules
   *
   * @description:
   *   Add the set of default drivers to a given library object.  This is
   *   only useful when you create a library object with @FT_New_Library
   *   (usually to plug a custom memory manager).
   *
   * @inout:
   *   library ::
   *     A handle to a new library object.
   */
   void 
  FT_Add_Default_Modules( FT_Library  library );



  /**************************************************************************
   *
   * @section:
   *   truetype_engine
   *
   * @title:
   *   The TrueType Engine
   *
   * @abstract:
   *   TrueType bytecode support.
   *
   * @description:
   *   This section contains a function used to query the level of TrueType
   *   bytecode support compiled in this version of the library.
   *
   */


  /**************************************************************************
   *
   * @enum:
   *    FT_TrueTypeEngineType
   *
   * @description:
   *    A list of values describing which kind of TrueType bytecode engine is
   *    implemented in a given FT_Library instance.  It is used by the
   *    @FT_Get_TrueType_Engine_Type function.
   *
   * @values:
   *    FT_TRUETYPE_ENGINE_TYPE_NONE ::
   *      The library doesn't implement any kind of bytecode interpreter.
   *
   *    FT_TRUETYPE_ENGINE_TYPE_UNPATENTED ::
   *      Deprecated and removed.
   *
   *    FT_TRUETYPE_ENGINE_TYPE_PATENTED ::
   *      The library implements a bytecode interpreter that covers the full
   *      instruction set of the TrueType virtual machine (this was governed
   *      by patents until May 2010, hence the name).
   *
   * @since:
   *    2.2
   *
   */
  typedef enum  FT_TrueTypeEngineType_
  {
    FT_TRUETYPE_ENGINE_TYPE_NONE = 0,
    FT_TRUETYPE_ENGINE_TYPE_UNPATENTED,
    FT_TRUETYPE_ENGINE_TYPE_PATENTED

  } FT_TrueTypeEngineType;


  /**************************************************************************
   *
   * @function:
   *    FT_Get_TrueType_Engine_Type
   *
   * @description:
   *    Return an @FT_TrueTypeEngineType value to indicate which level of the
   *    TrueType virtual machine a given library instance supports.
   *
   * @input:
   *    library ::
   *      A library instance.
   *
   * @return:
   *    A value indicating which level is supported.
   *
   * @since:
   *    2.2
   *
   */
   FT_TrueTypeEngineType 
  FT_Get_TrueType_Engine_Type( FT_Library  library );

  /* */




#endif /* FTMODAPI_H_ */


/* END */
//
// ===========================  ftadvanc.h  ===========================
//
/****************************************************************************
 *
 * ftadvanc.h
 *
 *   Quick computation of advance widths (specification only).
 *
 * Copyright (C) 2008-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTADVANC_H_
#define FTADVANC_H_


#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   quick_advance
   *
   * @title:
   *   Quick retrieval of advance values
   *
   * @abstract:
   *   Retrieve horizontal and vertical advance values without processing
   *   glyph outlines, if possible.
   *
   * @description:
   *   This section contains functions to quickly extract advance values
   *   without handling glyph outlines, if possible.
   *
   * @order:
   *   FT_Get_Advance
   *   FT_Get_Advances
   *
   */


  /**************************************************************************
   *
   * @enum:
   *   FT_ADVANCE_FLAG_FAST_ONLY
   *
   * @description:
   *   A bit-flag to be OR-ed with the `flags` parameter of the
   *   @FT_Get_Advance and @FT_Get_Advances functions.
   *
   *   If set, it indicates that you want these functions to fail if the
   *   corresponding hinting mode or font driver doesn't allow for very quick
   *   advance computation.
   *
   *   Typically, glyphs that are either unscaled, unhinted, bitmapped, or
   *   light-hinted can have their advance width computed very quickly.
   *
   *   Normal and bytecode hinted modes that require loading, scaling, and
   *   hinting of the glyph outline, are extremely slow by comparison.
   */
#define FT_ADVANCE_FLAG_FAST_ONLY  0x20000000L


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Advance
   *
   * @description:
   *   Retrieve the advance value of a given glyph outline in an @FT_Face.
   *
   * @input:
   *   face ::
   *     The source @FT_Face handle.
   *
   *   gindex ::
   *     The glyph index.
   *
   *   load_flags ::
   *     A set of bit flags similar to those used when calling
   *     @FT_Load_Glyph, used to determine what kind of advances you need.
   *
   * @output:
   *   padvance ::
   *     The advance value.  If scaling is performed (based on the value of
   *     `load_flags`), the advance value is in 16.16 format.  Otherwise, it
   *     is in font units.
   *
   *     If @FT_LOAD_VERTICAL_LAYOUT is set, this is the vertical advance
   *     corresponding to a vertical layout.  Otherwise, it is the horizontal
   *     advance in a horizontal layout.
   *
   * @return:
   *   FreeType error code.  0 means success.
   *
   * @note:
   *   This function may fail if you use @FT_ADVANCE_FLAG_FAST_ONLY and if
   *   the corresponding font backend doesn't have a quick way to retrieve
   *   the advances.
   *
   *   A scaled advance is returned in 16.16 format but isn't transformed by
   *   the affine transformation specified by @FT_Set_Transform.
   */
   FT_Error 
  FT_Get_Advance( FT_Face    face,
                  FT_UInt    gindex,
                  FT_Int32   load_flags,
                  FT_Fixed  *padvance );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Advances
   *
   * @description:
   *   Retrieve the advance values of several glyph outlines in an @FT_Face.
   *
   * @input:
   *   face ::
   *     The source @FT_Face handle.
   *
   *   start ::
   *     The first glyph index.
   *
   *   count ::
   *     The number of advance values you want to retrieve.
   *
   *   load_flags ::
   *     A set of bit flags similar to those used when calling
   *     @FT_Load_Glyph.
   *
   * @output:
   *   padvance ::
   *     The advance values.  This array, to be provided by the caller, must
   *     contain at least `count` elements.
   *
   *     If scaling is performed (based on the value of `load_flags`), the
   *     advance values are in 16.16 format.  Otherwise, they are in font
   *     units.
   *
   *     If @FT_LOAD_VERTICAL_LAYOUT is set, these are the vertical advances
   *     corresponding to a vertical layout.  Otherwise, they are the
   *     horizontal advances in a horizontal layout.
   *
   * @return:
   *   FreeType error code.  0 means success.
   *
   * @note:
   *   This function may fail if you use @FT_ADVANCE_FLAG_FAST_ONLY and if
   *   the corresponding font backend doesn't have a quick way to retrieve
   *   the advances.
   *
   *   Scaled advances are returned in 16.16 format but aren't transformed by
   *   the affine transformation specified by @FT_Set_Transform.
   */
   FT_Error 
  FT_Get_Advances( FT_Face    face,
                   FT_UInt    start,
                   FT_UInt    count,
                   FT_Int32   load_flags,
                   FT_Fixed  *padvances );

  /* */




#endif /* FTADVANC_H_ */


/* END */
//
// ===========================  ftmac.h  ===========================
//
/****************************************************************************
 *
 * ftmac.h
 *
 *   Additional Mac-specific API.
 *
 * Copyright (C) 1996-2024 by
 * Just van Rossum, David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


/****************************************************************************
 *
 * NOTE: Include this file after `FT_FREETYPE_H` and after any
 *       Mac-specific headers (because this header uses Mac types such as
 *       'Handle', 'FSSpec', 'FSRef', etc.)
 *
 */


#ifndef FTMAC_H_
#define FTMAC_H_







  /* gcc-3.1 and later can warn about functions tagged as deprecated */
/*#ifndef 
#if defined( __GNUC__ )                                     && \
    ( ( __GNUC__ >= 4 )                                  ||    \
      ( ( __GNUC__ == 3 ) && ( __GNUC_MINOR__ >= 1 ) ) )
#define   __attribute__(( deprecated ))
#else
#define 
#endif
#endif*/


  /**************************************************************************
   *
   * @section:
   *   mac_specific
   *
   * @title:
   *   Mac Specific Interface
   *
   * @abstract:
   *   Only available on the Macintosh.
   *
   * @description:
   *   The following definitions are only available if FreeType is compiled
   *   on a Macintosh.
   *
   */


  /**************************************************************************
   *
   * @function:
   *   FT_New_Face_From_FOND
   *
   * @description:
   *   Create a new face object from a FOND resource.
   *
   * @inout:
   *   library ::
   *     A handle to the library resource.
   *
   * @input:
   *   fond ::
   *     A FOND resource.
   *
   *   face_index ::
   *     Only supported for the -1 'sanity check' special case.
   *
   * @output:
   *   aface ::
   *     A handle to a new face object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @example:
   *   This function can be used to create @FT_Face objects from fonts that
   *   are installed in the system as follows.
   *
   *   ```
   *     fond  = GetResource( 'FOND', fontName );
   *     error = FT_New_Face_From_FOND( library, fond, 0, &face );
   *   ```
   */
   FT_Error 
  FT_New_Face_From_FOND( FT_Library  library,
                         Handle      fond,
                         FT_Long     face_index,
                         FT_Face    *aface )
                       ;


  /**************************************************************************
   *
   * @function:
   *   FT_GetFile_From_Mac_Name
   *
   * @description:
   *   Return an FSSpec for the disk file containing the named font.
   *
   * @input:
   *   fontName ::
   *     Mac OS name of the font (e.g., Times New Roman Bold).
   *
   * @output:
   *   pathSpec ::
   *     FSSpec to the file.  For passing to @FT_New_Face_From_FSSpec.
   *
   *   face_index ::
   *     Index of the face.  For passing to @FT_New_Face_From_FSSpec.
   *
   * @return:
   *   FreeType error code.  0~means success.
   */
   FT_Error 
  FT_GetFile_From_Mac_Name( const char*  fontName,
                            FSSpec*      pathSpec,
                            FT_Long*     face_index )
                          ;


  /**************************************************************************
   *
   * @function:
   *   FT_GetFile_From_Mac_ATS_Name
   *
   * @description:
   *   Return an FSSpec for the disk file containing the named font.
   *
   * @input:
   *   fontName ::
   *     Mac OS name of the font in ATS framework.
   *
   * @output:
   *   pathSpec ::
   *     FSSpec to the file. For passing to @FT_New_Face_From_FSSpec.
   *
   *   face_index ::
   *     Index of the face. For passing to @FT_New_Face_From_FSSpec.
   *
   * @return:
   *   FreeType error code.  0~means success.
   */
   FT_Error 
  FT_GetFile_From_Mac_ATS_Name( const char*  fontName,
                                FSSpec*      pathSpec,
                                FT_Long*     face_index )
                              ;


  /**************************************************************************
   *
   * @function:
   *   FT_GetFilePath_From_Mac_ATS_Name
   *
   * @description:
   *   Return a pathname of the disk file and face index for given font name
   *   that is handled by ATS framework.
   *
   * @input:
   *   fontName ::
   *     Mac OS name of the font in ATS framework.
   *
   * @output:
   *   path ::
   *     Buffer to store pathname of the file.  For passing to @FT_New_Face.
   *     The client must allocate this buffer before calling this function.
   *
   *   maxPathSize ::
   *     Lengths of the buffer `path` that client allocated.
   *
   *   face_index ::
   *     Index of the face.  For passing to @FT_New_Face.
   *
   * @return:
   *   FreeType error code.  0~means success.
   */
   FT_Error 
  FT_GetFilePath_From_Mac_ATS_Name( const char*  fontName,
                                    UInt8*       path,
                                    UInt32       maxPathSize,
                                    FT_Long*     face_index )
                                  ;


  /**************************************************************************
   *
   * @function:
   *   FT_New_Face_From_FSSpec
   *
   * @description:
   *   Create a new face object from a given resource and typeface index
   *   using an FSSpec to the font file.
   *
   * @inout:
   *   library ::
   *     A handle to the library resource.
   *
   * @input:
   *   spec ::
   *     FSSpec to the font file.
   *
   *   face_index ::
   *     The index of the face within the resource.  The first face has
   *     index~0.
   * @output:
   *   aface ::
   *     A handle to a new face object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   @FT_New_Face_From_FSSpec is identical to @FT_New_Face except it
   *   accepts an FSSpec instead of a path.
   */
   FT_Error 
  FT_New_Face_From_FSSpec( FT_Library     library,
                           const FSSpec  *spec,
                           FT_Long        face_index,
                           FT_Face       *aface )
                         ;


  /**************************************************************************
   *
   * @function:
   *   FT_New_Face_From_FSRef
   *
   * @description:
   *   Create a new face object from a given resource and typeface index
   *   using an FSRef to the font file.
   *
   * @inout:
   *   library ::
   *     A handle to the library resource.
   *
   * @input:
   *   spec ::
   *     FSRef to the font file.
   *
   *   face_index ::
   *     The index of the face within the resource.  The first face has
   *     index~0.
   * @output:
   *   aface ::
   *     A handle to a new face object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   @FT_New_Face_From_FSRef is identical to @FT_New_Face except it accepts
   *   an FSRef instead of a path.
   */
   FT_Error 
  FT_New_Face_From_FSRef( FT_Library    library,
                          const FSRef  *ref,
                          FT_Long       face_index,
                          FT_Face      *aface )
                        ;

  /* */





#endif /* FTMAC_H_ */


/* END */
//
// ===========================  ftgxval.h  ===========================
//
/****************************************************************************
 *
 * ftgxval.h
 *
 *   FreeType API for validating TrueTypeGX/AAT tables (specification).
 *
 * Copyright (C) 2004-2024 by
 * Masatake YAMATO, Redhat K.K,
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */

/****************************************************************************
 *
 * gxvalid is derived from both gxlayout module and otvalid module.
 * Development of gxlayout is supported by the Information-technology
 * Promotion Agency(IPA), Japan.
 *
 */


#ifndef FTGXVAL_H_
#define FTGXVAL_H_

#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   gx_validation
   *
   * @title:
   *   TrueTypeGX/AAT Validation
   *
   * @abstract:
   *   An API to validate TrueTypeGX/AAT tables.
   *
   * @description:
   *   This section contains the declaration of functions to validate some
   *   TrueTypeGX tables (feat, mort, morx, bsln, just, kern, opbd, trak,
   *   prop, lcar).
   *
   * @order:
   *   FT_TrueTypeGX_Validate
   *   FT_TrueTypeGX_Free
   *
   *   FT_ClassicKern_Validate
   *   FT_ClassicKern_Free
   *
   *   FT_VALIDATE_GX_LENGTH
   *   FT_VALIDATE_GXXXX
   *   FT_VALIDATE_CKERNXXX
   *
   */

  /**************************************************************************
   *
   *
   * Warning: Use `FT_VALIDATE_XXX` to validate a table.
   *          Following definitions are for gxvalid developers.
   *
   *
   */

#define FT_VALIDATE_feat_INDEX     0
#define FT_VALIDATE_mort_INDEX     1
#define FT_VALIDATE_morx_INDEX     2
#define FT_VALIDATE_bsln_INDEX     3
#define FT_VALIDATE_just_INDEX     4
#define FT_VALIDATE_kern_INDEX     5
#define FT_VALIDATE_opbd_INDEX     6
#define FT_VALIDATE_trak_INDEX     7
#define FT_VALIDATE_prop_INDEX     8
#define FT_VALIDATE_lcar_INDEX     9
#define FT_VALIDATE_GX_LAST_INDEX  FT_VALIDATE_lcar_INDEX


  /**************************************************************************
   *
   * @macro:
   *   FT_VALIDATE_GX_LENGTH
   *
   * @description:
   *   The number of tables checked in this module.  Use it as a parameter
   *   for the `table-length` argument of function @FT_TrueTypeGX_Validate.
   */
#define FT_VALIDATE_GX_LENGTH  ( FT_VALIDATE_GX_LAST_INDEX + 1 )

  /* */

  /* Up to 0x1000 is used by otvalid.
     Ox2xxx is reserved for feature OT extension. */
//#define FT_VALIDATE_GX_START  0x4000
//#define FT_VALIDATE_GX_BITFIELD( tag ) \
//          ( FT_VALIDATE_GX_START << FT_VALIDATE_##tag##_INDEX )


  /**************************************************************************
   *
   * @enum:
   *    FT_VALIDATE_GXXXX
   *
   * @description:
   *    A list of bit-field constants used with @FT_TrueTypeGX_Validate to
   *    indicate which TrueTypeGX/AAT Type tables should be validated.
   *
   * @values:
   *    FT_VALIDATE_feat ::
   *      Validate 'feat' table.
   *
   *    FT_VALIDATE_mort ::
   *      Validate 'mort' table.
   *
   *    FT_VALIDATE_morx ::
   *      Validate 'morx' table.
   *
   *    FT_VALIDATE_bsln ::
   *      Validate 'bsln' table.
   *
   *    FT_VALIDATE_just ::
   *      Validate 'just' table.
   *
   *    FT_VALIDATE_kern ::
   *      Validate 'kern' table.
   *
   *    FT_VALIDATE_opbd ::
   *      Validate 'opbd' table.
   *
   *    FT_VALIDATE_trak ::
   *      Validate 'trak' table.
   *
   *    FT_VALIDATE_prop ::
   *      Validate 'prop' table.
   *
   *    FT_VALIDATE_lcar ::
   *      Validate 'lcar' table.
   *
   *    FT_VALIDATE_GX ::
   *      Validate all TrueTypeGX tables (feat, mort, morx, bsln, just, kern,
   *      opbd, trak, prop and lcar).
   *
   */

#define FT_VALIDATE_feat  FT_VALIDATE_GX_BITFIELD( feat )
#define FT_VALIDATE_mort  FT_VALIDATE_GX_BITFIELD( mort )
#define FT_VALIDATE_morx  FT_VALIDATE_GX_BITFIELD( morx )
#define FT_VALIDATE_bsln  FT_VALIDATE_GX_BITFIELD( bsln )
#define FT_VALIDATE_just  FT_VALIDATE_GX_BITFIELD( just )
#define FT_VALIDATE_kern  FT_VALIDATE_GX_BITFIELD( kern )
#define FT_VALIDATE_opbd  FT_VALIDATE_GX_BITFIELD( opbd )
#define FT_VALIDATE_trak  FT_VALIDATE_GX_BITFIELD( trak )
#define FT_VALIDATE_prop  FT_VALIDATE_GX_BITFIELD( prop )
#define FT_VALIDATE_lcar  FT_VALIDATE_GX_BITFIELD( lcar )

#define FT_VALIDATE_GX  ( FT_VALIDATE_feat | \
                          FT_VALIDATE_mort | \
                          FT_VALIDATE_morx | \
                          FT_VALIDATE_bsln | \
                          FT_VALIDATE_just | \
                          FT_VALIDATE_kern | \
                          FT_VALIDATE_opbd | \
                          FT_VALIDATE_trak | \
                          FT_VALIDATE_prop | \
                          FT_VALIDATE_lcar )


  /**************************************************************************
   *
   * @function:
   *    FT_TrueTypeGX_Validate
   *
   * @description:
   *    Validate various TrueTypeGX tables to assure that all offsets and
   *    indices are valid.  The idea is that a higher-level library that
   *    actually does the text layout can access those tables without error
   *    checking (which can be quite time consuming).
   *
   * @input:
   *    face ::
   *      A handle to the input face.
   *
   *    validation_flags ::
   *      A bit field that specifies the tables to be validated.  See
   *      @FT_VALIDATE_GXXXX for possible values.
   *
   *    table_length ::
   *      The size of the `tables` array.  Normally, @FT_VALIDATE_GX_LENGTH
   *      should be passed.
   *
   * @output:
   *    tables ::
   *      The array where all validated sfnt tables are stored.  The array
   *      itself must be allocated by a client.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function only works with TrueTypeGX fonts, returning an error
   *   otherwise.
   *
   *   After use, the application should deallocate the buffers pointed to by
   *   each `tables` element, by calling @FT_TrueTypeGX_Free.  A `NULL` value
   *   indicates that the table either doesn't exist in the font, the
   *   application hasn't asked for validation, or the validator doesn't have
   *   the ability to validate the sfnt table.
   */
   FT_Error 
  FT_TrueTypeGX_Validate( FT_Face   face,
                          FT_UInt   validation_flags,
                          FT_Bytes  tables[FT_VALIDATE_GX_LENGTH],
                          FT_UInt   table_length );


  /**************************************************************************
   *
   * @function:
   *    FT_TrueTypeGX_Free
   *
   * @description:
   *    Free the buffer allocated by TrueTypeGX validator.
   *
   * @input:
   *    face ::
   *      A handle to the input face.
   *
   *    table ::
   *      The pointer to the buffer allocated by @FT_TrueTypeGX_Validate.
   *
   * @note:
   *   This function must be used to free the buffer allocated by
   *   @FT_TrueTypeGX_Validate only.
   */
   void 
  FT_TrueTypeGX_Free( FT_Face   face,
                      FT_Bytes  table );


  /**************************************************************************
   *
   * @enum:
   *    FT_VALIDATE_CKERNXXX
   *
   * @description:
   *    A list of bit-field constants used with @FT_ClassicKern_Validate to
   *    indicate the classic kern dialect or dialects.  If the selected type
   *    doesn't fit, @FT_ClassicKern_Validate regards the table as invalid.
   *
   * @values:
   *    FT_VALIDATE_MS ::
   *      Handle the 'kern' table as a classic Microsoft kern table.
   *
   *    FT_VALIDATE_APPLE ::
   *      Handle the 'kern' table as a classic Apple kern table.
   *
   *    FT_VALIDATE_CKERN ::
   *      Handle the 'kern' as either classic Apple or Microsoft kern table.
   */
#define FT_VALIDATE_MS     ( FT_VALIDATE_GX_START << 0 )
#define FT_VALIDATE_APPLE  ( FT_VALIDATE_GX_START << 1 )

#define FT_VALIDATE_CKERN  ( FT_VALIDATE_MS | FT_VALIDATE_APPLE )


  /**************************************************************************
   *
   * @function:
   *    FT_ClassicKern_Validate
   *
   * @description:
   *    Validate classic (16-bit format) kern table to assure that the
   *    offsets and indices are valid.  The idea is that a higher-level
   *    library that actually does the text layout can access those tables
   *    without error checking (which can be quite time consuming).
   *
   *    The 'kern' table validator in @FT_TrueTypeGX_Validate deals with both
   *    the new 32-bit format and the classic 16-bit format, while
   *    FT_ClassicKern_Validate only supports the classic 16-bit format.
   *
   * @input:
   *    face ::
   *      A handle to the input face.
   *
   *    validation_flags ::
   *      A bit field that specifies the dialect to be validated.  See
   *      @FT_VALIDATE_CKERNXXX for possible values.
   *
   * @output:
   *    ckern_table ::
   *      A pointer to the kern table.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   After use, the application should deallocate the buffers pointed to by
   *   `ckern_table`, by calling @FT_ClassicKern_Free.  A `NULL` value
   *   indicates that the table doesn't exist in the font.
   */
   FT_Error 
  FT_ClassicKern_Validate( FT_Face    face,
                           FT_UInt    validation_flags,
                           FT_Bytes  *ckern_table );


  /**************************************************************************
   *
   * @function:
   *    FT_ClassicKern_Free
   *
   * @description:
   *    Free the buffer allocated by classic Kern validator.
   *
   * @input:
   *    face ::
   *      A handle to the input face.
   *
   *    table ::
   *      The pointer to the buffer that is allocated by
   *      @FT_ClassicKern_Validate.
   *
   * @note:
   *   This function must be used to free the buffer allocated by
   *   @FT_ClassicKern_Validate only.
   */
   void 
  FT_ClassicKern_Free( FT_Face   face,
                       FT_Bytes  table );

  /* */




#endif /* FTGXVAL_H_ */


/* END */
//
// ===========================  tttags.h  ===========================
//
/****************************************************************************
 *
 * tttags.h
 *
 *   Tags for TrueType and OpenType tables (specification only).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef TTAGS_H_
#define TTAGS_H_


#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





#define TTAG_avar  FT_MAKE_TAG( 'a', 'v', 'a', 'r' )
#define TTAG_BASE  FT_MAKE_TAG( 'B', 'A', 'S', 'E' )
#define TTAG_bdat  FT_MAKE_TAG( 'b', 'd', 'a', 't' )
#define TTAG_BDF   FT_MAKE_TAG( 'B', 'D', 'F', ' ' )
#define TTAG_bhed  FT_MAKE_TAG( 'b', 'h', 'e', 'd' )
#define TTAG_bloc  FT_MAKE_TAG( 'b', 'l', 'o', 'c' )
#define TTAG_bsln  FT_MAKE_TAG( 'b', 's', 'l', 'n' )
#define TTAG_CBDT  FT_MAKE_TAG( 'C', 'B', 'D', 'T' )
#define TTAG_CBLC  FT_MAKE_TAG( 'C', 'B', 'L', 'C' )
#define TTAG_CFF   FT_MAKE_TAG( 'C', 'F', 'F', ' ' )
#define TTAG_CFF2  FT_MAKE_TAG( 'C', 'F', 'F', '2' )
#define TTAG_CID   FT_MAKE_TAG( 'C', 'I', 'D', ' ' )
#define TTAG_cmap  FT_MAKE_TAG( 'c', 'm', 'a', 'p' )
#define TTAG_COLR  FT_MAKE_TAG( 'C', 'O', 'L', 'R' )
#define TTAG_CPAL  FT_MAKE_TAG( 'C', 'P', 'A', 'L' )
#define TTAG_cvar  FT_MAKE_TAG( 'c', 'v', 'a', 'r' )
#define TTAG_cvt   FT_MAKE_TAG( 'c', 'v', 't', ' ' )
#define TTAG_DSIG  FT_MAKE_TAG( 'D', 'S', 'I', 'G' )
#define TTAG_EBDT  FT_MAKE_TAG( 'E', 'B', 'D', 'T' )
#define TTAG_EBLC  FT_MAKE_TAG( 'E', 'B', 'L', 'C' )
#define TTAG_EBSC  FT_MAKE_TAG( 'E', 'B', 'S', 'C' )
#define TTAG_feat  FT_MAKE_TAG( 'f', 'e', 'a', 't' )
#define TTAG_FOND  FT_MAKE_TAG( 'F', 'O', 'N', 'D' )
#define TTAG_fpgm  FT_MAKE_TAG( 'f', 'p', 'g', 'm' )
#define TTAG_fvar  FT_MAKE_TAG( 'f', 'v', 'a', 'r' )
#define TTAG_gasp  FT_MAKE_TAG( 'g', 'a', 's', 'p' )
#define TTAG_GDEF  FT_MAKE_TAG( 'G', 'D', 'E', 'F' )
#define TTAG_glyf  FT_MAKE_TAG( 'g', 'l', 'y', 'f' )
#define TTAG_GPOS  FT_MAKE_TAG( 'G', 'P', 'O', 'S' )
#define TTAG_GSUB  FT_MAKE_TAG( 'G', 'S', 'U', 'B' )
#define TTAG_gvar  FT_MAKE_TAG( 'g', 'v', 'a', 'r' )
#define TTAG_HVAR  FT_MAKE_TAG( 'H', 'V', 'A', 'R' )
#define TTAG_hdmx  FT_MAKE_TAG( 'h', 'd', 'm', 'x' )
#define TTAG_head  FT_MAKE_TAG( 'h', 'e', 'a', 'd' )
#define TTAG_hhea  FT_MAKE_TAG( 'h', 'h', 'e', 'a' )
#define TTAG_hmtx  FT_MAKE_TAG( 'h', 'm', 't', 'x' )
#define TTAG_JSTF  FT_MAKE_TAG( 'J', 'S', 'T', 'F' )
#define TTAG_just  FT_MAKE_TAG( 'j', 'u', 's', 't' )
#define TTAG_kern  FT_MAKE_TAG( 'k', 'e', 'r', 'n' )
#define TTAG_lcar  FT_MAKE_TAG( 'l', 'c', 'a', 'r' )
#define TTAG_loca  FT_MAKE_TAG( 'l', 'o', 'c', 'a' )
#define TTAG_LTSH  FT_MAKE_TAG( 'L', 'T', 'S', 'H' )
#define TTAG_LWFN  FT_MAKE_TAG( 'L', 'W', 'F', 'N' )
#define TTAG_MATH  FT_MAKE_TAG( 'M', 'A', 'T', 'H' )
#define TTAG_maxp  FT_MAKE_TAG( 'm', 'a', 'x', 'p' )
#define TTAG_META  FT_MAKE_TAG( 'M', 'E', 'T', 'A' )
#define TTAG_MMFX  FT_MAKE_TAG( 'M', 'M', 'F', 'X' )
#define TTAG_MMSD  FT_MAKE_TAG( 'M', 'M', 'S', 'D' )
#define TTAG_mort  FT_MAKE_TAG( 'm', 'o', 'r', 't' )
#define TTAG_morx  FT_MAKE_TAG( 'm', 'o', 'r', 'x' )
#define TTAG_MVAR  FT_MAKE_TAG( 'M', 'V', 'A', 'R' )
#define TTAG_name  FT_MAKE_TAG( 'n', 'a', 'm', 'e' )
#define TTAG_opbd  FT_MAKE_TAG( 'o', 'p', 'b', 'd' )
#define TTAG_OS2   FT_MAKE_TAG( 'O', 'S', '/', '2' )
#define TTAG_OTTO  FT_MAKE_TAG( 'O', 'T', 'T', 'O' )
#define TTAG_PCLT  FT_MAKE_TAG( 'P', 'C', 'L', 'T' )
#define TTAG_POST  FT_MAKE_TAG( 'P', 'O', 'S', 'T' )
#define TTAG_post  FT_MAKE_TAG( 'p', 'o', 's', 't' )
#define TTAG_prep  FT_MAKE_TAG( 'p', 'r', 'e', 'p' )
#define TTAG_prop  FT_MAKE_TAG( 'p', 'r', 'o', 'p' )
#define TTAG_sbix  FT_MAKE_TAG( 's', 'b', 'i', 'x' )
#define TTAG_sfnt  FT_MAKE_TAG( 's', 'f', 'n', 't' )
#define TTAG_SING  FT_MAKE_TAG( 'S', 'I', 'N', 'G' )
#define TTAG_SVG   FT_MAKE_TAG( 'S', 'V', 'G', ' ' )
#define TTAG_trak  FT_MAKE_TAG( 't', 'r', 'a', 'k' )
#define TTAG_true  FT_MAKE_TAG( 't', 'r', 'u', 'e' )
#define TTAG_ttc   FT_MAKE_TAG( 't', 't', 'c', ' ' )
#define TTAG_ttcf  FT_MAKE_TAG( 't', 't', 'c', 'f' )
#define TTAG_TYP1  FT_MAKE_TAG( 'T', 'Y', 'P', '1' )
#define TTAG_typ1  FT_MAKE_TAG( 't', 'y', 'p', '1' )
#define TTAG_VDMX  FT_MAKE_TAG( 'V', 'D', 'M', 'X' )
#define TTAG_vhea  FT_MAKE_TAG( 'v', 'h', 'e', 'a' )
#define TTAG_vmtx  FT_MAKE_TAG( 'v', 'm', 't', 'x' )
#define TTAG_VVAR  FT_MAKE_TAG( 'V', 'V', 'A', 'R' )
#define TTAG_wOFF  FT_MAKE_TAG( 'w', 'O', 'F', 'F' )
#define TTAG_wOF2  FT_MAKE_TAG( 'w', 'O', 'F', '2' )

/* used by "Keyboard.dfont" on legacy Mac OS X */
#define TTAG_0xA5kbd  FT_MAKE_TAG( 0xA5, 'k', 'b', 'd' )

/* used by "LastResort.dfont" on legacy Mac OS X */
#define TTAG_0xA5lst  FT_MAKE_TAG( 0xA5, 'l', 's', 't' )




#endif /* TTAGS_H_ */


/* END */
//
// ===========================  ftoutln.h  ===========================
//
/****************************************************************************
 *
 * ftoutln.h
 *
 *   Support for the FT_Outline type used to store glyph shapes of
 *   most scalable font formats (specification).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTOUTLN_H_
#define FTOUTLN_H_


#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   outline_processing
   *
   * @title:
   *   Outline Processing
   *
   * @abstract:
   *   Functions to create, transform, and render vectorial glyph images.
   *
   * @description:
   *   This section contains routines used to create and destroy scalable
   *   glyph images known as 'outlines'.  These can also be measured,
   *   transformed, and converted into bitmaps and pixmaps.
   *
   * @order:
   *   FT_Outline
   *   FT_Outline_New
   *   FT_Outline_Done
   *   FT_Outline_Copy
   *   FT_Outline_Translate
   *   FT_Outline_Transform
   *   FT_Outline_Embolden
   *   FT_Outline_EmboldenXY
   *   FT_Outline_Reverse
   *   FT_Outline_Check
   *
   *   FT_Outline_Get_CBox
   *   FT_Outline_Get_BBox
   *
   *   FT_Outline_Get_Bitmap
   *   FT_Outline_Render
   *   FT_Outline_Decompose
   *   FT_Outline_Funcs
   *   FT_Outline_MoveToFunc
   *   FT_Outline_LineToFunc
   *   FT_Outline_ConicToFunc
   *   FT_Outline_CubicToFunc
   *
   *   FT_Orientation
   *   FT_Outline_Get_Orientation
   *
   *   FT_OUTLINE_XXX
   *
   */


  /**************************************************************************
   *
   * @function:
   *   FT_Outline_Decompose
   *
   * @description:
   *   Walk over an outline's structure to decompose it into individual
   *   segments and Bezier arcs.  This function also emits 'move to'
   *   operations to indicate the start of new contours in the outline.
   *
   * @input:
   *   outline ::
   *     A pointer to the source target.
   *
   *   func_interface ::
   *     A table of 'emitters', i.e., function pointers called during
   *     decomposition to indicate path operations.
   *
   * @inout:
   *   user ::
   *     A typeless pointer that is passed to each emitter during the
   *     decomposition.  It can be used to store the state during the
   *     decomposition.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   Degenerate contours, segments, and Bezier arcs may be reported.  In
   *   most cases, it is best to filter these out before using the outline
   *   for stroking or other path modification purposes (which may cause
   *   degenerate segments to become non-degenerate and visible, like when
   *   stroke caps are used or the path is otherwise outset).  Some glyph
   *   outlines may contain deliberate degenerate single points for mark
   *   attachement.
   *
   *   Similarly, the function returns success for an empty outline also
   *   (doing nothing, that is, not calling any emitter); if necessary, you
   *   should filter this out, too.
   */
   FT_Error 
  FT_Outline_Decompose( FT_Outline*              outline,
                        const FT_Outline_Funcs*  func_interface,
                        void*                    user );


  /**************************************************************************
   *
   * @function:
   *   FT_Outline_New
   *
   * @description:
   *   Create a new outline of a given size.
   *
   * @input:
   *   library ::
   *     A handle to the library object from where the outline is allocated.
   *     Note however that the new outline will **not** necessarily be
   *     **freed**, when destroying the library, by @FT_Done_FreeType.
   *
   *   numPoints ::
   *     The maximum number of points within the outline.  Must be smaller
   *     than or equal to 0xFFFF (65535).
   *
   *   numContours ::
   *     The maximum number of contours within the outline.  This value must
   *     be in the range 0 to `numPoints`.
   *
   * @output:
   *   anoutline ::
   *     A handle to the new outline.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The reason why this function takes a `library` parameter is simply to
   *   use the library's memory allocator.
   */
   FT_Error 
  FT_Outline_New( FT_Library   library,
                  FT_UInt      numPoints,
                  FT_Int       numContours,
                  FT_Outline  *anoutline );


  /**************************************************************************
   *
   * @function:
   *   FT_Outline_Done
   *
   * @description:
   *   Destroy an outline created with @FT_Outline_New.
   *
   * @input:
   *   library ::
   *     A handle of the library object used to allocate the outline.
   *
   *   outline ::
   *     A pointer to the outline object to be discarded.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   If the outline's 'owner' field is not set, only the outline descriptor
   *   will be released.
   */
   FT_Error 
  FT_Outline_Done( FT_Library   library,
                   FT_Outline*  outline );


  /**************************************************************************
   *
   * @function:
   *   FT_Outline_Check
   *
   * @description:
   *   Check the contents of an outline descriptor.
   *
   * @input:
   *   outline ::
   *     A handle to a source outline.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   An empty outline, or an outline with a single point only is also
   *   valid.
   */
   FT_Error 
  FT_Outline_Check( FT_Outline*  outline );


  /**************************************************************************
   *
   * @function:
   *   FT_Outline_Get_CBox
   *
   * @description:
   *   Return an outline's 'control box'.  The control box encloses all the
   *   outline's points, including Bezier control points.  Though it
   *   coincides with the exact bounding box for most glyphs, it can be
   *   slightly larger in some situations (like when rotating an outline that
   *   contains Bezier outside arcs).
   *
   *   Computing the control box is very fast, while getting the bounding box
   *   can take much more time as it needs to walk over all segments and arcs
   *   in the outline.  To get the latter, you can use the 'ftbbox'
   *   component, which is dedicated to this single task.
   *
   * @input:
   *   outline ::
   *     A pointer to the source outline descriptor.
   *
   * @output:
   *   acbox ::
   *     The outline's control box.
   *
   * @note:
   *   See @FT_Glyph_Get_CBox for a discussion of tricky fonts.
   */
   void 
  FT_Outline_Get_CBox( const FT_Outline*  outline,
                       FT_BBox           *acbox );


  /**************************************************************************
   *
   * @function:
   *   FT_Outline_Translate
   *
   * @description:
   *   Apply a simple translation to the points of an outline.
   *
   * @inout:
   *   outline ::
   *     A pointer to the target outline descriptor.
   *
   * @input:
   *   xOffset ::
   *     The horizontal offset.
   *
   *   yOffset ::
   *     The vertical offset.
   */
   void 
  FT_Outline_Translate( const FT_Outline*  outline,
                        FT_Pos             xOffset,
                        FT_Pos             yOffset );


  /**************************************************************************
   *
   * @function:
   *   FT_Outline_Copy
   *
   * @description:
   *   Copy an outline into another one.  Both objects must have the same
   *   sizes (number of points & number of contours) when this function is
   *   called.
   *
   * @input:
   *   source ::
   *     A handle to the source outline.
   *
   * @output:
   *   target ::
   *     A handle to the target outline.
   *
   * @return:
   *   FreeType error code.  0~means success.
   */
   FT_Error 
  FT_Outline_Copy( const FT_Outline*  source,
                   FT_Outline        *target );


  /**************************************************************************
   *
   * @function:
   *   FT_Outline_Transform
   *
   * @description:
   *   Apply a simple 2x2 matrix to all of an outline's points.  Useful for
   *   applying rotations, slanting, flipping, etc.
   *
   * @inout:
   *   outline ::
   *     A pointer to the target outline descriptor.
   *
   * @input:
   *   matrix ::
   *     A pointer to the transformation matrix.
   *
   * @note:
   *   You can use @FT_Outline_Translate if you need to translate the
   *   outline's points.
   */
   void 
  FT_Outline_Transform( const FT_Outline*  outline,
                        const FT_Matrix*   matrix );


  /**************************************************************************
   *
   * @function:
   *   FT_Outline_Embolden
   *
   * @description:
   *   Embolden an outline.  The new outline will be at most 4~times
   *   `strength` pixels wider and higher.  You may think of the left and
   *   bottom borders as unchanged.
   *
   *   Negative `strength` values to reduce the outline thickness are
   *   possible also.
   *
   * @inout:
   *   outline ::
   *     A handle to the target outline.
   *
   * @input:
   *   strength ::
   *     How strong the glyph is emboldened.  Expressed in 26.6 pixel format.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The used algorithm to increase or decrease the thickness of the glyph
   *   doesn't change the number of points; this means that certain
   *   situations like acute angles or intersections are sometimes handled
   *   incorrectly.
   *
   *   If you need 'better' metrics values you should call
   *   @FT_Outline_Get_CBox or @FT_Outline_Get_BBox.
   *
   *   To get meaningful results, font scaling values must be set with
   *   functions like @FT_Set_Char_Size before calling FT_Render_Glyph.
   *
   * @example:
   *   ```
   *     FT_Load_Glyph( face, index, FT_LOAD_DEFAULT );
   *
   *     if ( face->glyph->format == FT_GLYPH_FORMAT_OUTLINE )
   *       FT_Outline_Embolden( &face->glyph->outline, strength );
   *   ```
   *
   */
   FT_Error 
  FT_Outline_Embolden( FT_Outline*  outline,
                       FT_Pos       strength );


  /**************************************************************************
   *
   * @function:
   *   FT_Outline_EmboldenXY
   *
   * @description:
   *   Embolden an outline.  The new outline will be `xstrength` pixels wider
   *   and `ystrength` pixels higher.  Otherwise, it is similar to
   *   @FT_Outline_Embolden, which uses the same strength in both directions.
   *
   * @since:
   *   2.4.10
   */
   FT_Error 
  FT_Outline_EmboldenXY( FT_Outline*  outline,
                         FT_Pos       xstrength,
                         FT_Pos       ystrength );


  /**************************************************************************
   *
   * @function:
   *   FT_Outline_Reverse
   *
   * @description:
   *   Reverse the drawing direction of an outline.  This is used to ensure
   *   consistent fill conventions for mirrored glyphs.
   *
   * @inout:
   *   outline ::
   *     A pointer to the target outline descriptor.
   *
   * @note:
   *   This function toggles the bit flag @FT_OUTLINE_REVERSE_FILL in the
   *   outline's `flags` field.
   *
   *   It shouldn't be used by a normal client application, unless it knows
   *   what it is doing.
   */
   void 
  FT_Outline_Reverse( FT_Outline*  outline );


  /**************************************************************************
   *
   * @function:
   *   FT_Outline_Get_Bitmap
   *
   * @description:
   *   Render an outline within a bitmap.  The outline's image is simply
   *   OR-ed to the target bitmap.
   *
   * @input:
   *   library ::
   *     A handle to a FreeType library object.
   *
   *   outline ::
   *     A pointer to the source outline descriptor.
   *
   * @inout:
   *   abitmap ::
   *     A pointer to the target bitmap descriptor.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function does **not create** the bitmap, it only renders an
   *   outline image within the one you pass to it!  Consequently, the
   *   various fields in `abitmap` should be set accordingly.
   *
   *   It will use the raster corresponding to the default glyph format.
   *
   *   The value of the `num_grays` field in `abitmap` is ignored.  If you
   *   select the gray-level rasterizer, and you want less than 256 gray
   *   levels, you have to use @FT_Outline_Render directly.
   */
   FT_Error 
  FT_Outline_Get_Bitmap( FT_Library        library,
                         FT_Outline*       outline,
                         const FT_Bitmap  *abitmap );


  /**************************************************************************
   *
   * @function:
   *   FT_Outline_Render
   *
   * @description:
   *   Render an outline within a bitmap using the current scan-convert.
   *
   * @input:
   *   library ::
   *     A handle to a FreeType library object.
   *
   *   outline ::
   *     A pointer to the source outline descriptor.
   *
   * @inout:
   *   params ::
   *     A pointer to an @FT_Raster_Params structure used to describe the
   *     rendering operation.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This advanced function uses @FT_Raster_Params as an argument.
   *   The field `params.source` will be set to `outline` before the scan
   *   converter is called, which means that the value you give to it is
   *   actually ignored.  Either `params.target` must point to preallocated
   *   bitmap, or @FT_RASTER_FLAG_DIRECT must be set in `params.flags`
   *   allowing FreeType rasterizer to be used for direct composition,
   *   translucency, etc.  See @FT_Raster_Params for more details.
   */
   FT_Error 
  FT_Outline_Render( FT_Library         library,
                     FT_Outline*        outline,
                     FT_Raster_Params*  params );


  /**************************************************************************
   *
   * @enum:
   *   FT_Orientation
   *
   * @description:
   *   A list of values used to describe an outline's contour orientation.
   *
   *   The TrueType and PostScript specifications use different conventions
   *   to determine whether outline contours should be filled or unfilled.
   *
   * @values:
   *   FT_ORIENTATION_TRUETYPE ::
   *     According to the TrueType specification, clockwise contours must be
   *     filled, and counter-clockwise ones must be unfilled.
   *
   *   FT_ORIENTATION_POSTSCRIPT ::
   *     According to the PostScript specification, counter-clockwise
   *     contours must be filled, and clockwise ones must be unfilled.
   *
   *   FT_ORIENTATION_FILL_RIGHT ::
   *     This is identical to @FT_ORIENTATION_TRUETYPE, but is used to
   *     remember that in TrueType, everything that is to the right of the
   *     drawing direction of a contour must be filled.
   *
   *   FT_ORIENTATION_FILL_LEFT ::
   *     This is identical to @FT_ORIENTATION_POSTSCRIPT, but is used to
   *     remember that in PostScript, everything that is to the left of the
   *     drawing direction of a contour must be filled.
   *
   *   FT_ORIENTATION_NONE ::
   *     The orientation cannot be determined.  That is, different parts of
   *     the glyph have different orientation.
   *
   */
  typedef enum  FT_Orientation_
  {
    FT_ORIENTATION_TRUETYPE   = 0,
    FT_ORIENTATION_POSTSCRIPT = 1,
    FT_ORIENTATION_FILL_RIGHT = FT_ORIENTATION_TRUETYPE,
    FT_ORIENTATION_FILL_LEFT  = FT_ORIENTATION_POSTSCRIPT,
    FT_ORIENTATION_NONE

  } FT_Orientation;


  /**************************************************************************
   *
   * @function:
   *   FT_Outline_Get_Orientation
   *
   * @description:
   *   This function analyzes a glyph outline and tries to compute its fill
   *   orientation (see @FT_Orientation).  This is done by integrating the
   *   total area covered by the outline. The positive integral corresponds
   *   to the clockwise orientation and @FT_ORIENTATION_POSTSCRIPT is
   *   returned. The negative integral corresponds to the counter-clockwise
   *   orientation and @FT_ORIENTATION_TRUETYPE is returned.
   *
   *   Note that this will return @FT_ORIENTATION_TRUETYPE for empty
   *   outlines.
   *
   * @input:
   *   outline ::
   *     A handle to the source outline.
   *
   * @return:
   *   The orientation.
   *
   */
   FT_Orientation 
  FT_Outline_Get_Orientation( FT_Outline*  outline );


  /* */




#endif /* FTOUTLN_H_ */


/* END */


/* Local Variables: */
/* coding: utf-8    */
/* End:             */
//
// ===========================  t1tables.h  ===========================
//
/****************************************************************************
 *
 * t1tables.h
 *
 *   Basic Type 1/Type 2 tables definitions and interface (specification
 *   only).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef T1TABLES_H_
#define T1TABLES_H_


#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   type1_tables
   *
   * @title:
   *   Type 1 Tables
   *
   * @abstract:
   *   Type~1-specific font tables.
   *
   * @description:
   *   This section contains the definition of Type~1-specific tables,
   *   including structures related to other PostScript font formats.
   *
   * @order:
   *   PS_FontInfoRec
   *   PS_FontInfo
   *   PS_PrivateRec
   *   PS_Private
   *
   *   CID_FaceDictRec
   *   CID_FaceDict
   *   CID_FaceInfoRec
   *   CID_FaceInfo
   *
   *   FT_Has_PS_Glyph_Names
   *   FT_Get_PS_Font_Info
   *   FT_Get_PS_Font_Private
   *   FT_Get_PS_Font_Value
   *
   *   T1_Blend_Flags
   *   T1_EncodingType
   *   PS_Dict_Keys
   *
   */


  /* Note that we separate font data in PS_FontInfoRec and PS_PrivateRec */
  /* structures in order to support Multiple Master fonts.               */


  /**************************************************************************
   *
   * @struct:
   *   PS_FontInfoRec
   *
   * @description:
   *   A structure used to model a Type~1 or Type~2 FontInfo dictionary.
   *   Note that for Multiple Master fonts, each instance has its own
   *   FontInfo dictionary.
   */
  typedef struct  PS_FontInfoRec_
  {
    FT_String*  version;
    FT_String*  notice;
    FT_String*  full_name;
    FT_String*  family_name;
    FT_String*  weight;
    FT_Long     italic_angle;
    FT_Bool     is_fixed_pitch;
    FT_Short    underline_position;
    FT_UShort   underline_thickness;

  } PS_FontInfoRec;


  /**************************************************************************
   *
   * @struct:
   *   PS_FontInfo
   *
   * @description:
   *   A handle to a @PS_FontInfoRec structure.
   */
  typedef struct PS_FontInfoRec_*  PS_FontInfo;


  /**************************************************************************
   *
   * @struct:
   *   T1_FontInfo
   *
   * @description:
   *   This type is equivalent to @PS_FontInfoRec.  It is deprecated but kept
   *   to maintain source compatibility between various versions of FreeType.
   */
  typedef PS_FontInfoRec  T1_FontInfo;


  /**************************************************************************
   *
   * @struct:
   *   PS_PrivateRec
   *
   * @description:
   *   A structure used to model a Type~1 or Type~2 private dictionary.  Note
   *   that for Multiple Master fonts, each instance has its own Private
   *   dictionary.
   */
  typedef struct  PS_PrivateRec_
  {
    FT_Int     unique_id;
    FT_Int     lenIV;

    FT_Byte    num_blue_values;
    FT_Byte    num_other_blues;
    FT_Byte    num_family_blues;
    FT_Byte    num_family_other_blues;

    FT_Short   blue_values[14];
    FT_Short   other_blues[10];

    FT_Short   family_blues      [14];
    FT_Short   family_other_blues[10];

    FT_Fixed   blue_scale;
    FT_Int     blue_shift;
    FT_Int     blue_fuzz;

    FT_UShort  standard_width[1];
    FT_UShort  standard_height[1];

    FT_Byte    num_snap_widths;
    FT_Byte    num_snap_heights;
    FT_Bool    force_bold;
    FT_Bool    round_stem_up;

    FT_Short   snap_widths [13];  /* including std width  */
    FT_Short   snap_heights[13];  /* including std height */

    FT_Fixed   expansion_factor;

    FT_Long    language_group;
    FT_Long    password;

    FT_Short   min_feature[2];

  } PS_PrivateRec;


  /**************************************************************************
   *
   * @struct:
   *   PS_Private
   *
   * @description:
   *   A handle to a @PS_PrivateRec structure.
   */
  typedef struct PS_PrivateRec_*  PS_Private;


  /**************************************************************************
   *
   * @struct:
   *   T1_Private
   *
   * @description:
   *  This type is equivalent to @PS_PrivateRec.  It is deprecated but kept
   *  to maintain source compatibility between various versions of FreeType.
   */
  typedef PS_PrivateRec  T1_Private;


  /**************************************************************************
   *
   * @enum:
   *   T1_Blend_Flags
   *
   * @description:
   *   A set of flags used to indicate which fields are present in a given
   *   blend dictionary (font info or private).  Used to support Multiple
   *   Masters fonts.
   *
   * @values:
   *   T1_BLEND_UNDERLINE_POSITION ::
   *   T1_BLEND_UNDERLINE_THICKNESS ::
   *   T1_BLEND_ITALIC_ANGLE ::
   *   T1_BLEND_BLUE_VALUES ::
   *   T1_BLEND_OTHER_BLUES ::
   *   T1_BLEND_STANDARD_WIDTH ::
   *   T1_BLEND_STANDARD_HEIGHT ::
   *   T1_BLEND_STEM_SNAP_WIDTHS ::
   *   T1_BLEND_STEM_SNAP_HEIGHTS ::
   *   T1_BLEND_BLUE_SCALE ::
   *   T1_BLEND_BLUE_SHIFT ::
   *   T1_BLEND_FAMILY_BLUES ::
   *   T1_BLEND_FAMILY_OTHER_BLUES ::
   *   T1_BLEND_FORCE_BOLD ::
   */
  typedef enum  T1_Blend_Flags_
  {
    /* required fields in a FontInfo blend dictionary */
    T1_BLEND_UNDERLINE_POSITION = 0,
    T1_BLEND_UNDERLINE_THICKNESS,
    T1_BLEND_ITALIC_ANGLE,

    /* required fields in a Private blend dictionary */
    T1_BLEND_BLUE_VALUES,
    T1_BLEND_OTHER_BLUES,
    T1_BLEND_STANDARD_WIDTH,
    T1_BLEND_STANDARD_HEIGHT,
    T1_BLEND_STEM_SNAP_WIDTHS,
    T1_BLEND_STEM_SNAP_HEIGHTS,
    T1_BLEND_BLUE_SCALE,
    T1_BLEND_BLUE_SHIFT,
    T1_BLEND_FAMILY_BLUES,
    T1_BLEND_FAMILY_OTHER_BLUES,
    T1_BLEND_FORCE_BOLD,

    T1_BLEND_MAX    /* do not remove */

  } T1_Blend_Flags;


  /* these constants are deprecated; use the corresponding */
  /* `T1_Blend_Flags` values instead                       */
#define t1_blend_underline_position   T1_BLEND_UNDERLINE_POSITION
#define t1_blend_underline_thickness  T1_BLEND_UNDERLINE_THICKNESS
#define t1_blend_italic_angle         T1_BLEND_ITALIC_ANGLE
#define t1_blend_blue_values          T1_BLEND_BLUE_VALUES
#define t1_blend_other_blues          T1_BLEND_OTHER_BLUES
#define t1_blend_standard_widths      T1_BLEND_STANDARD_WIDTH
#define t1_blend_standard_height      T1_BLEND_STANDARD_HEIGHT
#define t1_blend_stem_snap_widths     T1_BLEND_STEM_SNAP_WIDTHS
#define t1_blend_stem_snap_heights    T1_BLEND_STEM_SNAP_HEIGHTS
#define t1_blend_blue_scale           T1_BLEND_BLUE_SCALE
#define t1_blend_blue_shift           T1_BLEND_BLUE_SHIFT
#define t1_blend_family_blues         T1_BLEND_FAMILY_BLUES
#define t1_blend_family_other_blues   T1_BLEND_FAMILY_OTHER_BLUES
#define t1_blend_force_bold           T1_BLEND_FORCE_BOLD
#define t1_blend_max                  T1_BLEND_MAX

  /* */


  /**************************************************************************
   *
   * @struct:
   *   CID_FaceDictRec
   *
   * @description:
   *   A structure used to represent data in a CID top-level dictionary.  In
   *   most cases, they are part of the font's '/FDArray' array.  Within a
   *   CID font file, such (internal) subfont dictionaries are enclosed by
   *   '%ADOBeginFontDict' and '%ADOEndFontDict' comments.
   *
   *   Note that `CID_FaceDictRec` misses a field for the '/FontName'
   *   keyword, specifying the subfont's name (the top-level font name is
   *   given by the '/CIDFontName' keyword).  This is an oversight, but it
   *   doesn't limit the 'cid' font module's functionality because FreeType
   *   neither needs this entry nor gives access to CID subfonts.
   */
  typedef struct  CID_FaceDictRec_
  {
    PS_PrivateRec  private_dict;

    FT_UInt        len_buildchar;
    FT_Fixed       forcebold_threshold;
    FT_Pos         stroke_width;
    FT_Fixed       expansion_factor;   /* this is a duplicate of           */
                                       /* `private_dict->expansion_factor' */
    FT_Byte        paint_type;
    FT_Byte        font_type;
    FT_Matrix      font_matrix;
    FT_Vector      font_offset;

    FT_UInt        num_subrs;
    FT_ULong       subrmap_offset;
    FT_UInt        sd_bytes;

  } CID_FaceDictRec;


  /**************************************************************************
   *
   * @struct:
   *   CID_FaceDict
   *
   * @description:
   *   A handle to a @CID_FaceDictRec structure.
   */
  typedef struct CID_FaceDictRec_*  CID_FaceDict;


  /**************************************************************************
   *
   * @struct:
   *   CID_FontDict
   *
   * @description:
   *   This type is equivalent to @CID_FaceDictRec.  It is deprecated but
   *   kept to maintain source compatibility between various versions of
   *   FreeType.
   */
  typedef CID_FaceDictRec  CID_FontDict;


  /**************************************************************************
   *
   * @struct:
   *   CID_FaceInfoRec
   *
   * @description:
   *   A structure used to represent CID Face information.
   */
  typedef struct  CID_FaceInfoRec_
  {
    FT_String*      cid_font_name;
    FT_Fixed        cid_version;
    FT_Int          cid_font_type;

    FT_String*      registry;
    FT_String*      ordering;
    FT_Int          supplement;

    PS_FontInfoRec  font_info;
    FT_BBox         font_bbox;
    FT_ULong        uid_base;

    FT_Int          num_xuid;
    FT_ULong        xuid[16];

    FT_ULong        cidmap_offset;
    FT_UInt         fd_bytes;
    FT_UInt         gd_bytes;
    FT_ULong        cid_count;

    FT_UInt         num_dicts;
    CID_FaceDict    font_dicts;

    FT_ULong        data_offset;

  } CID_FaceInfoRec;


  /**************************************************************************
   *
   * @struct:
   *   CID_FaceInfo
   *
   * @description:
   *   A handle to a @CID_FaceInfoRec structure.
   */
  typedef struct CID_FaceInfoRec_*  CID_FaceInfo;


  /**************************************************************************
   *
   * @struct:
   *   CID_Info
   *
   * @description:
   *  This type is equivalent to @CID_FaceInfoRec.  It is deprecated but kept
   *  to maintain source compatibility between various versions of FreeType.
   */
  typedef CID_FaceInfoRec  CID_Info;


  /**************************************************************************
   *
   * @function:
   *   FT_Has_PS_Glyph_Names
   *
   * @description:
   *   Return true if a given face provides reliable PostScript glyph names.
   *   This is similar to using the @FT_HAS_GLYPH_NAMES macro, except that
   *   certain fonts (mostly TrueType) contain incorrect glyph name tables.
   *
   *   When this function returns true, the caller is sure that the glyph
   *   names returned by @FT_Get_Glyph_Name are reliable.
   *
   * @input:
   *   face ::
   *     face handle
   *
   * @return:
   *   Boolean.  True if glyph names are reliable.
   *
   */
   FT_Int 
  FT_Has_PS_Glyph_Names( FT_Face  face );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_PS_Font_Info
   *
   * @description:
   *   Retrieve the @PS_FontInfoRec structure corresponding to a given
   *   PostScript font.
   *
   * @input:
   *   face ::
   *     PostScript face handle.
   *
   * @output:
   *   afont_info ::
   *     A pointer to a @PS_FontInfoRec object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   String pointers within the @PS_FontInfoRec structure are owned by the
   *   face and don't need to be freed by the caller.  Missing entries in the
   *   font's FontInfo dictionary are represented by `NULL` pointers.
   *
   *   The following font formats support this feature: 'Type~1', 'Type~42',
   *   'CFF', 'CID~Type~1'.  For other font formats this function returns the
   *   `FT_Err_Invalid_Argument` error code.
   *
   * @example:
   *   ```
   *     PS_FontInfoRec  font_info;
   *
   *
   *     error = FT_Get_PS_Font_Info( face, &font_info );
   *     ...
   *   ```
   *
   */
   FT_Error 
  FT_Get_PS_Font_Info( FT_Face      face,
                       PS_FontInfo  afont_info );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_PS_Font_Private
   *
   * @description:
   *   Retrieve the @PS_PrivateRec structure corresponding to a given
   *   PostScript font.
   *
   * @input:
   *   face ::
   *     PostScript face handle.
   *
   * @output:
   *   afont_private ::
   *     A pointer to a @PS_PrivateRec object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The string pointers within the @PS_PrivateRec structure are owned by
   *   the face and don't need to be freed by the caller.
   *
   *   Only the 'Type~1' font format supports this feature.  For other font
   *   formats this function returns the `FT_Err_Invalid_Argument` error
   *   code.
   *
   * @example:
   *   ```
   *     PS_PrivateRec  font_private;
   *
   *
   *     error = FT_Get_PS_Font_Private( face, &font_private );
   *     ...
   *   ```
   *
   */
   FT_Error 
  FT_Get_PS_Font_Private( FT_Face     face,
                          PS_Private  afont_private );


  /**************************************************************************
   *
   * @enum:
   *   T1_EncodingType
   *
   * @description:
   *   An enumeration describing the 'Encoding' entry in a Type 1 dictionary.
   *
   * @values:
   *   T1_ENCODING_TYPE_NONE ::
   *   T1_ENCODING_TYPE_ARRAY ::
   *   T1_ENCODING_TYPE_STANDARD ::
   *   T1_ENCODING_TYPE_ISOLATIN1 ::
   *   T1_ENCODING_TYPE_EXPERT ::
   *
   * @since:
   *   2.4.8
   */
  typedef enum  T1_EncodingType_
  {
    T1_ENCODING_TYPE_NONE = 0,
    T1_ENCODING_TYPE_ARRAY,
    T1_ENCODING_TYPE_STANDARD,
    T1_ENCODING_TYPE_ISOLATIN1,
    T1_ENCODING_TYPE_EXPERT

  } T1_EncodingType;


  /**************************************************************************
   *
   * @enum:
   *   PS_Dict_Keys
   *
   * @description:
   *   An enumeration used in calls to @FT_Get_PS_Font_Value to identify the
   *   Type~1 dictionary entry to retrieve.
   *
   * @values:
   *   PS_DICT_FONT_TYPE ::
   *   PS_DICT_FONT_MATRIX ::
   *   PS_DICT_FONT_BBOX ::
   *   PS_DICT_PAINT_TYPE ::
   *   PS_DICT_FONT_NAME ::
   *   PS_DICT_UNIQUE_ID ::
   *   PS_DICT_NUM_CHAR_STRINGS ::
   *   PS_DICT_CHAR_STRING_KEY ::
   *   PS_DICT_CHAR_STRING ::
   *   PS_DICT_ENCODING_TYPE ::
   *   PS_DICT_ENCODING_ENTRY ::
   *   PS_DICT_NUM_SUBRS ::
   *   PS_DICT_SUBR ::
   *   PS_DICT_STD_HW ::
   *   PS_DICT_STD_VW ::
   *   PS_DICT_NUM_BLUE_VALUES ::
   *   PS_DICT_BLUE_VALUE ::
   *   PS_DICT_BLUE_FUZZ ::
   *   PS_DICT_NUM_OTHER_BLUES ::
   *   PS_DICT_OTHER_BLUE ::
   *   PS_DICT_NUM_FAMILY_BLUES ::
   *   PS_DICT_FAMILY_BLUE ::
   *   PS_DICT_NUM_FAMILY_OTHER_BLUES ::
   *   PS_DICT_FAMILY_OTHER_BLUE ::
   *   PS_DICT_BLUE_SCALE ::
   *   PS_DICT_BLUE_SHIFT ::
   *   PS_DICT_NUM_STEM_SNAP_H ::
   *   PS_DICT_STEM_SNAP_H ::
   *   PS_DICT_NUM_STEM_SNAP_V ::
   *   PS_DICT_STEM_SNAP_V ::
   *   PS_DICT_FORCE_BOLD ::
   *   PS_DICT_RND_STEM_UP ::
   *   PS_DICT_MIN_FEATURE ::
   *   PS_DICT_LEN_IV ::
   *   PS_DICT_PASSWORD ::
   *   PS_DICT_LANGUAGE_GROUP ::
   *   PS_DICT_VERSION ::
   *   PS_DICT_NOTICE ::
   *   PS_DICT_FULL_NAME ::
   *   PS_DICT_FAMILY_NAME ::
   *   PS_DICT_WEIGHT ::
   *   PS_DICT_IS_FIXED_PITCH ::
   *   PS_DICT_UNDERLINE_POSITION ::
   *   PS_DICT_UNDERLINE_THICKNESS ::
   *   PS_DICT_FS_TYPE ::
   *   PS_DICT_ITALIC_ANGLE ::
   *
   * @since:
   *   2.4.8
   */
  typedef enum  PS_Dict_Keys_
  {
    /* conventionally in the font dictionary */
    PS_DICT_FONT_TYPE,              /* FT_Byte         */
    PS_DICT_FONT_MATRIX,            /* FT_Fixed        */
    PS_DICT_FONT_BBOX,              /* FT_Fixed        */
    PS_DICT_PAINT_TYPE,             /* FT_Byte         */
    PS_DICT_FONT_NAME,              /* FT_String*      */
    PS_DICT_UNIQUE_ID,              /* FT_Int          */
    PS_DICT_NUM_CHAR_STRINGS,       /* FT_Int          */
    PS_DICT_CHAR_STRING_KEY,        /* FT_String*      */
    PS_DICT_CHAR_STRING,            /* FT_String*      */
    PS_DICT_ENCODING_TYPE,          /* T1_EncodingType */
    PS_DICT_ENCODING_ENTRY,         /* FT_String*      */

    /* conventionally in the font Private dictionary */
    PS_DICT_NUM_SUBRS,              /* FT_Int     */
    PS_DICT_SUBR,                   /* FT_String* */
    PS_DICT_STD_HW,                 /* FT_UShort  */
    PS_DICT_STD_VW,                 /* FT_UShort  */
    PS_DICT_NUM_BLUE_VALUES,        /* FT_Byte    */
    PS_DICT_BLUE_VALUE,             /* FT_Short   */
    PS_DICT_BLUE_FUZZ,              /* FT_Int     */
    PS_DICT_NUM_OTHER_BLUES,        /* FT_Byte    */
    PS_DICT_OTHER_BLUE,             /* FT_Short   */
    PS_DICT_NUM_FAMILY_BLUES,       /* FT_Byte    */
    PS_DICT_FAMILY_BLUE,            /* FT_Short   */
    PS_DICT_NUM_FAMILY_OTHER_BLUES, /* FT_Byte    */
    PS_DICT_FAMILY_OTHER_BLUE,      /* FT_Short   */
    PS_DICT_BLUE_SCALE,             /* FT_Fixed   */
    PS_DICT_BLUE_SHIFT,             /* FT_Int     */
    PS_DICT_NUM_STEM_SNAP_H,        /* FT_Byte    */
    PS_DICT_STEM_SNAP_H,            /* FT_Short   */
    PS_DICT_NUM_STEM_SNAP_V,        /* FT_Byte    */
    PS_DICT_STEM_SNAP_V,            /* FT_Short   */
    PS_DICT_FORCE_BOLD,             /* FT_Bool    */
    PS_DICT_RND_STEM_UP,            /* FT_Bool    */
    PS_DICT_MIN_FEATURE,            /* FT_Short   */
    PS_DICT_LEN_IV,                 /* FT_Int     */
    PS_DICT_PASSWORD,               /* FT_Long    */
    PS_DICT_LANGUAGE_GROUP,         /* FT_Long    */

    /* conventionally in the font FontInfo dictionary */
    PS_DICT_VERSION,                /* FT_String* */
    PS_DICT_NOTICE,                 /* FT_String* */
    PS_DICT_FULL_NAME,              /* FT_String* */
    PS_DICT_FAMILY_NAME,            /* FT_String* */
    PS_DICT_WEIGHT,                 /* FT_String* */
    PS_DICT_IS_FIXED_PITCH,         /* FT_Bool    */
    PS_DICT_UNDERLINE_POSITION,     /* FT_Short   */
    PS_DICT_UNDERLINE_THICKNESS,    /* FT_UShort  */
    PS_DICT_FS_TYPE,                /* FT_UShort  */
    PS_DICT_ITALIC_ANGLE,           /* FT_Long    */

    PS_DICT_MAX = PS_DICT_ITALIC_ANGLE

  } PS_Dict_Keys;


  /**************************************************************************
   *
   * @function:
   *   FT_Get_PS_Font_Value
   *
   * @description:
   *   Retrieve the value for the supplied key from a PostScript font.
   *
   * @input:
   *   face ::
   *     PostScript face handle.
   *
   *   key ::
   *     An enumeration value representing the dictionary key to retrieve.
   *
   *   idx ::
   *     For array values, this specifies the index to be returned.
   *
   *   value ::
   *     A pointer to memory into which to write the value.
   *
   *   valen_len ::
   *     The size, in bytes, of the memory supplied for the value.
   *
   * @output:
   *   value ::
   *     The value matching the above key, if it exists.
   *
   * @return:
   *   The amount of memory (in bytes) required to hold the requested value
   *   (if it exists, -1 otherwise).
   *
   * @note:
   *   The values returned are not pointers into the internal structures of
   *   the face, but are 'fresh' copies, so that the memory containing them
   *   belongs to the calling application.  This also enforces the
   *   'read-only' nature of these values, i.e., this function cannot be
   *   used to manipulate the face.
   *
   *   `value` is a void pointer because the values returned can be of
   *   various types.
   *
   *   If either `value` is `NULL` or `value_len` is too small, just the
   *   required memory size for the requested entry is returned.
   *
   *   The `idx` parameter is used, not only to retrieve elements of, for
   *   example, the FontMatrix or FontBBox, but also to retrieve name keys
   *   from the CharStrings dictionary, and the charstrings themselves.  It
   *   is ignored for atomic values.
   *
   *   `PS_DICT_BLUE_SCALE` returns a value that is scaled up by 1000.  To
   *   get the value as in the font stream, you need to divide by 65536000.0
   *   (to remove the FT_Fixed scale, and the x1000 scale).
   *
   *   IMPORTANT: Only key/value pairs read by the FreeType interpreter can
   *   be retrieved.  So, for example, PostScript procedures such as NP, ND,
   *   and RD are not available.  Arbitrary keys are, obviously, not be
   *   available either.
   *
   *   If the font's format is not PostScript-based, this function returns
   *   the `FT_Err_Invalid_Argument` error code.
   *
   * @since:
   *   2.4.8
   *
   */
   FT_Long 
  FT_Get_PS_Font_Value( FT_Face       face,
                        PS_Dict_Keys  key,
                        FT_UInt       idx,
                        void         *value,
                        FT_Long       value_len );

  /* */



#endif /* T1TABLES_H_ */


/* END */
//
// ===========================  ftchapters.h  ===========================
//
/****************************************************************************
 *
 * This file defines the structure of the FreeType reference.
 * It is used by the python script that generates the HTML files.
 *
 */


  /**************************************************************************
   *
   * @chapter:
   *   general_remarks
   *
   * @title:
   *   General Remarks
   *
   * @sections:
   *   preamble
   *   header_inclusion
   *   user_allocation
   *
   */


  /**************************************************************************
   *
   * @chapter:
   *   core_api
   *
   * @title:
   *   Core API
   *
   * @sections:
   *   basic_types
   *   library_setup
   *   face_creation
   *   font_testing_macros
   *   sizing_and_scaling
   *   glyph_retrieval
   *   character_mapping
   *   information_retrieval
   *   other_api_data
   *
   */


  /**************************************************************************
   *
   * @chapter:
   *   extended_api
   *
   * @title:
   *   Extended API
   *
   * @sections:
   *   glyph_variants
   *   color_management
   *   layer_management
   *   glyph_management
   *   mac_specific
   *   sizes_management
   *   header_file_macros
   *
   */


  /**************************************************************************
   *
   * @chapter:
   *   format_specific
   *
   * @title:
   *   Format-Specific API
   *
   * @sections:
   *   multiple_masters
   *   truetype_tables
   *   type1_tables
   *   sfnt_names
   *   bdf_fonts
   *   cid_fonts
   *   pfr_fonts
   *   winfnt_fonts
   *   svg_fonts
   *   font_formats
   *   gasp_table
   *
   */


  /**************************************************************************
   *
   * @chapter:
   *   module_specific
   *
   * @title:
   *   Controlling FreeType Modules
   *
   * @sections:
   *   auto_hinter
   *   cff_driver
   *   t1_cid_driver
   *   tt_driver
   *   pcf_driver
   *   ot_svg_driver
   *   properties
   *   parameter_tags
   *   lcd_rendering
   *
   */


  /**************************************************************************
   *
   * @chapter:
   *   cache_subsystem
   *
   * @title:
   *   Cache Sub-System
   *
   * @sections:
   *   cache_subsystem
   *
   */


  /**************************************************************************
   *
   * @chapter:
   *   support_api
   *
   * @title:
   *   Support API
   *
   * @sections:
   *   computations
   *   list_processing
   *   outline_processing
   *   quick_advance
   *   bitmap_handling
   *   raster
   *   glyph_stroker
   *   system_interface
   *   module_management
   *   gzip
   *   lzw
   *   bzip2
   *   debugging_apis
   *
   */


  /**************************************************************************
   *
   * @chapter:
   *   error_codes
   *
   * @title:
   *   Error Codes
   *
   * @sections:
   *   error_enumerations
   *   error_code_values
   *
   */


/* END */
//
// ===========================  ftmm.h  ===========================
//
/****************************************************************************
 *
 * ftmm.h
 *
 *   FreeType Multiple Master font interface (specification).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTMM_H_
#define FTMM_H_

#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   multiple_masters
   *
   * @title:
   *   Multiple Masters
   *
   * @abstract:
   *   How to manage Multiple Masters fonts.
   *
   * @description:
   *   The following types and functions are used to manage Multiple Master
   *   fonts, i.e., the selection of specific design instances by setting
   *   design axis coordinates.
   *
   *   Besides Adobe MM fonts, the interface supports Apple's TrueType GX and
   *   OpenType variation fonts.  Some of the routines only work with Adobe
   *   MM fonts, others will work with all three types.  They are similar
   *   enough that a consistent interface makes sense.
   *
   *   For Adobe MM fonts, macro @FT_IS_SFNT returns false.  For GX and
   *   OpenType variation fonts, it returns true.
   *
   */


  /**************************************************************************
   *
   * @enum:
   *   T1_MAX_MM_XXX
   *
   * @description:
   *   Multiple Masters limits as defined in their specifications.
   *
   * @values:
   *   T1_MAX_MM_AXIS ::
   *     The maximum number of Multiple Masters axes.
   *
   *   T1_MAX_MM_DESIGNS ::
   *     The maximum number of Multiple Masters designs.
   *
   *   T1_MAX_MM_MAP_POINTS ::
   *     The maximum number of elements in a design map.
   *
   */
#define T1_MAX_MM_AXIS         4
#define T1_MAX_MM_DESIGNS     16
#define T1_MAX_MM_MAP_POINTS  20


  /**************************************************************************
   *
   * @struct:
   *   FT_MM_Axis
   *
   * @description:
   *   A structure to model a given axis in design space for Multiple Masters
   *   fonts.
   *
   *   This structure can't be used for TrueType GX or OpenType variation
   *   fonts.
   *
   * @fields:
   *   name ::
   *     The axis's name.
   *
   *   minimum ::
   *     The axis's minimum design coordinate.
   *
   *   maximum ::
   *     The axis's maximum design coordinate.
   */
  typedef struct  FT_MM_Axis_
  {
    FT_String*  name;
    FT_Long     minimum;
    FT_Long     maximum;

  } FT_MM_Axis;


  /**************************************************************************
   *
   * @struct:
   *   FT_Multi_Master
   *
   * @description:
   *   A structure to model the axes and space of a Multiple Masters font.
   *
   *   This structure can't be used for TrueType GX or OpenType variation
   *   fonts.
   *
   * @fields:
   *   num_axis ::
   *     Number of axes.  Cannot exceed~4.
   *
   *   num_designs ::
   *     Number of designs; should be normally 2^num_axis even though the
   *     Type~1 specification strangely allows for intermediate designs to be
   *     present.  This number cannot exceed~16.
   *
   *   axis ::
   *     A table of axis descriptors.
   */
  typedef struct  FT_Multi_Master_
  {
    FT_UInt     num_axis;
    FT_UInt     num_designs;
    FT_MM_Axis  axis[T1_MAX_MM_AXIS];

  } FT_Multi_Master;


  /**************************************************************************
   *
   * @struct:
   *   FT_Var_Axis
   *
   * @description:
   *   A structure to model a given axis in design space for Multiple
   *   Masters, TrueType GX, and OpenType variation fonts.
   *
   * @fields:
   *   name ::
   *     The axis's name.  Not always meaningful for TrueType GX or OpenType
   *     variation fonts.
   *
   *   minimum ::
   *     The axis's minimum design coordinate.
   *
   *   def ::
   *     The axis's default design coordinate.  FreeType computes meaningful
   *     default values for Adobe MM fonts.
   *
   *   maximum ::
   *     The axis's maximum design coordinate.
   *
   *   tag ::
   *     The axis's tag (the equivalent to 'name' for TrueType GX and
   *     OpenType variation fonts).  FreeType provides default values for
   *     Adobe MM fonts if possible.
   *
   *   strid ::
   *     The axis name entry in the font's 'name' table.  This is another
   *     (and often better) version of the 'name' field for TrueType GX or
   *     OpenType variation fonts.  Not meaningful for Adobe MM fonts.
   *
   * @note:
   *   The fields `minimum`, `def`, and `maximum` are 16.16 fractional values
   *   for TrueType GX and OpenType variation fonts.  For Adobe MM fonts, the
   *   values are whole numbers (i.e., the fractional part is zero).
   */
  typedef struct  FT_Var_Axis_
  {
    FT_String*  name;

    FT_Fixed    minimum;
    FT_Fixed    def;
    FT_Fixed    maximum;

    FT_ULong    tag;
    FT_UInt     strid;

  } FT_Var_Axis;


  /**************************************************************************
   *
   * @struct:
   *   FT_Var_Named_Style
   *
   * @description:
   *   A structure to model a named instance in a TrueType GX or OpenType
   *   variation font.
   *
   *   This structure can't be used for Adobe MM fonts.
   *
   * @fields:
   *   coords ::
   *     The design coordinates for this instance.  This is an array with one
   *     entry for each axis.
   *
   *   strid ::
   *     The entry in 'name' table identifying this instance.
   *
   *   psid ::
   *     The entry in 'name' table identifying a PostScript name for this
   *     instance.  Value 0xFFFF indicates a missing entry.
   */
  typedef struct  FT_Var_Named_Style_
  {
    FT_Fixed*  coords;
    FT_UInt    strid;
    FT_UInt    psid;   /* since 2.7.1 */

  } FT_Var_Named_Style;


  /**************************************************************************
   *
   * @struct:
   *   FT_MM_Var
   *
   * @description:
   *   A structure to model the axes and space of an Adobe MM, TrueType GX,
   *   or OpenType variation font.
   *
   *   Some fields are specific to one format and not to the others.
   *
   * @fields:
   *   num_axis ::
   *     The number of axes.  The maximum value is~4 for Adobe MM fonts; no
   *     limit in TrueType GX or OpenType variation fonts.
   *
   *   num_designs ::
   *     The number of designs; should be normally 2^num_axis for Adobe MM
   *     fonts.  Not meaningful for TrueType GX or OpenType variation fonts
   *     (where every glyph could have a different number of designs).
   *
   *   num_namedstyles ::
   *     The number of named styles; a 'named style' is a tuple of design
   *     coordinates that has a string ID (in the 'name' table) associated
   *     with it.  The font can tell the user that, for example,
   *     [Weight=1.5,Width=1.1] is 'Bold'.  Another name for 'named style' is
   *     'named instance'.
   *
   *     For Adobe Multiple Masters fonts, this value is always zero because
   *     the format does not support named styles.
   *
   *   axis ::
   *     An axis descriptor table.  TrueType GX and OpenType variation fonts
   *     contain slightly more data than Adobe MM fonts.  Memory management
   *     of this pointer is done internally by FreeType.
   *
   *   namedstyle ::
   *     A named style (instance) table.  Only meaningful for TrueType GX and
   *     OpenType variation fonts.  Memory management of this pointer is done
   *     internally by FreeType.
   */
  typedef struct  FT_MM_Var_
  {
    FT_UInt              num_axis;
    FT_UInt              num_designs;
    FT_UInt              num_namedstyles;
    FT_Var_Axis*         axis;
    FT_Var_Named_Style*  namedstyle;

  } FT_MM_Var;


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Multi_Master
   *
   * @description:
   *   Retrieve a variation descriptor of a given Adobe MM font.
   *
   *   This function can't be used with TrueType GX or OpenType variation
   *   fonts.
   *
   * @input:
   *   face ::
   *     A handle to the source face.
   *
   * @output:
   *   amaster ::
   *     The Multiple Masters descriptor.
   *
   * @return:
   *   FreeType error code.  0~means success.
   */
   FT_Error 
  FT_Get_Multi_Master( FT_Face           face,
                       FT_Multi_Master  *amaster );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_MM_Var
   *
   * @description:
   *   Retrieve a variation descriptor for a given font.
   *
   *   This function works with all supported variation formats.
   *
   * @input:
   *   face ::
   *     A handle to the source face.
   *
   * @output:
   *   amaster ::
   *     The variation descriptor.  Allocates a data structure, which the
   *     user must deallocate with a call to @FT_Done_MM_Var after use.
   *
   * @return:
   *   FreeType error code.  0~means success.
   */
   FT_Error 
  FT_Get_MM_Var( FT_Face      face,
                 FT_MM_Var*  *amaster );


  /**************************************************************************
   *
   * @function:
   *   FT_Done_MM_Var
   *
   * @description:
   *   Free the memory allocated by @FT_Get_MM_Var.
   *
   * @input:
   *   library ::
   *     A handle of the face's parent library object that was used in the
   *     call to @FT_Get_MM_Var to create `amaster`.
   *
   * @return:
   *   FreeType error code.  0~means success.
   */
   FT_Error 
  FT_Done_MM_Var( FT_Library   library,
                  FT_MM_Var   *amaster );


  /**************************************************************************
   *
   * @function:
   *   FT_Set_MM_Design_Coordinates
   *
   * @description:
   *   For Adobe MM fonts, choose an interpolated font design through design
   *   coordinates.
   *
   *   This function can't be used with TrueType GX or OpenType variation
   *   fonts.
   *
   * @inout:
   *   face ::
   *     A handle to the source face.
   *
   * @input:
   *   num_coords ::
   *     The number of available design coordinates.  If it is larger than
   *     the number of axes, ignore the excess values.  If it is smaller than
   *     the number of axes, use default values for the remaining axes.
   *
   *   coords ::
   *     An array of design coordinates.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   [Since 2.8.1] To reset all axes to the default values, call the
   *   function with `num_coords` set to zero and `coords` set to `NULL`.
   *
   *   [Since 2.9] If `num_coords` is larger than zero, this function sets
   *   the @FT_FACE_FLAG_VARIATION bit in @FT_Face's `face_flags` field
   *   (i.e., @FT_IS_VARIATION will return true).  If `num_coords` is zero,
   *   this bit flag gets unset.
   */
   FT_Error 
  FT_Set_MM_Design_Coordinates( FT_Face   face,
                                FT_UInt   num_coords,
                                FT_Long*  coords );


  /**************************************************************************
   *
   * @function:
   *   FT_Set_Var_Design_Coordinates
   *
   * @description:
   *   Choose an interpolated font design through design coordinates.
   *
   *   This function works with all supported variation formats.
   *
   * @inout:
   *   face ::
   *     A handle to the source face.
   *
   * @input:
   *   num_coords ::
   *     The number of available design coordinates.  If it is larger than
   *     the number of axes, ignore the excess values.  If it is smaller than
   *     the number of axes, use default values for the remaining axes.
   *
   *   coords ::
   *     An array of design coordinates.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The design coordinates are 16.16 fractional values for TrueType GX and
   *   OpenType variation fonts.  For Adobe MM fonts, the values are supposed
   *   to be whole numbers (i.e., the fractional part is zero).
   *
   *   [Since 2.8.1] To reset all axes to the default values, call the
   *   function with `num_coords` set to zero and `coords` set to `NULL`.
   *   [Since 2.9] 'Default values' means the currently selected named
   *   instance (or the base font if no named instance is selected).
   *
   *   [Since 2.9] If `num_coords` is larger than zero, this function sets
   *   the @FT_FACE_FLAG_VARIATION bit in @FT_Face's `face_flags` field
   *   (i.e., @FT_IS_VARIATION will return true).  If `num_coords` is zero,
   *   this bit flag gets unset.
   */
   FT_Error 
  FT_Set_Var_Design_Coordinates( FT_Face    face,
                                 FT_UInt    num_coords,
                                 FT_Fixed*  coords );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Var_Design_Coordinates
   *
   * @description:
   *   Get the design coordinates of the currently selected interpolated
   *   font.
   *
   *   This function works with all supported variation formats.
   *
   * @input:
   *   face ::
   *     A handle to the source face.
   *
   *   num_coords ::
   *     The number of design coordinates to retrieve.  If it is larger than
   *     the number of axes, set the excess values to~0.
   *
   * @output:
   *   coords ::
   *     The design coordinates array.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The design coordinates are 16.16 fractional values for TrueType GX and
   *   OpenType variation fonts.  For Adobe MM fonts, the values are whole
   *   numbers (i.e., the fractional part is zero).
   *
   * @since:
   *   2.7.1
   */
   FT_Error 
  FT_Get_Var_Design_Coordinates( FT_Face    face,
                                 FT_UInt    num_coords,
                                 FT_Fixed*  coords );


  /**************************************************************************
   *
   * @function:
   *   FT_Set_MM_Blend_Coordinates
   *
   * @description:
   *   Choose an interpolated font design through normalized blend
   *   coordinates.
   *
   *   This function works with all supported variation formats.
   *
   * @inout:
   *   face ::
   *     A handle to the source face.
   *
   * @input:
   *   num_coords ::
   *     The number of available design coordinates.  If it is larger than
   *     the number of axes, ignore the excess values.  If it is smaller than
   *     the number of axes, use default values for the remaining axes.
   *
   *   coords ::
   *     The design coordinates array.  Each element is a 16.16 fractional
   *     value and must be between 0 and 1.0 for Adobe MM fonts, and between
   *     -1.0 and 1.0 for TrueType GX and OpenType variation fonts.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   [Since 2.8.1] To reset all axes to the default values, call the
   *   function with `num_coords` set to zero and `coords` set to `NULL`.
   *   [Since 2.9] 'Default values' means the currently selected named
   *   instance (or the base font if no named instance is selected).
   *
   *   [Since 2.9] If `num_coords` is larger than zero, this function sets
   *   the @FT_FACE_FLAG_VARIATION bit in @FT_Face's `face_flags` field
   *   (i.e., @FT_IS_VARIATION will return true).  If `num_coords` is zero,
   *   this bit flag gets unset.
   */
   FT_Error 
  FT_Set_MM_Blend_Coordinates( FT_Face    face,
                               FT_UInt    num_coords,
                               FT_Fixed*  coords );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_MM_Blend_Coordinates
   *
   * @description:
   *   Get the normalized blend coordinates of the currently selected
   *   interpolated font.
   *
   *   This function works with all supported variation formats.
   *
   * @input:
   *   face ::
   *     A handle to the source face.
   *
   *   num_coords ::
   *     The number of normalized blend coordinates to retrieve.  If it is
   *     larger than the number of axes, set the excess values to~0.5 for
   *     Adobe MM fonts, and to~0 for TrueType GX and OpenType variation
   *     fonts.
   *
   * @output:
   *   coords ::
   *     The normalized blend coordinates array (as 16.16 fractional values).
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @since:
   *   2.7.1
   */
   FT_Error 
  FT_Get_MM_Blend_Coordinates( FT_Face    face,
                               FT_UInt    num_coords,
                               FT_Fixed*  coords );


  /**************************************************************************
   *
   * @function:
   *   FT_Set_Var_Blend_Coordinates
   *
   * @description:
   *   This is another name of @FT_Set_MM_Blend_Coordinates.
   */
   FT_Error 
  FT_Set_Var_Blend_Coordinates( FT_Face    face,
                                FT_UInt    num_coords,
                                FT_Fixed*  coords );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Var_Blend_Coordinates
   *
   * @description:
   *   This is another name of @FT_Get_MM_Blend_Coordinates.
   *
   * @since:
   *   2.7.1
   */
   FT_Error 
  FT_Get_Var_Blend_Coordinates( FT_Face    face,
                                FT_UInt    num_coords,
                                FT_Fixed*  coords );


  /**************************************************************************
   *
   * @function:
   *   FT_Set_MM_WeightVector
   *
   * @description:
   *   For Adobe MM fonts, choose an interpolated font design by directly
   *   setting the weight vector.
   *
   *   This function can't be used with TrueType GX or OpenType variation
   *   fonts.
   *
   * @inout:
   *   face ::
   *     A handle to the source face.
   *
   * @input:
   *   len ::
   *     The length of the weight vector array.  If it is larger than the
   *     number of designs, the extra values are ignored.  If it is less than
   *     the number of designs, the remaining values are set to zero.
   *
   *   weightvector ::
   *     An array representing the weight vector.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   Adobe Multiple Master fonts limit the number of designs, and thus the
   *   length of the weight vector to 16~elements.
   *
   *   If `len` is larger than zero, this function sets the
   *   @FT_FACE_FLAG_VARIATION bit in @FT_Face's `face_flags` field (i.e.,
   *   @FT_IS_VARIATION will return true).  If `len` is zero, this bit flag
   *   is unset and the weight vector array is reset to the default values.
   *
   *   The Adobe documentation also states that the values in the
   *   WeightVector array must total 1.0 +/-~0.001.  In practice this does
   *   not seem to be enforced, so is not enforced here, either.
   *
   * @since:
   *   2.10
   */
   FT_Error 
  FT_Set_MM_WeightVector( FT_Face    face,
                          FT_UInt    len,
                          FT_Fixed*  weightvector );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_MM_WeightVector
   *
   * @description:
   *   For Adobe MM fonts, retrieve the current weight vector of the font.
   *
   *   This function can't be used with TrueType GX or OpenType variation
   *   fonts.
   *
   * @inout:
   *   face ::
   *     A handle to the source face.
   *
   *   len ::
   *     A pointer to the size of the array to be filled.  If the size of the
   *     array is less than the number of designs, `FT_Err_Invalid_Argument`
   *     is returned, and `len` is set to the required size (the number of
   *     designs).  If the size of the array is greater than the number of
   *     designs, the remaining entries are set to~0.  On successful
   *     completion, `len` is set to the number of designs (i.e., the number
   *     of values written to the array).
   *
   * @output:
   *   weightvector ::
   *     An array to be filled.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   Adobe Multiple Master fonts limit the number of designs, and thus the
   *   length of the WeightVector to~16.
   *
   * @since:
   *   2.10
   */
   FT_Error 
  FT_Get_MM_WeightVector( FT_Face    face,
                          FT_UInt*   len,
                          FT_Fixed*  weightvector );


  /**************************************************************************
   *
   * @enum:
   *   FT_VAR_AXIS_FLAG_XXX
   *
   * @description:
   *   A list of bit flags used in the return value of
   *   @FT_Get_Var_Axis_Flags.
   *
   * @values:
   *   FT_VAR_AXIS_FLAG_HIDDEN ::
   *     The variation axis should not be exposed to user interfaces.
   *
   * @since:
   *   2.8.1
   */
#define FT_VAR_AXIS_FLAG_HIDDEN  1


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Var_Axis_Flags
   *
   * @description:
   *   Get the 'flags' field of an OpenType Variation Axis Record.
   *
   *   Not meaningful for Adobe MM fonts (`*flags` is always zero).
   *
   * @input:
   *   master ::
   *     The variation descriptor.
   *
   *   axis_index ::
   *     The index of the requested variation axis.
   *
   * @output:
   *   flags ::
   *     The 'flags' field.  See @FT_VAR_AXIS_FLAG_XXX for possible values.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @since:
   *   2.8.1
   */
   FT_Error 
  FT_Get_Var_Axis_Flags( FT_MM_Var*  master,
                         FT_UInt     axis_index,
                         FT_UInt*    flags );


  /**************************************************************************
   *
   * @function:
   *   FT_Set_Named_Instance
   *
   * @description:
   *   Set or change the current named instance.
   *
   * @input:
   *   face ::
   *     A handle to the source face.
   *
   *   instance_index ::
   *     The index of the requested instance, starting with value 1.  If set
   *     to value 0, FreeType switches to font access without a named
   *     instance.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The function uses the value of `instance_index` to set bits 16-30 of
   *   the face's `face_index` field.  It also resets any variation applied
   *   to the font, and the @FT_FACE_FLAG_VARIATION bit of the face's
   *   `face_flags` field gets reset to zero (i.e., @FT_IS_VARIATION will
   *   return false).
   *
   *   For Adobe MM fonts (which don't have named instances) this function
   *   simply resets the current face to the default instance.
   *
   * @since:
   *   2.9
   */
   FT_Error 
  FT_Set_Named_Instance( FT_Face  face,
                         FT_UInt  instance_index );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Default_Named_Instance
   *
   * @description:
   *   Retrieve the index of the default named instance, to be used with
   *   @FT_Set_Named_Instance.
   *
   *   The default instance of a variation font is that instance for which
   *   the nth axis coordinate is equal to `axis[n].def` (as specified in the
   *   @FT_MM_Var structure), with~n covering all axes.
   *
   *   FreeType synthesizes a named instance for the default instance if the
   *   font does not contain such an entry.
   *
   * @input:
   *   face ::
   *     A handle to the source face.
   *
   * @output:
   *   instance_index ::
   *     The index of the default named instance.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   For Adobe MM fonts (which don't have named instances) this function
   *   always returns zero for `instance_index`.
   *
   * @since:
   *   2.13.1
   */
   FT_Error 
  FT_Get_Default_Named_Instance( FT_Face   face,
                                 FT_UInt  *instance_index );

  /* */




#endif /* FTMM_H_ */


/* END */
//
// ===========================  fttypes.h  ===========================
//
/****************************************************************************
 *
 * fttypes.h
 *
 *   FreeType simple types definitions (specification only).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTTYPES_H_
#define FTTYPES_H_


#include <ft2build.h>
#include FT_CONFIG_CONFIG_H
#include <freetype/ftsystem.h>
#include <freetype/ftimage.h>

#include <stddef.h>





  /**************************************************************************
   *
   * @section:
   *   basic_types
   *
   * @title:
   *   Basic Data Types
   *
   * @abstract:
   *   The basic data types defined by the library.
   *
   * @description:
   *   This section contains the basic data types defined by FreeType~2,
   *   ranging from simple scalar types to bitmap descriptors.  More
   *   font-specific structures are defined in a different section.  Note
   *   that FreeType does not use floating-point data types.  Fractional
   *   values are represented by fixed-point integers, with lower bits
   *   storing the fractional part.
   *
   * @order:
   *   FT_Byte
   *   FT_Bytes
   *   FT_Char
   *   FT_Int
   *   FT_UInt
   *   FT_Int16
   *   FT_UInt16
   *   FT_Int32
   *   FT_UInt32
   *   FT_Int64
   *   FT_UInt64
   *   FT_Short
   *   FT_UShort
   *   FT_Long
   *   FT_ULong
   *   FT_Bool
   *   FT_Offset
   *   FT_PtrDist
   *   FT_String
   *   FT_Tag
   *   FT_Error
   *   FT_Fixed
   *   FT_Pointer
   *   FT_Pos
   *   FT_Vector
   *   FT_BBox
   *   FT_Matrix
   *   FT_FWord
   *   FT_UFWord
   *   FT_F2Dot14
   *   FT_UnitVector
   *   FT_F26Dot6
   *   FT_Data
   *
   *   FT_MAKE_TAG
   *
   *   FT_Generic
   *   FT_Generic_Finalizer
   *
   *   FT_Bitmap
   *   FT_Pixel_Mode
   *   FT_Palette_Mode
   *   FT_Glyph_Format
   *   FT_IMAGE_TAG
   *
   */


  /**************************************************************************
   *
   * @type:
   *   FT_Bool
   *
   * @description:
   *   A typedef of unsigned char, used for simple booleans.  As usual,
   *   values 1 and~0 represent true and false, respectively.
   */
  typedef unsigned char  FT_Bool;


  /**************************************************************************
   *
   * @type:
   *   FT_FWord
   *
   * @description:
   *   A signed 16-bit integer used to store a distance in original font
   *   units.
   */
  typedef signed short  FT_FWord;   /* distance in FUnits */


  /**************************************************************************
   *
   * @type:
   *   FT_UFWord
   *
   * @description:
   *   An unsigned 16-bit integer used to store a distance in original font
   *   units.
   */
  typedef unsigned short  FT_UFWord;  /* unsigned distance */


  /**************************************************************************
   *
   * @type:
   *   FT_Char
   *
   * @description:
   *   A simple typedef for the _signed_ char type.
   */
  typedef signed char  FT_Char;


  /**************************************************************************
   *
   * @type:
   *   FT_Byte
   *
   * @description:
   *   A simple typedef for the _unsigned_ char type.
   */
  typedef unsigned char  FT_Byte;


  /**************************************************************************
   *
   * @type:
   *   FT_Bytes
   *
   * @description:
   *   A typedef for constant memory areas.
   */
  typedef const FT_Byte*  FT_Bytes;


  /**************************************************************************
   *
   * @type:
   *   FT_Tag
   *
   * @description:
   *   A typedef for 32-bit tags (as used in the SFNT format).
   */
  typedef FT_UInt32  FT_Tag;


  /**************************************************************************
   *
   * @type:
   *   FT_String
   *
   * @description:
   *   A simple typedef for the char type, usually used for strings.
   */
  typedef char  FT_String;


  /**************************************************************************
   *
   * @type:
   *   FT_Short
   *
   * @description:
   *   A typedef for signed short.
   */
  typedef signed short  FT_Short;


  /**************************************************************************
   *
   * @type:
   *   FT_UShort
   *
   * @description:
   *   A typedef for unsigned short.
   */
  typedef unsigned short  FT_UShort;


  /**************************************************************************
   *
   * @type:
   *   FT_Int
   *
   * @description:
   *   A typedef for the int type.
   */
  typedef signed int  FT_Int;


  /**************************************************************************
   *
   * @type:
   *   FT_UInt
   *
   * @description:
   *   A typedef for the unsigned int type.
   */
  typedef unsigned int  FT_UInt;


  /**************************************************************************
   *
   * @type:
   *   FT_Long
   *
   * @description:
   *   A typedef for signed long.
   */
  typedef signed long  FT_Long;


  /**************************************************************************
   *
   * @type:
   *   FT_ULong
   *
   * @description:
   *   A typedef for unsigned long.
   */
  typedef unsigned long  FT_ULong;


  /**************************************************************************
   *
   * @type:
   *   FT_F2Dot14
   *
   * @description:
   *   A signed 2.14 fixed-point type used for unit vectors.
   */
  typedef signed short  FT_F2Dot14;


  /**************************************************************************
   *
   * @type:
   *   FT_F26Dot6
   *
   * @description:
   *   A signed 26.6 fixed-point type used for vectorial pixel coordinates.
   */
  typedef signed long  FT_F26Dot6;


  /**************************************************************************
   *
   * @type:
   *   FT_Fixed
   *
   * @description:
   *   This type is used to store 16.16 fixed-point values, like scaling
   *   values or matrix coefficients.
   */
  typedef signed long  FT_Fixed;


  /**************************************************************************
   *
   * @type:
   *   FT_Error
   *
   * @description:
   *   The FreeType error code type.  A value of~0 is always interpreted as a
   *   successful operation.
   */
  typedef int  FT_Error;


  /**************************************************************************
   *
   * @type:
   *   FT_Pointer
   *
   * @description:
   *   A simple typedef for a typeless pointer.
   */
  typedef void*  FT_Pointer;


  /**************************************************************************
   *
   * @type:
   *   FT_Offset
   *
   * @description:
   *   This is equivalent to the ANSI~C `size_t` type, i.e., the largest
   *   _unsigned_ integer type used to express a file size or position, or a
   *   memory block size.
   */
  typedef size_t  FT_Offset;


  /**************************************************************************
   *
   * @type:
   *   FT_PtrDist
   *
   * @description:
   *   This is equivalent to the ANSI~C `ptrdiff_t` type, i.e., the largest
   *   _signed_ integer type used to express the distance between two
   *   pointers.
   */
  typedef ft_ptrdiff_t  FT_PtrDist;


  /**************************************************************************
   *
   * @struct:
   *   FT_UnitVector
   *
   * @description:
   *   A simple structure used to store a 2D vector unit vector.  Uses
   *   FT_F2Dot14 types.
   *
   * @fields:
   *   x ::
   *     Horizontal coordinate.
   *
   *   y ::
   *     Vertical coordinate.
   */
  typedef struct  FT_UnitVector_
  {
    FT_F2Dot14  x;
    FT_F2Dot14  y;

  } FT_UnitVector;


  /**************************************************************************
   *
   * @struct:
   *   FT_Matrix
   *
   * @description:
   *   A simple structure used to store a 2x2 matrix.  Coefficients are in
   *   16.16 fixed-point format.  The computation performed is:
   *
   *   ```
   *     x' = x*xx + y*xy
   *     y' = x*yx + y*yy
   *   ```
   *
   * @fields:
   *   xx ::
   *     Matrix coefficient.
   *
   *   xy ::
   *     Matrix coefficient.
   *
   *   yx ::
   *     Matrix coefficient.
   *
   *   yy ::
   *     Matrix coefficient.
   */
  typedef struct  FT_Matrix_
  {
    FT_Fixed  xx, xy;
    FT_Fixed  yx, yy;

  } FT_Matrix;


  /**************************************************************************
   *
   * @struct:
   *   FT_Data
   *
   * @description:
   *   Read-only binary data represented as a pointer and a length.
   *
   * @fields:
   *   pointer ::
   *     The data.
   *
   *   length ::
   *     The length of the data in bytes.
   */
  typedef struct  FT_Data_
  {
    const FT_Byte*  pointer;
    FT_UInt         length;

  } FT_Data;


  /**************************************************************************
   *
   * @functype:
   *   FT_Generic_Finalizer
   *
   * @description:
   *   Describe a function used to destroy the 'client' data of any FreeType
   *   object.  See the description of the @FT_Generic type for details of
   *   usage.
   *
   * @input:
   *   The address of the FreeType object that is under finalization.  Its
   *   client data is accessed through its `generic` field.
   */
  typedef void  (*FT_Generic_Finalizer)( void*  object );


  /**************************************************************************
   *
   * @struct:
   *   FT_Generic
   *
   * @description:
   *   Client applications often need to associate their own data to a
   *   variety of FreeType core objects.  For example, a text layout API
   *   might want to associate a glyph cache to a given size object.
   *
   *   Some FreeType object contains a `generic` field, of type `FT_Generic`,
   *   which usage is left to client applications and font servers.
   *
   *   It can be used to store a pointer to client-specific data, as well as
   *   the address of a 'finalizer' function, which will be called by
   *   FreeType when the object is destroyed (for example, the previous
   *   client example would put the address of the glyph cache destructor in
   *   the `finalizer` field).
   *
   * @fields:
   *   data ::
   *     A typeless pointer to any client-specified data. This field is
   *     completely ignored by the FreeType library.
   *
   *   finalizer ::
   *     A pointer to a 'generic finalizer' function, which will be called
   *     when the object is destroyed.  If this field is set to `NULL`, no
   *     code will be called.
   */
  typedef struct  FT_Generic_
  {
    void*                 data;
    FT_Generic_Finalizer  finalizer;

  } FT_Generic;


  /**************************************************************************
   *
   * @macro:
   *   FT_MAKE_TAG
   *
   * @description:
   *   This macro converts four-letter tags that are used to label TrueType
   *   tables into an `FT_Tag` type, to be used within FreeType.
   *
   * @note:
   *   The produced values **must** be 32-bit integers.  Don't redefine this
   *   macro.
   */
#define FT_MAKE_TAG( _x1, _x2, _x3, _x4 )                  \
          ( ( FT_STATIC_BYTE_CAST( FT_Tag, _x1 ) << 24 ) | \
            ( FT_STATIC_BYTE_CAST( FT_Tag, _x2 ) << 16 ) | \
            ( FT_STATIC_BYTE_CAST( FT_Tag, _x3 ) <<  8 ) | \
              FT_STATIC_BYTE_CAST( FT_Tag, _x4 )         )


  /*************************************************************************/
  /*************************************************************************/
  /*                                                                       */
  /*                    L I S T   M A N A G E M E N T                      */
  /*                                                                       */
  /*************************************************************************/
  /*************************************************************************/


  /**************************************************************************
   *
   * @section:
   *   list_processing
   *
   */


  /**************************************************************************
   *
   * @type:
   *   FT_ListNode
   *
   * @description:
   *    Many elements and objects in FreeType are listed through an @FT_List
   *    record (see @FT_ListRec).  As its name suggests, an FT_ListNode is a
   *    handle to a single list element.
   */
  typedef struct FT_ListNodeRec_*  FT_ListNode;


  /**************************************************************************
   *
   * @type:
   *   FT_List
   *
   * @description:
   *   A handle to a list record (see @FT_ListRec).
   */
  typedef struct FT_ListRec_*  FT_List;


  /**************************************************************************
   *
   * @struct:
   *   FT_ListNodeRec
   *
   * @description:
   *   A structure used to hold a single list element.
   *
   * @fields:
   *   prev ::
   *     The previous element in the list.  `NULL` if first.
   *
   *   next ::
   *     The next element in the list.  `NULL` if last.
   *
   *   data ::
   *     A typeless pointer to the listed object.
   */
  typedef struct  FT_ListNodeRec_
  {
    FT_ListNode  prev;
    FT_ListNode  next;
    void*        data;

  } FT_ListNodeRec;


  /**************************************************************************
   *
   * @struct:
   *   FT_ListRec
   *
   * @description:
   *   A structure used to hold a simple doubly-linked list.  These are used
   *   in many parts of FreeType.
   *
   * @fields:
   *   head ::
   *     The head (first element) of doubly-linked list.
   *
   *   tail ::
   *     The tail (last element) of doubly-linked list.
   */
  typedef struct  FT_ListRec_
  {
    FT_ListNode  head;
    FT_ListNode  tail;

  } FT_ListRec;

  /* */


#define FT_IS_EMPTY( list )  ( (list).head == 0 )
#define FT_BOOL( x )         FT_STATIC_CAST( FT_Bool, (x) != 0 )

  /* concatenate C tokens */
//#define FT_ERR_XCAT( x, y )  x ## y
#define FT_ERR_CAT( x, y )   FT_ERR_XCAT( x, y )

  /* see `ftmoderr.h` for descriptions of the following macros */

#define FT_ERR( e )  FT_ERR_CAT( FT_ERR_PREFIX, e )

#define FT_ERROR_BASE( x )    ( (x) & 0xFF )
#define FT_ERROR_MODULE( x )  ( (x) & 0xFF00U )

#define FT_ERR_EQ( x, e )                                        \
          ( FT_ERROR_BASE( x ) == FT_ERROR_BASE( FT_ERR( e ) ) )
#define FT_ERR_NEQ( x, e )                                       \
          ( FT_ERROR_BASE( x ) != FT_ERROR_BASE( FT_ERR( e ) ) )




#endif /* FTTYPES_H_ */


/* END */
//
// ===========================  ftotval.h  ===========================
//
/****************************************************************************
 *
 * ftotval.h
 *
 *   FreeType API for validating OpenType tables (specification).
 *
 * Copyright (C) 2004-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


/****************************************************************************
 *
 *
 * Warning: This module might be moved to a different library in the
 *          future to avoid a tight dependency between FreeType and the
 *          OpenType specification.
 *
 *
 */


#ifndef FTOTVAL_H_
#define FTOTVAL_H_

#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   ot_validation
   *
   * @title:
   *   OpenType Validation
   *
   * @abstract:
   *   An API to validate OpenType tables.
   *
   * @description:
   *   This section contains the declaration of functions to validate some
   *   OpenType tables (BASE, GDEF, GPOS, GSUB, JSTF, MATH).
   *
   * @order:
   *   FT_OpenType_Validate
   *   FT_OpenType_Free
   *
   *   FT_VALIDATE_OTXXX
   *
   */


  /**************************************************************************
   *
   * @enum:
   *    FT_VALIDATE_OTXXX
   *
   * @description:
   *    A list of bit-field constants used with @FT_OpenType_Validate to
   *    indicate which OpenType tables should be validated.
   *
   * @values:
   *    FT_VALIDATE_BASE ::
   *      Validate BASE table.
   *
   *    FT_VALIDATE_GDEF ::
   *      Validate GDEF table.
   *
   *    FT_VALIDATE_GPOS ::
   *      Validate GPOS table.
   *
   *    FT_VALIDATE_GSUB ::
   *      Validate GSUB table.
   *
   *    FT_VALIDATE_JSTF ::
   *      Validate JSTF table.
   *
   *    FT_VALIDATE_MATH ::
   *      Validate MATH table.
   *
   *    FT_VALIDATE_OT ::
   *      Validate all OpenType tables (BASE, GDEF, GPOS, GSUB, JSTF, MATH).
   *
   */
#define FT_VALIDATE_BASE  0x0100
#define FT_VALIDATE_GDEF  0x0200
#define FT_VALIDATE_GPOS  0x0400
#define FT_VALIDATE_GSUB  0x0800
#define FT_VALIDATE_JSTF  0x1000
#define FT_VALIDATE_MATH  0x2000

#define FT_VALIDATE_OT  ( FT_VALIDATE_BASE | \
                          FT_VALIDATE_GDEF | \
                          FT_VALIDATE_GPOS | \
                          FT_VALIDATE_GSUB | \
                          FT_VALIDATE_JSTF | \
                          FT_VALIDATE_MATH )


  /**************************************************************************
   *
   * @function:
   *    FT_OpenType_Validate
   *
   * @description:
   *    Validate various OpenType tables to assure that all offsets and
   *    indices are valid.  The idea is that a higher-level library that
   *    actually does the text layout can access those tables without error
   *    checking (which can be quite time consuming).
   *
   * @input:
   *    face ::
   *      A handle to the input face.
   *
   *    validation_flags ::
   *      A bit field that specifies the tables to be validated.  See
   *      @FT_VALIDATE_OTXXX for possible values.
   *
   * @output:
   *    BASE_table ::
   *      A pointer to the BASE table.
   *
   *    GDEF_table ::
   *      A pointer to the GDEF table.
   *
   *    GPOS_table ::
   *      A pointer to the GPOS table.
   *
   *    GSUB_table ::
   *      A pointer to the GSUB table.
   *
   *    JSTF_table ::
   *      A pointer to the JSTF table.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function only works with OpenType fonts, returning an error
   *   otherwise.
   *
   *   After use, the application should deallocate the five tables with
   *   @FT_OpenType_Free.  A `NULL` value indicates that the table either
   *   doesn't exist in the font, or the application hasn't asked for
   *   validation.
   */
   FT_Error 
  FT_OpenType_Validate( FT_Face    face,
                        FT_UInt    validation_flags,
                        FT_Bytes  *BASE_table,
                        FT_Bytes  *GDEF_table,
                        FT_Bytes  *GPOS_table,
                        FT_Bytes  *GSUB_table,
                        FT_Bytes  *JSTF_table );


  /**************************************************************************
   *
   * @function:
   *    FT_OpenType_Free
   *
   * @description:
   *    Free the buffer allocated by OpenType validator.
   *
   * @input:
   *    face ::
   *      A handle to the input face.
   *
   *    table ::
   *      The pointer to the buffer that is allocated by
   *      @FT_OpenType_Validate.
   *
   * @note:
   *   This function must be used to free the buffer allocated by
   *   @FT_OpenType_Validate only.
   */
   void 
  FT_OpenType_Free( FT_Face   face,
                    FT_Bytes  table );


  /* */




#endif /* FTOTVAL_H_ */


/* END */
//
// ===========================  ftlist.h  ===========================
//
/****************************************************************************
 *
 * ftlist.h
 *
 *   Generic list support for FreeType (specification).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


  /**************************************************************************
   *
   * This file implements functions relative to list processing.  Its data
   * structures are defined in `freetype.h`.
   *
   */


#ifndef FTLIST_H_
#define FTLIST_H_


#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   list_processing
   *
   * @title:
   *   List Processing
   *
   * @abstract:
   *   Simple management of lists.
   *
   * @description:
   *   This section contains various definitions related to list processing
   *   using doubly-linked nodes.
   *
   * @order:
   *   FT_List
   *   FT_ListNode
   *   FT_ListRec
   *   FT_ListNodeRec
   *
   *   FT_List_Add
   *   FT_List_Insert
   *   FT_List_Find
   *   FT_List_Remove
   *   FT_List_Up
   *   FT_List_Iterate
   *   FT_List_Iterator
   *   FT_List_Finalize
   *   FT_List_Destructor
   *
   */


  /**************************************************************************
   *
   * @function:
   *   FT_List_Find
   *
   * @description:
   *   Find the list node for a given listed object.
   *
   * @input:
   *   list ::
   *     A pointer to the parent list.
   *   data ::
   *     The address of the listed object.
   *
   * @return:
   *   List node.  `NULL` if it wasn't found.
   */
   FT_ListNode 
  FT_List_Find( FT_List  list,
                void*    data );


  /**************************************************************************
   *
   * @function:
   *   FT_List_Add
   *
   * @description:
   *   Append an element to the end of a list.
   *
   * @inout:
   *   list ::
   *     A pointer to the parent list.
   *   node ::
   *     The node to append.
   */
   void 
  FT_List_Add( FT_List      list,
               FT_ListNode  node );


  /**************************************************************************
   *
   * @function:
   *   FT_List_Insert
   *
   * @description:
   *   Insert an element at the head of a list.
   *
   * @inout:
   *   list ::
   *     A pointer to parent list.
   *   node ::
   *     The node to insert.
   */
   void 
  FT_List_Insert( FT_List      list,
                  FT_ListNode  node );


  /**************************************************************************
   *
   * @function:
   *   FT_List_Remove
   *
   * @description:
   *   Remove a node from a list.  This function doesn't check whether the
   *   node is in the list!
   *
   * @input:
   *   node ::
   *     The node to remove.
   *
   * @inout:
   *   list ::
   *     A pointer to the parent list.
   */
   void 
  FT_List_Remove( FT_List      list,
                  FT_ListNode  node );


  /**************************************************************************
   *
   * @function:
   *   FT_List_Up
   *
   * @description:
   *   Move a node to the head/top of a list.  Used to maintain LRU lists.
   *
   * @inout:
   *   list ::
   *     A pointer to the parent list.
   *   node ::
   *     The node to move.
   */
   void 
  FT_List_Up( FT_List      list,
              FT_ListNode  node );


  /**************************************************************************
   *
   * @functype:
   *   FT_List_Iterator
   *
   * @description:
   *   An FT_List iterator function that is called during a list parse by
   *   @FT_List_Iterate.
   *
   * @input:
   *   node ::
   *     The current iteration list node.
   *
   *   user ::
   *     A typeless pointer passed to @FT_List_Iterate.  Can be used to point
   *     to the iteration's state.
   */
  typedef FT_Error
  (*FT_List_Iterator)( FT_ListNode  node,
                       void*        user );


  /**************************************************************************
   *
   * @function:
   *   FT_List_Iterate
   *
   * @description:
   *   Parse a list and calls a given iterator function on each element.
   *   Note that parsing is stopped as soon as one of the iterator calls
   *   returns a non-zero value.
   *
   * @input:
   *   list ::
   *     A handle to the list.
   *   iterator ::
   *     An iterator function, called on each node of the list.
   *   user ::
   *     A user-supplied field that is passed as the second argument to the
   *     iterator.
   *
   * @return:
   *   The result (a FreeType error code) of the last iterator call.
   */
   FT_Error 
  FT_List_Iterate( FT_List           list,
                   FT_List_Iterator  iterator,
                   void*             user );


  /**************************************************************************
   *
   * @functype:
   *   FT_List_Destructor
   *
   * @description:
   *   An @FT_List iterator function that is called during a list
   *   finalization by @FT_List_Finalize to destroy all elements in a given
   *   list.
   *
   * @input:
   *   system ::
   *     The current system object.
   *
   *   data ::
   *     The current object to destroy.
   *
   *   user ::
   *     A typeless pointer passed to @FT_List_Iterate.  It can be used to
   *     point to the iteration's state.
   */
  typedef void
  (*FT_List_Destructor)( FT_Memory  memory,
                         void*      data,
                         void*      user );


  /**************************************************************************
   *
   * @function:
   *   FT_List_Finalize
   *
   * @description:
   *   Destroy all elements in the list as well as the list itself.
   *
   * @input:
   *   list ::
   *     A handle to the list.
   *
   *   destroy ::
   *     A list destructor that will be applied to each element of the list.
   *     Set this to `NULL` if not needed.
   *
   *   memory ::
   *     The current memory object that handles deallocation.
   *
   *   user ::
   *     A user-supplied field that is passed as the last argument to the
   *     destructor.
   *
   * @note:
   *   This function expects that all nodes added by @FT_List_Add or
   *   @FT_List_Insert have been dynamically allocated.
   */
   void 
  FT_List_Finalize( FT_List             list,
                    FT_List_Destructor  destroy,
                    FT_Memory           memory,
                    void*               user );

  /* */




#endif /* FTLIST_H_ */


/* END */
//
// ===========================  ftsizes.h  ===========================
//
/****************************************************************************
 *
 * ftsizes.h
 *
 *   FreeType size objects management (specification).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


  /**************************************************************************
   *
   * Typical application would normally not need to use these functions.
   * However, they have been placed in a public API for the rare cases where
   * they are needed.
   *
   */


#ifndef FTSIZES_H_
#define FTSIZES_H_


#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   sizes_management
   *
   * @title:
   *   Size Management
   *
   * @abstract:
   *   Managing multiple sizes per face.
   *
   * @description:
   *   When creating a new face object (e.g., with @FT_New_Face), an @FT_Size
   *   object is automatically created and used to store all pixel-size
   *   dependent information, available in the `face->size` field.
   *
   *   It is however possible to create more sizes for a given face, mostly
   *   in order to manage several character pixel sizes of the same font
   *   family and style.  See @FT_New_Size and @FT_Done_Size.
   *
   *   Note that @FT_Set_Pixel_Sizes and @FT_Set_Char_Size only modify the
   *   contents of the current 'active' size; you thus need to use
   *   @FT_Activate_Size to change it.
   *
   *   99% of applications won't need the functions provided here, especially
   *   if they use the caching sub-system, so be cautious when using these.
   *
   */


  /**************************************************************************
   *
   * @function:
   *   FT_New_Size
   *
   * @description:
   *   Create a new size object from a given face object.
   *
   * @input:
   *   face ::
   *     A handle to a parent face object.
   *
   * @output:
   *   asize ::
   *     A handle to a new size object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   You need to call @FT_Activate_Size in order to select the new size for
   *   upcoming calls to @FT_Set_Pixel_Sizes, @FT_Set_Char_Size,
   *   @FT_Load_Glyph, @FT_Load_Char, etc.
   */
   FT_Error 
  FT_New_Size( FT_Face   face,
               FT_Size*  size );


  /**************************************************************************
   *
   * @function:
   *   FT_Done_Size
   *
   * @description:
   *   Discard a given size object.  Note that @FT_Done_Face automatically
   *   discards all size objects allocated with @FT_New_Size.
   *
   * @input:
   *   size ::
   *     A handle to a target size object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   */
   FT_Error 
  FT_Done_Size( FT_Size  size );


  /**************************************************************************
   *
   * @function:
   *   FT_Activate_Size
   *
   * @description:
   *   Even though it is possible to create several size objects for a given
   *   face (see @FT_New_Size for details), functions like @FT_Load_Glyph or
   *   @FT_Load_Char only use the one that has been activated last to
   *   determine the 'current character pixel size'.
   *
   *   This function can be used to 'activate' a previously created size
   *   object.
   *
   * @input:
   *   size ::
   *     A handle to a target size object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   If `face` is the size's parent face object, this function changes the
   *   value of `face->size` to the input size handle.
   */
   FT_Error 
  FT_Activate_Size( FT_Size  size );

  /* */




#endif /* FTSIZES_H_ */


/* END */
//
// ===========================  ftmoderr.h  ===========================
//
/****************************************************************************
 *
 * ftmoderr.h
 *
 *   FreeType module error offsets (specification).
 *
 * Copyright (C) 2001-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


  /**************************************************************************
   *
   * This file is used to define the FreeType module error codes.
   *
   * If the macro `FT_CONFIG_OPTION_USE_MODULE_ERRORS` in `ftoption.h` is
   * set, the lower byte of an error value identifies the error code as
   * usual.  In addition, the higher byte identifies the module.  For
   * example, the error `FT_Err_Invalid_File_Format` has value 0x0003, the
   * error `TT_Err_Invalid_File_Format` has value 0x1303, the error
   * `T1_Err_Invalid_File_Format` has value 0x1403, etc.
   *
   * Note that `FT_Err_Ok`, `TT_Err_Ok`, etc. are always equal to zero,
   * including the high byte.
   *
   * If `FT_CONFIG_OPTION_USE_MODULE_ERRORS` isn't set, the higher byte of an
   * error value is set to zero.
   *
   * To hide the various `XXX_Err_` prefixes in the source code, FreeType
   * provides some macros in `fttypes.h`.
   *
   *   FT_ERR( err )
   *
   *     Add current error module prefix (as defined with the `FT_ERR_PREFIX`
   *     macro) to `err`.  For example, in the BDF module the line
   *
   *     ```
   *       error = FT_ERR( Invalid_Outline );
   *     ```
   *
   *     expands to
   *
   *     ```
   *       error = BDF_Err_Invalid_Outline;
   *     ```
   *
   *     For simplicity, you can always use `FT_Err_Ok` directly instead of
   *     `FT_ERR( Ok )`.
   *
   *   FT_ERR_EQ( errcode, err )
   *   FT_ERR_NEQ( errcode, err )
   *
   *     Compare error code `errcode` with the error `err` for equality and
   *     inequality, respectively.  Example:
   *
   *     ```
   *       if ( FT_ERR_EQ( error, Invalid_Outline ) )
   *         ...
   *     ```
   *
   *     Using this macro you don't have to think about error prefixes.  Of
   *     course, if module errors are not active, the above example is the
   *     same as
   *
   *     ```
   *       if ( error == FT_Err_Invalid_Outline )
   *         ...
   *     ```
   *
   *   FT_ERROR_BASE( errcode )
   *   FT_ERROR_MODULE( errcode )
   *
   *     Get base error and module error code, respectively.
   *
   * It can also be used to create a module error message table easily with
   * something like
   *
   * ```
   *   #undef FTMODERR_H_
   *   #define FT_MODERRDEF( e, v, s )  { FT_Mod_Err_ ## e, s },
   *   #define FT_MODERR_START_LIST     {
   *   #define FT_MODERR_END_LIST       { 0, 0 } };
   *
   *   const struct
   *   {
   *     int          mod_err_offset;
   *     const char*  mod_err_msg
   *   } ft_mod_errors[] =
   *
   *   #include <freetype/ftmoderr.h>
   * ```
   *
   */


#ifndef FTMODERR_H_
#define FTMODERR_H_


  /*******************************************************************/
  /*******************************************************************/
  /*****                                                         *****/
  /*****                       SETUP MACROS                      *****/
  /*****                                                         *****/
  /*******************************************************************/
  /*******************************************************************/


#undef  FT_NEED_EXTERN_C

#ifndef FT_MODERRDEF

//#ifdef FT_CONFIG_OPTION_USE_MODULE_ERRORS
//#define FT_MODERRDEF( e, v, s )  FT_Mod_Err_ ## e = v,
//#else
//#define FT_MODERRDEF( e, v, s )  FT_Mod_Err_ ## e = 0,
//#endif

//#define FT_MODERR_START_LIST  enum {
//#define FT_MODERR_END_LIST    FT_Mod_Err_Max };

//#ifdef __cplusplus
//#define FT_NEED_EXTERN_C
//  extern "C" {
//#endif

#endif /* !FT_MODERRDEF */


  /*******************************************************************/
  /*******************************************************************/
  /*****                                                         *****/
  /*****               LIST MODULE ERROR BASES                   *****/
  /*****                                                         *****/
  /*******************************************************************/
  /*******************************************************************/


//#ifdef FT_MODERR_START_LIST
//  FT_MODERR_START_LIST
//#endif

/*
  FT_MODERRDEF( Base,      0x000, "base module" )
  FT_MODERRDEF( Autofit,   0x100, "autofitter module" )
  FT_MODERRDEF( BDF,       0x200, "BDF module" )
  FT_MODERRDEF( Bzip2,     0x300, "Bzip2 module" )
  FT_MODERRDEF( Cache,     0x400, "cache module" )
  FT_MODERRDEF( CFF,       0x500, "CFF module" )
  FT_MODERRDEF( CID,       0x600, "CID module" )
  FT_MODERRDEF( Gzip,      0x700, "Gzip module" )
  FT_MODERRDEF( LZW,       0x800, "LZW module" )
  FT_MODERRDEF( OTvalid,   0x900, "OpenType validation module" )
  FT_MODERRDEF( PCF,       0xA00, "PCF module" )
  FT_MODERRDEF( PFR,       0xB00, "PFR module" )
  FT_MODERRDEF( PSaux,     0xC00, "PS auxiliary module" )
  FT_MODERRDEF( PShinter,  0xD00, "PS hinter module" )
  FT_MODERRDEF( PSnames,   0xE00, "PS names module" )
  FT_MODERRDEF( Raster,    0xF00, "raster module" )
  FT_MODERRDEF( SFNT,     0x1000, "SFNT module" )
  FT_MODERRDEF( Smooth,   0x1100, "smooth raster module" )
  FT_MODERRDEF( TrueType, 0x1200, "TrueType module" )
  FT_MODERRDEF( Type1,    0x1300, "Type 1 module" )
  FT_MODERRDEF( Type42,   0x1400, "Type 42 module" )
  FT_MODERRDEF( Winfonts, 0x1500, "Windows FON/FNT module" )
  FT_MODERRDEF( GXvalid,  0x1600, "GX validation module" )
  FT_MODERRDEF( Sdf,      0x1700, "Signed distance field raster module" )


#ifdef FT_MODERR_END_LIST
  FT_MODERR_END_LIST
#endif

*/
  /*******************************************************************/
  /*******************************************************************/
  /*****                                                         *****/
  /*****                      CLEANUP                            *****/
  /*****                                                         *****/
  /*******************************************************************/
  /*******************************************************************/


//#ifdef FT_NEED_EXTERN_C
//  }
//#endif

#undef FT_MODERR_START_LIST
#undef FT_MODERR_END_LIST
#undef FT_MODERRDEF
#undef FT_NEED_EXTERN_C


#endif /* FTMODERR_H_ */


/* END */
//
// ===========================  ftdriver.h  ===========================
//
/****************************************************************************
 *
 * ftdriver.h
 *
 *   FreeType API for controlling driver modules (specification only).
 *
 * Copyright (C) 2017-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTDRIVER_H_
#define FTDRIVER_H_

#include <freetype/freetype.h>
#include <freetype/ftparams.h>

//#ifdef FREETYPE_H
//#error "freetype.h of FreeType 1 has been loaded!"
//#error "Please fix the directory search order for header files"
//#error "so that freetype.h of FreeType 2 is found first."
//#endif





  /**************************************************************************
   *
   * @section:
   *   auto_hinter
   *
   * @title:
   *   The auto-hinter
   *
   * @abstract:
   *   Controlling the auto-hinting module.
   *
   * @description:
   *   While FreeType's auto-hinter doesn't expose API functions by itself,
   *   it is possible to control its behaviour with @FT_Property_Set and
   *   @FT_Property_Get.  The following lists the available properties
   *   together with the necessary macros and structures.
   *
   *   Note that the auto-hinter's module name is 'autofitter' for historical
   *   reasons.
   *
   *   Available properties are @increase-x-height, @no-stem-darkening
   *   (experimental), @darkening-parameters (experimental),
   *   @glyph-to-script-map (experimental), @fallback-script (experimental),
   *   and @default-script (experimental), as documented in the @properties
   *   section.
   *
   */


  /**************************************************************************
   *
   * @section:
   *   cff_driver
   *
   * @title:
   *   The CFF driver
   *
   * @abstract:
   *   Controlling the CFF driver module.
   *
   * @description:
   *   While FreeType's CFF driver doesn't expose API functions by itself, it
   *   is possible to control its behaviour with @FT_Property_Set and
   *   @FT_Property_Get.
   *
   *   The CFF driver's module name is 'cff'.
   *
   *   Available properties are @hinting-engine, @no-stem-darkening,
   *   @darkening-parameters, and @random-seed, as documented in the
   *   @properties section.
   *
   *
   *   **Hinting and anti-aliasing principles of the new engine**
   *
   *   The rasterizer is positioning horizontal features (e.g., ascender
   *   height & x-height, or crossbars) on the pixel grid and minimizing the
   *   amount of anti-aliasing applied to them, while placing vertical
   *   features (vertical stems) on the pixel grid without hinting, thus
   *   representing the stem position and weight accurately.  Sometimes the
   *   vertical stems may be only partially black.  In this context,
   *   'anti-aliasing' means that stems are not positioned exactly on pixel
   *   borders, causing a fuzzy appearance.
   *
   *   There are two principles behind this approach.
   *
   *   1) No hinting in the horizontal direction: Unlike 'superhinted'
   *   TrueType, which changes glyph widths to accommodate regular
   *   inter-glyph spacing, Adobe's approach is 'faithful to the design' in
   *   representing both the glyph width and the inter-glyph spacing designed
   *   for the font.  This makes the screen display as close as it can be to
   *   the result one would get with infinite resolution, while preserving
   *   what is considered the key characteristics of each glyph.  Note that
   *   the distances between unhinted and grid-fitted positions at small
   *   sizes are comparable to kerning values and thus would be noticeable
   *   (and distracting) while reading if hinting were applied.
   *
   *   One of the reasons to not hint horizontally is anti-aliasing for LCD
   *   screens: The pixel geometry of modern displays supplies three vertical
   *   subpixels as the eye moves horizontally across each visible pixel.  On
   *   devices where we can be certain this characteristic is present a
   *   rasterizer can take advantage of the subpixels to add increments of
   *   weight.  In Western writing systems this turns out to be the more
   *   critical direction anyway; the weights and spacing of vertical stems
   *   (see above) are central to Armenian, Cyrillic, Greek, and Latin type
   *   designs.  Even when the rasterizer uses greyscale anti-aliasing instead
   *   of color (a necessary compromise when one doesn't know the screen
   *   characteristics), the unhinted vertical features preserve the design's
   *   weight and spacing much better than aliased type would.
   *
   *   2) Alignment in the vertical direction: Weights and spacing along the
   *   y~axis are less critical; what is much more important is the visual
   *   alignment of related features (like cap-height and x-height).  The
   *   sense of alignment for these is enhanced by the sharpness of grid-fit
   *   edges, while the cruder vertical resolution (full pixels instead of
   *   1/3 pixels) is less of a problem.
   *
   *   On the technical side, horizontal alignment zones for ascender,
   *   x-height, and other important height values (traditionally called
   *   'blue zones') as defined in the font are positioned independently,
   *   each being rounded to the nearest pixel edge, taking care of overshoot
   *   suppression at small sizes, stem darkening, and scaling.
   *
   *   Hstems (that is, hint values defined in the font to help align
   *   horizontal features) that fall within a blue zone are said to be
   *   'captured' and are aligned to that zone.  Uncaptured stems are moved
   *   in one of four ways, top edge up or down, bottom edge up or down.
   *   Unless there are conflicting hstems, the smallest movement is taken to
   *   minimize distortion.
   *
   */


  /**************************************************************************
   *
   * @section:
   *   pcf_driver
   *
   * @title:
   *   The PCF driver
   *
   * @abstract:
   *   Controlling the PCF driver module.
   *
   * @description:
   *   While FreeType's PCF driver doesn't expose API functions by itself, it
   *   is possible to control its behaviour with @FT_Property_Set and
   *   @FT_Property_Get.  Right now, there is a single property
   *   @no-long-family-names available if FreeType is compiled with
   *   PCF_CONFIG_OPTION_LONG_FAMILY_NAMES.
   *
   *   The PCF driver's module name is 'pcf'.
   *
   */


  /**************************************************************************
   *
   * @section:
   *   t1_cid_driver
   *
   * @title:
   *   The Type 1 and CID drivers
   *
   * @abstract:
   *   Controlling the Type~1 and CID driver modules.
   *
   * @description:
   *   It is possible to control the behaviour of FreeType's Type~1 and
   *   Type~1 CID drivers with @FT_Property_Set and @FT_Property_Get.
   *
   *   Behind the scenes, both drivers use the Adobe CFF engine for hinting;
   *   however, the used properties must be specified separately.
   *
   *   The Type~1 driver's module name is 'type1'; the CID driver's module
   *   name is 't1cid'.
   *
   *   Available properties are @hinting-engine, @no-stem-darkening,
   *   @darkening-parameters, and @random-seed, as documented in the
   *   @properties section.
   *
   *   Please see the @cff_driver section for more details on the new hinting
   *   engine.
   *
   */


  /**************************************************************************
   *
   * @section:
   *   tt_driver
   *
   * @title:
   *   The TrueType driver
   *
   * @abstract:
   *   Controlling the TrueType driver module.
   *
   * @description:
   *   While FreeType's TrueType driver doesn't expose API functions by
   *   itself, it is possible to control its behaviour with @FT_Property_Set
   *   and @FT_Property_Get.
   *
   *   The TrueType driver's module name is 'truetype'; a single property
   *   @interpreter-version is available, as documented in the @properties
   *   section.
   *
   *   To help understand the differences between interpreter versions, we
   *   introduce a list of definitions, kindly provided by Greg Hitchcock.
   *
   *   _Bi-Level Rendering_
   *
   *   Monochromatic rendering, exclusively used in the early days of
   *   TrueType by both Apple and Microsoft.  Microsoft's GDI interface
   *   supported hinting of the right-side bearing point, such that the
   *   advance width could be non-linear.  Most often this was done to
   *   achieve some level of glyph symmetry.  To enable reasonable
   *   performance (e.g., not having to run hinting on all glyphs just to get
   *   the widths) there was a bit in the head table indicating if the side
   *   bearing was hinted, and additional tables, 'hdmx' and 'LTSH', to cache
   *   hinting widths across multiple sizes and device aspect ratios.
   *
   *   _Font Smoothing_
   *
   *   Microsoft's GDI implementation of anti-aliasing.  Not traditional
   *   anti-aliasing as the outlines were hinted before the sampling.  The
   *   widths matched the bi-level rendering.
   *
   *   _ClearType Rendering_
   *
   *   Technique that uses physical subpixels to improve rendering on LCD
   *   (and other) displays.  Because of the higher resolution, many methods
   *   of improving symmetry in glyphs through hinting the right-side bearing
   *   were no longer necessary.  This lead to what GDI calls 'natural
   *   widths' ClearType, see
   *   http://rastertragedy.com/RTRCh4.htm#Sec21.  Since hinting
   *   has extra resolution, most non-linearity went away, but it is still
   *   possible for hints to change the advance widths in this mode.
   *
   *   _ClearType Compatible Widths_
   *
   *   One of the earliest challenges with ClearType was allowing the
   *   implementation in GDI to be selected without requiring all UI and
   *   documents to reflow.  To address this, a compatible method of
   *   rendering ClearType was added where the font hints are executed once
   *   to determine the width in bi-level rendering, and then re-run in
   *   ClearType, with the difference in widths being absorbed in the font
   *   hints for ClearType (mostly in the white space of hints); see
   *   http://rastertragedy.com/RTRCh4.htm#Sec20.  Somewhat by
   *   definition, compatible width ClearType allows for non-linear widths,
   *   but only when the bi-level version has non-linear widths.
   *
   *   _ClearType Subpixel Positioning_
   *
   *   One of the nice benefits of ClearType is the ability to more crisply
   *   display fractional widths; unfortunately, the GDI model of integer
   *   bitmaps did not support this.  However, the WPF and Direct Write
   *   frameworks do support fractional widths.  DWrite calls this 'natural
   *   mode', not to be confused with GDI's 'natural widths'.  Subpixel
   *   positioning, in the current implementation of Direct Write,
   *   unfortunately does not support hinted advance widths, see
   *   http://rastertragedy.com/RTRCh4.htm#Sec22.  Note that the
   *   TrueType interpreter fully allows the advance width to be adjusted in
   *   this mode, just the DWrite client will ignore those changes.
   *
   *   _ClearType Backward Compatibility_
   *
   *   This is a set of exceptions made in the TrueType interpreter to
   *   minimize hinting techniques that were problematic with the extra
   *   resolution of ClearType; see
   *   http://rastertragedy.com/RTRCh4.htm#Sec1 and
   *   https://www.microsoft.com/typography/cleartype/truetypecleartype.aspx.
   *   This technique is not to be confused with ClearType compatible widths.
   *   ClearType backward compatibility has no direct impact on changing
   *   advance widths, but there might be an indirect impact on disabling
   *   some deltas.  This could be worked around in backward compatibility
   *   mode.
   *
   *   _Native ClearType Mode_
   *
   *   (Not to be confused with 'natural widths'.)  This mode removes all the
   *   exceptions in the TrueType interpreter when running with ClearType.
   *   Any issues on widths would still apply, though.
   *
   */


  /**************************************************************************
   *
   * @section:
   *   ot_svg_driver
   *
   * @title:
   *   The SVG driver
   *
   * @abstract:
   *   Controlling the external rendering of OT-SVG glyphs.
   *
   * @description:
   *   By default, FreeType can only load the 'SVG~' table of OpenType fonts
   *   if configuration macro `FT_CONFIG_OPTION_SVG` is defined.  To make it
   *   render SVG glyphs, an external SVG rendering library is needed.  All
   *   details on the interface between FreeType and the external library
   *   via function hooks can be found in section @svg_fonts.
   *
   *   The OT-SVG driver's module name is 'ot-svg'; it supports a single
   *   property called @svg-hooks, documented below in the @properties
   *   section.
   *
   */


  /**************************************************************************
   *
   * @section:
   *   properties
   *
   * @title:
   *   Driver properties
   *
   * @abstract:
   *   Controlling driver modules.
   *
   * @description:
   *   Driver modules can be controlled by setting and unsetting properties,
   *   using the functions @FT_Property_Set and @FT_Property_Get.  This
   *   section documents the available properties, together with auxiliary
   *   macros and structures.
   *
   */


  /**************************************************************************
   *
   * @enum:
   *   FT_HINTING_XXX
   *
   * @description:
   *   A list of constants used for the @hinting-engine property to select
   *   the hinting engine for CFF, Type~1, and CID fonts.
   *
   * @values:
   *   FT_HINTING_FREETYPE ::
   *     Use the old FreeType hinting engine.
   *
   *   FT_HINTING_ADOBE ::
   *     Use the hinting engine contributed by Adobe.
   *
   * @since:
   *   2.9
   *
   */
#define FT_HINTING_FREETYPE  0
#define FT_HINTING_ADOBE     1

  /* these constants (introduced in 2.4.12) are deprecated */
#define FT_CFF_HINTING_FREETYPE  FT_HINTING_FREETYPE
#define FT_CFF_HINTING_ADOBE     FT_HINTING_ADOBE


  /**************************************************************************
   *
   * @property:
   *   hinting-engine
   *
   * @description:
   *   Thanks to Adobe, which contributed a new hinting (and parsing) engine,
   *   an application can select between 'freetype' and 'adobe' if compiled
   *   with `CFF_CONFIG_OPTION_OLD_ENGINE`.  If this configuration macro
   *   isn't defined, 'hinting-engine' does nothing.
   *
   *   The same holds for the Type~1 and CID modules if compiled with
   *   `T1_CONFIG_OPTION_OLD_ENGINE`.
   *
   *   For the 'cff' module, the default engine is 'adobe'.  For both the
   *   'type1' and 't1cid' modules, the default engine is 'adobe', too.
   *
   * @note:
   *   This property can be used with @FT_Property_Get also.
   *
   *   This property can be set via the `FREETYPE_PROPERTIES` environment
   *   variable (using values 'adobe' or 'freetype').
   *
   * @example:
   *   The following example code demonstrates how to select Adobe's hinting
   *   engine for the 'cff' module (omitting the error handling).
   *
   *   ```
   *     FT_Library  library;
   *     FT_UInt     hinting_engine = FT_HINTING_ADOBE;
   *
   *
   *     FT_Init_FreeType( &library );
   *
   *     FT_Property_Set( library, "cff",
   *                               "hinting-engine", &hinting_engine );
   *   ```
   *
   * @since:
   *   2.4.12 (for 'cff' module)
   *
   *   2.9 (for 'type1' and 't1cid' modules)
   *
   */


  /**************************************************************************
   *
   * @property:
   *   no-stem-darkening
   *
   * @description:
   *   All glyphs that pass through the auto-hinter will be emboldened unless
   *   this property is set to TRUE.  The same is true for the CFF, Type~1,
   *   and CID font modules if the 'Adobe' engine is selected (which is the
   *   default).
   *
   *   Stem darkening emboldens glyphs at smaller sizes to make them more
   *   readable on common low-DPI screens when using linear alpha blending
   *   and gamma correction, see @FT_Render_Glyph.  When not using linear
   *   alpha blending and gamma correction, glyphs will appear heavy and
   *   fuzzy!
   *
   *   Gamma correction essentially lightens fonts since shades of grey are
   *   shifted to higher pixel values (=~higher brightness) to match the
   *   original intention to the reality of our screens.  The side-effect is
   *   that glyphs 'thin out'.  Mac OS~X and Adobe's proprietary font
   *   rendering library implement a counter-measure: stem darkening at
   *   smaller sizes where shades of gray dominate.  By emboldening a glyph
   *   slightly in relation to its pixel size, individual pixels get higher
   *   coverage of filled-in outlines and are therefore 'blacker'.  This
   *   counteracts the 'thinning out' of glyphs, making text remain readable
   *   at smaller sizes.
   *
   *   For the auto-hinter, stem-darkening is experimental currently and thus
   *   switched off by default (that is, `no-stem-darkening` is set to TRUE
   *   by default).  Total consistency with the CFF driver is not achieved
   *   right now because the emboldening method differs and glyphs must be
   *   scaled down on the Y-axis to keep outline points inside their
   *   precomputed blue zones.  The smaller the size (especially 9ppem and
   *   down), the higher the loss of emboldening versus the CFF driver.
   *
   *   Note that stem darkening is never applied if @FT_LOAD_NO_SCALE is set.
   *
   * @note:
   *   This property can be used with @FT_Property_Get also.
   *
   *   This property can be set via the `FREETYPE_PROPERTIES` environment
   *   variable (using values 1 and 0 for 'on' and 'off', respectively).  It
   *   can also be set per face using @FT_Face_Properties with
   *   @FT_PARAM_TAG_STEM_DARKENING.
   *
   * @example:
   *   ```
   *     FT_Library  library;
   *     FT_Bool     no_stem_darkening = TRUE;
   *
   *
   *     FT_Init_FreeType( &library );
   *
   *     FT_Property_Set( library, "cff",
   *                               "no-stem-darkening", &no_stem_darkening );
   *   ```
   *
   * @since:
   *   2.4.12 (for 'cff' module)
   *
   *   2.6.2 (for 'autofitter' module)
   *
   *   2.9 (for 'type1' and 't1cid' modules)
   *
   */


  /**************************************************************************
   *
   * @property:
   *   darkening-parameters
   *
   * @description:
   *   By default, the Adobe hinting engine, as used by the CFF, Type~1, and
   *   CID font drivers, darkens stems as follows (if the `no-stem-darkening`
   *   property isn't set):
   *
   *   ```
   *     stem width <= 0.5px:   darkening amount = 0.4px
   *     stem width  = 1px:     darkening amount = 0.275px
   *     stem width  = 1.667px: darkening amount = 0.275px
   *     stem width >= 2.333px: darkening amount = 0px
   *   ```
   *
   *   and piecewise linear in-between.  At configuration time, these four
   *   control points can be set with the macro
   *   `CFF_CONFIG_OPTION_DARKENING_PARAMETERS`; the CFF, Type~1, and CID
   *   drivers share these values.  At runtime, the control points can be
   *   changed using the `darkening-parameters` property (see the example
   *   below that demonstrates this for the Type~1 driver).
   *
   *   The x~values give the stem width, and the y~values the darkening
   *   amount.  The unit is 1000th of pixels.  All coordinate values must be
   *   positive; the x~values must be monotonically increasing; the y~values
   *   must be monotonically decreasing and smaller than or equal to 500
   *   (corresponding to half a pixel); the slope of each linear piece must
   *   be shallower than -1 (e.g., -.4).
   *
   *   The auto-hinter provides this property, too, as an experimental
   *   feature.  See @no-stem-darkening for more.
   *
   * @note:
   *   This property can be used with @FT_Property_Get also.
   *
   *   This property can be set via the `FREETYPE_PROPERTIES` environment
   *   variable, using eight comma-separated integers without spaces.  Here
   *   the above example, using `\` to break the line for readability.
   *
   *   ```
   *     FREETYPE_PROPERTIES=\
   *     type1:darkening-parameters=500,300,1000,200,1500,100,2000,0
   *   ```
   *
   * @example:
   *   ```
   *     FT_Library  library;
   *     FT_Int      darken_params[8] = {  500, 300,   // x1, y1
   *                                      1000, 200,   // x2, y2
   *                                      1500, 100,   // x3, y3
   *                                      2000,   0 }; // x4, y4
   *
   *
   *     FT_Init_FreeType( &library );
   *
   *     FT_Property_Set( library, "type1",
   *                               "darkening-parameters", darken_params );
   *   ```
   *
   * @since:
   *   2.5.1 (for 'cff' module)
   *
   *   2.6.2 (for 'autofitter' module)
   *
   *   2.9 (for 'type1' and 't1cid' modules)
   *
   */


  /**************************************************************************
   *
   * @property:
   *   random-seed
   *
   * @description:
   *   By default, the seed value for the CFF 'random' operator and the
   *   similar '0 28 callothersubr pop' command for the Type~1 and CID
   *   drivers is set to a random value.  However, mainly for debugging
   *   purposes, it is often necessary to use a known value as a seed so that
   *   the pseudo-random number sequences generated by 'random' are
   *   repeatable.
   *
   *   The `random-seed` property does that.  Its argument is a signed 32bit
   *   integer; if the value is zero or negative, the seed given by the
   *   `intitialRandomSeed` private DICT operator in a CFF file gets used (or
   *   a default value if there is no such operator).  If the value is
   *   positive, use it instead of `initialRandomSeed`, which is consequently
   *   ignored.
   *
   * @note:
   *   This property can be set via the `FREETYPE_PROPERTIES` environment
   *   variable.  It can also be set per face using @FT_Face_Properties with
   *   @FT_PARAM_TAG_RANDOM_SEED.
   *
   * @since:
   *   2.8 (for 'cff' module)
   *
   *   2.9 (for 'type1' and 't1cid' modules)
   *
   */


  /**************************************************************************
   *
   * @property:
   *   no-long-family-names
   *
   * @description:
   *   If `PCF_CONFIG_OPTION_LONG_FAMILY_NAMES` is active while compiling
   *   FreeType, the PCF driver constructs long family names.
   *
   *   There are many PCF fonts just called 'Fixed' which look completely
   *   different, and which have nothing to do with each other.  When
   *   selecting 'Fixed' in KDE or Gnome one gets results that appear rather
   *   random, the style changes often if one changes the size and one cannot
   *   select some fonts at all.  The improve this situation, the PCF module
   *   prepends the foundry name (plus a space) to the family name.  It also
   *   checks whether there are 'wide' characters; all put together, family
   *   names like 'Sony Fixed' or 'Misc Fixed Wide' are constructed.
   *
   *   If `no-long-family-names` is set, this feature gets switched off.
   *
   * @note:
   *   This property can be used with @FT_Property_Get also.
   *
   *   This property can be set via the `FREETYPE_PROPERTIES` environment
   *   variable (using values 1 and 0 for 'on' and 'off', respectively).
   *
   * @example:
   *   ```
   *     FT_Library  library;
   *     FT_Bool     no_long_family_names = TRUE;
   *
   *
   *     FT_Init_FreeType( &library );
   *
   *     FT_Property_Set( library, "pcf",
   *                               "no-long-family-names",
   *                               &no_long_family_names );
   *   ```
   *
   * @since:
   *   2.8
   */


  /**************************************************************************
   *
   * @enum:
   *   TT_INTERPRETER_VERSION_XXX
   *
   * @description:
   *   A list of constants used for the @interpreter-version property to
   *   select the hinting engine for Truetype fonts.
   *
   *   The numeric value in the constant names represents the version number
   *   as returned by the 'GETINFO' bytecode instruction.
   *
   * @values:
   *   TT_INTERPRETER_VERSION_35 ::
   *     Version~35 corresponds to MS rasterizer v.1.7 as used e.g. in
   *     Windows~98; only grayscale and B/W rasterizing is supported.
   *
   *   TT_INTERPRETER_VERSION_38 ::
   *     Version~38 is the same Version~40. The original 'Infinality' code is
   *     no longer available.
   *
   *   TT_INTERPRETER_VERSION_40 ::
   *     Version~40 corresponds to MS rasterizer v.2.1; it is roughly
   *     equivalent to the hinting provided by DirectWrite ClearType (as can
   *     be found, for example, in Microsoft's Edge Browser on Windows~10).
   *     It is used in FreeType to select the 'minimal' subpixel hinting
   *     code, a stripped-down and higher performance version of the
   *     'Infinality' code.
   *
   * @note:
   *   This property controls the behaviour of the bytecode interpreter and
   *   thus how outlines get hinted.  It does **not** control how glyph get
   *   rasterized!  In particular, it does not control subpixel color
   *   filtering.
   *
   *   If FreeType has not been compiled with the configuration option
   *   `TT_CONFIG_OPTION_SUBPIXEL_HINTING`, selecting version~38 or~40 causes
   *   an `FT_Err_Unimplemented_Feature` error.
   *
   *   Depending on the graphics framework, Microsoft uses different bytecode
   *   and rendering engines.  As a consequence, the version numbers returned
   *   by a call to the 'GETINFO' bytecode instruction are more convoluted
   *   than desired.
   *
   *   Here are two tables that try to shed some light on the possible values
   *   for the MS rasterizer engine, together with the additional features
   *   introduced by it.
   *
   *   ```
   *     GETINFO framework               version feature
   *     -------------------------------------------------------------------
   *         3   GDI (Win 3.1),            v1.0  16-bit, first version
   *             TrueImage
   *        33   GDI (Win NT 3.1),         v1.5  32-bit
   *             HP Laserjet
   *        34   GDI (Win 95)              v1.6  font smoothing,
   *                                             new SCANTYPE opcode
   *        35   GDI (Win 98/2000)         v1.7  (UN)SCALED_COMPONENT_OFFSET
   *                                               bits in composite glyphs
   *        36   MGDI (Win CE 2)           v1.6+ classic ClearType
   *        37   GDI (XP and later),       v1.8  ClearType
   *             GDI+ old (before Vista)
   *        38   GDI+ old (Vista, Win 7),  v1.9  subpixel ClearType,
   *             WPF                             Y-direction ClearType,
   *                                             additional error checking
   *        39   DWrite (before Win 8)     v2.0  subpixel ClearType flags
   *                                               in GETINFO opcode,
   *                                             bug fixes
   *        40   GDI+ (after Win 7),       v2.1  Y-direction ClearType flag
   *             DWrite (Win 8)                    in GETINFO opcode,
   *                                             Gray ClearType
   *   ```
   *
   *   The 'version' field gives a rough orientation only, since some
   *   applications provided certain features much earlier (as an example,
   *   Microsoft Reader used subpixel and Y-direction ClearType already in
   *   Windows 2000).  Similarly, updates to a given framework might include
   *   improved hinting support.
   *
   *   ```
   *      version   sampling          rendering        comment
   *               x        y       x           y
   *     --------------------------------------------------------------
   *       v1.0   normal  normal  B/W           B/W    bi-level
   *       v1.6   high    high    gray          gray   grayscale
   *       v1.8   high    normal  color-filter  B/W    (GDI) ClearType
   *       v1.9   high    high    color-filter  gray   Color ClearType
   *       v2.1   high    normal  gray          B/W    Gray ClearType
   *       v2.1   high    high    gray          gray   Gray ClearType
   *   ```
   *
   *   Color and Gray ClearType are the two available variants of
   *   'Y-direction ClearType', meaning grayscale rasterization along the
   *   Y-direction; the name used in the TrueType specification for this
   *   feature is 'symmetric smoothing'.  'Classic ClearType' is the original
   *   algorithm used before introducing a modified version in Win~XP.
   *   Another name for v1.6's grayscale rendering is 'font smoothing', and
   *   'Color ClearType' is sometimes also called 'DWrite ClearType'.  To
   *   differentiate between today's Color ClearType and the earlier
   *   ClearType variant with B/W rendering along the vertical axis, the
   *   latter is sometimes called 'GDI ClearType'.
   *
   *   'Normal' and 'high' sampling describe the (virtual) resolution to
   *   access the rasterized outline after the hinting process.  'Normal'
   *   means 1 sample per grid line (i.e., B/W).  In the current Microsoft
   *   implementation, 'high' means an extra virtual resolution of 16x16 (or
   *   16x1) grid lines per pixel for bytecode instructions like 'MIRP'.
   *   After hinting, these 16 grid lines are mapped to 6x5 (or 6x1) grid
   *   lines for color filtering if Color ClearType is activated.
   *
   *   Note that 'Gray ClearType' is essentially the same as v1.6's grayscale
   *   rendering.  However, the GETINFO instruction handles it differently:
   *   v1.6 returns bit~12 (hinting for grayscale), while v2.1 returns
   *   bits~13 (hinting for ClearType), 18 (symmetrical smoothing), and~19
   *   (Gray ClearType).  Also, this mode respects bits 2 and~3 for the
   *   version~1 gasp table exclusively (like Color ClearType), while v1.6
   *   only respects the values of version~0 (bits 0 and~1).
   *
   *   Keep in mind that the features of the above interpreter versions might
   *   not map exactly to FreeType features or behavior because it is a
   *   fundamentally different library with different internals.
   *
   */
#define TT_INTERPRETER_VERSION_35  35
#define TT_INTERPRETER_VERSION_38  38
#define TT_INTERPRETER_VERSION_40  40


  /**************************************************************************
   *
   * @property:
   *   interpreter-version
   *
   * @description:
   *   Currently, three versions are available, two representing the bytecode
   *   interpreter with subpixel hinting support (old 'Infinality' code and
   *   new stripped-down and higher performance 'minimal' code) and one
   *   without, respectively.  The default is subpixel support if
   *   `TT_CONFIG_OPTION_SUBPIXEL_HINTING` is defined, and no subpixel
   *   support otherwise (since it isn't available then).
   *
   *   If subpixel hinting is on, many TrueType bytecode instructions behave
   *   differently compared to B/W or grayscale rendering (except if 'native
   *   ClearType' is selected by the font).  Microsoft's main idea is to
   *   render at a much increased horizontal resolution, then sampling down
   *   the created output to subpixel precision.  However, many older fonts
   *   are not suited to this and must be specially taken care of by applying
   *   (hardcoded) tweaks in Microsoft's interpreter.
   *
   *   Details on subpixel hinting and some of the necessary tweaks can be
   *   found in Greg Hitchcock's whitepaper at
   *   'https://www.microsoft.com/typography/cleartype/truetypecleartype.aspx'.
   *   Note that FreeType currently doesn't really 'subpixel hint' (6x1, 6x2,
   *   or 6x5 supersampling) like discussed in the paper.  Depending on the
   *   chosen interpreter, it simply ignores instructions on vertical stems
   *   to arrive at very similar results.
   *
   * @note:
   *   This property can be used with @FT_Property_Get also.
   *
   *   This property can be set via the `FREETYPE_PROPERTIES` environment
   *   variable (using values '35', '38', or '40').
   *
   * @example:
   *   The following example code demonstrates how to deactivate subpixel
   *   hinting (omitting the error handling).
   *
   *   ```
   *     FT_Library  library;
   *     FT_Face     face;
   *     FT_UInt     interpreter_version = TT_INTERPRETER_VERSION_35;
   *
   *
   *     FT_Init_FreeType( &library );
   *
   *     FT_Property_Set( library, "truetype",
   *                               "interpreter-version",
   *                               &interpreter_version );
   *   ```
   *
   * @since:
   *   2.5
   */


  /**************************************************************************
   *
   * @property:
   *   spread
   *
   * @description:
   *   This property of the 'sdf' and 'bsdf' renderers defines how the signed
   *   distance field (SDF) is represented in the output bitmap.  The output
   *   values are calculated as follows, '128 * ( SDF / spread + 1 )', with
   *   the result clamped to the 8-bit range [0..255].  Therefore, 'spread'
   *   is also the maximum euclidean distance from the edge after which the
   *   values are clamped.  The spread is specified in pixels with the
   *   default value of 8.  For accurate SDF texture mapping (interpolation),
   *   the spread should be large enough to accommodate the target grid unit.
   *
   * @example:
   *   The following example code demonstrates how to set the SDF spread
   *   (omitting the error handling).
   *
   *   ```
   *     FT_Library  library;
   *     FT_Int      spread = 2;
   *
   *
   *     FT_Init_FreeType( &library );
   *
   *     FT_Property_Set( library, "sdf", "spread", &spread );
   *   ```
   *
   * @note
   *   FreeType has two rasterizers for generating SDF, namely:
   *
   *   1. `sdf` for generating SDF directly from glyph's outline, and
   *
   *   2. `bsdf` for generating SDF from rasterized bitmaps.
   *
   *   Depending on the glyph type (i.e., outline or bitmap), one of the two
   *   rasterizers is chosen at runtime and used for generating SDFs.  To
   *   force the use of `bsdf` you should render the glyph with any of the
   *   FreeType's other rendering modes (e.g., `FT_RENDER_MODE_NORMAL`) and
   *   then re-render with `FT_RENDER_MODE_SDF`.
   *
   *   There are some issues with stability and possible failures of the SDF
   *   renderers (specifically `sdf`).
   *
   *   1. The `sdf` rasterizer is sensitive to really small features (e.g.,
   *      sharp turns that are less than 1~pixel) and imperfections in the
   *      glyph's outline, causing artifacts in the final output.
   *
   *   2. The `sdf` rasterizer has limited support for handling intersecting
   *      contours and *cannot* handle self-intersecting contours whatsoever.
   *      Self-intersection happens when a single connected contour
   *      intersects itself at some point; having these in your font
   *      definitely poses a problem to the rasterizer and cause artifacts,
   *      too.
   *
   *   3. Generating SDF for really small glyphs may result in undesirable
   *      output; the pixel grid (which stores distance information) becomes
   *      too coarse.
   *
   *   4. Since the output buffer is normalized, precision at smaller spreads
   *      is greater than precision at larger spread values because the
   *      output range of [0..255] gets mapped to a smaller SDF range.  A
   *      spread of~2 should be sufficient in most cases.
   *
   *   Points (1) and (2) can be avoided by using the `bsdf` rasterizer,
   *   which is more stable than the `sdf` rasterizer in general.
   *
   * @since:
   *   2.11
   */


  /**************************************************************************
   *
   * @property:
   *   svg-hooks
   *
   * @description:
   *   Set up the interface between FreeType and an extern SVG rendering
   *   library like 'librsvg'.  All details on the function hooks can be
   *   found in section @svg_fonts.
   *
   * @example:
   *   The following example code expects that the four hook functions
   *   `svg_*` are defined elsewhere.  Error handling is omitted, too.
   *
   *   ```
   *     FT_Library  library;
   *     SVG_RendererHooks  hooks = {
   *                          (SVG_Lib_Init_Func)svg_init,
   *                          (SVG_Lib_Free_Func)svg_free,
   *                          (SVG_Lib_Render_Func)svg_render,
   *                          (SVG_Lib_Preset_Slot_Func)svg_preset_slot };
   *
   *
   *     FT_Init_FreeType( &library );
   *
   *     FT_Property_Set( library, "ot-svg",
   *                               "svg-hooks", &hooks );
   *   ```
   *
   * @since:
   *   2.12
   */


  /**************************************************************************
   *
   * @property:
   *   glyph-to-script-map
   *
   * @description:
   *   **Experimental only**
   *
   *   The auto-hinter provides various script modules to hint glyphs.
   *   Examples of supported scripts are Latin or CJK.  Before a glyph is
   *   auto-hinted, the Unicode character map of the font gets examined, and
   *   the script is then determined based on Unicode character ranges, see
   *   below.
   *
   *   OpenType fonts, however, often provide much more glyphs than character
   *   codes (small caps, superscripts, ligatures, swashes, etc.), to be
   *   controlled by so-called 'features'.  Handling OpenType features can be
   *   quite complicated and thus needs a separate library on top of
   *   FreeType.
   *
   *   The mapping between glyph indices and scripts (in the auto-hinter
   *   sense, see the @FT_AUTOHINTER_SCRIPT_XXX values) is stored as an array
   *   with `num_glyphs` elements, as found in the font's @FT_Face structure.
   *   The `glyph-to-script-map` property returns a pointer to this array,
   *   which can be modified as needed.  Note that the modification should
   *   happen before the first glyph gets processed by the auto-hinter so
   *   that the global analysis of the font shapes actually uses the modified
   *   mapping.
   *
   * @example:
   *   The following example code demonstrates how to access it (omitting the
   *   error handling).
   *
   *   ```
   *     FT_Library                library;
   *     FT_Face                   face;
   *     FT_Prop_GlyphToScriptMap  prop;
   *
   *
   *     FT_Init_FreeType( &library );
   *     FT_New_Face( library, "foo.ttf", 0, &face );
   *
   *     prop.face = face;
   *
   *     FT_Property_Get( library, "autofitter",
   *                               "glyph-to-script-map", &prop );
   *
   *     // adjust `prop.map' as needed right here
   *
   *     FT_Load_Glyph( face, ..., FT_LOAD_FORCE_AUTOHINT );
   *   ```
   *
   * @since:
   *   2.4.11
   *
   */


  /**************************************************************************
   *
   * @enum:
   *   FT_AUTOHINTER_SCRIPT_XXX
   *
   * @description:
   *   **Experimental only**
   *
   *   A list of constants used for the @glyph-to-script-map property to
   *   specify the script submodule the auto-hinter should use for hinting a
   *   particular glyph.
   *
   * @values:
   *   FT_AUTOHINTER_SCRIPT_NONE ::
   *     Don't auto-hint this glyph.
   *
   *   FT_AUTOHINTER_SCRIPT_LATIN ::
   *     Apply the latin auto-hinter.  For the auto-hinter, 'latin' is a very
   *     broad term, including Cyrillic and Greek also since characters from
   *     those scripts share the same design constraints.
   *
   *     By default, characters from the following Unicode ranges are
   *     assigned to this submodule.
   *
   *     ```
   *       U+0020 - U+007F  // Basic Latin (no control characters)
   *       U+00A0 - U+00FF  // Latin-1 Supplement (no control characters)
   *       U+0100 - U+017F  // Latin Extended-A
   *       U+0180 - U+024F  // Latin Extended-B
   *       U+0250 - U+02AF  // IPA Extensions
   *       U+02B0 - U+02FF  // Spacing Modifier Letters
   *       U+0300 - U+036F  // Combining Diacritical Marks
   *       U+0370 - U+03FF  // Greek and Coptic
   *       U+0400 - U+04FF  // Cyrillic
   *       U+0500 - U+052F  // Cyrillic Supplement
   *       U+1D00 - U+1D7F  // Phonetic Extensions
   *       U+1D80 - U+1DBF  // Phonetic Extensions Supplement
   *       U+1DC0 - U+1DFF  // Combining Diacritical Marks Supplement
   *       U+1E00 - U+1EFF  // Latin Extended Additional
   *       U+1F00 - U+1FFF  // Greek Extended
   *       U+2000 - U+206F  // General Punctuation
   *       U+2070 - U+209F  // Superscripts and Subscripts
   *       U+20A0 - U+20CF  // Currency Symbols
   *       U+2150 - U+218F  // Number Forms
   *       U+2460 - U+24FF  // Enclosed Alphanumerics
   *       U+2C60 - U+2C7F  // Latin Extended-C
   *       U+2DE0 - U+2DFF  // Cyrillic Extended-A
   *       U+2E00 - U+2E7F  // Supplemental Punctuation
   *       U+A640 - U+A69F  // Cyrillic Extended-B
   *       U+A720 - U+A7FF  // Latin Extended-D
   *       U+FB00 - U+FB06  // Alphab. Present. Forms (Latin Ligatures)
   *      U+1D400 - U+1D7FF // Mathematical Alphanumeric Symbols
   *      U+1F100 - U+1F1FF // Enclosed Alphanumeric Supplement
   *     ```
   *
   *   FT_AUTOHINTER_SCRIPT_CJK ::
   *     Apply the CJK auto-hinter, covering Chinese, Japanese, Korean, old
   *     Vietnamese, and some other scripts.
   *
   *     By default, characters from the following Unicode ranges are
   *     assigned to this submodule.
   *
   *     ```
   *       U+1100 - U+11FF  // Hangul Jamo
   *       U+2E80 - U+2EFF  // CJK Radicals Supplement
   *       U+2F00 - U+2FDF  // Kangxi Radicals
   *       U+2FF0 - U+2FFF  // Ideographic Description Characters
   *       U+3000 - U+303F  // CJK Symbols and Punctuation
   *       U+3040 - U+309F  // Hiragana
   *       U+30A0 - U+30FF  // Katakana
   *       U+3100 - U+312F  // Bopomofo
   *       U+3130 - U+318F  // Hangul Compatibility Jamo
   *       U+3190 - U+319F  // Kanbun
   *       U+31A0 - U+31BF  // Bopomofo Extended
   *       U+31C0 - U+31EF  // CJK Strokes
   *       U+31F0 - U+31FF  // Katakana Phonetic Extensions
   *       U+3200 - U+32FF  // Enclosed CJK Letters and Months
   *       U+3300 - U+33FF  // CJK Compatibility
   *       U+3400 - U+4DBF  // CJK Unified Ideographs Extension A
   *       U+4DC0 - U+4DFF  // Yijing Hexagram Symbols
   *       U+4E00 - U+9FFF  // CJK Unified Ideographs
   *       U+A960 - U+A97F  // Hangul Jamo Extended-A
   *       U+AC00 - U+D7AF  // Hangul Syllables
   *       U+D7B0 - U+D7FF  // Hangul Jamo Extended-B
   *       U+F900 - U+FAFF  // CJK Compatibility Ideographs
   *       U+FE10 - U+FE1F  // Vertical forms
   *       U+FE30 - U+FE4F  // CJK Compatibility Forms
   *       U+FF00 - U+FFEF  // Halfwidth and Fullwidth Forms
   *      U+1B000 - U+1B0FF // Kana Supplement
   *      U+1D300 - U+1D35F // Tai Xuan Hing Symbols
   *      U+1F200 - U+1F2FF // Enclosed Ideographic Supplement
   *      U+20000 - U+2A6DF // CJK Unified Ideographs Extension B
   *      U+2A700 - U+2B73F // CJK Unified Ideographs Extension C
   *      U+2B740 - U+2B81F // CJK Unified Ideographs Extension D
   *      U+2F800 - U+2FA1F // CJK Compatibility Ideographs Supplement
   *     ```
   *
   *   FT_AUTOHINTER_SCRIPT_INDIC ::
   *     Apply the indic auto-hinter, covering all major scripts from the
   *     Indian sub-continent and some other related scripts like Thai, Lao,
   *     or Tibetan.
   *
   *     By default, characters from the following Unicode ranges are
   *     assigned to this submodule.
   *
   *     ```
   *       U+0900 - U+0DFF  // Indic Range
   *       U+0F00 - U+0FFF  // Tibetan
   *       U+1900 - U+194F  // Limbu
   *       U+1B80 - U+1BBF  // Sundanese
   *       U+A800 - U+A82F  // Syloti Nagri
   *       U+ABC0 - U+ABFF  // Meetei Mayek
   *      U+11800 - U+118DF // Sharada
   *     ```
   *
   *     Note that currently Indic support is rudimentary only, missing blue
   *     zone support.
   *
   * @since:
   *   2.4.11
   *
   */
#define FT_AUTOHINTER_SCRIPT_NONE   0
#define FT_AUTOHINTER_SCRIPT_LATIN  1
#define FT_AUTOHINTER_SCRIPT_CJK    2
#define FT_AUTOHINTER_SCRIPT_INDIC  3


  /**************************************************************************
   *
   * @struct:
   *   FT_Prop_GlyphToScriptMap
   *
   * @description:
   *   **Experimental only**
   *
   *   The data exchange structure for the @glyph-to-script-map property.
   *
   * @since:
   *   2.4.11
   *
   */
  typedef struct  FT_Prop_GlyphToScriptMap_
  {
    FT_Face     face;
    FT_UShort*  map;

  } FT_Prop_GlyphToScriptMap;


  /**************************************************************************
   *
   * @property:
   *   fallback-script
   *
   * @description:
   *   **Experimental only**
   *
   *   If no auto-hinter script module can be assigned to a glyph, a fallback
   *   script gets assigned to it (see also the @glyph-to-script-map
   *   property).  By default, this is @FT_AUTOHINTER_SCRIPT_CJK.  Using the
   *   `fallback-script` property, this fallback value can be changed.
   *
   * @note:
   *   This property can be used with @FT_Property_Get also.
   *
   *   It's important to use the right timing for changing this value: The
   *   creation of the glyph-to-script map that eventually uses the fallback
   *   script value gets triggered either by setting or reading a
   *   face-specific property like @glyph-to-script-map, or by auto-hinting
   *   any glyph from that face.  In particular, if you have already created
   *   an @FT_Face structure but not loaded any glyph (using the
   *   auto-hinter), a change of the fallback script will affect this face.
   *
   * @example:
   *   ```
   *     FT_Library  library;
   *     FT_UInt     fallback_script = FT_AUTOHINTER_SCRIPT_NONE;
   *
   *
   *     FT_Init_FreeType( &library );
   *
   *     FT_Property_Set( library, "autofitter",
   *                               "fallback-script", &fallback_script );
   *   ```
   *
   * @since:
   *   2.4.11
   *
   */


  /**************************************************************************
   *
   * @property:
   *   default-script
   *
   * @description:
   *   **Experimental only**
   *
   *   If FreeType gets compiled with `FT_CONFIG_OPTION_USE_HARFBUZZ` to make
   *   the HarfBuzz library access OpenType features for getting better glyph
   *   coverages, this property sets the (auto-fitter) script to be used for
   *   the default (OpenType) script data of a font's GSUB table.  Features
   *   for the default script are intended for all scripts not explicitly
   *   handled in GSUB; an example is a 'dlig' feature, containing the
   *   combination of the characters 'T', 'E', and 'L' to form a 'TEL'
   *   ligature.
   *
   *   By default, this is @FT_AUTOHINTER_SCRIPT_LATIN.  Using the
   *   `default-script` property, this default value can be changed.
   *
   * @note:
   *   This property can be used with @FT_Property_Get also.
   *
   *   It's important to use the right timing for changing this value: The
   *   creation of the glyph-to-script map that eventually uses the default
   *   script value gets triggered either by setting or reading a
   *   face-specific property like @glyph-to-script-map, or by auto-hinting
   *   any glyph from that face.  In particular, if you have already created
   *   an @FT_Face structure but not loaded any glyph (using the
   *   auto-hinter), a change of the default script will affect this face.
   *
   * @example:
   *   ```
   *     FT_Library  library;
   *     FT_UInt     default_script = FT_AUTOHINTER_SCRIPT_NONE;
   *
   *
   *     FT_Init_FreeType( &library );
   *
   *     FT_Property_Set( library, "autofitter",
   *                               "default-script", &default_script );
   *   ```
   *
   * @since:
   *   2.5.3
   *
   */


  /**************************************************************************
   *
   * @property:
   *   increase-x-height
   *
   * @description:
   *   For ppem values in the range 6~<= ppem <= `increase-x-height`, round
   *   up the font's x~height much more often than normally.  If the value is
   *   set to~0, which is the default, this feature is switched off.  Use
   *   this property to improve the legibility of small font sizes if
   *   necessary.
   *
   * @note:
   *   This property can be used with @FT_Property_Get also.
   *
   *   Set this value right after calling @FT_Set_Char_Size, but before
   *   loading any glyph (using the auto-hinter).
   *
   * @example:
   *   ```
   *     FT_Library               library;
   *     FT_Face                  face;
   *     FT_Prop_IncreaseXHeight  prop;
   *
   *
   *     FT_Init_FreeType( &library );
   *     FT_New_Face( library, "foo.ttf", 0, &face );
   *     FT_Set_Char_Size( face, 10 * 64, 0, 72, 0 );
   *
   *     prop.face  = face;
   *     prop.limit = 14;
   *
   *     FT_Property_Set( library, "autofitter",
   *                               "increase-x-height", &prop );
   *   ```
   *
   * @since:
   *   2.4.11
   *
   */


  /**************************************************************************
   *
   * @struct:
   *   FT_Prop_IncreaseXHeight
   *
   * @description:
   *   The data exchange structure for the @increase-x-height property.
   *
   */
  typedef struct  FT_Prop_IncreaseXHeight_
  {
    FT_Face  face;
    FT_UInt  limit;

  } FT_Prop_IncreaseXHeight;


  /**************************************************************************
   *
   * @property:
   *   warping
   *
   * @description:
   *   **Obsolete**
   *
   *   This property was always experimental and probably never worked
   *   correctly.  It was entirely removed from the FreeType~2 sources.  This
   *   entry is only here for historical reference.
   *
   *   Warping only worked in 'normal' auto-hinting mode replacing it.  The
   *   idea of the code was to slightly scale and shift a glyph along the
   *   non-hinted dimension (which is usually the horizontal axis) so that as
   *   much of its segments were aligned (more or less) to the grid.  To find
   *   out a glyph's optimal scaling and shifting value, various parameter
   *   combinations were tried and scored.
   *
   * @since:
   *   2.6
   *
   */


 /* */





#endif /* FTDRIVER_H_ */


/* END */
//
// ===========================  ftgasp.h  ===========================
//
/****************************************************************************
 *
 * ftgasp.h
 *
 *   Access of TrueType's 'gasp' table (specification).
 *
 * Copyright (C) 2007-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTGASP_H_
#define FTGASP_H_

#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   gasp_table
   *
   * @title:
   *   Gasp Table
   *
   * @abstract:
   *   Retrieving TrueType 'gasp' table entries.
   *
   * @description:
   *   The function @FT_Get_Gasp can be used to query a TrueType or OpenType
   *   font for specific entries in its 'gasp' table, if any.  This is mainly
   *   useful when implementing native TrueType hinting with the bytecode
   *   interpreter to duplicate the Windows text rendering results.
   */

  /**************************************************************************
   *
   * @enum:
   *   FT_GASP_XXX
   *
   * @description:
   *   A list of values and/or bit-flags returned by the @FT_Get_Gasp
   *   function.
   *
   * @values:
   *   FT_GASP_NO_TABLE ::
   *     This special value means that there is no GASP table in this face.
   *     It is up to the client to decide what to do.
   *
   *   FT_GASP_DO_GRIDFIT ::
   *     Grid-fitting and hinting should be performed at the specified ppem.
   *     This **really** means TrueType bytecode interpretation.  If this bit
   *     is not set, no hinting gets applied.
   *
   *   FT_GASP_DO_GRAY ::
   *     Anti-aliased rendering should be performed at the specified ppem.
   *     If not set, do monochrome rendering.
   *
   *   FT_GASP_SYMMETRIC_SMOOTHING ::
   *     If set, smoothing along multiple axes must be used with ClearType.
   *
   *   FT_GASP_SYMMETRIC_GRIDFIT ::
   *     Grid-fitting must be used with ClearType's symmetric smoothing.
   *
   * @note:
   *   The bit-flags `FT_GASP_DO_GRIDFIT` and `FT_GASP_DO_GRAY` are to be
   *   used for standard font rasterization only.  Independently of that,
   *   `FT_GASP_SYMMETRIC_SMOOTHING` and `FT_GASP_SYMMETRIC_GRIDFIT` are to
   *   be used if ClearType is enabled (and `FT_GASP_DO_GRIDFIT` and
   *   `FT_GASP_DO_GRAY` are consequently ignored).
   *
   *   'ClearType' is Microsoft's implementation of LCD rendering, partly
   *   protected by patents.
   *
   * @since:
   *   2.3.0
   */
#define FT_GASP_NO_TABLE               -1
#define FT_GASP_DO_GRIDFIT           0x01
#define FT_GASP_DO_GRAY              0x02
#define FT_GASP_SYMMETRIC_GRIDFIT    0x04
#define FT_GASP_SYMMETRIC_SMOOTHING  0x08


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Gasp
   *
   * @description:
   *   For a TrueType or OpenType font file, return the rasterizer behaviour
   *   flags from the font's 'gasp' table corresponding to a given character
   *   pixel size.
   *
   * @input:
   *   face ::
   *     The source face handle.
   *
   *   ppem ::
   *     The vertical character pixel size.
   *
   * @return:
   *   Bit flags (see @FT_GASP_XXX), or @FT_GASP_NO_TABLE if there is no
   *   'gasp' table in the face.
   *
   * @note:
   *   If you want to use the MM functionality of OpenType variation fonts
   *   (i.e., using @FT_Set_Var_Design_Coordinates and friends), call this
   *   function **after** setting an instance since the return values can
   *   change.
   *
   * @since:
   *   2.3.0
   */
   FT_Int 
  FT_Get_Gasp( FT_Face  face,
               FT_UInt  ppem );

  /* */




#endif /* FTGASP_H_ */


/* END */
//
// ===========================  ftfntfmt.h  ===========================
//
/****************************************************************************
 *
 * ftfntfmt.h
 *
 *   Support functions for font formats.
 *
 * Copyright (C) 2002-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTFNTFMT_H_
#define FTFNTFMT_H_

#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *  font_formats
   *
   * @title:
   *  Font Formats
   *
   * @abstract:
   *  Getting the font format.
   *
   * @description:
   *  The single function in this section can be used to get the font format.
   *  Note that this information is not needed normally; however, there are
   *  special cases (like in PDF devices) where it is important to
   *  differentiate, in spite of FreeType's uniform API.
   *
   */


  /**************************************************************************
   *
   * @function:
   *  FT_Get_Font_Format
   *
   * @description:
   *  Return a string describing the format of a given face.  Possible values
   *  are 'TrueType', 'Type~1', 'BDF', 'PCF', 'Type~42', 'CID~Type~1', 'CFF',
   *  'PFR', and 'Windows~FNT'.
   *
   *  The return value is suitable to be used as an X11 FONT_PROPERTY.
   *
   * @input:
   *  face ::
   *    Input face handle.
   *
   * @return:
   *  Font format string.  `NULL` in case of error.
   *
   * @note:
   *  A deprecated name for the same function is `FT_Get_X11_Font_Format`.
   */
   const char* 
  FT_Get_Font_Format( FT_Face  face );


  /* deprecated */
   const char* 
  FT_Get_X11_Font_Format( FT_Face  face );


  /* */




#endif /* FTFNTFMT_H_ */


/* END */
//
// ===========================  ftwinfnt.h  ===========================
//
/****************************************************************************
 *
 * ftwinfnt.h
 *
 *   FreeType API for accessing Windows fnt-specific data.
 *
 * Copyright (C) 2003-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTWINFNT_H_
#define FTWINFNT_H_

#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   winfnt_fonts
   *
   * @title:
   *   Window FNT Files
   *
   * @abstract:
   *   Windows FNT-specific API.
   *
   * @description:
   *   This section contains the declaration of Windows FNT-specific
   *   functions.
   *
   */


  /**************************************************************************
   *
   * @enum:
   *   FT_WinFNT_ID_XXX
   *
   * @description:
   *   A list of valid values for the `charset` byte in @FT_WinFNT_HeaderRec.
   *   Exact mapping tables for the various 'cpXXXX' encodings (except for
   *   'cp1361') can be found at 'ftp://ftp.unicode.org/Public/' in the
   *   `MAPPINGS/VENDORS/MICSFT/WINDOWS` subdirectory.  'cp1361' is roughly a
   *   superset of `MAPPINGS/OBSOLETE/EASTASIA/KSC/JOHAB.TXT`.
   *
   * @values:
   *   FT_WinFNT_ID_DEFAULT ::
   *     This is used for font enumeration and font creation as a 'don't
   *     care' value.  Valid font files don't contain this value.  When
   *     querying for information about the character set of the font that is
   *     currently selected into a specified device context, this return
   *     value (of the related Windows API) simply denotes failure.
   *
   *   FT_WinFNT_ID_SYMBOL ::
   *     There is no known mapping table available.
   *
   *   FT_WinFNT_ID_MAC ::
   *     Mac Roman encoding.
   *
   *   FT_WinFNT_ID_OEM ::
   *     From Michael Poettgen <michael@poettgen.de>:
   *
   *     The 'Windows Font Mapping' article says that `FT_WinFNT_ID_OEM` is
   *     used for the charset of vector fonts, like `modern.fon`,
   *     `roman.fon`, and `script.fon` on Windows.
   *
   *     The 'CreateFont' documentation says: The `FT_WinFNT_ID_OEM` value
   *     specifies a character set that is operating-system dependent.
   *
   *     The 'IFIMETRICS' documentation from the 'Windows Driver Development
   *     Kit' says: This font supports an OEM-specific character set.  The
   *     OEM character set is system dependent.
   *
   *     In general OEM, as opposed to ANSI (i.e., 'cp1252'), denotes the
   *     second default codepage that most international versions of Windows
   *     have.  It is one of the OEM codepages from
   *
   *     https://docs.microsoft.com/en-us/windows/desktop/intl/code-page-identifiers
   *     ,
   *
   *     and is used for the 'DOS boxes', to support legacy applications.  A
   *     German Windows version for example usually uses ANSI codepage 1252
   *     and OEM codepage 850.
   *
   *   FT_WinFNT_ID_CP874 ::
   *     A superset of Thai TIS 620 and ISO 8859-11.
   *
   *   FT_WinFNT_ID_CP932 ::
   *     A superset of Japanese Shift-JIS (with minor deviations).
   *
   *   FT_WinFNT_ID_CP936 ::
   *     A superset of simplified Chinese GB 2312-1980 (with different
   *     ordering and minor deviations).
   *
   *   FT_WinFNT_ID_CP949 ::
   *     A superset of Korean Hangul KS~C 5601-1987 (with different ordering
   *     and minor deviations).
   *
   *   FT_WinFNT_ID_CP950 ::
   *     A superset of traditional Chinese Big~5 ETen (with different
   *     ordering and minor deviations).
   *
   *   FT_WinFNT_ID_CP1250 ::
   *     A superset of East European ISO 8859-2 (with slightly different
   *     ordering).
   *
   *   FT_WinFNT_ID_CP1251 ::
   *     A superset of Russian ISO 8859-5 (with different ordering).
   *
   *   FT_WinFNT_ID_CP1252 ::
   *     ANSI encoding.  A superset of ISO 8859-1.
   *
   *   FT_WinFNT_ID_CP1253 ::
   *     A superset of Greek ISO 8859-7 (with minor modifications).
   *
   *   FT_WinFNT_ID_CP1254 ::
   *     A superset of Turkish ISO 8859-9.
   *
   *   FT_WinFNT_ID_CP1255 ::
   *     A superset of Hebrew ISO 8859-8 (with some modifications).
   *
   *   FT_WinFNT_ID_CP1256 ::
   *     A superset of Arabic ISO 8859-6 (with different ordering).
   *
   *   FT_WinFNT_ID_CP1257 ::
   *     A superset of Baltic ISO 8859-13 (with some deviations).
   *
   *   FT_WinFNT_ID_CP1258 ::
   *     For Vietnamese.  This encoding doesn't cover all necessary
   *     characters.
   *
   *   FT_WinFNT_ID_CP1361 ::
   *     Korean (Johab).
   */

#define FT_WinFNT_ID_CP1252    0
#define FT_WinFNT_ID_DEFAULT   1
#define FT_WinFNT_ID_SYMBOL    2
#define FT_WinFNT_ID_MAC      77
#define FT_WinFNT_ID_CP932   128
#define FT_WinFNT_ID_CP949   129
#define FT_WinFNT_ID_CP1361  130
#define FT_WinFNT_ID_CP936   134
#define FT_WinFNT_ID_CP950   136
#define FT_WinFNT_ID_CP1253  161
#define FT_WinFNT_ID_CP1254  162
#define FT_WinFNT_ID_CP1258  163
#define FT_WinFNT_ID_CP1255  177
#define FT_WinFNT_ID_CP1256  178
#define FT_WinFNT_ID_CP1257  186
#define FT_WinFNT_ID_CP1251  204
#define FT_WinFNT_ID_CP874   222
#define FT_WinFNT_ID_CP1250  238
#define FT_WinFNT_ID_OEM     255


  /**************************************************************************
   *
   * @struct:
   *   FT_WinFNT_HeaderRec
   *
   * @description:
   *   Windows FNT Header info.
   */
  typedef struct  FT_WinFNT_HeaderRec_
  {
    FT_UShort  version;
    FT_ULong   file_size;
    FT_Byte    copyright[60];
    FT_UShort  file_type;
    FT_UShort  nominal_point_size;
    FT_UShort  vertical_resolution;
    FT_UShort  horizontal_resolution;
    FT_UShort  ascent;
    FT_UShort  internal_leading;
    FT_UShort  external_leading;
    FT_Byte    italic;
    FT_Byte    underline;
    FT_Byte    strike_out;
    FT_UShort  weight;
    FT_Byte    charset;
    FT_UShort  pixel_width;
    FT_UShort  pixel_height;
    FT_Byte    pitch_and_family;
    FT_UShort  avg_width;
    FT_UShort  max_width;
    FT_Byte    first_char;
    FT_Byte    last_char;
    FT_Byte    default_char;
    FT_Byte    break_char;
    FT_UShort  bytes_per_row;
    FT_ULong   device_offset;
    FT_ULong   face_name_offset;
    FT_ULong   bits_pointer;
    FT_ULong   bits_offset;
    FT_Byte    reserved;
    FT_ULong   flags;
    FT_UShort  A_space;
    FT_UShort  B_space;
    FT_UShort  C_space;
    FT_UShort  color_table_offset;
    FT_ULong   reserved1[4];

  } FT_WinFNT_HeaderRec;


  /**************************************************************************
   *
   * @struct:
   *   FT_WinFNT_Header
   *
   * @description:
   *   A handle to an @FT_WinFNT_HeaderRec structure.
   */
  typedef struct FT_WinFNT_HeaderRec_*  FT_WinFNT_Header;


  /**************************************************************************
   *
   * @function:
   *    FT_Get_WinFNT_Header
   *
   * @description:
   *    Retrieve a Windows FNT font info header.
   *
   * @input:
   *    face ::
   *      A handle to the input face.
   *
   * @output:
   *    aheader ::
   *      The WinFNT header.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function only works with Windows FNT faces, returning an error
   *   otherwise.
   */
   FT_Error 
  FT_Get_WinFNT_Header( FT_Face               face,
                        FT_WinFNT_HeaderRec  *aheader );

  /* */




#endif /* FTWINFNT_H_ */


/* END */


/* Local Variables: */
/* coding: utf-8    */
/* End:             */
//
// ===========================  ftcolor.h  ===========================
//
/****************************************************************************
 *
 * ftcolor.h
 *
 *   FreeType's glyph color management (specification).
 *
 * Copyright (C) 2018-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTCOLOR_H_
#define FTCOLOR_H_

#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   color_management
   *
   * @title:
   *   Glyph Color Management
   *
   * @abstract:
   *   Retrieving and manipulating OpenType's 'CPAL' table data.
   *
   * @description:
   *   The functions described here allow access and manipulation of color
   *   palette entries in OpenType's 'CPAL' tables.
   */


  /**************************************************************************
   *
   * @struct:
   *   FT_Color
   *
   * @description:
   *   This structure models a BGRA color value of a 'CPAL' palette entry.
   *
   *   The used color space is sRGB; the colors are not pre-multiplied, and
   *   alpha values must be explicitly set.
   *
   * @fields:
   *   blue ::
   *     Blue value.
   *
   *   green ::
   *     Green value.
   *
   *   red ::
   *     Red value.
   *
   *   alpha ::
   *     Alpha value, giving the red, green, and blue color's opacity.
   *
   * @since:
   *   2.10
   */
  typedef struct  FT_Color_
  {
    FT_Byte  blue;
    FT_Byte  green;
    FT_Byte  red;
    FT_Byte  alpha;

  } FT_Color;


  /**************************************************************************
   *
   * @enum:
   *   FT_PALETTE_XXX
   *
   * @description:
   *   A list of bit field constants used in the `palette_flags` array of the
   *   @FT_Palette_Data structure to indicate for which background a palette
   *   with a given index is usable.
   *
   * @values:
   *   FT_PALETTE_FOR_LIGHT_BACKGROUND ::
   *     The palette is appropriate to use when displaying the font on a
   *     light background such as white.
   *
   *   FT_PALETTE_FOR_DARK_BACKGROUND ::
   *     The palette is appropriate to use when displaying the font on a dark
   *     background such as black.
   *
   * @since:
   *   2.10
   */
#define FT_PALETTE_FOR_LIGHT_BACKGROUND  0x01
#define FT_PALETTE_FOR_DARK_BACKGROUND   0x02


  /**************************************************************************
   *
   * @struct:
   *   FT_Palette_Data
   *
   * @description:
   *   This structure holds the data of the 'CPAL' table.
   *
   * @fields:
   *   num_palettes ::
   *     The number of palettes.
   *
   *   palette_name_ids ::
   *     An optional read-only array of palette name IDs with `num_palettes`
   *     elements, corresponding to entries like 'dark' or 'light' in the
   *     font's 'name' table.
   *
   *     An empty name ID in the 'CPAL' table gets represented as value
   *     0xFFFF.
   *
   *     `NULL` if the font's 'CPAL' table doesn't contain appropriate data.
   *
   *   palette_flags ::
   *     An optional read-only array of palette flags with `num_palettes`
   *     elements.  Possible values are an ORed combination of
   *     @FT_PALETTE_FOR_LIGHT_BACKGROUND and
   *     @FT_PALETTE_FOR_DARK_BACKGROUND.
   *
   *     `NULL` if the font's 'CPAL' table doesn't contain appropriate data.
   *
   *   num_palette_entries ::
   *     The number of entries in a single palette.  All palettes have the
   *     same size.
   *
   *   palette_entry_name_ids ::
   *     An optional read-only array of palette entry name IDs with
   *     `num_palette_entries`.  In each palette, entries with the same index
   *     have the same function.  For example, index~0 might correspond to
   *     string 'outline' in the font's 'name' table to indicate that this
   *     palette entry is used for outlines, index~1 might correspond to
   *     'fill' to indicate the filling color palette entry, etc.
   *
   *     An empty entry name ID in the 'CPAL' table gets represented as value
   *     0xFFFF.
   *
   *     `NULL` if the font's 'CPAL' table doesn't contain appropriate data.
   *
   * @note:
   *   Use function @FT_Get_Sfnt_Name to map name IDs and entry name IDs to
   *   name strings.
   *
   *   Use function @FT_Palette_Select to get the colors associated with a
   *   palette entry.
   *
   * @since:
   *   2.10
   */
  typedef struct  FT_Palette_Data_ {
    FT_UShort         num_palettes;
    const FT_UShort*  palette_name_ids;
    const FT_UShort*  palette_flags;

    FT_UShort         num_palette_entries;
    const FT_UShort*  palette_entry_name_ids;

  } FT_Palette_Data;


  /**************************************************************************
   *
   * @function:
   *   FT_Palette_Data_Get
   *
   * @description:
   *   Retrieve the face's color palette data.
   *
   * @input:
   *   face ::
   *     The source face handle.
   *
   * @output:
   *   apalette ::
   *     A pointer to an @FT_Palette_Data structure.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   All arrays in the returned @FT_Palette_Data structure are read-only.
   *
   *   This function always returns an error if the config macro
   *   `TT_CONFIG_OPTION_COLOR_LAYERS` is not defined in `ftoption.h`.
   *
   * @since:
   *   2.10
   */
   FT_Error 
  FT_Palette_Data_Get( FT_Face           face,
                       FT_Palette_Data  *apalette );


  /**************************************************************************
   *
   * @function:
   *   FT_Palette_Select
   *
   * @description:
   *   This function has two purposes.
   *
   *   (1) It activates a palette for rendering color glyphs, and
   *
   *   (2) it retrieves all (unmodified) color entries of this palette.  This
   *       function returns a read-write array, which means that a calling
   *       application can modify the palette entries on demand.
   *
   * A corollary of (2) is that calling the function, then modifying some
   * values, then calling the function again with the same arguments resets
   * all color entries to the original 'CPAL' values; all user modifications
   * are lost.
   *
   * @input:
   *   face ::
   *     The source face handle.
   *
   *   palette_index ::
   *     The palette index.
   *
   * @output:
   *   apalette ::
   *     An array of color entries for a palette with index `palette_index`,
   *     having `num_palette_entries` elements (as found in the
   *     `FT_Palette_Data` structure).  If `apalette` is set to `NULL`, no
   *     array gets returned (and no color entries can be modified).
   *
   *     In case the font doesn't support color palettes, `NULL` is returned.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The array pointed to by `apalette_entries` is owned and managed by
   *   FreeType.
   *
   *   This function always returns an error if the config macro
   *   `TT_CONFIG_OPTION_COLOR_LAYERS` is not defined in `ftoption.h`.
   *
   * @since:
   *   2.10
   */
   FT_Error 
  FT_Palette_Select( FT_Face     face,
                     FT_UShort   palette_index,
                     FT_Color*  *apalette );


  /**************************************************************************
   *
   * @function:
   *   FT_Palette_Set_Foreground_Color
   *
   * @description:
   *   'COLR' uses palette index 0xFFFF to indicate a 'text foreground
   *   color'.  This function sets this value.
   *
   * @input:
   *   face ::
   *     The source face handle.
   *
   *   foreground_color ::
   *     An `FT_Color` structure to define the text foreground color.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   If this function isn't called, the text foreground color is set to
   *   white opaque (BGRA value 0xFFFFFFFF) if
   *   @FT_PALETTE_FOR_DARK_BACKGROUND is present for the current palette,
   *   and black opaque (BGRA value 0x000000FF) otherwise, including the case
   *   that no palette types are available in the 'CPAL' table.
   *
   *   This function always returns an error if the config macro
   *   `TT_CONFIG_OPTION_COLOR_LAYERS` is not defined in `ftoption.h`.
   *
   * @since:
   *   2.10
   */
   FT_Error 
  FT_Palette_Set_Foreground_Color( FT_Face   face,
                                   FT_Color  foreground_color );


  /**************************************************************************
   *
   * @section:
   *   layer_management
   *
   * @title:
   *   Glyph Layer Management
   *
   * @abstract:
   *   Retrieving and manipulating OpenType's 'COLR' table data.
   *
   * @description:
   *   The functions described here allow access of colored glyph layer data
   *   in OpenType's 'COLR' tables.
   */


  /**************************************************************************
   *
   * @struct:
   *   FT_LayerIterator
   *
   * @description:
   *   This iterator object is needed for @FT_Get_Color_Glyph_Layer.
   *
   * @fields:
   *   num_layers ::
   *     The number of glyph layers for the requested glyph index.  Will be
   *     set by @FT_Get_Color_Glyph_Layer.
   *
   *   layer ::
   *     The current layer.  Will be set by @FT_Get_Color_Glyph_Layer.
   *
   *   p ::
   *     An opaque pointer into 'COLR' table data.  The caller must set this
   *     to `NULL` before the first call of @FT_Get_Color_Glyph_Layer.
   */
  typedef struct  FT_LayerIterator_
  {
    FT_UInt   num_layers;
    FT_UInt   layer;
    FT_Byte*  p;

  } FT_LayerIterator;


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Color_Glyph_Layer
   *
   * @description:
   *   This is an interface to the 'COLR' table in OpenType fonts to
   *   iteratively retrieve the colored glyph layers associated with the
   *   current glyph slot.
   *
   *     https://docs.microsoft.com/en-us/typography/opentype/spec/colr
   *
   *   The glyph layer data for a given glyph index, if present, provides an
   *   alternative, multi-color glyph representation: Instead of rendering
   *   the outline or bitmap with the given glyph index, glyphs with the
   *   indices and colors returned by this function are rendered layer by
   *   layer.
   *
   *   The returned elements are ordered in the z~direction from bottom to
   *   top; the 'n'th element should be rendered with the associated palette
   *   color and blended on top of the already rendered layers (elements 0,
   *   1, ..., n-1).
   *
   * @input:
   *   face ::
   *     A handle to the parent face object.
   *
   *   base_glyph ::
   *     The glyph index the colored glyph layers are associated with.
   *
   * @inout:
   *   iterator ::
   *     An @FT_LayerIterator object.  For the first call you should set
   *     `iterator->p` to `NULL`.  For all following calls, simply use the
   *     same object again.
   *
   * @output:
   *   aglyph_index ::
   *     The glyph index of the current layer.
   *
   *   acolor_index ::
   *     The color index into the font face's color palette of the current
   *     layer.  The value 0xFFFF is special; it doesn't reference a palette
   *     entry but indicates that the text foreground color should be used
   *     instead (to be set up by the application outside of FreeType).
   *
   *     The color palette can be retrieved with @FT_Palette_Select.
   *
   * @return:
   *   Value~1 if everything is OK.  If there are no more layers (or if there
   *   are no layers at all), value~0 gets returned.  In case of an error,
   *   value~0 is returned also.
   *
   * @note:
   *   This function is necessary if you want to handle glyph layers by
   *   yourself.  In particular, functions that operate with @FT_GlyphRec
   *   objects (like @FT_Get_Glyph or @FT_Glyph_To_Bitmap) don't have access
   *   to this information.
   *
   *   Note that @FT_Render_Glyph is able to handle colored glyph layers
   *   automatically if the @FT_LOAD_COLOR flag is passed to a previous call
   *   to @FT_Load_Glyph.  [This is an experimental feature.]
   *
   * @example:
   *   ```
   *     FT_Color*         palette;
   *     FT_LayerIterator  iterator;
   *
   *     FT_Bool  have_layers;
   *     FT_UInt  layer_glyph_index;
   *     FT_UInt  layer_color_index;
   *
   *
   *     error = FT_Palette_Select( face, palette_index, &palette );
   *     if ( error )
   *       palette = NULL;
   *
   *     iterator.p  = NULL;
   *     have_layers = FT_Get_Color_Glyph_Layer( face,
   *                                             glyph_index,
   *                                             &layer_glyph_index,
   *                                             &layer_color_index,
   *                                             &iterator );
   *
   *     if ( palette && have_layers )
   *     {
   *       do
   *       {
   *         FT_Color  layer_color;
   *
   *
   *         if ( layer_color_index == 0xFFFF )
   *           layer_color = text_foreground_color;
   *         else
   *           layer_color = palette[layer_color_index];
   *
   *         // Load and render glyph `layer_glyph_index', then
   *         // blend resulting pixmap (using color `layer_color')
   *         // with previously created pixmaps.
   *
   *       } while ( FT_Get_Color_Glyph_Layer( face,
   *                                           glyph_index,
   *                                           &layer_glyph_index,
   *                                           &layer_color_index,
   *                                           &iterator ) );
   *     }
   *   ```
   *
   * @since:
   *   2.10
   */
   FT_Bool 
  FT_Get_Color_Glyph_Layer( FT_Face            face,
                            FT_UInt            base_glyph,
                            FT_UInt           *aglyph_index,
                            FT_UInt           *acolor_index,
                            FT_LayerIterator*  iterator );


  /**************************************************************************
   *
   * @enum:
   *   FT_PaintFormat
   *
   * @description:
   *   Enumeration describing the different paint format types of the v1
   *   extensions to the 'COLR' table, see
   *   'https://github.com/googlefonts/colr-gradients-spec'.
   *
   *   The enumeration values loosely correspond with the format numbers of
   *   the specification: FreeType always returns a fully specified 'Paint'
   *   structure for the 'Transform', 'Translate', 'Scale', 'Rotate', and
   *   'Skew' table types even though the specification has different formats
   *   depending on whether or not a center is specified, whether the scale
   *   is uniform in x and y~direction or not, etc.  Also, only non-variable
   *   format identifiers are listed in this enumeration; as soon as support
   *   for variable 'COLR' v1 fonts is implemented, interpolation is
   *   performed dependent on axis coordinates, which are configured on the
   *   @FT_Face through @FT_Set_Var_Design_Coordinates.  This implies that
   *   always static, readily interpolated values are returned in the 'Paint'
   *   structures.
   *
   * @since:
   *   2.13
   */
  typedef enum  FT_PaintFormat_
  {
    FT_COLR_PAINTFORMAT_COLR_LAYERS     = 1,
    FT_COLR_PAINTFORMAT_SOLID           = 2,
    FT_COLR_PAINTFORMAT_LINEAR_GRADIENT = 4,
    FT_COLR_PAINTFORMAT_RADIAL_GRADIENT = 6,
    FT_COLR_PAINTFORMAT_SWEEP_GRADIENT  = 8,
    FT_COLR_PAINTFORMAT_GLYPH           = 10,
    FT_COLR_PAINTFORMAT_COLR_GLYPH      = 11,
    FT_COLR_PAINTFORMAT_TRANSFORM       = 12,
    FT_COLR_PAINTFORMAT_TRANSLATE       = 14,
    FT_COLR_PAINTFORMAT_SCALE           = 16,
    FT_COLR_PAINTFORMAT_ROTATE          = 24,
    FT_COLR_PAINTFORMAT_SKEW            = 28,
    FT_COLR_PAINTFORMAT_COMPOSITE       = 32,
    FT_COLR_PAINT_FORMAT_MAX            = 33,
    FT_COLR_PAINTFORMAT_UNSUPPORTED     = 255

  } FT_PaintFormat;


  /**************************************************************************
   *
   * @struct:
   *   FT_ColorStopIterator
   *
   * @description:
   *   This iterator object is needed for @FT_Get_Colorline_Stops.  It keeps
   *   state while iterating over the stops of an @FT_ColorLine, representing
   *   the `ColorLine` struct of the v1 extensions to 'COLR', see
   *   'https://github.com/googlefonts/colr-gradients-spec'.  Do not manually
   *   modify fields of this iterator.
   *
   * @fields:
   *   num_color_stops ::
   *     The number of color stops for the requested glyph index.  Set by
   *     @FT_Get_Paint.
   *
   *   current_color_stop ::
   *     The current color stop.  Set by @FT_Get_Colorline_Stops.
   *
   *   p ::
   *     An opaque pointer into 'COLR' table data.  Set by @FT_Get_Paint.
   *     Updated by @FT_Get_Colorline_Stops.
   *
   *   read_variable ::
   *     A boolean keeping track of whether variable color lines are to be
   *     read.  Set by @FT_Get_Paint.
   *
   * @since:
   *   2.13
   */
  typedef struct  FT_ColorStopIterator_
  {
    FT_UInt  num_color_stops;
    FT_UInt  current_color_stop;

    FT_Byte*  p;

    FT_Bool  read_variable;

  } FT_ColorStopIterator;


  /**************************************************************************
   *
   * @struct:
   *   FT_ColorIndex
   *
   * @description:
   *   A structure representing a `ColorIndex` value of the 'COLR' v1
   *   extensions, see 'https://github.com/googlefonts/colr-gradients-spec'.
   *
   * @fields:
   *   palette_index ::
   *     The palette index into a 'CPAL' palette.
   *
   *   alpha ::
   *     Alpha transparency value multiplied with the value from 'CPAL'.
   *
   * @since:
   *   2.13
   */
  typedef struct  FT_ColorIndex_
  {
    FT_UInt16   palette_index;
    FT_F2Dot14  alpha;

  } FT_ColorIndex;


  /**************************************************************************
   *
   * @struct:
   *   FT_ColorStop
   *
   * @description:
   *   A structure representing a `ColorStop` value of the 'COLR' v1
   *   extensions, see 'https://github.com/googlefonts/colr-gradients-spec'.
   *
   * @fields:
   *   stop_offset ::
   *     The stop offset along the gradient, expressed as a 16.16 fixed-point
   *     coordinate.
   *
   *   color ::
   *     The color information for this stop, see @FT_ColorIndex.
   *
   * @since:
   *   2.13
   */
  typedef struct  FT_ColorStop_
  {
    FT_Fixed       stop_offset;
    FT_ColorIndex  color;

  } FT_ColorStop;


  /**************************************************************************
   *
   * @enum:
   *   FT_PaintExtend
   *
   * @description:
   *   An enumeration representing the 'Extend' mode of the 'COLR' v1
   *   extensions, see 'https://github.com/googlefonts/colr-gradients-spec'.
   *   It describes how the gradient fill continues at the other boundaries.
   *
   * @since:
   *   2.13
   */
  typedef enum  FT_PaintExtend_
  {
    FT_COLR_PAINT_EXTEND_PAD     = 0,
    FT_COLR_PAINT_EXTEND_REPEAT  = 1,
    FT_COLR_PAINT_EXTEND_REFLECT = 2

  } FT_PaintExtend;


  /**************************************************************************
   *
   * @struct:
   *   FT_ColorLine
   *
   * @description:
   *   A structure representing a `ColorLine` value of the 'COLR' v1
   *   extensions, see 'https://github.com/googlefonts/colr-gradients-spec'.
   *   It describes a list of color stops along the defined gradient.
   *
   * @fields:
   *   extend ::
   *     The extend mode at the outer boundaries, see @FT_PaintExtend.
   *
   *   color_stop_iterator ::
   *     The @FT_ColorStopIterator used to enumerate and retrieve the
   *     actual @FT_ColorStop's.
   *
   * @since:
   *   2.13
   */
  typedef struct  FT_ColorLine_
  {
    FT_PaintExtend        extend;
    FT_ColorStopIterator  color_stop_iterator;

  } FT_ColorLine;


  /**************************************************************************
   *
   * @struct:
   *   FT_Affine23
   *
   * @description:
   *   A structure used to store a 2x3 matrix.  Coefficients are in
   *   16.16 fixed-point format.  The computation performed is
   *
   *   ```
   *     x' = x*xx + y*xy + dx
   *     y' = x*yx + y*yy + dy
   *   ```
   *
   * @fields:
   *   xx ::
   *     Matrix coefficient.
   *
   *   xy ::
   *     Matrix coefficient.
   *
   *   dx ::
   *     x translation.
   *
   *   yx ::
   *     Matrix coefficient.
   *
   *   yy ::
   *     Matrix coefficient.
   *
   *   dy ::
   *     y translation.
   *
   * @since:
   *   2.13
   */
  typedef struct  FT_Affine_23_
  {
    FT_Fixed  xx, xy, dx;
    FT_Fixed  yx, yy, dy;

  } FT_Affine23;


  /**************************************************************************
   *
   * @enum:
   *   FT_Composite_Mode
   *
   * @description:
   *   An enumeration listing the 'COLR' v1 composite modes used in
   *   @FT_PaintComposite.  For more details on each paint mode, see
   *   'https://www.w3.org/TR/compositing-1/#porterduffcompositingoperators'.
   *
   * @since:
   *   2.13
   */
  typedef enum  FT_Composite_Mode_
  {
    FT_COLR_COMPOSITE_CLEAR          = 0,
    FT_COLR_COMPOSITE_SRC            = 1,
    FT_COLR_COMPOSITE_DEST           = 2,
    FT_COLR_COMPOSITE_SRC_OVER       = 3,
    FT_COLR_COMPOSITE_DEST_OVER      = 4,
    FT_COLR_COMPOSITE_SRC_IN         = 5,
    FT_COLR_COMPOSITE_DEST_IN        = 6,
    FT_COLR_COMPOSITE_SRC_OUT        = 7,
    FT_COLR_COMPOSITE_DEST_OUT       = 8,
    FT_COLR_COMPOSITE_SRC_ATOP       = 9,
    FT_COLR_COMPOSITE_DEST_ATOP      = 10,
    FT_COLR_COMPOSITE_XOR            = 11,
    FT_COLR_COMPOSITE_PLUS           = 12,
    FT_COLR_COMPOSITE_SCREEN         = 13,
    FT_COLR_COMPOSITE_OVERLAY        = 14,
    FT_COLR_COMPOSITE_DARKEN         = 15,
    FT_COLR_COMPOSITE_LIGHTEN        = 16,
    FT_COLR_COMPOSITE_COLOR_DODGE    = 17,
    FT_COLR_COMPOSITE_COLOR_BURN     = 18,
    FT_COLR_COMPOSITE_HARD_LIGHT     = 19,
    FT_COLR_COMPOSITE_SOFT_LIGHT     = 20,
    FT_COLR_COMPOSITE_DIFFERENCE     = 21,
    FT_COLR_COMPOSITE_EXCLUSION      = 22,
    FT_COLR_COMPOSITE_MULTIPLY       = 23,
    FT_COLR_COMPOSITE_HSL_HUE        = 24,
    FT_COLR_COMPOSITE_HSL_SATURATION = 25,
    FT_COLR_COMPOSITE_HSL_COLOR      = 26,
    FT_COLR_COMPOSITE_HSL_LUMINOSITY = 27,
    FT_COLR_COMPOSITE_MAX            = 28

  } FT_Composite_Mode;


  /**************************************************************************
   *
   * @struct:
   *   FT_OpaquePaint
   *
   * @description:
   *   A structure representing an offset to a `Paint` value stored in any
   *   of the paint tables of a 'COLR' v1 font.  Compare Offset<24> there.
   *   When 'COLR' v1 paint tables represented by FreeType objects such as
   *   @FT_PaintColrLayers, @FT_PaintComposite, or @FT_PaintTransform
   *   reference downstream nested paint tables, we do not immediately
   *   retrieve them but encapsulate their location in this type.  Use
   *   @FT_Get_Paint to retrieve the actual @FT_COLR_Paint object that
   *   describes the details of the respective paint table.
   *
   * @fields:
   *   p ::
   *     An internal offset to a Paint table, needs to be set to NULL before
   *     passing this struct as an argument to @FT_Get_Paint.
   *
   *   insert_root_transform ::
   *     An internal boolean to track whether an initial root transform is
   *     to be provided.  Do not set this value.
   *
   * @since:
   *   2.13
   */
  typedef struct  FT_Opaque_Paint_
  {
    FT_Byte*  p;
    FT_Bool   insert_root_transform;
  } FT_OpaquePaint;


  /**************************************************************************
   *
   * @struct:
   *   FT_PaintColrLayers
   *
   * @description:
   *   A structure representing a `PaintColrLayers` table of a 'COLR' v1
   *   font.  This table describes a set of layers that are to be composited
   *   with composite mode `FT_COLR_COMPOSITE_SRC_OVER`.  The return value
   *   of this function is an @FT_LayerIterator initialized so that it can
   *   be used with @FT_Get_Paint_Layers to retrieve the @FT_OpaquePaint
   *   objects as references to each layer.
   *
   * @fields:
   *   layer_iterator ::
   *     The layer iterator that describes the layers of this paint.
   *
   * @since:
   *   2.13
   */
  typedef struct  FT_PaintColrLayers_
  {
    FT_LayerIterator  layer_iterator;

  } FT_PaintColrLayers;


  /**************************************************************************
   *
   * @struct:
   *   FT_PaintSolid
   *
   * @description:
   *   A structure representing a `PaintSolid` value of the 'COLR' v1
   *   extensions, see 'https://github.com/googlefonts/colr-gradients-spec'.
   *   Using a `PaintSolid` value means that the glyph layer filled with
   *   this paint is solid-colored and does not contain a gradient.
   *
   * @fields:
   *   color ::
   *     The color information for this solid paint, see @FT_ColorIndex.
   *
   * @since:
   *   2.13
   */
  typedef struct  FT_PaintSolid_
  {
    FT_ColorIndex  color;

  } FT_PaintSolid;


  /**************************************************************************
   *
   * @struct:
   *   FT_PaintLinearGradient
   *
   * @description:
   *   A structure representing a `PaintLinearGradient` value of the 'COLR'
   *   v1 extensions, see
   *   'https://github.com/googlefonts/colr-gradients-spec'.  The glyph
   *   layer filled with this paint is drawn filled with a linear gradient.
   *
   * @fields:
   *   colorline ::
   *     The @FT_ColorLine information for this paint, i.e., the list of
   *     color stops along the gradient.
   *
   *   p0 ::
   *     The starting point of the gradient definition in font units
   *     represented as a 16.16 fixed-point `FT_Vector`.
   *
   *   p1 ::
   *     The end point of the gradient definition in font units
   *     represented as a 16.16 fixed-point `FT_Vector`.
   *
   *   p2 ::
   *     Optional point~p2 to rotate the gradient in font units
   *     represented as a 16.16 fixed-point `FT_Vector`.
   *     Otherwise equal to~p0.
   *
   * @since:
   *   2.13
   */
  typedef struct  FT_PaintLinearGradient_
  {
    FT_ColorLine  colorline;

    /* TODO: Potentially expose those as x0, y0 etc. */
    FT_Vector  p0;
    FT_Vector  p1;
    FT_Vector  p2;

  } FT_PaintLinearGradient;


  /**************************************************************************
   *
   * @struct:
   *   FT_PaintRadialGradient
   *
   * @description:
   *   A structure representing a `PaintRadialGradient` value of the 'COLR'
   *   v1 extensions, see
   *   'https://github.com/googlefonts/colr-gradients-spec'.  The glyph
   *   layer filled with this paint is drawn filled with a radial gradient.
   *
   * @fields:
   *   colorline ::
   *     The @FT_ColorLine information for this paint, i.e., the list of
   *     color stops along the gradient.
   *
   *   c0 ::
   *     The center of the starting point of the radial gradient in font
   *     units represented as a 16.16 fixed-point `FT_Vector`.
   *
   *   r0 ::
   *     The radius of the starting circle of the radial gradient in font
   *     units represented as a 16.16 fixed-point value.
   *
   *   c1 ::
   *     The center of the end point of the radial gradient in font units
   *     represented as a 16.16 fixed-point `FT_Vector`.
   *
   *   r1 ::
   *     The radius of the end circle of the radial gradient in font
   *     units represented as a 16.16 fixed-point value.
   *
   * @since:
   *   2.13
   */
  typedef struct  FT_PaintRadialGradient_
  {
    FT_ColorLine  colorline;

    FT_Vector  c0;
    FT_Pos     r0;
    FT_Vector  c1;
    FT_Pos     r1;

  } FT_PaintRadialGradient;


  /**************************************************************************
   *
   * @struct:
   *   FT_PaintSweepGradient
   *
   * @description:
   *   A structure representing a `PaintSweepGradient` value of the 'COLR'
   *   v1 extensions, see
   *   'https://github.com/googlefonts/colr-gradients-spec'.  The glyph
   *   layer filled with this paint is drawn filled with a sweep gradient
   *   from `start_angle` to `end_angle`.
   *
   * @fields:
   *   colorline ::
   *     The @FT_ColorLine information for this paint, i.e., the list of
   *     color stops along the gradient.
   *
   *   center ::
   *     The center of the sweep gradient in font units represented as a
   *     vector of 16.16 fixed-point values.
   *
   *   start_angle ::
   *     The start angle of the sweep gradient in 16.16 fixed-point
   *     format specifying degrees divided by 180.0 (as in the
   *     spec).  Multiply by 180.0f to receive degrees value.  Values are
   *     given counter-clockwise, starting from the (positive) y~axis.
   *
   *   end_angle ::
   *     The end angle of the sweep gradient in 16.16 fixed-point
   *     format specifying degrees divided by 180.0 (as in the
   *     spec).  Multiply by 180.0f to receive degrees value.  Values are
   *     given counter-clockwise, starting from the (positive) y~axis.
   *
   * @since:
   *   2.13
   */
  typedef struct  FT_PaintSweepGradient_
  {
    FT_ColorLine  colorline;

    FT_Vector  center;
    FT_Fixed   start_angle;
    FT_Fixed   end_angle;

  } FT_PaintSweepGradient;


  /**************************************************************************
   *
   * @struct:
   *   FT_PaintGlyph
   *
   * @description:
   *   A structure representing a 'COLR' v1 `PaintGlyph` paint table.
   *
   * @fields:
   *   paint ::
   *     An opaque paint object pointing to a `Paint` table that serves as
   *     the fill for the glyph ID.
   *
   *   glyphID ::
   *     The glyph ID from the 'glyf' table, which serves as the contour
   *     information that is filled with paint.
   *
   * @since:
   *   2.13
   */
  typedef struct  FT_PaintGlyph_
  {
    FT_OpaquePaint  paint;
    FT_UInt         glyphID;

  } FT_PaintGlyph;


  /**************************************************************************
   *
   * @struct:
   *   FT_PaintColrGlyph
   *
   * @description:
   *   A structure representing a 'COLR' v1 `PaintColorGlyph` paint table.
   *
   * @fields:
   *   glyphID ::
   *     The glyph ID from the `BaseGlyphV1List` table that is drawn for
   *     this paint.
   *
   * @since:
   *   2.13
   */
  typedef struct  FT_PaintColrGlyph_
  {
    FT_UInt  glyphID;

  } FT_PaintColrGlyph;


  /**************************************************************************
   *
   * @struct:
   *   FT_PaintTransform
   *
   * @description:
   *   A structure representing a 'COLR' v1 `PaintTransform` paint table.
   *
   * @fields:
   *   paint ::
   *     An opaque paint that is subject to being transformed.
   *
   *   affine ::
   *     A 2x3 transformation matrix in @FT_Affine23 format containing
   *     16.16 fixed-point values.
   *
   * @since:
   *   2.13
   */
  typedef struct  FT_PaintTransform_
  {
    FT_OpaquePaint  paint;
    FT_Affine23     affine;

  } FT_PaintTransform;


  /**************************************************************************
   *
   * @struct:
   *   FT_PaintTranslate
   *
   * @description:
   *   A structure representing a 'COLR' v1 `PaintTranslate` paint table.
   *   Used for translating downstream paints by a given x and y~delta.
   *
   * @fields:
   *   paint ::
   *     An @FT_OpaquePaint object referencing the paint that is to be
   *     rotated.
   *
   *   dx ::
   *     Translation in x~direction in font units represented as a
   *     16.16 fixed-point value.
   *
   *   dy ::
   *     Translation in y~direction in font units represented as a
   *     16.16 fixed-point value.
   *
   * @since:
   *   2.13
   */
  typedef struct  FT_PaintTranslate_
  {
    FT_OpaquePaint  paint;

    FT_Fixed  dx;
    FT_Fixed  dy;

  } FT_PaintTranslate;


  /**************************************************************************
   *
   * @struct:
   *   FT_PaintScale
   *
   * @description:
   *   A structure representing all of the 'COLR' v1 'PaintScale*' paint
   *   tables.  Used for scaling downstream paints by a given x and y~scale,
   *   with a given center.  This structure is used for all 'PaintScale*'
   *   types that are part of specification; fields of this structure are
   *   filled accordingly.  If there is a center, the center values are set,
   *   otherwise they are set to the zero coordinate.  If the source font
   *   file has 'PaintScaleUniform*' set, the scale values are set
   *   accordingly to the same value.
   *
   * @fields:
   *   paint ::
   *     An @FT_OpaquePaint object referencing the paint that is to be
   *     scaled.
   *
   *   scale_x ::
   *     Scale factor in x~direction represented as a
   *     16.16 fixed-point value.
   *
   *   scale_y ::
   *     Scale factor in y~direction represented as a
   *     16.16 fixed-point value.
   *
   *   center_x ::
   *     x~coordinate of center point to scale from represented as a
   *     16.16 fixed-point value.
   *
   *   center_y ::
   *     y~coordinate of center point to scale from represented as a
   *     16.16 fixed-point value.
   *
   * @since:
   *   2.13
   */
  typedef struct  FT_PaintScale_
  {
    FT_OpaquePaint  paint;

    FT_Fixed  scale_x;
    FT_Fixed  scale_y;

    FT_Fixed  center_x;
    FT_Fixed  center_y;

  } FT_PaintScale;


  /**************************************************************************
   *
   * @struct:
   *   FT_PaintRotate
   *
   * @description:
   *   A structure representing a 'COLR' v1 `PaintRotate` paint table.  Used
   *   for rotating downstream paints with a given center and angle.
   *
   * @fields:
   *   paint ::
   *     An @FT_OpaquePaint object referencing the paint that is to be
   *     rotated.
   *
   *   angle ::
   *     The rotation angle that is to be applied in degrees divided by
   *     180.0 (as in the spec) represented as a 16.16 fixed-point
   *     value.  Multiply by 180.0f to receive degrees value.
   *
   *   center_x ::
   *     The x~coordinate of the pivot point of the rotation in font
   *     units represented as a 16.16 fixed-point value.
   *
   *   center_y ::
   *     The y~coordinate of the pivot point of the rotation in font
   *     units represented as a 16.16 fixed-point value.
   *
   * @since:
   *   2.13
   */

  typedef struct  FT_PaintRotate_
  {
    FT_OpaquePaint  paint;

    FT_Fixed  angle;

    FT_Fixed  center_x;
    FT_Fixed  center_y;

  } FT_PaintRotate;


  /**************************************************************************
   *
   * @struct:
   *   FT_PaintSkew
   *
   * @description:
   *   A structure representing a 'COLR' v1 `PaintSkew` paint table.  Used
   *   for skewing or shearing downstream paints by a given center and
   *   angle.
   *
   * @fields:
   *   paint ::
   *     An @FT_OpaquePaint object referencing the paint that is to be
   *     skewed.
   *
   *   x_skew_angle ::
   *     The skewing angle in x~direction in degrees divided by 180.0
   *     (as in the spec) represented as a 16.16 fixed-point
   *     value. Multiply by 180.0f to receive degrees.
   *
   *   y_skew_angle ::
   *     The skewing angle in y~direction in degrees divided by 180.0
   *     (as in the spec) represented as a 16.16 fixed-point
   *     value.  Multiply by 180.0f to receive degrees.
   *
   *   center_x ::
   *     The x~coordinate of the pivot point of the skew in font units
   *     represented as a 16.16 fixed-point value.
   *
   *   center_y ::
   *     The y~coordinate of the pivot point of the skew in font units
   *     represented as a 16.16 fixed-point value.
   *
   * @since:
   *   2.13
   */
  typedef struct  FT_PaintSkew_
  {
    FT_OpaquePaint  paint;

    FT_Fixed  x_skew_angle;
    FT_Fixed  y_skew_angle;

    FT_Fixed  center_x;
    FT_Fixed  center_y;

  } FT_PaintSkew;


  /**************************************************************************
   *
   * @struct:
   *   FT_PaintComposite
   *
   * @description:
   *   A structure representing a 'COLR' v1 `PaintComposite` paint table.
   *   Used for compositing two paints in a 'COLR' v1 directed acyclic graph.
   *
   * @fields:
   *   source_paint ::
   *     An @FT_OpaquePaint object referencing the source that is to be
   *     composited.
   *
   *   composite_mode ::
   *     An @FT_Composite_Mode enum value determining the composition
   *     operation.
   *
   *   backdrop_paint ::
   *     An @FT_OpaquePaint object referencing the backdrop paint that
   *     `source_paint` is composited onto.
   *
   * @since:
   *   2.13
   */
  typedef struct  FT_PaintComposite_
  {
    FT_OpaquePaint     source_paint;
    FT_Composite_Mode  composite_mode;
    FT_OpaquePaint     backdrop_paint;

  } FT_PaintComposite;


  /**************************************************************************
   *
   * @union:
   *   FT_COLR_Paint
   *
   * @description:
   *   A union object representing format and details of a paint table of a
   *   'COLR' v1 font, see
   *   'https://github.com/googlefonts/colr-gradients-spec'.  Use
   *   @FT_Get_Paint to retrieve a @FT_COLR_Paint for an @FT_OpaquePaint
   *   object.
   *
   * @fields:
   *   format ::
   *     The gradient format for this Paint structure.
   *
   *   u ::
   *     Union of all paint table types:
   *
   *       * @FT_PaintColrLayers
   *       * @FT_PaintGlyph
   *       * @FT_PaintSolid
   *       * @FT_PaintLinearGradient
   *       * @FT_PaintRadialGradient
   *       * @FT_PaintSweepGradient
   *       * @FT_PaintTransform
   *       * @FT_PaintTranslate
   *       * @FT_PaintRotate
   *       * @FT_PaintSkew
   *       * @FT_PaintComposite
   *       * @FT_PaintColrGlyph
   *
   * @since:
   *   2.13
   */
  typedef struct  FT_COLR_Paint_
  {
    FT_PaintFormat format;

    union
    {
      FT_PaintColrLayers      colr_layers;
      FT_PaintGlyph           glyph;
      FT_PaintSolid           solid;
      FT_PaintLinearGradient  linear_gradient;
      FT_PaintRadialGradient  radial_gradient;
      FT_PaintSweepGradient   sweep_gradient;
      FT_PaintTransform       transform;
      FT_PaintTranslate       translate;
      FT_PaintScale           scale;
      FT_PaintRotate          rotate;
      FT_PaintSkew            skew;
      FT_PaintComposite       composite;
      FT_PaintColrGlyph       colr_glyph;

    } u;

  } FT_COLR_Paint;


  /**************************************************************************
   *
   * @enum:
   *   FT_Color_Root_Transform
   *
   * @description:
   *   An enumeration to specify whether @FT_Get_Color_Glyph_Paint is to
   *   return a root transform to configure the client's graphics context
   *   matrix.
   *
   * @values:
   *   FT_COLOR_INCLUDE_ROOT_TRANSFORM ::
   *     Do include the root transform as the initial @FT_COLR_Paint object.
   *
   *   FT_COLOR_NO_ROOT_TRANSFORM ::
   *     Do not output an initial root transform.
   *
   * @since:
   *   2.13
   */
  typedef enum  FT_Color_Root_Transform_
  {
    FT_COLOR_INCLUDE_ROOT_TRANSFORM,
    FT_COLOR_NO_ROOT_TRANSFORM,

    FT_COLOR_ROOT_TRANSFORM_MAX

  } FT_Color_Root_Transform;


  /**************************************************************************
   *
   * @struct:
   *   FT_ClipBox
   *
   * @description:
   *   A structure representing a 'COLR' v1 'ClipBox' table.  'COLR' v1
   *   glyphs may optionally define a clip box for aiding allocation or
   *   defining a maximum drawable region.  Use @FT_Get_Color_Glyph_ClipBox
   *   to retrieve it.
   *
   * @fields:
   *   bottom_left ::
   *     The bottom left corner of the clip box as an @FT_Vector with
   *     fixed-point coordinates in 26.6 format.
   *
   *   top_left ::
   *     The top left corner of the clip box as an @FT_Vector with
   *     fixed-point coordinates in 26.6 format.
   *
   *   top_right ::
   *     The top right corner of the clip box as an @FT_Vector with
   *     fixed-point coordinates in 26.6 format.
   *
   *   bottom_right ::
   *     The bottom right corner of the clip box as an @FT_Vector with
   *     fixed-point coordinates in 26.6 format.
   *
   * @since:
   *   2.13
   */
  typedef struct  FT_ClipBox_
  {
    FT_Vector  bottom_left;
    FT_Vector  top_left;
    FT_Vector  top_right;
    FT_Vector  bottom_right;

  } FT_ClipBox;


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Color_Glyph_Paint
   *
   * @description:
   *   This is the starting point and interface to color gradient
   *   information in a 'COLR' v1 table in OpenType fonts to recursively
   *   retrieve the paint tables for the directed acyclic graph of a colored
   *   glyph, given a glyph ID.
   *
   *     https://github.com/googlefonts/colr-gradients-spec
   *
   *   In a 'COLR' v1 font, each color glyph defines a directed acyclic
   *   graph of nested paint tables, such as `PaintGlyph`, `PaintSolid`,
   *   `PaintLinearGradient`, `PaintRadialGradient`, and so on.  Using this
   *   function and specifying a glyph ID, one retrieves the root paint
   *   table for this glyph ID.
   *
   *   This function allows control whether an initial root transform is
   *   returned to configure scaling, transform, and translation correctly
   *   on the client's graphics context.  The initial root transform is
   *   computed and returned according to the values configured for @FT_Size
   *   and @FT_Set_Transform on the @FT_Face object, see below for details
   *   of the `root_transform` parameter.  This has implications for a
   *   client 'COLR' v1 implementation: When this function returns an
   *   initially computed root transform, at the time of executing the
   *   @FT_PaintGlyph operation, the contours should be retrieved using
   *   @FT_Load_Glyph at unscaled, untransformed size.  This is because the
   *   root transform applied to the graphics context will take care of
   *   correct scaling.
   *
   *   Alternatively, to allow hinting of contours, at the time of executing
   *   @FT_Load_Glyph, the current graphics context transformation matrix
   *   can be decomposed into a scaling matrix and a remainder, and
   *   @FT_Load_Glyph can be used to retrieve the contours at scaled size.
   *   Care must then be taken to blit or clip to the graphics context with
   *   taking this remainder transformation into account.
   *
   * @input:
   *   face ::
   *     A handle to the parent face object.
   *
   *   base_glyph ::
   *     The glyph index for which to retrieve the root paint table.
   *
   *   root_transform ::
   *     Specifies whether an initially computed root is returned by the
   *     @FT_PaintTransform operation to account for the activated size
   *     (see @FT_Activate_Size) and the configured transform and translate
   *     (see @FT_Set_Transform).
   *
   *     This root transform is returned before nodes of the glyph graph of
   *     the font are returned.  Subsequent @FT_COLR_Paint structures
   *     contain unscaled and untransformed values.  The inserted root
   *     transform enables the client application to apply an initial
   *     transform to its graphics context.  When executing subsequent
   *     FT_COLR_Paint operations, values from @FT_COLR_Paint operations
   *     will ultimately be correctly scaled because of the root transform
   *     applied to the graphics context.  Use
   *     @FT_COLOR_INCLUDE_ROOT_TRANSFORM to include the root transform, use
   *     @FT_COLOR_NO_ROOT_TRANSFORM to not include it.  The latter may be
   *     useful when traversing the 'COLR' v1 glyph graph and reaching a
   *     @FT_PaintColrGlyph.  When recursing into @FT_PaintColrGlyph and
   *     painting that inline, no additional root transform is needed as it
   *     has already been applied to the graphics context at the beginning
   *     of drawing this glyph.
   *
   * @output:
   *   paint ::
   *     The @FT_OpaquePaint object that references the actual paint table.
   *
   *     The respective actual @FT_COLR_Paint object is retrieved via
   *     @FT_Get_Paint.
   *
   * @return:
   *   Value~1 if everything is OK.  If no color glyph is found, or the root
   *   paint could not be retrieved, value~0 gets returned.  In case of an
   *   error, value~0 is returned also.
   *
   * @since:
   *   2.13
   */
   FT_Bool 
  FT_Get_Color_Glyph_Paint( FT_Face                  face,
                            FT_UInt                  base_glyph,
                            FT_Color_Root_Transform  root_transform,
                            FT_OpaquePaint*          paint );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Color_Glyph_ClipBox
   *
   * @description:
   *   Search for a 'COLR' v1 clip box for the specified `base_glyph` and
   *   fill the `clip_box` parameter with the 'COLR' v1 'ClipBox' information
   *   if one is found.
   *
   * @input:
   *   face ::
   *     A handle to the parent face object.
   *
   *   base_glyph ::
   *     The glyph index for which to retrieve the clip box.
   *
   * @output:
   *   clip_box ::
   *     The clip box for the requested `base_glyph` if one is found.  The
   *     clip box is computed taking scale and transformations configured on
   *     the @FT_Face into account.  @FT_ClipBox contains @FT_Vector values
   *     in 26.6 format.
   *
   * @return:
   *   Value~1 if a clip box is found.  If no clip box is found or an error
   *   occured, value~0 is returned.
   *
   * @note:
   *   To retrieve the clip box in font units, reset scale to units-per-em
   *   and remove transforms configured using @FT_Set_Transform.
   *
   * @since:
   *   2.13
   */
   FT_Bool 
  FT_Get_Color_Glyph_ClipBox( FT_Face      face,
                              FT_UInt      base_glyph,
                              FT_ClipBox*  clip_box );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Paint_Layers
   *
   * @description:
   *   Access the layers of a `PaintColrLayers` table.
   *
   *   If the root paint of a color glyph, or a nested paint of a 'COLR'
   *   glyph is a `PaintColrLayers` table, this function retrieves the
   *   layers of the `PaintColrLayers` table.
   *
   *   The @FT_PaintColrLayers object contains an @FT_LayerIterator, which
   *   is used here to iterate over the layers.  Each layer is returned as
   *   an @FT_OpaquePaint object, which then can be used with @FT_Get_Paint
   *   to retrieve the actual paint object.
   *
   * @input:
   *   face ::
   *     A handle to the parent face object.
   *
   * @inout:
   *   iterator ::
   *     The @FT_LayerIterator from an @FT_PaintColrLayers object, for which
   *     the layers are to be retrieved.  The internal state of the iterator
   *     is incremented after one call to this function for retrieving one
   *     layer.
   *
   * @output:
   *   paint ::
   *     The @FT_OpaquePaint object that references the actual paint table.
   *     The respective actual @FT_COLR_Paint object is retrieved via
   *     @FT_Get_Paint.
   *
   * @return:
   *   Value~1 if everything is OK.  Value~0 gets returned when the paint
   *   object can not be retrieved or any other error occurs.
   *
   * @since:
   *   2.13
   */
   FT_Bool 
  FT_Get_Paint_Layers( FT_Face            face,
                       FT_LayerIterator*  iterator,
                       FT_OpaquePaint*    paint );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Colorline_Stops
   *
   * @description:
   *   This is an interface to color gradient information in a 'COLR' v1
   *   table in OpenType fonts to iteratively retrieve the gradient and
   *   solid fill information for colored glyph layers for a specified glyph
   *   ID.
   *
   *     https://github.com/googlefonts/colr-gradients-spec
   *
   * @input:
   *   face ::
   *     A handle to the parent face object.
   *
   * @inout:
   *   iterator ::
   *     The retrieved @FT_ColorStopIterator, configured on an @FT_ColorLine,
   *     which in turn got retrieved via paint information in
   *     @FT_PaintLinearGradient or @FT_PaintRadialGradient.
   *
   * @output:
   *   color_stop ::
   *     Color index and alpha value for the retrieved color stop.
   *
   * @return:
   *   Value~1 if everything is OK.  If there are no more color stops,
   *   value~0 gets returned.  In case of an error, value~0 is returned
   *   also.
   *
   * @since:
   *   2.13
   */
   FT_Bool 
  FT_Get_Colorline_Stops( FT_Face                face,
                          FT_ColorStop*          color_stop,
                          FT_ColorStopIterator*  iterator );


  /**************************************************************************
   *
   * @function:
   *  FT_Get_Paint
   *
   * @description:
   *   Access the details of a paint using an @FT_OpaquePaint opaque paint
   *   object, which internally stores the offset to the respective `Paint`
   *   object in the 'COLR' table.
   *
   * @input:
   *   face ::
   *     A handle to the parent face object.
   *
   *   opaque_paint ::
   *     The opaque paint object for which the underlying @FT_COLR_Paint
   *     data is to be retrieved.
   *
   * @output:
   *   paint ::
   *     The specific @FT_COLR_Paint object containing information coming
   *     from one of the font's `Paint*` tables.
   *
   * @return:
   *   Value~1 if everything is OK.  Value~0 if no details can be found for
   *   this paint or any other error occured.
   *
   * @since:
   *   2.13
   */
   FT_Bool 
  FT_Get_Paint( FT_Face         face,
                FT_OpaquePaint  opaque_paint,
                FT_COLR_Paint*  paint );

  /* */




#endif /* FTCOLOR_H_ */


/* END */
//
// ===========================  ftimage.h  ===========================
//
/****************************************************************************
 *
 * ftimage.h
 *
 *   FreeType glyph image formats and default raster interface
 *   (specification).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */

  /**************************************************************************
   *
   * Note: A 'raster' is simply a scan-line converter, used to render
   *       `FT_Outline`s into `FT_Bitmap`s.
   *
   */


#ifndef FTIMAGE_H_
#define FTIMAGE_H_





  /**************************************************************************
   *
   * @section:
   *   basic_types
   *
   */


  /**************************************************************************
   *
   * @type:
   *   FT_Pos
   *
   * @description:
   *   The type FT_Pos is used to store vectorial coordinates.  Depending on
   *   the context, these can represent distances in integer font units, or
   *   16.16, or 26.6 fixed-point pixel coordinates.
   */
  typedef signed long  FT_Pos;


  /**************************************************************************
   *
   * @struct:
   *   FT_Vector
   *
   * @description:
   *   A simple structure used to store a 2D vector; coordinates are of the
   *   FT_Pos type.
   *
   * @fields:
   *   x ::
   *     The horizontal coordinate.
   *   y ::
   *     The vertical coordinate.
   */
  typedef struct  FT_Vector_
  {
    FT_Pos  x;
    FT_Pos  y;

  } FT_Vector;


  /**************************************************************************
   *
   * @struct:
   *   FT_BBox
   *
   * @description:
   *   A structure used to hold an outline's bounding box, i.e., the
   *   coordinates of its extrema in the horizontal and vertical directions.
   *
   * @fields:
   *   xMin ::
   *     The horizontal minimum (left-most).
   *
   *   yMin ::
   *     The vertical minimum (bottom-most).
   *
   *   xMax ::
   *     The horizontal maximum (right-most).
   *
   *   yMax ::
   *     The vertical maximum (top-most).
   *
   * @note:
   *   The bounding box is specified with the coordinates of the lower left
   *   and the upper right corner.  In PostScript, those values are often
   *   called (llx,lly) and (urx,ury), respectively.
   *
   *   If `yMin` is negative, this value gives the glyph's descender.
   *   Otherwise, the glyph doesn't descend below the baseline.  Similarly,
   *   if `ymax` is positive, this value gives the glyph's ascender.
   *
   *   `xMin` gives the horizontal distance from the glyph's origin to the
   *   left edge of the glyph's bounding box.  If `xMin` is negative, the
   *   glyph extends to the left of the origin.
   */
  typedef struct  FT_BBox_
  {
    FT_Pos  xMin, yMin;
    FT_Pos  xMax, yMax;

  } FT_BBox;


  /**************************************************************************
   *
   * @enum:
   *   FT_Pixel_Mode
   *
   * @description:
   *   An enumeration type used to describe the format of pixels in a given
   *   bitmap.  Note that additional formats may be added in the future.
   *
   * @values:
   *   FT_PIXEL_MODE_NONE ::
   *     Value~0 is reserved.
   *
   *   FT_PIXEL_MODE_MONO ::
   *     A monochrome bitmap, using 1~bit per pixel.  Note that pixels are
   *     stored in most-significant order (MSB), which means that the
   *     left-most pixel in a byte has value 128.
   *
   *   FT_PIXEL_MODE_GRAY ::
   *     An 8-bit bitmap, generally used to represent anti-aliased glyph
   *     images.  Each pixel is stored in one byte.  Note that the number of
   *     'gray' levels is stored in the `num_grays` field of the @FT_Bitmap
   *     structure (it generally is 256).
   *
   *   FT_PIXEL_MODE_GRAY2 ::
   *     A 2-bit per pixel bitmap, used to represent embedded anti-aliased
   *     bitmaps in font files according to the OpenType specification.  We
   *     haven't found a single font using this format, however.
   *
   *   FT_PIXEL_MODE_GRAY4 ::
   *     A 4-bit per pixel bitmap, representing embedded anti-aliased bitmaps
   *     in font files according to the OpenType specification.  We haven't
   *     found a single font using this format, however.
   *
   *   FT_PIXEL_MODE_LCD ::
   *     An 8-bit bitmap, representing RGB or BGR decimated glyph images used
   *     for display on LCD displays; the bitmap is three times wider than
   *     the original glyph image.  See also @FT_RENDER_MODE_LCD.
   *
   *   FT_PIXEL_MODE_LCD_V ::
   *     An 8-bit bitmap, representing RGB or BGR decimated glyph images used
   *     for display on rotated LCD displays; the bitmap is three times
   *     taller than the original glyph image.  See also
   *     @FT_RENDER_MODE_LCD_V.
   *
   *   FT_PIXEL_MODE_BGRA ::
   *     [Since 2.5] An image with four 8-bit channels per pixel,
   *     representing a color image (such as emoticons) with alpha channel.
   *     For each pixel, the format is BGRA, which means, the blue channel
   *     comes first in memory.  The color channels are pre-multiplied and in
   *     the sRGB colorspace.  For example, full red at half-translucent
   *     opacity will be represented as '00,00,80,80', not '00,00,FF,80'.
   *     See also @FT_LOAD_COLOR.
   */
  typedef enum  FT_Pixel_Mode_
  {
    FT_PIXEL_MODE_NONE = 0,
    FT_PIXEL_MODE_MONO,
    FT_PIXEL_MODE_GRAY,
    FT_PIXEL_MODE_GRAY2,
    FT_PIXEL_MODE_GRAY4,
    FT_PIXEL_MODE_LCD,
    FT_PIXEL_MODE_LCD_V,
    FT_PIXEL_MODE_BGRA,

    FT_PIXEL_MODE_MAX      /* do not remove */

  } FT_Pixel_Mode;


  /* these constants are deprecated; use the corresponding `FT_Pixel_Mode` */
  /* values instead.                                                       */
#define ft_pixel_mode_none   FT_PIXEL_MODE_NONE
#define ft_pixel_mode_mono   FT_PIXEL_MODE_MONO
#define ft_pixel_mode_grays  FT_PIXEL_MODE_GRAY
#define ft_pixel_mode_pal2   FT_PIXEL_MODE_GRAY2
#define ft_pixel_mode_pal4   FT_PIXEL_MODE_GRAY4

  /* */

  /* For debugging, the @FT_Pixel_Mode enumeration must stay in sync */
  /* with the `pixel_modes` array in file `ftobjs.c`.                */


  /**************************************************************************
   *
   * @struct:
   *   FT_Bitmap
   *
   * @description:
   *   A structure used to describe a bitmap or pixmap to the raster.  Note
   *   that we now manage pixmaps of various depths through the `pixel_mode`
   *   field.
   *
   * @fields:
   *   rows ::
   *     The number of bitmap rows.
   *
   *   width ::
   *     The number of pixels in bitmap row.
   *
   *   pitch ::
   *     The pitch's absolute value is the number of bytes taken by one
   *     bitmap row, including padding.  However, the pitch is positive when
   *     the bitmap has a 'down' flow, and negative when it has an 'up' flow.
   *     In all cases, the pitch is an offset to add to a bitmap pointer in
   *     order to go down one row.
   *
   *     Note that 'padding' means the alignment of a bitmap to a byte
   *     border, and FreeType functions normally align to the smallest
   *     possible integer value.
   *
   *     For the B/W rasterizer, `pitch` is always an even number.
   *
   *     To change the pitch of a bitmap (say, to make it a multiple of 4),
   *     use @FT_Bitmap_Convert.  Alternatively, you might use callback
   *     functions to directly render to the application's surface; see the
   *     file `example2.cpp` in the tutorial for a demonstration.
   *
   *   buffer ::
   *     A typeless pointer to the bitmap buffer.  This value should be
   *     aligned on 32-bit boundaries in most cases.
   *
   *   num_grays ::
   *     This field is only used with @FT_PIXEL_MODE_GRAY; it gives the
   *     number of gray levels used in the bitmap.
   *
   *   pixel_mode ::
   *     The pixel mode, i.e., how pixel bits are stored.  See @FT_Pixel_Mode
   *     for possible values.
   *
   *   palette_mode ::
   *     This field is intended for paletted pixel modes; it indicates how
   *     the palette is stored.  Not used currently.
   *
   *   palette ::
   *     A typeless pointer to the bitmap palette; this field is intended for
   *     paletted pixel modes.  Not used currently.
   *
   * @note:
   *   `width` and `rows` refer to the *physical* size of the bitmap, not the
   *   *logical* one.  For example, if @FT_Pixel_Mode is set to
   *   `FT_PIXEL_MODE_LCD`, the logical width is a just a third of the
   *   physical one.
   */
  typedef struct  FT_Bitmap_
  {
    unsigned int    rows;
    unsigned int    width;
    int             pitch;
    unsigned char*  buffer;
    unsigned short  num_grays;
    unsigned char   pixel_mode;
    unsigned char   palette_mode;
    void*           palette;

  } FT_Bitmap;


  /**************************************************************************
   *
   * @section:
   *   outline_processing
   *
   */


  /**************************************************************************
   *
   * @struct:
   *   FT_Outline
   *
   * @description:
   *   This structure is used to describe an outline to the scan-line
   *   converter.
   *
   * @fields:
   *   n_contours ::
   *     The number of contours in the outline.
   *
   *   n_points ::
   *     The number of points in the outline.
   *
   *   points ::
   *     A pointer to an array of `n_points` @FT_Vector elements, giving the
   *     outline's point coordinates.
   *
   *   tags ::
   *     A pointer to an array of `n_points` chars, giving each outline
   *     point's type.
   *
   *     If bit~0 is unset, the point is 'off' the curve, i.e., a Bezier
   *     control point, while it is 'on' if set.
   *
   *     Bit~1 is meaningful for 'off' points only.  If set, it indicates a
   *     third-order Bezier arc control point; and a second-order control
   *     point if unset.
   *
   *     If bit~2 is set, bits 5-7 contain the drop-out mode (as defined in
   *     the OpenType specification; the value is the same as the argument to
   *     the 'SCANTYPE' instruction).
   *
   *     Bits 3 and~4 are reserved for internal purposes.
   *
   *   contours ::
   *     An array of `n_contours` shorts, giving the end point of each
   *     contour within the outline.  For example, the first contour is
   *     defined by the points '0' to `contours[0]`, the second one is
   *     defined by the points `contours[0]+1` to `contours[1]`, etc.
   *
   *   flags ::
   *     A set of bit flags used to characterize the outline and give hints
   *     to the scan-converter and hinter on how to convert/grid-fit it.  See
   *     @FT_OUTLINE_XXX.
   *
   * @note:
   *   The B/W rasterizer only checks bit~2 in the `tags` array for the first
   *   point of each contour.  The drop-out mode as given with
   *   @FT_OUTLINE_IGNORE_DROPOUTS, @FT_OUTLINE_SMART_DROPOUTS, and
   *   @FT_OUTLINE_INCLUDE_STUBS in `flags` is then overridden.
   */
  typedef struct  FT_Outline_
  {
    short       n_contours;      /* number of contours in glyph        */
    short       n_points;        /* number of points in the glyph      */

    FT_Vector*  points;          /* the outline's points               */
    char*       tags;            /* the points flags                   */
    short*      contours;        /* the contour end points             */

    int         flags;           /* outline masks                      */

  } FT_Outline;

  /* */

  /* Following limits must be consistent with */
  /* FT_Outline.{n_contours,n_points}         */
#define FT_OUTLINE_CONTOURS_MAX  SHRT_MAX
#define FT_OUTLINE_POINTS_MAX    SHRT_MAX


  /**************************************************************************
   *
   * @enum:
   *   FT_OUTLINE_XXX
   *
   * @description:
   *   A list of bit-field constants used for the flags in an outline's
   *   `flags` field.
   *
   * @values:
   *   FT_OUTLINE_NONE ::
   *     Value~0 is reserved.
   *
   *   FT_OUTLINE_OWNER ::
   *     If set, this flag indicates that the outline's field arrays (i.e.,
   *     `points`, `flags`, and `contours`) are 'owned' by the outline
   *     object, and should thus be freed when it is destroyed.
   *
   *   FT_OUTLINE_EVEN_ODD_FILL ::
   *     By default, outlines are filled using the non-zero winding rule.  If
   *     set to 1, the outline will be filled using the even-odd fill rule
   *     (only works with the smooth rasterizer).
   *
   *   FT_OUTLINE_REVERSE_FILL ::
   *     By default, outside contours of an outline are oriented in
   *     clock-wise direction, as defined in the TrueType specification.
   *     This flag is set if the outline uses the opposite direction
   *     (typically for Type~1 fonts).  This flag is ignored by the scan
   *     converter.
   *
   *   FT_OUTLINE_IGNORE_DROPOUTS ::
   *     By default, the scan converter will try to detect drop-outs in an
   *     outline and correct the glyph bitmap to ensure consistent shape
   *     continuity.  If set, this flag hints the scan-line converter to
   *     ignore such cases.  See below for more information.
   *
   *   FT_OUTLINE_SMART_DROPOUTS ::
   *     Select smart dropout control.  If unset, use simple dropout control.
   *     Ignored if @FT_OUTLINE_IGNORE_DROPOUTS is set.  See below for more
   *     information.
   *
   *   FT_OUTLINE_INCLUDE_STUBS ::
   *     If set, turn pixels on for 'stubs', otherwise exclude them.  Ignored
   *     if @FT_OUTLINE_IGNORE_DROPOUTS is set.  See below for more
   *     information.
   *
   *   FT_OUTLINE_OVERLAP ::
   *     [Since 2.10.3] This flag indicates that this outline contains
   *     overlapping contours and the anti-aliased renderer should perform
   *     oversampling to mitigate possible artifacts.  This flag should _not_
   *     be set for well designed glyphs without overlaps because it quadruples
   *     the rendering time.
   *
   *   FT_OUTLINE_HIGH_PRECISION ::
   *     This flag indicates that the scan-line converter should try to
   *     convert this outline to bitmaps with the highest possible quality.
   *     It is typically set for small character sizes.  Note that this is
   *     only a hint that might be completely ignored by a given
   *     scan-converter.
   *
   *   FT_OUTLINE_SINGLE_PASS ::
   *     This flag is set to force a given scan-converter to only use a
   *     single pass over the outline to render a bitmap glyph image.
   *     Normally, it is set for very large character sizes.  It is only a
   *     hint that might be completely ignored by a given scan-converter.
   *
   * @note:
   *   The flags @FT_OUTLINE_IGNORE_DROPOUTS, @FT_OUTLINE_SMART_DROPOUTS, and
   *   @FT_OUTLINE_INCLUDE_STUBS are ignored by the smooth rasterizer.
   *
   *   There exists a second mechanism to pass the drop-out mode to the B/W
   *   rasterizer; see the `tags` field in @FT_Outline.
   *
   *   Please refer to the description of the 'SCANTYPE' instruction in the
   *   [OpenType specification](https://learn.microsoft.com/en-us/typography/opentype/spec/tt_instructions#scantype)
   *   how simple drop-outs, smart drop-outs, and stubs are defined.
   */
#define FT_OUTLINE_NONE             0x0
#define FT_OUTLINE_OWNER            0x1
#define FT_OUTLINE_EVEN_ODD_FILL    0x2
#define FT_OUTLINE_REVERSE_FILL     0x4
#define FT_OUTLINE_IGNORE_DROPOUTS  0x8
#define FT_OUTLINE_SMART_DROPOUTS   0x10
#define FT_OUTLINE_INCLUDE_STUBS    0x20
#define FT_OUTLINE_OVERLAP          0x40

#define FT_OUTLINE_HIGH_PRECISION   0x100
#define FT_OUTLINE_SINGLE_PASS      0x200


  /* these constants are deprecated; use the corresponding */
  /* `FT_OUTLINE_XXX` values instead                       */
#define ft_outline_none             FT_OUTLINE_NONE
#define ft_outline_owner            FT_OUTLINE_OWNER
#define ft_outline_even_odd_fill    FT_OUTLINE_EVEN_ODD_FILL
#define ft_outline_reverse_fill     FT_OUTLINE_REVERSE_FILL
#define ft_outline_ignore_dropouts  FT_OUTLINE_IGNORE_DROPOUTS
#define ft_outline_high_precision   FT_OUTLINE_HIGH_PRECISION
#define ft_outline_single_pass      FT_OUTLINE_SINGLE_PASS

  /* */

#define FT_CURVE_TAG( flag )  ( flag & 0x03 )

  /* see the `tags` field in `FT_Outline` for a description of the values */
#define FT_CURVE_TAG_ON            0x01
#define FT_CURVE_TAG_CONIC         0x00
#define FT_CURVE_TAG_CUBIC         0x02

#define FT_CURVE_TAG_HAS_SCANMODE  0x04

#define FT_CURVE_TAG_TOUCH_X       0x08  /* reserved for TrueType hinter */
#define FT_CURVE_TAG_TOUCH_Y       0x10  /* reserved for TrueType hinter */

#define FT_CURVE_TAG_TOUCH_BOTH    ( FT_CURVE_TAG_TOUCH_X | \
                                     FT_CURVE_TAG_TOUCH_Y )
  /* values 0x20, 0x40, and 0x80 are reserved */


  /* these constants are deprecated; use the corresponding */
  /* `FT_CURVE_TAG_XXX` values instead                     */
#define FT_Curve_Tag_On       FT_CURVE_TAG_ON
#define FT_Curve_Tag_Conic    FT_CURVE_TAG_CONIC
#define FT_Curve_Tag_Cubic    FT_CURVE_TAG_CUBIC
#define FT_Curve_Tag_Touch_X  FT_CURVE_TAG_TOUCH_X
#define FT_Curve_Tag_Touch_Y  FT_CURVE_TAG_TOUCH_Y


  /**************************************************************************
   *
   * @functype:
   *   FT_Outline_MoveToFunc
   *
   * @description:
   *   A function pointer type used to describe the signature of a 'move to'
   *   function during outline walking/decomposition.
   *
   *   A 'move to' is emitted to start a new contour in an outline.
   *
   * @input:
   *   to ::
   *     A pointer to the target point of the 'move to'.
   *
   *   user ::
   *     A typeless pointer, which is passed from the caller of the
   *     decomposition function.
   *
   * @return:
   *   Error code.  0~means success.
   */
  typedef int
  (*FT_Outline_MoveToFunc)( const FT_Vector*  to,
                            void*             user );

#define FT_Outline_MoveTo_Func  FT_Outline_MoveToFunc


  /**************************************************************************
   *
   * @functype:
   *   FT_Outline_LineToFunc
   *
   * @description:
   *   A function pointer type used to describe the signature of a 'line to'
   *   function during outline walking/decomposition.
   *
   *   A 'line to' is emitted to indicate a segment in the outline.
   *
   * @input:
   *   to ::
   *     A pointer to the target point of the 'line to'.
   *
   *   user ::
   *     A typeless pointer, which is passed from the caller of the
   *     decomposition function.
   *
   * @return:
   *   Error code.  0~means success.
   */
  typedef int
  (*FT_Outline_LineToFunc)( const FT_Vector*  to,
                            void*             user );

#define FT_Outline_LineTo_Func  FT_Outline_LineToFunc


  /**************************************************************************
   *
   * @functype:
   *   FT_Outline_ConicToFunc
   *
   * @description:
   *   A function pointer type used to describe the signature of a 'conic to'
   *   function during outline walking or decomposition.
   *
   *   A 'conic to' is emitted to indicate a second-order Bezier arc in the
   *   outline.
   *
   * @input:
   *   control ::
   *     An intermediate control point between the last position and the new
   *     target in `to`.
   *
   *   to ::
   *     A pointer to the target end point of the conic arc.
   *
   *   user ::
   *     A typeless pointer, which is passed from the caller of the
   *     decomposition function.
   *
   * @return:
   *   Error code.  0~means success.
   */
  typedef int
  (*FT_Outline_ConicToFunc)( const FT_Vector*  control,
                             const FT_Vector*  to,
                             void*             user );

#define FT_Outline_ConicTo_Func  FT_Outline_ConicToFunc


  /**************************************************************************
   *
   * @functype:
   *   FT_Outline_CubicToFunc
   *
   * @description:
   *   A function pointer type used to describe the signature of a 'cubic to'
   *   function during outline walking or decomposition.
   *
   *   A 'cubic to' is emitted to indicate a third-order Bezier arc.
   *
   * @input:
   *   control1 ::
   *     A pointer to the first Bezier control point.
   *
   *   control2 ::
   *     A pointer to the second Bezier control point.
   *
   *   to ::
   *     A pointer to the target end point.
   *
   *   user ::
   *     A typeless pointer, which is passed from the caller of the
   *     decomposition function.
   *
   * @return:
   *   Error code.  0~means success.
   */
  typedef int
  (*FT_Outline_CubicToFunc)( const FT_Vector*  control1,
                             const FT_Vector*  control2,
                             const FT_Vector*  to,
                             void*             user );

#define FT_Outline_CubicTo_Func  FT_Outline_CubicToFunc


  /**************************************************************************
   *
   * @struct:
   *   FT_Outline_Funcs
   *
   * @description:
   *   A structure to hold various function pointers used during outline
   *   decomposition in order to emit segments, conic, and cubic Beziers.
   *
   * @fields:
   *   move_to ::
   *     The 'move to' emitter.
   *
   *   line_to ::
   *     The segment emitter.
   *
   *   conic_to ::
   *     The second-order Bezier arc emitter.
   *
   *   cubic_to ::
   *     The third-order Bezier arc emitter.
   *
   *   shift ::
   *     The shift that is applied to coordinates before they are sent to the
   *     emitter.
   *
   *   delta ::
   *     The delta that is applied to coordinates before they are sent to the
   *     emitter, but after the shift.
   *
   * @note:
   *   The point coordinates sent to the emitters are the transformed version
   *   of the original coordinates (this is important for high accuracy
   *   during scan-conversion).  The transformation is simple:
   *
   *   ```
   *     x' = (x << shift) - delta
   *     y' = (y << shift) - delta
   *   ```
   *
   *   Set the values of `shift` and `delta` to~0 to get the original point
   *   coordinates.
   */
  typedef struct  FT_Outline_Funcs_
  {
    FT_Outline_MoveToFunc   move_to;
    FT_Outline_LineToFunc   line_to;
    FT_Outline_ConicToFunc  conic_to;
    FT_Outline_CubicToFunc  cubic_to;

    int                     shift;
    FT_Pos                  delta;

  } FT_Outline_Funcs;


  /**************************************************************************
   *
   * @section:
   *   basic_types
   *
   */


  /**************************************************************************
   *
   * @macro:
   *   FT_IMAGE_TAG
   *
   * @description:
   *   This macro converts four-letter tags to an unsigned long type.
   *
   * @note:
   *   Since many 16-bit compilers don't like 32-bit enumerations, you should
   *   redefine this macro in case of problems to something like this:
   *
   *   ```
   *     #define FT_IMAGE_TAG( value, _x1, _x2, _x3, _x4 )  value
   *   ```
   *
   *   to get a simple enumeration without assigning special numbers.
   */
//#ifndef FT_IMAGE_TAG

//#define FT_IMAGE_TAG( value, _x1, _x2, _x3, _x4 )                         \
          //value = ( ( FT_STATIC_BYTE_CAST( unsigned long, _x1 ) << 24 ) | \
//                    ( FT_STATIC_BYTE_CAST( unsigned long, _x2 ) << 16 ) | \
//                    ( FT_STATIC_BYTE_CAST( unsigned long, _x3 ) << 8  ) | \
//                      FT_STATIC_BYTE_CAST( unsigned long, _x4 )         )

//#endif /* FT_IMAGE_TAG */


  /**************************************************************************
   *
   * @enum:
   *   FT_Glyph_Format
   *
   * @description:
   *   An enumeration type used to describe the format of a given glyph
   *   image.  Note that this version of FreeType only supports two image
   *   formats, even though future font drivers will be able to register
   *   their own format.
   *
   * @values:
   *   FT_GLYPH_FORMAT_NONE ::
   *     The value~0 is reserved.
   *
   *   FT_GLYPH_FORMAT_COMPOSITE ::
   *     The glyph image is a composite of several other images.  This format
   *     is _only_ used with @FT_LOAD_NO_RECURSE, and is used to report
   *     compound glyphs (like accented characters).
   *
   *   FT_GLYPH_FORMAT_BITMAP ::
   *     The glyph image is a bitmap, and can be described as an @FT_Bitmap.
   *     You generally need to access the `bitmap` field of the
   *     @FT_GlyphSlotRec structure to read it.
   *
   *   FT_GLYPH_FORMAT_OUTLINE ::
   *     The glyph image is a vectorial outline made of line segments and
   *     Bezier arcs; it can be described as an @FT_Outline; you generally
   *     want to access the `outline` field of the @FT_GlyphSlotRec structure
   *     to read it.
   *
   *   FT_GLYPH_FORMAT_PLOTTER ::
   *     The glyph image is a vectorial path with no inside and outside
   *     contours.  Some Type~1 fonts, like those in the Hershey family,
   *     contain glyphs in this format.  These are described as @FT_Outline,
   *     but FreeType isn't currently capable of rendering them correctly.
   *
   *   FT_GLYPH_FORMAT_SVG ::
   *     [Since 2.12] The glyph is represented by an SVG document in the
   *     'SVG~' table.
   */

/*
  typedef enum  FT_Glyph_Format_
  {
    FT_IMAGE_TAG( FT_GLYPH_FORMAT_NONE, 0, 0, 0, 0 ),

    FT_IMAGE_TAG( FT_GLYPH_FORMAT_COMPOSITE, 'c', 'o', 'm', 'p' ),
    FT_IMAGE_TAG( FT_GLYPH_FORMAT_BITMAP,    'b', 'i', 't', 's' ),
    FT_IMAGE_TAG( FT_GLYPH_FORMAT_OUTLINE,   'o', 'u', 't', 'l' ),
    FT_IMAGE_TAG( FT_GLYPH_FORMAT_PLOTTER,   'p', 'l', 'o', 't' ),
    FT_IMAGE_TAG( FT_GLYPH_FORMAT_SVG,       'S', 'V', 'G', ' ' )

  } FT_Glyph_Format;
*/

  /* these constants are deprecated; use the corresponding */
  /* `FT_Glyph_Format` values instead.                     */
#define ft_glyph_format_none       FT_GLYPH_FORMAT_NONE
#define ft_glyph_format_composite  FT_GLYPH_FORMAT_COMPOSITE
#define ft_glyph_format_bitmap     FT_GLYPH_FORMAT_BITMAP
#define ft_glyph_format_outline    FT_GLYPH_FORMAT_OUTLINE
#define ft_glyph_format_plotter    FT_GLYPH_FORMAT_PLOTTER


  /*************************************************************************/
  /*************************************************************************/
  /*************************************************************************/
  /*****                                                               *****/
  /*****            R A S T E R   D E F I N I T I O N S                *****/
  /*****                                                               *****/
  /*************************************************************************/
  /*************************************************************************/
  /*************************************************************************/



  /**************************************************************************
   *
   * @section:
   *   raster
   *
   * @title:
   *   Scanline Converter
   *
   * @abstract:
   *   How vectorial outlines are converted into bitmaps and pixmaps.
   *
   * @description:
   *   A raster or a rasterizer is a scan converter in charge of producing a
   *   pixel coverage bitmap that can be used as an alpha channel when
   *   compositing a glyph with a background.  FreeType comes with two
   *   rasterizers: bilevel `raster1` and anti-aliased `smooth` are two
   *   separate modules.  They are usually called from the high-level
   *   @FT_Load_Glyph or @FT_Render_Glyph functions and produce the entire
   *   coverage bitmap at once, while staying largely invisible to users.
   *
   *   Instead of working with complete coverage bitmaps, it is also possible
   *   to intercept consecutive pixel runs on the same scanline with the same
   *   coverage, called _spans_, and process them individually.  Only the
   *   `smooth` rasterizer permits this when calling @FT_Outline_Render with
   *   @FT_Raster_Params as described below.
   *
   *   Working with either complete bitmaps or spans it is important to think
   *   of them as colorless coverage objects suitable as alpha channels to
   *   blend arbitrary colors with a background.  For best results, it is
   *   recommended to use gamma correction, too.
   *
   *   This section also describes the public API needed to set up alternative
   *   @FT_Renderer modules.
   *
   * @order:
   *   FT_Span
   *   FT_SpanFunc
   *   FT_Raster_Params
   *   FT_RASTER_FLAG_XXX
   *
   *   FT_Raster
   *   FT_Raster_NewFunc
   *   FT_Raster_DoneFunc
   *   FT_Raster_ResetFunc
   *   FT_Raster_SetModeFunc
   *   FT_Raster_RenderFunc
   *   FT_Raster_Funcs
   *
   */


  /**************************************************************************
   *
   * @struct:
   *   FT_Span
   *
   * @description:
   *   A structure to model a single span of consecutive pixels when
   *   rendering an anti-aliased bitmap.
   *
   * @fields:
   *   x ::
   *     The span's horizontal start position.
   *
   *   len ::
   *     The span's length in pixels.
   *
   *   coverage ::
   *     The span color/coverage, ranging from 0 (background) to 255
   *     (foreground).
   *
   * @note:
   *   This structure is used by the span drawing callback type named
   *   @FT_SpanFunc that takes the y~coordinate of the span as a parameter.
   *
   *   The anti-aliased rasterizer produces coverage values from 0 to 255,
   *   that is, from completely transparent to completely opaque.
   */
  typedef struct  FT_Span_
  {
    short           x;
    unsigned short  len;
    unsigned char   coverage;

  } FT_Span;


  /**************************************************************************
   *
   * @functype:
   *   FT_SpanFunc
   *
   * @description:
   *   A function used as a call-back by the anti-aliased renderer in order
   *   to let client applications draw themselves the pixel spans on each
   *   scan line.
   *
   * @input:
   *   y ::
   *     The scanline's upward y~coordinate.
   *
   *   count ::
   *     The number of spans to draw on this scanline.
   *
   *   spans ::
   *     A table of `count` spans to draw on the scanline.
   *
   *   user ::
   *     User-supplied data that is passed to the callback.
   *
   * @note:
   *   This callback allows client applications to directly render the spans
   *   of the anti-aliased bitmap to any kind of surfaces.
   *
   *   This can be used to write anti-aliased outlines directly to a given
   *   background bitmap using alpha compositing.  It can also be used for
   *   oversampling and averaging.
   */
  typedef void
  (*FT_SpanFunc)( int             y,
                  int             count,
                  const FT_Span*  spans,
                  void*           user );

#define FT_Raster_Span_Func  FT_SpanFunc


  /**************************************************************************
   *
   * @functype:
   *   FT_Raster_BitTest_Func
   *
   * @description:
   *   Deprecated, unimplemented.
   */
  typedef int
  (*FT_Raster_BitTest_Func)( int    y,
                             int    x,
                             void*  user );


  /**************************************************************************
   *
   * @functype:
   *   FT_Raster_BitSet_Func
   *
   * @description:
   *   Deprecated, unimplemented.
   */
  typedef void
  (*FT_Raster_BitSet_Func)( int    y,
                            int    x,
                            void*  user );


  /**************************************************************************
   *
   * @enum:
   *   FT_RASTER_FLAG_XXX
   *
   * @description:
   *   A list of bit flag constants as used in the `flags` field of a
   *   @FT_Raster_Params structure.
   *
   * @values:
   *   FT_RASTER_FLAG_DEFAULT ::
   *     This value is 0.
   *
   *   FT_RASTER_FLAG_AA ::
   *     This flag is set to indicate that an anti-aliased glyph image should
   *     be generated.  Otherwise, it will be monochrome (1-bit).
   *
   *   FT_RASTER_FLAG_DIRECT ::
   *     This flag is set to indicate direct rendering.  In this mode, client
   *     applications must provide their own span callback.  This lets them
   *     directly draw or compose over an existing bitmap.  If this bit is
   *     _not_ set, the target pixmap's buffer _must_ be zeroed before
   *     rendering and the output will be clipped to its size.
   *
   *     Direct rendering is only possible with anti-aliased glyphs.
   *
   *   FT_RASTER_FLAG_CLIP ::
   *     This flag is only used in direct rendering mode.  If set, the output
   *     will be clipped to a box specified in the `clip_box` field of the
   *     @FT_Raster_Params structure.  Otherwise, the `clip_box` is
   *     effectively set to the bounding box and all spans are generated.
   *
   *   FT_RASTER_FLAG_SDF ::
   *     This flag is set to indicate that a signed distance field glyph
   *     image should be generated.  This is only used while rendering with
   *     the @FT_RENDER_MODE_SDF render mode.
   */
#define FT_RASTER_FLAG_DEFAULT  0x0
#define FT_RASTER_FLAG_AA       0x1
#define FT_RASTER_FLAG_DIRECT   0x2
#define FT_RASTER_FLAG_CLIP     0x4
#define FT_RASTER_FLAG_SDF      0x8

  /* these constants are deprecated; use the corresponding */
  /* `FT_RASTER_FLAG_XXX` values instead                   */
#define ft_raster_flag_default  FT_RASTER_FLAG_DEFAULT
#define ft_raster_flag_aa       FT_RASTER_FLAG_AA
#define ft_raster_flag_direct   FT_RASTER_FLAG_DIRECT
#define ft_raster_flag_clip     FT_RASTER_FLAG_CLIP


  /**************************************************************************
   *
   * @struct:
   *   FT_Raster_Params
   *
   * @description:
   *   A structure to hold the parameters used by a raster's render function,
   *   passed as an argument to @FT_Outline_Render.
   *
   * @fields:
   *   target ::
   *     The target bitmap.
   *
   *   source ::
   *     A pointer to the source glyph image (e.g., an @FT_Outline).
   *
   *   flags ::
   *     The rendering flags.
   *
   *   gray_spans ::
   *     The gray span drawing callback.
   *
   *   black_spans ::
   *     Unused.
   *
   *   bit_test ::
   *     Unused.
   *
   *   bit_set ::
   *     Unused.
   *
   *   user ::
   *     User-supplied data that is passed to each drawing callback.
   *
   *   clip_box ::
   *     An optional span clipping box expressed in _integer_ pixels
   *     (not in 26.6 fixed-point units).
   *
   * @note:
   *   The @FT_RASTER_FLAG_AA bit flag must be set in the `flags` to
   *   generate an anti-aliased glyph bitmap, otherwise a monochrome bitmap
   *   is generated.  The `target` should have appropriate pixel mode and its
   *   dimensions define the clipping region.
   *
   *   If both @FT_RASTER_FLAG_AA and @FT_RASTER_FLAG_DIRECT bit flags
   *   are set in `flags`, the raster calls an @FT_SpanFunc callback
   *   `gray_spans` with `user` data as an argument ignoring `target`.  This
   *   allows direct composition over a pre-existing user surface to perform
   *   the span drawing and composition.  To optionally clip the spans, set
   *   the @FT_RASTER_FLAG_CLIP flag and `clip_box`.  The monochrome raster
   *   does not support the direct mode.
   *
   *   The gray-level rasterizer always uses 256 gray levels.  If you want
   *   fewer gray levels, you have to use @FT_RASTER_FLAG_DIRECT and reduce
   *   the levels in the callback function.
   */
  typedef struct  FT_Raster_Params_
  {
    const FT_Bitmap*        target;
    const void*             source;
    int                     flags;
    FT_SpanFunc             gray_spans;
    FT_SpanFunc             black_spans;  /* unused */
    FT_Raster_BitTest_Func  bit_test;     /* unused */
    FT_Raster_BitSet_Func   bit_set;      /* unused */
    void*                   user;
    FT_BBox                 clip_box;

  } FT_Raster_Params;


  /**************************************************************************
   *
   * @type:
   *   FT_Raster
   *
   * @description:
   *   An opaque handle (pointer) to a raster object.  Each object can be
   *   used independently to convert an outline into a bitmap or pixmap.
   *
   * @note:
   *   In FreeType 2, all rasters are now encapsulated within specific
   *   @FT_Renderer modules and only used in their context.
   *
   */
  typedef struct FT_RasterRec_*  FT_Raster;


  /**************************************************************************
   *
   * @functype:
   *   FT_Raster_NewFunc
   *
   * @description:
   *   A function used to create a new raster object.
   *
   * @input:
   *   memory ::
   *     A handle to the memory allocator.
   *
   * @output:
   *   raster ::
   *     A handle to the new raster object.
   *
   * @return:
   *   Error code.  0~means success.
   *
   * @note:
   *   The `memory` parameter is a typeless pointer in order to avoid
   *   un-wanted dependencies on the rest of the FreeType code.  In practice,
   *   it is an @FT_Memory object, i.e., a handle to the standard FreeType
   *   memory allocator.  However, this field can be completely ignored by a
   *   given raster implementation.
   */
  typedef int
  (*FT_Raster_NewFunc)( void*       memory,
                        FT_Raster*  raster );

#define FT_Raster_New_Func  FT_Raster_NewFunc


  /**************************************************************************
   *
   * @functype:
   *   FT_Raster_DoneFunc
   *
   * @description:
   *   A function used to destroy a given raster object.
   *
   * @input:
   *   raster ::
   *     A handle to the raster object.
   */
  typedef void
  (*FT_Raster_DoneFunc)( FT_Raster  raster );

#define FT_Raster_Done_Func  FT_Raster_DoneFunc


  /**************************************************************************
   *
   * @functype:
   *   FT_Raster_ResetFunc
   *
   * @description:
   *   FreeType used to provide an area of memory called the 'render pool'
   *   available to all registered rasterizers.  This was not thread safe,
   *   however, and now FreeType never allocates this pool.
   *
   *   This function is called after a new raster object is created.
   *
   * @input:
   *   raster ::
   *     A handle to the new raster object.
   *
   *   pool_base ::
   *     Previously, the address in memory of the render pool.  Set this to
   *     `NULL`.
   *
   *   pool_size ::
   *     Previously, the size in bytes of the render pool.  Set this to 0.
   *
   * @note:
   *   Rasterizers should rely on dynamic or stack allocation if they want to
   *   (a handle to the memory allocator is passed to the rasterizer
   *   constructor).
   */
  typedef void
  (*FT_Raster_ResetFunc)( FT_Raster       raster,
                          unsigned char*  pool_base,
                          unsigned long   pool_size );

#define FT_Raster_Reset_Func  FT_Raster_ResetFunc


  /**************************************************************************
   *
   * @functype:
   *   FT_Raster_SetModeFunc
   *
   * @description:
   *   This function is a generic facility to change modes or attributes in a
   *   given raster.  This can be used for debugging purposes, or simply to
   *   allow implementation-specific 'features' in a given raster module.
   *
   * @input:
   *   raster ::
   *     A handle to the new raster object.
   *
   *   mode ::
   *     A 4-byte tag used to name the mode or property.
   *
   *   args ::
   *     A pointer to the new mode/property to use.
   */
  typedef int
  (*FT_Raster_SetModeFunc)( FT_Raster      raster,
                            unsigned long  mode,
                            void*          args );

#define FT_Raster_Set_Mode_Func  FT_Raster_SetModeFunc


  /**************************************************************************
   *
   * @functype:
   *   FT_Raster_RenderFunc
   *
   * @description:
   *   Invoke a given raster to scan-convert a given glyph image into a
   *   target bitmap.
   *
   * @input:
   *   raster ::
   *     A handle to the raster object.
   *
   *   params ::
   *     A pointer to an @FT_Raster_Params structure used to store the
   *     rendering parameters.
   *
   * @return:
   *   Error code.  0~means success.
   *
   * @note:
   *   The exact format of the source image depends on the raster's glyph
   *   format defined in its @FT_Raster_Funcs structure.  It can be an
   *   @FT_Outline or anything else in order to support a large array of
   *   glyph formats.
   *
   *   Note also that the render function can fail and return a
   *   `FT_Err_Unimplemented_Feature` error code if the raster used does not
   *   support direct composition.
   */
  typedef int
  (*FT_Raster_RenderFunc)( FT_Raster                raster,
                           const FT_Raster_Params*  params );

#define FT_Raster_Render_Func  FT_Raster_RenderFunc


  /**************************************************************************
   *
   * @struct:
   *   FT_Raster_Funcs
   *
   * @description:
   *  A structure used to describe a given raster class to the library.
   *
   * @fields:
   *   glyph_format ::
   *     The supported glyph format for this raster.
   *
   *   raster_new ::
   *     The raster constructor.
   *
   *   raster_reset ::
   *     Used to reset the render pool within the raster.
   *
   *   raster_render ::
   *     A function to render a glyph into a given bitmap.
   *
   *   raster_done ::
   *     The raster destructor.
   */
  typedef struct  FT_Raster_Funcs_
  {
    FT_Glyph_Format        glyph_format;

    FT_Raster_NewFunc      raster_new;
    FT_Raster_ResetFunc    raster_reset;
    FT_Raster_SetModeFunc  raster_set_mode;
    FT_Raster_RenderFunc   raster_render;
    FT_Raster_DoneFunc     raster_done;

  } FT_Raster_Funcs;

  /* */




#endif /* FTIMAGE_H_ */


/* END */


/* Local Variables: */
/* coding: utf-8    */
/* End:             */
//
// ===========================  ftbbox.h  ===========================
//
/****************************************************************************
 *
 * ftbbox.h
 *
 *   FreeType exact bbox computation (specification).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


  /**************************************************************************
   *
   * This component has a _single_ role: to compute exact outline bounding
   * boxes.
   *
   * It is separated from the rest of the engine for various technical
   * reasons.  It may well be integrated in 'ftoutln' later.
   *
   */


#ifndef FTBBOX_H_
#define FTBBOX_H_


#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   outline_processing
   *
   */


  /**************************************************************************
   *
   * @function:
   *   FT_Outline_Get_BBox
   *
   * @description:
   *   Compute the exact bounding box of an outline.  This is slower than
   *   computing the control box.  However, it uses an advanced algorithm
   *   that returns _very_ quickly when the two boxes coincide.  Otherwise,
   *   the outline Bezier arcs are traversed to extract their extrema.
   *
   * @input:
   *   outline ::
   *     A pointer to the source outline.
   *
   * @output:
   *   abbox ::
   *     The outline's exact bounding box.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   If the font is tricky and the glyph has been loaded with
   *   @FT_LOAD_NO_SCALE, the resulting BBox is meaningless.  To get
   *   reasonable values for the BBox it is necessary to load the glyph at a
   *   large ppem value (so that the hinting instructions can properly shift
   *   and scale the subglyphs), then extracting the BBox, which can be
   *   eventually converted back to font units.
   */
   FT_Error 
  FT_Outline_Get_BBox( FT_Outline*  outline,
                       FT_BBox     *abbox );

  /* */




#endif /* FTBBOX_H_ */


/* END */


/* Local Variables: */
/* coding: utf-8    */
/* End:             */
//
// ===========================  ftstroke.h  ===========================
//
/****************************************************************************
 *
 * ftstroke.h
 *
 *   FreeType path stroker (specification).
 *
 * Copyright (C) 2002-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTSTROKE_H_
#define FTSTROKE_H_

#include <freetype/ftoutln.h>
#include <freetype/ftglyph.h>





  /**************************************************************************
   *
   * @section:
   *    glyph_stroker
   *
   * @title:
   *    Glyph Stroker
   *
   * @abstract:
   *    Generating bordered and stroked glyphs.
   *
   * @description:
   *    This component generates stroked outlines of a given vectorial glyph.
   *    It also allows you to retrieve the 'outside' and/or the 'inside'
   *    borders of the stroke.
   *
   *    This can be useful to generate 'bordered' glyph, i.e., glyphs
   *    displayed with a colored (and anti-aliased) border around their
   *    shape.
   *
   * @order:
   *    FT_Stroker
   *
   *    FT_Stroker_LineJoin
   *    FT_Stroker_LineCap
   *    FT_StrokerBorder
   *
   *    FT_Outline_GetInsideBorder
   *    FT_Outline_GetOutsideBorder
   *
   *    FT_Glyph_Stroke
   *    FT_Glyph_StrokeBorder
   *
   *    FT_Stroker_New
   *    FT_Stroker_Set
   *    FT_Stroker_Rewind
   *    FT_Stroker_ParseOutline
   *    FT_Stroker_Done
   *
   *    FT_Stroker_BeginSubPath
   *    FT_Stroker_EndSubPath
   *
   *    FT_Stroker_LineTo
   *    FT_Stroker_ConicTo
   *    FT_Stroker_CubicTo
   *
   *    FT_Stroker_GetBorderCounts
   *    FT_Stroker_ExportBorder
   *    FT_Stroker_GetCounts
   *    FT_Stroker_Export
   *
   */


  /**************************************************************************
   *
   * @type:
   *   FT_Stroker
   *
   * @description:
   *   Opaque handle to a path stroker object.
   */
  typedef struct FT_StrokerRec_*  FT_Stroker;


  /**************************************************************************
   *
   * @enum:
   *   FT_Stroker_LineJoin
   *
   * @description:
   *   These values determine how two joining lines are rendered in a
   *   stroker.
   *
   * @values:
   *   FT_STROKER_LINEJOIN_ROUND ::
   *     Used to render rounded line joins.  Circular arcs are used to join
   *     two lines smoothly.
   *
   *   FT_STROKER_LINEJOIN_BEVEL ::
   *     Used to render beveled line joins.  The outer corner of the joined
   *     lines is filled by enclosing the triangular region of the corner
   *     with a straight line between the outer corners of each stroke.
   *
   *   FT_STROKER_LINEJOIN_MITER_FIXED ::
   *     Used to render mitered line joins, with fixed bevels if the miter
   *     limit is exceeded.  The outer edges of the strokes for the two
   *     segments are extended until they meet at an angle.  A bevel join
   *     (see above) is used if the segments meet at too sharp an angle and
   *     the outer edges meet beyond a distance corresponding to the meter
   *     limit.  This prevents long spikes being created.
   *     `FT_STROKER_LINEJOIN_MITER_FIXED` generates a miter line join as
   *     used in PostScript and PDF.
   *
   *   FT_STROKER_LINEJOIN_MITER_VARIABLE ::
   *   FT_STROKER_LINEJOIN_MITER ::
   *     Used to render mitered line joins, with variable bevels if the miter
   *     limit is exceeded.  The intersection of the strokes is clipped
   *     perpendicularly to the bisector, at a distance corresponding to
   *     the miter limit. This prevents long spikes being created.
   *     `FT_STROKER_LINEJOIN_MITER_VARIABLE` generates a mitered line join
   *     as used in XPS.  `FT_STROKER_LINEJOIN_MITER` is an alias for
   *     `FT_STROKER_LINEJOIN_MITER_VARIABLE`, retained for backward
   *     compatibility.
   */
  typedef enum  FT_Stroker_LineJoin_
  {
    FT_STROKER_LINEJOIN_ROUND          = 0,
    FT_STROKER_LINEJOIN_BEVEL          = 1,
    FT_STROKER_LINEJOIN_MITER_VARIABLE = 2,
    FT_STROKER_LINEJOIN_MITER          = FT_STROKER_LINEJOIN_MITER_VARIABLE,
    FT_STROKER_LINEJOIN_MITER_FIXED    = 3

  } FT_Stroker_LineJoin;


  /**************************************************************************
   *
   * @enum:
   *   FT_Stroker_LineCap
   *
   * @description:
   *   These values determine how the end of opened sub-paths are rendered in
   *   a stroke.
   *
   * @values:
   *   FT_STROKER_LINECAP_BUTT ::
   *     The end of lines is rendered as a full stop on the last point
   *     itself.
   *
   *   FT_STROKER_LINECAP_ROUND ::
   *     The end of lines is rendered as a half-circle around the last point.
   *
   *   FT_STROKER_LINECAP_SQUARE ::
   *     The end of lines is rendered as a square around the last point.
   */
  typedef enum  FT_Stroker_LineCap_
  {
    FT_STROKER_LINECAP_BUTT = 0,
    FT_STROKER_LINECAP_ROUND,
    FT_STROKER_LINECAP_SQUARE

  } FT_Stroker_LineCap;


  /**************************************************************************
   *
   * @enum:
   *   FT_StrokerBorder
   *
   * @description:
   *   These values are used to select a given stroke border in
   *   @FT_Stroker_GetBorderCounts and @FT_Stroker_ExportBorder.
   *
   * @values:
   *   FT_STROKER_BORDER_LEFT ::
   *     Select the left border, relative to the drawing direction.
   *
   *   FT_STROKER_BORDER_RIGHT ::
   *     Select the right border, relative to the drawing direction.
   *
   * @note:
   *   Applications are generally interested in the 'inside' and 'outside'
   *   borders.  However, there is no direct mapping between these and the
   *   'left' and 'right' ones, since this really depends on the glyph's
   *   drawing orientation, which varies between font formats.
   *
   *   You can however use @FT_Outline_GetInsideBorder and
   *   @FT_Outline_GetOutsideBorder to get these.
   */
  typedef enum  FT_StrokerBorder_
  {
    FT_STROKER_BORDER_LEFT = 0,
    FT_STROKER_BORDER_RIGHT

  } FT_StrokerBorder;


  /**************************************************************************
   *
   * @function:
   *   FT_Outline_GetInsideBorder
   *
   * @description:
   *   Retrieve the @FT_StrokerBorder value corresponding to the 'inside'
   *   borders of a given outline.
   *
   * @input:
   *   outline ::
   *     The source outline handle.
   *
   * @return:
   *   The border index.  @FT_STROKER_BORDER_RIGHT for empty or invalid
   *   outlines.
   */
   FT_StrokerBorder 
  FT_Outline_GetInsideBorder( FT_Outline*  outline );


  /**************************************************************************
   *
   * @function:
   *   FT_Outline_GetOutsideBorder
   *
   * @description:
   *   Retrieve the @FT_StrokerBorder value corresponding to the 'outside'
   *   borders of a given outline.
   *
   * @input:
   *   outline ::
   *     The source outline handle.
   *
   * @return:
   *   The border index.  @FT_STROKER_BORDER_LEFT for empty or invalid
   *   outlines.
   */
   FT_StrokerBorder 
  FT_Outline_GetOutsideBorder( FT_Outline*  outline );


  /**************************************************************************
   *
   * @function:
   *   FT_Stroker_New
   *
   * @description:
   *   Create a new stroker object.
   *
   * @input:
   *   library ::
   *     FreeType library handle.
   *
   * @output:
   *   astroker ::
   *     A new stroker object handle.  `NULL` in case of error.
   *
   * @return:
   *    FreeType error code.  0~means success.
   */
   FT_Error 
  FT_Stroker_New( FT_Library   library,
                  FT_Stroker  *astroker );


  /**************************************************************************
   *
   * @function:
   *   FT_Stroker_Set
   *
   * @description:
   *   Reset a stroker object's attributes.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   *   radius ::
   *     The border radius.
   *
   *   line_cap ::
   *     The line cap style.
   *
   *   line_join ::
   *     The line join style.
   *
   *   miter_limit ::
   *     The maximum reciprocal sine of half-angle at the miter join,
   *     expressed as 16.16 fixed-point value.
   *
   * @note:
   *   The `radius` is expressed in the same units as the outline
   *   coordinates.
   *
   *   The `miter_limit` multiplied by the `radius` gives the maximum size
   *   of a miter spike, at which it is clipped for
   *   @FT_STROKER_LINEJOIN_MITER_VARIABLE or replaced with a bevel join for
   *   @FT_STROKER_LINEJOIN_MITER_FIXED.
   *
   *   This function calls @FT_Stroker_Rewind automatically.
   */
   void 
  FT_Stroker_Set( FT_Stroker           stroker,
                  FT_Fixed             radius,
                  FT_Stroker_LineCap   line_cap,
                  FT_Stroker_LineJoin  line_join,
                  FT_Fixed             miter_limit );


  /**************************************************************************
   *
   * @function:
   *   FT_Stroker_Rewind
   *
   * @description:
   *   Reset a stroker object without changing its attributes.  You should
   *   call this function before beginning a new series of calls to
   *   @FT_Stroker_BeginSubPath or @FT_Stroker_EndSubPath.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   */
   void 
  FT_Stroker_Rewind( FT_Stroker  stroker );


  /**************************************************************************
   *
   * @function:
   *   FT_Stroker_ParseOutline
   *
   * @description:
   *   A convenience function used to parse a whole outline with the stroker.
   *   The resulting outline(s) can be retrieved later by functions like
   *   @FT_Stroker_GetCounts and @FT_Stroker_Export.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   *   outline ::
   *     The source outline.
   *
   *   opened ::
   *     A boolean.  If~1, the outline is treated as an open path instead of
   *     a closed one.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   If `opened` is~0 (the default), the outline is treated as a closed
   *   path, and the stroker generates two distinct 'border' outlines.
   *
   *   If `opened` is~1, the outline is processed as an open path, and the
   *   stroker generates a single 'stroke' outline.
   *
   *   This function calls @FT_Stroker_Rewind automatically.
   */
   FT_Error 
  FT_Stroker_ParseOutline( FT_Stroker   stroker,
                           FT_Outline*  outline,
                           FT_Bool      opened );


  /**************************************************************************
   *
   * @function:
   *   FT_Stroker_BeginSubPath
   *
   * @description:
   *   Start a new sub-path in the stroker.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   *   to ::
   *     A pointer to the start vector.
   *
   *   open ::
   *     A boolean.  If~1, the sub-path is treated as an open one.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function is useful when you need to stroke a path that is not
   *   stored as an @FT_Outline object.
   */
   FT_Error 
  FT_Stroker_BeginSubPath( FT_Stroker  stroker,
                           FT_Vector*  to,
                           FT_Bool     open );


  /**************************************************************************
   *
   * @function:
   *   FT_Stroker_EndSubPath
   *
   * @description:
   *   Close the current sub-path in the stroker.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   You should call this function after @FT_Stroker_BeginSubPath.  If the
   *   subpath was not 'opened', this function 'draws' a single line segment
   *   to the start position when needed.
   */
   FT_Error 
  FT_Stroker_EndSubPath( FT_Stroker  stroker );


  /**************************************************************************
   *
   * @function:
   *   FT_Stroker_LineTo
   *
   * @description:
   *   'Draw' a single line segment in the stroker's current sub-path, from
   *   the last position.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   *   to ::
   *     A pointer to the destination point.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   You should call this function between @FT_Stroker_BeginSubPath and
   *   @FT_Stroker_EndSubPath.
   */
   FT_Error 
  FT_Stroker_LineTo( FT_Stroker  stroker,
                     FT_Vector*  to );


  /**************************************************************************
   *
   * @function:
   *   FT_Stroker_ConicTo
   *
   * @description:
   *   'Draw' a single quadratic Bezier in the stroker's current sub-path,
   *   from the last position.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   *   control ::
   *     A pointer to a Bezier control point.
   *
   *   to ::
   *     A pointer to the destination point.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   You should call this function between @FT_Stroker_BeginSubPath and
   *   @FT_Stroker_EndSubPath.
   */
   FT_Error 
  FT_Stroker_ConicTo( FT_Stroker  stroker,
                      FT_Vector*  control,
                      FT_Vector*  to );


  /**************************************************************************
   *
   * @function:
   *   FT_Stroker_CubicTo
   *
   * @description:
   *   'Draw' a single cubic Bezier in the stroker's current sub-path, from
   *   the last position.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   *   control1 ::
   *     A pointer to the first Bezier control point.
   *
   *   control2 ::
   *     A pointer to second Bezier control point.
   *
   *   to ::
   *     A pointer to the destination point.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   You should call this function between @FT_Stroker_BeginSubPath and
   *   @FT_Stroker_EndSubPath.
   */
   FT_Error 
  FT_Stroker_CubicTo( FT_Stroker  stroker,
                      FT_Vector*  control1,
                      FT_Vector*  control2,
                      FT_Vector*  to );


  /**************************************************************************
   *
   * @function:
   *   FT_Stroker_GetBorderCounts
   *
   * @description:
   *   Call this function once you have finished parsing your paths with the
   *   stroker.  It returns the number of points and contours necessary to
   *   export one of the 'border' or 'stroke' outlines generated by the
   *   stroker.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   *   border ::
   *     The border index.
   *
   * @output:
   *   anum_points ::
   *     The number of points.
   *
   *   anum_contours ::
   *     The number of contours.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   When an outline, or a sub-path, is 'closed', the stroker generates two
   *   independent 'border' outlines, named 'left' and 'right'.
   *
   *   When the outline, or a sub-path, is 'opened', the stroker merges the
   *   'border' outlines with caps.  The 'left' border receives all points,
   *   while the 'right' border becomes empty.
   *
   *   Use the function @FT_Stroker_GetCounts instead if you want to retrieve
   *   the counts associated to both borders.
   */
   FT_Error 
  FT_Stroker_GetBorderCounts( FT_Stroker        stroker,
                              FT_StrokerBorder  border,
                              FT_UInt          *anum_points,
                              FT_UInt          *anum_contours );


  /**************************************************************************
   *
   * @function:
   *   FT_Stroker_ExportBorder
   *
   * @description:
   *   Call this function after @FT_Stroker_GetBorderCounts to export the
   *   corresponding border to your own @FT_Outline structure.
   *
   *   Note that this function appends the border points and contours to your
   *   outline, but does not try to resize its arrays.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   *   border ::
   *     The border index.
   *
   *   outline ::
   *     The target outline handle.
   *
   * @note:
   *   Always call this function after @FT_Stroker_GetBorderCounts to get
   *   sure that there is enough room in your @FT_Outline object to receive
   *   all new data.
   *
   *   When an outline, or a sub-path, is 'closed', the stroker generates two
   *   independent 'border' outlines, named 'left' and 'right'.
   *
   *   When the outline, or a sub-path, is 'opened', the stroker merges the
   *   'border' outlines with caps.  The 'left' border receives all points,
   *   while the 'right' border becomes empty.
   *
   *   Use the function @FT_Stroker_Export instead if you want to retrieve
   *   all borders at once.
   */
   void 
  FT_Stroker_ExportBorder( FT_Stroker        stroker,
                           FT_StrokerBorder  border,
                           FT_Outline*       outline );


  /**************************************************************************
   *
   * @function:
   *   FT_Stroker_GetCounts
   *
   * @description:
   *   Call this function once you have finished parsing your paths with the
   *   stroker.  It returns the number of points and contours necessary to
   *   export all points/borders from the stroked outline/path.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   * @output:
   *   anum_points ::
   *     The number of points.
   *
   *   anum_contours ::
   *     The number of contours.
   *
   * @return:
   *   FreeType error code.  0~means success.
   */
   FT_Error 
  FT_Stroker_GetCounts( FT_Stroker  stroker,
                        FT_UInt    *anum_points,
                        FT_UInt    *anum_contours );


  /**************************************************************************
   *
   * @function:
   *   FT_Stroker_Export
   *
   * @description:
   *   Call this function after @FT_Stroker_GetBorderCounts to export all
   *   borders to your own @FT_Outline structure.
   *
   *   Note that this function appends the border points and contours to your
   *   outline, but does not try to resize its arrays.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   *   outline ::
   *     The target outline handle.
   */
   void 
  FT_Stroker_Export( FT_Stroker   stroker,
                     FT_Outline*  outline );


  /**************************************************************************
   *
   * @function:
   *   FT_Stroker_Done
   *
   * @description:
   *   Destroy a stroker object.
   *
   * @input:
   *   stroker ::
   *     A stroker handle.  Can be `NULL`.
   */
   void 
  FT_Stroker_Done( FT_Stroker  stroker );


  /**************************************************************************
   *
   * @function:
   *   FT_Glyph_Stroke
   *
   * @description:
   *   Stroke a given outline glyph object with a given stroker.
   *
   * @inout:
   *   pglyph ::
   *     Source glyph handle on input, new glyph handle on output.
   *
   * @input:
   *   stroker ::
   *     A stroker handle.
   *
   *   destroy ::
   *     A Boolean.  If~1, the source glyph object is destroyed on success.
   *
   * @return:
   *    FreeType error code.  0~means success.
   *
   * @note:
   *   The source glyph is untouched in case of error.
   *
   *   Adding stroke may yield a significantly wider and taller glyph
   *   depending on how large of a radius was used to stroke the glyph.  You
   *   may need to manually adjust horizontal and vertical advance amounts to
   *   account for this added size.
   */
   FT_Error 
  FT_Glyph_Stroke( FT_Glyph    *pglyph,
                   FT_Stroker   stroker,
                   FT_Bool      destroy );


  /**************************************************************************
   *
   * @function:
   *   FT_Glyph_StrokeBorder
   *
   * @description:
   *   Stroke a given outline glyph object with a given stroker, but only
   *   return either its inside or outside border.
   *
   * @inout:
   *   pglyph ::
   *     Source glyph handle on input, new glyph handle on output.
   *
   * @input:
   *   stroker ::
   *     A stroker handle.
   *
   *   inside ::
   *     A Boolean.  If~1, return the inside border, otherwise the outside
   *     border.
   *
   *   destroy ::
   *     A Boolean.  If~1, the source glyph object is destroyed on success.
   *
   * @return:
   *    FreeType error code.  0~means success.
   *
   * @note:
   *   The source glyph is untouched in case of error.
   *
   *   Adding stroke may yield a significantly wider and taller glyph
   *   depending on how large of a radius was used to stroke the glyph.  You
   *   may need to manually adjust horizontal and vertical advance amounts to
   *   account for this added size.
   */
   FT_Error 
  FT_Glyph_StrokeBorder( FT_Glyph    *pglyph,
                         FT_Stroker   stroker,
                         FT_Bool      inside,
                         FT_Bool      destroy );

  /* */



#endif /* FTSTROKE_H_ */


/* END */


/* Local Variables: */
/* coding: utf-8    */
/* End:             */
//
// ===========================  fterrdef.h  ===========================
//
/****************************************************************************
 *
 * fterrdef.h
 *
 *   FreeType error codes (specification).
 *
 * Copyright (C) 2002-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


  /**************************************************************************
   *
   * @section:
   *  error_code_values
   *
   * @title:
   *  Error Code Values
   *
   * @abstract:
   *  All possible error codes returned by FreeType functions.
   *
   * @description:
   *  The list below is taken verbatim from the file `fterrdef.h` (loaded
   *  automatically by including `FT_FREETYPE_H`).  The first argument of the
   *  `FT_ERROR_DEF_` macro is the error label; by default, the prefix
   *  `FT_Err_` gets added so that you get error names like
   *  `FT_Err_Cannot_Open_Resource`.  The second argument is the error code,
   *  and the last argument an error string, which is not used by FreeType.
   *
   *  Within your application you should **only** use error names and
   *  **never** its numeric values!  The latter might (and actually do)
   *  change in forthcoming FreeType versions.
   *
   *  Macro `FT_NOERRORDEF_` defines `FT_Err_Ok`, which is always zero.  See
   *  the 'Error Enumerations' subsection how to automatically generate a
   *  list of error strings.
   *
   */


  /**************************************************************************
   *
   * @enum:
   *   FT_Err_XXX
   *
   */

  /* generic errors */
/*
  FT_NOERRORDEF_( Ok,                                        0x00,
                  "no error" )

  FT_ERRORDEF_( Cannot_Open_Resource,                        0x01,
                "cannot open resource" )
  FT_ERRORDEF_( Unknown_File_Format,                         0x02,
                "unknown file format" )
  FT_ERRORDEF_( Invalid_File_Format,                         0x03,
                "broken file" )
  FT_ERRORDEF_( Invalid_Version,                             0x04,
                "invalid FreeType version" )
  FT_ERRORDEF_( Lower_Module_Version,                        0x05,
                "module version is too low" )
  FT_ERRORDEF_( Invalid_Argument,                            0x06,
                "invalid argument" )
  FT_ERRORDEF_( Unimplemented_Feature,                       0x07,
                "unimplemented feature" )
  FT_ERRORDEF_( Invalid_Table,                               0x08,
                "broken table" )
  FT_ERRORDEF_( Invalid_Offset,                              0x09,
                "broken offset within table" )
  FT_ERRORDEF_( Array_Too_Large,                             0x0A,
                "array allocation size too large" )
  FT_ERRORDEF_( Missing_Module,                              0x0B,
                "missing module" )
  FT_ERRORDEF_( Missing_Property,                            0x0C,
                "missing property" )

//   glyph/character errors 

  FT_ERRORDEF_( Invalid_Glyph_Index,                         0x10,
                "invalid glyph index" )
  FT_ERRORDEF_( Invalid_Character_Code,                      0x11,
                "invalid character code" )
  FT_ERRORDEF_( Invalid_Glyph_Format,                        0x12,
                "unsupported glyph image format" )
  FT_ERRORDEF_( Cannot_Render_Glyph,                         0x13,
                "cannot render this glyph format" )
  FT_ERRORDEF_( Invalid_Outline,                             0x14,
                "invalid outline" )
  FT_ERRORDEF_( Invalid_Composite,                           0x15,
                "invalid composite glyph" )
  FT_ERRORDEF_( Too_Many_Hints,                              0x16,
                "too many hints" )
  FT_ERRORDEF_( Invalid_Pixel_Size,                          0x17,
                "invalid pixel size" )
  FT_ERRORDEF_( Invalid_SVG_Document,                        0x18,
                "invalid SVG document" )

  // handle errors 

  FT_ERRORDEF_( Invalid_Handle,                              0x20,
                "invalid object handle" )
  FT_ERRORDEF_( Invalid_Library_Handle,                      0x21,
                "invalid library handle" )
  FT_ERRORDEF_( Invalid_Driver_Handle,                       0x22,
                "invalid module handle" )
  FT_ERRORDEF_( Invalid_Face_Handle,                         0x23,
                "invalid face handle" )
  FT_ERRORDEF_( Invalid_Size_Handle,                         0x24,
                "invalid size handle" )
  FT_ERRORDEF_( Invalid_Slot_Handle,                         0x25,
                "invalid glyph slot handle" )
  FT_ERRORDEF_( Invalid_CharMap_Handle,                      0x26,
                "invalid charmap handle" )
  FT_ERRORDEF_( Invalid_Cache_Handle,                        0x27,
                "invalid cache manager handle" )
  FT_ERRORDEF_( Invalid_Stream_Handle,                       0x28,
                "invalid stream handle" )

  // driver errors 

  FT_ERRORDEF_( Too_Many_Drivers,                            0x30,
                "too many modules" )
  FT_ERRORDEF_( Too_Many_Extensions,                         0x31,
                "too many extensions" )

 // memory errors 

  FT_ERRORDEF_( Out_Of_Memory,                               0x40,
                "out of memory" )
  FT_ERRORDEF_( Unlisted_Object,                             0x41,
                "unlisted object" )

  // stream errors 

  FT_ERRORDEF_( Cannot_Open_Stream,                          0x51,
                "cannot open stream" )
  FT_ERRORDEF_( Invalid_Stream_Seek,                         0x52,
                "invalid stream seek" )
  FT_ERRORDEF_( Invalid_Stream_Skip,                         0x53,
                "invalid stream skip" )
  FT_ERRORDEF_( Invalid_Stream_Read,                         0x54,
                "invalid stream read" )
  FT_ERRORDEF_( Invalid_Stream_Operation,                    0x55,
                "invalid stream operation" )
  FT_ERRORDEF_( Invalid_Frame_Operation,                     0x56,
                "invalid frame operation" )
  FT_ERRORDEF_( Nested_Frame_Access,                         0x57,
                "nested frame access" )
  FT_ERRORDEF_( Invalid_Frame_Read,                          0x58,
                "invalid frame read" )

  // raster errors 

  FT_ERRORDEF_( Raster_Uninitialized,                        0x60,
                "raster uninitialized" )
  FT_ERRORDEF_( Raster_Corrupted,                            0x61,
                "raster corrupted" )
  FT_ERRORDEF_( Raster_Overflow,                             0x62,
                "raster overflow" )
  FT_ERRORDEF_( Raster_Negative_Height,                      0x63,
                "negative height while rastering" )

  // cache errors 

  FT_ERRORDEF_( Too_Many_Caches,                             0x70,
                "too many registered caches" )

  //TrueType and SFNT errors 

  FT_ERRORDEF_( Invalid_Opcode,                              0x80,
                "invalid opcode" )
  FT_ERRORDEF_( Too_Few_Arguments,                           0x81,
                "too few arguments" )
  FT_ERRORDEF_( Stack_Overflow,                              0x82,
                "stack overflow" )
  FT_ERRORDEF_( Code_Overflow,                               0x83,
                "code overflow" )
  FT_ERRORDEF_( Bad_Argument,                                0x84,
                "bad argument" )
  FT_ERRORDEF_( Divide_By_Zero,                              0x85,
                "division by zero" )
  FT_ERRORDEF_( Invalid_Reference,                           0x86,
                "invalid reference" )
  FT_ERRORDEF_( Debug_OpCode,                                0x87,
                "found debug opcode" )
  FT_ERRORDEF_( ENDF_In_Exec_Stream,                         0x88,
                "found ENDF opcode in execution stream" )
  FT_ERRORDEF_( Nested_DEFS,                                 0x89,
                "nested DEFS" )
  FT_ERRORDEF_( Invalid_CodeRange,                           0x8A,
                "invalid code range" )
  FT_ERRORDEF_( Execution_Too_Long,                          0x8B,
                "execution context too long" )
  FT_ERRORDEF_( Too_Many_Function_Defs,                      0x8C,
                "too many function definitions" )
  FT_ERRORDEF_( Too_Many_Instruction_Defs,                   0x8D,
                "too many instruction definitions" )
  FT_ERRORDEF_( Table_Missing,                               0x8E,
                "SFNT font table missing" )
  FT_ERRORDEF_( Horiz_Header_Missing,                        0x8F,
                "horizontal header (hhea) table missing" )
  FT_ERRORDEF_( Locations_Missing,                           0x90,
                "locations (loca) table missing" )
  FT_ERRORDEF_( Name_Table_Missing,                          0x91,
                "name table missing" )
  FT_ERRORDEF_( CMap_Table_Missing,                          0x92,
                "character map (cmap) table missing" )
  FT_ERRORDEF_( Hmtx_Table_Missing,                          0x93,
                "horizontal metrics (hmtx) table missing" )
  FT_ERRORDEF_( Post_Table_Missing,                          0x94,
                "PostScript (post) table missing" )
  FT_ERRORDEF_( Invalid_Horiz_Metrics,                       0x95,
                "invalid horizontal metrics" )
  FT_ERRORDEF_( Invalid_CharMap_Format,                      0x96,
                "invalid character map (cmap) format" )
  FT_ERRORDEF_( Invalid_PPem,                                0x97,
                "invalid ppem value" )
  FT_ERRORDEF_( Invalid_Vert_Metrics,                        0x98,
                "invalid vertical metrics" )
  FT_ERRORDEF_( Could_Not_Find_Context,                      0x99,
                "could not find context" )
  FT_ERRORDEF_( Invalid_Post_Table_Format,                   0x9A,
                "invalid PostScript (post) table format" )
  FT_ERRORDEF_( Invalid_Post_Table,                          0x9B,
                "invalid PostScript (post) table" )
  FT_ERRORDEF_( DEF_In_Glyf_Bytecode,                        0x9C,
                "found FDEF or IDEF opcode in glyf bytecode" )
  FT_ERRORDEF_( Missing_Bitmap,                              0x9D,
                "missing bitmap in strike" )
  FT_ERRORDEF_( Missing_SVG_Hooks,                           0x9E,
                "SVG hooks have not been set" )

  // CFF, CID, and Type 1 errors 

  FT_ERRORDEF_( Syntax_Error,                                0xA0,
                "opcode syntax error" )
  FT_ERRORDEF_( Stack_Underflow,                             0xA1,
                "argument stack underflow" )
  FT_ERRORDEF_( Ignore,                                      0xA2,
                "ignore" )
  FT_ERRORDEF_( No_Unicode_Glyph_Name,                       0xA3,
                "no Unicode glyph name found" )
  FT_ERRORDEF_( Glyph_Too_Big,                               0xA4,
                "glyph too big for hinting" )

  // BDF errors 

  FT_ERRORDEF_( Missing_Startfont_Field,                     0xB0,
                "`STARTFONT' field missing" )
  FT_ERRORDEF_( Missing_Font_Field,                          0xB1,
                "`FONT' field missing" )
  FT_ERRORDEF_( Missing_Size_Field,                          0xB2,
                "`SIZE' field missing" )
  FT_ERRORDEF_( Missing_Fontboundingbox_Field,               0xB3,
                "`FONTBOUNDINGBOX' field missing" )
  FT_ERRORDEF_( Missing_Chars_Field,                         0xB4,
                "`CHARS' field missing" )
  FT_ERRORDEF_( Missing_Startchar_Field,                     0xB5,
                "`STARTCHAR' field missing" )
  FT_ERRORDEF_( Missing_Encoding_Field,                      0xB6,
                "`ENCODING' field missing" )
  FT_ERRORDEF_( Missing_Bbx_Field,                           0xB7,
                "`BBX' field missing" )
  FT_ERRORDEF_( Bbx_Too_Big,                                 0xB8,
                "`BBX' too big" )
  FT_ERRORDEF_( Corrupted_Font_Header,                       0xB9,
                "Font header corrupted or missing fields" )
  FT_ERRORDEF_( Corrupted_Font_Glyphs,                       0xBA,
                "Font glyphs corrupted or missing fields" )

*/

  /* */


/* END */
//
// ===========================  ftrender.h  ===========================
//
/****************************************************************************
 *
 * ftrender.h
 *
 *   FreeType renderer modules public interface (specification).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTRENDER_H_
#define FTRENDER_H_


#include <freetype/ftmodapi.h>
#include <freetype/ftglyph.h>





  /**************************************************************************
   *
   * @section:
   *   module_management
   *
   */


  /* create a new glyph object */
  typedef FT_Error
  (*FT_Glyph_InitFunc)( FT_Glyph      glyph,
                        FT_GlyphSlot  slot );

  /* destroys a given glyph object */
  typedef void
  (*FT_Glyph_DoneFunc)( FT_Glyph  glyph );

  typedef void
  (*FT_Glyph_TransformFunc)( FT_Glyph          glyph,
                             const FT_Matrix*  matrix,
                             const FT_Vector*  delta );

  typedef void
  (*FT_Glyph_GetBBoxFunc)( FT_Glyph  glyph,
                           FT_BBox*  abbox );

  typedef FT_Error
  (*FT_Glyph_CopyFunc)( FT_Glyph   source,
                        FT_Glyph   target );

  typedef FT_Error
  (*FT_Glyph_PrepareFunc)( FT_Glyph      glyph,
                           FT_GlyphSlot  slot );

/* deprecated */
#define FT_Glyph_Init_Func       FT_Glyph_InitFunc
#define FT_Glyph_Done_Func       FT_Glyph_DoneFunc
#define FT_Glyph_Transform_Func  FT_Glyph_TransformFunc
#define FT_Glyph_BBox_Func       FT_Glyph_GetBBoxFunc
#define FT_Glyph_Copy_Func       FT_Glyph_CopyFunc
#define FT_Glyph_Prepare_Func    FT_Glyph_PrepareFunc


  struct  FT_Glyph_Class_
  {
    FT_Long                 glyph_size;
    FT_Glyph_Format         glyph_format;

    FT_Glyph_InitFunc       glyph_init;
    FT_Glyph_DoneFunc       glyph_done;
    FT_Glyph_CopyFunc       glyph_copy;
    FT_Glyph_TransformFunc  glyph_transform;
    FT_Glyph_GetBBoxFunc    glyph_bbox;
    FT_Glyph_PrepareFunc    glyph_prepare;
  };


  typedef FT_Error
  (*FT_Renderer_RenderFunc)( FT_Renderer       renderer,
                             FT_GlyphSlot      slot,
                             FT_Render_Mode    mode,
                             const FT_Vector*  origin );

  typedef FT_Error
  (*FT_Renderer_TransformFunc)( FT_Renderer       renderer,
                                FT_GlyphSlot      slot,
                                const FT_Matrix*  matrix,
                                const FT_Vector*  delta );


  typedef void
  (*FT_Renderer_GetCBoxFunc)( FT_Renderer   renderer,
                              FT_GlyphSlot  slot,
                              FT_BBox*      cbox );


  typedef FT_Error
  (*FT_Renderer_SetModeFunc)( FT_Renderer  renderer,
                              FT_ULong     mode_tag,
                              FT_Pointer   mode_ptr );

/* deprecated identifiers */
#define FTRenderer_render  FT_Renderer_RenderFunc
#define FTRenderer_transform  FT_Renderer_TransformFunc
#define FTRenderer_getCBox  FT_Renderer_GetCBoxFunc
#define FTRenderer_setMode  FT_Renderer_SetModeFunc


  /**************************************************************************
   *
   * @struct:
   *   FT_Renderer_Class
   *
   * @description:
   *   The renderer module class descriptor.
   *
   * @fields:
   *   root ::
   *     The root @FT_Module_Class fields.
   *
   *   glyph_format ::
   *     The glyph image format this renderer handles.
   *
   *   render_glyph ::
   *     A method used to render the image that is in a given glyph slot into
   *     a bitmap.
   *
   *   transform_glyph ::
   *     A method used to transform the image that is in a given glyph slot.
   *
   *   get_glyph_cbox ::
   *     A method used to access the glyph's cbox.
   *
   *   set_mode ::
   *     A method used to pass additional parameters.
   *
   *   raster_class ::
   *     For @FT_GLYPH_FORMAT_OUTLINE renderers only.  This is a pointer to
   *     its raster's class.
   */
  typedef struct  FT_Renderer_Class_
  {
    FT_Module_Class            root;

    FT_Glyph_Format            glyph_format;

    FT_Renderer_RenderFunc     render_glyph;
    FT_Renderer_TransformFunc  transform_glyph;
    FT_Renderer_GetCBoxFunc    get_glyph_cbox;
    FT_Renderer_SetModeFunc    set_mode;

    const FT_Raster_Funcs*     raster_class;

  } FT_Renderer_Class;


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Renderer
   *
   * @description:
   *   Retrieve the current renderer for a given glyph format.
   *
   * @input:
   *   library ::
   *     A handle to the library object.
   *
   *   format ::
   *     The glyph format.
   *
   * @return:
   *   A renderer handle.  0~if none found.
   *
   * @note:
   *   An error will be returned if a module already exists by that name, or
   *   if the module requires a version of FreeType that is too great.
   *
   *   To add a new renderer, simply use @FT_Add_Module.  To retrieve a
   *   renderer by its name, use @FT_Get_Module.
   */
   FT_Renderer 
  FT_Get_Renderer( FT_Library       library,
                   FT_Glyph_Format  format );


  /**************************************************************************
   *
   * @function:
   *   FT_Set_Renderer
   *
   * @description:
   *   Set the current renderer to use, and set additional mode.
   *
   * @inout:
   *   library ::
   *     A handle to the library object.
   *
   * @input:
   *   renderer ::
   *     A handle to the renderer object.
   *
   *   num_params ::
   *     The number of additional parameters.
   *
   *   parameters ::
   *     Additional parameters.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   In case of success, the renderer will be used to convert glyph images
   *   in the renderer's known format into bitmaps.
   *
   *   This doesn't change the current renderer for other formats.
   *
   *   Currently, no FreeType renderer module uses `parameters`; you should
   *   thus always pass `NULL` as the value.
   */
   FT_Error 
  FT_Set_Renderer( FT_Library     library,
                   FT_Renderer    renderer,
                   FT_UInt        num_params,
                   FT_Parameter*  parameters );

  /* */




#endif /* FTRENDER_H_ */


/* END */
//
// ===========================  ftsynth.h  ===========================
//
/****************************************************************************
 *
 * ftsynth.h
 *
 *   FreeType synthesizing code for emboldening and slanting
 *   (specification).
 *
 * Copyright (C) 2000-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


  /*************************************************************************/
  /*************************************************************************/
  /*************************************************************************/
  /*************************************************************************/
  /*************************************************************************/
  /*********                                                       *********/
  /*********        WARNING, THIS IS ALPHA CODE!  THIS API         *********/
  /*********    IS DUE TO CHANGE UNTIL STRICTLY NOTIFIED BY THE    *********/
  /*********            FREETYPE DEVELOPMENT TEAM                  *********/
  /*********                                                       *********/
  /*************************************************************************/
  /*************************************************************************/
  /*************************************************************************/
  /*************************************************************************/
  /*************************************************************************/


  /* Main reason for not lifting the functions in this module to a  */
  /* 'standard' API is that the used parameters for emboldening and */
  /* slanting are not configurable.  Consider the functions as a    */
  /* code resource that should be copied into the application and   */
  /* adapted to the particular needs.                               */


#ifndef FTSYNTH_H_
#define FTSYNTH_H_


#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif




  /* Embolden a glyph by a 'reasonable' value (which is highly a matter of */
  /* taste).  This function is actually a convenience function, providing  */
  /* a wrapper for @FT_Outline_Embolden and @FT_Bitmap_Embolden.           */
  /*                                                                       */
  /* For emboldened outlines the height, width, and advance metrics are    */
  /* increased by the strength of the emboldening -- this even affects     */
  /* mono-width fonts!                                                     */
  /*                                                                       */
  /* You can also call @FT_Outline_Get_CBox to get precise values.         */
   void 
  FT_GlyphSlot_Embolden( FT_GlyphSlot  slot );

  /* Precisely adjust the glyph weight either horizontally or vertically.  */
  /* The `xdelta` and `ydelta` values are fractions of the face Em size    */
  /* (in fixed-point format).  Considering that a regular face would have  */
  /* stem widths on the order of 0.1 Em, a delta of 0.05 (0x0CCC) should   */
  /* be very noticeable.  To increase or decrease the weight, use positive */
  /* or negative values, respectively.                                     */
   void 
  FT_GlyphSlot_AdjustWeight( FT_GlyphSlot  slot,
                             FT_Fixed      xdelta,
                             FT_Fixed      ydelta );


  /* Slant an outline glyph to the right by about 12 degrees.              */
   void 
  FT_GlyphSlot_Oblique( FT_GlyphSlot  slot );

  /* Slant an outline glyph by a given sine of an angle.  You can apply    */
  /* slant along either x- or y-axis by choosing a corresponding non-zero  */
  /* argument.  If both slants are non-zero, some affine transformation    */
  /* will result.                                                          */
   void 
  FT_GlyphSlot_Slant( FT_GlyphSlot  slot,
                      FT_Fixed      xslant,
                      FT_Fixed      yslant );

  /* */




#endif /* FTSYNTH_H_ */


/* END */
//
// ===========================  ftglyph.h  ===========================
//
/****************************************************************************
 *
 * ftglyph.h
 *
 *   FreeType convenience functions to handle glyphs (specification).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


  /**************************************************************************
   *
   * This file contains the definition of several convenience functions that
   * can be used by client applications to easily retrieve glyph bitmaps and
   * outlines from a given face.
   *
   * These functions should be optional if you are writing a font server or
   * text layout engine on top of FreeType.  However, they are pretty handy
   * for many other simple uses of the library.
   *
   */


#ifndef FTGLYPH_H_
#define FTGLYPH_H_


#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *   glyph_management
   *
   * @title:
   *   Glyph Management
   *
   * @abstract:
   *   Generic interface to manage individual glyph data.
   *
   * @description:
   *   This section contains definitions used to manage glyph data through
   *   generic @FT_Glyph objects.  Each of them can contain a bitmap,
   *   a vector outline, or even images in other formats.  These objects are
   *   detached from @FT_Face, contrary to @FT_GlyphSlot.
   *
   */


  /* forward declaration to a private type */
  typedef struct FT_Glyph_Class_  FT_Glyph_Class;


  /**************************************************************************
   *
   * @type:
   *   FT_Glyph
   *
   * @description:
   *   Handle to an object used to model generic glyph images.  It is a
   *   pointer to the @FT_GlyphRec structure and can contain a glyph bitmap
   *   or pointer.
   *
   * @note:
   *   Glyph objects are not owned by the library.  You must thus release
   *   them manually (through @FT_Done_Glyph) _before_ calling
   *   @FT_Done_FreeType.
   */
  typedef struct FT_GlyphRec_*  FT_Glyph;


  /**************************************************************************
   *
   * @struct:
   *   FT_GlyphRec
   *
   * @description:
   *   The root glyph structure contains a given glyph image plus its advance
   *   width in 16.16 fixed-point format.
   *
   * @fields:
   *   library ::
   *     A handle to the FreeType library object.
   *
   *   clazz ::
   *     A pointer to the glyph's class.  Private.
   *
   *   format ::
   *     The format of the glyph's image.
   *
   *   advance ::
   *     A 16.16 vector that gives the glyph's advance width.
   */
  typedef struct  FT_GlyphRec_
  {
    FT_Library             library;
    const FT_Glyph_Class*  clazz;
    FT_Glyph_Format        format;
    FT_Vector              advance;

  } FT_GlyphRec;


  /**************************************************************************
   *
   * @type:
   *   FT_BitmapGlyph
   *
   * @description:
   *   A handle to an object used to model a bitmap glyph image.  This is a
   *   'sub-class' of @FT_Glyph, and a pointer to @FT_BitmapGlyphRec.
   */
  typedef struct FT_BitmapGlyphRec_*  FT_BitmapGlyph;


  /**************************************************************************
   *
   * @struct:
   *   FT_BitmapGlyphRec
   *
   * @description:
   *   A structure used for bitmap glyph images.  This really is a
   *   'sub-class' of @FT_GlyphRec.
   *
   * @fields:
   *   root ::
   *     The root fields of @FT_Glyph.
   *
   *   left ::
   *     The left-side bearing, i.e., the horizontal distance from the
   *     current pen position to the left border of the glyph bitmap.
   *
   *   top ::
   *     The top-side bearing, i.e., the vertical distance from the current
   *     pen position to the top border of the glyph bitmap.  This distance
   *     is positive for upwards~y!
   *
   *   bitmap ::
   *     A descriptor for the bitmap.
   *
   * @note:
   *   You can typecast an @FT_Glyph to @FT_BitmapGlyph if you have
   *   `glyph->format == FT_GLYPH_FORMAT_BITMAP`.  This lets you access the
   *   bitmap's contents easily.
   *
   *   The corresponding pixel buffer is always owned by @FT_BitmapGlyph and
   *   is thus created and destroyed with it.
   */
  typedef struct  FT_BitmapGlyphRec_
  {
    FT_GlyphRec  root;
    FT_Int       left;
    FT_Int       top;
    FT_Bitmap    bitmap;

  } FT_BitmapGlyphRec;


  /**************************************************************************
   *
   * @type:
   *   FT_OutlineGlyph
   *
   * @description:
   *   A handle to an object used to model an outline glyph image.  This is a
   *   'sub-class' of @FT_Glyph, and a pointer to @FT_OutlineGlyphRec.
   */
  typedef struct FT_OutlineGlyphRec_*  FT_OutlineGlyph;


  /**************************************************************************
   *
   * @struct:
   *   FT_OutlineGlyphRec
   *
   * @description:
   *   A structure used for outline (vectorial) glyph images.  This really is
   *   a 'sub-class' of @FT_GlyphRec.
   *
   * @fields:
   *   root ::
   *     The root @FT_Glyph fields.
   *
   *   outline ::
   *     A descriptor for the outline.
   *
   * @note:
   *   You can typecast an @FT_Glyph to @FT_OutlineGlyph if you have
   *   `glyph->format == FT_GLYPH_FORMAT_OUTLINE`.  This lets you access the
   *   outline's content easily.
   *
   *   As the outline is extracted from a glyph slot, its coordinates are
   *   expressed normally in 26.6 pixels, unless the flag @FT_LOAD_NO_SCALE
   *   was used in @FT_Load_Glyph or @FT_Load_Char.
   *
   *   The outline's tables are always owned by the object and are destroyed
   *   with it.
   */
  typedef struct  FT_OutlineGlyphRec_
  {
    FT_GlyphRec  root;
    FT_Outline   outline;

  } FT_OutlineGlyphRec;


  /**************************************************************************
   *
   * @type:
   *   FT_SvgGlyph
   *
   * @description:
   *   A handle to an object used to model an SVG glyph.  This is a
   *   'sub-class' of @FT_Glyph, and a pointer to @FT_SvgGlyphRec.
   *
   * @since:
   *   2.12
   */
  typedef struct FT_SvgGlyphRec_*  FT_SvgGlyph;


  /**************************************************************************
   *
   * @struct:
   *   FT_SvgGlyphRec
   *
   * @description:
   *   A structure used for OT-SVG glyphs.  This is a 'sub-class' of
   *   @FT_GlyphRec.
   *
   * @fields:
   *   root ::
   *     The root @FT_GlyphRec fields.
   *
   *   svg_document ::
   *     A pointer to the SVG document.
   *
   *   svg_document_length ::
   *     The length of `svg_document`.
   *
   *   glyph_index ::
   *     The index of the glyph to be rendered.
   *
   *   metrics ::
   *     A metrics object storing the size information.
   *
   *   units_per_EM ::
   *     The size of the EM square.
   *
   *   start_glyph_id ::
   *     The first glyph ID in the glyph range covered by this document.
   *
   *   end_glyph_id ::
   *     The last glyph ID in the glyph range covered by this document.
   *
   *   transform ::
   *     A 2x2 transformation matrix to apply to the glyph while rendering
   *     it.
   *
   *   delta ::
   *     Translation to apply to the glyph while rendering.
   *
   * @note:
   *   The Glyph Management API requires @FT_Glyph or its 'sub-class' to have
   *   all the information needed to completely define the glyph's rendering.
   *   Outline-based glyphs can directly apply transformations to the outline
   *   but this is not possible for an SVG document that hasn't been parsed.
   *   Therefore, the transformation is stored along with the document.  In
   *   the absence of a 'ViewBox' or 'Width'/'Height' attribute, the size of
   *   the ViewPort should be assumed to be 'units_per_EM'.
   */
  typedef struct  FT_SvgGlyphRec_
  {
    FT_GlyphRec  root;

    FT_Byte*  svg_document;
    FT_ULong  svg_document_length;

    FT_UInt  glyph_index;

    FT_Size_Metrics  metrics;
    FT_UShort        units_per_EM;

    FT_UShort  start_glyph_id;
    FT_UShort  end_glyph_id;

    FT_Matrix  transform;
    FT_Vector  delta;

  } FT_SvgGlyphRec;


  /**************************************************************************
   *
   * @function:
   *   FT_New_Glyph
   *
   * @description:
   *   A function used to create a new empty glyph image.  Note that the
   *   created @FT_Glyph object must be released with @FT_Done_Glyph.
   *
   * @input:
   *   library ::
   *     A handle to the FreeType library object.
   *
   *   format ::
   *     The format of the glyph's image.
   *
   * @output:
   *   aglyph ::
   *     A handle to the glyph object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @since:
   *   2.10
   */
   FT_Error 
  FT_New_Glyph( FT_Library       library,
                FT_Glyph_Format  format,
                FT_Glyph         *aglyph );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Glyph
   *
   * @description:
   *   A function used to extract a glyph image from a slot.  Note that the
   *   created @FT_Glyph object must be released with @FT_Done_Glyph.
   *
   * @input:
   *   slot ::
   *     A handle to the source glyph slot.
   *
   * @output:
   *   aglyph ::
   *     A handle to the glyph object.  `NULL` in case of error.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   Because `*aglyph->advance.x` and `*aglyph->advance.y` are 16.16
   *   fixed-point numbers, `slot->advance.x` and `slot->advance.y` (which
   *   are in 26.6 fixed-point format) must be in the range ]-32768;32768[.
   */
   FT_Error 
  FT_Get_Glyph( FT_GlyphSlot  slot,
                FT_Glyph     *aglyph );


  /**************************************************************************
   *
   * @function:
   *   FT_Glyph_Copy
   *
   * @description:
   *   A function used to copy a glyph image.  Note that the created
   *   @FT_Glyph object must be released with @FT_Done_Glyph.
   *
   * @input:
   *   source ::
   *     A handle to the source glyph object.
   *
   * @output:
   *   target ::
   *     A handle to the target glyph object.  `NULL` in case of error.
   *
   * @return:
   *   FreeType error code.  0~means success.
   */
   FT_Error 
  FT_Glyph_Copy( FT_Glyph   source,
                 FT_Glyph  *target );


  /**************************************************************************
   *
   * @function:
   *   FT_Glyph_Transform
   *
   * @description:
   *   Transform a glyph image if its format is scalable.
   *
   * @inout:
   *   glyph ::
   *     A handle to the target glyph object.
   *
   * @input:
   *   matrix ::
   *     A pointer to a 2x2 matrix to apply.
   *
   *   delta ::
   *     A pointer to a 2d vector to apply.  Coordinates are expressed in
   *     1/64 of a pixel.
   *
   * @return:
   *   FreeType error code (if not 0, the glyph format is not scalable).
   *
   * @note:
   *   The 2x2 transformation matrix is also applied to the glyph's advance
   *   vector.
   */
   FT_Error 
  FT_Glyph_Transform( FT_Glyph          glyph,
                      const FT_Matrix*  matrix,
                      const FT_Vector*  delta );


  /**************************************************************************
   *
   * @enum:
   *   FT_Glyph_BBox_Mode
   *
   * @description:
   *   The mode how the values of @FT_Glyph_Get_CBox are returned.
   *
   * @values:
   *   FT_GLYPH_BBOX_UNSCALED ::
   *     Return unscaled font units.
   *
   *   FT_GLYPH_BBOX_SUBPIXELS ::
   *     Return unfitted 26.6 coordinates.
   *
   *   FT_GLYPH_BBOX_GRIDFIT ::
   *     Return grid-fitted 26.6 coordinates.
   *
   *   FT_GLYPH_BBOX_TRUNCATE ::
   *     Return coordinates in integer pixels.
   *
   *   FT_GLYPH_BBOX_PIXELS ::
   *     Return grid-fitted pixel coordinates.
   */
  typedef enum  FT_Glyph_BBox_Mode_
  {
    FT_GLYPH_BBOX_UNSCALED  = 0,
    FT_GLYPH_BBOX_SUBPIXELS = 0,
    FT_GLYPH_BBOX_GRIDFIT   = 1,
    FT_GLYPH_BBOX_TRUNCATE  = 2,
    FT_GLYPH_BBOX_PIXELS    = 3

  } FT_Glyph_BBox_Mode;


  /* these constants are deprecated; use the corresponding */
  /* `FT_Glyph_BBox_Mode` values instead                   */
#define ft_glyph_bbox_unscaled   FT_GLYPH_BBOX_UNSCALED
#define ft_glyph_bbox_subpixels  FT_GLYPH_BBOX_SUBPIXELS
#define ft_glyph_bbox_gridfit    FT_GLYPH_BBOX_GRIDFIT
#define ft_glyph_bbox_truncate   FT_GLYPH_BBOX_TRUNCATE
#define ft_glyph_bbox_pixels     FT_GLYPH_BBOX_PIXELS


  /**************************************************************************
   *
   * @function:
   *   FT_Glyph_Get_CBox
   *
   * @description:
   *   Return a glyph's 'control box'.  The control box encloses all the
   *   outline's points, including Bezier control points.  Though it
   *   coincides with the exact bounding box for most glyphs, it can be
   *   slightly larger in some situations (like when rotating an outline that
   *   contains Bezier outside arcs).
   *
   *   Computing the control box is very fast, while getting the bounding box
   *   can take much more time as it needs to walk over all segments and arcs
   *   in the outline.  To get the latter, you can use the 'ftbbox'
   *   component, which is dedicated to this single task.
   *
   * @input:
   *   glyph ::
   *     A handle to the source glyph object.
   *
   *   mode ::
   *     The mode that indicates how to interpret the returned bounding box
   *     values.
   *
   * @output:
   *   acbox ::
   *     The glyph coordinate bounding box.  Coordinates are expressed in
   *     1/64 of pixels if it is grid-fitted.
   *
   * @note:
   *   Coordinates are relative to the glyph origin, using the y~upwards
   *   convention.
   *
   *   If the glyph has been loaded with @FT_LOAD_NO_SCALE, `bbox_mode` must
   *   be set to @FT_GLYPH_BBOX_UNSCALED to get unscaled font units in 26.6
   *   pixel format.  The value @FT_GLYPH_BBOX_SUBPIXELS is another name for
   *   this constant.
   *
   *   If the font is tricky and the glyph has been loaded with
   *   @FT_LOAD_NO_SCALE, the resulting CBox is meaningless.  To get
   *   reasonable values for the CBox it is necessary to load the glyph at a
   *   large ppem value (so that the hinting instructions can properly shift
   *   and scale the subglyphs), then extracting the CBox, which can be
   *   eventually converted back to font units.
   *
   *   Note that the maximum coordinates are exclusive, which means that one
   *   can compute the width and height of the glyph image (be it in integer
   *   or 26.6 pixels) as:
   *
   *   ```
   *     width  = bbox.xMax - bbox.xMin;
   *     height = bbox.yMax - bbox.yMin;
   *   ```
   *
   *   Note also that for 26.6 coordinates, if `bbox_mode` is set to
   *   @FT_GLYPH_BBOX_GRIDFIT, the coordinates will also be grid-fitted,
   *   which corresponds to:
   *
   *   ```
   *     bbox.xMin = FLOOR(bbox.xMin);
   *     bbox.yMin = FLOOR(bbox.yMin);
   *     bbox.xMax = CEILING(bbox.xMax);
   *     bbox.yMax = CEILING(bbox.yMax);
   *   ```
   *
   *   To get the bbox in pixel coordinates, set `bbox_mode` to
   *   @FT_GLYPH_BBOX_TRUNCATE.
   *
   *   To get the bbox in grid-fitted pixel coordinates, set `bbox_mode` to
   *   @FT_GLYPH_BBOX_PIXELS.
   */
   void 
  FT_Glyph_Get_CBox( FT_Glyph  glyph,
                     FT_UInt   bbox_mode,
                     FT_BBox  *acbox );


  /**************************************************************************
   *
   * @function:
   *   FT_Glyph_To_Bitmap
   *
   * @description:
   *   Convert a given glyph object to a bitmap glyph object.
   *
   * @inout:
   *   the_glyph ::
   *     A pointer to a handle to the target glyph.
   *
   * @input:
   *   render_mode ::
   *     An enumeration that describes how the data is rendered.
   *
   *   origin ::
   *     A pointer to a vector used to translate the glyph image before
   *     rendering.  Can be~0 (if no translation).  The origin is expressed
   *     in 26.6 pixels.
   *
   *   destroy ::
   *     A boolean that indicates that the original glyph image should be
   *     destroyed by this function.  It is never destroyed in case of error.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function does nothing if the glyph format isn't scalable.
   *
   *   The glyph image is translated with the `origin` vector before
   *   rendering.
   *
   *   The first parameter is a pointer to an @FT_Glyph handle that will be
   *   _replaced_ by this function (with newly allocated data).  Typically,
   *   you would do something like the following (omitting error handling).
   *
   *   ```
   *     FT_Glyph        glyph;
   *     FT_BitmapGlyph  glyph_bitmap;
   *
   *
   *     // load glyph
   *     error = FT_Load_Char( face, glyph_index, FT_LOAD_DEFAULT );
   *
   *     // extract glyph image
   *     error = FT_Get_Glyph( face->glyph, &glyph );
   *
   *     // convert to a bitmap (default render mode + destroying old)
   *     if ( glyph->format != FT_GLYPH_FORMAT_BITMAP )
   *     {
   *       error = FT_Glyph_To_Bitmap( &glyph, FT_RENDER_MODE_NORMAL,
   *                                   0, 1 );
   *       if ( error ) // `glyph' unchanged
   *         ...
   *     }
   *
   *     // access bitmap content by typecasting
   *     glyph_bitmap = (FT_BitmapGlyph)glyph;
   *
   *     // do funny stuff with it, like blitting/drawing
   *     ...
   *
   *     // discard glyph image (bitmap or not)
   *     FT_Done_Glyph( glyph );
   *   ```
   *
   *   Here is another example, again without error handling.
   *
   *   ```
   *     FT_Glyph  glyphs[MAX_GLYPHS]
   *
   *
   *     ...
   *
   *     for ( idx = 0; i < MAX_GLYPHS; i++ )
   *       error = FT_Load_Glyph( face, idx, FT_LOAD_DEFAULT ) ||
   *               FT_Get_Glyph ( face->glyph, &glyphs[idx] );
   *
   *     ...
   *
   *     for ( idx = 0; i < MAX_GLYPHS; i++ )
   *     {
   *       FT_Glyph  bitmap = glyphs[idx];
   *
   *
   *       ...
   *
   *       // after this call, `bitmap' no longer points into
   *       // the `glyphs' array (and the old value isn't destroyed)
   *       FT_Glyph_To_Bitmap( &bitmap, FT_RENDER_MODE_MONO, 0, 0 );
   *
   *       ...
   *
   *       FT_Done_Glyph( bitmap );
   *     }
   *
   *     ...
   *
   *     for ( idx = 0; i < MAX_GLYPHS; i++ )
   *       FT_Done_Glyph( glyphs[idx] );
   *   ```
   */
   FT_Error 
  FT_Glyph_To_Bitmap( FT_Glyph*         the_glyph,
                      FT_Render_Mode    render_mode,
                      const FT_Vector*  origin,
                      FT_Bool           destroy );


  /**************************************************************************
   *
   * @function:
   *   FT_Done_Glyph
   *
   * @description:
   *   Destroy a given glyph.
   *
   * @input:
   *   glyph ::
   *     A handle to the target glyph object.  Can be `NULL`.
   */
   void 
  FT_Done_Glyph( FT_Glyph  glyph );

  /* */


  /* other helpful functions */

  /**************************************************************************
   *
   * @section:
   *   computations
   *
   */


  /**************************************************************************
   *
   * @function:
   *   FT_Matrix_Multiply
   *
   * @description:
   *   Perform the matrix operation `b = a*b`.
   *
   * @input:
   *   a ::
   *     A pointer to matrix `a`.
   *
   * @inout:
   *   b ::
   *     A pointer to matrix `b`.
   *
   * @note:
   *   The result is undefined if either `a` or `b` is zero.
   *
   *   Since the function uses wrap-around arithmetic, results become
   *   meaningless if the arguments are very large.
   */
   void 
  FT_Matrix_Multiply( const FT_Matrix*  a,
                      FT_Matrix*        b );


  /**************************************************************************
   *
   * @function:
   *   FT_Matrix_Invert
   *
   * @description:
   *   Invert a 2x2 matrix.  Return an error if it can't be inverted.
   *
   * @inout:
   *   matrix ::
   *     A pointer to the target matrix.  Remains untouched in case of error.
   *
   * @return:
   *   FreeType error code.  0~means success.
   */
   FT_Error 
  FT_Matrix_Invert( FT_Matrix*  matrix );

  /* */




#endif /* FTGLYPH_H_ */


/* END */


/* Local Variables: */
/* coding: utf-8    */
/* End:             */
//
// ===========================  freetype.h  ===========================
//
/****************************************************************************
 *
 * freetype.h
 *
 *   FreeType high-level API and common types (specification only).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FREETYPE_H_
#define FREETYPE_H_


#include <ft2build.h>
#include FT_CONFIG_CONFIG_H
#include <freetype/fttypes.h>
#include <freetype/fterrors.h>






  /**************************************************************************
   *
   * @section:
   *   preamble
   *
   * @title:
   *   Preamble
   *
   * @abstract:
   *   What FreeType is and isn't
   *
   * @description:
   *   FreeType is a library that provides access to glyphs in font files.  It
   *   scales the glyph images and their metrics to a requested size, and it
   *   rasterizes the glyph images to produce pixel or subpixel alpha coverage
   *   bitmaps.
   *
   *   Note that FreeType is _not_ a text layout engine.  You have to use
   *   higher-level libraries like HarfBuzz, Pango, or ICU for that.
   *
   *   Note also that FreeType does _not_ perform alpha blending or
   *   compositing the resulting bitmaps or pixmaps by itself.  Use your
   *   favourite graphics library (for example, Cairo or Skia) to further
   *   process FreeType's output.
   *
   */


  /**************************************************************************
   *
   * @section:
   *   header_inclusion
   *
   * @title:
   *   FreeType's header inclusion scheme
   *
   * @abstract:
   *   How client applications should include FreeType header files.
   *
   * @description:
   *   To be as flexible as possible (and for historical reasons), you must
   *   load file `ft2build.h` first before other header files, for example
   *
   *   ```
   *     #include <ft2build.h>
   *
   *     #include <freetype/freetype.h>
   *     #include <freetype/ftoutln.h>
   *   ```
   */


  /**************************************************************************
   *
   * @section:
   *   user_allocation
   *
   * @title:
   *   User allocation
   *
   * @abstract:
   *   How client applications should allocate FreeType data structures.
   *
   * @description:
   *   FreeType assumes that structures allocated by the user and passed as
   *   arguments are zeroed out except for the actual data.  In other words,
   *   it is recommended to use `calloc` (or variants of it) instead of
   *   `malloc` for allocation.
   *
   */


  /**************************************************************************
   *
   * @section:
   *   font_testing_macros
   *
   * @title:
   *   Font Testing Macros
   *
   * @abstract:
   *   Macros to test various properties of fonts.
   *
   * @description:
   *   Macros to test the most important font properties.
   *
   *   It is recommended to use these high-level macros instead of directly
   *   testing the corresponding flags, which are scattered over various
   *   structures.
   *
   * @order:
   *   FT_HAS_HORIZONTAL
   *   FT_HAS_VERTICAL
   *   FT_HAS_KERNING
   *   FT_HAS_FIXED_SIZES
   *   FT_HAS_GLYPH_NAMES
   *   FT_HAS_COLOR
   *   FT_HAS_MULTIPLE_MASTERS
   *   FT_HAS_SVG
   *   FT_HAS_SBIX
   *   FT_HAS_SBIX_OVERLAY
   *
   *   FT_IS_SFNT
   *   FT_IS_SCALABLE
   *   FT_IS_FIXED_WIDTH
   *   FT_IS_CID_KEYED
   *   FT_IS_TRICKY
   *   FT_IS_NAMED_INSTANCE
   *   FT_IS_VARIATION
   *
   */


  /**************************************************************************
   *
   * @section:
   *   library_setup
   *
   * @title:
   *   Library Setup
   *
   * @abstract:
   *   Functions to start and end the usage of the FreeType library.
   *
   * @description:
   *   Functions to start and end the usage of the FreeType library.
   *
   *   Note that @FT_Library_Version and @FREETYPE_XXX are of limited use
   *   because even a new release of FreeType with only documentation
   *   changes increases the version number.
   *
   * @order:
   *   FT_Library
   *   FT_Init_FreeType
   *   FT_Done_FreeType
   *
   *   FT_Library_Version
   *   FREETYPE_XXX
   *
   */


  /**************************************************************************
   *
   * @section:
   *   face_creation
   *
   * @title:
   *   Face Creation
   *
   * @abstract:
   *   Functions to manage fonts.
   *
   * @description:
   *   The functions and structures collected in this section operate on
   *   fonts globally.
   *
   * @order:
   *   FT_Face
   *   FT_FaceRec
   *   FT_FACE_FLAG_XXX
   *   FT_STYLE_FLAG_XXX
   *
   *   FT_New_Face
   *   FT_Done_Face
   *   FT_Reference_Face
   *   FT_New_Memory_Face
   *   FT_Face_Properties
   *   FT_Open_Face
   *   FT_Open_Args
   *   FT_OPEN_XXX
   *   FT_Parameter
   *   FT_Attach_File
   *   FT_Attach_Stream
   *
   */


  /**************************************************************************
   *
   * @section:
   *   sizing_and_scaling
   *
   * @title:
   *   Sizing and Scaling
   *
   * @abstract:
   *   Functions to manage font sizes.
   *
   * @description:
   *   The functions and structures collected in this section are related to
   *   selecting and manipulating the size of a font globally.
   *
   * @order:
   *   FT_Size
   *   FT_SizeRec
   *   FT_Size_Metrics
   *
   *   FT_Bitmap_Size
   *
   *   FT_Set_Char_Size
   *   FT_Set_Pixel_Sizes
   *   FT_Request_Size
   *   FT_Select_Size
   *   FT_Size_Request_Type
   *   FT_Size_RequestRec
   *   FT_Size_Request
   *
   *   FT_Set_Transform
   *   FT_Get_Transform
   *
   */


  /**************************************************************************
   *
   * @section:
   *   glyph_retrieval
   *
   * @title:
   *   Glyph Retrieval
   *
   * @abstract:
   *   Functions to manage glyphs.
   *
   * @description:
   *   The functions and structures collected in this section operate on
   *   single glyphs, of which @FT_Load_Glyph is most important.
   *
   * @order:
   *   FT_GlyphSlot
   *   FT_GlyphSlotRec
   *   FT_Glyph_Metrics
   *
   *   FT_Load_Glyph
   *   FT_LOAD_XXX
   *   FT_LOAD_TARGET_MODE
   *   FT_LOAD_TARGET_XXX
   *
   *   FT_Render_Glyph
   *   FT_Render_Mode
   *   FT_Get_Kerning
   *   FT_Kerning_Mode
   *   FT_Get_Track_Kerning
   *
   */


  /**************************************************************************
   *
   * @section:
   *   character_mapping
   *
   * @title:
   *   Character Mapping
   *
   * @abstract:
   *   Functions to manage character-to-glyph maps.
   *
   * @description:
   *   This section holds functions and structures that are related to
   *   mapping character input codes to glyph indices.
   *
   *   Note that for many scripts the simplistic approach used by FreeType
   *   of mapping a single character to a single glyph is not valid or
   *   possible!  In general, a higher-level library like HarfBuzz or ICU
   *   should be used for handling text strings.
   *
   * @order:
   *   FT_CharMap
   *   FT_CharMapRec
   *   FT_Encoding
   *   FT_ENC_TAG
   *
   *   FT_Select_Charmap
   *   FT_Set_Charmap
   *   FT_Get_Charmap_Index
   *
   *   FT_Get_Char_Index
   *   FT_Get_First_Char
   *   FT_Get_Next_Char
   *   FT_Load_Char
   *
   */


  /**************************************************************************
   *
   * @section:
   *   information_retrieval
   *
   * @title:
   *   Information Retrieval
   *
   * @abstract:
   *   Functions to retrieve font and glyph information.
   *
   * @description:
   *   Functions to retrieve font and glyph information.  Only some very
   *   basic data is covered; see also the chapter on the format-specific
   *   API for more.
   *
   *
   * @order:
   *   FT_Get_Name_Index
   *   FT_Get_Glyph_Name
   *   FT_Get_Postscript_Name
   *   FT_Get_FSType_Flags
   *   FT_FSTYPE_XXX
   *   FT_Get_SubGlyph_Info
   *   FT_SUBGLYPH_FLAG_XXX
   *
   */


  /**************************************************************************
   *
   * @section:
   *   other_api_data
   *
   * @title:
   *   Other API Data
   *
   * @abstract:
   *   Other structures, enumerations, and macros.
   *
   * @description:
   *   Other structures, enumerations, and macros.  Deprecated functions are
   *   also listed here.
   *
   * @order:
   *   FT_Face_Internal
   *   FT_Size_Internal
   *   FT_Slot_Internal
   *
   *   FT_SubGlyph
   *
   *   FT_HAS_FAST_GLYPHS
   *   FT_Face_CheckTrueTypePatents
   *   FT_Face_SetUnpatentedHinting
   *
   */


  /*************************************************************************/
  /*************************************************************************/
  /*                                                                       */
  /*                        B A S I C   T Y P E S                          */
  /*                                                                       */
  /*************************************************************************/
  /*************************************************************************/


  /**************************************************************************
   *
   * @section:
   *   glyph_retrieval
   *
   */

  /**************************************************************************
   *
   * @struct:
   *   FT_Glyph_Metrics
   *
   * @description:
   *   A structure to model the metrics of a single glyph.  The values are
   *   expressed in 26.6 fractional pixel format; if the flag
   *   @FT_LOAD_NO_SCALE has been used while loading the glyph, values are
   *   expressed in font units instead.
   *
   * @fields:
   *   width ::
   *     The glyph's width.
   *
   *   height ::
   *     The glyph's height.
   *
   *   horiBearingX ::
   *     Left side bearing for horizontal layout.
   *
   *   horiBearingY ::
   *     Top side bearing for horizontal layout.
   *
   *   horiAdvance ::
   *     Advance width for horizontal layout.
   *
   *   vertBearingX ::
   *     Left side bearing for vertical layout.
   *
   *   vertBearingY ::
   *     Top side bearing for vertical layout.  Larger positive values mean
   *     further below the vertical glyph origin.
   *
   *   vertAdvance ::
   *     Advance height for vertical layout.  Positive values mean the glyph
   *     has a positive advance downward.
   *
   * @note:
   *   If not disabled with @FT_LOAD_NO_HINTING, the values represent
   *   dimensions of the hinted glyph (in case hinting is applicable).
   *
   *   Stroking a glyph with an outside border does not increase
   *   `horiAdvance` or `vertAdvance`; you have to manually adjust these
   *   values to account for the added width and height.
   *
   *   FreeType doesn't use the 'VORG' table data for CFF fonts because it
   *   doesn't have an interface to quickly retrieve the glyph height.  The
   *   y~coordinate of the vertical origin can be simply computed as
   *   `vertBearingY + height` after loading a glyph.
   */
  typedef struct  FT_Glyph_Metrics_
  {
    FT_Pos  width;
    FT_Pos  height;

    FT_Pos  horiBearingX;
    FT_Pos  horiBearingY;
    FT_Pos  horiAdvance;

    FT_Pos  vertBearingX;
    FT_Pos  vertBearingY;
    FT_Pos  vertAdvance;

  } FT_Glyph_Metrics;


  /**************************************************************************
   *
   * @section:
   *   sizing_and_scaling
   *
   */

  /**************************************************************************
   *
   * @struct:
   *   FT_Bitmap_Size
   *
   * @description:
   *   This structure models the metrics of a bitmap strike (i.e., a set of
   *   glyphs for a given point size and resolution) in a bitmap font.  It is
   *   used for the `available_sizes` field of @FT_Face.
   *
   * @fields:
   *   height ::
   *     The vertical distance, in pixels, between two consecutive baselines.
   *     It is always positive.
   *
   *   width ::
   *     The average width, in pixels, of all glyphs in the strike.
   *
   *   size ::
   *     The nominal size of the strike in 26.6 fractional points.  This
   *     field is not very useful.
   *
   *   x_ppem ::
   *     The horizontal ppem (nominal width) in 26.6 fractional pixels.
   *
   *   y_ppem ::
   *     The vertical ppem (nominal height) in 26.6 fractional pixels.
   *
   * @note:
   *   Windows FNT:
   *     The nominal size given in a FNT font is not reliable.  If the driver
   *     finds it incorrect, it sets `size` to some calculated values, and
   *     `x_ppem` and `y_ppem` to the pixel width and height given in the
   *     font, respectively.
   *
   *   TrueType embedded bitmaps:
   *     `size`, `width`, and `height` values are not contained in the bitmap
   *     strike itself.  They are computed from the global font parameters.
   */
  typedef struct  FT_Bitmap_Size_
  {
    FT_Short  height;
    FT_Short  width;

    FT_Pos    size;

    FT_Pos    x_ppem;
    FT_Pos    y_ppem;

  } FT_Bitmap_Size;


  /*************************************************************************/
  /*************************************************************************/
  /*                                                                       */
  /*                     O B J E C T   C L A S S E S                       */
  /*                                                                       */
  /*************************************************************************/
  /*************************************************************************/

  /**************************************************************************
   *
   * @section:
   *   library_setup
   *
   */

  /**************************************************************************
   *
   * @type:
   *   FT_Library
   *
   * @description:
   *   A handle to a FreeType library instance.  Each 'library' is completely
   *   independent from the others; it is the 'root' of a set of objects like
   *   fonts, faces, sizes, etc.
   *
   *   It also embeds a memory manager (see @FT_Memory), as well as a
   *   scan-line converter object (see @FT_Raster).
   *
   *   [Since 2.5.6] In multi-threaded applications it is easiest to use one
   *   `FT_Library` object per thread.  In case this is too cumbersome, a
   *   single `FT_Library` object across threads is possible also, as long as
   *   a mutex lock is used around @FT_New_Face and @FT_Done_Face.
   *
   * @note:
   *   Library objects are normally created by @FT_Init_FreeType, and
   *   destroyed with @FT_Done_FreeType.  If you need reference-counting
   *   (cf. @FT_Reference_Library), use @FT_New_Library and @FT_Done_Library.
   */
  typedef struct FT_LibraryRec_  *FT_Library;


  /**************************************************************************
   *
   * @section:
   *   module_management
   *
   */

  /**************************************************************************
   *
   * @type:
   *   FT_Module
   *
   * @description:
   *   A handle to a given FreeType module object.  A module can be a font
   *   driver, a renderer, or anything else that provides services to the
   *   former.
   */
  typedef struct FT_ModuleRec_*  FT_Module;


  /**************************************************************************
   *
   * @type:
   *   FT_Driver
   *
   * @description:
   *   A handle to a given FreeType font driver object.  A font driver is a
   *   module capable of creating faces from font files.
   */
  typedef struct FT_DriverRec_*  FT_Driver;


  /**************************************************************************
   *
   * @type:
   *   FT_Renderer
   *
   * @description:
   *   A handle to a given FreeType renderer.  A renderer is a module in
   *   charge of converting a glyph's outline image to a bitmap.  It supports
   *   a single glyph image format, and one or more target surface depths.
   */
  typedef struct FT_RendererRec_*  FT_Renderer;


  /**************************************************************************
   *
   * @section:
   *   face_creation
   *
   */

  /**************************************************************************
   *
   * @type:
   *   FT_Face
   *
   * @description:
   *   A handle to a typographic face object.  A face object models a given
   *   typeface, in a given style.
   *
   * @note:
   *   A face object also owns a single @FT_GlyphSlot object, as well as one
   *   or more @FT_Size objects.
   *
   *   Use @FT_New_Face or @FT_Open_Face to create a new face object from a
   *   given filepath or a custom input stream.
   *
   *   Use @FT_Done_Face to destroy it (along with its slot and sizes).
   *
   *   An `FT_Face` object can only be safely used from one thread at a time.
   *   Similarly, creation and destruction of `FT_Face` with the same
   *   @FT_Library object can only be done from one thread at a time.  On the
   *   other hand, functions like @FT_Load_Glyph and its siblings are
   *   thread-safe and do not need the lock to be held as long as the same
   *   `FT_Face` object is not used from multiple threads at the same time.
   *
   * @also:
   *   See @FT_FaceRec for the publicly accessible fields of a given face
   *   object.
   */
  typedef struct FT_FaceRec_*  FT_Face;


  /**************************************************************************
   *
   * @section:
   *   sizing_and_scaling
   *
   */

  /**************************************************************************
   *
   * @type:
   *   FT_Size
   *
   * @description:
   *   A handle to an object that models a face scaled to a given character
   *   size.
   *
   * @note:
   *   An @FT_Face has one _active_ `FT_Size` object that is used by
   *   functions like @FT_Load_Glyph to determine the scaling transformation
   *   that in turn is used to load and hint glyphs and metrics.
   *
   *   A newly created `FT_Size` object contains only meaningless zero values.
   *   You must use @FT_Set_Char_Size, @FT_Set_Pixel_Sizes, @FT_Request_Size
   *   or even @FT_Select_Size to change the content (i.e., the scaling
   *   values) of the active `FT_Size`.  Otherwise, the scaling and hinting
   *   will not be performed.
   *
   *   You can use @FT_New_Size to create additional size objects for a given
   *   @FT_Face, but they won't be used by other functions until you activate
   *   it through @FT_Activate_Size.  Only one size can be activated at any
   *   given time per face.
   *
   * @also:
   *   See @FT_SizeRec for the publicly accessible fields of a given size
   *   object.
   */
  typedef struct FT_SizeRec_*  FT_Size;


  /**************************************************************************
   *
   * @section:
   *   glyph_retrieval
   *
   */

  /**************************************************************************
   *
   * @type:
   *   FT_GlyphSlot
   *
   * @description:
   *   A handle to a given 'glyph slot'.  A slot is a container that can hold
   *   any of the glyphs contained in its parent face.
   *
   *   In other words, each time you call @FT_Load_Glyph or @FT_Load_Char,
   *   the slot's content is erased by the new glyph data, i.e., the glyph's
   *   metrics, its image (bitmap or outline), and other control information.
   *
   * @also:
   *   See @FT_GlyphSlotRec for the publicly accessible glyph fields.
   */
  typedef struct FT_GlyphSlotRec_*  FT_GlyphSlot;


  /**************************************************************************
   *
   * @section:
   *   character_mapping
   *
   */

  /**************************************************************************
   *
   * @type:
   *   FT_CharMap
   *
   * @description:
   *   A handle to a character map (usually abbreviated to 'charmap').  A
   *   charmap is used to translate character codes in a given encoding into
   *   glyph indexes for its parent's face.  Some font formats may provide
   *   several charmaps per font.
   *
   *   Each face object owns zero or more charmaps, but only one of them can
   *   be 'active', providing the data used by @FT_Get_Char_Index or
   *   @FT_Load_Char.
   *
   *   The list of available charmaps in a face is available through the
   *   `face->num_charmaps` and `face->charmaps` fields of @FT_FaceRec.
   *
   *   The currently active charmap is available as `face->charmap`.  You
   *   should call @FT_Set_Charmap to change it.
   *
   * @note:
   *   When a new face is created (either through @FT_New_Face or
   *   @FT_Open_Face), the library looks for a Unicode charmap within the
   *   list and automatically activates it.  If there is no Unicode charmap,
   *   FreeType doesn't set an 'active' charmap.
   *
   * @also:
   *   See @FT_CharMapRec for the publicly accessible fields of a given
   *   character map.
   */
  typedef struct FT_CharMapRec_*  FT_CharMap;


  /**************************************************************************
   *
   * @macro:
   *   FT_ENC_TAG
   *
   * @description:
   *   This macro converts four-letter tags into an unsigned long.  It is
   *   used to define 'encoding' identifiers (see @FT_Encoding).
   *
   * @note:
   *   Since many 16-bit compilers don't like 32-bit enumerations, you should
   *   redefine this macro in case of problems to something like this:
   *
   *   ```
   *     #define FT_ENC_TAG( value, a, b, c, d )  value
   *   ```
   *
   *   to get a simple enumeration without assigning special numbers.
   */
/*
#ifndef FT_ENC_TAG

#define FT_ENC_TAG( value, a, b, c, d )                             \
          value = ( ( FT_STATIC_BYTE_CAST( FT_UInt32, a ) << 24 ) | \
                    ( FT_STATIC_BYTE_CAST( FT_UInt32, b ) << 16 ) | \
                    ( FT_STATIC_BYTE_CAST( FT_UInt32, c ) <<  8 ) | \
                      FT_STATIC_BYTE_CAST( FT_UInt32, d )         )
*/
//#endif /* FT_ENC_TAG */


  /**************************************************************************
   *
   * @enum:
   *   FT_Encoding
   *
   * @description:
   *   An enumeration to specify character sets supported by charmaps.  Used
   *   in the @FT_Select_Charmap API function.
   *
   * @note:
   *   Despite the name, this enumeration lists specific character
   *   repertoires (i.e., charsets), and not text encoding methods (e.g.,
   *   UTF-8, UTF-16, etc.).
   *
   *   Other encodings might be defined in the future.
   *
   * @values:
   *   FT_ENCODING_NONE ::
   *     The encoding value~0 is reserved for all formats except BDF, PCF,
   *     and Windows FNT; see below for more information.
   *
   *   FT_ENCODING_UNICODE ::
   *     The Unicode character set.  This value covers all versions of the
   *     Unicode repertoire, including ASCII and Latin-1.  Most fonts include
   *     a Unicode charmap, but not all of them.
   *
   *     For example, if you want to access Unicode value U+1F028 (and the
   *     font contains it), use value 0x1F028 as the input value for
   *     @FT_Get_Char_Index.
   *
   *   FT_ENCODING_MS_SYMBOL ::
   *     Microsoft Symbol encoding, used to encode mathematical symbols and
   *     wingdings.  For more information, see
   *     'https://www.microsoft.com/typography/otspec/recom.htm#non-standard-symbol-fonts',
   *     'http://www.kostis.net/charsets/symbol.htm', and
   *     'http://www.kostis.net/charsets/wingding.htm'.
   *
   *     This encoding uses character codes from the PUA (Private Unicode
   *     Area) in the range U+F020-U+F0FF.
   *
   *   FT_ENCODING_SJIS ::
   *     Shift JIS encoding for Japanese.  More info at
   *     'https://en.wikipedia.org/wiki/Shift_JIS'.  See note on multi-byte
   *     encodings below.
   *
   *   FT_ENCODING_PRC ::
   *     Corresponds to encoding systems mainly for Simplified Chinese as
   *     used in People's Republic of China (PRC).  The encoding layout is
   *     based on GB~2312 and its supersets GBK and GB~18030.
   *
   *   FT_ENCODING_BIG5 ::
   *     Corresponds to an encoding system for Traditional Chinese as used in
   *     Taiwan and Hong Kong.
   *
   *   FT_ENCODING_WANSUNG ::
   *     Corresponds to the Korean encoding system known as Extended Wansung
   *     (MS Windows code page 949).  For more information see
   *     'https://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WindowsBestFit/bestfit949.txt'.
   *
   *   FT_ENCODING_JOHAB ::
   *     The Korean standard character set (KS~C 5601-1992), which
   *     corresponds to MS Windows code page 1361.  This character set
   *     includes all possible Hangul character combinations.
   *
   *   FT_ENCODING_ADOBE_LATIN_1 ::
   *     Corresponds to a Latin-1 encoding as defined in a Type~1 PostScript
   *     font.  It is limited to 256 character codes.
   *
   *   FT_ENCODING_ADOBE_STANDARD ::
   *     Adobe Standard encoding, as found in Type~1, CFF, and OpenType/CFF
   *     fonts.  It is limited to 256 character codes.
   *
   *   FT_ENCODING_ADOBE_EXPERT ::
   *     Adobe Expert encoding, as found in Type~1, CFF, and OpenType/CFF
   *     fonts.  It is limited to 256 character codes.
   *
   *   FT_ENCODING_ADOBE_CUSTOM ::
   *     Corresponds to a custom encoding, as found in Type~1, CFF, and
   *     OpenType/CFF fonts.  It is limited to 256 character codes.
   *
   *   FT_ENCODING_APPLE_ROMAN ::
   *     Apple roman encoding.  Many TrueType and OpenType fonts contain a
   *     charmap for this 8-bit encoding, since older versions of Mac OS are
   *     able to use it.
   *
   *   FT_ENCODING_OLD_LATIN_2 ::
   *     This value is deprecated and was neither used nor reported by
   *     FreeType.  Don't use or test for it.
   *
   *   FT_ENCODING_MS_SJIS ::
   *     Same as FT_ENCODING_SJIS.  Deprecated.
   *
   *   FT_ENCODING_MS_GB2312 ::
   *     Same as FT_ENCODING_PRC.  Deprecated.
   *
   *   FT_ENCODING_MS_BIG5 ::
   *     Same as FT_ENCODING_BIG5.  Deprecated.
   *
   *   FT_ENCODING_MS_WANSUNG ::
   *     Same as FT_ENCODING_WANSUNG.  Deprecated.
   *
   *   FT_ENCODING_MS_JOHAB ::
   *     Same as FT_ENCODING_JOHAB.  Deprecated.
   *
   * @note:
   *   When loading a font, FreeType makes a Unicode charmap active if
   *   possible (either if the font provides such a charmap, or if FreeType
   *   can synthesize one from PostScript glyph name dictionaries; in either
   *   case, the charmap is tagged with `FT_ENCODING_UNICODE`).  If such a
   *   charmap is synthesized, it is placed at the first position of the
   *   charmap array.
   *
   *   All other encodings are considered legacy and tagged only if
   *   explicitly defined in the font file.  Otherwise, `FT_ENCODING_NONE` is
   *   used.
   *
   *   `FT_ENCODING_NONE` is set by the BDF and PCF drivers if the charmap is
   *   neither Unicode nor ISO-8859-1 (otherwise it is set to
   *   `FT_ENCODING_UNICODE`).  Use @FT_Get_BDF_Charset_ID to find out which
   *   encoding is really present.  If, for example, the `cs_registry` field
   *   is 'KOI8' and the `cs_encoding` field is 'R', the font is encoded in
   *   KOI8-R.
   *
   *   `FT_ENCODING_NONE` is always set (with a single exception) by the
   *   winfonts driver.  Use @FT_Get_WinFNT_Header and examine the `charset`
   *   field of the @FT_WinFNT_HeaderRec structure to find out which encoding
   *   is really present.  For example, @FT_WinFNT_ID_CP1251 (204) means
   *   Windows code page 1251 (for Russian).
   *
   *   `FT_ENCODING_NONE` is set if `platform_id` is @TT_PLATFORM_MACINTOSH
   *   and `encoding_id` is not `TT_MAC_ID_ROMAN` (otherwise it is set to
   *   `FT_ENCODING_APPLE_ROMAN`).
   *
   *   If `platform_id` is @TT_PLATFORM_MACINTOSH, use the function
   *   @FT_Get_CMap_Language_ID to query the Mac language ID that may be
   *   needed to be able to distinguish Apple encoding variants.  See
   *
   *     https://www.unicode.org/Public/MAPPINGS/VENDORS/APPLE/Readme.txt
   *
   *   to get an idea how to do that.  Basically, if the language ID is~0,
   *   don't use it, otherwise subtract 1 from the language ID.  Then examine
   *   `encoding_id`.  If, for example, `encoding_id` is `TT_MAC_ID_ROMAN`
   *   and the language ID (minus~1) is `TT_MAC_LANGID_GREEK`, it is the
   *   Greek encoding, not Roman.  `TT_MAC_ID_ARABIC` with
   *   `TT_MAC_LANGID_FARSI` means the Farsi variant of the Arabic encoding.
   */
/*
  typedef enum  FT_Encoding_
  {
    FT_ENC_TAG( FT_ENCODING_NONE, 0, 0, 0, 0 ),

    FT_ENC_TAG( FT_ENCODING_MS_SYMBOL, 's', 'y', 'm', 'b' ),
    FT_ENC_TAG( FT_ENCODING_UNICODE,   'u', 'n', 'i', 'c' ),

    FT_ENC_TAG( FT_ENCODING_SJIS,    's', 'j', 'i', 's' ),
    FT_ENC_TAG( FT_ENCODING_PRC,     'g', 'b', ' ', ' ' ),
    FT_ENC_TAG( FT_ENCODING_BIG5,    'b', 'i', 'g', '5' ),
    FT_ENC_TAG( FT_ENCODING_WANSUNG, 'w', 'a', 'n', 's' ),
    FT_ENC_TAG( FT_ENCODING_JOHAB,   'j', 'o', 'h', 'a' ),

    // for backward compatibility 
    FT_ENCODING_GB2312     = FT_ENCODING_PRC,
    FT_ENCODING_MS_SJIS    = FT_ENCODING_SJIS,
    FT_ENCODING_MS_GB2312  = FT_ENCODING_PRC,
    FT_ENCODING_MS_BIG5    = FT_ENCODING_BIG5,
    FT_ENCODING_MS_WANSUNG = FT_ENCODING_WANSUNG,
    FT_ENCODING_MS_JOHAB   = FT_ENCODING_JOHAB,

    FT_ENC_TAG( FT_ENCODING_ADOBE_STANDARD, 'A', 'D', 'O', 'B' ),
    FT_ENC_TAG( FT_ENCODING_ADOBE_EXPERT,   'A', 'D', 'B', 'E' ),
    FT_ENC_TAG( FT_ENCODING_ADOBE_CUSTOM,   'A', 'D', 'B', 'C' ),
    FT_ENC_TAG( FT_ENCODING_ADOBE_LATIN_1,  'l', 'a', 't', '1' ),

    FT_ENC_TAG( FT_ENCODING_OLD_LATIN_2, 'l', 'a', 't', '2' ),

    FT_ENC_TAG( FT_ENCODING_APPLE_ROMAN, 'a', 'r', 'm', 'n' )

  } FT_Encoding;
*/

  /* these constants are deprecated; use the corresponding `FT_Encoding` */
  /* values instead                                                      */
#define ft_encoding_none            FT_ENCODING_NONE
#define ft_encoding_unicode         FT_ENCODING_UNICODE
#define ft_encoding_symbol          FT_ENCODING_MS_SYMBOL
#define ft_encoding_latin_1         FT_ENCODING_ADOBE_LATIN_1
#define ft_encoding_latin_2         FT_ENCODING_OLD_LATIN_2
#define ft_encoding_sjis            FT_ENCODING_SJIS
#define ft_encoding_gb2312          FT_ENCODING_PRC
#define ft_encoding_big5            FT_ENCODING_BIG5
#define ft_encoding_wansung         FT_ENCODING_WANSUNG
#define ft_encoding_johab           FT_ENCODING_JOHAB

#define ft_encoding_adobe_standard  FT_ENCODING_ADOBE_STANDARD
#define ft_encoding_adobe_expert    FT_ENCODING_ADOBE_EXPERT
#define ft_encoding_adobe_custom    FT_ENCODING_ADOBE_CUSTOM
#define ft_encoding_apple_roman     FT_ENCODING_APPLE_ROMAN


  /**************************************************************************
   *
   * @struct:
   *   FT_CharMapRec
   *
   * @description:
   *   The base charmap structure.
   *
   * @fields:
   *   face ::
   *     A handle to the parent face object.
   *
   *   encoding ::
   *     An @FT_Encoding tag identifying the charmap.  Use this with
   *     @FT_Select_Charmap.
   *
   *   platform_id ::
   *     An ID number describing the platform for the following encoding ID.
   *     This comes directly from the TrueType specification and gets
   *     emulated for other formats.
   *
   *   encoding_id ::
   *     A platform-specific encoding number.  This also comes from the
   *     TrueType specification and gets emulated similarly.
   */
  typedef struct  FT_CharMapRec_
  {
    FT_Face      face;
    FT_Encoding  encoding;
    FT_UShort    platform_id;
    FT_UShort    encoding_id;

  } FT_CharMapRec;


  /*************************************************************************/
  /*************************************************************************/
  /*                                                                       */
  /*                 B A S E   O B J E C T   C L A S S E S                 */
  /*                                                                       */
  /*************************************************************************/
  /*************************************************************************/


  /**************************************************************************
   *
   * @section:
   *   other_api_data
   *
   */

  /**************************************************************************
   *
   * @type:
   *   FT_Face_Internal
   *
   * @description:
   *   An opaque handle to an `FT_Face_InternalRec` structure that models the
   *   private data of a given @FT_Face object.
   *
   *   This structure might change between releases of FreeType~2 and is not
   *   generally available to client applications.
   */
  typedef struct FT_Face_InternalRec_*  FT_Face_Internal;


  /**************************************************************************
   *
   * @section:
   *   face_creation
   *
   */

  /**************************************************************************
   *
   * @struct:
   *   FT_FaceRec
   *
   * @description:
   *   FreeType root face class structure.  A face object models a typeface
   *   in a font file.
   *
   * @fields:
   *   num_faces ::
   *     The number of faces in the font file.  Some font formats can have
   *     multiple faces in a single font file.
   *
   *   face_index ::
   *     This field holds two different values.  Bits 0-15 are the index of
   *     the face in the font file (starting with value~0).  They are set
   *     to~0 if there is only one face in the font file.
   *
   *     [Since 2.6.1] Bits 16-30 are relevant to GX and OpenType variation
   *     fonts only, holding the named instance index for the current face
   *     index (starting with value~1; value~0 indicates font access without
   *     a named instance).  For non-variation fonts, bits 16-30 are ignored.
   *     If we have the third named instance of face~4, say, `face_index` is
   *     set to 0x00030004.
   *
   *     Bit 31 is always zero (that is, `face_index` is always a positive
   *     value).
   *
   *     [Since 2.9] Changing the design coordinates with
   *     @FT_Set_Var_Design_Coordinates or @FT_Set_Var_Blend_Coordinates does
   *     not influence the named instance index value (only
   *     @FT_Set_Named_Instance does that).
   *
   *   face_flags ::
   *     A set of bit flags that give important information about the face;
   *     see @FT_FACE_FLAG_XXX for the details.
   *
   *   style_flags ::
   *     The lower 16~bits contain a set of bit flags indicating the style of
   *     the face; see @FT_STYLE_FLAG_XXX for the details.
   *
   *     [Since 2.6.1] Bits 16-30 hold the number of named instances
   *     available for the current face if we have a GX or OpenType variation
   *     (sub)font.  Bit 31 is always zero (that is, `style_flags` is always
   *     a positive value).  Note that a variation font has always at least
   *     one named instance, namely the default instance.
   *
   *   num_glyphs ::
   *     The number of glyphs in the face.  If the face is scalable and has
   *     sbits (see `num_fixed_sizes`), it is set to the number of outline
   *     glyphs.
   *
   *     For CID-keyed fonts (not in an SFNT wrapper) this value gives the
   *     highest CID used in the font.
   *
   *   family_name ::
   *     The face's family name.  This is an ASCII string, usually in
   *     English, that describes the typeface's family (like 'Times New
   *     Roman', 'Bodoni', 'Garamond', etc).  This is a least common
   *     denominator used to list fonts.  Some formats (TrueType & OpenType)
   *     provide localized and Unicode versions of this string.  Applications
   *     should use the format-specific interface to access them.  Can be
   *     `NULL` (e.g., in fonts embedded in a PDF file).
   *
   *     In case the font doesn't provide a specific family name entry,
   *     FreeType tries to synthesize one, deriving it from other name
   *     entries.
   *
   *   style_name ::
   *     The face's style name.  This is an ASCII string, usually in English,
   *     that describes the typeface's style (like 'Italic', 'Bold',
   *     'Condensed', etc).  Not all font formats provide a style name, so
   *     this field is optional, and can be set to `NULL`.  As for
   *     `family_name`, some formats provide localized and Unicode versions
   *     of this string.  Applications should use the format-specific
   *     interface to access them.
   *
   *   num_fixed_sizes ::
   *     The number of bitmap strikes in the face.  Even if the face is
   *     scalable, there might still be bitmap strikes, which are called
   *     'sbits' in that case.
   *
   *   available_sizes ::
   *     An array of @FT_Bitmap_Size for all bitmap strikes in the face.  It
   *     is set to `NULL` if there is no bitmap strike.
   *
   *     Note that FreeType tries to sanitize the strike data since they are
   *     sometimes sloppy or incorrect, but this can easily fail.
   *
   *   num_charmaps ::
   *     The number of charmaps in the face.
   *
   *   charmaps ::
   *     An array of the charmaps of the face.
   *
   *   generic ::
   *     A field reserved for client uses.  See the @FT_Generic type
   *     description.
   *
   *   bbox ::
   *     The font bounding box.  Coordinates are expressed in font units (see
   *     `units_per_EM`).  The box is large enough to contain any glyph from
   *     the font.  Thus, `bbox.yMax` can be seen as the 'maximum ascender',
   *     and `bbox.yMin` as the 'minimum descender'.  Only relevant for
   *     scalable formats.
   *
   *     Note that the bounding box might be off by (at least) one pixel for
   *     hinted fonts.  See @FT_Size_Metrics for further discussion.
   *
   *     Note that the bounding box does not vary in OpenType variation fonts
   *     and should only be used in relation to the default instance.
   *
   *   units_per_EM ::
   *     The number of font units per EM square for this face.  This is
   *     typically 2048 for TrueType fonts, and 1000 for Type~1 fonts.  Only
   *     relevant for scalable formats.
   *
   *   ascender ::
   *     The typographic ascender of the face, expressed in font units.  For
   *     font formats not having this information, it is set to `bbox.yMax`.
   *     Only relevant for scalable formats.
   *
   *   descender ::
   *     The typographic descender of the face, expressed in font units.  For
   *     font formats not having this information, it is set to `bbox.yMin`.
   *     Note that this field is negative for values below the baseline.
   *     Only relevant for scalable formats.
   *
   *   height ::
   *     This value is the vertical distance between two consecutive
   *     baselines, expressed in font units.  It is always positive.  Only
   *     relevant for scalable formats.
   *
   *     If you want the global glyph height, use `ascender - descender`.
   *
   *   max_advance_width ::
   *     The maximum advance width, in font units, for all glyphs in this
   *     face.  This can be used to make word wrapping computations faster.
   *     Only relevant for scalable formats.
   *
   *   max_advance_height ::
   *     The maximum advance height, in font units, for all glyphs in this
   *     face.  This is only relevant for vertical layouts, and is set to
   *     `height` for fonts that do not provide vertical metrics.  Only
   *     relevant for scalable formats.
   *
   *   underline_position ::
   *     The position, in font units, of the underline line for this face.
   *     It is the center of the underlining stem.  Only relevant for
   *     scalable formats.
   *
   *   underline_thickness ::
   *     The thickness, in font units, of the underline for this face.  Only
   *     relevant for scalable formats.
   *
   *   glyph ::
   *     The face's associated glyph slot(s).
   *
   *   size ::
   *     The current active size for this face.
   *
   *   charmap ::
   *     The current active charmap for this face.
   *
   * @note:
   *   Fields may be changed after a call to @FT_Attach_File or
   *   @FT_Attach_Stream.
   *
   *   For an OpenType variation font, the values of the following fields can
   *   change after a call to @FT_Set_Var_Design_Coordinates (and friends) if
   *   the font contains an 'MVAR' table: `ascender`, `descender`, `height`,
   *   `underline_position`, and `underline_thickness`.
   *
   *   Especially for TrueType fonts see also the documentation for
   *   @FT_Size_Metrics.
   */
  typedef struct  FT_FaceRec_
  {
    FT_Long           num_faces;
    FT_Long           face_index;

    FT_Long           face_flags;
    FT_Long           style_flags;

    FT_Long           num_glyphs;

    FT_String*        family_name;
    FT_String*        style_name;

    FT_Int            num_fixed_sizes;
    FT_Bitmap_Size*   available_sizes;

    FT_Int            num_charmaps;
    FT_CharMap*       charmaps;

    FT_Generic        generic;

    /* The following member variables (down to `underline_thickness`) */
    /* are only relevant to scalable outlines; cf. @FT_Bitmap_Size    */
    /* for bitmap fonts.                                              */
    FT_BBox           bbox;

    FT_UShort         units_per_EM;
    FT_Short          ascender;
    FT_Short          descender;
    FT_Short          height;

    FT_Short          max_advance_width;
    FT_Short          max_advance_height;

    FT_Short          underline_position;
    FT_Short          underline_thickness;

    FT_GlyphSlot      glyph;
    FT_Size           size;
    FT_CharMap        charmap;

    /* private fields, internal to FreeType */

    FT_Driver         driver;
    FT_Memory         memory;
    FT_Stream         stream;

    FT_ListRec        sizes_list;

    FT_Generic        autohint;   /* face-specific auto-hinter data */
    void*             extensions; /* unused                         */

    FT_Face_Internal  internal;

  } FT_FaceRec;


  /**************************************************************************
   *
   * @enum:
   *   FT_FACE_FLAG_XXX
   *
   * @description:
   *   A list of bit flags used in the `face_flags` field of the @FT_FaceRec
   *   structure.  They inform client applications of properties of the
   *   corresponding face.
   *
   * @values:
   *   FT_FACE_FLAG_SCALABLE ::
   *     The face contains outline glyphs.  Note that a face can contain
   *     bitmap strikes also, i.e., a face can have both this flag and
   *     @FT_FACE_FLAG_FIXED_SIZES set.
   *
   *   FT_FACE_FLAG_FIXED_SIZES ::
   *     The face contains bitmap strikes.  See also the `num_fixed_sizes`
   *     and `available_sizes` fields of @FT_FaceRec.
   *
   *   FT_FACE_FLAG_FIXED_WIDTH ::
   *     The face contains fixed-width characters (like Courier, Lucida,
   *     MonoType, etc.).
   *
   *   FT_FACE_FLAG_SFNT ::
   *     The face uses the SFNT storage scheme.  For now, this means TrueType
   *     and OpenType.
   *
   *   FT_FACE_FLAG_HORIZONTAL ::
   *     The face contains horizontal glyph metrics.  This should be set for
   *     all common formats.
   *
   *   FT_FACE_FLAG_VERTICAL ::
   *     The face contains vertical glyph metrics.  This is only available in
   *     some formats, not all of them.
   *
   *   FT_FACE_FLAG_KERNING ::
   *     The face contains kerning information.  If set, the kerning distance
   *     can be retrieved using the function @FT_Get_Kerning.  Otherwise the
   *     function always returns the vector (0,0).
   *
   *     Note that for TrueType fonts only, FreeType supports both the 'kern'
   *     table and the basic, pair-wise kerning feature from the 'GPOS' table
   *     (with `TT_CONFIG_OPTION_GPOS_KERNING` enabled), though FreeType does
   *     not support the more advanced GPOS layout features; use a library
   *     like HarfBuzz for those instead.
   *
   *   FT_FACE_FLAG_FAST_GLYPHS ::
   *     THIS FLAG IS DEPRECATED.  DO NOT USE OR TEST IT.
   *
   *   FT_FACE_FLAG_MULTIPLE_MASTERS ::
   *     The face contains multiple masters and is capable of interpolating
   *     between them.  Supported formats are Adobe MM, TrueType GX, and
   *     OpenType variation fonts.
   *
   *     See section @multiple_masters for API details.
   *
   *   FT_FACE_FLAG_GLYPH_NAMES ::
   *     The face contains glyph names, which can be retrieved using
   *     @FT_Get_Glyph_Name.  Note that some TrueType fonts contain broken
   *     glyph name tables.  Use the function @FT_Has_PS_Glyph_Names when
   *     needed.
   *
   *   FT_FACE_FLAG_EXTERNAL_STREAM ::
   *     Used internally by FreeType to indicate that a face's stream was
   *     provided by the client application and should not be destroyed when
   *     @FT_Done_Face is called.  Don't read or test this flag.
   *
   *   FT_FACE_FLAG_HINTER ::
   *     The font driver has a hinting machine of its own.  For example, with
   *     TrueType fonts, it makes sense to use data from the SFNT 'gasp'
   *     table only if the native TrueType hinting engine (with the bytecode
   *     interpreter) is available and active.
   *
   *   FT_FACE_FLAG_CID_KEYED ::
   *     The face is CID-keyed.  In that case, the face is not accessed by
   *     glyph indices but by CID values.  For subsetted CID-keyed fonts this
   *     has the consequence that not all index values are a valid argument
   *     to @FT_Load_Glyph.  Only the CID values for which corresponding
   *     glyphs in the subsetted font exist make `FT_Load_Glyph` return
   *     successfully; in all other cases you get an
   *     `FT_Err_Invalid_Argument` error.
   *
   *     Note that CID-keyed fonts that are in an SFNT wrapper (that is, all
   *     OpenType/CFF fonts) don't have this flag set since the glyphs are
   *     accessed in the normal way (using contiguous indices); the
   *     'CID-ness' isn't visible to the application.
   *
   *   FT_FACE_FLAG_TRICKY ::
   *     The face is 'tricky', that is, it always needs the font format's
   *     native hinting engine to get a reasonable result.  A typical example
   *     is the old Chinese font `mingli.ttf` (but not `mingliu.ttc`) that
   *     uses TrueType bytecode instructions to move and scale all of its
   *     subglyphs.
   *
   *     It is not possible to auto-hint such fonts using
   *     @FT_LOAD_FORCE_AUTOHINT; it will also ignore @FT_LOAD_NO_HINTING.
   *     You have to set both @FT_LOAD_NO_HINTING and @FT_LOAD_NO_AUTOHINT to
   *     really disable hinting; however, you probably never want this except
   *     for demonstration purposes.
   *
   *     Currently, there are about a dozen TrueType fonts in the list of
   *     tricky fonts; they are hard-coded in file `ttobjs.c`.
   *
   *   FT_FACE_FLAG_COLOR ::
   *     [Since 2.5.1] The face has color glyph tables.  See @FT_LOAD_COLOR
   *     for more information.
   *
   *   FT_FACE_FLAG_VARIATION ::
   *     [Since 2.9] Set if the current face (or named instance) has been
   *     altered with @FT_Set_MM_Design_Coordinates,
   *     @FT_Set_Var_Design_Coordinates, @FT_Set_Var_Blend_Coordinates, or
   *     @FT_Set_MM_WeightVector to select a non-default instance.
   *
   *   FT_FACE_FLAG_SVG ::
   *     [Since 2.12] The face has an 'SVG~' OpenType table.
   *
   *   FT_FACE_FLAG_SBIX ::
   *     [Since 2.12] The face has an 'sbix' OpenType table *and* outlines.
   *     For such fonts, @FT_FACE_FLAG_SCALABLE is not set by default to
   *     retain backward compatibility.
   *
   *   FT_FACE_FLAG_SBIX_OVERLAY ::
   *     [Since 2.12] The face has an 'sbix' OpenType table where outlines
   *     should be drawn on top of bitmap strikes.
   *
   */
#define FT_FACE_FLAG_SCALABLE          ( 1L <<  0 )
#define FT_FACE_FLAG_FIXED_SIZES       ( 1L <<  1 )
#define FT_FACE_FLAG_FIXED_WIDTH       ( 1L <<  2 )
#define FT_FACE_FLAG_SFNT              ( 1L <<  3 )
#define FT_FACE_FLAG_HORIZONTAL        ( 1L <<  4 )
#define FT_FACE_FLAG_VERTICAL          ( 1L <<  5 )
#define FT_FACE_FLAG_KERNING           ( 1L <<  6 )
#define FT_FACE_FLAG_FAST_GLYPHS       ( 1L <<  7 )
#define FT_FACE_FLAG_MULTIPLE_MASTERS  ( 1L <<  8 )
#define FT_FACE_FLAG_GLYPH_NAMES       ( 1L <<  9 )
#define FT_FACE_FLAG_EXTERNAL_STREAM   ( 1L << 10 )
#define FT_FACE_FLAG_HINTER            ( 1L << 11 )
#define FT_FACE_FLAG_CID_KEYED         ( 1L << 12 )
#define FT_FACE_FLAG_TRICKY            ( 1L << 13 )
#define FT_FACE_FLAG_COLOR             ( 1L << 14 )
#define FT_FACE_FLAG_VARIATION         ( 1L << 15 )
#define FT_FACE_FLAG_SVG               ( 1L << 16 )
#define FT_FACE_FLAG_SBIX              ( 1L << 17 )
#define FT_FACE_FLAG_SBIX_OVERLAY      ( 1L << 18 )


  /**************************************************************************
   *
   * @section:
   *   font_testing_macros
   *
   */

  /**************************************************************************
   *
   * @macro:
   *   FT_HAS_HORIZONTAL
   *
   * @description:
   *   A macro that returns true whenever a face object contains horizontal
   *   metrics (this is true for all font formats though).
   *
   * @also:
   *   @FT_HAS_VERTICAL can be used to check for vertical metrics.
   *
   */
#define FT_HAS_HORIZONTAL( face ) \
          ( !!( (face)->face_flags & FT_FACE_FLAG_HORIZONTAL ) )


  /**************************************************************************
   *
   * @macro:
   *   FT_HAS_VERTICAL
   *
   * @description:
   *   A macro that returns true whenever a face object contains real
   *   vertical metrics (and not only synthesized ones).
   *
   */
#define FT_HAS_VERTICAL( face ) \
          ( !!( (face)->face_flags & FT_FACE_FLAG_VERTICAL ) )


  /**************************************************************************
   *
   * @macro:
   *   FT_HAS_KERNING
   *
   * @description:
   *   A macro that returns true whenever a face object contains kerning data
   *   that can be accessed with @FT_Get_Kerning.
   *
   */
#define FT_HAS_KERNING( face ) \
          ( !!( (face)->face_flags & FT_FACE_FLAG_KERNING ) )


  /**************************************************************************
   *
   * @macro:
   *   FT_IS_SCALABLE
   *
   * @description:
   *   A macro that returns true whenever a face object contains a scalable
   *   font face (true for TrueType, Type~1, Type~42, CID, OpenType/CFF, and
   *   PFR font formats).
   *
   */
#define FT_IS_SCALABLE( face ) \
          ( !!( (face)->face_flags & FT_FACE_FLAG_SCALABLE ) )


  /**************************************************************************
   *
   * @macro:
   *   FT_IS_SFNT
   *
   * @description:
   *   A macro that returns true whenever a face object contains a font whose
   *   format is based on the SFNT storage scheme.  This usually means:
   *   TrueType fonts, OpenType fonts, as well as SFNT-based embedded bitmap
   *   fonts.
   *
   *   If this macro is true, all functions defined in @FT_SFNT_NAMES_H and
   *   @FT_TRUETYPE_TABLES_H are available.
   *
   */
#define FT_IS_SFNT( face ) \
          ( !!( (face)->face_flags & FT_FACE_FLAG_SFNT ) )


  /**************************************************************************
   *
   * @macro:
   *   FT_IS_FIXED_WIDTH
   *
   * @description:
   *   A macro that returns true whenever a face object contains a font face
   *   that contains fixed-width (or 'monospace', 'fixed-pitch', etc.)
   *   glyphs.
   *
   */
#define FT_IS_FIXED_WIDTH( face ) \
          ( !!( (face)->face_flags & FT_FACE_FLAG_FIXED_WIDTH ) )


  /**************************************************************************
   *
   * @macro:
   *   FT_HAS_FIXED_SIZES
   *
   * @description:
   *   A macro that returns true whenever a face object contains some
   *   embedded bitmaps.  See the `available_sizes` field of the @FT_FaceRec
   *   structure.
   *
   */
#define FT_HAS_FIXED_SIZES( face ) \
          ( !!( (face)->face_flags & FT_FACE_FLAG_FIXED_SIZES ) )


  /**************************************************************************
   *
   * @section:
   *   other_api_data
   *
   */

  /**************************************************************************
   *
   * @macro:
   *   FT_HAS_FAST_GLYPHS
   *
   * @description:
   *   Deprecated.
   *
   */
#define FT_HAS_FAST_GLYPHS( face )  0


  /**************************************************************************
   *
   * @section:
   *   font_testing_macros
   *
   */

  /**************************************************************************
   *
   * @macro:
   *   FT_HAS_GLYPH_NAMES
   *
   * @description:
   *   A macro that returns true whenever a face object contains some glyph
   *   names that can be accessed through @FT_Get_Glyph_Name.
   *
   */
#define FT_HAS_GLYPH_NAMES( face ) \
          ( !!( (face)->face_flags & FT_FACE_FLAG_GLYPH_NAMES ) )


  /**************************************************************************
   *
   * @macro:
   *   FT_HAS_MULTIPLE_MASTERS
   *
   * @description:
   *   A macro that returns true whenever a face object contains some
   *   multiple masters.  The functions provided by @FT_MULTIPLE_MASTERS_H
   *   are then available to choose the exact design you want.
   *
   */
#define FT_HAS_MULTIPLE_MASTERS( face ) \
          ( !!( (face)->face_flags & FT_FACE_FLAG_MULTIPLE_MASTERS ) )


  /**************************************************************************
   *
   * @macro:
   *   FT_IS_NAMED_INSTANCE
   *
   * @description:
   *   A macro that returns true whenever a face object is a named instance
   *   of a GX or OpenType variation font.
   *
   *   [Since 2.9] Changing the design coordinates with
   *   @FT_Set_Var_Design_Coordinates or @FT_Set_Var_Blend_Coordinates does
   *   not influence the return value of this macro (only
   *   @FT_Set_Named_Instance does that).
   *
   * @since:
   *   2.7
   *
   */
#define FT_IS_NAMED_INSTANCE( face ) \
          ( !!( (face)->face_index & 0x7FFF0000L ) )


  /**************************************************************************
   *
   * @macro:
   *   FT_IS_VARIATION
   *
   * @description:
   *   A macro that returns true whenever a face object has been altered by
   *   @FT_Set_MM_Design_Coordinates, @FT_Set_Var_Design_Coordinates,
   *   @FT_Set_Var_Blend_Coordinates, or @FT_Set_MM_WeightVector.
   *
   * @since:
   *   2.9
   *
   */
#define FT_IS_VARIATION( face ) \
          ( !!( (face)->face_flags & FT_FACE_FLAG_VARIATION ) )


  /**************************************************************************
   *
   * @macro:
   *   FT_IS_CID_KEYED
   *
   * @description:
   *   A macro that returns true whenever a face object contains a CID-keyed
   *   font.  See the discussion of @FT_FACE_FLAG_CID_KEYED for more details.
   *
   *   If this macro is true, all functions defined in @FT_CID_H are
   *   available.
   *
   */
#define FT_IS_CID_KEYED( face ) \
          ( !!( (face)->face_flags & FT_FACE_FLAG_CID_KEYED ) )


  /**************************************************************************
   *
   * @macro:
   *   FT_IS_TRICKY
   *
   * @description:
   *   A macro that returns true whenever a face represents a 'tricky' font.
   *   See the discussion of @FT_FACE_FLAG_TRICKY for more details.
   *
   */
#define FT_IS_TRICKY( face ) \
          ( !!( (face)->face_flags & FT_FACE_FLAG_TRICKY ) )


  /**************************************************************************
   *
   * @macro:
   *   FT_HAS_COLOR
   *
   * @description:
   *   A macro that returns true whenever a face object contains tables for
   *   color glyphs.
   *
   * @since:
   *   2.5.1
   *
   */
#define FT_HAS_COLOR( face ) \
          ( !!( (face)->face_flags & FT_FACE_FLAG_COLOR ) )


  /**************************************************************************
   *
   * @macro:
   *   FT_HAS_SVG
   *
   * @description:
   *   A macro that returns true whenever a face object contains an 'SVG~'
   *   OpenType table.
   *
   * @since:
   *   2.12
   */
#define FT_HAS_SVG( face ) \
          ( !!( (face)->face_flags & FT_FACE_FLAG_SVG ) )


  /**************************************************************************
   *
   * @macro:
   *   FT_HAS_SBIX
   *
   * @description:
   *   A macro that returns true whenever a face object contains an 'sbix'
   *   OpenType table *and* outline glyphs.
   *
   *   Currently, FreeType only supports bitmap glyphs in PNG format for this
   *   table (i.e., JPEG and TIFF formats are unsupported, as are
   *   Apple-specific formats not part of the OpenType specification).
   *
   * @note:
   *   For backward compatibility, a font with an 'sbix' table is treated as
   *   a bitmap-only face.  Using @FT_Open_Face with
   *   @FT_PARAM_TAG_IGNORE_SBIX, an application can switch off 'sbix'
   *   handling so that the face is treated as an ordinary outline font with
   *   scalable outlines.
   *
   *   Here is some pseudo code that roughly illustrates how to implement
   *   'sbix' handling according to the OpenType specification.
   *
   * ```
   *   if ( FT_HAS_SBIX( face ) )
   *   {
   *     // open font as a scalable one without sbix handling
   *     FT_Face       face2;
   *     FT_Parameter  param = { FT_PARAM_TAG_IGNORE_SBIX, NULL };
   *     FT_Open_Args  args  = { FT_OPEN_PARAMS | ...,
   *                             ...,
   *                             1, &param };
   *
   *
   *     FT_Open_Face( library, &args, 0, &face2 );
   *
   *     <sort `face->available_size` as necessary into
   *      `preferred_sizes`[*]>
   *
   *     for ( i = 0; i < face->num_fixed_sizes; i++ )
   *     {
   *       size = preferred_sizes[i].size;
   *
   *       error = FT_Set_Pixel_Sizes( face, size, size );
   *       <error handling omitted>
   *
   *       // check whether we have a glyph in a bitmap strike
   *       error = FT_Load_Glyph( face,
   *                              glyph_index,
   *                              FT_LOAD_SBITS_ONLY          |
   *                              FT_LOAD_BITMAP_METRICS_ONLY );
   *       if ( error == FT_Err_Invalid_Argument )
   *         continue;
   *       else if ( error )
   *         <other error handling omitted>
   *       else
   *         break;
   *     }
   *
   *     if ( i != face->num_fixed_sizes )
   *       <load embedded bitmap with `FT_Load_Glyph`,
   *        scale it, display it, etc.>
   *
   *     if ( i == face->num_fixed_sizes  ||
   *          FT_HAS_SBIX_OVERLAY( face ) )
   *       <use `face2` to load outline glyph with `FT_Load_Glyph`,
   *        scale it, display it on top of the bitmap, etc.>
   *   }
   * ```
   *
   * [*] Assuming a target value of 400dpi and available strike sizes 100,
   * 200, 300, and 400dpi, a possible order might be [400, 200, 300, 100]:
   * scaling 200dpi to 400dpi usually gives better results than scaling
   * 300dpi to 400dpi; it is also much faster.  However, scaling 100dpi to
   * 400dpi can yield a too pixelated result, thus the preference might be
   * 300dpi over 100dpi.
   *
   * @since:
   *   2.12
   */
#define FT_HAS_SBIX( face ) \
          ( !!( (face)->face_flags & FT_FACE_FLAG_SBIX ) )


  /**************************************************************************
   *
   * @macro:
   *   FT_HAS_SBIX_OVERLAY
   *
   * @description:
   *   A macro that returns true whenever a face object contains an 'sbix'
   *   OpenType table with bit~1 in its `flags` field set, instructing the
   *   application to overlay the bitmap strike with the corresponding
   *   outline glyph.  See @FT_HAS_SBIX for pseudo code how to use it.
   *
   * @since:
   *   2.12
   */
#define FT_HAS_SBIX_OVERLAY( face ) \
          ( !!( (face)->face_flags & FT_FACE_FLAG_SBIX_OVERLAY ) )


  /**************************************************************************
   *
   * @section:
   *   face_creation
   *
   */

  /**************************************************************************
   *
   * @enum:
   *   FT_STYLE_FLAG_XXX
   *
   * @description:
   *   A list of bit flags to indicate the style of a given face.  These are
   *   used in the `style_flags` field of @FT_FaceRec.
   *
   * @values:
   *   FT_STYLE_FLAG_ITALIC ::
   *     The face style is italic or oblique.
   *
   *   FT_STYLE_FLAG_BOLD ::
   *     The face is bold.
   *
   * @note:
   *   The style information as provided by FreeType is very basic.  More
   *   details are beyond the scope and should be done on a higher level (for
   *   example, by analyzing various fields of the 'OS/2' table in SFNT based
   *   fonts).
   */
#define FT_STYLE_FLAG_ITALIC  ( 1 << 0 )
#define FT_STYLE_FLAG_BOLD    ( 1 << 1 )


  /**************************************************************************
   *
   * @section:
   *   other_api_data
   *
   */

  /**************************************************************************
   *
   * @type:
   *   FT_Size_Internal
   *
   * @description:
   *   An opaque handle to an `FT_Size_InternalRec` structure, used to model
   *   private data of a given @FT_Size object.
   */
  typedef struct FT_Size_InternalRec_*  FT_Size_Internal;


  /**************************************************************************
   *
   * @section:
   *   sizing_and_scaling
   *
   */

  /**************************************************************************
   *
   * @struct:
   *   FT_Size_Metrics
   *
   * @description:
   *   The size metrics structure gives the metrics of a size object.
   *
   * @fields:
   *   x_ppem ::
   *     The width of the scaled EM square in pixels, hence the term 'ppem'
   *     (pixels per EM).  It is also referred to as 'nominal width'.
   *
   *   y_ppem ::
   *     The height of the scaled EM square in pixels, hence the term 'ppem'
   *     (pixels per EM).  It is also referred to as 'nominal height'.
   *
   *   x_scale ::
   *     A 16.16 fractional scaling value to convert horizontal metrics from
   *     font units to 26.6 fractional pixels.  Only relevant for scalable
   *     font formats.
   *
   *   y_scale ::
   *     A 16.16 fractional scaling value to convert vertical metrics from
   *     font units to 26.6 fractional pixels.  Only relevant for scalable
   *     font formats.
   *
   *   ascender ::
   *     The ascender in 26.6 fractional pixels, rounded up to an integer
   *     value.  See @FT_FaceRec for the details.
   *
   *   descender ::
   *     The descender in 26.6 fractional pixels, rounded down to an integer
   *     value.  See @FT_FaceRec for the details.
   *
   *   height ::
   *     The height in 26.6 fractional pixels, rounded to an integer value.
   *     See @FT_FaceRec for the details.
   *
   *   max_advance ::
   *     The maximum advance width in 26.6 fractional pixels, rounded to an
   *     integer value.  See @FT_FaceRec for the details.
   *
   * @note:
   *   The scaling values, if relevant, are determined first during a size
   *   changing operation.  The remaining fields are then set by the driver.
   *   For scalable formats, they are usually set to scaled values of the
   *   corresponding fields in @FT_FaceRec.  Some values like ascender or
   *   descender are rounded for historical reasons; more precise values (for
   *   outline fonts) can be derived by scaling the corresponding @FT_FaceRec
   *   values manually, with code similar to the following.
   *
   *   ```
   *     scaled_ascender = FT_MulFix( face->ascender,
   *                                  size_metrics->y_scale );
   *   ```
   *
   *   Note that due to glyph hinting and the selected rendering mode these
   *   values are usually not exact; consequently, they must be treated as
   *   unreliable with an error margin of at least one pixel!
   *
   *   Indeed, the only way to get the exact metrics is to render _all_
   *   glyphs.  As this would be a definite performance hit, it is up to
   *   client applications to perform such computations.
   *
   *   The `FT_Size_Metrics` structure is valid for bitmap fonts also.
   *
   *
   *   **TrueType fonts with native bytecode hinting**
   *
   *   All applications that handle TrueType fonts with native hinting must
   *   be aware that TTFs expect different rounding of vertical font
   *   dimensions.  The application has to cater for this, especially if it
   *   wants to rely on a TTF's vertical data (for example, to properly align
   *   box characters vertically).
   *
   *   Only the application knows _in advance_ that it is going to use native
   *   hinting for TTFs!  FreeType, on the other hand, selects the hinting
   *   mode not at the time of creating an @FT_Size object but much later,
   *   namely while calling @FT_Load_Glyph.
   *
   *   Here is some pseudo code that illustrates a possible solution.
   *
   *   ```
   *     font_format = FT_Get_Font_Format( face );
   *
   *     if ( !strcmp( font_format, "TrueType" ) &&
   *          do_native_bytecode_hinting         )
   *     {
   *       ascender  = ROUND( FT_MulFix( face->ascender,
   *                                     size_metrics->y_scale ) );
   *       descender = ROUND( FT_MulFix( face->descender,
   *                                     size_metrics->y_scale ) );
   *     }
   *     else
   *     {
   *       ascender  = size_metrics->ascender;
   *       descender = size_metrics->descender;
   *     }
   *
   *     height      = size_metrics->height;
   *     max_advance = size_metrics->max_advance;
   *   ```
   */
  typedef struct  FT_Size_Metrics_
  {
    FT_UShort  x_ppem;      /* horizontal pixels per EM               */
    FT_UShort  y_ppem;      /* vertical pixels per EM                 */

    FT_Fixed   x_scale;     /* scaling values used to convert font    */
    FT_Fixed   y_scale;     /* units to 26.6 fractional pixels        */

    FT_Pos     ascender;    /* ascender in 26.6 frac. pixels          */
    FT_Pos     descender;   /* descender in 26.6 frac. pixels         */
    FT_Pos     height;      /* text height in 26.6 frac. pixels       */
    FT_Pos     max_advance; /* max horizontal advance, in 26.6 pixels */

  } FT_Size_Metrics;


  /**************************************************************************
   *
   * @struct:
   *   FT_SizeRec
   *
   * @description:
   *   FreeType root size class structure.  A size object models a face
   *   object at a given size.
   *
   * @fields:
   *   face ::
   *     Handle to the parent face object.
   *
   *   generic ::
   *     A typeless pointer, unused by the FreeType library or any of its
   *     drivers.  It can be used by client applications to link their own
   *     data to each size object.
   *
   *   metrics ::
   *     Metrics for this size object.  This field is read-only.
   */
  typedef struct  FT_SizeRec_
  {
    FT_Face           face;      /* parent face object              */
    FT_Generic        generic;   /* generic pointer for client uses */
    FT_Size_Metrics   metrics;   /* size metrics                    */
    FT_Size_Internal  internal;

  } FT_SizeRec;


  /**************************************************************************
   *
   * @section:
   *   other_api_data
   *
   */

  /**************************************************************************
   *
   * @struct:
   *   FT_SubGlyph
   *
   * @description:
   *   The subglyph structure is an internal object used to describe
   *   subglyphs (for example, in the case of composites).
   *
   * @note:
   *   The subglyph implementation is not part of the high-level API, hence
   *   the forward structure declaration.
   *
   *   You can however retrieve subglyph information with
   *   @FT_Get_SubGlyph_Info.
   */
  typedef struct FT_SubGlyphRec_*  FT_SubGlyph;


  /**************************************************************************
   *
   * @type:
   *   FT_Slot_Internal
   *
   * @description:
   *   An opaque handle to an `FT_Slot_InternalRec` structure, used to model
   *   private data of a given @FT_GlyphSlot object.
   */
  typedef struct FT_Slot_InternalRec_*  FT_Slot_Internal;


  /**************************************************************************
   *
   * @section:
   *   glyph_retrieval
   *
   */

  /**************************************************************************
   *
   * @struct:
   *   FT_GlyphSlotRec
   *
   * @description:
   *   FreeType root glyph slot class structure.  A glyph slot is a container
   *   where individual glyphs can be loaded, be they in outline or bitmap
   *   format.
   *
   * @fields:
   *   library ::
   *     A handle to the FreeType library instance this slot belongs to.
   *
   *   face ::
   *     A handle to the parent face object.
   *
   *   next ::
   *     In some cases (like some font tools), several glyph slots per face
   *     object can be a good thing.  As this is rare, the glyph slots are
   *     listed through a direct, single-linked list using its `next` field.
   *
   *   glyph_index ::
   *     [Since 2.10] The glyph index passed as an argument to @FT_Load_Glyph
   *     while initializing the glyph slot.
   *
   *   generic ::
   *     A typeless pointer unused by the FreeType library or any of its
   *     drivers.  It can be used by client applications to link their own
   *     data to each glyph slot object.
   *
   *   metrics ::
   *     The metrics of the last loaded glyph in the slot.  The returned
   *     values depend on the last load flags (see the @FT_Load_Glyph API
   *     function) and can be expressed either in 26.6 fractional pixels or
   *     font units.
   *
   *     Note that even when the glyph image is transformed, the metrics are
   *     not.
   *
   *   linearHoriAdvance ::
   *     The advance width of the unhinted glyph.  Its value is expressed in
   *     16.16 fractional pixels, unless @FT_LOAD_LINEAR_DESIGN is set when
   *     loading the glyph.  This field can be important to perform correct
   *     WYSIWYG layout.  Only relevant for scalable glyphs.
   *
   *   linearVertAdvance ::
   *     The advance height of the unhinted glyph.  Its value is expressed in
   *     16.16 fractional pixels, unless @FT_LOAD_LINEAR_DESIGN is set when
   *     loading the glyph.  This field can be important to perform correct
   *     WYSIWYG layout.  Only relevant for scalable glyphs.
   *
   *   advance ::
   *     This shorthand is, depending on @FT_LOAD_IGNORE_TRANSFORM, the
   *     transformed (hinted) advance width for the glyph, in 26.6 fractional
   *     pixel format.  As specified with @FT_LOAD_VERTICAL_LAYOUT, it uses
   *     either the `horiAdvance` or the `vertAdvance` value of `metrics`
   *     field.
   *
   *   format ::
   *     This field indicates the format of the image contained in the glyph
   *     slot.  Typically @FT_GLYPH_FORMAT_BITMAP, @FT_GLYPH_FORMAT_OUTLINE,
   *     or @FT_GLYPH_FORMAT_COMPOSITE, but other values are possible.
   *
   *   bitmap ::
   *     This field is used as a bitmap descriptor.  Note that the address
   *     and content of the bitmap buffer can change between calls of
   *     @FT_Load_Glyph and a few other functions.
   *
   *   bitmap_left ::
   *     The bitmap's left bearing expressed in integer pixels.
   *
   *   bitmap_top ::
   *     The bitmap's top bearing expressed in integer pixels.  This is the
   *     distance from the baseline to the top-most glyph scanline, upwards
   *     y~coordinates being **positive**.
   *
   *   outline ::
   *     The outline descriptor for the current glyph image if its format is
   *     @FT_GLYPH_FORMAT_OUTLINE.  Once a glyph is loaded, `outline` can be
   *     transformed, distorted, emboldened, etc.  However, it must not be
   *     freed.
   *
   *     [Since 2.10.1] If @FT_LOAD_NO_SCALE is set, outline coordinates of
   *     OpenType variation fonts for a selected instance are internally
   *     handled as 26.6 fractional font units but returned as (rounded)
   *     integers, as expected.  To get unrounded font units, don't use
   *     @FT_LOAD_NO_SCALE but load the glyph with @FT_LOAD_NO_HINTING and
   *     scale it, using the font's `units_per_EM` value as the ppem.
   *
   *   num_subglyphs ::
   *     The number of subglyphs in a composite glyph.  This field is only
   *     valid for the composite glyph format that should normally only be
   *     loaded with the @FT_LOAD_NO_RECURSE flag.
   *
   *   subglyphs ::
   *     An array of subglyph descriptors for composite glyphs.  There are
   *     `num_subglyphs` elements in there.  Currently internal to FreeType.
   *
   *   control_data ::
   *     Certain font drivers can also return the control data for a given
   *     glyph image (e.g.  TrueType bytecode, Type~1 charstrings, etc.).
   *     This field is a pointer to such data; it is currently internal to
   *     FreeType.
   *
   *   control_len ::
   *     This is the length in bytes of the control data.  Currently internal
   *     to FreeType.
   *
   *   other ::
   *     Reserved.
   *
   *   lsb_delta ::
   *     The difference between hinted and unhinted left side bearing while
   *     auto-hinting is active.  Zero otherwise.
   *
   *   rsb_delta ::
   *     The difference between hinted and unhinted right side bearing while
   *     auto-hinting is active.  Zero otherwise.
   *
   * @note:
   *   If @FT_Load_Glyph is called with default flags (see @FT_LOAD_DEFAULT)
   *   the glyph image is loaded in the glyph slot in its native format
   *   (e.g., an outline glyph for TrueType and Type~1 formats).  [Since 2.9]
   *   The prospective bitmap metrics are calculated according to
   *   @FT_LOAD_TARGET_XXX and other flags even for the outline glyph, even
   *   if @FT_LOAD_RENDER is not set.
   *
   *   This image can later be converted into a bitmap by calling
   *   @FT_Render_Glyph.  This function searches the current renderer for the
   *   native image's format, then invokes it.
   *
   *   The renderer is in charge of transforming the native image through the
   *   slot's face transformation fields, then converting it into a bitmap
   *   that is returned in `slot->bitmap`.
   *
   *   Note that `slot->bitmap_left` and `slot->bitmap_top` are also used to
   *   specify the position of the bitmap relative to the current pen
   *   position (e.g., coordinates (0,0) on the baseline).  Of course,
   *   `slot->format` is also changed to @FT_GLYPH_FORMAT_BITMAP.
   *
   *   Here is a small pseudo code fragment that shows how to use `lsb_delta`
   *   and `rsb_delta` to do fractional positioning of glyphs:
   *
   *   ```
   *     FT_GlyphSlot  slot     = face->glyph;
   *     FT_Pos        origin_x = 0;
   *
   *
   *     for all glyphs do
   *       <load glyph with `FT_Load_Glyph'>
   *
   *       FT_Outline_Translate( slot->outline, origin_x & 63, 0 );
   *
   *       <save glyph image, or render glyph, or ...>
   *
   *       <compute kern between current and next glyph
   *        and add it to `origin_x'>
   *
   *       origin_x += slot->advance.x;
   *       origin_x += slot->lsb_delta - slot->rsb_delta;
   *     endfor
   *   ```
   *
   *   Here is another small pseudo code fragment that shows how to use
   *   `lsb_delta` and `rsb_delta` to improve integer positioning of glyphs:
   *
   *   ```
   *     FT_GlyphSlot  slot           = face->glyph;
   *     FT_Pos        origin_x       = 0;
   *     FT_Pos        prev_rsb_delta = 0;
   *
   *
   *     for all glyphs do
   *       <compute kern between current and previous glyph
   *        and add it to `origin_x'>
   *
   *       <load glyph with `FT_Load_Glyph'>
   *
   *       if ( prev_rsb_delta - slot->lsb_delta >  32 )
   *         origin_x -= 64;
   *       else if ( prev_rsb_delta - slot->lsb_delta < -31 )
   *         origin_x += 64;
   *
   *       prev_rsb_delta = slot->rsb_delta;
   *
   *       <save glyph image, or render glyph, or ...>
   *
   *       origin_x += slot->advance.x;
   *     endfor
   *   ```
   *
   *   If you use strong auto-hinting, you **must** apply these delta values!
   *   Otherwise you will experience far too large inter-glyph spacing at
   *   small rendering sizes in most cases.  Note that it doesn't harm to use
   *   the above code for other hinting modes also, since the delta values
   *   are zero then.
   */
  typedef struct  FT_GlyphSlotRec_
  {
    FT_Library        library;
    FT_Face           face;
    FT_GlyphSlot      next;
    FT_UInt           glyph_index; /* new in 2.10; was reserved previously */
    FT_Generic        generic;

    FT_Glyph_Metrics  metrics;
    FT_Fixed          linearHoriAdvance;
    FT_Fixed          linearVertAdvance;
    FT_Vector         advance;

    FT_Glyph_Format   format;

    FT_Bitmap         bitmap;
    FT_Int            bitmap_left;
    FT_Int            bitmap_top;

    FT_Outline        outline;

    FT_UInt           num_subglyphs;
    FT_SubGlyph       subglyphs;

    void*             control_data;
    long              control_len;

    FT_Pos            lsb_delta;
    FT_Pos            rsb_delta;

    void*             other;

    FT_Slot_Internal  internal;

  } FT_GlyphSlotRec;


  /*************************************************************************/
  /*************************************************************************/
  /*                                                                       */
  /*                         F U N C T I O N S                             */
  /*                                                                       */
  /*************************************************************************/
  /*************************************************************************/


  /**************************************************************************
   *
   * @section:
   *   library_setup
   *
   */

  /**************************************************************************
   *
   * @function:
   *   FT_Init_FreeType
   *
   * @description:
   *   Initialize a new FreeType library object.  The set of modules that are
   *   registered by this function is determined at build time.
   *
   * @output:
   *   alibrary ::
   *     A handle to a new library object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   In case you want to provide your own memory allocating routines, use
   *   @FT_New_Library instead, followed by a call to @FT_Add_Default_Modules
   *   (or a series of calls to @FT_Add_Module) and
   *   @FT_Set_Default_Properties.
   *
   *   See the documentation of @FT_Library and @FT_Face for multi-threading
   *   issues.
   *
   *   If you need reference-counting (cf. @FT_Reference_Library), use
   *   @FT_New_Library and @FT_Done_Library.
   *
   *   If compilation option `FT_CONFIG_OPTION_ENVIRONMENT_PROPERTIES` is
   *   set, this function reads the `FREETYPE_PROPERTIES` environment
   *   variable to control driver properties.  See section @properties for
   *   more.
   */
   FT_Error 
  FT_Init_FreeType( FT_Library  *alibrary );


  /**************************************************************************
   *
   * @function:
   *   FT_Done_FreeType
   *
   * @description:
   *   Destroy a given FreeType library object and all of its children,
   *   including resources, drivers, faces, sizes, etc.
   *
   * @input:
   *   library ::
   *     A handle to the target library object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   */
   FT_Error 
  FT_Done_FreeType( FT_Library  library );


  /**************************************************************************
   *
   * @section:
   *   face_creation
   *
   */

  /**************************************************************************
   *
   * @enum:
   *   FT_OPEN_XXX
   *
   * @description:
   *   A list of bit field constants used within the `flags` field of the
   *   @FT_Open_Args structure.
   *
   * @values:
   *   FT_OPEN_MEMORY ::
   *     This is a memory-based stream.
   *
   *   FT_OPEN_STREAM ::
   *     Copy the stream from the `stream` field.
   *
   *   FT_OPEN_PATHNAME ::
   *     Create a new input stream from a C~path name.
   *
   *   FT_OPEN_DRIVER ::
   *     Use the `driver` field.
   *
   *   FT_OPEN_PARAMS ::
   *     Use the `num_params` and `params` fields.
   *
   * @note:
   *   The `FT_OPEN_MEMORY`, `FT_OPEN_STREAM`, and `FT_OPEN_PATHNAME` flags
   *   are mutually exclusive.
   */
#define FT_OPEN_MEMORY    0x1
#define FT_OPEN_STREAM    0x2
#define FT_OPEN_PATHNAME  0x4
#define FT_OPEN_DRIVER    0x8
#define FT_OPEN_PARAMS    0x10


  /* these constants are deprecated; use the corresponding `FT_OPEN_XXX` */
  /* values instead                                                      */
#define ft_open_memory    FT_OPEN_MEMORY
#define ft_open_stream    FT_OPEN_STREAM
#define ft_open_pathname  FT_OPEN_PATHNAME
#define ft_open_driver    FT_OPEN_DRIVER
#define ft_open_params    FT_OPEN_PARAMS


  /**************************************************************************
   *
   * @struct:
   *   FT_Parameter
   *
   * @description:
   *   A simple structure to pass more or less generic parameters to
   *   @FT_Open_Face and @FT_Face_Properties.
   *
   * @fields:
   *   tag ::
   *     A four-byte identification tag.
   *
   *   data ::
   *     A pointer to the parameter data.
   *
   * @note:
   *   The ID and function of parameters are driver-specific.  See section
   *   @parameter_tags for more information.
   */
  typedef struct  FT_Parameter_
  {
    FT_ULong    tag;
    FT_Pointer  data;

  } FT_Parameter;


  /**************************************************************************
   *
   * @struct:
   *   FT_Open_Args
   *
   * @description:
   *   A structure to indicate how to open a new font file or stream.  A
   *   pointer to such a structure can be used as a parameter for the
   *   functions @FT_Open_Face and @FT_Attach_Stream.
   *
   * @fields:
   *   flags ::
   *     A set of bit flags indicating how to use the structure.
   *
   *   memory_base ::
   *     The first byte of the file in memory.
   *
   *   memory_size ::
   *     The size in bytes of the file in memory.
   *
   *   pathname ::
   *     A pointer to an 8-bit file pathname, which must be a C~string (i.e.,
   *     no null bytes except at the very end).  The pointer is not owned by
   *     FreeType.
   *
   *   stream ::
   *     A handle to a source stream object.
   *
   *   driver ::
   *     This field is exclusively used by @FT_Open_Face; it simply specifies
   *     the font driver to use for opening the face.  If set to `NULL`,
   *     FreeType tries to load the face with each one of the drivers in its
   *     list.
   *
   *   num_params ::
   *     The number of extra parameters.
   *
   *   params ::
   *     Extra parameters passed to the font driver when opening a new face.
   *
   * @note:
   *   The stream type is determined by the contents of `flags`:
   *
   *   If the @FT_OPEN_MEMORY bit is set, assume that this is a memory file
   *   of `memory_size` bytes, located at `memory_address`.  The data are not
   *   copied, and the client is responsible for releasing and destroying
   *   them _after_ the corresponding call to @FT_Done_Face.
   *
   *   Otherwise, if the @FT_OPEN_STREAM bit is set, assume that a custom
   *   input stream `stream` is used.
   *
   *   Otherwise, if the @FT_OPEN_PATHNAME bit is set, assume that this is a
   *   normal file and use `pathname` to open it.
   *
   *   If none of the above bits are set or if multiple are set at the same
   *   time, the flags are invalid and @FT_Open_Face fails.
   *
   *   If the @FT_OPEN_DRIVER bit is set, @FT_Open_Face only tries to open
   *   the file with the driver whose handler is in `driver`.
   *
   *   If the @FT_OPEN_PARAMS bit is set, the parameters given by
   *   `num_params` and `params` is used.  They are ignored otherwise.
   *
   *   Ideally, both the `pathname` and `params` fields should be tagged as
   *   'const'; this is missing for API backward compatibility.  In other
   *   words, applications should treat them as read-only.
   */
  typedef struct  FT_Open_Args_
  {
    FT_UInt         flags;
    const FT_Byte*  memory_base;
    FT_Long         memory_size;
    FT_String*      pathname;
    FT_Stream       stream;
    FT_Module       driver;
    FT_Int          num_params;
    FT_Parameter*   params;

  } FT_Open_Args;


  /**************************************************************************
   *
   * @function:
   *   FT_New_Face
   *
   * @description:
   *   Call @FT_Open_Face to open a font by its pathname.
   *
   * @inout:
   *   library ::
   *     A handle to the library resource.
   *
   * @input:
   *   pathname ::
   *     A path to the font file.
   *
   *   face_index ::
   *     See @FT_Open_Face for a detailed description of this parameter.
   *
   * @output:
   *   aface ::
   *     A handle to a new face object.  If `face_index` is greater than or
   *     equal to zero, it must be non-`NULL`.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The `pathname` string should be recognizable as such by a standard
   *   `fopen` call on your system; in particular, this means that `pathname`
   *   must not contain null bytes.  If that is not sufficient to address all
   *   file name possibilities (for example, to handle wide character file
   *   names on Windows in UTF-16 encoding) you might use @FT_Open_Face to
   *   pass a memory array or a stream object instead.
   *
   *   Use @FT_Done_Face to destroy the created @FT_Face object (along with
   *   its slot and sizes).
   */
   FT_Error 
  FT_New_Face( FT_Library   library,
               const char*  filepathname,
               FT_Long      face_index,
               FT_Face     *aface );


  /**************************************************************************
   *
   * @function:
   *   FT_New_Memory_Face
   *
   * @description:
   *   Call @FT_Open_Face to open a font that has been loaded into memory.
   *
   * @inout:
   *   library ::
   *     A handle to the library resource.
   *
   * @input:
   *   file_base ::
   *     A pointer to the beginning of the font data.
   *
   *   file_size ::
   *     The size of the memory chunk used by the font data.
   *
   *   face_index ::
   *     See @FT_Open_Face for a detailed description of this parameter.
   *
   * @output:
   *   aface ::
   *     A handle to a new face object.  If `face_index` is greater than or
   *     equal to zero, it must be non-`NULL`.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   You must not deallocate the memory before calling @FT_Done_Face.
   */
   FT_Error 
  FT_New_Memory_Face( FT_Library      library,
                      const FT_Byte*  file_base,
                      FT_Long         file_size,
                      FT_Long         face_index,
                      FT_Face        *aface );


  /**************************************************************************
   *
   * @function:
   *   FT_Open_Face
   *
   * @description:
   *   Create a face object from a given resource described by @FT_Open_Args.
   *
   * @inout:
   *   library ::
   *     A handle to the library resource.
   *
   * @input:
   *   args ::
   *     A pointer to an `FT_Open_Args` structure that must be filled by the
   *     caller.
   *
   *   face_index ::
   *     This field holds two different values.  Bits 0-15 are the index of
   *     the face in the font file (starting with value~0).  Set it to~0 if
   *     there is only one face in the font file.
   *
   *     [Since 2.6.1] Bits 16-30 are relevant to GX and OpenType variation
   *     fonts only, specifying the named instance index for the current face
   *     index (starting with value~1; value~0 makes FreeType ignore named
   *     instances).  For non-variation fonts, bits 16-30 are ignored.
   *     Assuming that you want to access the third named instance in face~4,
   *     `face_index` should be set to 0x00030004.  If you want to access
   *     face~4 without variation handling, simply set `face_index` to
   *     value~4.
   *
   *     `FT_Open_Face` and its siblings can be used to quickly check whether
   *     the font format of a given font resource is supported by FreeType.
   *     In general, if the `face_index` argument is negative, the function's
   *     return value is~0 if the font format is recognized, or non-zero
   *     otherwise.  The function allocates a more or less empty face handle
   *     in `*aface` (if `aface` isn't `NULL`); the only two useful fields in
   *     this special case are `face->num_faces` and `face->style_flags`.
   *     For any negative value of `face_index`, `face->num_faces` gives the
   *     number of faces within the font file.  For the negative value
   *     '-(N+1)' (with 'N' a non-negative 16-bit value), bits 16-30 in
   *     `face->style_flags` give the number of named instances in face 'N'
   *     if we have a variation font (or zero otherwise).  After examination,
   *     the returned @FT_Face structure should be deallocated with a call to
   *     @FT_Done_Face.
   *
   * @output:
   *   aface ::
   *     A handle to a new face object.  If `face_index` is greater than or
   *     equal to zero, it must be non-`NULL`.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   Unlike FreeType 1.x, this function automatically creates a glyph slot
   *   for the face object that can be accessed directly through
   *   `face->glyph`.
   *
   *   Each new face object created with this function also owns a default
   *   @FT_Size object, accessible as `face->size`.
   *
   *   One @FT_Library instance can have multiple face objects, that is,
   *   @FT_Open_Face and its siblings can be called multiple times using the
   *   same `library` argument.
   *
   *   See the discussion of reference counters in the description of
   *   @FT_Reference_Face.
   *
   *   If `FT_OPEN_STREAM` is set in `args->flags`, the stream in
   *   `args->stream` is automatically closed before this function returns
   *   any error (including `FT_Err_Invalid_Argument`).
   *
   * @example:
   *   To loop over all faces, use code similar to the following snippet
   *   (omitting the error handling).
   *
   *   ```
   *     ...
   *     FT_Face  face;
   *     FT_Long  i, num_faces;
   *
   *
   *     error = FT_Open_Face( library, args, -1, &face );
   *     if ( error ) { ... }
   *
   *     num_faces = face->num_faces;
   *     FT_Done_Face( face );
   *
   *     for ( i = 0; i < num_faces; i++ )
   *     {
   *       ...
   *       error = FT_Open_Face( library, args, i, &face );
   *       ...
   *       FT_Done_Face( face );
   *       ...
   *     }
   *   ```
   *
   *   To loop over all valid values for `face_index`, use something similar
   *   to the following snippet, again without error handling.  The code
   *   accesses all faces immediately (thus only a single call of
   *   `FT_Open_Face` within the do-loop), with and without named instances.
   *
   *   ```
   *     ...
   *     FT_Face  face;
   *
   *     FT_Long  num_faces     = 0;
   *     FT_Long  num_instances = 0;
   *
   *     FT_Long  face_idx     = 0;
   *     FT_Long  instance_idx = 0;
   *
   *
   *     do
   *     {
   *       FT_Long  id = ( instance_idx << 16 ) + face_idx;
   *
   *
   *       error = FT_Open_Face( library, args, id, &face );
   *       if ( error ) { ... }
   *
   *       num_faces     = face->num_faces;
   *       num_instances = face->style_flags >> 16;
   *
   *       ...
   *
   *       FT_Done_Face( face );
   *
   *       if ( instance_idx < num_instances )
   *         instance_idx++;
   *       else
   *       {
   *         face_idx++;
   *         instance_idx = 0;
   *       }
   *
   *     } while ( face_idx < num_faces )
   *   ```
   */
   FT_Error 
  FT_Open_Face( FT_Library           library,
                const FT_Open_Args*  args,
                FT_Long              face_index,
                FT_Face             *aface );


  /**************************************************************************
   *
   * @function:
   *   FT_Attach_File
   *
   * @description:
   *   Call @FT_Attach_Stream to attach a file.
   *
   * @inout:
   *   face ::
   *     The target face object.
   *
   * @input:
   *   filepathname ::
   *     The pathname.
   *
   * @return:
   *   FreeType error code.  0~means success.
   */
   FT_Error 
  FT_Attach_File( FT_Face      face,
                  const char*  filepathname );


  /**************************************************************************
   *
   * @function:
   *   FT_Attach_Stream
   *
   * @description:
   *   'Attach' data to a face object.  Normally, this is used to read
   *   additional information for the face object.  For example, you can
   *   attach an AFM file that comes with a Type~1 font to get the kerning
   *   values and other metrics.
   *
   * @inout:
   *   face ::
   *     The target face object.
   *
   * @input:
   *   parameters ::
   *     A pointer to @FT_Open_Args that must be filled by the caller.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The meaning of the 'attach' (i.e., what really happens when the new
   *   file is read) is not fixed by FreeType itself.  It really depends on
   *   the font format (and thus the font driver).
   *
   *   Client applications are expected to know what they are doing when
   *   invoking this function.  Most drivers simply do not implement file or
   *   stream attachments.
   */
   FT_Error 
  FT_Attach_Stream( FT_Face              face,
                    const FT_Open_Args*  parameters );


  /**************************************************************************
   *
   * @function:
   *   FT_Reference_Face
   *
   * @description:
   *   A counter gets initialized to~1 at the time an @FT_Face structure is
   *   created.  This function increments the counter.  @FT_Done_Face then
   *   only destroys a face if the counter is~1, otherwise it simply
   *   decrements the counter.
   *
   *   This function helps in managing life-cycles of structures that
   *   reference @FT_Face objects.
   *
   * @input:
   *   face ::
   *     A handle to a target face object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @since:
   *   2.4.2
   *
   */
   FT_Error 
  FT_Reference_Face( FT_Face  face );


  /**************************************************************************
   *
   * @function:
   *   FT_Done_Face
   *
   * @description:
   *   Discard a given face object, as well as all of its child slots and
   *   sizes.
   *
   * @input:
   *   face ::
   *     A handle to a target face object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   See the discussion of reference counters in the description of
   *   @FT_Reference_Face.
   */
   FT_Error 
  FT_Done_Face( FT_Face  face );


  /**************************************************************************
   *
   * @section:
   *   sizing_and_scaling
   *
   */

  /**************************************************************************
   *
   * @function:
   *   FT_Select_Size
   *
   * @description:
   *   Select a bitmap strike.  To be more precise, this function sets the
   *   scaling factors of the active @FT_Size object in a face so that
   *   bitmaps from this particular strike are taken by @FT_Load_Glyph and
   *   friends.
   *
   * @inout:
   *   face ::
   *     A handle to a target face object.
   *
   * @input:
   *   strike_index ::
   *     The index of the bitmap strike in the `available_sizes` field of
   *     @FT_FaceRec structure.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   For bitmaps embedded in outline fonts it is common that only a subset
   *   of the available glyphs at a given ppem value is available.  FreeType
   *   silently uses outlines if there is no bitmap for a given glyph index.
   *
   *   For GX and OpenType variation fonts, a bitmap strike makes sense only
   *   if the default instance is active (that is, no glyph variation takes
   *   place); otherwise, FreeType simply ignores bitmap strikes.  The same
   *   is true for all named instances that are different from the default
   *   instance.
   *
   *   Don't use this function if you are using the FreeType cache API.
   */
   FT_Error 
  FT_Select_Size( FT_Face  face,
                  FT_Int   strike_index );


  /**************************************************************************
   *
   * @enum:
   *   FT_Size_Request_Type
   *
   * @description:
   *   An enumeration type that lists the supported size request types, i.e.,
   *   what input size (in font units) maps to the requested output size (in
   *   pixels, as computed from the arguments of @FT_Size_Request).
   *
   * @values:
   *   FT_SIZE_REQUEST_TYPE_NOMINAL ::
   *     The nominal size.  The `units_per_EM` field of @FT_FaceRec is used
   *     to determine both scaling values.
   *
   *     This is the standard scaling found in most applications.  In
   *     particular, use this size request type for TrueType fonts if they
   *     provide optical scaling or something similar.  Note, however, that
   *     `units_per_EM` is a rather abstract value which bears no relation to
   *     the actual size of the glyphs in a font.
   *
   *   FT_SIZE_REQUEST_TYPE_REAL_DIM ::
   *     The real dimension.  The sum of the `ascender` and (minus of) the
   *     `descender` fields of @FT_FaceRec is used to determine both scaling
   *     values.
   *
   *   FT_SIZE_REQUEST_TYPE_BBOX ::
   *     The font bounding box.  The width and height of the `bbox` field of
   *     @FT_FaceRec are used to determine the horizontal and vertical
   *     scaling value, respectively.
   *
   *   FT_SIZE_REQUEST_TYPE_CELL ::
   *     The `max_advance_width` field of @FT_FaceRec is used to determine
   *     the horizontal scaling value; the vertical scaling value is
   *     determined the same way as @FT_SIZE_REQUEST_TYPE_REAL_DIM does.
   *     Finally, both scaling values are set to the smaller one.  This type
   *     is useful if you want to specify the font size for, say, a window of
   *     a given dimension and 80x24 cells.
   *
   *   FT_SIZE_REQUEST_TYPE_SCALES ::
   *     Specify the scaling values directly.
   *
   * @note:
   *   The above descriptions only apply to scalable formats.  For bitmap
   *   formats, the behaviour is up to the driver.
   *
   *   See the note section of @FT_Size_Metrics if you wonder how size
   *   requesting relates to scaling values.
   */
  typedef enum  FT_Size_Request_Type_
  {
    FT_SIZE_REQUEST_TYPE_NOMINAL,
    FT_SIZE_REQUEST_TYPE_REAL_DIM,
    FT_SIZE_REQUEST_TYPE_BBOX,
    FT_SIZE_REQUEST_TYPE_CELL,
    FT_SIZE_REQUEST_TYPE_SCALES,

    FT_SIZE_REQUEST_TYPE_MAX

  } FT_Size_Request_Type;


  /**************************************************************************
   *
   * @struct:
   *   FT_Size_RequestRec
   *
   * @description:
   *   A structure to model a size request.
   *
   * @fields:
   *   type ::
   *     See @FT_Size_Request_Type.
   *
   *   width ::
   *     The desired width, given as a 26.6 fractional point value (with 72pt
   *     = 1in).
   *
   *   height ::
   *     The desired height, given as a 26.6 fractional point value (with
   *     72pt = 1in).
   *
   *   horiResolution ::
   *     The horizontal resolution (dpi, i.e., pixels per inch).  If set to
   *     zero, `width` is treated as a 26.6 fractional **pixel** value, which
   *     gets internally rounded to an integer.
   *
   *   vertResolution ::
   *     The vertical resolution (dpi, i.e., pixels per inch).  If set to
   *     zero, `height` is treated as a 26.6 fractional **pixel** value,
   *     which gets internally rounded to an integer.
   *
   * @note:
   *   If `width` is zero, the horizontal scaling value is set equal to the
   *   vertical scaling value, and vice versa.
   *
   *   If `type` is `FT_SIZE_REQUEST_TYPE_SCALES`, `width` and `height` are
   *   interpreted directly as 16.16 fractional scaling values, without any
   *   further modification, and both `horiResolution` and `vertResolution`
   *   are ignored.
   */
  typedef struct  FT_Size_RequestRec_
  {
    FT_Size_Request_Type  type;
    FT_Long               width;
    FT_Long               height;
    FT_UInt               horiResolution;
    FT_UInt               vertResolution;

  } FT_Size_RequestRec;


  /**************************************************************************
   *
   * @struct:
   *   FT_Size_Request
   *
   * @description:
   *   A handle to a size request structure.
   */
  typedef struct FT_Size_RequestRec_  *FT_Size_Request;


  /**************************************************************************
   *
   * @function:
   *   FT_Request_Size
   *
   * @description:
   *   Resize the scale of the active @FT_Size object in a face.
   *
   * @inout:
   *   face ::
   *     A handle to a target face object.
   *
   * @input:
   *   req ::
   *     A pointer to a @FT_Size_RequestRec.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   Although drivers may select the bitmap strike matching the request,
   *   you should not rely on this if you intend to select a particular
   *   bitmap strike.  Use @FT_Select_Size instead in that case.
   *
   *   The relation between the requested size and the resulting glyph size
   *   is dependent entirely on how the size is defined in the source face.
   *   The font designer chooses the final size of each glyph relative to
   *   this size.  For more information refer to
   *   'https://www.freetype.org/freetype2/docs/glyphs/glyphs-2.html'.
   *
   *   Contrary to @FT_Set_Char_Size, this function doesn't have special code
   *   to normalize zero-valued widths, heights, or resolutions, which are
   *   treated as @FT_LOAD_NO_SCALE.
   *
   *   Don't use this function if you are using the FreeType cache API.
   */
   FT_Error 
  FT_Request_Size( FT_Face          face,
                   FT_Size_Request  req );


  /**************************************************************************
   *
   * @function:
   *   FT_Set_Char_Size
   *
   * @description:
   *   Call @FT_Request_Size to request the nominal size (in points).
   *
   * @inout:
   *   face ::
   *     A handle to a target face object.
   *
   * @input:
   *   char_width ::
   *     The nominal width, in 26.6 fractional points.
   *
   *   char_height ::
   *     The nominal height, in 26.6 fractional points.
   *
   *   horz_resolution ::
   *     The horizontal resolution in dpi.
   *
   *   vert_resolution ::
   *     The vertical resolution in dpi.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   While this function allows fractional points as input values, the
   *   resulting ppem value for the given resolution is always rounded to the
   *   nearest integer.
   *
   *   If either the character width or height is zero, it is set equal to
   *   the other value.
   *
   *   If either the horizontal or vertical resolution is zero, it is set
   *   equal to the other value.
   *
   *   A character width or height smaller than 1pt is set to 1pt; if both
   *   resolution values are zero, they are set to 72dpi.
   *
   *   Don't use this function if you are using the FreeType cache API.
   */
   FT_Error 
  FT_Set_Char_Size( FT_Face     face,
                    FT_F26Dot6  char_width,
                    FT_F26Dot6  char_height,
                    FT_UInt     horz_resolution,
                    FT_UInt     vert_resolution );


  /**************************************************************************
   *
   * @function:
   *   FT_Set_Pixel_Sizes
   *
   * @description:
   *   Call @FT_Request_Size to request the nominal size (in pixels).
   *
   * @inout:
   *   face ::
   *     A handle to the target face object.
   *
   * @input:
   *   pixel_width ::
   *     The nominal width, in pixels.
   *
   *   pixel_height ::
   *     The nominal height, in pixels.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   You should not rely on the resulting glyphs matching or being
   *   constrained to this pixel size.  Refer to @FT_Request_Size to
   *   understand how requested sizes relate to actual sizes.
   *
   *   Don't use this function if you are using the FreeType cache API.
   */
   FT_Error 
  FT_Set_Pixel_Sizes( FT_Face  face,
                      FT_UInt  pixel_width,
                      FT_UInt  pixel_height );


  /**************************************************************************
   *
   * @section:
   *   glyph_retrieval
   *
   */

  /**************************************************************************
   *
   * @function:
   *   FT_Load_Glyph
   *
   * @description:
   *   Load a glyph into the glyph slot of a face object.
   *
   * @inout:
   *   face ::
   *     A handle to the target face object where the glyph is loaded.
   *
   * @input:
   *   glyph_index ::
   *     The index of the glyph in the font file.  For CID-keyed fonts
   *     (either in PS or in CFF format) this argument specifies the CID
   *     value.
   *
   *   load_flags ::
   *     A flag indicating what to load for this glyph.  The @FT_LOAD_XXX
   *     flags can be used to control the glyph loading process (e.g.,
   *     whether the outline should be scaled, whether to load bitmaps or
   *     not, whether to hint the outline, etc).
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   For proper scaling and hinting, the active @FT_Size object owned by
   *   the face has to be meaningfully initialized by calling
   *   @FT_Set_Char_Size before this function, for example.  The loaded
   *   glyph may be transformed.  See @FT_Set_Transform for the details.
   *
   *   For subsetted CID-keyed fonts, `FT_Err_Invalid_Argument` is returned
   *   for invalid CID values (that is, for CID values that don't have a
   *   corresponding glyph in the font).  See the discussion of the
   *   @FT_FACE_FLAG_CID_KEYED flag for more details.
   *
   *   If you receive `FT_Err_Glyph_Too_Big`, try getting the glyph outline
   *   at EM size, then scale it manually and fill it as a graphics
   *   operation.
   */
   FT_Error 
  FT_Load_Glyph( FT_Face   face,
                 FT_UInt   glyph_index,
                 FT_Int32  load_flags );


  /**************************************************************************
   *
   * @section:
   *   character_mapping
   *
   */

  /**************************************************************************
   *
   * @function:
   *   FT_Load_Char
   *
   * @description:
   *   Load a glyph into the glyph slot of a face object, accessed by its
   *   character code.
   *
   * @inout:
   *   face ::
   *     A handle to a target face object where the glyph is loaded.
   *
   * @input:
   *   char_code ::
   *     The glyph's character code, according to the current charmap used in
   *     the face.
   *
   *   load_flags ::
   *     A flag indicating what to load for this glyph.  The @FT_LOAD_XXX
   *     constants can be used to control the glyph loading process (e.g.,
   *     whether the outline should be scaled, whether to load bitmaps or
   *     not, whether to hint the outline, etc).
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function simply calls @FT_Get_Char_Index and @FT_Load_Glyph.
   *
   *   Many fonts contain glyphs that can't be loaded by this function since
   *   its glyph indices are not listed in any of the font's charmaps.
   *
   *   If no active cmap is set up (i.e., `face->charmap` is zero), the call
   *   to @FT_Get_Char_Index is omitted, and the function behaves identically
   *   to @FT_Load_Glyph.
   */
   FT_Error 
  FT_Load_Char( FT_Face   face,
                FT_ULong  char_code,
                FT_Int32  load_flags );


  /**************************************************************************
   *
   * @section:
   *   glyph_retrieval
   *
   */

  /**************************************************************************
   *
   * @enum:
   *   FT_LOAD_XXX
   *
   * @description:
   *   A list of bit field constants for @FT_Load_Glyph to indicate what kind
   *   of operations to perform during glyph loading.
   *
   * @values:
   *   FT_LOAD_DEFAULT ::
   *     Corresponding to~0, this value is used as the default glyph load
   *     operation.  In this case, the following happens:
   *
   *     1. FreeType looks for a bitmap for the glyph corresponding to the
   *     face's current size.  If one is found, the function returns.  The
   *     bitmap data can be accessed from the glyph slot (see note below).
   *
   *     2. If no embedded bitmap is searched for or found, FreeType looks
   *     for a scalable outline.  If one is found, it is loaded from the font
   *     file, scaled to device pixels, then 'hinted' to the pixel grid in
   *     order to optimize it.  The outline data can be accessed from the
   *     glyph slot (see note below).
   *
   *     Note that by default the glyph loader doesn't render outlines into
   *     bitmaps.  The following flags are used to modify this default
   *     behaviour to more specific and useful cases.
   *
   *   FT_LOAD_NO_SCALE ::
   *     Don't scale the loaded outline glyph but keep it in font units.
   *     This flag is also assumed if @FT_Size owned by the face was not
   *     properly initialized.
   *
   *     This flag implies @FT_LOAD_NO_HINTING and @FT_LOAD_NO_BITMAP, and
   *     unsets @FT_LOAD_RENDER.
   *
   *     If the font is 'tricky' (see @FT_FACE_FLAG_TRICKY for more), using
   *     `FT_LOAD_NO_SCALE` usually yields meaningless outlines because the
   *     subglyphs must be scaled and positioned with hinting instructions.
   *     This can be solved by loading the font without `FT_LOAD_NO_SCALE`
   *     and setting the character size to `font->units_per_EM`.
   *
   *   FT_LOAD_NO_HINTING ::
   *     Disable hinting.  This generally generates 'blurrier' bitmap glyphs
   *     when the glyphs are rendered in any of the anti-aliased modes.  See
   *     also the note below.
   *
   *     This flag is implied by @FT_LOAD_NO_SCALE.
   *
   *   FT_LOAD_RENDER ::
   *     Call @FT_Render_Glyph after the glyph is loaded.  By default, the
   *     glyph is rendered in @FT_RENDER_MODE_NORMAL mode.  This can be
   *     overridden by @FT_LOAD_TARGET_XXX or @FT_LOAD_MONOCHROME.
   *
   *     This flag is unset by @FT_LOAD_NO_SCALE.
   *
   *   FT_LOAD_NO_BITMAP ::
   *     Ignore bitmap strikes when loading.  Bitmap-only fonts ignore this
   *     flag.
   *
   *     @FT_LOAD_NO_SCALE always sets this flag.
   *
   *   FT_LOAD_SBITS_ONLY ::
   *     [Since 2.12] This is the opposite of @FT_LOAD_NO_BITMAP, more or
   *     less: @FT_Load_Glyph returns `FT_Err_Invalid_Argument` if the face
   *     contains a bitmap strike for the given size (or the strike selected
   *     by @FT_Select_Size) but there is no glyph in the strike.
   *
   *     Note that this load flag was part of FreeType since version 2.0.6
   *     but previously tagged as internal.
   *
   *   FT_LOAD_VERTICAL_LAYOUT ::
   *     Load the glyph for vertical text layout.  In particular, the
   *     `advance` value in the @FT_GlyphSlotRec structure is set to the
   *     `vertAdvance` value of the `metrics` field.
   *
   *     In case @FT_HAS_VERTICAL doesn't return true, you shouldn't use this
   *     flag currently.  Reason is that in this case vertical metrics get
   *     synthesized, and those values are not always consistent across
   *     various font formats.
   *
   *   FT_LOAD_FORCE_AUTOHINT ::
   *     Prefer the auto-hinter over the font's native hinter.  See also the
   *     note below.
   *
   *   FT_LOAD_PEDANTIC ::
   *     Make the font driver perform pedantic verifications during glyph
   *     loading and hinting.  This is mostly used to detect broken glyphs in
   *     fonts.  By default, FreeType tries to handle broken fonts also.
   *
   *     In particular, errors from the TrueType bytecode engine are not
   *     passed to the application if this flag is not set; this might result
   *     in partially hinted or distorted glyphs in case a glyph's bytecode
   *     is buggy.
   *
   *   FT_LOAD_NO_RECURSE ::
   *     Don't load composite glyphs recursively.  Instead, the font driver
   *     fills the `num_subglyph` and `subglyphs` values of the glyph slot;
   *     it also sets `glyph->format` to @FT_GLYPH_FORMAT_COMPOSITE.  The
   *     description of subglyphs can then be accessed with
   *     @FT_Get_SubGlyph_Info.
   *
   *     Don't use this flag for retrieving metrics information since some
   *     font drivers only return rudimentary data.
   *
   *     This flag implies @FT_LOAD_NO_SCALE and @FT_LOAD_IGNORE_TRANSFORM.
   *
   *   FT_LOAD_IGNORE_TRANSFORM ::
   *     Ignore the transform matrix set by @FT_Set_Transform.
   *
   *   FT_LOAD_MONOCHROME ::
   *     This flag is used with @FT_LOAD_RENDER to indicate that you want to
   *     render an outline glyph to a 1-bit monochrome bitmap glyph, with
   *     8~pixels packed into each byte of the bitmap data.
   *
   *     Note that this has no effect on the hinting algorithm used.  You
   *     should rather use @FT_LOAD_TARGET_MONO so that the
   *     monochrome-optimized hinting algorithm is used.
   *
   *   FT_LOAD_LINEAR_DESIGN ::
   *     Keep `linearHoriAdvance` and `linearVertAdvance` fields of
   *     @FT_GlyphSlotRec in font units.  See @FT_GlyphSlotRec for details.
   *
   *   FT_LOAD_NO_AUTOHINT ::
   *     Disable the auto-hinter.  See also the note below.
   *
   *   FT_LOAD_COLOR ::
   *     Load colored glyphs.  FreeType searches in the following order;
   *     there are slight differences depending on the font format.
   *
   *     [Since 2.5] Load embedded color bitmap images (provided
   *     @FT_LOAD_NO_BITMAP is not set).  The resulting color bitmaps, if
   *     available, have the @FT_PIXEL_MODE_BGRA format, with pre-multiplied
   *     color channels.  If the flag is not set and color bitmaps are found,
   *     they are converted to 256-level gray bitmaps, using the
   *     @FT_PIXEL_MODE_GRAY format.
   *
   *     [Since 2.12] If the glyph index maps to an entry in the face's
   *     'SVG~' table, load the associated SVG document from this table and
   *     set the `format` field of @FT_GlyphSlotRec to @FT_GLYPH_FORMAT_SVG
   *     ([since 2.13.1] provided @FT_LOAD_NO_SVG is not set).  Note that
   *     FreeType itself can't render SVG documents; however, the library
   *     provides hooks to seamlessly integrate an external renderer.  See
   *     sections @ot_svg_driver and @svg_fonts for more.
   *
   *     [Since 2.10, experimental] If the glyph index maps to an entry in
   *     the face's 'COLR' table with a 'CPAL' palette table (as defined in
   *     the OpenType specification), make @FT_Render_Glyph provide a default
   *     blending of the color glyph layers associated with the glyph index,
   *     using the same bitmap format as embedded color bitmap images.  This
   *     is mainly for convenience and works only for glyphs in 'COLR' v0
   *     tables (or glyphs in 'COLR' v1 tables that exclusively use v0
   *     features).  For full control of color layers use
   *     @FT_Get_Color_Glyph_Layer and FreeType's color functions like
   *     @FT_Palette_Select instead of setting @FT_LOAD_COLOR for rendering
   *     so that the client application can handle blending by itself.
   *
   *   FT_LOAD_NO_SVG ::
   *     [Since 2.13.1] Ignore SVG glyph data when loading.
   *
   *   FT_LOAD_COMPUTE_METRICS ::
   *     [Since 2.6.1] Compute glyph metrics from the glyph data, without the
   *     use of bundled metrics tables (for example, the 'hdmx' table in
   *     TrueType fonts).  This flag is mainly used by font validating or
   *     font editing applications, which need to ignore, verify, or edit
   *     those tables.
   *
   *     Currently, this flag is only implemented for TrueType fonts.
   *
   *   FT_LOAD_BITMAP_METRICS_ONLY ::
   *     [Since 2.7.1] Request loading of the metrics and bitmap image
   *     information of a (possibly embedded) bitmap glyph without allocating
   *     or copying the bitmap image data itself.  No effect if the target
   *     glyph is not a bitmap image.
   *
   *     This flag unsets @FT_LOAD_RENDER.
   *
   *   FT_LOAD_CROP_BITMAP ::
   *     Ignored.  Deprecated.
   *
   *   FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH ::
   *     Ignored.  Deprecated.
   *
   * @note:
   *   By default, hinting is enabled and the font's native hinter (see
   *   @FT_FACE_FLAG_HINTER) is preferred over the auto-hinter.  You can
   *   disable hinting by setting @FT_LOAD_NO_HINTING or change the
   *   precedence by setting @FT_LOAD_FORCE_AUTOHINT.  You can also set
   *   @FT_LOAD_NO_AUTOHINT in case you don't want the auto-hinter to be used
   *   at all.
   *
   *   See the description of @FT_FACE_FLAG_TRICKY for a special exception
   *   (affecting only a handful of Asian fonts).
   *
   *   Besides deciding which hinter to use, you can also decide which
   *   hinting algorithm to use.  See @FT_LOAD_TARGET_XXX for details.
   *
   *   Note that the auto-hinter needs a valid Unicode cmap (either a native
   *   one or synthesized by FreeType) for producing correct results.  If a
   *   font provides an incorrect mapping (for example, assigning the
   *   character code U+005A, LATIN CAPITAL LETTER~Z, to a glyph depicting a
   *   mathematical integral sign), the auto-hinter might produce useless
   *   results.
   *
   */
#define FT_LOAD_DEFAULT                      0x0
#define FT_LOAD_NO_SCALE                     ( 1L << 0  )
#define FT_LOAD_NO_HINTING                   ( 1L << 1  )
#define FT_LOAD_RENDER                       ( 1L << 2  )
#define FT_LOAD_NO_BITMAP                    ( 1L << 3  )
#define FT_LOAD_VERTICAL_LAYOUT              ( 1L << 4  )
#define FT_LOAD_FORCE_AUTOHINT               ( 1L << 5  )
#define FT_LOAD_CROP_BITMAP                  ( 1L << 6  )
#define FT_LOAD_PEDANTIC                     ( 1L << 7  )
#define FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH  ( 1L << 9  )
#define FT_LOAD_NO_RECURSE                   ( 1L << 10 )
#define FT_LOAD_IGNORE_TRANSFORM             ( 1L << 11 )
#define FT_LOAD_MONOCHROME                   ( 1L << 12 )
#define FT_LOAD_LINEAR_DESIGN                ( 1L << 13 )
#define FT_LOAD_SBITS_ONLY                   ( 1L << 14 )
#define FT_LOAD_NO_AUTOHINT                  ( 1L << 15 )
  /* Bits 16-19 are used by `FT_LOAD_TARGET_` */
#define FT_LOAD_COLOR                        ( 1L << 20 )
#define FT_LOAD_COMPUTE_METRICS              ( 1L << 21 )
#define FT_LOAD_BITMAP_METRICS_ONLY          ( 1L << 22 )
#define FT_LOAD_NO_SVG                       ( 1L << 24 )

  /* */

  /* used internally only by certain font drivers */
#define FT_LOAD_ADVANCE_ONLY                 ( 1L << 8  )
#define FT_LOAD_SVG_ONLY                     ( 1L << 23 )


  /**************************************************************************
   *
   * @enum:
   *   FT_LOAD_TARGET_XXX
   *
   * @description:
   *   A list of values to select a specific hinting algorithm for the
   *   hinter.  You should OR one of these values to your `load_flags` when
   *   calling @FT_Load_Glyph.
   *
   *   Note that a font's native hinters may ignore the hinting algorithm you
   *   have specified (e.g., the TrueType bytecode interpreter).  You can set
   *   @FT_LOAD_FORCE_AUTOHINT to ensure that the auto-hinter is used.
   *
   * @values:
   *   FT_LOAD_TARGET_NORMAL ::
   *     The default hinting algorithm, optimized for standard gray-level
   *     rendering.  For monochrome output, use @FT_LOAD_TARGET_MONO instead.
   *
   *   FT_LOAD_TARGET_LIGHT ::
   *     A lighter hinting algorithm for gray-level modes.  Many generated
   *     glyphs are fuzzier but better resemble their original shape.  This
   *     is achieved by snapping glyphs to the pixel grid only vertically
   *     (Y-axis), as is done by FreeType's new CFF engine or Microsoft's
   *     ClearType font renderer.  This preserves inter-glyph spacing in
   *     horizontal text.  The snapping is done either by the native font
   *     driver, if the driver itself and the font support it, or by the
   *     auto-hinter.
   *
   *     Advance widths are rounded to integer values; however, using the
   *     `lsb_delta` and `rsb_delta` fields of @FT_GlyphSlotRec, it is
   *     possible to get fractional advance widths for subpixel positioning
   *     (which is recommended to use).
   *
   *     If configuration option `AF_CONFIG_OPTION_TT_SIZE_METRICS` is
   *     active, TrueType-like metrics are used to make this mode behave
   *     similarly as in unpatched FreeType versions between 2.4.6 and 2.7.1
   *     (inclusive).
   *
   *   FT_LOAD_TARGET_MONO ::
   *     Strong hinting algorithm that should only be used for monochrome
   *     output.  The result is probably unpleasant if the glyph is rendered
   *     in non-monochrome modes.
   *
   *     Note that for outline fonts only the TrueType font driver has proper
   *     monochrome hinting support, provided the TTFs contain hints for B/W
   *     rendering (which most fonts no longer provide).  If these conditions
   *     are not met it is very likely that you get ugly results at smaller
   *     sizes.
   *
   *   FT_LOAD_TARGET_LCD ::
   *     A variant of @FT_LOAD_TARGET_LIGHT optimized for horizontally
   *     decimated LCD displays.
   *
   *   FT_LOAD_TARGET_LCD_V ::
   *     A variant of @FT_LOAD_TARGET_NORMAL optimized for vertically
   *     decimated LCD displays.
   *
   * @note:
   *   You should use only _one_ of the `FT_LOAD_TARGET_XXX` values in your
   *   `load_flags`.  They can't be ORed.
   *
   *   If @FT_LOAD_RENDER is also set, the glyph is rendered in the
   *   corresponding mode (i.e., the mode that matches the used algorithm
   *   best).  An exception is `FT_LOAD_TARGET_MONO` since it implies
   *   @FT_LOAD_MONOCHROME.
   *
   *   You can use a hinting algorithm that doesn't correspond to the same
   *   rendering mode.  As an example, it is possible to use the 'light'
   *   hinting algorithm and have the results rendered in horizontal LCD
   *   pixel mode, with code like
   *
   *   ```
   *     FT_Load_Glyph( face, glyph_index,
   *                    load_flags | FT_LOAD_TARGET_LIGHT );
   *
   *     FT_Render_Glyph( face->glyph, FT_RENDER_MODE_LCD );
   *   ```
   *
   *   In general, you should stick with one rendering mode.  For example,
   *   switching between @FT_LOAD_TARGET_NORMAL and @FT_LOAD_TARGET_MONO
   *   enforces a lot of recomputation for TrueType fonts, which is slow.
   *   Another reason is caching: Selecting a different mode usually causes
   *   changes in both the outlines and the rasterized bitmaps; it is thus
   *   necessary to empty the cache after a mode switch to avoid false hits.
   *
   */
#define FT_LOAD_TARGET_( x )   ( FT_STATIC_CAST( FT_Int32, (x) & 15 ) << 16 )

#define FT_LOAD_TARGET_NORMAL  FT_LOAD_TARGET_( FT_RENDER_MODE_NORMAL )
#define FT_LOAD_TARGET_LIGHT   FT_LOAD_TARGET_( FT_RENDER_MODE_LIGHT  )
#define FT_LOAD_TARGET_MONO    FT_LOAD_TARGET_( FT_RENDER_MODE_MONO   )
#define FT_LOAD_TARGET_LCD     FT_LOAD_TARGET_( FT_RENDER_MODE_LCD    )
#define FT_LOAD_TARGET_LCD_V   FT_LOAD_TARGET_( FT_RENDER_MODE_LCD_V  )


  /**************************************************************************
   *
   * @macro:
   *   FT_LOAD_TARGET_MODE
   *
   * @description:
   *   Return the @FT_Render_Mode corresponding to a given
   *   @FT_LOAD_TARGET_XXX value.
   *
   */
#define FT_LOAD_TARGET_MODE( x )                               \
          FT_STATIC_CAST( FT_Render_Mode, ( (x) >> 16 ) & 15 )


  /**************************************************************************
   *
   * @section:
   *   sizing_and_scaling
   *
   */

  /**************************************************************************
   *
   * @function:
   *   FT_Set_Transform
   *
   * @description:
   *   Set the transformation that is applied to glyph images when they are
   *   loaded into a glyph slot through @FT_Load_Glyph.
   *
   * @inout:
   *   face ::
   *     A handle to the source face object.
   *
   * @input:
   *   matrix ::
   *     A pointer to the transformation's 2x2 matrix.  Use `NULL` for the
   *     identity matrix.
   *   delta ::
   *     A pointer to the translation vector.  Use `NULL` for the null
   *     vector.
   *
   * @note:
   *   This function is provided as a convenience, but keep in mind that
   *   @FT_Matrix coefficients are only 16.16 fixed-point values, which can
   *   limit the accuracy of the results.  Using floating-point computations
   *   to perform the transform directly in client code instead will always
   *   yield better numbers.
   *
   *   The transformation is only applied to scalable image formats after the
   *   glyph has been loaded.  It means that hinting is unaltered by the
   *   transformation and is performed on the character size given in the
   *   last call to @FT_Set_Char_Size or @FT_Set_Pixel_Sizes.
   *
   *   Note that this also transforms the `face.glyph.advance` field, but
   *   **not** the values in `face.glyph.metrics`.
   */
   void 
  FT_Set_Transform( FT_Face     face,
                    FT_Matrix*  matrix,
                    FT_Vector*  delta );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Transform
   *
   * @description:
   *   Return the transformation that is applied to glyph images when they
   *   are loaded into a glyph slot through @FT_Load_Glyph.  See
   *   @FT_Set_Transform for more details.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   * @output:
   *   matrix ::
   *     A pointer to a transformation's 2x2 matrix.  Set this to NULL if you
   *     are not interested in the value.
   *
   *   delta ::
   *     A pointer to a translation vector.  Set this to NULL if you are not
   *     interested in the value.
   *
   * @since:
   *   2.11
   *
   */
   void 
  FT_Get_Transform( FT_Face     face,
                    FT_Matrix*  matrix,
                    FT_Vector*  delta );


  /**************************************************************************
   *
   * @section:
   *   glyph_retrieval
   *
   */

  /**************************************************************************
   *
   * @enum:
   *   FT_Render_Mode
   *
   * @description:
   *   Render modes supported by FreeType~2.  Each mode corresponds to a
   *   specific type of scanline conversion performed on the outline.
   *
   *   For bitmap fonts and embedded bitmaps the `bitmap->pixel_mode` field
   *   in the @FT_GlyphSlotRec structure gives the format of the returned
   *   bitmap.
   *
   *   All modes except @FT_RENDER_MODE_MONO use 256 levels of opacity,
   *   indicating pixel coverage.  Use linear alpha blending and gamma
   *   correction to correctly render non-monochrome glyph bitmaps onto a
   *   surface; see @FT_Render_Glyph.
   *
   *   The @FT_RENDER_MODE_SDF is a special render mode that uses up to 256
   *   distance values, indicating the signed distance from the grid position
   *   to the nearest outline.
   *
   * @values:
   *   FT_RENDER_MODE_NORMAL ::
   *     Default render mode; it corresponds to 8-bit anti-aliased bitmaps.
   *
   *   FT_RENDER_MODE_LIGHT ::
   *     This is equivalent to @FT_RENDER_MODE_NORMAL.  It is only defined as
   *     a separate value because render modes are also used indirectly to
   *     define hinting algorithm selectors.  See @FT_LOAD_TARGET_XXX for
   *     details.
   *
   *   FT_RENDER_MODE_MONO ::
   *     This mode corresponds to 1-bit bitmaps (with 2~levels of opacity).
   *
   *   FT_RENDER_MODE_LCD ::
   *     This mode corresponds to horizontal RGB and BGR subpixel displays
   *     like LCD screens.  It produces 8-bit bitmaps that are 3~times the
   *     width of the original glyph outline in pixels, and which use the
   *     @FT_PIXEL_MODE_LCD mode.
   *
   *   FT_RENDER_MODE_LCD_V ::
   *     This mode corresponds to vertical RGB and BGR subpixel displays
   *     (like PDA screens, rotated LCD displays, etc.).  It produces 8-bit
   *     bitmaps that are 3~times the height of the original glyph outline in
   *     pixels and use the @FT_PIXEL_MODE_LCD_V mode.
   *
   *   FT_RENDER_MODE_SDF ::
   *     The positive (unsigned) 8-bit bitmap values can be converted to the
   *     single-channel signed distance field (SDF) by subtracting 128, with
   *     the positive and negative results corresponding to the inside and
   *     the outside of a glyph contour, respectively.  The distance units are
   *     arbitrarily determined by an adjustable @spread property.
   *
   * @note:
   *   The selected render mode only affects scalable vector glyphs of a font.
   *   Embedded bitmaps often have a different pixel mode like
   *   @FT_PIXEL_MODE_MONO.  You can use @FT_Bitmap_Convert to transform them
   *   into 8-bit pixmaps.
   *
   */
  typedef enum  FT_Render_Mode_
  {
    FT_RENDER_MODE_NORMAL = 0,
    FT_RENDER_MODE_LIGHT,
    FT_RENDER_MODE_MONO,
    FT_RENDER_MODE_LCD,
    FT_RENDER_MODE_LCD_V,
    FT_RENDER_MODE_SDF,

    FT_RENDER_MODE_MAX

  } FT_Render_Mode;


  /* these constants are deprecated; use the corresponding */
  /* `FT_Render_Mode` values instead                       */
#define ft_render_mode_normal  FT_RENDER_MODE_NORMAL
#define ft_render_mode_mono    FT_RENDER_MODE_MONO


  /**************************************************************************
   *
   * @function:
   *   FT_Render_Glyph
   *
   * @description:
   *   Convert a given glyph image to a bitmap.  It does so by inspecting the
   *   glyph image format, finding the relevant renderer, and invoking it.
   *
   * @inout:
   *   slot ::
   *     A handle to the glyph slot containing the image to convert.
   *
   * @input:
   *   render_mode ::
   *     The render mode used to render the glyph image into a bitmap.  See
   *     @FT_Render_Mode for a list of possible values.
   *
   *     If @FT_RENDER_MODE_NORMAL is used, a previous call of @FT_Load_Glyph
   *     with flag @FT_LOAD_COLOR makes `FT_Render_Glyph` provide a default
   *     blending of colored glyph layers associated with the current glyph
   *     slot (provided the font contains such layers) instead of rendering
   *     the glyph slot's outline.  This is an experimental feature; see
   *     @FT_LOAD_COLOR for more information.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   When FreeType outputs a bitmap of a glyph, it really outputs an alpha
   *   coverage map.  If a pixel is completely covered by a filled-in
   *   outline, the bitmap contains 0xFF at that pixel, meaning that
   *   0xFF/0xFF fraction of that pixel is covered, meaning the pixel is 100%
   *   black (or 0% bright).  If a pixel is only 50% covered (value 0x80),
   *   the pixel is made 50% black (50% bright or a middle shade of grey).
   *   0% covered means 0% black (100% bright or white).
   *
   *   On high-DPI screens like on smartphones and tablets, the pixels are so
   *   small that their chance of being completely covered and therefore
   *   completely black are fairly good.  On the low-DPI screens, however,
   *   the situation is different.  The pixels are too large for most of the
   *   details of a glyph and shades of gray are the norm rather than the
   *   exception.
   *
   *   This is relevant because all our screens have a second problem: they
   *   are not linear.  1~+~1 is not~2.  Twice the value does not result in
   *   twice the brightness.  When a pixel is only 50% covered, the coverage
   *   map says 50% black, and this translates to a pixel value of 128 when
   *   you use 8~bits per channel (0-255).  However, this does not translate
   *   to 50% brightness for that pixel on our sRGB and gamma~2.2 screens.
   *   Due to their non-linearity, they dwell longer in the darks and only a
   *   pixel value of about 186 results in 50% brightness -- 128 ends up too
   *   dark on both bright and dark backgrounds.  The net result is that dark
   *   text looks burnt-out, pixely and blotchy on bright background, bright
   *   text too frail on dark backgrounds, and colored text on colored
   *   background (for example, red on green) seems to have dark halos or
   *   'dirt' around it.  The situation is especially ugly for diagonal stems
   *   like in 'w' glyph shapes where the quality of FreeType's anti-aliasing
   *   depends on the correct display of grays.  On high-DPI screens where
   *   smaller, fully black pixels reign supreme, this doesn't matter, but on
   *   our low-DPI screens with all the gray shades, it does.  0% and 100%
   *   brightness are the same things in linear and non-linear space, just
   *   all the shades in-between aren't.
   *
   *   The blending function for placing text over a background is
   *
   *   ```
   *     dst = alpha * src + (1 - alpha) * dst    ,
   *   ```
   *
   *   which is known as the OVER operator.
   *
   *   To correctly composite an anti-aliased pixel of a glyph onto a
   *   surface,
   *
   *   1. take the foreground and background colors (e.g., in sRGB space)
   *      and apply gamma to get them in a linear space,
   *
   *   2. use OVER to blend the two linear colors using the glyph pixel
   *      as the alpha value (remember, the glyph bitmap is an alpha coverage
   *      bitmap), and
   *
   *   3. apply inverse gamma to the blended pixel and write it back to
   *      the image.
   *
   *   Internal testing at Adobe found that a target inverse gamma of~1.8 for
   *   step~3 gives good results across a wide range of displays with an sRGB
   *   gamma curve or a similar one.
   *
   *   This process can cost performance.  There is an approximation that
   *   does not need to know about the background color; see
   *   https://bel.fi/alankila/lcd/ and
   *   https://bel.fi/alankila/lcd/alpcor.html for details.
   *
   *   **ATTENTION**: Linear blending is even more important when dealing
   *   with subpixel-rendered glyphs to prevent color-fringing!  A
   *   subpixel-rendered glyph must first be filtered with a filter that
   *   gives equal weight to the three color primaries and does not exceed a
   *   sum of 0x100, see section @lcd_rendering.  Then the only difference to
   *   gray linear blending is that subpixel-rendered linear blending is done
   *   3~times per pixel: red foreground subpixel to red background subpixel
   *   and so on for green and blue.
   */
   FT_Error 
  FT_Render_Glyph( FT_GlyphSlot    slot,
                   FT_Render_Mode  render_mode );


  /**************************************************************************
   *
   * @enum:
   *   FT_Kerning_Mode
   *
   * @description:
   *   An enumeration to specify the format of kerning values returned by
   *   @FT_Get_Kerning.
   *
   * @values:
   *   FT_KERNING_DEFAULT ::
   *     Return grid-fitted kerning distances in 26.6 fractional pixels.
   *
   *   FT_KERNING_UNFITTED ::
   *     Return un-grid-fitted kerning distances in 26.6 fractional pixels.
   *
   *   FT_KERNING_UNSCALED ::
   *     Return the kerning vector in original font units.
   *
   * @note:
   *   `FT_KERNING_DEFAULT` returns full pixel values; it also makes FreeType
   *   heuristically scale down kerning distances at small ppem values so
   *   that they don't become too big.
   *
   *   Both `FT_KERNING_DEFAULT` and `FT_KERNING_UNFITTED` use the current
   *   horizontal scaling factor (as set e.g. with @FT_Set_Char_Size) to
   *   convert font units to pixels.
   */
  typedef enum  FT_Kerning_Mode_
  {
    FT_KERNING_DEFAULT = 0,
    FT_KERNING_UNFITTED,
    FT_KERNING_UNSCALED

  } FT_Kerning_Mode;


  /* these constants are deprecated; use the corresponding */
  /* `FT_Kerning_Mode` values instead                      */
#define ft_kerning_default   FT_KERNING_DEFAULT
#define ft_kerning_unfitted  FT_KERNING_UNFITTED
#define ft_kerning_unscaled  FT_KERNING_UNSCALED


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Kerning
   *
   * @description:
   *   Return the kerning vector between two glyphs of the same face.
   *
   * @input:
   *   face ::
   *     A handle to a source face object.
   *
   *   left_glyph ::
   *     The index of the left glyph in the kern pair.
   *
   *   right_glyph ::
   *     The index of the right glyph in the kern pair.
   *
   *   kern_mode ::
   *     See @FT_Kerning_Mode for more information.  Determines the scale and
   *     dimension of the returned kerning vector.
   *
   * @output:
   *   akerning ::
   *     The kerning vector.  This is either in font units, fractional pixels
   *     (26.6 format), or pixels for scalable formats, and in pixels for
   *     fixed-sizes formats.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   Only horizontal layouts (left-to-right & right-to-left) are supported
   *   by this method.  Other layouts, or more sophisticated kernings, are
   *   out of the scope of this API function -- they can be implemented
   *   through format-specific interfaces.
   *
   *   Note that, for TrueType fonts only, this can extract data from both
   *   the 'kern' table and the basic, pair-wise kerning feature from the
   *   GPOS table (with `TT_CONFIG_OPTION_GPOS_KERNING` enabled), though
   *   FreeType does not support the more advanced GPOS layout features; use
   *   a library like HarfBuzz for those instead.  If a font has both a
   *   'kern' table and kern features of a GPOS table, the 'kern' table will
   *   be used.
   *
   *   Also note for right-to-left scripts, the functionality may differ for
   *   fonts with GPOS tables vs. 'kern' tables.  For GPOS, right-to-left
   *   fonts typically use both a placement offset and an advance for pair
   *   positioning, which this API does not support, so it would output
   *   kerning values of zero; though if the right-to-left font used only
   *   advances in GPOS pair positioning, then this API could output kerning
   *   values for it, but it would use `left_glyph` to mean the first glyph
   *   for that case.  Whereas 'kern' tables are always advance-only and
   *   always store the left glyph first.
   *
   *   Use @FT_HAS_KERNING to find out whether a font has data that can be
   *   extracted with `FT_Get_Kerning`.
   */
   FT_Error 
  FT_Get_Kerning( FT_Face     face,
                  FT_UInt     left_glyph,
                  FT_UInt     right_glyph,
                  FT_UInt     kern_mode,
                  FT_Vector  *akerning );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Track_Kerning
   *
   * @description:
   *   Return the track kerning for a given face object at a given size.
   *
   * @input:
   *   face ::
   *     A handle to a source face object.
   *
   *   point_size ::
   *     The point size in 16.16 fractional points.
   *
   *   degree ::
   *     The degree of tightness.  Increasingly negative values represent
   *     tighter track kerning, while increasingly positive values represent
   *     looser track kerning.  Value zero means no track kerning.
   *
   * @output:
   *   akerning ::
   *     The kerning in 16.16 fractional points, to be uniformly applied
   *     between all glyphs.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   Currently, only the Type~1 font driver supports track kerning, using
   *   data from AFM files (if attached with @FT_Attach_File or
   *   @FT_Attach_Stream).
   *
   *   Only very few AFM files come with track kerning data; please refer to
   *   Adobe's AFM specification for more details.
   */
   FT_Error 
  FT_Get_Track_Kerning( FT_Face    face,
                        FT_Fixed   point_size,
                        FT_Int     degree,
                        FT_Fixed*  akerning );


  /**************************************************************************
   *
   * @section:
   *   character_mapping
   *
   */

  /**************************************************************************
   *
   * @function:
   *   FT_Select_Charmap
   *
   * @description:
   *   Select a given charmap by its encoding tag (as listed in
   *   `freetype.h`).
   *
   * @inout:
   *   face ::
   *     A handle to the source face object.
   *
   * @input:
   *   encoding ::
   *     A handle to the selected encoding.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function returns an error if no charmap in the face corresponds
   *   to the encoding queried here.
   *
   *   Because many fonts contain more than a single cmap for Unicode
   *   encoding, this function has some special code to select the one that
   *   covers Unicode best ('best' in the sense that a UCS-4 cmap is
   *   preferred to a UCS-2 cmap).  It is thus preferable to @FT_Set_Charmap
   *   in this case.
   */
   FT_Error 
  FT_Select_Charmap( FT_Face      face,
                     FT_Encoding  encoding );


  /**************************************************************************
   *
   * @function:
   *   FT_Set_Charmap
   *
   * @description:
   *   Select a given charmap for character code to glyph index mapping.
   *
   * @inout:
   *   face ::
   *     A handle to the source face object.
   *
   * @input:
   *   charmap ::
   *     A handle to the selected charmap.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function returns an error if the charmap is not part of the face
   *   (i.e., if it is not listed in the `face->charmaps` table).
   *
   *   It also fails if an OpenType type~14 charmap is selected (which
   *   doesn't map character codes to glyph indices at all).
   */
   FT_Error 
  FT_Set_Charmap( FT_Face     face,
                  FT_CharMap  charmap );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Charmap_Index
   *
   * @description:
   *   Retrieve index of a given charmap.
   *
   * @input:
   *   charmap ::
   *     A handle to a charmap.
   *
   * @return:
   *   The index into the array of character maps within the face to which
   *   `charmap` belongs.  If an error occurs, -1 is returned.
   *
   */
   FT_Int 
  FT_Get_Charmap_Index( FT_CharMap  charmap );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Char_Index
   *
   * @description:
   *   Return the glyph index of a given character code.  This function uses
   *   the currently selected charmap to do the mapping.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   *   charcode ::
   *     The character code.
   *
   * @return:
   *   The glyph index.  0~means 'undefined character code'.
   *
   * @note:
   *   If you use FreeType to manipulate the contents of font files directly,
   *   be aware that the glyph index returned by this function doesn't always
   *   correspond to the internal indices used within the file.  This is done
   *   to ensure that value~0 always corresponds to the 'missing glyph'.  If
   *   the first glyph is not named '.notdef', then for Type~1 and Type~42
   *   fonts, '.notdef' will be moved into the glyph ID~0 position, and
   *   whatever was there will be moved to the position '.notdef' had.  For
   *   Type~1 fonts, if there is no '.notdef' glyph at all, then one will be
   *   created at index~0 and whatever was there will be moved to the last
   *   index -- Type~42 fonts are considered invalid under this condition.
   */
   FT_UInt 
  FT_Get_Char_Index( FT_Face   face,
                     FT_ULong  charcode );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_First_Char
   *
   * @description:
   *   Return the first character code in the current charmap of a given
   *   face, together with its corresponding glyph index.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   * @output:
   *   agindex ::
   *     Glyph index of first character code.  0~if charmap is empty.
   *
   * @return:
   *   The charmap's first character code.
   *
   * @note:
   *   You should use this function together with @FT_Get_Next_Char to parse
   *   all character codes available in a given charmap.  The code should
   *   look like this:
   *
   *   ```
   *     FT_ULong  charcode;
   *     FT_UInt   gindex;
   *
   *
   *     charcode = FT_Get_First_Char( face, &gindex );
   *     while ( gindex != 0 )
   *     {
   *       ... do something with (charcode,gindex) pair ...
   *
   *       charcode = FT_Get_Next_Char( face, charcode, &gindex );
   *     }
   *   ```
   *
   *   Be aware that character codes can have values up to 0xFFFFFFFF; this
   *   might happen for non-Unicode or malformed cmaps.  However, even with
   *   regular Unicode encoding, so-called 'last resort fonts' (using SFNT
   *   cmap format 13, see function @FT_Get_CMap_Format) normally have
   *   entries for all Unicode characters up to 0x1FFFFF, which can cause *a
   *   lot* of iterations.
   *
   *   Note that `*agindex` is set to~0 if the charmap is empty.  The result
   *   itself can be~0 in two cases: if the charmap is empty or if the
   *   value~0 is the first valid character code.
   */
   FT_ULong 
  FT_Get_First_Char( FT_Face   face,
                     FT_UInt  *agindex );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Next_Char
   *
   * @description:
   *   Return the next character code in the current charmap of a given face
   *   following the value `char_code`, as well as the corresponding glyph
   *   index.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   *   char_code ::
   *     The starting character code.
   *
   * @output:
   *   agindex ::
   *     Glyph index of next character code.  0~if charmap is empty.
   *
   * @return:
   *   The charmap's next character code.
   *
   * @note:
   *   You should use this function with @FT_Get_First_Char to walk over all
   *   character codes available in a given charmap.  See the note for that
   *   function for a simple code example.
   *
   *   Note that `*agindex` is set to~0 when there are no more codes in the
   *   charmap.
   */
   FT_ULong 
  FT_Get_Next_Char( FT_Face    face,
                    FT_ULong   char_code,
                    FT_UInt   *agindex );


  /**************************************************************************
   *
   * @section:
   *   face_creation
   *
   */

  /**************************************************************************
   *
   * @function:
   *   FT_Face_Properties
   *
   * @description:
   *   Set or override certain (library or module-wide) properties on a
   *   face-by-face basis.  Useful for finer-grained control and avoiding
   *   locks on shared structures (threads can modify their own faces as they
   *   see fit).
   *
   *   Contrary to @FT_Property_Set, this function uses @FT_Parameter so that
   *   you can pass multiple properties to the target face in one call.  Note
   *   that only a subset of the available properties can be controlled.
   *
   *   * @FT_PARAM_TAG_STEM_DARKENING (stem darkening, corresponding to the
   *     property `no-stem-darkening` provided by the 'autofit', 'cff',
   *     'type1', and 't1cid' modules; see @no-stem-darkening).
   *
   *   * @FT_PARAM_TAG_LCD_FILTER_WEIGHTS (LCD filter weights, corresponding
   *     to function @FT_Library_SetLcdFilterWeights).
   *
   *   * @FT_PARAM_TAG_RANDOM_SEED (seed value for the CFF, Type~1, and CID
   *     'random' operator, corresponding to the `random-seed` property
   *     provided by the 'cff', 'type1', and 't1cid' modules; see
   *     @random-seed).
   *
   *   Pass `NULL` as `data` in @FT_Parameter for a given tag to reset the
   *   option and use the library or module default again.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   *   num_properties ::
   *     The number of properties that follow.
   *
   *   properties ::
   *     A handle to an @FT_Parameter array with `num_properties` elements.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @example:
   *   Here is an example that sets three properties.  You must define
   *   `FT_CONFIG_OPTION_SUBPIXEL_RENDERING` to make the LCD filter examples
   *   work.
   *
   *   ```
   *     FT_Parameter         property1;
   *     FT_Bool              darken_stems = 1;
   *
   *     FT_Parameter         property2;
   *     FT_LcdFiveTapFilter  custom_weight =
   *                            { 0x11, 0x44, 0x56, 0x44, 0x11 };
   *
   *     FT_Parameter         property3;
   *     FT_Int32             random_seed = 314159265;
   *
   *     FT_Parameter         properties[3] = { property1,
   *                                            property2,
   *                                            property3 };
   *
   *
   *     property1.tag  = FT_PARAM_TAG_STEM_DARKENING;
   *     property1.data = &darken_stems;
   *
   *     property2.tag  = FT_PARAM_TAG_LCD_FILTER_WEIGHTS;
   *     property2.data = custom_weight;
   *
   *     property3.tag  = FT_PARAM_TAG_RANDOM_SEED;
   *     property3.data = &random_seed;
   *
   *     FT_Face_Properties( face, 3, properties );
   *   ```
   *
   *   The next example resets a single property to its default value.
   *
   *   ```
   *     FT_Parameter  property;
   *
   *
   *     property.tag  = FT_PARAM_TAG_LCD_FILTER_WEIGHTS;
   *     property.data = NULL;
   *
   *     FT_Face_Properties( face, 1, &property );
   *   ```
   *
   * @since:
   *   2.8
   *
   */
   FT_Error 
  FT_Face_Properties( FT_Face        face,
                      FT_UInt        num_properties,
                      FT_Parameter*  properties );


  /**************************************************************************
   *
   * @section:
   *   information_retrieval
   *
   */

  /**************************************************************************
   *
   * @function:
   *   FT_Get_Name_Index
   *
   * @description:
   *   Return the glyph index of a given glyph name.  This only works
   *   for those faces where @FT_HAS_GLYPH_NAMES returns true.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   *   glyph_name ::
   *     The glyph name.
   *
   * @return:
   *   The glyph index.  0~means 'undefined character code'.
   *
   * @note:
   *   Acceptable glyph names might come from the [Adobe Glyph
   *   List](https://github.com/adobe-type-tools/agl-aglfn).  See
   *   @FT_Get_Glyph_Name for the inverse functionality.
   *
   *   This function has limited capabilities if the config macro
   *   `FT_CONFIG_OPTION_POSTSCRIPT_NAMES` is not defined in `ftoption.h`:
   *   It then works only for fonts that actually embed glyph names (which
   *   many recent OpenType fonts do not).
   */
   FT_UInt 
  FT_Get_Name_Index( FT_Face           face,
                     const FT_String*  glyph_name );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Glyph_Name
   *
   * @description:
   *   Retrieve the ASCII name of a given glyph in a face.  This only works
   *   for those faces where @FT_HAS_GLYPH_NAMES returns true.
   *
   * @input:
   *   face ::
   *     A handle to a source face object.
   *
   *   glyph_index ::
   *     The glyph index.
   *
   *   buffer_max ::
   *     The maximum number of bytes available in the buffer.
   *
   * @output:
   *   buffer ::
   *     A pointer to a target buffer where the name is copied to.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   An error is returned if the face doesn't provide glyph names or if the
   *   glyph index is invalid.  In all cases of failure, the first byte of
   *   `buffer` is set to~0 to indicate an empty name.
   *
   *   The glyph name is truncated to fit within the buffer if it is too
   *   long.  The returned string is always zero-terminated.
   *
   *   Be aware that FreeType reorders glyph indices internally so that glyph
   *   index~0 always corresponds to the 'missing glyph' (called '.notdef').
   *
   *   This function has limited capabilities if the config macro
   *   `FT_CONFIG_OPTION_POSTSCRIPT_NAMES` is not defined in `ftoption.h`:
   *   It then works only for fonts that actually embed glyph names (which
   *   many recent OpenType fonts do not).
   */
   FT_Error 
  FT_Get_Glyph_Name( FT_Face     face,
                     FT_UInt     glyph_index,
                     FT_Pointer  buffer,
                     FT_UInt     buffer_max );


  /**************************************************************************
   *
   * @function:
   *   FT_Get_Postscript_Name
   *
   * @description:
   *   Retrieve the ASCII PostScript name of a given face, if available.
   *   This only works with PostScript, TrueType, and OpenType fonts.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   * @return:
   *   A pointer to the face's PostScript name.  `NULL` if unavailable.
   *
   * @note:
   *   The returned pointer is owned by the face and is destroyed with it.
   *
   *   For variation fonts, this string changes if you select a different
   *   instance, and you have to call `FT_Get_PostScript_Name` again to
   *   retrieve it.  FreeType follows Adobe TechNote #5902, 'Generating
   *   PostScript Names for Fonts Using OpenType Font Variations'.
   *
   *     https://download.macromedia.com/pub/developer/opentype/tech-notes/5902.AdobePSNameGeneration.html
   *
   *   [Since 2.9] Special PostScript names for named instances are only
   *   returned if the named instance is set with @FT_Set_Named_Instance (and
   *   the font has corresponding entries in its 'fvar' table or is the
   *   default named instance).  If @FT_IS_VARIATION returns true, the
   *   algorithmically derived PostScript name is provided, not looking up
   *   special entries for named instances.
   */
   const char* 
  FT_Get_Postscript_Name( FT_Face  face );


  /**************************************************************************
   *
   * @enum:
   *   FT_SUBGLYPH_FLAG_XXX
   *
   * @description:
   *   A list of constants describing subglyphs.  Please refer to the 'glyf'
   *   table description in the OpenType specification for the meaning of the
   *   various flags (which get synthesized for non-OpenType subglyphs).
   *
   *     https://docs.microsoft.com/en-us/typography/opentype/spec/glyf#composite-glyph-description
   *
   * @values:
   *   FT_SUBGLYPH_FLAG_ARGS_ARE_WORDS ::
   *   FT_SUBGLYPH_FLAG_ARGS_ARE_XY_VALUES ::
   *   FT_SUBGLYPH_FLAG_ROUND_XY_TO_GRID ::
   *   FT_SUBGLYPH_FLAG_SCALE ::
   *   FT_SUBGLYPH_FLAG_XY_SCALE ::
   *   FT_SUBGLYPH_FLAG_2X2 ::
   *   FT_SUBGLYPH_FLAG_USE_MY_METRICS ::
   *
   */
#define FT_SUBGLYPH_FLAG_ARGS_ARE_WORDS          1
#define FT_SUBGLYPH_FLAG_ARGS_ARE_XY_VALUES      2
#define FT_SUBGLYPH_FLAG_ROUND_XY_TO_GRID        4
#define FT_SUBGLYPH_FLAG_SCALE                   8
#define FT_SUBGLYPH_FLAG_XY_SCALE             0x40
#define FT_SUBGLYPH_FLAG_2X2                  0x80
#define FT_SUBGLYPH_FLAG_USE_MY_METRICS      0x200


  /**************************************************************************
   *
   * @function:
   *   FT_Get_SubGlyph_Info
   *
   * @description:
   *   Retrieve a description of a given subglyph.  Only use it if
   *   `glyph->format` is @FT_GLYPH_FORMAT_COMPOSITE; an error is returned
   *   otherwise.
   *
   * @input:
   *   glyph ::
   *     The source glyph slot.
   *
   *   sub_index ::
   *     The index of the subglyph.  Must be less than
   *     `glyph->num_subglyphs`.
   *
   * @output:
   *   p_index ::
   *     The glyph index of the subglyph.
   *
   *   p_flags ::
   *     The subglyph flags, see @FT_SUBGLYPH_FLAG_XXX.
   *
   *   p_arg1 ::
   *     The subglyph's first argument (if any).
   *
   *   p_arg2 ::
   *     The subglyph's second argument (if any).
   *
   *   p_transform ::
   *     The subglyph transformation (if any).
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The values of `*p_arg1`, `*p_arg2`, and `*p_transform` must be
   *   interpreted depending on the flags returned in `*p_flags`.  See the
   *   OpenType specification for details.
   *
   *     https://docs.microsoft.com/en-us/typography/opentype/spec/glyf#composite-glyph-description
   *
   */
   FT_Error 
  FT_Get_SubGlyph_Info( FT_GlyphSlot  glyph,
                        FT_UInt       sub_index,
                        FT_Int       *p_index,
                        FT_UInt      *p_flags,
                        FT_Int       *p_arg1,
                        FT_Int       *p_arg2,
                        FT_Matrix    *p_transform );


  /**************************************************************************
   *
   * @enum:
   *   FT_FSTYPE_XXX
   *
   * @description:
   *   A list of bit flags used in the `fsType` field of the OS/2 table in a
   *   TrueType or OpenType font and the `FSType` entry in a PostScript font.
   *   These bit flags are returned by @FT_Get_FSType_Flags; they inform
   *   client applications of embedding and subsetting restrictions
   *   associated with a font.
   *
   *   See
   *   https://www.adobe.com/content/dam/Adobe/en/devnet/acrobat/pdfs/FontPolicies.pdf
   *   for more details.
   *
   * @values:
   *   FT_FSTYPE_INSTALLABLE_EMBEDDING ::
   *     Fonts with no fsType bit set may be embedded and permanently
   *     installed on the remote system by an application.
   *
   *   FT_FSTYPE_RESTRICTED_LICENSE_EMBEDDING ::
   *     Fonts that have only this bit set must not be modified, embedded or
   *     exchanged in any manner without first obtaining permission of the
   *     font software copyright owner.
   *
   *   FT_FSTYPE_PREVIEW_AND_PRINT_EMBEDDING ::
   *     The font may be embedded and temporarily loaded on the remote
   *     system.  Documents containing Preview & Print fonts must be opened
   *     'read-only'; no edits can be applied to the document.
   *
   *   FT_FSTYPE_EDITABLE_EMBEDDING ::
   *     The font may be embedded but must only be installed temporarily on
   *     other systems.  In contrast to Preview & Print fonts, documents
   *     containing editable fonts may be opened for reading, editing is
   *     permitted, and changes may be saved.
   *
   *   FT_FSTYPE_NO_SUBSETTING ::
   *     The font may not be subsetted prior to embedding.
   *
   *   FT_FSTYPE_BITMAP_EMBEDDING_ONLY ::
   *     Only bitmaps contained in the font may be embedded; no outline data
   *     may be embedded.  If there are no bitmaps available in the font,
   *     then the font is unembeddable.
   *
   * @note:
   *   The flags are ORed together, thus more than a single value can be
   *   returned.
   *
   *   While the `fsType` flags can indicate that a font may be embedded, a
   *   license with the font vendor may be separately required to use the
   *   font in this way.
   */
#define FT_FSTYPE_INSTALLABLE_EMBEDDING         0x0000
#define FT_FSTYPE_RESTRICTED_LICENSE_EMBEDDING  0x0002
#define FT_FSTYPE_PREVIEW_AND_PRINT_EMBEDDING   0x0004
#define FT_FSTYPE_EDITABLE_EMBEDDING            0x0008
#define FT_FSTYPE_NO_SUBSETTING                 0x0100
#define FT_FSTYPE_BITMAP_EMBEDDING_ONLY         0x0200


  /**************************************************************************
   *
   * @function:
   *   FT_Get_FSType_Flags
   *
   * @description:
   *   Return the `fsType` flags for a font.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   * @return:
   *   The `fsType` flags, see @FT_FSTYPE_XXX.
   *
   * @note:
   *   Use this function rather than directly reading the `fs_type` field in
   *   the @PS_FontInfoRec structure, which is only guaranteed to return the
   *   correct results for Type~1 fonts.
   *
   * @since:
   *   2.3.8
   *
   */
   FT_UShort 
  FT_Get_FSType_Flags( FT_Face  face );


  /**************************************************************************
   *
   * @section:
   *   glyph_variants
   *
   * @title:
   *   Unicode Variation Sequences
   *
   * @abstract:
   *   The FreeType~2 interface to Unicode Variation Sequences (UVS), using
   *   the SFNT cmap format~14.
   *
   * @description:
   *   Many characters, especially for CJK scripts, have variant forms.  They
   *   are a sort of grey area somewhere between being totally irrelevant and
   *   semantically distinct; for this reason, the Unicode consortium decided
   *   to introduce Variation Sequences (VS), consisting of a Unicode base
   *   character and a variation selector instead of further extending the
   *   already huge number of characters.
   *
   *   Unicode maintains two different sets, namely 'Standardized Variation
   *   Sequences' and registered 'Ideographic Variation Sequences' (IVS),
   *   collected in the 'Ideographic Variation Database' (IVD).
   *
   *     https://unicode.org/Public/UCD/latest/ucd/StandardizedVariants.txt
   *     https://unicode.org/reports/tr37/ https://unicode.org/ivd/
   *
   *   To date (January 2017), the character with the most ideographic
   *   variations is U+9089, having 32 such IVS.
   *
   *   Three Mongolian Variation Selectors have the values U+180B-U+180D; 256
   *   generic Variation Selectors are encoded in the ranges U+FE00-U+FE0F
   *   and U+E0100-U+E01EF.  IVS currently use Variation Selectors from the
   *   range U+E0100-U+E01EF only.
   *
   *   A VS consists of the base character value followed by a single
   *   Variation Selector.  For example, to get the first variation of
   *   U+9089, you have to write the character sequence `U+9089 U+E0100`.
   *
   *   Adobe and MS decided to support both standardized and ideographic VS
   *   with a new cmap subtable (format~14).  It is an odd subtable because
   *   it is not a mapping of input code points to glyphs, but contains lists
   *   of all variations supported by the font.
   *
   *   A variation may be either 'default' or 'non-default' for a given font.
   *   A default variation is the one you will get for that code point if you
   *   look it up in the standard Unicode cmap.  A non-default variation is a
   *   different glyph.
   *
   */


  /**************************************************************************
   *
   * @function:
   *   FT_Face_GetCharVariantIndex
   *
   * @description:
   *   Return the glyph index of a given character code as modified by the
   *   variation selector.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   *   charcode ::
   *     The character code point in Unicode.
   *
   *   variantSelector ::
   *     The Unicode code point of the variation selector.
   *
   * @return:
   *   The glyph index.  0~means either 'undefined character code', or
   *   'undefined selector code', or 'no variation selector cmap subtable',
   *   or 'current CharMap is not Unicode'.
   *
   * @note:
   *   If you use FreeType to manipulate the contents of font files directly,
   *   be aware that the glyph index returned by this function doesn't always
   *   correspond to the internal indices used within the file.  This is done
   *   to ensure that value~0 always corresponds to the 'missing glyph'.
   *
   *   This function is only meaningful if
   *     a) the font has a variation selector cmap sub table, and
   *     b) the current charmap has a Unicode encoding.
   *
   * @since:
   *   2.3.6
   *
   */
   FT_UInt 
  FT_Face_GetCharVariantIndex( FT_Face   face,
                               FT_ULong  charcode,
                               FT_ULong  variantSelector );


  /**************************************************************************
   *
   * @function:
   *   FT_Face_GetCharVariantIsDefault
   *
   * @description:
   *   Check whether this variation of this Unicode character is the one to
   *   be found in the charmap.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   *   charcode ::
   *     The character codepoint in Unicode.
   *
   *   variantSelector ::
   *     The Unicode codepoint of the variation selector.
   *
   * @return:
   *   1~if found in the standard (Unicode) cmap, 0~if found in the variation
   *   selector cmap, or -1 if it is not a variation.
   *
   * @note:
   *   This function is only meaningful if the font has a variation selector
   *   cmap subtable.
   *
   * @since:
   *   2.3.6
   *
   */
   FT_Int 
  FT_Face_GetCharVariantIsDefault( FT_Face   face,
                                   FT_ULong  charcode,
                                   FT_ULong  variantSelector );


  /**************************************************************************
   *
   * @function:
   *   FT_Face_GetVariantSelectors
   *
   * @description:
   *   Return a zero-terminated list of Unicode variation selectors found in
   *   the font.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   * @return:
   *   A pointer to an array of selector code points, or `NULL` if there is
   *   no valid variation selector cmap subtable.
   *
   * @note:
   *   The last item in the array is~0; the array is owned by the @FT_Face
   *   object but can be overwritten or released on the next call to a
   *   FreeType function.
   *
   * @since:
   *   2.3.6
   *
   */
   FT_UInt32* 
  FT_Face_GetVariantSelectors( FT_Face  face );


  /**************************************************************************
   *
   * @function:
   *   FT_Face_GetVariantsOfChar
   *
   * @description:
   *   Return a zero-terminated list of Unicode variation selectors found for
   *   the specified character code.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   *   charcode ::
   *     The character codepoint in Unicode.
   *
   * @return:
   *   A pointer to an array of variation selector code points that are
   *   active for the given character, or `NULL` if the corresponding list is
   *   empty.
   *
   * @note:
   *   The last item in the array is~0; the array is owned by the @FT_Face
   *   object but can be overwritten or released on the next call to a
   *   FreeType function.
   *
   * @since:
   *   2.3.6
   *
   */
   FT_UInt32* 
  FT_Face_GetVariantsOfChar( FT_Face   face,
                             FT_ULong  charcode );


  /**************************************************************************
   *
   * @function:
   *   FT_Face_GetCharsOfVariant
   *
   * @description:
   *   Return a zero-terminated list of Unicode character codes found for the
   *   specified variation selector.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   *   variantSelector ::
   *     The variation selector code point in Unicode.
   *
   * @return:
   *   A list of all the code points that are specified by this selector
   *   (both default and non-default codes are returned) or `NULL` if there
   *   is no valid cmap or the variation selector is invalid.
   *
   * @note:
   *   The last item in the array is~0; the array is owned by the @FT_Face
   *   object but can be overwritten or released on the next call to a
   *   FreeType function.
   *
   * @since:
   *   2.3.6
   *
   */
   FT_UInt32* 
  FT_Face_GetCharsOfVariant( FT_Face   face,
                             FT_ULong  variantSelector );


  /**************************************************************************
   *
   * @section:
   *   computations
   *
   * @title:
   *   Computations
   *
   * @abstract:
   *   Crunching fixed numbers and vectors.
   *
   * @description:
   *   This section contains various functions used to perform computations
   *   on 16.16 fixed-point numbers or 2D vectors.  FreeType does not use
   *   floating-point data types.
   *
   *   **Attention**: Most arithmetic functions take `FT_Long` as arguments.
   *   For historical reasons, FreeType was designed under the assumption
   *   that `FT_Long` is a 32-bit integer; results can thus be undefined if
   *   the arguments don't fit into 32 bits.
   *
   * @order:
   *   FT_MulDiv
   *   FT_MulFix
   *   FT_DivFix
   *   FT_RoundFix
   *   FT_CeilFix
   *   FT_FloorFix
   *   FT_Vector_Transform
   *   FT_Matrix_Multiply
   *   FT_Matrix_Invert
   *
   */


  /**************************************************************************
   *
   * @function:
   *   FT_MulDiv
   *
   * @description:
   *   Compute `(a*b)/c` with maximum accuracy, using a 64-bit intermediate
   *   integer whenever necessary.
   *
   *   This function isn't necessarily as fast as some processor-specific
   *   operations, but is at least completely portable.
   *
   * @input:
   *   a ::
   *     The first multiplier.
   *
   *   b ::
   *     The second multiplier.
   *
   *   c ::
   *     The divisor.
   *
   * @return:
   *   The result of `(a*b)/c`.  This function never traps when trying to
   *   divide by zero; it simply returns 'MaxInt' or 'MinInt' depending on
   *   the signs of `a` and `b`.
   */
   FT_Long 
  FT_MulDiv( FT_Long  a,
             FT_Long  b,
             FT_Long  c );


  /**************************************************************************
   *
   * @function:
   *   FT_MulFix
   *
   * @description:
   *   Compute `(a*b)/0x10000` with maximum accuracy.  Its main use is to
   *   multiply a given value by a 16.16 fixed-point factor.
   *
   * @input:
   *   a ::
   *     The first multiplier.
   *
   *   b ::
   *     The second multiplier.  Use a 16.16 factor here whenever possible
   *     (see note below).
   *
   * @return:
   *   The result of `(a*b)/0x10000`.
   *
   * @note:
   *   This function has been optimized for the case where the absolute value
   *   of `a` is less than 2048, and `b` is a 16.16 scaling factor.  As this
   *   happens mainly when scaling from notional units to fractional pixels
   *   in FreeType, it resulted in noticeable speed improvements between
   *   versions 2.x and 1.x.
   *
   *   As a conclusion, always try to place a 16.16 factor as the _second_
   *   argument of this function; this can make a great difference.
   */
   FT_Long 
  FT_MulFix( FT_Long  a,
             FT_Long  b );


  /**************************************************************************
   *
   * @function:
   *   FT_DivFix
   *
   * @description:
   *   Compute `(a*0x10000)/b` with maximum accuracy.  Its main use is to
   *   divide a given value by a 16.16 fixed-point factor.
   *
   * @input:
   *   a ::
   *     The numerator.
   *
   *   b ::
   *     The denominator.  Use a 16.16 factor here.
   *
   * @return:
   *   The result of `(a*0x10000)/b`.
   */
   FT_Long 
  FT_DivFix( FT_Long  a,
             FT_Long  b );


  /**************************************************************************
   *
   * @function:
   *   FT_RoundFix
   *
   * @description:
   *   Round a 16.16 fixed number.
   *
   * @input:
   *   a ::
   *     The number to be rounded.
   *
   * @return:
   *   `a` rounded to the nearest 16.16 fixed integer, halfway cases away
   *   from zero.
   *
   * @note:
   *   The function uses wrap-around arithmetic.
   */
   FT_Fixed 
  FT_RoundFix( FT_Fixed  a );


  /**************************************************************************
   *
   * @function:
   *   FT_CeilFix
   *
   * @description:
   *   Compute the smallest following integer of a 16.16 fixed number.
   *
   * @input:
   *   a ::
   *     The number for which the ceiling function is to be computed.
   *
   * @return:
   *   `a` rounded towards plus infinity.
   *
   * @note:
   *   The function uses wrap-around arithmetic.
   */
   FT_Fixed 
  FT_CeilFix( FT_Fixed  a );


  /**************************************************************************
   *
   * @function:
   *   FT_FloorFix
   *
   * @description:
   *   Compute the largest previous integer of a 16.16 fixed number.
   *
   * @input:
   *   a ::
   *     The number for which the floor function is to be computed.
   *
   * @return:
   *   `a` rounded towards minus infinity.
   */
   FT_Fixed 
  FT_FloorFix( FT_Fixed  a );


  /**************************************************************************
   *
   * @function:
   *   FT_Vector_Transform
   *
   * @description:
   *   Transform a single vector through a 2x2 matrix.
   *
   * @inout:
   *   vector ::
   *     The target vector to transform.
   *
   * @input:
   *   matrix ::
   *     A pointer to the source 2x2 matrix.
   *
   * @note:
   *   The result is undefined if either `vector` or `matrix` is invalid.
   */
   void 
  FT_Vector_Transform( FT_Vector*        vector,
                       const FT_Matrix*  matrix );


  /**************************************************************************
   *
   * @section:
   *   library_setup
   *
   */

  /**************************************************************************
   *
   * @enum:
   *   FREETYPE_XXX
   *
   * @description:
   *   These three macros identify the FreeType source code version.  Use
   *   @FT_Library_Version to access them at runtime.
   *
   * @values:
   *   FREETYPE_MAJOR ::
   *     The major version number.
   *   FREETYPE_MINOR ::
   *     The minor version number.
   *   FREETYPE_PATCH ::
   *     The patch level.
   *
   * @note:
   *   The version number of FreeType if built as a dynamic link library with
   *   the 'libtool' package is _not_ controlled by these three macros.
   *
   */
#define FREETYPE_MAJOR  2
#define FREETYPE_MINOR  13
#define FREETYPE_PATCH  2


  /**************************************************************************
   *
   * @function:
   *   FT_Library_Version
   *
   * @description:
   *   Return the version of the FreeType library being used.  This is useful
   *   when dynamically linking to the library, since one cannot use the
   *   macros @FREETYPE_MAJOR, @FREETYPE_MINOR, and @FREETYPE_PATCH.
   *
   * @input:
   *   library ::
   *     A source library handle.
   *
   * @output:
   *   amajor ::
   *     The major version number.
   *
   *   aminor ::
   *     The minor version number.
   *
   *   apatch ::
   *     The patch version number.
   *
   * @note:
   *   The reason why this function takes a `library` argument is because
   *   certain programs implement library initialization in a custom way that
   *   doesn't use @FT_Init_FreeType.
   *
   *   In such cases, the library version might not be available before the
   *   library object has been created.
   */
   void 
  FT_Library_Version( FT_Library   library,
                      FT_Int      *amajor,
                      FT_Int      *aminor,
                      FT_Int      *apatch );


  /**************************************************************************
   *
   * @section:
   *   other_api_data
   *
   */

  /**************************************************************************
   *
   * @function:
   *   FT_Face_CheckTrueTypePatents
   *
   * @description:
   *   Deprecated, does nothing.
   *
   * @input:
   *   face ::
   *     A face handle.
   *
   * @return:
   *   Always returns false.
   *
   * @note:
   *   Since May 2010, TrueType hinting is no longer patented.
   *
   * @since:
   *   2.3.5
   *
   */
   FT_Bool 
  FT_Face_CheckTrueTypePatents( FT_Face  face );


  /**************************************************************************
   *
   * @function:
   *   FT_Face_SetUnpatentedHinting
   *
   * @description:
   *   Deprecated, does nothing.
   *
   * @input:
   *   face ::
   *     A face handle.
   *
   *   value ::
   *     New boolean setting.
   *
   * @return:
   *   Always returns false.
   *
   * @note:
   *   Since May 2010, TrueType hinting is no longer patented.
   *
   * @since:
   *   2.3.5
   *
   */
   FT_Bool 
  FT_Face_SetUnpatentedHinting( FT_Face  face,
                                FT_Bool  value );

  /* */




#endif /* FREETYPE_H_ */


/* END */
//
// ===========================  fttrigon.h  ===========================
//
/****************************************************************************
 *
 * fttrigon.h
 *
 *   FreeType trigonometric functions (specification).
 *
 * Copyright (C) 2001-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTTRIGON_H_
#define FTTRIGON_H_

#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif





  /**************************************************************************
   *
   * @section:
   *  computations
   *
   */


  /**************************************************************************
   *
   * @type:
   *   FT_Angle
   *
   * @description:
   *   This type is used to model angle values in FreeType.  Note that the
   *   angle is a 16.16 fixed-point value expressed in degrees.
   *
   */
  typedef FT_Fixed  FT_Angle;


  /**************************************************************************
   *
   * @macro:
   *   FT_ANGLE_PI
   *
   * @description:
   *   The angle pi expressed in @FT_Angle units.
   *
   */
#define FT_ANGLE_PI  ( 180L << 16 )


  /**************************************************************************
   *
   * @macro:
   *   FT_ANGLE_2PI
   *
   * @description:
   *   The angle 2*pi expressed in @FT_Angle units.
   *
   */
// #define FT_ANGLE_2PI  ( FT_ANGLE_PI * 2 )


  /**************************************************************************
   *
   * @macro:
   *   FT_ANGLE_PI2
   *
   * @description:
   *   The angle pi/2 expressed in @FT_Angle units.
   *
   */
#define FT_ANGLE_PI2  ( FT_ANGLE_PI / 2 )


  /**************************************************************************
   *
   * @macro:
   *   FT_ANGLE_PI4
   *
   * @description:
   *   The angle pi/4 expressed in @FT_Angle units.
   *
   */
#define FT_ANGLE_PI4  ( FT_ANGLE_PI / 4 )


  /**************************************************************************
   *
   * @function:
   *   FT_Sin
   *
   * @description:
   *   Return the sinus of a given angle in fixed-point format.
   *
   * @input:
   *   angle ::
   *     The input angle.
   *
   * @return:
   *   The sinus value.
   *
   * @note:
   *   If you need both the sinus and cosinus for a given angle, use the
   *   function @FT_Vector_Unit.
   *
   */
   FT_Fixed 
  FT_Sin( FT_Angle  angle );


  /**************************************************************************
   *
   * @function:
   *   FT_Cos
   *
   * @description:
   *   Return the cosinus of a given angle in fixed-point format.
   *
   * @input:
   *   angle ::
   *     The input angle.
   *
   * @return:
   *   The cosinus value.
   *
   * @note:
   *   If you need both the sinus and cosinus for a given angle, use the
   *   function @FT_Vector_Unit.
   *
   */
   FT_Fixed 
  FT_Cos( FT_Angle  angle );


  /**************************************************************************
   *
   * @function:
   *   FT_Tan
   *
   * @description:
   *   Return the tangent of a given angle in fixed-point format.
   *
   * @input:
   *   angle ::
   *     The input angle.
   *
   * @return:
   *   The tangent value.
   *
   */
   FT_Fixed 
  FT_Tan( FT_Angle  angle );


  /**************************************************************************
   *
   * @function:
   *   FT_Atan2
   *
   * @description:
   *   Return the arc-tangent corresponding to a given vector (x,y) in the 2d
   *   plane.
   *
   * @input:
   *   x ::
   *     The horizontal vector coordinate.
   *
   *   y ::
   *     The vertical vector coordinate.
   *
   * @return:
   *   The arc-tangent value (i.e. angle).
   *
   */
   FT_Angle 
  FT_Atan2( FT_Fixed  x,
            FT_Fixed  y );


  /**************************************************************************
   *
   * @function:
   *   FT_Angle_Diff
   *
   * @description:
   *   Return the difference between two angles.  The result is always
   *   constrained to the ]-PI..PI] interval.
   *
   * @input:
   *   angle1 ::
   *     First angle.
   *
   *   angle2 ::
   *     Second angle.
   *
   * @return:
   *   Constrained value of `angle2-angle1`.
   *
   */
   FT_Angle 
  FT_Angle_Diff( FT_Angle  angle1,
                 FT_Angle  angle2 );


  /**************************************************************************
   *
   * @function:
   *   FT_Vector_Unit
   *
   * @description:
   *   Return the unit vector corresponding to a given angle.  After the
   *   call, the value of `vec.x` will be `cos(angle)`, and the value of
   *   `vec.y` will be `sin(angle)`.
   *
   *   This function is useful to retrieve both the sinus and cosinus of a
   *   given angle quickly.
   *
   * @output:
   *   vec ::
   *     The address of target vector.
   *
   * @input:
   *   angle ::
   *     The input angle.
   *
   */
   void 
  FT_Vector_Unit( FT_Vector*  vec,
                  FT_Angle    angle );


  /**************************************************************************
   *
   * @function:
   *   FT_Vector_Rotate
   *
   * @description:
   *   Rotate a vector by a given angle.
   *
   * @inout:
   *   vec ::
   *     The address of target vector.
   *
   * @input:
   *   angle ::
   *     The input angle.
   *
   */
   void 
  FT_Vector_Rotate( FT_Vector*  vec,
                    FT_Angle    angle );


  /**************************************************************************
   *
   * @function:
   *   FT_Vector_Length
   *
   * @description:
   *   Return the length of a given vector.
   *
   * @input:
   *   vec ::
   *     The address of target vector.
   *
   * @return:
   *   The vector length, expressed in the same units that the original
   *   vector coordinates.
   *
   */
   FT_Fixed 
  FT_Vector_Length( FT_Vector*  vec );


  /**************************************************************************
   *
   * @function:
   *   FT_Vector_Polarize
   *
   * @description:
   *   Compute both the length and angle of a given vector.
   *
   * @input:
   *   vec ::
   *     The address of source vector.
   *
   * @output:
   *   length ::
   *     The vector length.
   *
   *   angle ::
   *     The vector angle.
   *
   */
   void 
  FT_Vector_Polarize( FT_Vector*  vec,
                      FT_Fixed   *length,
                      FT_Angle   *angle );


  /**************************************************************************
   *
   * @function:
   *   FT_Vector_From_Polar
   *
   * @description:
   *   Compute vector coordinates from a length and angle.
   *
   * @output:
   *   vec ::
   *     The address of source vector.
   *
   * @input:
   *   length ::
   *     The vector length.
   *
   *   angle ::
   *     The vector angle.
   *
   */
   void 
  FT_Vector_From_Polar( FT_Vector*  vec,
                        FT_Fixed    length,
                        FT_Angle    angle );

  /* */




#endif /* FTTRIGON_H_ */


/* END */
//
// ===========================  ftincrem.h  ===========================
//
/****************************************************************************
 *
 * ftincrem.h
 *
 *   FreeType incremental loading (specification).
 *
 * Copyright (C) 2002-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTINCREM_H_
#define FTINCREM_H_

#include <freetype/freetype.h>
#include <freetype/ftparams.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif




  /**************************************************************************
   *
   * @section:
   *    incremental
   *
   * @title:
   *    Incremental Loading
   *
   * @abstract:
   *    Custom Glyph Loading.
   *
   * @description:
   *   This section contains various functions used to perform so-called
   *   'incremental' glyph loading.  This is a mode where all glyphs loaded
   *   from a given @FT_Face are provided by the client application.
   *
   *   Apart from that, all other tables are loaded normally from the font
   *   file.  This mode is useful when FreeType is used within another
   *   engine, e.g., a PostScript Imaging Processor.
   *
   *   To enable this mode, you must use @FT_Open_Face, passing an
   *   @FT_Parameter with the @FT_PARAM_TAG_INCREMENTAL tag and an
   *   @FT_Incremental_Interface value.  See the comments for
   *   @FT_Incremental_InterfaceRec for an example.
   *
   */


  /**************************************************************************
   *
   * @type:
   *   FT_Incremental
   *
   * @description:
   *   An opaque type describing a user-provided object used to implement
   *   'incremental' glyph loading within FreeType.  This is used to support
   *   embedded fonts in certain environments (e.g., PostScript
   *   interpreters), where the glyph data isn't in the font file, or must be
   *   overridden by different values.
   *
   * @note:
   *   It is up to client applications to create and implement
   *   @FT_Incremental objects, as long as they provide implementations for
   *   the methods @FT_Incremental_GetGlyphDataFunc,
   *   @FT_Incremental_FreeGlyphDataFunc and
   *   @FT_Incremental_GetGlyphMetricsFunc.
   *
   *   See the description of @FT_Incremental_InterfaceRec to understand how
   *   to use incremental objects with FreeType.
   *
   */
  typedef struct FT_IncrementalRec_*  FT_Incremental;


  /**************************************************************************
   *
   * @struct:
   *   FT_Incremental_MetricsRec
   *
   * @description:
   *   A small structure used to contain the basic glyph metrics returned by
   *   the @FT_Incremental_GetGlyphMetricsFunc method.
   *
   * @fields:
   *   bearing_x ::
   *     Left bearing, in font units.
   *
   *   bearing_y ::
   *     Top bearing, in font units.
   *
   *   advance ::
   *     Horizontal component of glyph advance, in font units.
   *
   *   advance_v ::
   *     Vertical component of glyph advance, in font units.
   *
   * @note:
   *   These correspond to horizontal or vertical metrics depending on the
   *   value of the `vertical` argument to the function
   *   @FT_Incremental_GetGlyphMetricsFunc.
   *
   */
  typedef struct  FT_Incremental_MetricsRec_
  {
    FT_Long  bearing_x;
    FT_Long  bearing_y;
    FT_Long  advance;
    FT_Long  advance_v;     /* since 2.3.12 */

  } FT_Incremental_MetricsRec;


  /**************************************************************************
   *
   * @struct:
   *   FT_Incremental_Metrics
   *
   * @description:
   *   A handle to an @FT_Incremental_MetricsRec structure.
   *
   */
   typedef struct FT_Incremental_MetricsRec_*  FT_Incremental_Metrics;


  /**************************************************************************
   *
   * @type:
   *   FT_Incremental_GetGlyphDataFunc
   *
   * @description:
   *   A function called by FreeType to access a given glyph's data bytes
   *   during @FT_Load_Glyph or @FT_Load_Char if incremental loading is
   *   enabled.
   *
   *   Note that the format of the glyph's data bytes depends on the font
   *   file format.  For TrueType, it must correspond to the raw bytes within
   *   the 'glyf' table.  For PostScript formats, it must correspond to the
   *   **unencrypted** charstring bytes, without any `lenIV` header.  It is
   *   undefined for any other format.
   *
   * @input:
   *   incremental ::
   *     Handle to an opaque @FT_Incremental handle provided by the client
   *     application.
   *
   *   glyph_index ::
   *     Index of relevant glyph.
   *
   * @output:
   *   adata ::
   *     A structure describing the returned glyph data bytes (which will be
   *     accessed as a read-only byte block).
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   If this function returns successfully the method
   *   @FT_Incremental_FreeGlyphDataFunc will be called later to release the
   *   data bytes.
   *
   *   Nested calls to @FT_Incremental_GetGlyphDataFunc can happen for
   *   compound glyphs.
   *
   */
  typedef FT_Error
  (*FT_Incremental_GetGlyphDataFunc)( FT_Incremental  incremental,
                                      FT_UInt         glyph_index,
                                      FT_Data*        adata );


  /**************************************************************************
   *
   * @type:
   *   FT_Incremental_FreeGlyphDataFunc
   *
   * @description:
   *   A function used to release the glyph data bytes returned by a
   *   successful call to @FT_Incremental_GetGlyphDataFunc.
   *
   * @input:
   *   incremental ::
   *     A handle to an opaque @FT_Incremental handle provided by the client
   *     application.
   *
   *   data ::
   *     A structure describing the glyph data bytes (which will be accessed
   *     as a read-only byte block).
   *
   */
  typedef void
  (*FT_Incremental_FreeGlyphDataFunc)( FT_Incremental  incremental,
                                       FT_Data*        data );


  /**************************************************************************
   *
   * @type:
   *   FT_Incremental_GetGlyphMetricsFunc
   *
   * @description:
   *   A function used to retrieve the basic metrics of a given glyph index
   *   before accessing its data.  This allows for handling font types such
   *   as PCL~XL Format~1, Class~2 downloaded TrueType fonts, where the glyph
   *   metrics (`hmtx` and `vmtx` tables) are permitted to be omitted from
   *   the font, and the relevant metrics included in the header of the glyph
   *   outline data.  Importantly, this is not intended to allow custom glyph
   *   metrics (for example, Postscript Metrics dictionaries), because that
   *   conflicts with the requirements of outline hinting.  Such custom
   *   metrics must be handled separately, by the calling application.
   *
   * @input:
   *   incremental ::
   *     A handle to an opaque @FT_Incremental handle provided by the client
   *     application.
   *
   *   glyph_index ::
   *     Index of relevant glyph.
   *
   *   vertical ::
   *     If true, return vertical metrics.
   *
   *   ametrics ::
   *     This parameter is used for both input and output.  The original
   *     glyph metrics, if any, in font units.  If metrics are not available
   *     all the values must be set to zero.
   *
   * @output:
   *   ametrics ::
   *     The glyph metrics in font units.
   *
   */
  typedef FT_Error
  (*FT_Incremental_GetGlyphMetricsFunc)
                      ( FT_Incremental              incremental,
                        FT_UInt                     glyph_index,
                        FT_Bool                     vertical,
                        FT_Incremental_MetricsRec  *ametrics );


  /**************************************************************************
   *
   * @struct:
   *   FT_Incremental_FuncsRec
   *
   * @description:
   *   A table of functions for accessing fonts that load data incrementally.
   *   Used in @FT_Incremental_InterfaceRec.
   *
   * @fields:
   *   get_glyph_data ::
   *     The function to get glyph data.  Must not be null.
   *
   *   free_glyph_data ::
   *     The function to release glyph data.  Must not be null.
   *
   *   get_glyph_metrics ::
   *     The function to get glyph metrics.  May be null if the font does not
   *     require it.
   *
   */
  typedef struct  FT_Incremental_FuncsRec_
  {
    FT_Incremental_GetGlyphDataFunc     get_glyph_data;
    FT_Incremental_FreeGlyphDataFunc    free_glyph_data;
    FT_Incremental_GetGlyphMetricsFunc  get_glyph_metrics;

  } FT_Incremental_FuncsRec;


  /**************************************************************************
   *
   * @struct:
   *   FT_Incremental_InterfaceRec
   *
   * @description:
   *   A structure to be used with @FT_Open_Face to indicate that the user
   *   wants to support incremental glyph loading.  You should use it with
   *   @FT_PARAM_TAG_INCREMENTAL as in the following example:
   *
   *   ```
   *     FT_Incremental_InterfaceRec  inc_int;
   *     FT_Parameter                 parameter;
   *     FT_Open_Args                 open_args;
   *
   *
   *     // set up incremental descriptor
   *     inc_int.funcs  = my_funcs;
   *     inc_int.object = my_object;
   *
   *     // set up optional parameter
   *     parameter.tag  = FT_PARAM_TAG_INCREMENTAL;
   *     parameter.data = &inc_int;
   *
   *     // set up FT_Open_Args structure
   *     open_args.flags      = FT_OPEN_PATHNAME | FT_OPEN_PARAMS;
   *     open_args.pathname   = my_font_pathname;
   *     open_args.num_params = 1;
   *     open_args.params     = &parameter; // we use one optional argument
   *
   *     // open the font
   *     error = FT_Open_Face( library, &open_args, index, &face );
   *     ...
   *   ```
   *
   */
  typedef struct  FT_Incremental_InterfaceRec_
  {
    const FT_Incremental_FuncsRec*  funcs;
    FT_Incremental                  object;

  } FT_Incremental_InterfaceRec;


  /**************************************************************************
   *
   * @type:
   *   FT_Incremental_Interface
   *
   * @description:
   *   A pointer to an @FT_Incremental_InterfaceRec structure.
   *
   */
  typedef FT_Incremental_InterfaceRec*   FT_Incremental_Interface;


  /* */




#endif /* FTINCREM_H_ */


/* END */
//
// ===========================  fterrors.h  ===========================
//
/****************************************************************************
 *
 * fterrors.h
 *
 *   FreeType error code handling (specification).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


  /**************************************************************************
   *
   * @section:
   *   error_enumerations
   *
   * @title:
   *   Error Enumerations
   *
   * @abstract:
   *   How to handle errors and error strings.
   *
   * @description:
   *   The header file `fterrors.h` (which is automatically included by
   *   `freetype.h`) defines the handling of FreeType's enumeration
   *   constants.  It can also be used to generate error message strings
   *   with a small macro trick explained below.
   *
   *   **Error Formats**
   *
   *   The configuration macro `FT_CONFIG_OPTION_USE_MODULE_ERRORS` can be
   *   defined in `ftoption.h` in order to make the higher byte indicate the
   *   module where the error has happened (this is not compatible with
   *   standard builds of FreeType~2, however).  See the file `ftmoderr.h`
   *   for more details.
   *
   *   **Error Message Strings**
   *
   *   Error definitions are set up with special macros that allow client
   *   applications to build a table of error message strings.  The strings
   *   are not included in a normal build of FreeType~2 to save space (most
   *   client applications do not use them).
   *
   *   To do so, you have to define the following macros before including
   *   this file.
   *
   *   ```
   *     FT_ERROR_START_LIST
   *   ```
   *
   *   This macro is called before anything else to define the start of the
   *   error list.  It is followed by several `FT_ERROR_DEF` calls.
   *
   *   ```
   *     FT_ERROR_DEF( e, v, s )
   *   ```
   *
   *   This macro is called to define one single error.  'e' is the error
   *   code identifier (e.g., `Invalid_Argument`), 'v' is the error's
   *   numerical value, and 's' is the corresponding error string.
   *
   *   ```
   *     FT_ERROR_END_LIST
   *   ```
   *
   *   This macro ends the list.
   *
   *   Additionally, you have to undefine `FTERRORS_H_` before #including
   *   this file.
   *
   *   Here is a simple example.
   *
   *   ```
   *     #undef FTERRORS_H_
   *     #define FT_ERRORDEF( e, v, s )  { e, s },
   *     #define FT_ERROR_START_LIST     {
   *     #define FT_ERROR_END_LIST       { 0, NULL } };
   *
   *     const struct
   *     {
   *       int          err_code;
   *       const char*  err_msg;
   *     } ft_errors[] =
   *
   *     #include <freetype/fterrors.h>
   *   ```
   *
   *   An alternative to using an array is a switch statement.
   *
   *   ```
   *     #undef FTERRORS_H_
   *     #define FT_ERROR_START_LIST     switch ( error_code ) {
   *     #define FT_ERRORDEF( e, v, s )    case v: return s;
   *     #define FT_ERROR_END_LIST       }
   *   ```
   *
   *   If you use `FT_CONFIG_OPTION_USE_MODULE_ERRORS`, `error_code` should
   *   be replaced with `FT_ERROR_BASE(error_code)` in the last example.
   */

  /* */

  /* In previous FreeType versions we used `__FTERRORS_H__`.  However, */
  /* using two successive underscores in a non-system symbol name      */
  /* violates the C (and C++) standard, so it was changed to the       */
  /* current form.  In spite of this, we have to make                  */
  /*                                                                   */
  /* ```                                                               */
  /*   #undefine __FTERRORS_H__                                        */
  /* ```                                                               */
  /*                                                                   */
  /* work for backward compatibility.                                  */
  /*                                                                   */
#if !( defined( FTERRORS_H_ ) && defined ( __FTERRORS_H__ ) )
#define FTERRORS_H_
#define __FTERRORS_H__


  /* include module base error codes */
#include <freetype/ftmoderr.h>


  /*******************************************************************/
  /*******************************************************************/
  /*****                                                         *****/
  /*****                       SETUP MACROS                      *****/
  /*****                                                         *****/
  /*******************************************************************/
  /*******************************************************************/


#undef  FT_NEED_EXTERN_C


  /* FT_ERR_PREFIX is used as a prefix for error identifiers. */
  /* By default, we use `FT_Err_`.                            */
  /*                                                          */
#ifndef FT_ERR_PREFIX
#define FT_ERR_PREFIX  FT_Err_
#endif


  /* FT_ERR_BASE is used as the base for module-specific errors. */
  /*                                                             */
#ifdef FT_CONFIG_OPTION_USE_MODULE_ERRORS

#ifndef FT_ERR_BASE
#define FT_ERR_BASE  FT_Mod_Err_Base
#endif

#else

#undef FT_ERR_BASE
#define FT_ERR_BASE  0

#endif /* FT_CONFIG_OPTION_USE_MODULE_ERRORS */


  /* If FT_ERRORDEF is not defined, we need to define a simple */
  /* enumeration type.                                         */
  /*                                                           */
#ifndef FT_ERRORDEF

#define FT_INCLUDE_ERR_PROTOS

//#define FT_ERRORDEF( e, v, s )  e = v,
//#define FT_ERROR_START_LIST     enum {
//#define FT_ERROR_END_LIST       FT_ERR_CAT( FT_ERR_PREFIX, Max ) };

#ifdef __cplusplus
#define FT_NEED_EXTERN_C
  extern "C" {
#endif

#endif /* !FT_ERRORDEF */


  /* this macro is used to define an error */
#define FT_ERRORDEF_( e, v, s )                                             \
          FT_ERRORDEF( FT_ERR_CAT( FT_ERR_PREFIX, e ), v + FT_ERR_BASE, s )

  /* this is only used for <module>_Err_Ok, which must be 0! */
#define FT_NOERRORDEF_( e, v, s )                             \
          FT_ERRORDEF( FT_ERR_CAT( FT_ERR_PREFIX, e ), v, s )


//#ifdef FT_ERROR_START_LIST
//  FT_ERROR_START_LIST
//#endif


  /* now include the error codes */
#include <freetype/fterrdef.h>


//#ifdef FT_ERROR_END_LIST
//  FT_ERROR_END_LIST
//#endif


  /*******************************************************************/
  /*******************************************************************/
  /*****                                                         *****/
  /*****                      SIMPLE CLEANUP                     *****/
  /*****                                                         *****/
  /*******************************************************************/
  /*******************************************************************/

//#ifdef FT_NEED_EXTERN_C
//  }
//#endif

//#undef FT_ERROR_START_LIST
//#undef FT_ERROR_END_LIST

//#undef FT_ERRORDEF
//#undef FT_ERRORDEF_
//#undef FT_NOERRORDEF_

//#undef FT_NEED_EXTERN_C
//#undef FT_ERR_BASE

  /* FT_ERR_PREFIX is needed internally */
//#ifndef FT2_BUILD_LIBRARY
//#undef FT_ERR_PREFIX
//#endif

  /* FT_INCLUDE_ERR_PROTOS: Control whether function prototypes should be */
  /*                        included with                                 */
  /*                                                                      */
  /*                          #include <freetype/fterrors.h>              */
  /*                                                                      */
  /*                        This is only true where `FT_ERRORDEF` is      */
  /*                        undefined.                                    */
  /*                                                                      */
  /* FT_ERR_PROTOS_DEFINED: Actual multiple-inclusion protection of       */
  /*                        `fterrors.h`.                                 */
//#ifdef FT_INCLUDE_ERR_PROTOS
//#undef FT_INCLUDE_ERR_PROTOS

//#ifndef FT_ERR_PROTOS_DEFINED
//#define FT_ERR_PROTOS_DEFINED




  /**************************************************************************
   *
   * @function:
   *   FT_Error_String
   *
   * @description:
   *   Retrieve the description of a valid FreeType error code.
   *
   * @input:
   *   error_code ::
   *     A valid FreeType error code.
   *
   * @return:
   *   A C~string or `NULL`, if any error occurred.
   *
   * @note:
   *   FreeType has to be compiled with `FT_CONFIG_OPTION_ERROR_STRINGS` or
   *   `FT_DEBUG_LEVEL_ERROR` to get meaningful descriptions.
   *   'error_string' will be `NULL` otherwise.
   *
   *   Module identification will be ignored:
   *
   *   ```c
   *     strcmp( FT_Error_String(  FT_Err_Unknown_File_Format ),
   *             FT_Error_String( BDF_Err_Unknown_File_Format ) ) == 0;
   *   ```
   */
   const char*   FT_Error_String( FT_Error  error_code );

  /* */




#endif /* FT_ERR_PROTOS_DEFINED */

#endif /* FT_INCLUDE_ERR_PROTOS */

#endif /* !(FTERRORS_H_ && __FTERRORS_H__) */


/* END */
//
// ===========================  ftgzip.h  ===========================
//
/****************************************************************************
 *
 * ftgzip.h
 *
 *   Gzip-compressed stream support.
 *
 * Copyright (C) 2002-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTGZIP_H_
#define FTGZIP_H_

#include <freetype/freetype.h>

#ifdef FREETYPE_H
#error "freetype.h of FreeType 1 has been loaded!"
#error "Please fix the directory search order for header files"
#error "so that freetype.h of FreeType 2 is found first."
#endif




  /**************************************************************************
   *
   * @section:
   *   gzip
   *
   * @title:
   *   GZIP Streams
   *
   * @abstract:
   *   Using gzip-compressed font files.
   *
   * @description:
   *   In certain builds of the library, gzip compression recognition is
   *   automatically handled when calling @FT_New_Face or @FT_Open_Face.
   *   This means that if no font driver is capable of handling the raw
   *   compressed file, the library will try to open a gzipped stream from it
   *   and re-open the face with it.
   *
   *   The stream implementation is very basic and resets the decompression
   *   process each time seeking backwards is needed within the stream,
   *   which significantly undermines the performance.
   *
   *   This section contains the declaration of Gzip-specific functions.
   *
   */


  /**************************************************************************
   *
   * @function:
   *   FT_Stream_OpenGzip
   *
   * @description:
   *   Open a new stream to parse gzip-compressed font files.  This is mainly
   *   used to support the compressed `*.pcf.gz` fonts that come with
   *   XFree86.
   *
   * @input:
   *   stream ::
   *     The target embedding stream.
   *
   *   source ::
   *     The source stream.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The source stream must be opened _before_ calling this function.
   *
   *   Calling the internal function `FT_Stream_Close` on the new stream will
   *   **not** call `FT_Stream_Close` on the source stream.  None of the
   *   stream objects will be released to the heap.
   *
   *   This function may return `FT_Err_Unimplemented_Feature` if your build
   *   of FreeType was not compiled with zlib support.
   */
   FT_Error 
  FT_Stream_OpenGzip( FT_Stream  stream,
                      FT_Stream  source );


  /**************************************************************************
   *
   * @function:
   *   FT_Gzip_Uncompress
   *
   * @description:
   *   Decompress a zipped input buffer into an output buffer.  This function
   *   is modeled after zlib's `uncompress` function.
   *
   * @input:
   *   memory ::
   *     A FreeType memory handle.
   *
   *   input ::
   *     The input buffer.
   *
   *   input_len ::
   *     The length of the input buffer.
   *
   * @output:
   *   output ::
   *     The output buffer.
   *
   * @inout:
   *   output_len ::
   *     Before calling the function, this is the total size of the output
   *     buffer, which must be large enough to hold the entire uncompressed
   *     data (so the size of the uncompressed data must be known in
   *     advance).  After calling the function, `output_len` is the size of
   *     the used data in `output`.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function may return `FT_Err_Unimplemented_Feature` if your build
   *   of FreeType was not compiled with zlib support.
   *
   * @since:
   *   2.5.1
   */
   FT_Error 
  FT_Gzip_Uncompress( FT_Memory       memory,
                      FT_Byte*        output,
                      FT_ULong*       output_len,
                      const FT_Byte*  input,
                      FT_ULong        input_len );

  /* */




#endif /* FTGZIP_H_ */


/* END */
//
// ===========================  ftstdlib.h  ===========================
//
/****************************************************************************
 *
 * ftstdlib.h
 *
 *   ANSI-specific library and header configuration file (specification
 *   only).
 *
 * Copyright (C) 2002-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


  /**************************************************************************
   *
   * This file is used to group all `#includes` to the ANSI~C library that
   * FreeType normally requires.  It also defines macros to rename the
   * standard functions within the FreeType source code.
   *
   * Load a file which defines `FTSTDLIB_H_` before this one to override it.
   *
   */


#ifndef FTSTDLIB_H_
#define FTSTDLIB_H_


#include <stddef.h>

#define ft_ptrdiff_t  ptrdiff_t


  /**************************************************************************
   *
   *                          integer limits
   *
   * `UINT_MAX` and `ULONG_MAX` are used to automatically compute the size of
   * `int` and `long` in bytes at compile-time.  So far, this works for all
   * platforms the library has been tested on.  We also check `ULLONG_MAX`
   * to see whether we can use 64-bit `long long` later on.
   *
   * Note that on the extremely rare platforms that do not provide integer
   * types that are _exactly_ 16 and 32~bits wide (e.g., some old Crays where
   * `int` is 36~bits), we do not make any guarantee about the correct
   * behaviour of FreeType~2 with all fonts.
   *
   * In these cases, `ftconfig.h` will refuse to compile anyway with a
   * message like 'couldn't find 32-bit type' or something similar.
   *
   */


#include <limits.h>

#define FT_CHAR_BIT    CHAR_BIT
#define FT_USHORT_MAX  USHRT_MAX
#define FT_INT_MAX     INT_MAX
#define FT_INT_MIN     INT_MIN
#define FT_UINT_MAX    UINT_MAX
#define FT_LONG_MIN    LONG_MIN
#define FT_LONG_MAX    LONG_MAX
#define FT_ULONG_MAX   ULONG_MAX
#ifdef LLONG_MAX
#define FT_LLONG_MAX   LLONG_MAX
#endif
#ifdef LLONG_MIN
#define FT_LLONG_MIN   LLONG_MIN
#endif
#ifdef ULLONG_MAX
#define FT_ULLONG_MAX  ULLONG_MAX
#endif


  /**************************************************************************
   *
   *                character and string processing
   *
   */


#include <string.h>

#define ft_memchr   memchr
#define ft_memcmp   memcmp
#define ft_memcpy   memcpy
#define ft_memmove  memmove
#define ft_memset   memset
#define ft_strcat   strcat
#define ft_strcmp   strcmp
#define ft_strcpy   strcpy
#define ft_strlen   strlen
#define ft_strncmp  strncmp
#define ft_strncpy  strncpy
#define ft_strrchr  strrchr
#define ft_strstr   strstr


  /**************************************************************************
   *
   *                          file handling
   *
   */


#include <stdio.h>

#define FT_FILE      FILE
#define ft_fclose    fclose
#define ft_fopen     fopen
#define ft_fread     fread
#define ft_fseek     fseek
#define ft_ftell     ftell
#define ft_snprintf  snprintf


  /**************************************************************************
   *
   *                            sorting
   *
   */


#include <stdlib.h>

#define ft_qsort  qsort


  /**************************************************************************
   *
   *                       memory allocation
   *
   */


#define ft_scalloc   calloc
#define ft_sfree     free
#define ft_smalloc   malloc
#define ft_srealloc  realloc


  /**************************************************************************
   *
   *                         miscellaneous
   *
   */


#define ft_strtol  strtol
#define ft_getenv  getenv


  /**************************************************************************
   *
   *                        execution control
   *
   */


#include <setjmp.h>

#define ft_jmp_buf     jmp_buf  /* note: this cannot be a typedef since  */
                                /*       `jmp_buf` is defined as a macro */
                                /*       on certain platforms            */

#define ft_longjmp     longjmp
//#define ft_setjmp( b ) setjmp( *(ft_jmp_buf*) &(b) ) /* same thing here */


  /* The following is only used for debugging purposes, i.e., if   */
  /* `FT_DEBUG_LEVEL_ERROR` or `FT_DEBUG_LEVEL_TRACE` are defined. */

#include <stdarg.h>


#endif /* FTSTDLIB_H_ */


/* END */
//
// ===========================  ftheader.h  ===========================
//
/****************************************************************************
 *
 * ftheader.h
 *
 *   Build macros of the FreeType 2 library.
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */

#ifndef FTHEADER_H_
#define FTHEADER_H_


  /*@***********************************************************************/
  /*                                                                       */
  /* <Macro>                                                               */
  /*                                                        */
  /*                                                                       */
  /* <Description>                                                         */
  /*    This macro is used in association with @ in header    */
  /*    files to ensure that the declarations within are properly          */
  /*    encapsulated in an `extern "C" { .. }` block when included from a  */
  /*    C++ compiler.                                                      */
  /*                                                                       */
//#ifndef 
//#  ifdef __cplusplus
//# //   define   extern "C" {
//#  else
///#  define   /* nothing */
//#  endif
//#endif


  /*@***********************************************************************/
  /*                                                                       */
  /* <Macro>                                                               */
  /*                                                          */
  /*                                                                       */
  /* <Description>                                                         */
  /*    This macro is used in association with @ in header  */
  /*    files to ensure that the declarations within are properly          */
  /*    encapsulated in an `extern "C" { .. }` block when included from a  */
  /*    C++ compiler.                                                      */
  /*                                                                       */
//#ifndef 
//#  ifdef __cplusplus
//#    define   }
//#  else
//#   define   /* nothing */
//#  endif
//#endif


  /**************************************************************************
   *
   * Aliases for the FreeType 2 public and configuration files.
   *
   */

  /**************************************************************************
   *
   * @section:
   *   header_file_macros
   *
   * @title:
   *   Header File Macros
   *
   * @abstract:
   *   Macro definitions used to `#include` specific header files.
   *
   * @description:
   *   In addition to the normal scheme of including header files like
   *
   *   ```
   *     #include <freetype/freetype.h>
   *     #include <freetype/ftmm.h>
   *     #include <freetype/ftglyph.h>
   *   ```
   *
   *   it is possible to used named macros instead.  They can be used
   *   directly in `#include` statements as in
   *
   *   ```
   *     #include FT_FREETYPE_H
   *     #include FT_MULTIPLE_MASTERS_H
   *     #include FT_GLYPH_H
   *   ```
   *
   *   These macros were introduced to overcome the infamous 8.3~naming rule
   *   required by DOS (and `FT_MULTIPLE_MASTERS_H` is a lot more meaningful
   *   than `ftmm.h`).
   *
   */



  /**************************************************************************
   *
   * @macro:
   *   FT_AUTOHINTER_H
   *
   * @description:
   *   A macro used in `#include` statements to name the file containing
   *   structures and macros related to the auto-hinting module.
   *
   *   Deprecated since version~2.9; use @FT_DRIVER_H instead.
   *
   */
#define FT_AUTOHINTER_H  FT_DRIVER_H


  /**************************************************************************
   *
   * @macro:
   *   FT_CFF_DRIVER_H
   *
   * @description:
   *   A macro used in `#include` statements to name the file containing
   *   structures and macros related to the CFF driver module.
   *
   *   Deprecated since version~2.9; use @FT_DRIVER_H instead.
   *
   */
#define FT_CFF_DRIVER_H  FT_DRIVER_H


  /**************************************************************************
   *
   * @macro:
   *   FT_TRUETYPE_DRIVER_H
   *
   * @description:
   *   A macro used in `#include` statements to name the file containing
   *   structures and macros related to the TrueType driver module.
   *
   *   Deprecated since version~2.9; use @FT_DRIVER_H instead.
   *
   */
#define FT_TRUETYPE_DRIVER_H  FT_DRIVER_H


  /**************************************************************************
   *
   * @macro:
   *   FT_PCF_DRIVER_H
   *
   * @description:
   *   A macro used in `#include` statements to name the file containing
   *   structures and macros related to the PCF driver module.
   *
   *   Deprecated since version~2.9; use @FT_DRIVER_H instead.
   *
   */
#define FT_PCF_DRIVER_H  FT_DRIVER_H



  /* */

  /* These header files don't need to be included by the user. */
//#define FT_ERROR_DEFINITIONS_H  <freetype/fterrdef.h>
//#define FT_PARAMETER_TAGS_H     <freetype/ftparams.h>

  /* Deprecated macros. */
//#define FT_UNPATENTED_HINTING_H   <freetype/ftparams.h>
//#define FT_TRUETYPE_UNPATENTED_H  <freetype/ftparams.h>

  /* `FT_CACHE_H` is the only header file needed for the cache subsystem. */
#define FT_CACHE_IMAGE_H          FT_CACHE_H
#define FT_CACHE_SMALL_BITMAPS_H  FT_CACHE_H
#define FT_CACHE_CHARMAP_H        FT_CACHE_H

  /* The internals of the cache sub-system are no longer exposed.  We */
  /* default to `FT_CACHE_H` at the moment just in case, but we know  */
  /* of no rogue client that uses them.                               */
  /*                                                                  */
#define FT_CACHE_MANAGER_H           FT_CACHE_H
#define FT_CACHE_INTERNAL_MRU_H      FT_CACHE_H
#define FT_CACHE_INTERNAL_MANAGER_H  FT_CACHE_H
#define FT_CACHE_INTERNAL_CACHE_H    FT_CACHE_H
#define FT_CACHE_INTERNAL_GLYPH_H    FT_CACHE_H
#define FT_CACHE_INTERNAL_IMAGE_H    FT_CACHE_H
#define FT_CACHE_INTERNAL_SBITS_H    FT_CACHE_H

/* TODO(david): Move this section below to a different header */
#ifdef FT2_BUILD_LIBRARY
#if defined( _MSC_VER )      /* Visual C++ (and Intel C++) */

  /* We disable the warning `conditional expression is constant' here */
  /* in order to compile cleanly with the maximum level of warnings.  */
  /* In particular, the warning complains about stuff like `while(0)' */
  /* which is very useful in macro definitions.  There is no benefit  */
  /* in having it enabled.                                            */
#pragma warning( disable : 4127 )

#endif /* _MSC_VER */
#endif /* FT2_BUILD_LIBRARY */

#endif /* FTHEADER_H_ */


/* END */
//
// ===========================  ftconfig.h  ===========================
//
/****************************************************************************
 *
 * ftconfig.h
 *
 *   ANSI-specific configuration file (specification only).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


  /**************************************************************************
   *
   * This header file contains a number of macro definitions that are used by
   * the rest of the engine.  Most of the macros here are automatically
   * determined at compile time, and you should not need to change it to port
   * FreeType, except to compile the library with a non-ANSI compiler.
   *
   * Note however that if some specific modifications are needed, we advise
   * you to place a modified copy in your build directory.
   *
   * The build directory is usually `builds/<system>`, and contains
   * system-specific files that are always included first when building the
   * library.
   *
   * This ANSI version should stay in `include/config/`.
   *
   */

#ifndef FTCONFIG_H_
#define FTCONFIG_H_

#include <ft2build.h>
#include FT_CONFIG_OPTIONS_H
#include FT_CONFIG_STANDARD_LIBRARY_H

#include <freetype/config/integer-types.h>
#include <freetype/config/public-macros.h>
#include <freetype/config/mac-support.h>

#endif /* FTCONFIG_H_ */


/* END */
//
// ===========================  mac-support.h  ===========================
//
/****************************************************************************
 *
 * config/mac-support.h
 *
 *   Mac/OS X support configuration header.
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */
#ifndef FREETYPE_CONFIG_MAC_SUPPORT_H_
#define FREETYPE_CONFIG_MAC_SUPPORT_H_

  /**************************************************************************
   *
   * Mac support
   *
   *   This is the only necessary change, so it is defined here instead
   *   providing a new configuration file.
   */
#if defined( __APPLE__ ) || ( defined( __MWERKS__ ) && defined( macintosh ) )
  /* No Carbon frameworks for 64bit 10.4.x.                         */
  /* `AvailabilityMacros.h` is available since Mac OS X 10.2,       */
  /* so guess the system version by maximum errno before inclusion. */
#include <errno.h>
#ifdef ECANCELED /* defined since 10.2 */
#include "AvailabilityMacros.h"
#endif
//#if defined( __LP64__ ) && \
//    ( MAC_OS_X_VERSION_MIN_REQUIRED <= MAC_OS_X_VERSION_10_4 )
//#undef FT_MACINTOSH
#endif

#elif defined( __SC__ ) || defined( __MRC__ )
  /* Classic MacOS compilers */
#include "ConditionalMacros.h"
#if TARGET_OS_MAC
#define FT_MACINTOSH 1
#endif

#endif  /* Mac support */

#endif  /* FREETYPE_CONFIG_MAC_SUPPORT_H_ */
//
// ===========================  public-macros.h  ===========================
//
/****************************************************************************
 *
 * config/public-macros.h
 *
 *   Define a set of compiler macros used in public FreeType headers.
 *
 * Copyright (C) 2020-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */

  /*
   * The definitions in this file are used by the public FreeType headers
   * and thus should be considered part of the public API.
   *
   * Other compiler-specific macro definitions that are not exposed by the
   * FreeType API should go into
   * `include/freetype/internal/compiler-macros.h` instead.
   */
#ifndef FREETYPE_CONFIG_PUBLIC_MACROS_H_
#define FREETYPE_CONFIG_PUBLIC_MACROS_H_

  /*
   * `` and `` might have already been defined
   * by `freetype/config/ftheader.h`, but we don't want to include this
   * header here, so redefine the macros here only when needed.  Their
   * definition is very stable, so keeping them in sync with the ones in the
   * header should not be a maintenance issue.
   */
#ifndef 
#ifdef __cplusplus
#define   extern "C" {
#else
//#define   /* empty */
#endif
#endif  /*  */

#ifndef 
#ifdef __cplusplus
#define   }
#else
//#define   /* empty */
#endif
#endif  /*  */




  /*
   * Mark a function declaration as public.  This ensures it will be
   * properly exported to client code.  Place this before a function
   * declaration.
   *
   * NOTE: This macro should be considered an internal implementation
   * detail, and not part of the FreeType API.  It is only defined here
   * because it is needed by `FT_EXPORT`.
   */

  /* Visual C, mingw */
#if defined( _WIN32 )

#if defined( FT2_BUILD_LIBRARY ) && defined( DLL_EXPORT )
#define FT_PUBLIC_FUNCTION_ATTRIBUTE  __declspec( dllexport )
#elif defined( DLL_IMPORT )
#define FT_PUBLIC_FUNCTION_ATTRIBUTE  __declspec( dllimport )
#endif

  /* gcc, clang */
#elif ( defined( __GNUC__ ) && __GNUC__ >= 4 ) || defined( __clang__ )
#define FT_PUBLIC_FUNCTION_ATTRIBUTE \
          __attribute__(( visibility( "default" ) ))

  /* Sun */
#elif defined( __SUNPRO_C ) && __SUNPRO_C >= 0x550
#define FT_PUBLIC_FUNCTION_ATTRIBUTE  __global
#endif


#ifndef FT_PUBLIC_FUNCTION_ATTRIBUTE
#define FT_PUBLIC_FUNCTION_ATTRIBUTE  /* empty */
#endif


  /*
   * Define a public FreeType API function.  This ensures it is properly
   * exported or imported at build time.  The macro parameter is the
   * function's return type as in:
   *
   *    FT_Bool 
   *   FT_Object_Method( FT_Object  obj,
   *                     ... );
   *
   * NOTE: This requires that all `FT_EXPORT` uses are inside
   * ` ... ` blocks.  This guarantees that the
   * functions are exported with C linkage, even when the header is included
   * by a C++ source file.
   */
//#define  x   FT_PUBLIC_FUNCTION_ATTRIBUTE extern x


  /*
   * `FT_UNUSED` indicates that a given parameter is not used -- this is
   * only used to get rid of unpleasant compiler warnings.
   *
   * Technically, this was not meant to be part of the public API, but some
   * third-party code depends on it.
   */
#ifndef FT_UNUSED
#define FT_UNUSED( arg )  ( (arg) = (arg) )
#endif


  /*
   * Support for casts in both C and C++.
   */
#ifdef __cplusplus
#define FT_STATIC_CAST( type, var )       static_cast<type>(var)
#define FT_REINTERPRET_CAST( type, var )  reinterpret_cast<type>(var)

#define FT_STATIC_BYTE_CAST( type, var )                         \
          static_cast<type>( static_cast<unsigned char>( var ) )
#else
#define FT_STATIC_CAST( type, var )       (type)(var)
#define FT_REINTERPRET_CAST( type, var )  (type)(var)

#define FT_STATIC_BYTE_CAST( type, var )  (type)(unsigned char)(var)
#endif




#endif  /* FREETYPE_CONFIG_PUBLIC_MACROS_H_ */
//
// ===========================  ftoption.h  ===========================
//
/****************************************************************************
 *
 * ftoption.h
 *
 *   User-selectable configuration macros (specification only).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */


#ifndef FTOPTION_H_
#define FTOPTION_H_


#include <ft2build.h>




  /**************************************************************************
   *
   *                USER-SELECTABLE CONFIGURATION MACROS
   *
   * This file contains the default configuration macro definitions for a
   * standard build of the FreeType library.  There are three ways to use
   * this file to build project-specific versions of the library:
   *
   * - You can modify this file by hand, but this is not recommended in
   *   cases where you would like to build several versions of the library
   *   from a single source directory.
   *
   * - You can put a copy of this file in your build directory, more
   *   precisely in `$BUILD/freetype/config/ftoption.h`, where `$BUILD` is
   *   the name of a directory that is included _before_ the FreeType include
   *   path during compilation.
   *
   *   The default FreeType Makefiles use the build directory
   *   `builds/<system>` by default, but you can easily change that for your
   *   own projects.
   *
   * - Copy the file <ft2build.h> to `$BUILD/ft2build.h` and modify it
   *   slightly to pre-define the macro `FT_CONFIG_OPTIONS_H` used to locate
   *   this file during the build.  For example,
   *
   *   ```
   *     #define FT_CONFIG_OPTIONS_H  <myftoptions.h>
   *     #include <freetype/config/ftheader.h>
   *   ```
   *
   *   will use `$BUILD/myftoptions.h` instead of this file for macro
   *   definitions.
   *
   *   Note also that you can similarly pre-define the macro
   *   `FT_CONFIG_MODULES_H` used to locate the file listing of the modules
   *   that are statically linked to the library at compile time.  By
   *   default, this file is `<freetype/config/ftmodule.h>`.
   *
   * We highly recommend using the third method whenever possible.
   *
   */


  /*************************************************************************/
  /*************************************************************************/
  /****                                                                 ****/
  /**** G E N E R A L   F R E E T Y P E   2   C O N F I G U R A T I O N ****/
  /****                                                                 ****/
  /*************************************************************************/
  /*************************************************************************/


  /*#************************************************************************
   *
   * If you enable this configuration option, FreeType recognizes an
   * environment variable called `FREETYPE_PROPERTIES`, which can be used to
   * control the various font drivers and modules.  The controllable
   * properties are listed in the section @properties.
   *
   * You have to undefine this configuration option on platforms that lack
   * the concept of environment variables (and thus don't have the `getenv`
   * function), for example Windows CE.
   *
   * `FREETYPE_PROPERTIES` has the following syntax form (broken here into
   * multiple lines for better readability).
   *
   * ```
   *   <optional whitespace>
   *   <module-name1> ':'
   *   <property-name1> '=' <property-value1>
   *   <whitespace>
   *   <module-name2> ':'
   *   <property-name2> '=' <property-value2>
   *   ...
   * ```
   *
   * Example:
   *
   * ```
   *   FREETYPE_PROPERTIES=truetype:interpreter-version=35 \
   *                       cff:no-stem-darkening=1
   * ```
   *
   */
#define FT_CONFIG_OPTION_ENVIRONMENT_PROPERTIES


  /**************************************************************************
   *
   * Uncomment the line below if you want to activate LCD rendering
   * technology similar to ClearType in this build of the library.  This
   * technology triples the resolution in the direction color subpixels.  To
   * mitigate color fringes inherent to this technology, you also need to
   * explicitly set up LCD filtering.
   *
   * When this macro is not defined, FreeType offers alternative LCD
   * rendering technology that produces excellent output.
   */
/* #define FT_CONFIG_OPTION_SUBPIXEL_RENDERING */


  /**************************************************************************
   *
   * Many compilers provide a non-ANSI 64-bit data type that can be used by
   * FreeType to speed up some computations.  However, this will create some
   * problems when compiling the library in strict ANSI mode.
   *
   * For this reason, the use of 64-bit integers is normally disabled when
   * the `__STDC__` macro is defined.  You can however disable this by
   * defining the macro `FT_CONFIG_OPTION_FORCE_INT64` here.
   *
   * For most compilers, this will only create compilation warnings when
   * building the library.
   *
   * ObNote: The compiler-specific 64-bit integers are detected in the
   *         file `ftconfig.h` either statically or through the `configure`
   *         script on supported platforms.
   */
#undef FT_CONFIG_OPTION_FORCE_INT64


  /**************************************************************************
   *
   * If this macro is defined, do not try to use an assembler version of
   * performance-critical functions (e.g., @FT_MulFix).  You should only do
   * that to verify that the assembler function works properly, or to execute
   * benchmark tests of the various implementations.
   */
/* #define FT_CONFIG_OPTION_NO_ASSEMBLER */


  /**************************************************************************
   *
   * If this macro is defined, try to use an inlined assembler version of the
   * @FT_MulFix function, which is a 'hotspot' when loading and hinting
   * glyphs, and which should be executed as fast as possible.
   *
   * Note that if your compiler or CPU is not supported, this will default to
   * the standard and portable implementation found in `ftcalc.c`.
   */
#define FT_CONFIG_OPTION_INLINE_MULFIX


  /**************************************************************************
   *
   * LZW-compressed file support.
   *
   *   FreeType now handles font files that have been compressed with the
   *   `compress` program.  This is mostly used to parse many of the PCF
   *   files that come with various X11 distributions.  The implementation
   *   uses NetBSD's `zopen` to partially uncompress the file on the fly (see
   *   `src/lzw/ftgzip.c`).
   *
   *   Define this macro if you want to enable this 'feature'.
   */
#define FT_CONFIG_OPTION_USE_LZW


  /**************************************************************************
   *
   * Gzip-compressed file support.
   *
   *   FreeType now handles font files that have been compressed with the
   *   `gzip` program.  This is mostly used to parse many of the PCF files
   *   that come with XFree86.  The implementation uses 'zlib' to partially
   *   uncompress the file on the fly (see `src/gzip/ftgzip.c`).
   *
   *   Define this macro if you want to enable this 'feature'.  See also the
   *   macro `FT_CONFIG_OPTION_SYSTEM_ZLIB` below.
   */
#define FT_CONFIG_OPTION_USE_ZLIB


  /**************************************************************************
   *
   * ZLib library selection
   *
   *   This macro is only used when `FT_CONFIG_OPTION_USE_ZLIB` is defined.
   *   It allows FreeType's 'ftgzip' component to link to the system's
   *   installation of the ZLib library.  This is useful on systems like
   *   Unix or VMS where it generally is already available.
   *
   *   If you let it undefined, the component will use its own copy of the
   *   zlib sources instead.  These have been modified to be included
   *   directly within the component and **not** export external function
   *   names.  This allows you to link any program with FreeType _and_ ZLib
   *   without linking conflicts.
   *
   *   Do not `#undef` this macro here since the build system might define
   *   it for certain configurations only.
   *
   *   If you use a build system like cmake or the `configure` script,
   *   options set by those programs have precedence, overwriting the value
   *   here with the configured one.
   *
   *   If you use the GNU make build system directly (that is, without the
   *   `configure` script) and you define this macro, you also have to pass
   *   `SYSTEM_ZLIB=yes` as an argument to make.
   */
/* #define FT_CONFIG_OPTION_SYSTEM_ZLIB */


  /**************************************************************************
   *
   * Bzip2-compressed file support.
   *
   *   FreeType now handles font files that have been compressed with the
   *   `bzip2` program.  This is mostly used to parse many of the PCF files
   *   that come with XFree86.  The implementation uses `libbz2` to partially
   *   uncompress the file on the fly (see `src/bzip2/ftbzip2.c`).  Contrary
   *   to gzip, bzip2 currently is not included and need to use the system
   *   available bzip2 implementation.
   *
   *   Define this macro if you want to enable this 'feature'.
   *
   *   If you use a build system like cmake or the `configure` script,
   *   options set by those programs have precedence, overwriting the value
   *   here with the configured one.
   */
/* #define FT_CONFIG_OPTION_USE_BZIP2 */


  /**************************************************************************
   *
   * Define to disable the use of file stream functions and types, `FILE`,
   * `fopen`, etc.  Enables the use of smaller system libraries on embedded
   * systems that have multiple system libraries, some with or without file
   * stream support, in the cases where file stream support is not necessary
   * such as memory loading of font files.
   */
/* #define FT_CONFIG_OPTION_DISABLE_STREAM_SUPPORT */


  /**************************************************************************
   *
   * PNG bitmap support.
   *
   *   FreeType now handles loading color bitmap glyphs in the PNG format.
   *   This requires help from the external libpng library.  Uncompressed
   *   color bitmaps do not need any external libraries and will be supported
   *   regardless of this configuration.
   *
   *   Define this macro if you want to enable this 'feature'.
   *
   *   If you use a build system like cmake or the `configure` script,
   *   options set by those programs have precedence, overwriting the value
   *   here with the configured one.
   */
#define FT_CONFIG_OPTION_USE_PNG


  /**************************************************************************
   *
   * HarfBuzz support.
   *
   *   FreeType uses the HarfBuzz library to improve auto-hinting of OpenType
   *   fonts.  If available, many glyphs not directly addressable by a font's
   *   character map will be hinted also.
   *
   *   Define this macro if you want to enable this 'feature'.
   *
   *   If you use a build system like cmake or the `configure` script,
   *   options set by those programs have precedence, overwriting the value
   *   here with the configured one.
   */
/* #define FT_CONFIG_OPTION_USE_HARFBUZZ */


  /**************************************************************************
   *
   * Brotli support.
   *
   *   FreeType uses the Brotli library to provide support for decompressing
   *   WOFF2 streams.
   *
   *   Define this macro if you want to enable this 'feature'.
   *
   *   If you use a build system like cmake or the `configure` script,
   *   options set by those programs have precedence, overwriting the value
   *   here with the configured one.
   */
/* #define FT_CONFIG_OPTION_USE_BROTLI */


  /**************************************************************************
   *
   * Glyph Postscript Names handling
   *
   *   By default, FreeType 2 is compiled with the 'psnames' module.  This
   *   module is in charge of converting a glyph name string into a Unicode
   *   value, or return a Macintosh standard glyph name for the use with the
   *   TrueType 'post' table.
   *
   *   Undefine this macro if you do not want 'psnames' compiled in your
   *   build of FreeType.  This has the following effects:
   *
   *   - The TrueType driver will provide its own set of glyph names, if you
   *     build it to support postscript names in the TrueType 'post' table,
   *     but will not synthesize a missing Unicode charmap.
   *
   *   - The Type~1 driver will not be able to synthesize a Unicode charmap
   *     out of the glyphs found in the fonts.
   *
   *   You would normally undefine this configuration macro when building a
   *   version of FreeType that doesn't contain a Type~1 or CFF driver.
   */
#define FT_CONFIG_OPTION_POSTSCRIPT_NAMES


  /**************************************************************************
   *
   * Postscript Names to Unicode Values support
   *
   *   By default, FreeType~2 is built with the 'psnames' module compiled in.
   *   Among other things, the module is used to convert a glyph name into a
   *   Unicode value.  This is especially useful in order to synthesize on
   *   the fly a Unicode charmap from the CFF/Type~1 driver through a big
   *   table named the 'Adobe Glyph List' (AGL).
   *
   *   Undefine this macro if you do not want the Adobe Glyph List compiled
   *   in your 'psnames' module.  The Type~1 driver will not be able to
   *   synthesize a Unicode charmap out of the glyphs found in the fonts.
   */
#define FT_CONFIG_OPTION_ADOBE_GLYPH_LIST


  /**************************************************************************
   *
   * Support for Mac fonts
   *
   *   Define this macro if you want support for outline fonts in Mac format
   *   (mac dfont, mac resource, macbinary containing a mac resource) on
   *   non-Mac platforms.
   *
   *   Note that the 'FOND' resource isn't checked.
   */
#define FT_CONFIG_OPTION_MAC_FONTS


  /**************************************************************************
   *
   * Guessing methods to access embedded resource forks
   *
   *   Enable extra Mac fonts support on non-Mac platforms (e.g., GNU/Linux).
   *
   *   Resource forks which include fonts data are stored sometimes in
   *   locations which users or developers don't expected.  In some cases,
   *   resource forks start with some offset from the head of a file.  In
   *   other cases, the actual resource fork is stored in file different from
   *   what the user specifies.  If this option is activated, FreeType tries
   *   to guess whether such offsets or different file names must be used.
   *
   *   Note that normal, direct access of resource forks is controlled via
   *   the `FT_CONFIG_OPTION_MAC_FONTS` option.
   */
#ifdef FT_CONFIG_OPTION_MAC_FONTS
#define FT_CONFIG_OPTION_GUESSING_EMBEDDED_RFORK
#endif


  /**************************************************************************
   *
   * Allow the use of `FT_Incremental_Interface` to load typefaces that
   * contain no glyph data, but supply it via a callback function.  This is
   * required by clients supporting document formats which supply font data
   * incrementally as the document is parsed, such as the Ghostscript
   * interpreter for the PostScript language.
   */
#define FT_CONFIG_OPTION_INCREMENTAL


  /**************************************************************************
   *
   * The size in bytes of the render pool used by the scan-line converter to
   * do all of its work.
   */
#define FT_RENDER_POOL_SIZE  16384L


  /**************************************************************************
   *
   * FT_MAX_MODULES
   *
   *   The maximum number of modules that can be registered in a single
   *   FreeType library object.  32~is the default.
   */
#define FT_MAX_MODULES  32


  /**************************************************************************
   *
   * Debug level
   *
   *   FreeType can be compiled in debug or trace mode.  In debug mode,
   *   errors are reported through the 'ftdebug' component.  In trace mode,
   *   additional messages are sent to the standard output during execution.
   *
   *   Define `FT_DEBUG_LEVEL_ERROR` to build the library in debug mode.
   *   Define `FT_DEBUG_LEVEL_TRACE` to build it in trace mode.
   *
   *   Don't define any of these macros to compile in 'release' mode!
   *
   *   Do not `#undef` these macros here since the build system might define
   *   them for certain configurations only.
   */
/* #define FT_DEBUG_LEVEL_ERROR */
/* #define FT_DEBUG_LEVEL_TRACE */


  /**************************************************************************
   *
   * Logging
   *
   *   Compiling FreeType in debug or trace mode makes FreeType write error
   *   and trace log messages to `stderr`.  Enabling this macro
   *   automatically forces the `FT_DEBUG_LEVEL_ERROR` and
   *   `FT_DEBUG_LEVEL_TRACE` macros and allows FreeType to write error and
   *   trace log messages to a file instead of `stderr`.  For writing logs
   *   to a file, FreeType uses an the external `dlg` library (the source
   *   code is in `src/dlg`).
   *
   *   This option needs a C99 compiler.
   */
/* #define FT_DEBUG_LOGGING */


  /**************************************************************************
   *
   * Autofitter debugging
   *
   *   If `FT_DEBUG_AUTOFIT` is defined, FreeType provides some means to
   *   control the autofitter behaviour for debugging purposes with global
   *   boolean variables (consequently, you should **never** enable this
   *   while compiling in 'release' mode):
   *
   *   ```
   *     af_debug_disable_horz_hints_
   *     af_debug_disable_vert_hints_
   *     af_debug_disable_blue_hints_
   *   ```
   *
   *   Additionally, the following functions provide dumps of various
   *   internal autofit structures to stdout (using `printf`):
   *
   *   ```
   *     af_glyph_hints_dump_points
   *     af_glyph_hints_dump_segments
   *     af_glyph_hints_dump_edges
   *     af_glyph_hints_get_num_segments
   *     af_glyph_hints_get_segment_offset
   *   ```
   *
   *   As an argument, they use another global variable:
   *
   *   ```
   *     af_debug_hints_
   *   ```
   *
   *   Please have a look at the `ftgrid` demo program to see how those
   *   variables and macros should be used.
   *
   *   Do not `#undef` these macros here since the build system might define
   *   them for certain configurations only.
   */
/* #define FT_DEBUG_AUTOFIT */


  /**************************************************************************
   *
   * Memory Debugging
   *
   *   FreeType now comes with an integrated memory debugger that is capable
   *   of detecting simple errors like memory leaks or double deletes.  To
   *   compile it within your build of the library, you should define
   *   `FT_DEBUG_MEMORY` here.
   *
   *   Note that the memory debugger is only activated at runtime when when
   *   the _environment_ variable `FT2_DEBUG_MEMORY` is defined also!
   *
   *   Do not `#undef` this macro here since the build system might define it
   *   for certain configurations only.
   */
/* #define FT_DEBUG_MEMORY */


  /**************************************************************************
   *
   * Module errors
   *
   *   If this macro is set (which is _not_ the default), the higher byte of
   *   an error code gives the module in which the error has occurred, while
   *   the lower byte is the real error code.
   *
   *   Setting this macro makes sense for debugging purposes only, since it
   *   would break source compatibility of certain programs that use
   *   FreeType~2.
   *
   *   More details can be found in the files `ftmoderr.h` and `fterrors.h`.
   */
#undef FT_CONFIG_OPTION_USE_MODULE_ERRORS


  /**************************************************************************
   *
   * OpenType SVG Glyph Support
   *
   *   Setting this macro enables support for OpenType SVG glyphs.  By
   *   default, FreeType can only fetch SVG documents.  However, it can also
   *   render them if external rendering hook functions are plugged in at
   *   runtime.
   *
   *   More details on the hooks can be found in file `otsvg.h`.
   */
#define FT_CONFIG_OPTION_SVG


  /**************************************************************************
   *
   * Error Strings
   *
   *   If this macro is set, `FT_Error_String` will return meaningful
   *   descriptions.  This is not enabled by default to reduce the overall
   *   size of FreeType.
   *
   *   More details can be found in the file `fterrors.h`.
   */
/* #define FT_CONFIG_OPTION_ERROR_STRINGS */


  /*************************************************************************/
  /*************************************************************************/
  /****                                                                 ****/
  /****        S F N T   D R I V E R    C O N F I G U R A T I O N       ****/
  /****                                                                 ****/
  /*************************************************************************/
  /*************************************************************************/


  /**************************************************************************
   *
   * Define `TT_CONFIG_OPTION_EMBEDDED_BITMAPS` if you want to support
   * embedded bitmaps in all formats using the 'sfnt' module (namely
   * TrueType~& OpenType).
   */
#define TT_CONFIG_OPTION_EMBEDDED_BITMAPS


  /**************************************************************************
   *
   * Define `TT_CONFIG_OPTION_COLOR_LAYERS` if you want to support colored
   * outlines (from the 'COLR'/'CPAL' tables) in all formats using the 'sfnt'
   * module (namely TrueType~& OpenType).
   */
#define TT_CONFIG_OPTION_COLOR_LAYERS


  /**************************************************************************
   *
   * Define `TT_CONFIG_OPTION_POSTSCRIPT_NAMES` if you want to be able to
   * load and enumerate Postscript names of glyphs in a TrueType or OpenType
   * file.
   *
   * Note that if you do not compile the 'psnames' module by undefining the
   * above `FT_CONFIG_OPTION_POSTSCRIPT_NAMES` macro, the 'sfnt' module will
   * contain additional code to read the PostScript name table from a font.
   *
   * (By default, the module uses 'psnames' to extract glyph names.)
   */
#define TT_CONFIG_OPTION_POSTSCRIPT_NAMES


  /**************************************************************************
   *
   * Define `TT_CONFIG_OPTION_SFNT_NAMES` if your applications need to access
   * the internal name table in a SFNT-based format like TrueType or
   * OpenType.  The name table contains various strings used to describe the
   * font, like family name, copyright, version, etc.  It does not contain
   * any glyph name though.
   *
   * Accessing SFNT names is done through the functions declared in
   * `ftsnames.h`.
   */
#define TT_CONFIG_OPTION_SFNT_NAMES


  /**************************************************************************
   *
   * TrueType CMap support
   *
   *   Here you can fine-tune which TrueType CMap table format shall be
   *   supported.
   */
#define TT_CONFIG_CMAP_FORMAT_0
#define TT_CONFIG_CMAP_FORMAT_2
#define TT_CONFIG_CMAP_FORMAT_4
#define TT_CONFIG_CMAP_FORMAT_6
#define TT_CONFIG_CMAP_FORMAT_8
#define TT_CONFIG_CMAP_FORMAT_10
#define TT_CONFIG_CMAP_FORMAT_12
#define TT_CONFIG_CMAP_FORMAT_13
#define TT_CONFIG_CMAP_FORMAT_14


  /*************************************************************************/
  /*************************************************************************/
  /****                                                                 ****/
  /****    T R U E T Y P E   D R I V E R    C O N F I G U R A T I O N   ****/
  /****                                                                 ****/
  /*************************************************************************/
  /*************************************************************************/

  /**************************************************************************
   *
   * Define `TT_CONFIG_OPTION_BYTECODE_INTERPRETER` if you want to compile a
   * bytecode interpreter in the TrueType driver.
   *
   * By undefining this, you will only compile the code necessary to load
   * TrueType glyphs without hinting.
   *
   * Do not `#undef` this macro here, since the build system might define it
   * for certain configurations only.
   */
#define TT_CONFIG_OPTION_BYTECODE_INTERPRETER


  /**************************************************************************
   *
   * Define `TT_CONFIG_OPTION_SUBPIXEL_HINTING` if you want to compile
   * subpixel hinting support into the TrueType driver.  This modifies the
   * TrueType hinting mechanism when anything but `FT_RENDER_MODE_MONO` is
   * requested.
   *
   * In particular, it modifies the bytecode interpreter to interpret (or
   * not) instructions in a certain way so that all TrueType fonts look like
   * they do in a Windows ClearType (DirectWrite) environment.  See [1] for a
   * technical overview on what this means.  See `ttinterp.h` for more
   * details on this option.
   *
   * The new default mode focuses on applying a minimal set of rules to all
   * fonts indiscriminately so that modern and web fonts render well while
   * legacy fonts render okay.  The corresponding interpreter version is v40.
   * The so-called Infinality mode (v38) is no longer available in FreeType.
   *
   * By undefining these, you get rendering behavior like on Windows without
   * ClearType, i.e., Windows XP without ClearType enabled and Win9x
   * (interpreter version v35).  Or not, depending on how much hinting blood
   * and testing tears the font designer put into a given font.  If you
   * define one or both subpixel hinting options, you can switch between
   * between v35 and the ones you define (using `FT_Property_Set`).
   *
   * This option requires `TT_CONFIG_OPTION_BYTECODE_INTERPRETER` to be
   * defined.
   *
   * [1]
   * https://www.microsoft.com/typography/cleartype/truetypecleartype.aspx
   */
#define TT_CONFIG_OPTION_SUBPIXEL_HINTING


  /**************************************************************************
   *
   * Define `TT_CONFIG_OPTION_COMPONENT_OFFSET_SCALED` to compile the
   * TrueType glyph loader to use Apple's definition of how to handle
   * component offsets in composite glyphs.
   *
   * Apple and MS disagree on the default behavior of component offsets in
   * composites.  Apple says that they should be scaled by the scaling
   * factors in the transformation matrix (roughly, it's more complex) while
   * MS says they should not.  OpenType defines two bits in the composite
   * flags array which can be used to disambiguate, but old fonts will not
   * have them.
   *
   *   https://www.microsoft.com/typography/otspec/glyf.htm
   *   https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6glyf.html
   */
#undef TT_CONFIG_OPTION_COMPONENT_OFFSET_SCALED


  /**************************************************************************
   *
   * Define `TT_CONFIG_OPTION_GX_VAR_SUPPORT` if you want to include support
   * for Apple's distortable font technology ('fvar', 'gvar', 'cvar', and
   * 'avar' tables).  Tagged 'Font Variations', this is now part of OpenType
   * also.  This has many similarities to Type~1 Multiple Masters support.
   */
#define TT_CONFIG_OPTION_GX_VAR_SUPPORT


  /**************************************************************************
   *
   * Define `TT_CONFIG_OPTION_NO_BORING_EXPANSION` if you want to exclude
   * support for 'boring' OpenType specification expansions.
   *
   *   https://github.com/harfbuzz/boring-expansion-spec
   *
   * Right now, the following features are covered:
   *
   *   - 'avar' version 2.0
   *
   * Most likely, this is a temporary configuration option to be removed in
   * the near future, since it is assumed that eventually those features are
   * added to the OpenType standard.
   */
/* #define TT_CONFIG_OPTION_NO_BORING_EXPANSION */


  /**************************************************************************
   *
   * Define `TT_CONFIG_OPTION_BDF` if you want to include support for an
   * embedded 'BDF~' table within SFNT-based bitmap formats.
   */
#define TT_CONFIG_OPTION_BDF


  /**************************************************************************
   *
   * Option `TT_CONFIG_OPTION_MAX_RUNNABLE_OPCODES` controls the maximum
   * number of bytecode instructions executed for a single run of the
   * bytecode interpreter, needed to prevent infinite loops.  You don't want
   * to change this except for very special situations (e.g., making a
   * library fuzzer spend less time to handle broken fonts).
   *
   * It is not expected that this value is ever modified by a configuring
   * script; instead, it gets surrounded with `#ifndef ... #endif` so that
   * the value can be set as a preprocessor option on the compiler's command
   * line.
   */
#ifndef TT_CONFIG_OPTION_MAX_RUNNABLE_OPCODES
#define TT_CONFIG_OPTION_MAX_RUNNABLE_OPCODES  1000000L
#endif


  /**************************************************************************
   *
   * Option `TT_CONFIG_OPTION_GPOS_KERNING` enables a basic GPOS kerning
   * implementation (for TrueType fonts only).  With this defined, FreeType
   * is able to get kerning pair data from the GPOS 'kern' feature as well as
   * legacy 'kern' tables; without this defined, FreeType will only be able
   * to use legacy 'kern' tables.
   *
   * Note that FreeType does not support more advanced GPOS layout features;
   * even the 'kern' feature implemented here doesn't handle more
   * sophisticated kerning variants.  Use a higher-level library like
   * HarfBuzz instead for that.
   */
/* #define TT_CONFIG_OPTION_GPOS_KERNING */


  /*************************************************************************/
  /*************************************************************************/
  /****                                                                 ****/
  /****      T Y P E 1   D R I V E R    C O N F I G U R A T I O N       ****/
  /****                                                                 ****/
  /*************************************************************************/
  /*************************************************************************/


  /**************************************************************************
   *
   * `T1_MAX_DICT_DEPTH` is the maximum depth of nest dictionaries and arrays
   * in the Type~1 stream (see `t1load.c`).  A minimum of~4 is required.
   */
#define T1_MAX_DICT_DEPTH  5


  /**************************************************************************
   *
   * `T1_MAX_SUBRS_CALLS` details the maximum number of nested sub-routine
   * calls during glyph loading.
   */
#define T1_MAX_SUBRS_CALLS  16


  /**************************************************************************
   *
   * `T1_MAX_CHARSTRING_OPERANDS` is the charstring stack's capacity.  A
   * minimum of~16 is required.
   *
   * The Chinese font 'MingTiEG-Medium' (covering the CNS 11643 character
   * set) needs 256.
   */
#define T1_MAX_CHARSTRINGS_OPERANDS  256


  /**************************************************************************
   *
   * Define this configuration macro if you want to prevent the compilation
   * of the 't1afm' module, which is in charge of reading Type~1 AFM files
   * into an existing face.  Note that if set, the Type~1 driver will be
   * unable to produce kerning distances.
   */
#undef T1_CONFIG_OPTION_NO_AFM


  /**************************************************************************
   *
   * Define this configuration macro if you want to prevent the compilation
   * of the Multiple Masters font support in the Type~1 driver.
   */
#undef T1_CONFIG_OPTION_NO_MM_SUPPORT


  /**************************************************************************
   *
   * `T1_CONFIG_OPTION_OLD_ENGINE` controls whether the pre-Adobe Type~1
   * engine gets compiled into FreeType.  If defined, it is possible to
   * switch between the two engines using the `hinting-engine` property of
   * the 'type1' driver module.
   */
/* #define T1_CONFIG_OPTION_OLD_ENGINE */


  /*************************************************************************/
  /*************************************************************************/
  /****                                                                 ****/
  /****         C F F   D R I V E R    C O N F I G U R A T I O N        ****/
  /****                                                                 ****/
  /*************************************************************************/
  /*************************************************************************/


  /**************************************************************************
   *
   * Using `CFF_CONFIG_OPTION_DARKENING_PARAMETER_{X,Y}{1,2,3,4}` it is
   * possible to set up the default values of the four control points that
   * define the stem darkening behaviour of the (new) CFF engine.  For more
   * details please read the documentation of the `darkening-parameters`
   * property (file `ftdriver.h`), which allows the control at run-time.
   *
   * Do **not** undefine these macros!
   */
#define CFF_CONFIG_OPTION_DARKENING_PARAMETER_X1   500
#define CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y1   400

#define CFF_CONFIG_OPTION_DARKENING_PARAMETER_X2  1000
#define CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y2   275

#define CFF_CONFIG_OPTION_DARKENING_PARAMETER_X3  1667
#define CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y3   275

#define CFF_CONFIG_OPTION_DARKENING_PARAMETER_X4  2333
#define CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y4     0


  /**************************************************************************
   *
   * `CFF_CONFIG_OPTION_OLD_ENGINE` controls whether the pre-Adobe CFF engine
   * gets compiled into FreeType.  If defined, it is possible to switch
   * between the two engines using the `hinting-engine` property of the 'cff'
   * driver module.
   */
/* #define CFF_CONFIG_OPTION_OLD_ENGINE */


  /*************************************************************************/
  /*************************************************************************/
  /****                                                                 ****/
  /****         P C F   D R I V E R    C O N F I G U R A T I O N        ****/
  /****                                                                 ****/
  /*************************************************************************/
  /*************************************************************************/


  /**************************************************************************
   *
   * There are many PCF fonts just called 'Fixed' which look completely
   * different, and which have nothing to do with each other.  When selecting
   * 'Fixed' in KDE or Gnome one gets results that appear rather random, the
   * style changes often if one changes the size and one cannot select some
   * fonts at all.  This option makes the 'pcf' module prepend the foundry
   * name (plus a space) to the family name.
   *
   * We also check whether we have 'wide' characters; all put together, we
   * get family names like 'Sony Fixed' or 'Misc Fixed Wide'.
   *
   * If this option is activated, it can be controlled with the
   * `no-long-family-names` property of the 'pcf' driver module.
   */
/* #define PCF_CONFIG_OPTION_LONG_FAMILY_NAMES */


  /*************************************************************************/
  /*************************************************************************/
  /****                                                                 ****/
  /****    A U T O F I T   M O D U L E    C O N F I G U R A T I O N     ****/
  /****                                                                 ****/
  /*************************************************************************/
  /*************************************************************************/


  /**************************************************************************
   *
   * Compile 'autofit' module with CJK (Chinese, Japanese, Korean) script
   * support.
   */
#define AF_CONFIG_OPTION_CJK


  /**************************************************************************
   *
   * Compile 'autofit' module with fallback Indic script support, covering
   * some scripts that the 'latin' submodule of the 'autofit' module doesn't
   * (yet) handle.  Currently, this needs option `AF_CONFIG_OPTION_CJK`.
   */
#ifdef AF_CONFIG_OPTION_CJK
#define AF_CONFIG_OPTION_INDIC
#endif


  /**************************************************************************
   *
   * Use TrueType-like size metrics for 'light' auto-hinting.
   *
   * It is strongly recommended to avoid this option, which exists only to
   * help some legacy applications retain its appearance and behaviour with
   * respect to auto-hinted TrueType fonts.
   *
   * The very reason this option exists at all are GNU/Linux distributions
   * like Fedora that did not un-patch the following change (which was
   * present in FreeType between versions 2.4.6 and 2.7.1, inclusive).
   *
   * ```
   *   2011-07-16  Steven Chu  <steven.f.chu@gmail.com>
   *
   *     [truetype] Fix metrics on size request for scalable fonts.
   * ```
   *
   * This problematic commit is now reverted (more or less).
   */
/* #define AF_CONFIG_OPTION_TT_SIZE_METRICS */

  /* */


  /*
   * This macro is obsolete.  Support has been removed in FreeType version
   * 2.5.
   */
/* #define FT_CONFIG_OPTION_OLD_INTERNALS */


  /*
   * The next two macros are defined if native TrueType hinting is
   * requested by the definitions above.  Don't change this.
   */
#ifdef TT_CONFIG_OPTION_BYTECODE_INTERPRETER
#define  TT_USE_BYTECODE_INTERPRETER
#ifdef TT_CONFIG_OPTION_SUBPIXEL_HINTING
#define  TT_SUPPORT_SUBPIXEL_HINTING_MINIMAL
#endif
#endif


  /*
   * The TT_SUPPORT_COLRV1 macro is defined to indicate to clients that this
   * version of FreeType has support for 'COLR' v1 API.  This definition is
   * useful to FreeType clients that want to build in support for 'COLR' v1
   * depending on a tip-of-tree checkout before it is officially released in
   * FreeType, and while the feature cannot yet be tested against using
   * version macros.  Don't change this macro.  This may be removed once the
   * feature is in a FreeType release version and version macros can be used
   * to test for availability.
   */
#ifdef TT_CONFIG_OPTION_COLOR_LAYERS
#define  TT_SUPPORT_COLRV1
#endif


  /*
   * Check CFF darkening parameters.  The checks are the same as in function
   * `cff_property_set` in file `cffdrivr.c`.

#if CFF_CONFIG_OPTION_DARKENING_PARAMETER_X1 < 0   || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_X2 < 0   || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_X3 < 0   || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_X4 < 0   || \
                                                      \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y1 < 0   || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y2 < 0   || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y3 < 0   || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y4 < 0   || \
                                                      \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_X1 >        \
      CFF_CONFIG_OPTION_DARKENING_PARAMETER_X2     || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_X2 >        \
      CFF_CONFIG_OPTION_DARKENING_PARAMETER_X3     || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_X3 >        \
      CFF_CONFIG_OPTION_DARKENING_PARAMETER_X4     || \
                                                      \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y1 > 500 || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y2 > 500 || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y3 > 500 || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y4 > 500
#error "Invalid CFF darkening parameters!"
#endif

   */


#endif /* FTOPTION_H_ */


/* END */
//
// ===========================  integer-types.h  ===========================
//
/****************************************************************************
 *
 * config/integer-types.h
 *
 *   FreeType integer types definitions.
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */
#ifndef FREETYPE_CONFIG_INTEGER_TYPES_H_
#define FREETYPE_CONFIG_INTEGER_TYPES_H_

  /* There are systems (like the Texas Instruments 'C54x) where a `char`  */
  /* has 16~bits.  ANSI~C says that `sizeof(char)` is always~1.  Since an */
  /* `int` has 16~bits also for this system, `sizeof(int)` gives~1 which  */
  /* is probably unexpected.                                              */
  /*                                                                      */
  /* `CHAR_BIT` (defined in `limits.h`) gives the number of bits in a     */
  /* `char` type.                                                         */

#ifndef FT_CHAR_BIT
#define FT_CHAR_BIT  CHAR_BIT
#endif

#ifndef FT_SIZEOF_INT

  /* The size of an `int` type. */
#if                                 FT_UINT_MAX == 0xFFFFUL
#define FT_SIZEOF_INT  ( 16 / FT_CHAR_BIT )
#elif                               FT_UINT_MAX == 0xFFFFFFFFUL
#define FT_SIZEOF_INT  ( 32 / FT_CHAR_BIT )
#elif FT_UINT_MAX > 0xFFFFFFFFUL && FT_UINT_MAX == 0xFFFFFFFFFFFFFFFFUL
#define FT_SIZEOF_INT  ( 64 / FT_CHAR_BIT )
#else
#error "Unsupported size of `int' type!"
#endif

#endif  /* !defined(FT_SIZEOF_INT) */

#ifndef FT_SIZEOF_LONG

  /* The size of a `long` type.  A five-byte `long` (as used e.g. on the */
  /* DM642) is recognized but avoided.                                   */
#if                                  FT_ULONG_MAX == 0xFFFFFFFFUL
#define FT_SIZEOF_LONG  ( 32 / FT_CHAR_BIT )
#elif FT_ULONG_MAX > 0xFFFFFFFFUL && FT_ULONG_MAX == 0xFFFFFFFFFFUL
#define FT_SIZEOF_LONG  ( 32 / FT_CHAR_BIT )
#elif FT_ULONG_MAX > 0xFFFFFFFFUL && FT_ULONG_MAX == 0xFFFFFFFFFFFFFFFFUL
#define FT_SIZEOF_LONG  ( 64 / FT_CHAR_BIT )
#else
#error "Unsupported size of `long' type!"
#endif

#endif /* !defined(FT_SIZEOF_LONG) */

#ifndef FT_SIZEOF_LONG_LONG

  /* The size of a `long long` type if available */
#if defined( FT_ULLONG_MAX ) && FT_ULLONG_MAX >= 0xFFFFFFFFFFFFFFFFULL
#define FT_SIZEOF_LONG_LONG  ( 64 / FT_CHAR_BIT )
#else
#define FT_SIZEOF_LONG_LONG  0
#endif

#endif /* !defined(FT_SIZEOF_LONG_LONG) */


  /**************************************************************************
   *
   * @section:
   *   basic_types
   *
   */


  /**************************************************************************
   *
   * @type:
   *   FT_Int16
   *
   * @description:
   *   A typedef for a 16bit signed integer type.
   */
  typedef signed short  FT_Int16;


  /**************************************************************************
   *
   * @type:
   *   FT_UInt16
   *
   * @description:
   *   A typedef for a 16bit unsigned integer type.
   */
  typedef unsigned short  FT_UInt16;

  /* */


  /* this #if 0 ... #endif clause is for documentation purposes */
//#if 0

  /**************************************************************************
   *
   * @type:
   *   FT_Int32
   *
   * @description:
   *   A typedef for a 32bit signed integer type.  The size depends on the
   *   configuration.
   */
//  typedef signed XXX  FT_Int32;


  /**************************************************************************
   *
   * @type:
   *   FT_UInt32
   *
   *   A typedef for a 32bit unsigned integer type.  The size depends on the
   *   configuration.
   */
//  typedef unsigned XXX  FT_UInt32;


  /**************************************************************************
   *
   * @type:
   *   FT_Int64
   *
   *   A typedef for a 64bit signed integer type.  The size depends on the
   *   configuration.  Only defined if there is real 64bit support;
   *   otherwise, it gets emulated with a structure (if necessary).
   */
//  typedef signed XXX  FT_Int64;


  /**************************************************************************
   *
   * @type:
   *   FT_UInt64
   *
   *   A typedef for a 64bit unsigned integer type.  The size depends on the
   *   configuration.  Only defined if there is real 64bit support;
   *   otherwise, it gets emulated with a structure (if necessary).
   */
//  typedef unsigned XXX  FT_UInt64;

  /* */

#endif

#if FT_SIZEOF_INT == ( 32 / FT_CHAR_BIT )

  typedef signed int      FT_Int32;
  typedef unsigned int    FT_UInt32;

#elif FT_SIZEOF_LONG == ( 32 / FT_CHAR_BIT )

  typedef signed long     FT_Int32;
  typedef unsigned long   FT_UInt32;

#else
#error "no 32bit type found -- please check your configuration files"
#endif


  /* look up an integer type that is at least 32~bits */
#if FT_SIZEOF_INT >= ( 32 / FT_CHAR_BIT )

  typedef int            FT_Fast;
  typedef unsigned int   FT_UFast;

#elif FT_SIZEOF_LONG >= ( 32 / FT_CHAR_BIT )

  typedef long           FT_Fast;
  typedef unsigned long  FT_UFast;

#endif


  /* determine whether we have a 64-bit integer type */
#if FT_SIZEOF_LONG == ( 64 / FT_CHAR_BIT )

#define FT_INT64   long
#define FT_UINT64  unsigned long

#elif FT_SIZEOF_LONG_LONG >= ( 64 / FT_CHAR_BIT )

#define FT_INT64   long long int
#define FT_UINT64  unsigned long long int

  /**************************************************************************
   *
   * A 64-bit data type may create compilation problems if you compile in
   * strict ANSI mode.  To avoid them, we disable other 64-bit data types if
   * `__STDC__` is defined.  You can however ignore this rule by defining the
   * `FT_CONFIG_OPTION_FORCE_INT64` configuration macro.
   */
#elif !defined( __STDC__ ) || defined( FT_CONFIG_OPTION_FORCE_INT64 )

#if defined( _MSC_VER ) && _MSC_VER >= 900 /* Visual C++ (and Intel C++) */

  /* this compiler provides the `__int64` type */
#define FT_INT64   __int64
#define FT_UINT64  unsigned __int64

#elif defined( __BORLANDC__ )  /* Borland C++ */

  /* XXXX: We should probably check the value of `__BORLANDC__` in order */
  /*       to test the compiler version.                                 */

  /* this compiler provides the `__int64` type */
#define FT_INT64   __int64
#define FT_UINT64  unsigned __int64

#elif defined( __WATCOMC__ ) && __WATCOMC__ >= 1100  /* Watcom C++ */

#define FT_INT64   long long int
#define FT_UINT64  unsigned long long int

#elif defined( __MWERKS__ )    /* Metrowerks CodeWarrior */

#define FT_INT64   long long int
#define FT_UINT64  unsigned long long int

#elif defined( __GNUC__ )

  /* GCC provides the `long long` type */
#define FT_INT64   long long int
#define FT_UINT64  unsigned long long int

#endif /* !__STDC__ */

#endif /* FT_SIZEOF_LONG == (64 / FT_CHAR_BIT) */

#ifdef FT_INT64
  typedef FT_INT64   FT_Int64;
  typedef FT_UINT64  FT_UInt64;
#endif


#endif  /* FREETYPE_CONFIG_INTEGER_TYPES_H_ */
//
// ===========================  ftmodule.h  ===========================
//
/* This is a generated file. 
FT_USE_MODULE( FT_Driver_ClassRec, tt_driver_class )
FT_USE_MODULE( FT_Driver_ClassRec, t1_driver_class )
FT_USE_MODULE( FT_Driver_ClassRec, cff_driver_class )
FT_USE_MODULE( FT_Driver_ClassRec, t1cid_driver_class )
FT_USE_MODULE( FT_Driver_ClassRec, pfr_driver_class )
FT_USE_MODULE( FT_Driver_ClassRec, t42_driver_class )
FT_USE_MODULE( FT_Driver_ClassRec, winfnt_driver_class )
FT_USE_MODULE( FT_Driver_ClassRec, pcf_driver_class )
FT_USE_MODULE( FT_Driver_ClassRec, bdf_driver_class )
FT_USE_MODULE( FT_Module_Class, sfnt_module_class )
FT_USE_MODULE( FT_Module_Class, autofit_module_class )
FT_USE_MODULE( FT_Module_Class, pshinter_module_class )
FT_USE_MODULE( FT_Renderer_Class, ft_smooth_renderer_class )
FT_USE_MODULE( FT_Renderer_Class, ft_raster1_renderer_class )
FT_USE_MODULE( FT_Renderer_Class, ft_svg_renderer_class )
FT_USE_MODULE( FT_Renderer_Class, ft_sdf_renderer_class )
FT_USE_MODULE( FT_Renderer_Class, ft_bitmap_sdf_renderer_class )
FT_USE_MODULE( FT_Module_Class, psaux_module_class )
FT_USE_MODULE( FT_Module_Class, psnames_module_class )
/* EOF */
