
unit SDL_stdinc;
interface

{
  Automatically converted by H2Pas 1.0.0 from SDL_stdinc.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_stdinc.h
}

    { Pointers to basic pascal types, inserted by h2pas conversion program.}
    Type
      PLongint  = ^Longint;
      PSmallInt = ^SmallInt;
      PByte     = ^Byte;
      PWord     = ^Word;
      PDWord    = ^DWord;
      PDouble   = ^Double;

    Type
    Pchar  = ^char;
    Pdouble  = ^double;
    PSDL_calloc_func  = ^SDL_calloc_func;
    PSDL_free_func  = ^SDL_free_func;
    PSDL_FunctionPointer  = ^SDL_FunctionPointer;
    PSDL_iconv_data_t  = ^SDL_iconv_data_t;
    PSDL_iconv_t  = ^SDL_iconv_t;
    PSDL_malloc_func  = ^SDL_malloc_func;
    PSDL_realloc_func  = ^SDL_realloc_func;
    Psingle  = ^single;
    Psize_t  = ^size_t;
    PUint16  = ^Uint16;
    PUint32  = ^Uint32;
    PUint64  = ^Uint64;
    Pwchar_t  = ^wchar_t;
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
 * # CategoryStdinc
 *
 * This is a general header that includes C language support. It implements a
 * subset of the C runtime: these should all behave the same way as their C
 * runtime equivalents, but with an SDL_ prefix.
  }
type
  PSDL_malloc_func = ^TSDL_malloc_func;
  TSDL_malloc_func = function (size:Tsize_t):pointer;cdecl;

  PSDL_calloc_func = ^TSDL_calloc_func;
  TSDL_calloc_func = function (nmemb:Tsize_t; size:Tsize_t):pointer;cdecl;

  PSDL_realloc_func = ^TSDL_realloc_func;
  TSDL_realloc_func = function (mem:pointer; size:Tsize_t):pointer;cdecl;

  TSDL_free_func = procedure (mem:pointer);cdecl;
{*
 * Get the original set of SDL memory functions.
 *
 * This is what and friends will use by default, if there has been
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

procedure SDL_GetOriginalMemoryFunctions(malloc_func:PSDL_malloc_func; calloc_func:PSDL_calloc_func; realloc_func:PSDL_realloc_func; free_func:PSDL_free_func);cdecl;external;
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
procedure SDL_GetMemoryFunctions(malloc_func:PSDL_malloc_func; calloc_func:PSDL_calloc_func; realloc_func:PSDL_realloc_func; free_func:PSDL_free_func);cdecl;external;
{*
 * Replace SDL's memory allocation functions with a custom set.
 *
 * It is not safe to call this function once any allocations have been made,
 * as future calls to SDL_free will use the new allocator, even if they came
 * from an made with the old one!
 *
 * If used, usually this needs to be the first call made into the SDL library,
 * if not the very first thing done at program startup time.
 *
 * \param malloc_func custom malloc function.
 * \param calloc_func custom calloc function.
 * \param realloc_func custom realloc function.
 * \param free_func custom free function.
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
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
function SDL_SetMemoryFunctions(malloc_func:TSDL_malloc_func; calloc_func:TSDL_calloc_func; realloc_func:TSDL_realloc_func; free_func:TSDL_free_func):longint;cdecl;external;
{*
 * Allocate memory aligned to a specific value.
 *
 * If `alignment` is less than the size of `void *`, then it will be increased
 * to match that.
 *
 * The returned memory address will be a multiple of the alignment value, and
 * the amount of memory allocated will be a multiple of the alignment value.
 *
 * The memory returned by this function must be freed with SDL_aligned_free(),
 * and _not_ SDL_free.
 *
 * \param alignment the alignment requested.
 * \param size the size to allocate.
 * \returns a pointer to the aligned memory.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_aligned_free
  }
function SDL_aligned_alloc(alignment:Tsize_t; size:Tsize_t):pointer;cdecl;external;
{*
 * Free memory allocated by SDL_aligned_alloc().
 *
 * The pointer is no longer valid after this call and cannot be dereferenced
 * anymore.
 *
 * \param mem a pointer previously returned by SDL_aligned_alloc.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_aligned_alloc
  }
procedure SDL_aligned_free(mem:pointer);cdecl;external;
{*
 * Get the number of outstanding (unfreed) allocations.
 *
 * \returns the number of allocations.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetNumAllocations:longint;cdecl;external;
(* Const before type ignored *)
function SDL_getenv(name:Pchar):Pchar;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_setenv(name:Pchar; value:Pchar; overwrite:longint):longint;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
type

  TSDL_CompareCallback = function (a:pointer; b:pointer):longint;cdecl;

procedure SDL_qsort(base:pointer; nmemb:Tsize_t; size:Tsize_t; compare:TSDL_CompareCallback);cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_bsearch(key:pointer; base:pointer; nmemb:Tsize_t; size:Tsize_t; compare:TSDL_CompareCallback):pointer;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
type

  TSDL_CompareCallback_r = function (userdata:pointer; a:pointer; b:pointer):longint;cdecl;

procedure SDL_qsort_r(base:pointer; nmemb:Tsize_t; size:Tsize_t; compare:TSDL_CompareCallback_r; userdata:pointer);cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_bsearch_r(key:pointer; base:pointer; nmemb:Tsize_t; size:Tsize_t; compare:TSDL_CompareCallback_r; 
           userdata:pointer):pointer;cdecl;external;
function SDL_abs(x:longint):longint;cdecl;external;
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
function SDL_isalpha(x:longint):longint;cdecl;external;
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
function SDL_isalnum(x:longint):longint;cdecl;external;
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
function SDL_isblank(x:longint):longint;cdecl;external;
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
function SDL_iscntrl(x:longint):longint;cdecl;external;
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
function SDL_isdigit(x:longint):longint;cdecl;external;
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
function SDL_isxdigit(x:longint):longint;cdecl;external;
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
function SDL_ispunct(x:longint):longint;cdecl;external;
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
function SDL_isspace(x:longint):longint;cdecl;external;
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
function SDL_isupper(x:longint):longint;cdecl;external;
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
function SDL_islower(x:longint):longint;cdecl;external;
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
function SDL_isprint(x:longint):longint;cdecl;external;
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
function SDL_isgraph(x:longint):longint;cdecl;external;
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
function SDL_toupper(x:longint):longint;cdecl;external;
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
function SDL_tolower(x:longint):longint;cdecl;external;
(* Const before type ignored *)
function SDL_crc16(crc:TUint16; data:pointer; len:Tsize_t):TUint16;cdecl;external;
(* Const before type ignored *)
function SDL_crc32(crc:TUint32; data:pointer; len:Tsize_t):TUint32;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_memcmp(s1:pointer; s2:pointer; len:Tsize_t):longint;cdecl;external;
(* Const before type ignored *)
function SDL_wcslen(wstr:Pwchar_t):Tsize_t;cdecl;external;
(* Const before type ignored *)
function SDL_wcsnlen(wstr:Pwchar_t; maxlen:Tsize_t):Tsize_t;cdecl;external;
(* Const before type ignored *)
function SDL_wcsdup(wstr:Pwchar_t):Pwchar_t;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_wcsstr(haystack:Pwchar_t; needle:Pwchar_t):Pwchar_t;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_wcsnstr(haystack:Pwchar_t; needle:Pwchar_t; maxlen:Tsize_t):Pwchar_t;cdecl;external;
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
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_wcscmp(str1:Pwchar_t; str2:Pwchar_t):longint;cdecl;external;
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
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_wcsncmp(str1:Pwchar_t; str2:Pwchar_t; maxlen:Tsize_t):longint;cdecl;external;
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
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_wcscasecmp(str1:Pwchar_t; str2:Pwchar_t):longint;cdecl;external;
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
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_wcsncasecmp(str1:Pwchar_t; str2:Pwchar_t; maxlen:Tsize_t):longint;cdecl;external;
(* Const before type ignored *)
function SDL_wcstol(str:Pwchar_t; endp:PPwchar_t; base:longint):longint;cdecl;external;
(* Const before type ignored *)
function SDL_strlen(str:Pchar):Tsize_t;cdecl;external;
(* Const before type ignored *)
function SDL_strnlen(str:Pchar; maxlen:Tsize_t):Tsize_t;cdecl;external;
(* error 
extern  size_t  SDL_strlcpy(SDL_OUT_Z_CAP(maxlen) char *dst, const char *src, size_t maxlen);
(* error 
extern  size_t  SDL_strlcpy(SDL_OUT_Z_CAP(maxlen) char *dst, const char *src, size_t maxlen);
(* error 
extern  size_t  SDL_strlcpy(SDL_OUT_Z_CAP(maxlen) char *dst, const char *src, size_t maxlen);
 in declarator_list *)
 in declarator_list *)
 in declarator_list *)
(* error 
extern  size_t  SDL_utf8strlcpy(SDL_OUT_Z_CAP(dst_bytes) char *dst, const char *src, size_t dst_bytes);
(* error 
extern  size_t  SDL_utf8strlcpy(SDL_OUT_Z_CAP(dst_bytes) char *dst, const char *src, size_t dst_bytes);
(* error 
extern  size_t  SDL_utf8strlcpy(SDL_OUT_Z_CAP(dst_bytes) char *dst, const char *src, size_t dst_bytes);
 in declarator_list *)
 in declarator_list *)
 in declarator_list *)
(* error 
extern  size_t  SDL_strlcat(SDL_INOUT_Z_CAP(maxlen) char *dst, const char *src, size_t maxlen);
(* error 
extern  size_t  SDL_strlcat(SDL_INOUT_Z_CAP(maxlen) char *dst, const char *src, size_t maxlen);
(* error 
extern  size_t  SDL_strlcat(SDL_INOUT_Z_CAP(maxlen) char *dst, const char *src, size_t maxlen);
 in declarator_list *)
 in declarator_list *)
 in declarator_list *)
(* Const before type ignored *)
function SDL_strdup(str:Pchar):Pchar;cdecl;external;
(* Const before type ignored *)
function SDL_strndup(str:Pchar; maxlen:Tsize_t):Pchar;cdecl;external;
function SDL_strrev(str:Pchar):Pchar;cdecl;external;
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
function SDL_strupr(str:Pchar):Pchar;cdecl;external;
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
function SDL_strlwr(str:Pchar):Pchar;cdecl;external;
(* Const before type ignored *)
function SDL_strchr(str:Pchar; c:longint):Pchar;cdecl;external;
(* Const before type ignored *)
function SDL_strrchr(str:Pchar; c:longint):Pchar;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_strstr(haystack:Pchar; needle:Pchar):Pchar;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_strnstr(haystack:Pchar; needle:Pchar; maxlen:Tsize_t):Pchar;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_strcasestr(haystack:Pchar; needle:Pchar):Pchar;cdecl;external;
(* Const before type ignored *)
function SDL_strtok_r(s1:Pchar; s2:Pchar; saveptr:PPchar):Pchar;cdecl;external;
(* Const before type ignored *)
function SDL_utf8strlen(str:Pchar):Tsize_t;cdecl;external;
(* Const before type ignored *)
function SDL_utf8strnlen(str:Pchar; bytes:Tsize_t):Tsize_t;cdecl;external;
function SDL_itoa(value:longint; str:Pchar; radix:longint):Pchar;cdecl;external;
function SDL_uitoa(value:dword; str:Pchar; radix:longint):Pchar;cdecl;external;
function SDL_ltoa(value:longint; str:Pchar; radix:longint):Pchar;cdecl;external;
function SDL_ultoa(value:dword; str:Pchar; radix:longint):Pchar;cdecl;external;
function SDL_lltoa(value:TSint64; str:Pchar; radix:longint):Pchar;cdecl;external;
function SDL_ulltoa(value:TUint64; str:Pchar; radix:longint):Pchar;cdecl;external;
(* Const before type ignored *)
function SDL_atoi(str:Pchar):longint;cdecl;external;
(* Const before type ignored *)
function SDL_atof(str:Pchar):Tdouble;cdecl;external;
(* Const before type ignored *)
function SDL_strtol(str:Pchar; endp:PPchar; base:longint):longint;cdecl;external;
(* Const before type ignored *)
function SDL_strtoul(str:Pchar; endp:PPchar; base:longint):dword;cdecl;external;
(* Const before type ignored *)
function SDL_strtoll(str:Pchar; endp:PPchar; base:longint):TSint64;cdecl;external;
(* Const before type ignored *)
function SDL_strtoull(str:Pchar; endp:PPchar; base:longint):TUint64;cdecl;external;
(* Const before type ignored *)
function SDL_strtod(str:Pchar; endp:PPchar):Tdouble;cdecl;external;
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
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_strcmp(str1:Pchar; str2:Pchar):longint;cdecl;external;
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
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_strncmp(str1:Pchar; str2:Pchar; maxlen:Tsize_t):longint;cdecl;external;
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
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_strcasecmp(str1:Pchar; str2:Pchar):longint;cdecl;external;
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
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_strncasecmp(str1:Pchar; str2:Pchar; maxlen:Tsize_t):longint;cdecl;external;
(* Const before type ignored *)
(* error 
extern  int  SDL_sscanf(const char *text, SDL_SCANF_FORMAT_STRING const char *fmt, ...) SDL_SCANF_VARARG_FUNC(2);
(* error 
extern  int  SDL_sscanf(const char *text, SDL_SCANF_FORMAT_STRING const char *fmt, ...) SDL_SCANF_VARARG_FUNC(2);
 in declarator_list *)
 in declarator_list *)
(* Const before type ignored *)
(* error 
extern  int  SDL_vsscanf(const char *text, SDL_SCANF_FORMAT_STRING const char *fmt, va_list ap) SDL_SCANF_VARARG_FUNCV(2);
(* error 
extern  int  SDL_vsscanf(const char *text, SDL_SCANF_FORMAT_STRING const char *fmt, va_list ap) SDL_SCANF_VARARG_FUNCV(2);
 in declarator_list *)
 in declarator_list *)
(* error 
extern  int  SDL_snprintf(SDL_OUT_Z_CAP(maxlen) char *text, size_t maxlen, SDL_PRINTF_FORMAT_STRING const char *fmt, ... ) SDL_PRINTF_VARARG_FUNC(3);
(* error 
extern  int  SDL_snprintf(SDL_OUT_Z_CAP(maxlen) char *text, size_t maxlen, SDL_PRINTF_FORMAT_STRING const char *fmt, ... ) SDL_PRINTF_VARARG_FUNC(3);
(* error 
extern  int  SDL_snprintf(SDL_OUT_Z_CAP(maxlen) char *text, size_t maxlen, SDL_PRINTF_FORMAT_STRING const char *fmt, ... ) SDL_PRINTF_VARARG_FUNC(3);
(* error 
extern  int  SDL_snprintf(SDL_OUT_Z_CAP(maxlen) char *text, size_t maxlen, SDL_PRINTF_FORMAT_STRING const char *fmt, ... ) SDL_PRINTF_VARARG_FUNC(3);
 in declarator_list *)
 in declarator_list *)
 in declarator_list *)
 in declarator_list *)
(* error 
extern  int  SDL_swprintf(SDL_OUT_Z_CAP(maxlen) wchar_t *text, size_t maxlen, SDL_PRINTF_FORMAT_STRING const wchar_t *fmt, ... ) SDL_WPRINTF_VARARG_FUNC(3);
(* error 
extern  int  SDL_swprintf(SDL_OUT_Z_CAP(maxlen) wchar_t *text, size_t maxlen, SDL_PRINTF_FORMAT_STRING const wchar_t *fmt, ... ) SDL_WPRINTF_VARARG_FUNC(3);
(* error 
extern  int  SDL_swprintf(SDL_OUT_Z_CAP(maxlen) wchar_t *text, size_t maxlen, SDL_PRINTF_FORMAT_STRING const wchar_t *fmt, ... ) SDL_WPRINTF_VARARG_FUNC(3);
(* error 
extern  int  SDL_swprintf(SDL_OUT_Z_CAP(maxlen) wchar_t *text, size_t maxlen, SDL_PRINTF_FORMAT_STRING const wchar_t *fmt, ... ) SDL_WPRINTF_VARARG_FUNC(3);
 in declarator_list *)
 in declarator_list *)
 in declarator_list *)
 in declarator_list *)
(* error 
extern  int  SDL_vsnprintf(SDL_OUT_Z_CAP(maxlen) char *text, size_t maxlen, SDL_PRINTF_FORMAT_STRING const char *fmt, va_list ap) SDL_PRINTF_VARARG_FUNCV(3);
(* error 
extern  int  SDL_vsnprintf(SDL_OUT_Z_CAP(maxlen) char *text, size_t maxlen, SDL_PRINTF_FORMAT_STRING const char *fmt, va_list ap) SDL_PRINTF_VARARG_FUNCV(3);
(* error 
extern  int  SDL_vsnprintf(SDL_OUT_Z_CAP(maxlen) char *text, size_t maxlen, SDL_PRINTF_FORMAT_STRING const char *fmt, va_list ap) SDL_PRINTF_VARARG_FUNCV(3);
(* error 
extern  int  SDL_vsnprintf(SDL_OUT_Z_CAP(maxlen) char *text, size_t maxlen, SDL_PRINTF_FORMAT_STRING const char *fmt, va_list ap) SDL_PRINTF_VARARG_FUNCV(3);
 in declarator_list *)
 in declarator_list *)
 in declarator_list *)
 in declarator_list *)
(* error 
extern  int  SDL_vswprintf(SDL_OUT_Z_CAP(maxlen) wchar_t *text, size_t maxlen, const wchar_t *fmt, va_list ap);
(* error 
extern  int  SDL_vswprintf(SDL_OUT_Z_CAP(maxlen) wchar_t *text, size_t maxlen, const wchar_t *fmt, va_list ap);
(* error 
extern  int  SDL_vswprintf(SDL_OUT_Z_CAP(maxlen) wchar_t *text, size_t maxlen, const wchar_t *fmt, va_list ap);
(* error 
extern  int  SDL_vswprintf(SDL_OUT_Z_CAP(maxlen) wchar_t *text, size_t maxlen, const wchar_t *fmt, va_list ap);
 in declarator_list *)
 in declarator_list *)
 in declarator_list *)
 in declarator_list *)
(* error 
extern  int  SDL_asprintf(char **strp, SDL_PRINTF_FORMAT_STRING const char *fmt, ...) SDL_PRINTF_VARARG_FUNC(2);
(* error 
extern  int  SDL_asprintf(char **strp, SDL_PRINTF_FORMAT_STRING const char *fmt, ...) SDL_PRINTF_VARARG_FUNC(2);
 in declarator_list *)
 in declarator_list *)
(* error 
extern  int  SDL_vasprintf(char **strp, SDL_PRINTF_FORMAT_STRING const char *fmt, va_list ap) SDL_PRINTF_VARARG_FUNCV(2);
(* error 
extern  int  SDL_vasprintf(char **strp, SDL_PRINTF_FORMAT_STRING const char *fmt, va_list ap) SDL_PRINTF_VARARG_FUNCV(2);
 in declarator_list *)
 in declarator_list *)
{*
 * Seed the pseudo-random number generator
 *
 * \param seed the value to use as a random number seed, or 0 to use
 *             SDL_GetPerformanceCounter().
 *
 * \threadsafety This should be called on the same thread that calls
 *               SDL_rand()
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_rand
  }
procedure SDL_srand(seed:TUint64);cdecl;external;
{*
 * Get a pseudo-random number.
 *
 * There are no guarantees as to the quality of the random sequence produced,
 * and this should not be used for cryptography or anything that requires good
 * random distribution. There are many random number libraries available with
 * different characteristics and you should pick one of those to meet any
 * serious needs.
 *
 * \returns a random value in the range of [0-SDL_MAX_UINT32].
 *
 * \threadsafety All calls should be made from a single thread, use
 *               SDL_rand_r() when using multiple threads.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_rand_r
 * \sa SDL_srand
  }
function SDL_rand:TUint32;cdecl;external;
{*
 * Get a pseudo-random number.
 *
 * There are no guarantees as to the quality of the random sequence produced,
 * and this should not be used for cryptography or anything that requires good
 * random distribution. There are many random number libraries available with
 * different characteristics and you should pick one of those to meet any
 * serious needs.
 *
 * \param state a pointer to a 64-bit seed value that will be updated with
 *              each call to SDL_rand_r(). If the value of the seed is 0, it
 *              will be initialized with SDL_GetPerformanceCounter().
 * \returns a random value in the range of [0-SDL_MAX_UINT32], or 0 if state
 *          is NULL.
 *
 * \threadsafety This can be called from any thread, however each thread
 *               should pass its own state pointer.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_rand
  }
function SDL_rand_r(state:PUint64):TUint32;cdecl;external;
{$ifndef SDL_PI_D}
{*< pi (double)  }

const
  SDL_PI_D = 3.141592653589793238462643383279502884;  
{$endif}
{$ifndef SDL_PI_F}
(* error 
#define SDL_PI_F   3.141592653589793238462643383279502884F      /**< pi (float) */
{*< pi (float)  }
in define line 819 *)
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

function SDL_acos(x:Tdouble):Tdouble;cdecl;external;
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
function SDL_acosf(x:single):single;cdecl;external;
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
function SDL_asin(x:Tdouble):Tdouble;cdecl;external;
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
function SDL_asinf(x:single):single;cdecl;external;
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
function SDL_atan(x:Tdouble):Tdouble;cdecl;external;
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
function SDL_atanf(x:single):single;cdecl;external;
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
function SDL_atan2(y:Tdouble; x:Tdouble):Tdouble;cdecl;external;
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
function SDL_atan2f(y:single; x:single):single;cdecl;external;
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
function SDL_ceil(x:Tdouble):Tdouble;cdecl;external;
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
function SDL_ceilf(x:single):single;cdecl;external;
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
function SDL_copysign(x:Tdouble; y:Tdouble):Tdouble;cdecl;external;
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
function SDL_copysignf(x:single; y:single):single;cdecl;external;
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
function SDL_cos(x:Tdouble):Tdouble;cdecl;external;
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
function SDL_cosf(x:single):single;cdecl;external;
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
function SDL_exp(x:Tdouble):Tdouble;cdecl;external;
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
function SDL_expf(x:single):single;cdecl;external;
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
function SDL_fabs(x:Tdouble):Tdouble;cdecl;external;
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
function SDL_fabsf(x:single):single;cdecl;external;
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
function SDL_floor(x:Tdouble):Tdouble;cdecl;external;
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
function SDL_floorf(x:single):single;cdecl;external;
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
function SDL_trunc(x:Tdouble):Tdouble;cdecl;external;
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
function SDL_truncf(x:single):single;cdecl;external;
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
function SDL_fmod(x:Tdouble; y:Tdouble):Tdouble;cdecl;external;
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
function SDL_fmodf(x:single; y:single):single;cdecl;external;
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
function SDL_log(x:Tdouble):Tdouble;cdecl;external;
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
function SDL_logf(x:single):single;cdecl;external;
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
function SDL_log10(x:Tdouble):Tdouble;cdecl;external;
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
function SDL_log10f(x:single):single;cdecl;external;
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
function SDL_modf(x:Tdouble; y:Pdouble):Tdouble;cdecl;external;
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
function SDL_modff(x:single; y:Psingle):single;cdecl;external;
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
function SDL_pow(x:Tdouble; y:Tdouble):Tdouble;cdecl;external;
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
function SDL_powf(x:single; y:single):single;cdecl;external;
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
function SDL_round(x:Tdouble):Tdouble;cdecl;external;
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
function SDL_roundf(x:single):single;cdecl;external;
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
function SDL_lround(x:Tdouble):longint;cdecl;external;
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
function SDL_lroundf(x:single):longint;cdecl;external;
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
function SDL_scalbn(x:Tdouble; n:longint):Tdouble;cdecl;external;
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
function SDL_scalbnf(x:single; n:longint):single;cdecl;external;
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
function SDL_sin(x:Tdouble):Tdouble;cdecl;external;
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
function SDL_sinf(x:single):single;cdecl;external;
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
function SDL_sqrt(x:Tdouble):Tdouble;cdecl;external;
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
function SDL_sqrtf(x:single):single;cdecl;external;
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
function SDL_tan(x:Tdouble):Tdouble;cdecl;external;
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
function SDL_tanf(x:single):single;cdecl;external;
    { The SDL implementation of iconv() returns these error codes  }
    { was #define dname def_expr }
    function SDL_ICONV_ERROR : Tsize_t;      

    { was #define dname def_expr }
    function SDL_ICONV_E2BIG : Tsize_t;      

    { was #define dname def_expr }
    function SDL_ICONV_EILSEQ : Tsize_t;      

    { was #define dname def_expr }
    function SDL_ICONV_EINVAL : Tsize_t;      

    { SDL_iconv_* are now always real symbols/types, not macros or inlined.  }
    type
      PSDL_iconv_t = ^TSDL_iconv_t;
      TSDL_iconv_t = PSDL_iconv_data_t;
(* Const before type ignored *)
(* Const before type ignored *)

function SDL_iconv_open(tocode:Pchar; fromcode:Pchar):TSDL_iconv_t;cdecl;external;
function SDL_iconv_close(cd:TSDL_iconv_t):longint;cdecl;external;
(* Const before type ignored *)
function SDL_iconv(cd:TSDL_iconv_t; inbuf:PPchar; inbytesleft:Psize_t; outbuf:PPchar; outbytesleft:Psize_t):Tsize_t;cdecl;external;
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
      }
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_iconv_string(tocode:Pchar; fromcode:Pchar; inbuf:Pchar; inbytesleft:Tsize_t):Pchar;cdecl;external;
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
(* Const before type ignored *)

function strlcpy(dst:Pchar; src:Pchar; size:Tsize_t):Tsize_t;cdecl;external;
{$endif}
    { The analyzer knows about strlcat even when the system doesn't provide it  }
{$if !defined(HAVE_STRLCAT) && !defined(strlcat)}
(* Const before type ignored *)

function strlcat(dst:Pchar; src:Pchar; size:Tsize_t):Tsize_t;cdecl;external;
{$endif}
{$if !defined(HAVE_WCSLCPY) && !defined(wcslcpy)}
(* Const before type ignored *)

function wcslcpy(dst:Pwchar_t; src:Pwchar_t; size:Tsize_t):Tsize_t;cdecl;external;
{$endif}
{$if !defined(HAVE_WCSLCAT) && !defined(wcslcat)}
(* Const before type ignored *)

function wcslcat(dst:Pwchar_t; src:Pwchar_t; size:Tsize_t):Tsize_t;cdecl;external;
{$endif}
    { Starting LLVM 16, the analyser errors out if these functions do not have
       their prototype defined (clang-diagnostic-implicit-function-declaration)  }
{$include <stdlib.h>}
{$include <stdio.h>}
{$define malloc}    

    const
      SDL_calloc = calloc;      
      SDL_realloc = realloc;      
      SDL_free = free;      
{$ifndef SDL_memcpy}

    const
      SDL_memcpy = memcpy;      
{$endif}
{$ifndef SDL_memmove}

    const
      SDL_memmove = memmove;      
{$endif}
{$ifndef SDL_memset}

    const
      SDL_memset = memset;      
{$endif}

    const
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
      SDL_sscanf = sscanf;      
      SDL_vsscanf = vsscanf;      
      SDL_snprintf = snprintf;      
      SDL_vsnprintf = vsnprintf;      
{$endif}
    {*
     * Multiply two integers, checking for overflow.
     *
     * If `a * b` would overflow, return -1.
     *
     * Otherwise store `a * b` via ret and return 0.
     *
     * \param a the multiplicand.
     * \param b the multiplier.
     * \param ret on non-overflow output, stores the multiplication result. May
     *            not be NULL.
     * \returns -1 on overflow, 0 if result doesn't overflow.
     *
     * \threadsafety It is safe to call this function from any thread.
     *
     * \since This function is available since SDL 3.0.0.
      }
(* error 
SDL_FORCE_INLINE int SDL_size_mul_overflow (size_t a,
in declaration at line 2214 *)
(* error 
    }
in declaration at line 2216 *)
(* error 
    return 0;
in declaration at line 2217 *)
(* error 
}
{$ifndef SDL_WIKI_DOCUMENTATION_SECTION}
{$if SDL_HAS_BUILTIN(__builtin_mul_overflow)}
    { This needs to be wrapped in an inline rather than being a direct #define,
     * because __builtin_mul_overflow() is type-generic, but we want to be
     * consistent about interpreting a and b as size_t.  }
in declaration at line 2229 *)
(* error 
}
in define line 2231 *)
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
     * \param ret on non-overflow output, stores the addition result. May not be
     *            NULL.
     * \returns -1 on overflow, 0 if result doesn't overflow.
     *
     * \threadsafety It is safe to call this function from any thread.
     *
     * \since This function is available since SDL 3.0.0.
      }
(* error 
SDL_FORCE_INLINE int SDL_size_add_overflow (size_t a,
in declaration at line 2257 *)
(* error 
    }
in declaration at line 2259 *)
(* error 
    return 0;
in declaration at line 2260 *)
(* error 
}
{$ifndef SDL_WIKI_DOCUMENTATION_SECTION}
{$if SDL_HAS_BUILTIN(__builtin_add_overflow)}
    { This needs to be wrapped in an inline rather than being a direct #define,
     * the same as the call to __builtin_mul_overflow() above.  }
in declaration at line 2271 *)
(* error 
}
in define line 2273 *)
{$endif}
{$endif}
    { This is a generic function pointer which should be cast to the type you expect  }
{$ifdef SDL_FUNCTION_POINTER_IS_VOID_POINTER}
    type
      PSDL_FunctionPointer = ^TSDL_FunctionPointer;
      TSDL_FunctionPointer = pointer;
{$else}
    type

      TSDL_FunctionPointer = procedure (para1:pointer);cdecl;
{$endif}
    { Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
    { SDL_stdinc_h_  }

implementation

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
      SDL_iconv_wchar_utf8:=SDL_iconv_string('UTF-8','WCHAR_T',Pchar(S),((SDL_wcslen(S))+1)*(sizeof(wchar_t)));
    end;


end.
