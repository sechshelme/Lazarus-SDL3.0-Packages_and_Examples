unit SDL3_stdinc;

interface

uses
  ctypes;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

//{$LinkLib 'libSDL3.so.0'}
{$LinkLib 'SDL3'}
//{$LinkLib 'c'}

type
  Tsize_t=SizeInt;
  Psize_t=^Tsize_t;

  Twchar_t=word;
  Pwchar_t=^Twchar_t;
  PPwchar_t=^Pwchar_t;

  PSDL_iconv_data_t=Pointer;
  Tintptr_t=Pointer;

  PPUint8=^PUint8;

const SDL_SIZE_MAX = Tsize_t(-1);

// Entspricht Lenght()
//function SDL_arraysize(array : longint) : longint;
//function SDL_TABLESIZE(table : longint) : longint;

function SDL_FOURCC(A, B, C, D: Byte):longint;

const
  SDL_FALSE = 0;  
  SDL_TRUE = 1;  
type
  PSDL_bool = ^TSDL_bool;
  TSDL_bool = cint;

  const
SDL_MAX_SINT8 = int8(127);
 SDL_MIN_SINT8 = int8(-128);

 SDL_MAX_UINT8 = Uint8(255);
 SDL_MIN_UINT8 = Uint8(0);

 SDL_MAX_SINT16 = int16(32767);
 SDL_MIN_SINT16 = int16(-32768);

 SDL_MAX_UINT16 = int16(65535);
 SDL_MIN_UINT16 = int16(0);

SDL_MAX_SINT32 = int32(2147483647);
SDL_MIN_SINT32 = int32(-2147483648);

 SDL_MAX_UINT32 = int32( 4294967295 );
 SDL_MIN_UINT32 = int32(0);

 SDL_MAX_SINT64 = int64(9223372036854775807);
 SDL_MIN_SINT64 = int64(-9223372036854775808);

 SDL_MAX_UINT64 = int64(18446744073709551615);
 SDL_MIN_UINT64 = int64(0);

  SDL_FLT_EPSILON = 1.1920928955078125e-07;

const
  SDL_PRIs64 = 'lld';
  SDL_PRIu64 = 'llu';
  SDL_PRIx64 = 'llx';
  SDL_PRIs32 = 'd';
  SDL_PRIu32 = 'u';
  SDL_PRIx32 = 'x';

type
  PSDL_DUMMY_ENUM = ^TSDL_DUMMY_ENUM;
  TSDL_DUMMY_ENUM =  Longint;
  Const
    DUMMY_ENUM_VALUE = 0;

function SDL_malloc(size:Tsize_t):pointer;cdecl;external;
function SDL_calloc(nmemb:Tsize_t; size:Tsize_t):pointer;cdecl;external;
function SDL_realloc(mem:pointer; size:Tsize_t):pointer;cdecl;external;
procedure SDL_free(mem:pointer);cdecl;external;
type
  PSDL_malloc_func = ^TSDL_malloc_func;
  TSDL_malloc_func = function (size:Tsize_t):pointer;cdecl;

  PSDL_calloc_func = ^TSDL_calloc_func;
  TSDL_calloc_func = function (nmemb:Tsize_t; size:Tsize_t):pointer;cdecl;

  PSDL_realloc_func = ^TSDL_realloc_func;
  TSDL_realloc_func = function (mem:pointer; size:Tsize_t):pointer;cdecl;

  TSDL_free_func = procedure (mem:pointer);cdecl;
  PSDL_free_func=^TSDL_free_func;
procedure SDL_GetOriginalMemoryFunctions(malloc_func:PSDL_malloc_func; calloc_func:PSDL_calloc_func; realloc_func:PSDL_realloc_func; free_func:PSDL_free_func);cdecl;external;
procedure SDL_GetMemoryFunctions(malloc_func:PSDL_malloc_func; calloc_func:PSDL_calloc_func; realloc_func:PSDL_realloc_func; free_func:PSDL_free_func);cdecl;external;
function SDL_SetMemoryFunctions(malloc_func:TSDL_malloc_func; calloc_func:TSDL_calloc_func; realloc_func:TSDL_realloc_func; free_func:TSDL_free_func):longint;cdecl;external;
function SDL_aligned_alloc(alignment:Tsize_t; size:Tsize_t):pointer;cdecl;external;
procedure SDL_aligned_free(mem:pointer);cdecl;external;
function SDL_GetNumAllocations:longint;cdecl;external;
function SDL_getenv(name:Pchar):Pchar;cdecl;external;
function SDL_setenv(name:Pchar; value:Pchar; overwrite:longint):longint;cdecl;external;
type TSDL_qsort_func=function (para1:pointer; para2:pointer):LongInt;
procedure SDL_qsort(base:pointer; nmemb:Tsize_t; size:Tsize_t; compare:TSDL_qsort_func);cdecl;external;
type SDL_bsearch_func=function (para1:pointer; para2:pointer):longint;
function SDL_bsearch(key:pointer; base:pointer; nmemb:Tsize_t; size:Tsize_t; compare:SDL_bsearch_func):pointer;cdecl;external;
function SDL_abs(x:longint):longint;cdecl;external;

function SDL_min(x,y : longint) : longint;
function SDL_max(x,y : longint) : longint;
function SDL_clamp(x,a,b : longint) : longint;

function SDL_isalpha(x:longint):longint;cdecl;external;
function SDL_isalnum(x:longint):longint;cdecl;external;
function SDL_isblank(x:longint):longint;cdecl;external;
function SDL_iscntrl(x:longint):longint;cdecl;external;
function SDL_isdigit(x:longint):longint;cdecl;external;
function SDL_isxdigit(x:longint):longint;cdecl;external;
function SDL_ispunct(x:longint):longint;cdecl;external;
function SDL_isspace(x:longint):longint;cdecl;external;
function SDL_isupper(x:longint):longint;cdecl;external;
function SDL_islower(x:longint):longint;cdecl;external;
function SDL_isprint(x:longint):longint;cdecl;external;
function SDL_isgraph(x:longint):longint;cdecl;external;
function SDL_toupper(x:longint):longint;cdecl;external;
function SDL_tolower(x:longint):longint;cdecl;external;
function SDL_crc16(crc:Uint16; data:pointer; len:Tsize_t):Uint16;cdecl;external;
function SDL_crc32(crc:Uint32; data:pointer; len:Tsize_t):Uint32;cdecl;external;
function SDL_memcpy(dst:pointer; src:pointer; len:Tsize_t):pointer;cdecl;external;
function SDL_memmove(dst:pointer; src:pointer; len:Tsize_t):pointer;cdecl;external;
function SDL_memset(dst:pointer; c:longint; len:Tsize_t):pointer;cdecl;external;
function SDL_memset4(dst:pointer; val:Uint32; dwords:Tsize_t):pointer;cdecl;external;
function SDL_memcmp(s1:pointer; s2:pointer; len:Tsize_t):longint;cdecl;external;
function SDL_wcslen(wstr:Pwchar_t):Tsize_t;cdecl;external;
function SDL_wcsnlen(wstr:Pwchar_t; maxlen:Tsize_t):Tsize_t;cdecl;external;
function SDL_wcslcpy(dst:Pwchar_t; src:Pwchar_t; maxlen:Tsize_t):Tsize_t;cdecl;external;
function SDL_wcslcat(dst:Pwchar_t; src:Pwchar_t; maxlen:Tsize_t):Tsize_t;cdecl;external;
function SDL_wcsdup(wstr:Pwchar_t):Pwchar_t;cdecl;external;
function SDL_wcsstr(haystack:Pwchar_t; needle:Pwchar_t):Pwchar_t;cdecl;external;
function SDL_wcsnstr(haystack:Pwchar_t; needle:Pwchar_t; maxlen:Tsize_t):Pwchar_t;cdecl;external;
function SDL_wcscmp(str1:Pwchar_t; str2:Pwchar_t):longint;cdecl;external;
function SDL_wcsncmp(str1:Pwchar_t; str2:Pwchar_t; maxlen:Tsize_t):longint;cdecl;external;
function SDL_wcscasecmp(str1:Pwchar_t; str2:Pwchar_t):longint;cdecl;external;
function SDL_wcsncasecmp(str1:Pwchar_t; str2:Pwchar_t; len:Tsize_t):longint;cdecl;external;
function SDL_wcstol(str:Pwchar_t; endp:PPwchar_t; base:longint):longint;cdecl;external;
function SDL_strlen(str:Pchar):Tsize_t;cdecl;external;
function SDL_strnlen(str:Pchar; maxlen:Tsize_t):Tsize_t;cdecl;external;
function SDL_strlcpy(dst:Pchar; src:Pchar; maxlen:Tsize_t):Tsize_t;cdecl;external;
function SDL_utf8strlcpy(dst:Pchar; src:Pchar; dst_bytes:Tsize_t):Tsize_t;cdecl;external;
function SDL_strlcat(dst:Pchar; src:Pchar; maxlen:Tsize_t):Tsize_t;cdecl;external;
function SDL_strdup(str:Pchar):Pchar;cdecl;external;
function SDL_strndup(str:Pchar; maxlen:Tsize_t):Pchar;cdecl;external;
function SDL_strrev(str:Pchar):Pchar;cdecl;external;
function SDL_strupr(str:Pchar):Pchar;cdecl;external;
function SDL_strlwr(str:Pchar):Pchar;cdecl;external;
function SDL_strchr(str:Pchar; c:longint):Pchar;cdecl;external;
function SDL_strrchr(str:Pchar; c:longint):Pchar;cdecl;external;
function SDL_strstr(haystack:Pchar; needle:Pchar):Pchar;cdecl;external;
function SDL_strnstr(haystack:Pchar; needle:Pchar; maxlen:Tsize_t):Pchar;cdecl;external;
function SDL_strcasestr(haystack:Pchar; needle:Pchar):Pchar;cdecl;external;
function SDL_strtok_r(s1:Pchar; s2:Pchar; saveptr:PPchar):Pchar;cdecl;external;
function SDL_utf8strlen(str:Pchar):Tsize_t;cdecl;external;
function SDL_utf8strnlen(str:Pchar; bytes:Tsize_t):Tsize_t;cdecl;external;
function SDL_itoa(value:longint; str:Pchar; radix:longint):Pchar;cdecl;external;
function SDL_uitoa(value:dword; str:Pchar; radix:longint):Pchar;cdecl;external;
function SDL_ltoa(value:longint; str:Pchar; radix:longint):Pchar;cdecl;external;
function SDL_ultoa(value:dword; str:Pchar; radix:longint):Pchar;cdecl;external;
function SDL_lltoa(value:int64; str:Pchar; radix:longint):Pchar;cdecl;external;
function SDL_ulltoa(value:Uint64; str:Pchar; radix:longint):Pchar;cdecl;external;
function SDL_atoi(str:Pchar):longint;cdecl;external;
function SDL_atof(str:Pchar):cdouble;cdecl;external;
function SDL_strtol(str:Pchar; endp:PPchar; base:longint):longint;cdecl;external;
function SDL_strtoul(str:Pchar; endp:PPchar; base:longint):dword;cdecl;external;
function SDL_strtoll(str:Pchar; endp:PPchar; base:longint):int64;cdecl;external;
function SDL_strtoull(str:Pchar; endp:PPchar; base:longint):Uint64;cdecl;external;
function SDL_strtod(str:Pchar; endp:PPchar):cdouble;cdecl;external;
function SDL_strcmp(str1:Pchar; str2:Pchar):longint;cdecl;external;
function SDL_strncmp(str1:Pchar; str2:Pchar; maxlen:Tsize_t):longint;cdecl;external;
function SDL_strcasecmp(str1:Pchar; str2:Pchar):longint;cdecl;external;
function SDL_strncasecmp(str1:Pchar; str2:Pchar; len:Tsize_t):longint;cdecl;external;
function SDL_sscanf(text:Pchar; fmt:Pchar; args:array of const):longint;cdecl;external;
function SDL_sscanf(text:Pchar; fmt:Pchar):longint;cdecl;external;
function SDL_vsscanf(text:Pchar; fmt:Pchar):longint;varargs; cdecl;external;
function SDL_snprintf(text:Pchar; maxlen:Tsize_t; fmt:Pchar; args:array of const):longint;cdecl;external;
function SDL_snprintf(text:Pchar; maxlen:Tsize_t; fmt:Pchar):longint;cdecl;external;
function SDL_swprintf(text:Pwchar_t; maxlen:Tsize_t; fmt:Pwchar_t; args:array of const):longint;cdecl;external;
function SDL_swprintf(text:Pwchar_t; maxlen:Tsize_t; fmt:Pwchar_t):longint;cdecl;external;
function SDL_vsnprintf(text:Pchar; maxlen:Tsize_t; fmt:Pchar; args:array of const):longint;cdecl;external;
function SDL_vsnprintf(text:Pchar; maxlen:Tsize_t; fmt:Pchar):longint;cdecl;external;
function SDL_vswprintf(text:Pwchar_t; maxlen:Tsize_t; fmt:Pwchar_t; args:array of const):longint;cdecl;external;
function SDL_vswprintf(text:Pwchar_t; maxlen:Tsize_t; fmt:Pwchar_t):longint;cdecl;external;
function SDL_asprintf(strp:PPchar; fmt:Pchar; args:array of const):longint;cdecl;external;
function SDL_asprintf(strp:PPchar; fmt:Pchar):longint;cdecl;external;
function SDL_vasprintf(strp:PPchar; fmt:Pchar; args:array of const):longint;cdecl;external;
function SDL_vasprintf(strp:PPchar; fmt:Pchar):longint;cdecl;external;
const
  SDL_PI_D = 3.141592653589793238462643383279502884;  
  SDL_PI_F = 3.141592653589793238462643383279502884;

function SDL_acos(x:cdouble):cdouble;cdecl;external;
function SDL_acosf(x:single):single;cdecl;external;
function SDL_asin(x:cdouble):cdouble;cdecl;external;
function SDL_asinf(x:single):single;cdecl;external;
function SDL_atan(x:cdouble):cdouble;cdecl;external;
function SDL_atanf(x:single):single;cdecl;external;
function SDL_atan2(y:cdouble; x:cdouble):cdouble;cdecl;external;
function SDL_atan2f(y:single; x:single):single;cdecl;external;
function SDL_ceil(x:cdouble):cdouble;cdecl;external;
function SDL_ceilf(x:single):single;cdecl;external;
function SDL_copysign(x:cdouble; y:cdouble):cdouble;cdecl;external;
function SDL_copysignf(x:single; y:single):single;cdecl;external;
function SDL_cos(x:cdouble):cdouble;cdecl;external;
function SDL_cosf(x:single):single;cdecl;external;
function SDL_exp(x:cdouble):cdouble;cdecl;external;
function SDL_expf(x:single):single;cdecl;external;
function SDL_fabs(x:cdouble):cdouble;cdecl;external;
function SDL_fabsf(x:single):single;cdecl;external;
function SDL_floor(x:cdouble):cdouble;cdecl;external;
function SDL_floorf(x:single):single;cdecl;external;
function SDL_trunc(x:cdouble):cdouble;cdecl;external;
function SDL_truncf(x:single):single;cdecl;external;
function SDL_fmod(x:cdouble; y:cdouble):cdouble;cdecl;external;
function SDL_fmodf(x:single; y:single):single;cdecl;external;
function SDL_log(x:cdouble):cdouble;cdecl;external;
function SDL_logf(x:single):single;cdecl;external;
function SDL_log10(x:cdouble):cdouble;cdecl;external;
function SDL_log10f(x:single):single;cdecl;external;
function SDL_modf(x:cdouble; y:Pdouble):cdouble;cdecl;external;
function SDL_modff(x:single; y:Psingle):single;cdecl;external;
function SDL_pow(x:cdouble; y:cdouble):cdouble;cdecl;external;
function SDL_powf(x:single; y:single):single;cdecl;external;
function SDL_round(x:cdouble):cdouble;cdecl;external;
function SDL_roundf(x:single):single;cdecl;external;
function SDL_lround(x:cdouble):longint;cdecl;external;
function SDL_lroundf(x:single):longint;cdecl;external;
function SDL_scalbn(x:cdouble; n:longint):cdouble;cdecl;external;
function SDL_scalbnf(x:single; n:longint):single;cdecl;external;
function SDL_sin(x:cdouble):cdouble;cdecl;external;
function SDL_sinf(x:single):single;cdecl;external;
function SDL_sqrt(x:cdouble):cdouble;cdecl;external;
function SDL_sqrtf(x:single):single;cdecl;external;
function SDL_tan(x:cdouble):cdouble;cdecl;external;
function SDL_tanf(x:single):single;cdecl;external;

type
  PSDL_iconv_t = ^TSDL_iconv_t;
  TSDL_iconv_t = PSDL_iconv_data_t;

function SDL_iconv_open(tocode:Pchar; fromcode:Pchar):TSDL_iconv_t;cdecl;external;
function SDL_iconv_close(cd:TSDL_iconv_t):longint;cdecl;external;
function SDL_iconv(cd:TSDL_iconv_t; inbuf:PPchar; inbytesleft:Psize_t; outbuf:PPchar; outbytesleft:Psize_t):Tsize_t;cdecl;external;
function SDL_iconv_string(tocode:Pchar; fromcode:Pchar; inbuf:Pchar; inbytesleft:Tsize_t):Pchar;cdecl;external;

function SDL_iconv_utf8_locale(S : PChar) : PChar;
function SDL_iconv_utf8_ucs2(S : PChar) : PUint16;
function SDL_iconv_utf8_ucs4(S : PChar) : PUint32;
function SDL_iconv_wchar_utf8(S : Pwchar_t) : PChar;

{*
 * If a * b would overflow, return -1. Otherwise store a * b via ret
 * and return 0.
 *
 * \since This function is available since SDL 3.0.0.
  }
{SDL_FORCE_INLINE int SDL_size_mul_overflow (size_t a, }
{                                            size_t b, }
{                                            size_t *ret) }
{ }
{    if (a != 0 && b > SDL_SIZE_MAX / a)  }
{        return -1; }
{     }
{    *ret = a * b; }
{    return 0; }
{ }
//{$if SDL_HAS_BUILTIN(__builtin_mul_overflow)}
{ This needs to be wrapped in an inline rather than being a direct #define,
 * because __builtin_mul_overflow() is type-generic, but we want to be
 * consistent about interpreting a and b as size_t.  }
{SDL_FORCE_INLINE int SDL_size_mul_overflow_builtin (size_t a, }
{                                                     size_t b, }
{                                                     size_t *ret) }
{ }
{    return __builtin_mul_overflow(a, b, ret) == 0 ? 0 : -1; }
{ }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   


//{$endif}
{*
 * If a + b would overflow, return -1. Otherwise store a + b via ret
 * and return 0.
 *
 * \since This function is available since SDL 3.0.0.
  }
{//SDL_FORCE_INLINE int SDL_size_add_overflow (size_t a, }
{//                                            size_t b, }
{                                            size_t *ret) }
{ }
{    if (b > SDL_SIZE_MAX - a)  }
{        return -1; }
{     }
{    *ret = a + b; }
{    return 0; }
{ }
//{$if SDL_HAS_BUILTIN(__builtin_add_overflow)}
{ This needs to be wrapped in an inline rather than being a direct #define,
 * the same as the call to __builtin_mul_overflow() above.  }
{SDL_FORCE_INLINE int SDL_size_add_overflow_builtin (size_t a, }
{                                                     size_t b, }
{                                                     size_t *ret) }
{ }
{    return __builtin_add_overflow(a, b, ret) == 0 ? 0 : -1; }
{ }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   


type
  PSDL_FunctionPointer = ^TSDL_FunctionPointer;
  TSDL_FunctionPointer = procedure (para1:pointer);cdecl;

implementation

function SDL_FOURCC(A, B, C, D: Byte):longint;
begin
  Result := A shl 0 + B shl 8 + C shl 16 + D shl 24;
end;


function SDL_min(x,y : longint) : longint;
var
   if_local1 : longint;
begin
  if x<y then
    if_local1:=x
  else
    if_local1:=y;
  SDL_min:=if_local1;
end;

function SDL_max(x,y : longint) : longint;
var
   if_local1 : longint;
begin
  if x>y then
    if_local1:=x
  else
    if_local1:=y;
  SDL_max:=if_local1;
end;

function SDL_clamp(x,a,b : longint) : longint;
var
   if_local1, if_local2 : longint;
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

function SDL_iconv_utf8_locale(S : PChar) : PChar;
begin
  SDL_iconv_utf8_locale:=SDL_iconv_string('','UTF-8',S,(SDL_strlen(S))+1);
end;

function SDL_iconv_utf8_ucs2(S : PChar) : PUint16;
begin
  SDL_iconv_utf8_ucs2:=PUint16(SDL_iconv_string('UCS-2','UTF-8',S,(SDL_strlen(S))+1));
end;

function SDL_iconv_utf8_ucs4(S : PChar) : PUint32;
begin
  SDL_iconv_utf8_ucs4:=PUint32(SDL_iconv_string('UCS-4','UTF-8',S,(SDL_strlen(S))+1));
end;

function SDL_iconv_wchar_utf8(S : Pwchar_t) : PChar;
begin
  SDL_iconv_wchar_utf8:=SDL_iconv_string('UTF-8','WCHAR_T',Pchar(S),((SDL_wcslen(S))+1)*(sizeof(Twchar_t)));
end;

end.
