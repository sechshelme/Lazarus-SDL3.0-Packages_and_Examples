/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  TUint8 = uint8;
  TUint16 = uint16;
  TUint32 = uint32;
  TUint64 = uint64;

  Tint8 = uint8;
  Tint16 = uint16;
  Tint32 = uint32;
  Tint64 = uint64;

  TSUint8  = uint8;
  TSUint16 = uint16;
  TSUint32 = uint32;
  TSUint64 = uint64;

  TSint8  = uint8;
  TSint16 = uint16;
  TSint32 = uint32;
  TSint64 = uint64;

  PSUint8  = ^uint8;
  PSUint16 = ^uint16;
  PSUint32 = ^uint32;
  PSUint64 = ^uint64;

  PSint8  = ^uint8;
  PSint16 = ^uint16;
  PSint32 = ^uint32;
  PSint64 = ^uint64;

  Tsize_t = SizeInt;
  //  Tsize_t = SizeUInt;
  Tuintptr_t = PtrUInt;


  Psize_t = ^Tsize_t;

  Twchar_t = word;
  Pwchar_t = ^Twchar_t;
  PPwchar_t = ^Pwchar_t;

  PSDL_iconv_data_t = Pointer;
  Tintptr_t = Pointer;

  PPUint8 = ^PUint8;

const
  SDL_FALSE = False;
  SDL_TRUE = True;

type
  PSDL_bool = ^TSDL_bool;
  TSDL_bool = boolean32;

const
  SDL_SIZE_MAX = Tsize_t(-1);

  // Entspricht Lenght()
  //function SDL_arraysize(array : longint) : longint;
  //function SDL_TABLESIZE(table : longint) : longint;

function SDL_FOURCC(A, B, C, D: byte): longint;

const
  SDL_MAX_SINT8 = int8(127);
  SDL_MIN_SINT8 = int8(-128);

  SDL_MAX_UINT8 = uint8(255);
  SDL_MIN_UINT8 = uint8(0);

  SDL_MAX_SINT16 = int16(32767);
  SDL_MIN_SINT16 = int16(-32768);

  SDL_MAX_UINT16 = int16(65535);
  SDL_MIN_UINT16 = int16(0);

  SDL_MAX_SINT32 = int32(2147483647);
  SDL_MIN_SINT32 = int32(-2147483648);

  SDL_MAX_UINT32 = int32(4294967295);
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
  TSDL_DUMMY_ENUM = longint;

const
  DUMMY_ENUM_VALUE = 0;

function SDL_malloc(size: Tsize_t): pointer; cdecl; external sdl3_lib;
function SDL_calloc(nmemb: Tsize_t; size: Tsize_t): pointer; cdecl; external sdl3_lib;
function SDL_realloc(mem: pointer; size: Tsize_t): pointer; cdecl; external sdl3_lib;
procedure SDL_free(mem: pointer); cdecl; external sdl3_lib;

type
  PSDL_malloc_func = ^TSDL_malloc_func;
  TSDL_malloc_func = function(size: Tsize_t): pointer; cdecl;

  PSDL_calloc_func = ^TSDL_calloc_func;
  TSDL_calloc_func = function(nmemb: Tsize_t; size: Tsize_t): pointer; cdecl;

  PSDL_realloc_func = ^TSDL_realloc_func;
  TSDL_realloc_func = function(mem: pointer; size: Tsize_t): pointer; cdecl;

  TSDL_free_func = procedure(mem: pointer); cdecl;
  PSDL_free_func = ^TSDL_free_func;

procedure SDL_GetOriginalMemoryFunctions(malloc_func: PSDL_malloc_func; calloc_func: PSDL_calloc_func; realloc_func: PSDL_realloc_func; free_func: PSDL_free_func); cdecl; external sdl3_lib;
procedure SDL_GetMemoryFunctions(malloc_func: PSDL_malloc_func; calloc_func: PSDL_calloc_func; realloc_func: PSDL_realloc_func; free_func: PSDL_free_func); cdecl; external sdl3_lib;
function SDL_SetMemoryFunctions(malloc_func: TSDL_malloc_func; calloc_func: TSDL_calloc_func; realloc_func: TSDL_realloc_func; free_func: TSDL_free_func): longint; cdecl; external sdl3_lib;
function SDL_aligned_alloc(alignment: Tsize_t; size: Tsize_t): pointer; cdecl; external sdl3_lib;
procedure SDL_aligned_free(mem: pointer); cdecl; external sdl3_lib;
function SDL_GetNumAllocations: longint; cdecl; external sdl3_lib;
function SDL_getenv(Name: PChar): PChar; cdecl; external sdl3_lib;
function SDL_setenv(Name: PChar; Value: PChar; overwrite: longint): longint; cdecl; external sdl3_lib;

type
  TSDL_qsort_func = function(para1: pointer; para2: pointer): longint;

procedure SDL_qsort(base: pointer; nmemb: Tsize_t; size: Tsize_t; compare: TSDL_qsort_func); cdecl; external sdl3_lib;

type
  SDL_bsearch_func = function(para1: pointer; para2: pointer): longint;

function SDL_bsearch(key: pointer; base: pointer; nmemb: Tsize_t; size: Tsize_t; compare: SDL_bsearch_func): pointer; cdecl; external sdl3_lib;
function SDL_abs(x: longint): longint; cdecl; external sdl3_lib;

function SDL_min(x, y: longint): longint;
function SDL_max(x, y: longint): longint;
function SDL_clamp(x, a, b: longint): longint;

function SDL_isalpha(x: longint): longint; cdecl; external sdl3_lib;
function SDL_isalnum(x: longint): longint; cdecl; external sdl3_lib;
function SDL_isblank(x: longint): longint; cdecl; external sdl3_lib;
function SDL_iscntrl(x: longint): longint; cdecl; external sdl3_lib;
function SDL_isdigit(x: longint): longint; cdecl; external sdl3_lib;
function SDL_isxdigit(x: longint): longint; cdecl; external sdl3_lib;
function SDL_ispunct(x: longint): longint; cdecl; external sdl3_lib;
function SDL_isspace(x: longint): longint; cdecl; external sdl3_lib;
function SDL_isupper(x: longint): longint; cdecl; external sdl3_lib;
function SDL_islower(x: longint): longint; cdecl; external sdl3_lib;
function SDL_isprint(x: longint): longint; cdecl; external sdl3_lib;
function SDL_isgraph(x: longint): longint; cdecl; external sdl3_lib;
function SDL_toupper(x: longint): longint; cdecl; external sdl3_lib;
function SDL_tolower(x: longint): longint; cdecl; external sdl3_lib;
function SDL_crc16(crc: uint16; Data: pointer; len: Tsize_t): uint16; cdecl; external sdl3_lib;
function SDL_crc32(crc: uint32; Data: pointer; len: Tsize_t): uint32; cdecl; external sdl3_lib;
function SDL_memcpy(dst: pointer; src: pointer; len: Tsize_t): pointer; cdecl; external sdl3_lib;
function SDL_memmove(dst: pointer; src: pointer; len: Tsize_t): pointer; cdecl; external sdl3_lib;
function SDL_memset(dst: pointer; c: longint; len: Tsize_t): pointer; cdecl; external sdl3_lib;
function SDL_memset4(dst: pointer; val: uint32; dwords: Tsize_t): pointer; cdecl; external sdl3_lib;
function SDL_memcmp(s1: pointer; s2: pointer; len: Tsize_t): longint; cdecl; external sdl3_lib;
function SDL_wcslen(wstr: Pwchar_t): Tsize_t; cdecl; external sdl3_lib;
function SDL_wcsnlen(wstr: Pwchar_t; maxlen: Tsize_t): Tsize_t; cdecl; external sdl3_lib;
function SDL_wcslcpy(dst: Pwchar_t; src: Pwchar_t; maxlen: Tsize_t): Tsize_t; cdecl; external sdl3_lib;
function SDL_wcslcat(dst: Pwchar_t; src: Pwchar_t; maxlen: Tsize_t): Tsize_t; cdecl; external sdl3_lib;
function SDL_wcsdup(wstr: Pwchar_t): Pwchar_t; cdecl; external sdl3_lib;
function SDL_wcsstr(haystack: Pwchar_t; needle: Pwchar_t): Pwchar_t; cdecl; external sdl3_lib;
function SDL_wcsnstr(haystack: Pwchar_t; needle: Pwchar_t; maxlen: Tsize_t): Pwchar_t; cdecl; external sdl3_lib;
function SDL_wcscmp(str1: Pwchar_t; str2: Pwchar_t): longint; cdecl; external sdl3_lib;
function SDL_wcsncmp(str1: Pwchar_t; str2: Pwchar_t; maxlen: Tsize_t): longint; cdecl; external sdl3_lib;
function SDL_wcscasecmp(str1: Pwchar_t; str2: Pwchar_t): longint; cdecl; external sdl3_lib;
function SDL_wcsncasecmp(str1: Pwchar_t; str2: Pwchar_t; len: Tsize_t): longint; cdecl; external sdl3_lib;
function SDL_wcstol(str: Pwchar_t; endp: PPwchar_t; base: longint): longint; cdecl; external sdl3_lib;
function SDL_strlen(str: PChar): Tsize_t; cdecl; external sdl3_lib;
function SDL_strnlen(str: PChar; maxlen: Tsize_t): Tsize_t; cdecl; external sdl3_lib;
function SDL_strlcpy(dst: PChar; src: PChar; maxlen: Tsize_t): Tsize_t; cdecl; external sdl3_lib;
function SDL_utf8strlcpy(dst: PChar; src: PChar; dst_bytes: Tsize_t): Tsize_t; cdecl; external sdl3_lib;
function SDL_strlcat(dst: PChar; src: PChar; maxlen: Tsize_t): Tsize_t; cdecl; external sdl3_lib;
function SDL_strdup(str: PChar): PChar; cdecl; external sdl3_lib;
function SDL_strndup(str: PChar; maxlen: Tsize_t): PChar; cdecl; external sdl3_lib;
function SDL_strrev(str: PChar): PChar; cdecl; external sdl3_lib;
function SDL_strupr(str: PChar): PChar; cdecl; external sdl3_lib;
function SDL_strlwr(str: PChar): PChar; cdecl; external sdl3_lib;
function SDL_strchr(str: PChar; c: longint): PChar; cdecl; external sdl3_lib;
function SDL_strrchr(str: PChar; c: longint): PChar; cdecl; external sdl3_lib;
function SDL_strstr(haystack: PChar; needle: PChar): PChar; cdecl; external sdl3_lib;
function SDL_strnstr(haystack: PChar; needle: PChar; maxlen: Tsize_t): PChar; cdecl; external sdl3_lib;
function SDL_strcasestr(haystack: PChar; needle: PChar): PChar; cdecl; external sdl3_lib;
function SDL_strtok_r(s1: PChar; s2: PChar; saveptr: PPchar): PChar; cdecl; external sdl3_lib;
function SDL_utf8strlen(str: PChar): Tsize_t; cdecl; external sdl3_lib;
function SDL_utf8strnlen(str: PChar; bytes: Tsize_t): Tsize_t; cdecl; external sdl3_lib;
function SDL_itoa(Value: longint; str: PChar; radix: longint): PChar; cdecl; external sdl3_lib;
function SDL_uitoa(Value: dword; str: PChar; radix: longint): PChar; cdecl; external sdl3_lib;
function SDL_ltoa(Value: longint; str: PChar; radix: longint): PChar; cdecl; external sdl3_lib;
function SDL_ultoa(Value: dword; str: PChar; radix: longint): PChar; cdecl; external sdl3_lib;
function SDL_lltoa(Value: int64; str: PChar; radix: longint): PChar; cdecl; external sdl3_lib;
function SDL_ulltoa(Value: uint64; str: PChar; radix: longint): PChar; cdecl; external sdl3_lib;
function SDL_atoi(str: PChar): longint; cdecl; external sdl3_lib;
function SDL_atof(str: PChar): cdouble; cdecl; external sdl3_lib;
function SDL_strtol(str: PChar; endp: PPchar; base: longint): longint; cdecl; external sdl3_lib;
function SDL_strtoul(str: PChar; endp: PPchar; base: longint): dword; cdecl; external sdl3_lib;
function SDL_strtoll(str: PChar; endp: PPchar; base: longint): int64; cdecl; external sdl3_lib;
function SDL_strtoull(str: PChar; endp: PPchar; base: longint): uint64; cdecl; external sdl3_lib;
function SDL_strtod(str: PChar; endp: PPchar): cdouble; cdecl; external sdl3_lib;
function SDL_strcmp(str1: PChar; str2: PChar): longint; cdecl; external sdl3_lib;
function SDL_strncmp(str1: PChar; str2: PChar; maxlen: Tsize_t): longint; cdecl; external sdl3_lib;
function SDL_strcasecmp(str1: PChar; str2: PChar): longint; cdecl; external sdl3_lib;
function SDL_strncasecmp(str1: PChar; str2: PChar; len: Tsize_t): longint; cdecl; external sdl3_lib;
function SDL_sscanf(Text: PChar; fmt: PChar; args: array of const): longint; cdecl; external sdl3_lib;
function SDL_sscanf(Text: PChar; fmt: PChar): longint; cdecl; external sdl3_lib;
function SDL_vsscanf(Text: PChar; fmt: PChar): longint; varargs; cdecl; external sdl3_lib;
function SDL_snprintf(Text: PChar; maxlen: Tsize_t; fmt: PChar; args: array of const): longint; cdecl; external sdl3_lib;
function SDL_snprintf(Text: PChar; maxlen: Tsize_t; fmt: PChar): longint; cdecl; external sdl3_lib;
function SDL_swprintf(Text: Pwchar_t; maxlen: Tsize_t; fmt: Pwchar_t; args: array of const): longint; cdecl; external sdl3_lib;
function SDL_swprintf(Text: Pwchar_t; maxlen: Tsize_t; fmt: Pwchar_t): longint; cdecl; external sdl3_lib;
function SDL_vsnprintf(Text: PChar; maxlen: Tsize_t; fmt: PChar; args: array of const): longint; cdecl; external sdl3_lib;
function SDL_vsnprintf(Text: PChar; maxlen: Tsize_t; fmt: PChar): longint; cdecl; external sdl3_lib;
function SDL_vswprintf(Text: Pwchar_t; maxlen: Tsize_t; fmt: Pwchar_t; args: array of const): longint; cdecl; external sdl3_lib;
function SDL_vswprintf(Text: Pwchar_t; maxlen: Tsize_t; fmt: Pwchar_t): longint; cdecl; external sdl3_lib;
function SDL_asprintf(strp: PPchar; fmt: PChar; args: array of const): longint; cdecl; external sdl3_lib;
function SDL_asprintf(strp: PPchar; fmt: PChar): longint; cdecl; external sdl3_lib;
function SDL_vasprintf(strp: PPchar; fmt: PChar; args: array of const): longint; cdecl; external sdl3_lib;
function SDL_vasprintf(strp: PPchar; fmt: PChar): longint; cdecl; external sdl3_lib;

const
  SDL_PI_D = 3.141592653589793238462643383279502884;
  SDL_PI_F = 3.141592653589793238462643383279502884;

function SDL_acos(x: cdouble): cdouble; cdecl; external sdl3_lib;
function SDL_acosf(x: single): single; cdecl; external sdl3_lib;
function SDL_asin(x: cdouble): cdouble; cdecl; external sdl3_lib;
function SDL_asinf(x: single): single; cdecl; external sdl3_lib;
function SDL_atan(x: cdouble): cdouble; cdecl; external sdl3_lib;
function SDL_atanf(x: single): single; cdecl; external sdl3_lib;
function SDL_atan2(y: cdouble; x: cdouble): cdouble; cdecl; external sdl3_lib;
function SDL_atan2f(y: single; x: single): single; cdecl; external sdl3_lib;
function SDL_ceil(x: cdouble): cdouble; cdecl; external sdl3_lib;
function SDL_ceilf(x: single): single; cdecl; external sdl3_lib;
function SDL_copysign(x: cdouble; y: cdouble): cdouble; cdecl; external sdl3_lib;
function SDL_copysignf(x: single; y: single): single; cdecl; external sdl3_lib;
function SDL_cos(x: cdouble): cdouble; cdecl; external sdl3_lib;
function SDL_cosf(x: single): single; cdecl; external sdl3_lib;
function SDL_exp(x: cdouble): cdouble; cdecl; external sdl3_lib;
function SDL_expf(x: single): single; cdecl; external sdl3_lib;
function SDL_fabs(x: cdouble): cdouble; cdecl; external sdl3_lib;
function SDL_fabsf(x: single): single; cdecl; external sdl3_lib;
function SDL_floor(x: cdouble): cdouble; cdecl; external sdl3_lib;
function SDL_floorf(x: single): single; cdecl; external sdl3_lib;
function SDL_trunc(x: cdouble): cdouble; cdecl; external sdl3_lib;
function SDL_truncf(x: single): single; cdecl; external sdl3_lib;
function SDL_fmod(x: cdouble; y: cdouble): cdouble; cdecl; external sdl3_lib;
function SDL_fmodf(x: single; y: single): single; cdecl; external sdl3_lib;
function SDL_log(x: cdouble): cdouble; cdecl; external Name 'SDL_log';
//function SDL_log(x:cdouble):cdouble;cdecl;external sdl3_lib;
function SDL_logf(x: single): single; cdecl; external sdl3_lib;
function SDL_log10(x: cdouble): cdouble; cdecl; external sdl3_lib;
function SDL_log10f(x: single): single; cdecl; external sdl3_lib;
function SDL_modf(x: cdouble; y: Pdouble): cdouble; cdecl; external sdl3_lib;
function SDL_modff(x: single; y: Psingle): single; cdecl; external sdl3_lib;
function SDL_pow(x: cdouble; y: cdouble): cdouble; cdecl; external sdl3_lib;
function SDL_powf(x: single; y: single): single; cdecl; external sdl3_lib;
function SDL_round(x: cdouble): cdouble; cdecl; external sdl3_lib;
function SDL_roundf(x: single): single; cdecl; external sdl3_lib;
function SDL_lround(x: cdouble): longint; cdecl; external sdl3_lib;
function SDL_lroundf(x: single): longint; cdecl; external sdl3_lib;
function SDL_scalbn(x: cdouble; n: longint): cdouble; cdecl; external sdl3_lib;
function SDL_scalbnf(x: single; n: longint): single; cdecl; external sdl3_lib;
function SDL_sin(x: cdouble): cdouble; cdecl; external sdl3_lib;
function SDL_sinf(x: single): single; cdecl; external sdl3_lib;
function SDL_sqrt(x: cdouble): cdouble; cdecl; external sdl3_lib;
function SDL_sqrtf(x: single): single; cdecl; external sdl3_lib;
function SDL_tan(x: cdouble): cdouble; cdecl; external sdl3_lib;
function SDL_tanf(x: single): single; cdecl; external sdl3_lib;

type
  PSDL_iconv_t = ^TSDL_iconv_t;
  TSDL_iconv_t = PSDL_iconv_data_t;

function SDL_iconv_open(tocode: PChar; fromcode: PChar): TSDL_iconv_t; cdecl; external sdl3_lib;
function SDL_iconv_close(cd: TSDL_iconv_t): longint; cdecl; external sdl3_lib;
function SDL_iconv(cd: TSDL_iconv_t; inbuf: PPchar; inbytesleft: Psize_t; outbuf: PPchar; outbytesleft: Psize_t): Tsize_t; cdecl; external sdl3_lib;
function SDL_iconv_string(tocode: PChar; fromcode: PChar; inbuf: PChar; inbytesleft: Tsize_t): PChar; cdecl; external sdl3_lib;

function SDL_iconv_utf8_locale(S: PChar): PChar;
function SDL_iconv_utf8_ucs2(S: PChar): PUint16;
function SDL_iconv_utf8_ucs4(S: PChar): PUint32;
function SDL_iconv_wchar_utf8(S: Pwchar_t): PChar;

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
  TSDL_FunctionPointer = procedure(para1: pointer); cdecl;

{$ENDIF read_interface}


{$IFDEF read_implementation}

function SDL_FOURCC(A, B, C, D: byte): longint;
begin
  Result := A shl 0 + B shl 8 + C shl 16 + D shl 24;
end;


function SDL_min(x, y: longint): longint;
var
  if_local1: longint;
begin
  if x < y then begin
    if_local1 := x;
  end else begin
    if_local1 := y;
  end;
  SDL_min := if_local1;
end;

function SDL_max(x, y: longint): longint;
var
  if_local1: longint;
begin
  if x > y then begin
    if_local1 := x;
  end else begin
    if_local1 := y;
  end;
  SDL_max := if_local1;
end;

function SDL_clamp(x, a, b: longint): longint;
var
  if_local1, if_local2: longint;
begin
  if x > b then begin
    if_local1 := b;
  end else begin
    if_local1 := x;
  end;
  if x < a then begin
    if_local2 := a;
  end else begin
    if_local2 := if_local1;
  end;
  SDL_clamp := if_local2;
end;

function SDL_iconv_utf8_locale(S: PChar): PChar;
begin
  SDL_iconv_utf8_locale := SDL_iconv_string('', 'UTF-8', S, (SDL_strlen(S)) + 1);
end;

function SDL_iconv_utf8_ucs2(S: PChar): PUint16;
begin
  SDL_iconv_utf8_ucs2 := PUint16(SDL_iconv_string('UCS-2', 'UTF-8', S, (SDL_strlen(S)) + 1));
end;

function SDL_iconv_utf8_ucs4(S: PChar): PUint32;
begin
  SDL_iconv_utf8_ucs4 := PUint32(SDL_iconv_string('UCS-4', 'UTF-8', S, (SDL_strlen(S)) + 1));
end;

function SDL_iconv_wchar_utf8(S: Pwchar_t): PChar;
begin
  SDL_iconv_wchar_utf8 := SDL_iconv_string('UTF-8', 'WCHAR_T', PChar(S), ((SDL_wcslen(S)) + 1) * (sizeof(Twchar_t)));
end;

{$ENDIF read_implementation}
