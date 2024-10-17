unit SDL_stdinc;

interface

uses
  crt;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  PSDL_GLContextState=Pointer; // ??????????????????????

  Tbool = boolean;
  Tdouble = double;

  TUint8 = uint8;
  TUint16 = uint16;
  TUint32 = uint32;
  TUint64 = uint64;

  Tint8 = uint8;
  Tint16 = uint16;
  Tint32 = uint32;
  Tint64 = uint64;

  TSUint8 = uint8;
  TSUint16 = uint16;
  TSUint32 = uint32;
  TSUint64 = uint64;

  TSint8 = int8;
  TSint16 = int16;
  TSint32 = int32;
  TSint64 = int64;

  PSUint8 = ^uint8;
  PSUint16 = ^uint16;
  PSUint32 = ^uint32;
  PSUint64 = ^uint64;

  PSint8 = ^int8;
  PSint16 = ^int16;
  PSint32 = ^int32;
  PSint64 = ^int64;

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

  PSDL_Time = ^TSDL_Time;
  TSDL_Time = TSint64;

  Tva_list = Pointer; // ?????


const
  SDL_FALSE = False;
  SDL_TRUE = True;

type
  PSDL_bool = ^TSDL_bool;
  TSDL_bool = boolean32;

const
  SDL_SIZE_MAX = Tsize_t(-1);
  SDL_HAS_BUILTIN = 0;


  // Entspricht Lenght() von Static Arrays
  // function SDL_arraysize(array : longint) : longint;
  // #define SDL_arraysize(array) (sizeof(array)/sizeof(array[0]))

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


const
  SDL_MAX_TIME = SDL_MAX_SINT64;
  SDL_MIN_TIME = SDL_MIN_SINT64;


  SDL_FLT_EPSILON = single(1.1920928955078125e-07);

  SDL_PI_D = double(Pi);
  SDL_PI_F = single(Pi);

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

function SDL_malloc(size: Tsize_t): pointer; cdecl; external libSDL3;
function SDL_calloc(nmemb: Tsize_t; size: Tsize_t): pointer; cdecl; external libSDL3;
function SDL_realloc(mem: pointer; size: Tsize_t): pointer; cdecl; external libSDL3;
procedure SDL_free(mem: pointer); cdecl; external libSDL3;

type
  TSDL_malloc_func = function(size: Tsize_t): pointer; cdecl;
  PSDL_malloc_func = ^TSDL_malloc_func;

  TSDL_calloc_func = function(nmemb: Tsize_t; size: Tsize_t): pointer; cdecl;
  PSDL_calloc_func = ^TSDL_calloc_func;

  TSDL_realloc_func = function(mem: pointer; size: Tsize_t): pointer; cdecl;
  PSDL_realloc_func = ^TSDL_realloc_func;

  TSDL_free_func = procedure(mem: pointer); cdecl;
  PSDL_free_func = ^TSDL_free_func;

  TSDL_Environment = record
  end;
  PSDL_Environment = ^TSDL_Environment;

procedure SDL_GetOriginalMemoryFunctions(malloc_func: PSDL_malloc_func; calloc_func: PSDL_calloc_func; realloc_func: PSDL_realloc_func; free_func: PSDL_free_func); cdecl; external libSDL3;
procedure SDL_GetMemoryFunctions(malloc_func: PSDL_malloc_func; calloc_func: PSDL_calloc_func; realloc_func: PSDL_realloc_func; free_func: PSDL_free_func); cdecl; external libSDL3;
function SDL_SetMemoryFunctions(malloc_func: TSDL_malloc_func; calloc_func: TSDL_calloc_func; realloc_func: TSDL_realloc_func; free_func: TSDL_free_func): Tbool; cdecl; external libSDL3;
function SDL_aligned_alloc(alignment: Tsize_t; size: Tsize_t): pointer; cdecl; external libSDL3;
procedure SDL_aligned_free(mem: pointer); cdecl; external libSDL3;
function SDL_GetNumAllocations: longint; cdecl; external libSDL3;
function SDL_GetEnvironment: PSDL_Environment; cdecl; external libSDL3;
function SDL_CreateEnvironment(populated: Tbool): PSDL_Environment; cdecl; external libSDL3;
function SDL_GetEnvironmentVariable(env: PSDL_Environment; Name: pansichar): pansichar; cdecl; external libSDL3;
function SDL_GetEnvironmentVariables(env: PSDL_Environment): PPChar; cdecl; external libSDL3;
function SDL_SetEnvironmentVariable(env: PSDL_Environment; Name: pansichar; Value: pansichar; overwrite: Tbool): Tbool; cdecl; external libSDL3;
function SDL_UnsetEnvironmentVariable(env: PSDL_Environment; Name: pansichar): Tbool; cdecl; external libSDL3;
procedure SDL_DestroyEnvironment(env: PSDL_Environment); cdecl; external libSDL3;
function SDL_getenv(Name: pansichar): pansichar; cdecl; external libSDL3;
function SDL_getenv_unsafe(Name: pansichar): pansichar; cdecl; external libSDL3;
function SDL_setenv_unsafe(Name: pansichar; Value: pansichar; overwrite: longint): longint; cdecl; external libSDL3;
function SDL_unsetenv_unsafe(Name: pansichar): longint; cdecl; external libSDL3;

type
  TSDL_CompareCallback = function(a: pointer; b: pointer): longint; cdecl;

procedure SDL_qsort(base: pointer; nmemb: Tsize_t; size: Tsize_t; compare: TSDL_CompareCallback); cdecl; external libSDL3;
function SDL_bsearch(key: pointer; base: pointer; nmemb: Tsize_t; size: Tsize_t; compare: TSDL_CompareCallback): pointer; cdecl; external libSDL3;

type
  TSDL_CompareCallback_r = function(userdata: pointer; a: pointer; b: pointer): longint; cdecl;

procedure SDL_qsort_r(base: pointer; nmemb: Tsize_t; size: Tsize_t; compare: TSDL_CompareCallback_r; userdata: pointer); cdecl; external libSDL3;
function SDL_bsearch_r(key: pointer; base: pointer; nmemb: Tsize_t; size: Tsize_t; compare: TSDL_CompareCallback_r;
  userdata: pointer): pointer; cdecl; external libSDL3;
function SDL_abs(x: longint): longint; cdecl; external libSDL3;
function SDL_isalpha(x: longint): longint; cdecl; external libSDL3;
function SDL_isalnum(x: longint): longint; cdecl; external libSDL3;
function SDL_isblank(x: longint): longint; cdecl; external libSDL3;
function SDL_iscntrl(x: longint): longint; cdecl; external libSDL3;
function SDL_isdigit(x: longint): longint; cdecl; external libSDL3;
function SDL_isxdigit(x: longint): longint; cdecl; external libSDL3;
function SDL_ispunct(x: longint): longint; cdecl; external libSDL3;
function SDL_isspace(x: longint): longint; cdecl; external libSDL3;
function SDL_isupper(x: longint): longint; cdecl; external libSDL3;
function SDL_islower(x: longint): longint; cdecl; external libSDL3;
function SDL_isprint(x: longint): longint; cdecl; external libSDL3;
function SDL_isgraph(x: longint): longint; cdecl; external libSDL3;
function SDL_toupper(x: longint): longint; cdecl; external libSDL3;
function SDL_tolower(x: longint): longint; cdecl; external libSDL3;
function SDL_crc16(crc: TUint16; Data: pointer; len: Tsize_t): TUint16; cdecl; external libSDL3;
function SDL_crc32(crc: TUint32; Data: pointer; len: Tsize_t): TUint32; cdecl; external libSDL3;
function SDL_murmur3_32(Data: pointer; len: Tsize_t; seed: TUint32): TUint32; cdecl; external libSDL3;
function SDL_memcpy(dst: pointer; src: pointer; len: Tsize_t): pointer; cdecl; external libSDL3;
function SDL_memmove(dst: pointer; src: pointer; len: Tsize_t): pointer; cdecl; external libSDL3;
function SDL_memset(dst: pointer; c: longint; len: Tsize_t): pointer; cdecl; external libSDL3;
function SDL_memset4(dst: pointer; val: TUint32; dwords: Tsize_t): pointer; cdecl; external libSDL3;
function SDL_memcmp(s1: pointer; s2: pointer; len: Tsize_t): longint; cdecl; external libSDL3;
function SDL_wcslen(wstr: Pwchar_t): Tsize_t; cdecl; external libSDL3;
function SDL_wcsnlen(wstr: Pwchar_t; maxlen: Tsize_t): Tsize_t; cdecl; external libSDL3;
function SDL_wcslcpy(dst: Pwchar_t; src: Pwchar_t; maxlen: Tsize_t): Tsize_t; cdecl; external libSDL3;
function SDL_wcslcat(dst: Pwchar_t; src: Pwchar_t; maxlen: Tsize_t): Tsize_t; cdecl; external libSDL3;
function SDL_wcsdup(wstr: Pwchar_t): Pwchar_t; cdecl; external libSDL3;
function SDL_wcsstr(haystack: Pwchar_t; needle: Pwchar_t): Pwchar_t; cdecl; external libSDL3;
function SDL_wcsnstr(haystack: Pwchar_t; needle: Pwchar_t; maxlen: Tsize_t): Pwchar_t; cdecl; external libSDL3;
function SDL_wcscmp(str1: Pwchar_t; str2: Pwchar_t): longint; cdecl; external libSDL3;
function SDL_wcsncmp(str1: Pwchar_t; str2: Pwchar_t; maxlen: Tsize_t): longint; cdecl; external libSDL3;
function SDL_wcscasecmp(str1: Pwchar_t; str2: Pwchar_t): longint; cdecl; external libSDL3;
function SDL_wcsncasecmp(str1: Pwchar_t; str2: Pwchar_t; maxlen: Tsize_t): longint; cdecl; external libSDL3;
function SDL_wcstol(str: Pwchar_t; endp: PPwchar_t; base: longint): longint; cdecl; external libSDL3;
function SDL_strlen(str: pansichar): Tsize_t; cdecl; external libSDL3;
function SDL_strnlen(str: pansichar; maxlen: Tsize_t): Tsize_t; cdecl; external libSDL3;
function SDL_strlcpy(dst: pansichar; src: pansichar; maxlen: Tsize_t): Tsize_t; cdecl; external libSDL3;
function SDL_utf8strlcpy(dst: pansichar; src: pansichar; dst_bytes: Tsize_t): Tsize_t; cdecl; external libSDL3;
function SDL_strlcat(dst: pansichar; src: pansichar; maxlen: Tsize_t): Tsize_t; cdecl; external libSDL3;
function SDL_strdup(str: pansichar): pansichar; cdecl; external libSDL3;
function SDL_strndup(str: pansichar; maxlen: Tsize_t): pansichar; cdecl; external libSDL3;
function SDL_strrev(str: pansichar): pansichar; cdecl; external libSDL3;
function SDL_strupr(str: pansichar): pansichar; cdecl; external libSDL3;
function SDL_strlwr(str: pansichar): pansichar; cdecl; external libSDL3;
function SDL_strchr(str: pansichar; c: longint): pansichar; cdecl; external libSDL3;
function SDL_strrchr(str: pansichar; c: longint): pansichar; cdecl; external libSDL3;
function SDL_strstr(haystack: pansichar; needle: pansichar): pansichar; cdecl; external libSDL3;
function SDL_strnstr(haystack: pansichar; needle: pansichar; maxlen: Tsize_t): pansichar; cdecl; external libSDL3;
function SDL_strcasestr(haystack: pansichar; needle: pansichar): pansichar; cdecl; external libSDL3;
function SDL_strtok_r(s1: pansichar; s2: pansichar; saveptr: PPansichar): pansichar; cdecl; external libSDL3;
function SDL_utf8strlen(str: pansichar): Tsize_t; cdecl; external libSDL3;
function SDL_utf8strnlen(str: pansichar; bytes: Tsize_t): Tsize_t; cdecl; external libSDL3;
function SDL_itoa(Value: longint; str: pansichar; radix: longint): pansichar; cdecl; external libSDL3;
function SDL_uitoa(Value: dword; str: pansichar; radix: longint): pansichar; cdecl; external libSDL3;
function SDL_ltoa(Value: longint; str: pansichar; radix: longint): pansichar; cdecl; external libSDL3;
function SDL_ultoa(Value: dword; str: pansichar; radix: longint): pansichar; cdecl; external libSDL3;
function SDL_lltoa(Value: int64; str: pansichar; radix: longint): pansichar; cdecl; external libSDL3;
function SDL_ulltoa(Value: qword; str: pansichar; radix: longint): pansichar; cdecl; external libSDL3;
function SDL_atoi(str: pansichar): longint; cdecl; external libSDL3;
function SDL_atof(str: pansichar): Tdouble; cdecl; external libSDL3;
function SDL_strtol(str: pansichar; endp: PPansichar; base: longint): longint; cdecl; external libSDL3;
function SDL_strtoul(str: pansichar; endp: PPansichar; base: longint): dword; cdecl; external libSDL3;
function SDL_strtoll(str: pansichar; endp: PPansichar; base: longint): int64; cdecl; external libSDL3;
function SDL_strtoull(str: pansichar; endp: PPansichar; base: longint): qword; cdecl; external libSDL3;
function SDL_strtod(str: pansichar; endp: PPansichar): Tdouble; cdecl; external libSDL3;
function SDL_strcmp(str1: pansichar; str2: pansichar): longint; cdecl; external libSDL3;
function SDL_strncmp(str1: pansichar; str2: pansichar; maxlen: Tsize_t): longint; cdecl; external libSDL3;
function SDL_strcasecmp(str1: pansichar; str2: pansichar): longint; cdecl; external libSDL3;
function SDL_strncasecmp(str1: pansichar; str2: pansichar; maxlen: Tsize_t): longint; cdecl; external libSDL3;
function SDL_strpbrk(str: pansichar; breakset: pansichar): pansichar; cdecl; external libSDL3;

const
  SDL_INVALID_UNICODE_CODEPOINT = $FFFD;

function SDL_StepUTF8(pstr: PPansichar; pslen: Psize_t): TUint32; cdecl; external libSDL3;
function SDL_UCS4ToUTF8(codepoint: TUint32; dst: pansichar): pansichar; cdecl; external libSDL3;
function SDL_sscanf(Text: pansichar; fmt: pansichar; args: array of const): longint; cdecl; external libSDL3;
function SDL_sscanf(Text: pansichar; fmt: pansichar): longint; cdecl; external libSDL3;
function SDL_vsscanf(Text: pansichar; fmt: pansichar; ap: Tva_list): longint; cdecl; external libSDL3;
function SDL_snprintf(Text: pansichar; maxlen: Tsize_t; fmt: pansichar; args: array of const): longint; cdecl; external libSDL3;
function SDL_snprintf(Text: pansichar; maxlen: Tsize_t; fmt: pansichar): longint; cdecl; external libSDL3;
function SDL_swprintf(Text: Pwchar_t; maxlen: Tsize_t; fmt: Pwchar_t; args: array of const): longint; cdecl; external libSDL3;
function SDL_swprintf(Text: Pwchar_t; maxlen: Tsize_t; fmt: Pwchar_t): longint; cdecl; external libSDL3;
function SDL_vsnprintf(Text: pansichar; maxlen: Tsize_t; fmt: pansichar; ap: Tva_list): longint; cdecl; external libSDL3;
function SDL_vswprintf(Text: Pwchar_t; maxlen: Tsize_t; fmt: Pwchar_t; ap: Tva_list): longint; cdecl; external libSDL3;
function SDL_asprintf(strp: PPansichar; fmt: pansichar; args: array of const): longint; cdecl; external libSDL3;
function SDL_asprintf(strp: PPansichar; fmt: pansichar): longint; cdecl; external libSDL3;
function SDL_vasprintf(strp: PPansichar; fmt: pansichar; ap: Tva_list): longint; cdecl; external libSDL3;
procedure SDL_srand(seed: TUint64); cdecl; external libSDL3;
function SDL_rand(n: TSint32): TSint32; cdecl; external libSDL3;
function SDL_randf: single; cdecl; external libSDL3;
function SDL_rand_bits: TUint32; cdecl; external libSDL3;
function SDL_rand_r(state: PUint64; n: TSint32): TSint32; cdecl; external libSDL3;
function SDL_randf_r(state: PUint64): single; cdecl; external libSDL3;
function SDL_rand_bits_r(state: PUint64): TUint32; cdecl; external libSDL3;
function SDL_acos(x: Tdouble): Tdouble; cdecl; external libSDL3;
function SDL_acosf(x: single): single; cdecl; external libSDL3;
function SDL_asin(x: Tdouble): Tdouble; cdecl; external libSDL3;
function SDL_asinf(x: single): single; cdecl; external libSDL3;
function SDL_atan(x: Tdouble): Tdouble; cdecl; external libSDL3;
function SDL_atanf(x: single): single; cdecl; external libSDL3;
function SDL_atan2(y: Tdouble; x: Tdouble): Tdouble; cdecl; external libSDL3;
function SDL_atan2f(y: single; x: single): single; cdecl; external libSDL3;
function SDL_ceil(x: Tdouble): Tdouble; cdecl; external libSDL3;
function SDL_ceilf(x: single): single; cdecl; external libSDL3;
function SDL_copysign(x: Tdouble; y: Tdouble): Tdouble; cdecl; external libSDL3;
function SDL_copysignf(x: single; y: single): single; cdecl; external libSDL3;
function SDL_cos(x: Tdouble): Tdouble; cdecl; external libSDL3;
function SDL_cosf(x: single): single; cdecl; external libSDL3;
function SDL_exp(x: Tdouble): Tdouble; cdecl; external libSDL3;
function SDL_expf(x: single): single; cdecl; external libSDL3;
function SDL_fabs(x: Tdouble): Tdouble; cdecl; external libSDL3;
function SDL_fabsf(x: single): single; cdecl; external libSDL3;
function SDL_floor(x: Tdouble): Tdouble; cdecl; external libSDL3;
function SDL_floorf(x: single): single; cdecl; external libSDL3;
function SDL_trunc(x: Tdouble): Tdouble; cdecl; external libSDL3;
function SDL_truncf(x: single): single; cdecl; external libSDL3;
function SDL_fmod(x: Tdouble; y: Tdouble): Tdouble; cdecl; external libSDL3;
function SDL_fmodf(x: single; y: single): single; cdecl; external libSDL3;
function SDL_isinf(x: Tdouble): longint; cdecl; external libSDL3;
function SDL_isinff(x: single): longint; cdecl; external libSDL3;
function SDL_isnan(x: Tdouble): longint; cdecl; external libSDL3;
function SDL_isnanf(x: single): longint; cdecl; external libSDL3;
function SDL_log(x: Tdouble): Tdouble; cdecl; external libSDL3;
function SDL_logf(x: single): single; cdecl; external libSDL3;
function SDL_log10(x: Tdouble): Tdouble; cdecl; external libSDL3;
function SDL_log10f(x: single): single; cdecl; external libSDL3;
function SDL_modf(x: Tdouble; y: Pdouble): Tdouble; cdecl; external libSDL3;
function SDL_modff(x: single; y: Psingle): single; cdecl; external libSDL3;
function SDL_pow(x: Tdouble; y: Tdouble): Tdouble; cdecl; external libSDL3;
function SDL_powf(x: single; y: single): single; cdecl; external libSDL3;
function SDL_round(x: Tdouble): Tdouble; cdecl; external libSDL3;
function SDL_roundf(x: single): single; cdecl; external libSDL3;
function SDL_lround(x: Tdouble): longint; cdecl; external libSDL3;
function SDL_lroundf(x: single): longint; cdecl; external libSDL3;
function SDL_scalbn(x: Tdouble; n: longint): Tdouble; cdecl; external libSDL3;
function SDL_scalbnf(x: single; n: longint): single; cdecl; external libSDL3;
function SDL_sin(x: Tdouble): Tdouble; cdecl; external libSDL3;
function SDL_sinf(x: single): single; cdecl; external libSDL3;
function SDL_sqrt(x: Tdouble): Tdouble; cdecl; external libSDL3;
function SDL_sqrtf(x: single): single; cdecl; external libSDL3;
function SDL_tan(x: Tdouble): Tdouble; cdecl; external libSDL3;
function SDL_tanf(x: single): single; cdecl; external libSDL3;

type
  PSDL_iconv_t = ^TSDL_iconv_t;
  TSDL_iconv_t = PSDL_iconv_data_t;

function SDL_iconv_open(tocode: pansichar; fromcode: pansichar): TSDL_iconv_t; cdecl; external libSDL3;
function SDL_iconv_close(cd: TSDL_iconv_t): longint; cdecl; external libSDL3;
function SDL_iconv(cd: TSDL_iconv_t; inbuf: PPansichar; inbytesleft: Psize_t; outbuf: PPansichar; outbytesleft: Psize_t): Tsize_t; cdecl; external libSDL3;
function SDL_iconv_string(tocode: pansichar; fromcode: pansichar; inbuf: pansichar; inbytesleft: Tsize_t): pansichar; cdecl; external libSDL3;

function SDL_FOURCC(A, B, C, D: byte): uint32;

function SDL_min(x, y: longint): longint;
function SDL_min(x, y: single): single;
function SDL_max(x, y: longint): longint;
function SDL_max(x, y: single): single;
function SDL_clamp(x, a, b: longint): longint;
function SDL_clamp(x, a, b: single): single;

function SDL_iconv_utf8_locale(S: pchar): pchar;
function SDL_iconv_utf8_ucs2(S: pchar): PUint16;
function SDL_iconv_utf8_ucs4(S: pchar): PUint32;
function SDL_iconv_wchar_utf8(S: Pwchar_t): pchar;

function SDL_ICONV_ERROR: Tsize_t;
function SDL_ICONV_E2BIG: Tsize_t;
function SDL_ICONV_EILSEQ: Tsize_t;
function SDL_ICONV_EINVAL: Tsize_t;

type
  PSDL_FunctionPointer = ^TSDL_FunctionPointer;
  TSDL_FunctionPointer = pointer;

implementation

function SDL_FOURCC(A, B, C, D: byte): uint32;
begin
  Result := A shl 0 + B shl 8 + C shl 16 + D shl 24;
end;

function SDL_min(x, y: longint): longint; inline;
begin
  if x < y then begin
    Result := x;
  end else begin
    Result := y;
  end;
end;

function SDL_min(x, y: single): single; inline;
begin
  if x < y then begin
    Result := x;
  end else begin
    Result := y;
  end;
end;

function SDL_max(x, y: longint): longint; inline;
begin
  if x > y then begin
    Result := x;
  end else begin
    Result := y;
  end;
end;

function SDL_max(x, y: single): single; inline;
begin
  if x > y then begin
    Result := x;
  end else begin
    Result := y;
  end;
end;

function SDL_clamp(x, a, b: longint): longint; inline;
var
  if_local1: longint;
begin
  if x > b then begin
    if_local1 := b;
  end else begin
    if_local1 := x;
  end;
  if x < a then begin
    Result := a;
  end else begin
    Result := if_local1;
  end;
end;

function SDL_clamp(x, a, b: single): single; inline;
var
  if_local1: single;
begin
  if x > b then begin
    if_local1 := b;
  end else begin
    if_local1 := x;
  end;
  if x < a then begin
    Result := a;
  end else begin
    Result := if_local1;
  end;
end;


function SDL_ICONV_ERROR: Tsize_t;
begin
  SDL_ICONV_ERROR := Tsize_t(-(1));
end;

function SDL_ICONV_E2BIG: Tsize_t;
begin
  SDL_ICONV_E2BIG := Tsize_t(-(2));
end;

function SDL_ICONV_EILSEQ: Tsize_t;
begin
  SDL_ICONV_EILSEQ := Tsize_t(-(3));
end;

function SDL_ICONV_EINVAL: Tsize_t;
begin
  SDL_ICONV_EINVAL := Tsize_t(-(4));
end;

function SDL_iconv_utf8_locale(S: pchar): pchar;
begin
  SDL_iconv_utf8_locale := SDL_iconv_string('', 'UTF-8', S, (SDL_strlen(S)) + 1);
end;

function SDL_iconv_utf8_ucs2(S: pchar): PUint16;
begin
  SDL_iconv_utf8_ucs2 := PUint16(SDL_iconv_string('UCS-2', 'UTF-8', S, (SDL_strlen(S)) + 1));
end;

function SDL_iconv_utf8_ucs4(S: pchar): PUint32;
begin
  SDL_iconv_utf8_ucs4 := PUint32(SDL_iconv_string('UCS-4', 'UTF-8', S, (SDL_strlen(S)) + 1));
end;

function SDL_iconv_wchar_utf8(S: Pwchar_t): pchar;
begin
  SDL_iconv_wchar_utf8 := SDL_iconv_string('UTF-8', 'WCHAR_T', PChar(S), ((SDL_wcslen(S)) + 1) * (sizeof(Twchar_t)));
end;

end.
