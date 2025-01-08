
{$IFDEF read_interface}

const
  SDL_LSX_INTRINSICS = 1;
  SDL_LASX_INTRINSICS = 1;
  SDL_NEON_INTRINSICS = 1;
  SDL_ALTIVEC_INTRINSICS = 1;
  SDL_MMX_INTRINSICS = 1;
  SDL_SSE_INTRINSICS = 1;
  SDL_SSE2_INTRINSICS = 1;
  SDL_SSE3_INTRINSICS = 1;
  SDL_SSE4_1_INTRINSICS = 1;
  SDL_SSE4_2_INTRINSICS = 1;
  SDL_AVX_INTRINSICS = 1;
  SDL_AVX2_INTRINSICS = 1;
  SDL_AVX512F_INTRINSICS = 1;

const
  __ARM_NEON = 1;
  __ARM_ARCH = 8;

function SDL_TARGETING(x : longint) : longint;

{$ENDIF read_interface}


{$IFDEF read_implementation}

function SDL_TARGETING(x : longint) : longint;
begin
//  SDL_TARGETING:=__attribute__(target(x)); ??????????
end;


{$ENDIF read_implementation}
