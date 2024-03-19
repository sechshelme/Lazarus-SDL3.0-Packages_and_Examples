/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_BlendMode = ^TSDL_BlendMode;
  TSDL_BlendMode = longint;

const
  SDL_BLENDMODE_NONE = $00000000;
  SDL_BLENDMODE_BLEND = $00000001;
  SDL_BLENDMODE_ADD = $00000002;
  SDL_BLENDMODE_MOD = $00000004;
  SDL_BLENDMODE_MUL = $00000008;
  SDL_BLENDMODE_INVALID = $7FFFFFFF;

type
  PSDL_BlendOperation = ^TSDL_BlendOperation;
  TSDL_BlendOperation = longint;

const
  SDL_BLENDOPERATION_ADD = $1;
  SDL_BLENDOPERATION_SUBTRACT = $2;
  SDL_BLENDOPERATION_REV_SUBTRACT = $3;
  SDL_BLENDOPERATION_MINIMUM = $4;
  SDL_BLENDOPERATION_MAXIMUM = $5;

type
  PSDL_BlendFactor = ^TSDL_BlendFactor;
  TSDL_BlendFactor = longint;

const
  SDL_BLENDFACTOR_ZERO = $1;
  SDL_BLENDFACTOR_ONE = $2;
  SDL_BLENDFACTOR_SRC_COLOR = $3;
  SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR = $4;
  SDL_BLENDFACTOR_SRC_ALPHA = $5;
  SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA = $6;
  SDL_BLENDFACTOR_DST_COLOR = $7;
  SDL_BLENDFACTOR_ONE_MINUS_DST_COLOR = $8;
  SDL_BLENDFACTOR_DST_ALPHA = $9;
  SDL_BLENDFACTOR_ONE_MINUS_DST_ALPHA = $A;

function SDL_ComposeCustomBlendMode(srcColorFactor: TSDL_BlendFactor; dstColorFactor: TSDL_BlendFactor; colorOperation: TSDL_BlendOperation;
srcAlphaFactor: TSDL_BlendFactor; dstAlphaFactor: TSDL_BlendFactor; alphaOperation: TSDL_BlendOperation): TSDL_BlendMode; cdecl; external;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}
