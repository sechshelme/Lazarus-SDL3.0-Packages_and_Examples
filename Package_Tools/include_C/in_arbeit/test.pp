
unit test;
interface

{
  Automatically converted by H2Pas 0.99.16 from test.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    test.h
}


{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

function SDL_BYTESPERPIXEL(X : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_INDEXED(format : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_PACKED(format : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_ARRAY(format : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_ALPHA(format : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_10BIT(format : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_FLOAT(format : longint) : longint;


implementation

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_BYTESPERPIXEL(X : longint) : longint;
var
   if_local1, if_local2 : longint;
(* result types are not known *)
begin
  if (((X=SDL_PIXELFORMAT_YUY2) or (X=SDL_PIXELFORMAT_UYVY)) or (X=SDL_PIXELFORMAT_YVYU)) or (X=SDL_PIXELFORMAT_P010) then
    if_local1:=2
  else
    if_local1:=1;
  if SDL_ISPIXELFORMAT_FOURCC(X) then
    if_local2:=if_local1
  else
    if_local2:=(X shr &) and $FF;
  SDL_BYTESPERPIXEL:=if_local2;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_INDEXED(format : longint) : longint;
begin
  SDL_ISPIXELFORMAT_INDEXED:=( not (SDL_ISPIXELFORMAT_FOURCC(format))) and (@(((((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_INDEX1) or ((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_INDEX2)) or ((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_INDEX4)) or ((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_INDEX8)));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_PACKED(format : longint) : longint;
begin
  SDL_ISPIXELFORMAT_PACKED:=( not (SDL_ISPIXELFORMAT_FOURCC(format))) and (@((((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_PACKED8) or ((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_PACKED16)) or ((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_PACKED32)));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_ARRAY(format : longint) : longint;
begin
  SDL_ISPIXELFORMAT_ARRAY:=( not (SDL_ISPIXELFORMAT_FOURCC(format))) and (@((((((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_ARRAYU8) or ((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_ARRAYU16)) or ((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_ARRAYU32)) or ((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_ARRAYF16)) or ((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_ARRAYF32)));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_ALPHA(format : longint) : longint;
begin
  SDL_ISPIXELFORMAT_ALPHA:=(SDL_ISPIXELFORMAT_PACKED(format)) and (@(((((SDL_PIXELORDER(format))=SDL_PACKEDORDER_ARGB) or ((SDL_PIXELORDER(format))=SDL_PACKEDORDER_RGBA)) or ((SDL_PIXELORDER(format))=SDL_PACKEDORDER_ABGR)) or ((SDL_PIXELORDER(format))=SDL_PACKEDORDER_BGRA)));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_10BIT(format : longint) : longint;
begin
  SDL_ISPIXELFORMAT_10BIT:=( not (SDL_ISPIXELFORMAT_FOURCC(format))) and (@(((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_PACKED32) and (@((SDL_PIXELLAYOUT(format))=SDL_PACKEDLAYOUT_2101010))));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_FLOAT(format : longint) : longint;
begin
  SDL_ISPIXELFORMAT_FLOAT:=( not (SDL_ISPIXELFORMAT_FOURCC(format))) and (@(((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_ARRAYF16) or ((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_ARRAYF32)));
end;


end.
