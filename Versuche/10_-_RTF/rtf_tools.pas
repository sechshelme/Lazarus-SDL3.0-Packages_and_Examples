unit RTF_Tools;

interface

uses
  ctypes,
  SDL3,
  SDL_ttf,
  SDL_rtf;

function GetRTF_ctx(renderer: PSDL_Renderer): PRTF_Context;

implementation

function UTF8_to_UNICODE(_utf8: PChar; advance: pcint): TUint32;
var
  ch: TUint32;
  i: cint = 0;
  utf8: pbyte;
begin
  utf8 := pbyte(_utf8);

  ch := TUint32(utf8[i]);
  if ch >= $F0 then begin
    ch := (utf8[i] and $07) shl 18;
    Inc(i);
    ch := ch or (utf8[i] and $3F) shl 12;
    Inc(i);
    ch := ch or (utf8[i] and $3F) shl 6;
    Inc(i);
    ch := ch or (utf8[i] and $3F);
  end else if ch >= $E0 then begin
    ch := (utf8[i] and $3F) shl 12;
    Inc(i);
    ch := ch or (utf8[i] and $3F) shl 6;
    Inc(i);
    ch := ch or (utf8[i] and $3F);
  end else if ch >= $C0 then begin
    ch := ch or (utf8[i] and $3F) shl 6;
    Inc(i);
    ch := ch or (utf8[i] and $3F);
  end;

  advance^ := i + 1;
  Result := ch;
end;

function CreateFont(Name: PChar; family: TRTF_FontFamily; charset: longint; size: longint; style: longint): pointer; cdecl;
var
  font: PTTF_Font;
  TTFstyle: integer;
begin
  font := TTF_OpenFont('lazy.ttf', size);
  if font = nil then begin
    SDL_Log('Kann kein Font laden !    %s', TTF_GetError);
  end else begin
    TTF_SetFontStyle(font, style);
  end;

  WriteLn('CreateFont io.');
  Result := font;
end;

function GetLineSpacing(font: pointer): longint; cdecl;
begin
  Result := TTF_FontLineSkip(font);
  WriteLn('GetLineSpacing io.');
end;

function GetCharacterOffsets(font: pointer; Text: PChar; byteOffsets: Plongint; pixelOffsets: Plongint; maxOffsets: longint): longint; cdecl;
var
  i: cint = 0;
  bytes: cint = 0;
  pixels: cint = 0;
  advance: cint;
  ch: TUint16;
begin

  while (Text^ <> #0) and (i < maxOffsets) do begin
    byteOffsets[i] := bytes;
    pixelOffsets[i] := pixels;
    Inc(i);

    ch := UTF8_to_UNICODE(Text, @advance);
    Inc(Text, advance);
    Inc(bytes, advance);

    TTF_GlyphMetrics(font, ch, nil, nil, nil, nil, @advance);
    Inc(pixels, advance);
  end;

  if i < maxOffsets then begin
    byteOffsets[i] := bytes;
    pixelOffsets[i] := pixels;
  end;

  SDL_Log('offset: %i', i);
  SDL_Log('max offset: %i', maxOffsets);
  Result := i;
end;

function RenderText(font: pointer; renderer: PSDL_Renderer; Text: PChar; fg: TSDL_Color): PSDL_Texture; cdecl;
var
  surface: PSDL_Surface;
begin
  Result := nil;
  surface := TTF_RenderUTF8_Blended(font, Text, fg);
  if surface = nil then begin
    SDL_Log('Suface Fehler');
  end else begin
    Result := SDL_CreateTextureFromSurface(renderer, surface);
    SDL_DestroySurface(surface);
  end;

  WriteLn('Rendertext io.');
end;

procedure FreeFont(font: pointer); cdecl;
begin
  TTF_CloseFont(font);
  WriteLn('FreeFont');
end;

function GetRTF_ctx(renderer: PSDL_Renderer): PRTF_Context;
var
  fontEngine: TRTF_FontEngine;
begin
  fontEngine.version := RTF_FONT_ENGINE_VERSION;
  fontEngine.CreateFont := @CreateFont;
  fontEngine.GetLineSpacing := @GetLineSpacing;
  fontEngine.GetCharacterOffsets := @GetCharacterOffsets;
  fontEngine.RenderText := @RenderText;
  fontEngine.FreeFont := @FreeFont;

  Result := RTF_CreateContext(renderer, @fontEngine);
  if Result = nil then begin
    SDL_Log('Kann kein rtf erzeugen !    %s', RTF_GetError);
  end;
end;

end.
