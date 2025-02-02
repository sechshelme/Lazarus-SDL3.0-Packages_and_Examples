unit SDL_textengine;

interface

uses
  ctypes, SDL3;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


type
  PTTF_DrawCommand = ^TTTF_DrawCommand;
  TTTF_DrawCommand =  Longint;
  Const
    TTF_DRAW_COMMAND_NOOP = 0;
    TTF_DRAW_COMMAND_FILL = 1;
    TTF_DRAW_COMMAND_COPY = 2;
type
  PTTF_FillOperation = ^TTTF_FillOperation;
  TTTF_FillOperation = record
      cmd : TTTF_DrawCommand;
      rect : TSDL_Rect;
    end;

  PTTF_CopyOperation = ^TTTF_CopyOperation;
  TTTF_CopyOperation = record
      cmd : TTTF_DrawCommand;
      text_offset : longint;
      glyph_font : PTTF_Font;
      glyph_index : TUint32;
      src : TSDL_Rect;
      dst : TSDL_Rect;
      reserved : pointer;
    end;

  PTTF_DrawOperation = ^TTTF_DrawOperation;
  TTTF_DrawOperation = record
      case longint of
        0 : ( cmd : TTTF_DrawCommand );
        1 : ( fill : TTTF_FillOperation );
        2 : ( copy : TTTF_CopyOperation );
      end;

  PTTF_TextData = ^TTTF_TextData;
  TTTF_TextData = record
      font : PTTF_Font;
      color : TSDL_FColor;
      needs_layout_update : Tbool;
      layout : PTTF_TextLayout;
      x : longint;
      y : longint;
      w : longint;
      h : longint;
      num_ops : longint;
      ops : PTTF_DrawOperation;
      num_clusters : longint;
      clusters : PTTF_SubString;
      props : TSDL_PropertiesID;
      needs_engine_update : Tbool;
      engine : PTTF_TextEngine;
      engine_text : pointer;
    end;

  PTTF_TextEngine = ^TTTF_TextEngine;
  TTTF_TextEngine = record
      version : TUint32;
      userdata : pointer;
      CreateText : function (userdata:pointer; text:PTTF_Text):Tbool;cdecl;
      DestroyText : procedure (userdata:pointer; text:PTTF_Text);cdecl;
    end;


implementation


end.
