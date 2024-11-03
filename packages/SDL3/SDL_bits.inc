/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 03-11-2024   13:37                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

//function _SDL_bsr_watcom(para1:TUint32):longint;cdecl;external libSDL3;

function SDL_MostSignificantBitIndex32(x: UInt32): cint;
function SDL_HasExactlyOneBitSet32(x: UInt32): Boolean;

{$ENDIF read_interface}


{$IFDEF read_implementation}

function SDL_MostSignificantBitIndex32(x: uint32): cint;
const
  b: array[0..4] of uint32 = (2, $C, $F0, $FF00, $FFFF0000);
  S: array[0..4] of cint = (1, 2, 4, 8, 16);
var
  msbIndex: integer = 0;
  i: integer;
begin
  if x = 0 then  begin
    Exit(-1);
  end;

  for i := 4 downto 0 do begin
    if (x and b[i]) <> 0 then begin
      x := x shr S[i];
      msbIndex := msbIndex or S[i];
    end;
  end;

  Result := msbIndex;
end;

function SDL_HasExactlyOneBitSet32(x: uint32): boolean;
begin
  if (x <> 0) and ((x and (x - 1)) = 0) then begin
    Result := True;
  end else begin
    Result := False;
  end;
end;


{$ENDIF read_implementation}
