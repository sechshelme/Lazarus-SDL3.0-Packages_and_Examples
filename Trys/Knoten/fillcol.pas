unit FillCol;

interface

uses
  SDL3;

procedure fillColor(renderer: PSDL_Renderer; texture: PSDL_Texture; const color: TSDL_Color);


implementation

procedure fillColor(renderer: PSDL_Renderer; texture: PSDL_Texture; const color: TSDL_Color);
begin
  SDL_SetRenderTarget(renderer, texture);
  SDL_SetRenderDrawColor(renderer,color.r,color.g,color.b,color.a);
  SDL_RenderClear(renderer);
  SDL_SetRenderTarget(renderer, nil);
end;

end.
