unit FillCol;

interface

uses
  SDL3;

procedure fillColor(renderer: PSDL_Renderer; texture: PSDL_Texture; color: TSDL_Color);


implementation

procedure fillColor(renderer: PSDL_Renderer; texture: PSDL_Texture; color: TSDL_Color);
begin
  SDL_SetRenderTarget(renderer, texture);
  //  SDL_SetRenderDrawColor(renderer,color.r,color.g,color.b,SDL_ALPHA_OPAQUE);
  SDL_SetRenderDrawColor(renderer, color.r, color.g, color.b, $FF);
  SDL_RenderClear(renderer);
  SDL_SetRenderTarget(renderer, nil);
end;

end.
