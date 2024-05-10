#include <freetype2/ft2build.h>
//#include FT_FREETYPE_H

#include <freetype/freetype.h>
//#include <freetype2/freetype/fttypes.h>
//#include <freetype2/freetype/ftsystem.h>
//#include <freetype2/freetype/config/integer-types.h>

// https://freetype.org/freetype2/docs/tutorial/step1.html


typedef struct  Test_
  {
    FT_Long           num_faces;

  } Test;



int main()
{
printf(" Start\n");

FT_Library  library;   /* handle to library     */
FT_Face     face;      /* handle to face object */

FT_Error error;


error = FT_Init_FreeType( &library );
if ( error ) { printf("Init error\n"); }


printf("addr face: %i\n", &face);
error = FT_New_Face( library, "/usr/share/wine/fonts/courier.ttf", 0, &face );
printf("addr face: %i\n", &face);
if ( error == FT_Err_Unknown_File_Format )
{
  printf("... the font file could be opened and read, but it appears  ... that its font format is unsupported\n");
}
else if ( error )
{
printf("Face error %i\n", error);
}
else printf("io.  %i\n",error);

printf("Ende\n");
}
