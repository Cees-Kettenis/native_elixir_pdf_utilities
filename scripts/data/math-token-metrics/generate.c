/* Build-time FreeType sampler. No native code is linked into the library. */
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_OUTLINE_H

/* Dataset coverage in 26.6 CSS pixels, not a renderer resource limit. */
#define MIN_SIZE 64
#define MAX_SIZE 8192

static void write_word(FILE *output, int value) {
    fputc((value >> 8) & 255, output);
    fputc(value & 255, output);
}

int main(int argc, char **argv) {
    if (argc != 3) {
        fprintf(stderr, "Usage: generate FONT OUTPUT\n");
        return 1;
    }

    FT_Library library;
    FT_Face face;
    if (FT_Init_FreeType(&library) || FT_New_Face(library, argv[1], 0, &face)) {
        return 1;
    }
    FT_Int major, minor, patch;
    FT_Library_Version(library, &major, &minor, &patch);
    fprintf(stderr, "%d.%d.%d\n", major, minor, patch);

    FILE *output = fopen(argv[2], "wb");
    if (!output || face->num_glyphs > UINT16_MAX) {
        return 1;
    }
    write_word(output, face->num_glyphs);
    write_word(output, MAX_SIZE);

    for (int glyph = 0; glyph < face->num_glyphs; glyph++) {
        int count = 0, previous_bottom = 0, previous_top = 0;
        int16_t records[MAX_SIZE][3];
        for (int size = MIN_SIZE; size <= MAX_SIZE; size++) {
            if (FT_Set_Char_Size(face, size, size, 72, 72) ||
                FT_Load_Glyph(face, glyph, FT_LOAD_TARGET_LIGHT | FT_LOAD_NO_BITMAP)) {
                return 2;
            }
            if (face->glyph->format != FT_GLYPH_FORMAT_OUTLINE) {
                return 2;
            }
            FT_BBox bounds = {0};
            FT_Outline_Get_CBox(&face->glyph->outline, &bounds);
            int bottom = floor(bounds.yMin / 64.0);
            int top = ceil(bounds.yMax / 64.0);
            if (bottom < INT16_MIN || top > INT16_MAX) {
                return 2;
            }
            if (size == MIN_SIZE || bottom != previous_bottom || top != previous_top) {
                records[count][0] = size;
                records[count][1] = bottom;
                records[count++][2] = top;
                previous_bottom = bottom;
                previous_top = top;
            }
        }
        write_word(output, count);
        for (int index = 0; index < count; index++) {
            for (int field = 0; field < 3; field++) {
                write_word(output, records[index][field]);
            }
        }
        if (glyph % 1000 == 0) {
            fprintf(stderr, "%d/%ld\n", glyph, face->num_glyphs);
        }
    }

    int error = ferror(output);
    error |= fclose(output) != 0;
    FT_Done_Face(face);
    FT_Done_FreeType(library);
    return error ? 2 : 0;
}
