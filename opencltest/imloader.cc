#include "imloader.h"

using namespace iml;

#define FAIL(a)                                                                \
  {                                                                            \
    std::cerr << a << std::endl;                                               \
    ok = 0;                                                                    \
    return;                                                                    \
  }

Image::Image(std::string filename) {
  // Assuming nothing is broken before we even begin.
  ok = 1;

  // Open the file. libpng expects C file handle.
  FILE *fp = fopen(filename.c_str(), "rb");
  if (!fp)
    FAIL("Couldn't open image file.");

  // Test for png.
  unsigned char header[8];
  fread(header, 1, 8, fp);
  bool is_png = !png_sig_cmp(header, 0, 8);
  if (!is_png)
    FAIL("Not a png");

  pngp =
      png_create_read_struct(PNG_LIBPNG_VER_STRING, nullptr, nullptr, nullptr);
  if (!pngp)
    FAIL("Couldn't create png struct");

  png_set_sig_bytes(pngp, 8);

  pngi = png_create_info_struct(pngp);
  if (!pngi)
    FAIL("Couldn't create info struct");

  png_init_io(pngp, fp);
  png_read_info(pngp, pngi);

  auto image_height = height();
  if (image_height <= 0) {
    FAIL("Found no image data, zero height");
  }

  int rowbytes = png_get_rowbytes(pngp, pngi);
  unsigned char **row_pointers = new unsigned char *[image_height];
  if (!row_pointers) {
    delete row_pointers;
    FAIL("Couldn't allocate rowp");
  }

  data = new unsigned char[rowbytes * image_height];
  if (!data) {
    delete row_pointers;
    FAIL("Couldn't allocate image data buffer.");
  }

  for (unsigned int i = 0; i < image_height; ++i) {
    row_pointers[i] = data + i * rowbytes;
  }

  // Try to expand to rgba.
  int bit_depth = png_get_bit_depth(pngp, pngi);
  int color_type = png_get_color_type(pngp, pngi);
  if (color_type == PNG_COLOR_TYPE_PALETTE)
    png_set_palette_to_rgb(pngp);
  if (color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8)
    png_set_expand_gray_1_2_4_to_8(pngp);
  if (png_get_valid(pngp, pngi, PNG_INFO_tRNS))
    png_set_tRNS_to_alpha(pngp);
  if (color_type == PNG_COLOR_TYPE_RGB)
    png_set_filler(pngp, 255, PNG_FILLER_BEFORE);

  // Loads the data into row_pointers, hence actually into _data
  png_read_image(pngp, row_pointers);

  delete row_pointers;
  fclose(fp);
}

void Image::writepng(std::string filename) {}

size_t Image::height() { return png_get_image_height(pngp, pngi); }

size_t Image::width() { return png_get_image_width(pngp, pngi); }

Image::~Image() {
  png_destroy_read_struct(&pngp, &pngi, nullptr);
  delete data;
}
