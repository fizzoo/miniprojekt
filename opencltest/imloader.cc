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

  pngp =
      png_create_read_struct(PNG_LIBPNG_VER_STRING, nullptr, nullptr, nullptr);
  if (!pngp)
    FAIL("Couldn't create png struct");

  pngi = png_create_info_struct(pngp);
  if (!pngi)
    FAIL("Couldn't create info struct");

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

  _data = new unsigned char[rowbytes * image_height];
  if (!_data) {
    delete row_pointers;
    FAIL("Couldn't allocate image data buffer.");
  }

  for (unsigned int i = 0; i < image_height; ++i) {
    row_pointers[i] = _data + i * rowbytes;
  }

  // Loads the data into row_pointers, hence actually into _data
  png_read_image(pngp, row_pointers);

  delete row_pointers;
}

size_t Image::height() { return png_get_image_height(pngp, pngi); }

size_t Image::width() { return png_get_image_width(pngp, pngi); }

Image::~Image() {
  png_destroy_read_struct(&pngp, &pngi, nullptr);
  delete _data;
}
