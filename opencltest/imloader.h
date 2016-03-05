#ifndef IMLOADER_H
#define IMLOADER_H

#include <png.h>
#include <string>
#include <iostream>

namespace iml {

class Image {
private:
  bool ok;
  png_structp pngp;
  png_infop pngi;

public:
  /**
   * Free access. Don't use anything beyond width*height*4 (All images expanded
   * to RGBA 8bit).
   */
  unsigned char *data;

  /**
   * Loads image, allocates data and initializes everything.
   * Check if ok before using by a bool conversion.
   */
  Image(std::string filename);

  /**
   * Deallocates data, pngp and pngi.
   */
  ~Image();

  void writepng(std::string filename);

  size_t height();
  size_t width();

  operator bool() { return ok; }
};
}

#endif /* end of include guard: IMLOADER_H */
