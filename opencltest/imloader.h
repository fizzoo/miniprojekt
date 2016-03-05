#ifndef IMLOADER_H
#define IMLOADER_H

#include <png.h>
#include <string>
#include <iostream>

namespace iml {

class Image {
private:
  bool ok;
  size_t _width;
  size_t _height;

  Image(Image const & rhs) = delete;
  Image(Image && rhs) = delete;
  Image& operator=(Image const & rhs) noexcept = delete;
  Image& operator=(Image && rhs) noexcept = delete;

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
  Image(const std::string filename);

  /**
   * Deallocates data.
   */
  ~Image();

  size_t width() { return _width; }
  size_t height() { return _height; }

  operator bool() { return ok; }
};

/**
 * Writes the data to a png.
 * Can't const data due to libpng, but it shouldn't be edited.
 */
bool writepng(const std::string filename, Image *img);
bool writepng(const std::string filename, size_t width, size_t height,
              unsigned char *data);
}

#endif /* end of include guard: IMLOADER_H */
