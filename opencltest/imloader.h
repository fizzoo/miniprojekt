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
  unsigned char *_data;

public:
  Image(std::string filename);
  ~Image();

  unsigned char *data() { return _data; }
  void writepng(std::string filename, char *buffer);

  size_t height();
  size_t width();

  operator bool() { return ok; }
};
}

#endif /* end of include guard: IMLOADER_H */
