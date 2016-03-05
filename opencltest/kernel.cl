kernel void invert(read_only image2d_t input, write_only image2d_t output) {
  size_t x = get_global_id(0);
  size_t y = get_global_id(1);

  uint4 point = read_imageui(input, (int2)(x, y));
  point = (uint4)255 - point;
  write_imageui(output, (int2)(x, y), point);
}
