kernel void invert(image2d_t img) {
  size_t x = get_global_id(0);
  size_t y = get_global_id(1);

  uint4 point = read_imageui(img, (int2)(x,y));
  point = (uint4)255 - point;
  //write_imageui(img, (int2)(x,y), point);
}
