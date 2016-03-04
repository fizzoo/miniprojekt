kernel void invert(__global image2d_t *img) {
  size_t xid = get_global_id(0);
  size_t yid = get_global_id(1);
  int2 xy = int2(xid, yid);

  img[yid*2 + xid] = 255 - img[yid*2 + xid];
}
