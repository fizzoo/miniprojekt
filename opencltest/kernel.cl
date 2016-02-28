constant char hw[] = "HellooWorld\n";
kernel void hello(__global char * out) {
    size_t tid = get_global_id(0);
    out[tid] = hw[tid];
    if (tid == 5) {
        out[tid] = ' ';
    }
}
