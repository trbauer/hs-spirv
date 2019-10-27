
constant short constant_ss[4] =
  {-1,-2,-3,-4};

kernel void test_short(
  global short *dst,
  const global short *src,
  local short *dtile,
  short arg)
{
  local short tile[16];
  short pvt[4] = {-1,-2,-3,-4};
  int id = get_global_id(0);
  int lid = get_local_id(0);
  //
  if (lid < sizeof(tile)/sizeof(tile[0]) {
    tile[lid] = src[id];
  }
  dtile[lid] = src[id+1];
  barrier(CLK_GLOBAL_MEM_FENCE);
  //
  short val = -10;
  val += pvt[id%(sizeof(pvt)/sizeof(pvt[0]))];
  val += constant_ss[id%4];
  val += tile[(lid+1) % (sizeof(tile)/sizeof(tile[0])];
  val += dtile[(lid+1) % get_local_size(0)];
  val += arg;
  //
  dst[id] = val;
}

