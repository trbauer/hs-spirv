kernel void test_long(
  global volatile long *dst)
{
  long pvt[4] = {-1,-2,-3,-4};
  int id = get_global_id(0);
  long val = -2;
  val += pvt[id%(sizeof(pvt)/sizeof(pvt[0]))];
  //
  dst[id] = val;
}

