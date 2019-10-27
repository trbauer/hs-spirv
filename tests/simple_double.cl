
constant int4 constant_i4s[2] =
  {(int4)(-1,-2,-3,-4),(int4)(-5,-6,-7,-8)};

kernel void test_int4(
  global int4 *dst)
{
  const int4 pvt[2] = {(int4)(0,1,2,3),(int4)(4,5,6,7)};
  int id = get_global_id(0);
  int4 val = (int4)(8,9,10,11);
  //
  val += pvt[id%(sizeof(pvt)/sizeof(pvt[0]))];
  val += constant_i4s[id%2];
  //
  dst[id] = val;
}


kernel void test_long(
  global long *dst)
{
  long pvt[4] = {-1,-2,-3,-4};
  int id = get_global_id(0);
  long val = -2;
  val += pvt[id%(sizeof(pvt)/sizeof(pvt[0]))];
  //
  dst[id] = val;
}

constant double constant_ds[4] =
  {1.0, 2.0, 3.0, 4.0};

kernel void test_double(
  global double *dst)
{
  double pvt[4] = {-1.0,-2.0,-3.0,-4.0};
  int id = get_global_id(0);
  //
  double val = 3.14159;
  val += pvt[id%(sizeof(pvt)/sizeof(pvt[0]))];
  val += constant_ds[id%4];
  //
  dst[id] = val;
}


constant short constant_ss[4] =
  {-1,-2,-3,-4};

kernel void test_short(
  global short *dst)
{
  short pvt[4] = {-1,-2,-3,-4};
  int id = get_global_id(0);
  //
  short val = -10;
  val += pvt[id%(sizeof(pvt)/sizeof(pvt[0]))];
  val += constant_ss[id%4];
  //
  dst[id] = val;
}

