
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
