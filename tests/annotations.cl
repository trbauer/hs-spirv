__attribute__((reqd_work_group_size(16,8,2)))
kernel void test_wgsize_reqd(
  global int *dst)
{
  dst[get_global_id(0)] = 0;
}

__attribute__((work_group_size_hint(8,4,2)))
kernel void test_wgsize_hint(
  global int *dst)
{
  dst[get_global_id(0)] = 0;
}

__attribute__((vec_type_hint(int4)))
kernel void test_vectype_hint(
  global int *dst)
{
  dst[get_global_id(0)] = 0;
}

