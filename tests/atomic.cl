kernel void chain_atomics(
  global volatile int *dst)
{
  int id = get_global_id(0);
  //
  int val = id;
  val += atomic_add(dst + (val % get_global_size(0)), id);
  val += atomic_add(dst + (val % get_global_size(0)), id);
  val += atomic_min(dst + (val % get_global_size(0)), id);
  atomic_add(dst + (val % get_global_size(0)), id);
}


