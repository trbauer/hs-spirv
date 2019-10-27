
struct s1_st{
 int x;
 float y;
};
typedef struct s2_st {int x;} s2_t;
typedef struct {
  int x[2];
  char c;
  s2_t y __attribute__((packed));
} s3_t __attribute__((aligned (32)));

kernel void structs(
  global struct s1_st *s1,
  s2_t                 s2,
  local s3_t          *s3)
{
  int id = get_global_id(0);
  int lid = get_local_id(0);

  s3[lid].x[0] = lid;
  s3[lid].x[1] = lid+1;
  s3[lid].y = s2;
  //
  barrier(CLK_GLOBAL_MEM_FENCE);
  //
  s3_t s3v = s3[(lid+1)%get_local_size(0)];

  s1[id].x = s3v.x[0] + s3v.x[1];
  s1[id].y = (float)s2.x;
}

