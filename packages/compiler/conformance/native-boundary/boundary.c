#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

struct Packet { uint8_t lead; uint64_t wide; uint16_t tail[3]; };
extern int32_t silk_empty(void);
int32_t c_empty(const uint8_t *bytes, size_t length) { (void)bytes; return length == 0 ? 42 : 19; }
extern int8_t silk_i8(void);
extern uint16_t silk_u16(void);
int8_t c_i8(void) { return -7; }
uint16_t c_u16(void) { return 60000; }
extern int32_t silk_scalars(int8_t,uint8_t,int16_t,uint16_t,int32_t,uint32_t,int64_t,uint64_t,intptr_t,uintptr_t,float,double);
extern int32_t silk_callback(const int32_t *);
extern int32_t silk_pointers(int32_t *,uint8_t *,size_t,const uint8_t *const *,void *);
extern int32_t silk_packet(const struct Packet *,size_t,size_t);
extern int32_t silk_output(void);
extern int32_t silk_invoke(const int32_t *);

int32_t c_scalars(int8_t a,uint8_t b,int16_t c,uint16_t d,int32_t e,uint32_t f,int64_t g,uint64_t h,intptr_t i,uintptr_t j,float k,double l) {
  return a == -7 && b == 250 && c == -300 && d == 60000 && e == -70000 && f == UINT32_C(4000000000) && g == -INT64_C(4294967297) && h == UINT64_C(18446744073709551614) && i == -19 && j == UINT64_C(5000000000) && k == 1.5f && l == -2.25 ? 42 : 9;
}
int32_t c_nested(const uint8_t *const *values) {
  return values && values[0] && values[0][0] == 83 && values[1] == NULL ? 42 : 10;
}
int32_t c_invoke(int32_t (*callback)(const int32_t *), const int32_t *value) { return callback(value); }
void c_initialize(int32_t *value) { *value = 42; }

int main(void) {
  if (silk_empty() != 42) return 20;
  if (silk_i8() != -7 || silk_u16() != 60000) return 17;
  int32_t scalar_result = silk_scalars(-7,250,-300,60000,-70000,UINT32_C(4000000000),-INT64_C(4294967297),UINT64_C(18446744073709551614),-19,UINT64_C(5000000000),1.5f,-2.25);
  if (scalar_result != 42) { printf("scalar boundary failed: %d\n", scalar_result); return 11; }
  int32_t value = 42;
  int32_t (*callback)(const int32_t *) = silk_callback;
  if (callback(&value) != 42 || silk_invoke(&value) != 42) return 12;
  uint8_t bytes[3] = {0,0,0};
  const uint8_t text[] = "Silk";
  const uint8_t *nested[] = {text, NULL};
  _Alignas(8) uint8_t unaligned[8] = {0};
  value = 0;
  if (silk_pointers(&value,bytes,3,nested,unaligned + 1) != 42) return 13;
  int32_t copied;
  memcpy(&copied,unaligned + 1,sizeof copied);
  if (value != 42 || bytes[2] != 91 || copied != 1234567) return 14;
  struct Packet packet = {7, UINT64_C(4294967297), {1,2,60000}};
  if (silk_packet(&packet,sizeof packet,_Alignof(struct Packet)) != 42) return 15;
  if (silk_output() != 42) return 16;
  printf("native boundary passed: pointer=%zu long=%zu packet=%zu align=%zu wide=%zu tail=%zu\n",sizeof(void *),sizeof(long),sizeof packet,_Alignof(struct Packet),offsetof(struct Packet,wide),offsetof(struct Packet,tail));
  return 0;
}
