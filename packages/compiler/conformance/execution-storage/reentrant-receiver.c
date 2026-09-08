#include <stdio.h>
#include <string.h>

extern int storage_lifecycle(void);
static _Alignas(4096) unsigned char arena[1048576];
static unsigned char snapshot[1048576];
static struct { void *address; size_t size; int live, nested; } records[1024];
static size_t cursor;
static int count, active, nested, ordinal, selected, entered, invalid;

void storage_reenter(void) {
  if (!active || nested || ++ordinal != selected) return;
  ++entered;
  size_t saved = cursor;
  int saved_count = count;
  memcpy(snapshot, arena, saved);
  nested = 1;
  if (storage_lifecycle() != 42) invalid = 1;
  nested = 0;
  for (int i = 0; i < saved_count; ++i) {
    if (records[i].live) {
      size_t offset = (unsigned char *)records[i].address - arena;
      if (memcmp(records[i].address, snapshot + offset, records[i].size)) invalid = 1;
    }
  }
  for (int i = saved_count; i < count; ++i) if (records[i].live) invalid = 1;
}

void *malloc(size_t size) {
  size_t aligned = (cursor + 63) & ~(size_t)63;
  if (aligned > sizeof arena || size > sizeof arena - aligned || count == 1024) {
    invalid = 1; return NULL;
  }
  void *address = arena + aligned;
  cursor = aligned + size;
  records[count].address = address;
  records[count].size = size;
  records[count].live = 1;
  records[count].nested = nested;
  ++count;
  return address;
}

void free(void *pointer) {
  if (!pointer) return;
  for (int i = 0; i < count; ++i) {
    if (records[i].address == pointer) {
      if (!records[i].live || records[i].nested != nested) invalid = 1;
      records[i].live = 0;
      return;
    }
  }
  invalid = 1;
}

static int exercise(int at) {
  cursor = 0; count = 0; ordinal = 0; selected = at; entered = 0; invalid = 0;
  nested = 0; active = 1;
  int result = storage_lifecycle();
  active = 0;
  for (int i = 0; i < count; ++i) if (records[i].live) invalid = 1;
  if (result != 42 || invalid || entered != (at != 0)) return 1;
  return 0;
}

int main(void) {
  if (exercise(0)) return 1;
  int boundaries = ordinal;
  if (!boundaries) return 2;
  for (int i = 1; i <= boundaries; ++i) {
    if (exercise(i)) {
      fprintf(stderr, "reentrant callback=%d calls=%d entered=%d invalid=%d\n", i, ordinal, entered, invalid);
      return 3;
    }
  }
  printf("%d source callback boundaries preserve outer storage and release nested packages exactly once\n", boundaries);
  return 42;
}
