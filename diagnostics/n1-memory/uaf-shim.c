#define _GNU_SOURCE
#include <dlfcn.h>
#include <execinfo.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <stdlib.h>
/* Header before every block: magic, caller of malloc, caller of free, size. Freed blocks are never
   reused; UAF_POISON=1 also overwrites their payload with 0xA5. */
#define LIVE 0x11fe11fe11fe11feULL
#define FREED 0xdeaddeaddeaddeadULL
#define DEPTH 12
typedef struct { uint64_t magic; uint64_t alloc_site; uint64_t free_site; uint64_t size; uint64_t stack[DEPTH]; } hdr; /* 128 bytes */
static void *(*rmalloc)(size_t); static void (*rfree)(void*); static void *(*rrealloc)(void*,size_t);
static char boot[1<<20]; static size_t bootn; static int poison = -1;
static void init(void){ rmalloc=dlsym(RTLD_NEXT,"malloc"); rfree=dlsym(RTLD_NEXT,"free"); rrealloc=dlsym(RTLD_NEXT,"realloc"); }
static void *wrap(void *raw, size_t n, uintptr_t site){ if(!raw) return 0; hdr *h=raw; h->magic=LIVE; h->alloc_site=site; h->free_site=0; h->size=n; return (char*)raw+sizeof(hdr); }
static int inboot(void *p){ return (char*)p>=boot && (char*)p<boot+sizeof boot; }
void *malloc(size_t n){ if(!rmalloc) init(); if(!rmalloc){ void*p=boot+bootn; bootn+=(n+15)&~15; return p;} return wrap(rmalloc(n+sizeof(hdr)), n, (uintptr_t)__builtin_return_address(0)); }
void *calloc(size_t a, size_t b){ size_t n=a*b; void *p=malloc(n); if(p && !inboot(p)) memset(p,0,n); return p; }
void free(void *p){ if(!p||inboot(p)) return; hdr *h=(hdr*)((char*)p-sizeof(hdr)); if(h->magic!=LIVE){ if(h->magic!=FREED) { if(!rfree) init(); rfree(p);} return; }
  if(poison<0){ const char *e=getenv("UAF_POISON"); poison = e && *e=='1'; }
  h->magic=FREED; h->free_site=(uintptr_t)__builtin_return_address(0); { static __thread int busy; if(!busy){ busy=1; void *frames[DEPTH+2]; int n=backtrace(frames, DEPTH+2); for(int i=0;i<DEPTH;i++) h->stack[i]= (i+2<n)?(uintptr_t)frames[i+2]:0; busy=0; } } if(poison) memset(p,0xA5,h->size); }
void *realloc(void *p, size_t n){ if(!p) return malloc(n); if(inboot(p)){ void*q=malloc(n); memcpy(q,p,n); return q; }
  hdr *h=(hdr*)((char*)p-sizeof(hdr)); if(h->magic!=LIVE){ if(!rrealloc) init(); return rrealloc(p,n);} void *q=malloc(n); memcpy(q,p, h->size<n?h->size:n); free(p);
  ((hdr*)((char*)q-sizeof(hdr)))->alloc_site=(uintptr_t)__builtin_return_address(0); return q; }

__attribute__((constructor)) static void warm(void){ void *f[4]; backtrace(f,4); }
