#define _GNU_SOURCE
#include <dlfcn.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#define MAGIC 0x5eed5eedULL
typedef struct { uint64_t magic; uint32_t site; uint32_t pad; uint64_t size; uint64_t pad2; } hdr; /* 32 bytes keeps 16 alignment */
static void *(*rmalloc)(size_t); static void (*rfree)(void*); static void *(*rrealloc)(void*,size_t); static void *(*rcalloc)(size_t,size_t);
#define NS (1<<16)
static uintptr_t sites[NS]; static int64_t live[NS]; static int64_t cnt[NS];
static uint32_t site_of(uintptr_t a){ uint32_t h=(uint32_t)((a*0x9E3779B97F4A7C15ULL)>>48); for(;;){ if(sites[h]==a) return h; if(!sites[h]){ sites[h]=a; return h;} h=(h+1)&(NS-1);} }
static char boot[1<<20]; static size_t bootn;
static void init(void){ rmalloc=dlsym(RTLD_NEXT,"malloc"); rfree=dlsym(RTLD_NEXT,"free"); rrealloc=dlsym(RTLD_NEXT,"realloc"); rcalloc=dlsym(RTLD_NEXT,"calloc"); }
static void dump(int s){ (void)s; char path[256]; snprintf(path,256,"/tmp/n1-shim-dump.%d",getpid()); int fd=open(path,O_WRONLY|O_CREAT|O_TRUNC,0644); char line[96];
 for(int i=0;i<NS;i++) if(sites[i] && live[i]>0){ int n=snprintf(line,96,"%lx %ld %ld\n",(unsigned long)sites[i],(long)live[i],(long)cnt[i]); write(fd,line,n);} close(fd); }
__attribute__((constructor)) static void ctor(void){ init(); signal(SIGUSR1,dump); }
static void *wrap(void *raw,size_t n,uintptr_t ra){ if(!raw) return 0; hdr *h=raw; h->magic=MAGIC; h->size=n; uint32_t s=site_of(ra); h->site=s; __atomic_add_fetch(&live[s],(int64_t)n,0); __atomic_add_fetch(&cnt[s],1,0); return (char*)raw+sizeof(hdr); }
static int ours(void *p){ if(!p) return 0; if((char*)p>=boot && (char*)p<boot+sizeof boot) return 2; hdr *h=(hdr*)((char*)p-sizeof(hdr)); return h->magic==MAGIC; }
void *malloc(size_t n){ if(!rmalloc){ void*p=boot+bootn; bootn+=(n+15)&~15; return p;} return wrap(rmalloc(n+sizeof(hdr)),n,(uintptr_t)__builtin_return_address(0)); }
void *calloc(size_t a,size_t b){ size_t n=a*b; if(!rcalloc){ void*p=boot+bootn; bootn+=(n+15)&~15; memset(p,0,n); return p;} void *r=rmalloc(n+sizeof(hdr)); if(r) memset(r,0,n+sizeof(hdr)); return wrap(r,n,(uintptr_t)__builtin_return_address(0)); }
void free(void *p){ int o=ours(p); if(o==2||!p) return; if(!o){ rfree(p); return;} hdr *h=(hdr*)((char*)p-sizeof(hdr)); __atomic_sub_fetch(&live[h->site],(int64_t)h->size,0); __atomic_sub_fetch(&cnt[h->site],1,0); h->magic=0; rfree(h); }
void *realloc(void *p,size_t n){ if(!p) return wrap(rmalloc(n+sizeof(hdr)),n,(uintptr_t)__builtin_return_address(0)); int o=ours(p); if(o==2){ void*q=malloc(n); memcpy(q,p,n); return q;} if(!o) return rrealloc(p,n); hdr *h=(hdr*)((char*)p-sizeof(hdr)); uint32_t s=h->site; size_t old=h->size; void *r=rrealloc(h,n+sizeof(hdr)); if(!r) return 0; hdr *g=r; __atomic_add_fetch(&live[s],(int64_t)n-(int64_t)old,0); g->size=n; return (char*)r+sizeof(hdr); }
