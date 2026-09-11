extern const unsigned char silk_trust_pem[];
extern const size_t silk_trust_pem_length;
extern int trust_fixture(void);

static int scenario=-1, invalid, native_error, need_capture, error_reads;
static int opens, children, stats, reads, closes, next_fd, generation;
static size_t offset;
static int active[64], directory_fd[64];

static void before(void) { if (need_capture) invalid=1; }
static int failure(int error) { native_error=error; need_capture=1; return -1; }
#ifdef __APPLE__
int *__error(void) { ++error_reads; need_capture=0; return &native_error; }
#else
int *__errno_location(void) { ++error_reads; need_capture=0; return &native_error; }
#endif
static int acquire(int directory) {
  int fd=++next_fd;
  if(fd>=64) abort();
  active[fd]=1;
  directory_fd[fd]=directory;
  return fd;
}
static int clean(void) {
  for(int i=1;i<=next_fd;i++) if(active[i]) return 0;
  return !need_capture;
}
static void finish_previous(void) {
  if(scenario<0) return;
  int reload=scenario==10 || scenario==11;
  int expected=reload ? 2 : 1;
  if(!clean() || opens!=expected || children!=expected || stats!=(scenario==7 || scenario==14 || scenario==15 ? 0 : expected)) invalid=1;
  if(scenario==0 && (reads!=2 || closes!=2)) invalid=1;
  if(scenario==1 && (reads<=3 || closes!=2)) invalid=1;
  if((scenario==2 || scenario==3 || scenario==4 || scenario==6 || scenario==8 || scenario==9 || scenario==13) && closes!=2) invalid=1;
  if(scenario==5 && (reads!=1 || closes!=2 || error_reads!=2)) invalid=1;
  if(scenario==12 && (reads!=0 || closes!=2 || error_reads!=1)) invalid=1;
  if((scenario==7 || scenario==14 || scenario==15) && (reads!=0 || closes!=1)) invalid=1;
  if((scenario==10 || scenario==11) && (generation!=2 || closes!=4)) invalid=1;
}
static int parse_case(const char *path) {
  if(strncmp(path,"/case",5)) return -1;
  char *end=NULL;
  long value=strtol(path+5,&end,10);
  if(end==NULL || *end!='\0' || value<0 || value>16) return -1;
  return (int)value;
}

int open(const char *path,int flags,...) {
  before();
  int selected=parse_case(path);
  if(selected<0 || flags!=(O_DIRECTORY|O_NOFOLLOW|O_CLOEXEC)) invalid=1;
  if(selected!=scenario) {
    finish_previous();
    scenario=selected;
    opens=children=stats=reads=closes=next_fd=generation=error_reads=0;
    offset=0;
    memset(active,0,sizeof(active));
  }
  ++opens;
  if(scenario==16 && invalid) return failure(EINVAL);
  return acquire(1);
}

int openat(int parent,const char *path,int flags,...) {
  before(); ++children;
  if(parent<1 || !active[parent] || !directory_fd[parent] || strcmp(path,"file")) invalid=1;
  if(flags!=(O_NOFOLLOW|O_CLOEXEC|O_NONBLOCK)) invalid=1;
  if(scenario==7) return failure(EACCES);
  if(scenario==14) return failure(ELOOP);
  if(scenario==15) return failure(ENOENT);
  ++generation;
  offset=0;
  return acquire(0);
}

int close(int fd) {
  before(); ++closes;
  if(fd<1 || fd>=64 || !active[fd]) { invalid=1; return -1; }
  active[fd]=0;
  if(!directory_fd[fd] && (scenario==5 || scenario==6 || scenario==12)) {
    native_error=EBADF;
    return -1;
  }
  native_error=EBADF;
  return 0;
}

int fstat(int fd,struct stat *out) {
  before(); ++stats;
  if(fd<1 || fd>=64 || !active[fd]) invalid=1;
  memset(out,0,sizeof(*out));
  out->st_mode=(scenario==13 ? S_IFDIR : S_IFREG)|0644;
  out->st_size=(off_t)silk_trust_pem_length;
  native_error=EIO;
  return 0;
}

ssize_t read(int fd,void *data,size_t count) {
  before(); ++reads;
  if(fd<1 || fd>=64 || !active[fd] || directory_fd[fd] || count<1 || count>4096) invalid=1;
  if(scenario==5 || (scenario==11 && generation==2)) return failure(EIO);
  if(scenario==8) return 0;
  size_t copies=(scenario==0 || scenario==1 || (scenario==10 && generation==2)) ? 2 : 1;
  size_t total=silk_trust_pem_length*copies;
  static const unsigned char malformed[]="-----BEGIN CERTIFICATE-----\n!\n-----END CERTIFICATE-----\n";
  size_t malformed_length=sizeof(malformed)-1;
  if(scenario==9) total+=malformed_length;
  if(offset>=total) return 0;
  size_t available=total-offset;
  size_t accepted=count<available ? count : available;
  if(scenario==1 && accepted>17) accepted=17;
  unsigned char *output=(unsigned char *)data;
  size_t produced=0;
  while(produced<accepted) {
    size_t logical=offset+produced;
    if(logical<silk_trust_pem_length*copies) {
      size_t position=logical%silk_trust_pem_length;
      size_t chunk=silk_trust_pem_length-position;
      if(chunk>accepted-produced) chunk=accepted-produced;
      memcpy(output+produced,silk_trust_pem+position,chunk);
      produced+=chunk;
    } else {
      size_t position=logical-silk_trust_pem_length*copies;
      size_t chunk=malformed_length-position;
      if(chunk>accepted-produced) chunk=accepted-produced;
      memcpy(output+produced,malformed+position,chunk);
      produced+=chunk;
    }
  }
  offset+=accepted;
  return (ssize_t)accepted;
}

int main(void) { return trust_fixture(); }
