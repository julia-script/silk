export const nativeSocketAcceptanceSource = `
import silk.byte_duplex {ByteDuplex, ByteIoError, ByteIoOperation, ReadTransfer}
import silk.effect {Effect}
import silk.i64
import silk.monotonic_clock {MonotonicClock}
import silk.native_socket {ConnectOptions, ConnectOptionsError, ConnectOptionsReason, Connection, ConnectionPhase, InvalidEndpointReason, NativeSocketError, connectResolved, connectUnix}
import silk.network_address {Endpoint, IpAddress, Ipv4Address, Port}
import silk.option {Option}
import silk.result {Result}
import silk.system_clock {Instant, SystemClock}
import silk.u64
import silk.usize

unsafe extern "C" fn silk_socket_stub_reset(mode: i32) -> ()
unsafe extern "C" fn silk_socket_stub_sockets() -> i32
unsafe extern "C" fn silk_socket_stub_connects() -> i32
unsafe extern "C" fn silk_socket_stub_closes() -> i32
unsafe extern "C" fn silk_socket_stub_polls() -> i32
unsafe extern "C" fn silk_socket_stub_recvs() -> i32
unsafe extern "C" fn silk_socket_stub_shutdowns() -> i32
unsafe extern "C" fn silk_socket_stub_configuration_ok() -> i32
unsafe extern "C" fn silk_socket_stub_transfer_ok() -> i32
unsafe extern "C" fn silk_socket_darwin_witness() -> i32
unsafe extern "C" fn silk_socket_gnu_witness() -> i32

struct FixtureClock {
  nowValue: Instant
  waits: usize
}

impl MonotonicClock for FixtureClock {
  effect fn now(self: &mut Self) -> Instant {
    return SystemClock.make(
      SystemClock.seconds(&self.nowValue),
      SystemClock.nanoseconds(&self.nowValue),
    )
  }
  effect fn getResolution(self: &mut Self) -> u64 { return 1 }
  effect fn waitUntil(self: &mut Self, when: Instant) -> () with Intrinsic.nonParking() {
    self.nowValue = move when
    self.waits = self.waits + usize.ONE
    return ()
  }
  effect fn waitFor(self: &mut Self, howLong: u64) -> () with Intrinsic.nonParking() { return () }
}

fn endpoint(last: u8) -> Endpoint {
  return Endpoint.make(
    IpAddress.V4 {value: Ipv4Address.fromOctets([192, 0, 2, last])},
    Port.fromU16(443),
  )
}

effect fn exerciseConnection(connection: &mut Connection) -> i32
! ByteIoError
? &mut MonotonicClock {
  if Connection.phase(connection) != ConnectionPhase.Open { return 1 }
  let mut output: [u8; 4] = [0, 0, 0, 0]
  let first = run ByteDuplex.readSome(&mut output, Option.none<Instant>())
    |> Effect.provideMut<ByteDuplex>(&mut connection.*)
  match move first {
    ReadTransfer.Data {count} => {
      if count != 2 || output[0] != 104 || output[1] != 105 { return 2 }
    }
    ReadTransfer.End => { return 3 }
  }
  let second = run ByteDuplex.readSome(&mut output, Option.none<Instant>())
    |> Effect.provideMut<ByteDuplex>(&mut connection.*)
  match move second {
    ReadTransfer.Data {count} => { return 4 }
    ReadTransfer.End => {}
  }
  let third = run ByteDuplex.readSome(&mut output, Option.none<Instant>())
    |> Effect.provideMut<ByteDuplex>(&mut connection.*)
  match move third {
    ReadTransfer.Data {count} => { return 5 }
    ReadTransfer.End => {}
  }
  let written = run ByteDuplex.writeSome(b"hello", Option.none<Instant>())
    |> Effect.provideMut<ByteDuplex>(&mut connection.*)
  if written != 2 { return 6 }
  run ByteDuplex.flush(Option.none<Instant>())
    |> Effect.provideMut<ByteDuplex>(&mut connection.*)
  run ByteDuplex.shutdownWrite(Option.none<Instant>())
    |> Effect.provideMut<ByteDuplex>(&mut connection.*)
  run ByteDuplex.shutdownWrite(Option.none<Instant>())
    |> Effect.provideMut<ByteDuplex>(&mut connection.*)
  if Connection.phase(connection) != ConnectionPhase.WriteClosed { return 7 }
  return 42
}

effect fn runResolved(mode: i32, two: bool, expectedWaits: usize) -> i32
! NativeSocketError | ByteIoError {
  unsafe { silk_socket_stub_reset(mode) }
  let mut clock = FixtureClock {
    nowValue: SystemClock.make(0, 0),
    waits: usize.ZERO,
  }
  let first = endpoint(1)
  let second = endpoint(2)
  let mut result = 0
  if two {
    let endpoints = [first, second]
    result = run connectResolved(
      &endpoints,
      ConnectOptions.defaults(),
      Option.none<Instant>(),
      exerciseConnection,
    ) |> Effect.provideMut<MonotonicClock>(&mut clock)
  } else {
    let endpoints = [first]
    result = run connectResolved(
      &endpoints,
      ConnectOptions.defaults(),
      Option.none<Instant>(),
      exerciseConnection,
    ) |> Effect.provideMut<MonotonicClock>(&mut clock)
  }
  if result != 42 { return result }
  if clock.waits != expectedWaits { return 20 }
  if unsafe silk_socket_stub_configuration_ok() != 1 { return 21 }
  if unsafe silk_socket_stub_transfer_ok() != 1 { return 22 }
  if unsafe silk_socket_stub_shutdowns() != 1 { return 23 }
  if unsafe silk_socket_stub_closes() != unsafe silk_socket_stub_sockets() { return 24 }
  return 42
}

effect fn runUnixRetry() -> i32 ! NativeSocketError | ByteIoError {
  unsafe { silk_socket_stub_reset(3) }
  let mut clock = FixtureClock {
    nowValue: SystemClock.make(0, 0),
    waits: usize.ZERO,
  }
  let result = run connectUnix(
    b"/tmp/silk-jul-146.sock",
    ConnectOptions.defaults(),
    Option.none<Instant>(),
    exerciseConnection,
  ) |> Effect.provideMut<MonotonicClock>(&mut clock)
  if result != 42 { return result }
  if clock.waits != usize.ONE { return 30 }
  if unsafe silk_socket_stub_sockets() != 2 || unsafe silk_socket_stub_connects() != 2 { return 31 }
  if unsafe silk_socket_stub_closes() != 2 { return 32 }
  return 42
}

effect fn noTransfer(connection: &mut Connection) -> i32 { return 42 }

effect fn invalidCases() -> i32 {
  match move ConnectOptions.make(false, 0, usize.ONE) {
    Result<ConnectOptions, ConnectOptionsError>.Success {value} => { return 60 }
    Result<ConnectOptions, ConnectOptionsError>.Failure {error} => {
      if error.reason != ConnectOptionsReason.PollInterval || error.value != 0 { return 61 }
    }
  }
  match move ConnectOptions.make(false, 1, 1025) {
    Result<ConnectOptions, ConnectOptionsError>.Success {value} => { return 62 }
    Result<ConnectOptions, ConnectOptionsError>.Failure {error} => {
      if error.reason != ConnectOptionsReason.MaxAttempts || error.value != 1025 { return 63 }
    }
  }
  unsafe { silk_socket_stub_reset(0) }
  let mut clock = FixtureClock {
    nowValue: SystemClock.make(0, 0),
    waits: usize.ZERO,
  }
  let attempted = run Effect.result(connectUnix(
    b"relative.sock",
    ConnectOptions.defaults(),
    Option.none<Instant>(),
    noTransfer,
  ) |> Effect.provideMut<MonotonicClock>(&mut clock))
  match move attempted {
    Result<i32, NativeSocketError>.Success {value} => { return 64 }
    Result<i32, NativeSocketError>.Failure {error} => match move error {
      NativeSocketError.InvalidEndpoint {reason, offset, limit} => {
        if reason != InvalidEndpointReason.RelativePath || offset != usize.ZERO { return 65 }
      }
      _ => { return 66 }
    }
  }
  if unsafe silk_socket_stub_sockets() != 0 || clock.waits != usize.ZERO { return 67 }
  return 42
}

effect fn runTimed(mode: i32, now: Instant, deadline: Option<Instant>) -> i32 {
  unsafe { silk_socket_stub_reset(mode) }
  let mut clock = FixtureClock {nowValue: move now, waits: usize.ZERO}
  let endpoints = [endpoint(1)]
  let attempted = run Effect.result(connectResolved(
    &endpoints,
    ConnectOptions.defaults(),
    move deadline,
    noTransfer,
  ) |> Effect.provideMut<MonotonicClock>(&mut clock))
  return match move attempted {
    Result<i32, NativeSocketError>.Success {value} => 50
    Result<i32, NativeSocketError>.Failure {error} => match move error {
      NativeSocketError.Timeout => {
        if clock.waits != 2 || unsafe silk_socket_stub_polls() != 2 { return 51 }
        return 42
      }
      NativeSocketError.TimeRangeError => {
        if clock.waits != usize.ZERO || unsafe silk_socket_stub_polls() != 1 { return 52 }
        return 42
      }
      _ => 53
    }
  }
}

effect fn nativeCases() -> i32 ! NativeSocketError | ByteIoError {
  if unsafe silk_socket_darwin_witness() != 42 { return 40 }
  if unsafe silk_socket_gnu_witness() != 42 { return 41 }
  let invalid = run invalidCases()
  if invalid != 42 { return invalid }
  let immediate = run runResolved(0, false, usize.ZERO)
  if immediate != 42 { return immediate }
  let fallback = run runResolved(1, true, usize.ZERO)
  if fallback != 42 { return fallback }
  let pending = run runResolved(2, false, usize.ZERO)
  if pending != 42 { return pending }
  let interrupted = run runResolved(5, false, usize.ONE)
  if interrupted != 42 { return interrupted }
  let timed = run runTimed(
    4,
    SystemClock.make(0, 0),
    Option.some<Instant>(SystemClock.make(0, 2000000)),
  )
  if timed != 42 { return timed }
  let overflow = run runTimed(
    4,
    SystemClock.make(i64.MAX, 999500000),
    Option.none<Instant>(),
  )
  if overflow != 42 { return overflow }
  static if Intrinsic.targetOperatingSystem() == "linux" {
    let backlog = run runUnixRetry()
    if backlog != 42 { return backlog }
  }
  return 42
}

effect fn recoverNative(error: NativeSocketError | ByteIoError) -> i32 { return 90 }
pub fn main() -> i32 { return run nativeCases() |> Effect.catchAll(recoverNative) }
`

export const nativeSocketCancellationAnalysisSource = `
import silk.allocator {Allocator, OutOfMemoryError}
import silk.byte_duplex {ByteDuplex, ByteIoError, ReadTransfer}
import silk.effect {Effect}
import silk.execution {Execution}
import silk.monotonic_clock {MonotonicClock}
import silk.native_socket {ConnectOptions, Connection, NativeSocketError, connectResolved}
import silk.network_address {Endpoint, IpAddress, Ipv4Address, Port}
import silk.option {Option}
import silk.system_clock {Instant, SystemClock}
import silk.u64

fn cancellationEndpoint() -> Endpoint {
  return Endpoint.make(
    IpAddress.V4 {value: Ipv4Address.fromOctets([192, 0, 2, 1])},
    Port.fromU16(443),
  )
}

struct ParkingClock {}
struct ParkGuard {wake: Intrinsic.Wake}
fn retainWake(wake: Intrinsic.Wake) -> ParkGuard { return ParkGuard {wake: move wake} }
impl MonotonicClock for ParkingClock {
  effect fn now(self: &mut Self) -> Instant { return SystemClock.make(0, 0) }
  effect fn getResolution(self: &mut Self) -> u64 { return 1 }
  effect fn waitUntil(self: &mut Self, when: Instant) -> () {
    drop when
    run Execution.park(retainWake)
    return ()
  }
  effect fn waitFor(self: &mut Self, howLong: u64) -> () {
    drop howLong
    run Execution.park(retainWake)
    return ()
  }
}

effect fn parkRead(connection: &mut Connection) -> i32
! ByteIoError
? &mut MonotonicClock {
  let mut output: [u8; 1] = [0]
  let transfer = run ByteDuplex.readSome(&mut output, Option.none<Instant>())
    |> Effect.provideMut<ByteDuplex>(&mut connection.*)
  drop move transfer
  return 1
}

effect fn suspendedConnection() -> i32 ! NativeSocketError | ByteIoError {
  let endpoints = [cancellationEndpoint()]
  let mut clock = ParkingClock {}
  return run connectResolved(
    &endpoints,
    ConnectOptions.defaults(),
    Option.none<Instant>(),
    parkRead,
  ) |> Effect.provideMut<MonotonicClock>(&mut clock)
}

fn cancellationReady(state: &i32) -> () { return () }
fn cancellationComplete(state: &mut i32, value: i32) -> () { state.* = value return () }
fn cancellationParked(state: &mut i32, execution: Intrinsic.Execution<i32>) -> () {
  drop move execution
  return ()
}
effect fn cancellationFailed(error: NativeSocketError | ByteIoError) -> i32 { return 70 }

effect fn cancelSuspendedConnection() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let body = Effect.catchAll(suspendedConnection(), cancellationFailed)
  let execution = run Execution.make(move body, 0, cancellationReady)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut result = 0
  run Execution.drive(move execution, &mut result, cancellationComplete, cancellationParked)
  return result
}

effect fn recoverCancellation(error: OutOfMemoryError) -> i32 { return 90 }
pub fn main() -> i32 {
  return run cancelSuspendedConnection() |> Effect.catchAll(recoverCancellation)
}
`

export const nativeSocketStubSource = `
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>
#if defined(__linux__)
#include <sys/syscall.h>
#endif

static int silk_mode;
static int silk_sockets;
static int silk_connects;
static int silk_closes;
static int silk_shutdowns;
static int silk_polls;
static int silk_recv_calls;
static int silk_configuration_ok;
static int silk_transfer_ok;
static int silk_configuration_mask;

void silk_socket_stub_reset(int mode) {
  silk_mode = mode;
  silk_sockets = 0;
  silk_connects = 0;
  silk_closes = 0;
  silk_shutdowns = 0;
  silk_polls = 0;
  silk_recv_calls = 0;
  silk_configuration_ok = 1;
  silk_transfer_ok = 1;
  silk_configuration_mask = 0;
}
int silk_socket_stub_sockets(void) { return silk_sockets; }
int silk_socket_stub_connects(void) { return silk_connects; }
int silk_socket_stub_closes(void) {
#if defined(__APPLE__)
  int closed = 0;
  for (int index = 1; index <= silk_sockets; index += 1) {
    errno = 0;
    if (write(100 + index, "", 0) < 0 && errno == EBADF) closed += 1;
  }
  return closed;
#else
  return silk_closes;
#endif
}
int silk_socket_stub_polls(void) { return silk_polls; }
int silk_socket_stub_recvs(void) { return silk_recv_calls; }
int silk_socket_stub_shutdowns(void) { return silk_shutdowns; }
int silk_socket_stub_configuration_ok(void) {
#if defined(__APPLE__)
  return silk_configuration_ok && (silk_configuration_mask & 15) == 15;
#else
  return silk_configuration_ok && (silk_configuration_mask & 5) == 5;
#endif
}
int silk_socket_stub_transfer_ok(void) { return silk_transfer_ok; }

int socket(int domain, int type, int protocol) {
  silk_sockets += 1;
#if defined(__linux__)
  if ((type & SOCK_NONBLOCK) == 0 || (type & SOCK_CLOEXEC) == 0) silk_configuration_ok = 0;
  silk_configuration_mask |= 1;
#else
  if (type != SOCK_STREAM) silk_configuration_ok = 0;
#endif
  if (domain != AF_INET && domain != AF_INET6 && domain != AF_UNIX) silk_configuration_ok = 0;
  if (domain == AF_UNIX && protocol != 0) silk_configuration_ok = 0;
  if (domain != AF_UNIX && protocol != IPPROTO_TCP) silk_configuration_ok = 0;
#if defined(__APPLE__)
  int seed = open("/dev/null", O_RDWR);
  int descriptor = 100 + silk_sockets;
  if (seed < 0 || dup2(seed, descriptor) < 0) return -1;
  close(seed);
  return descriptor;
#else
  return 100 + silk_sockets;
#endif
}

#if defined(__APPLE__)
int fcntl(int fd, int command, ...) {
  if (fd < 100) return -1;
  if (command == F_GETFL) return 0;
  if (command == F_GETFD) return 0;
  va_list arguments;
  va_start(arguments, command);
  int value = va_arg(arguments, int);
  va_end(arguments);
  if (command == F_SETFL) {
    if ((value & O_NONBLOCK) == 0) silk_configuration_ok = 0;
    silk_configuration_mask |= 1;
    return 0;
  }
  if (command == F_SETFD) {
    if ((value & FD_CLOEXEC) == 0) silk_configuration_ok = 0;
    silk_configuration_mask |= 2;
    return 0;
  }
  silk_configuration_ok = 0;
  errno = EINVAL;
  return -1;
}
#endif

int setsockopt(int fd, int level, int option, const void *value, socklen_t length) {
  if (fd < 100 || value == NULL || length == 0) silk_configuration_ok = 0;
  if (level == SOL_SOCKET && option == SO_LINGER) {
    const struct linger *linger = (const struct linger *)value;
    if (length != sizeof(*linger) || linger->l_onoff != 0 || linger->l_linger != 0) silk_configuration_ok = 0;
    silk_configuration_mask |= 4;
  }
#if defined(__APPLE__)
  if (level == SOL_SOCKET && option == SO_NOSIGPIPE) {
    if (length != sizeof(int) || *(const int *)value != 1) silk_configuration_ok = 0;
    silk_configuration_mask |= 8;
  }
#endif
  if (level == IPPROTO_TCP && option == TCP_NODELAY) silk_configuration_ok = 0;
  return 0;
}

int connect(int fd, const struct sockaddr *address, socklen_t length) {
  silk_connects += 1;
  if (fd < 100 || address == NULL || length < 2) silk_configuration_ok = 0;
  if (address != NULL && address->sa_family == AF_INET) {
    const struct sockaddr_in *ipv4 = (const struct sockaddr_in *)address;
    unsigned char expected = silk_mode == 1 ? (unsigned char)silk_connects : 1;
    const unsigned char *bytes = (const unsigned char *)&ipv4->sin_addr;
    if (length != sizeof(*ipv4) || ntohs(ipv4->sin_port) != 443 || bytes[3] != expected) {
      silk_configuration_ok = 0;
    }
  }
  if (address != NULL && address->sa_family == AF_UNIX) {
    const struct sockaddr_un *local = (const struct sockaddr_un *)address;
    static const char expected[] = "/tmp/silk-jul-146.sock";
    if (length != offsetof(struct sockaddr_un, sun_path) + sizeof(expected)
      || memcmp(local->sun_path, expected, sizeof(expected)) != 0) {
      silk_configuration_ok = 0;
    }
  }
  if (silk_mode == 1 && silk_connects == 1) { errno = ECONNREFUSED; return -1; }
  if (silk_mode == 2 || silk_mode == 4 || silk_mode == 6) { errno = EINPROGRESS; return -1; }
  if (silk_mode == 5 && silk_connects == 1) { errno = EINTR; return -1; }
#if defined(__linux__)
  if (silk_mode == 3 && silk_connects == 1) { errno = EAGAIN; return -1; }
#endif
  return 0;
}

int poll(struct pollfd *fds, nfds_t count, int timeout) {
  silk_polls += 1;
  if (count != 1 || timeout != 0 || fds == NULL) silk_configuration_ok = 0;
  if (fds != NULL) fds[0].revents = (silk_mode == 4 || silk_mode == 6) ? 0 : POLLOUT;
  if (silk_mode == 4 || silk_mode == 6) return 0;
  return 1;
}

int getsockopt(int fd, int level, int option, void *value, socklen_t *length) {
  if (fd < 100 || level != SOL_SOCKET || option != SO_ERROR || value == NULL || length == NULL) {
    errno = EINVAL;
    return -1;
  }
  if (*length < sizeof(int)) { errno = EINVAL; return -1; }
  *(int *)value = 0;
  *length = sizeof(int);
  return 0;
}

ssize_t recv(int fd, void *buffer, size_t length, int flags) {
  if (fd < 100 || buffer == NULL || length == 0 || flags != 0) silk_transfer_ok = 0;
  silk_recv_calls += 1;
  if (silk_mode == 6) { errno = EAGAIN; return -1; }
  if (silk_recv_calls == 1) {
    if (length < 2) { errno = EINVAL; return -1; }
    ((unsigned char *)buffer)[0] = 'h';
    ((unsigned char *)buffer)[1] = 'i';
    return 2;
  }
  return 0;
}

ssize_t send(int fd, const void *buffer, size_t length, int flags) {
  if (fd < 100 || buffer == NULL || length != 5) silk_transfer_ok = 0;
#if defined(__linux__)
  if ((flags & MSG_NOSIGNAL) == 0) silk_transfer_ok = 0;
#else
  if (flags != 0) silk_transfer_ok = 0;
#endif
  return 2;
}

int shutdown(int fd, int how) {
  if (fd < 100 || how != SHUT_WR) silk_transfer_ok = 0;
  silk_shutdowns += 1;
  return 0;
}

#if !defined(__APPLE__)
int close(int fd) {
  if (fd >= 100) { silk_closes += 1; return 0; }
  return (int)syscall(SYS_close, fd);
}
#endif
`

export const nativeSocketDarwinWitnessSource = `
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <stddef.h>
#include <sys/socket.h>
#include <sys/un.h>
#if defined(__APPLE__)
_Static_assert(sizeof(struct sockaddr_in) == 16, "Darwin sockaddr_in size");
_Static_assert(sizeof(struct sockaddr_in6) == 28, "Darwin sockaddr_in6 size");
_Static_assert(offsetof(struct sockaddr_un, sun_path) == 2, "Darwin sockaddr_un header");
_Static_assert(sizeof(((struct sockaddr_un *)0)->sun_path) == 104, "Darwin sun_path capacity");
_Static_assert(AF_INET == 2 && AF_INET6 == 30 && AF_UNIX == 1, "Darwin families");
_Static_assert(SO_ERROR == 0x1007 && SO_LINGER == 0x80 && SO_NOSIGPIPE == 0x1022, "Darwin socket options");
_Static_assert(POLLIN == 1 && POLLOUT == 4 && POLLERR == 8 && POLLHUP == 16 && POLLNVAL == 32, "Darwin poll bits");
#endif
int silk_socket_darwin_witness(void) { return 42; }
`

export const nativeSocketGnuWitnessSource = `
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <stddef.h>
#include <sys/socket.h>
#include <sys/un.h>
#if defined(__linux__) && defined(__GLIBC__)
_Static_assert(sizeof(struct sockaddr_in) == 16, "GNU sockaddr_in size");
_Static_assert(sizeof(struct sockaddr_in6) == 28, "GNU sockaddr_in6 size");
_Static_assert(offsetof(struct sockaddr_un, sun_path) == 2, "GNU sockaddr_un header");
_Static_assert(sizeof(((struct sockaddr_un *)0)->sun_path) == 108, "GNU sun_path capacity");
_Static_assert(AF_INET == 2 && AF_INET6 == 10 && AF_UNIX == 1, "GNU families");
_Static_assert(SO_ERROR == 4 && SO_LINGER == 13, "GNU socket options");
_Static_assert(POLLIN == 1 && POLLOUT == 4 && POLLERR == 8 && POLLHUP == 16 && POLLNVAL == 32, "GNU poll bits");
#endif
int silk_socket_gnu_witness(void) { return 42; }
`

/** Profile-agnostic JUL-146 corpus entry; the shared harness owns debug/optimized profile selection. */
export const nativeSocketCorpusProgram = Object.freeze({
  name: 'native-socket-connections',
  source: 'pub fn main() -> i32 { return 42 }',
  nativeSource: nativeSocketAcceptanceSource,
  nativeCSources: Object.freeze({
    native_socket_stub: nativeSocketStubSource,
    native_socket_darwin_witness: nativeSocketDarwinWitnessSource,
    native_socket_gnu_witness: nativeSocketGnuWitnessSource,
  }),
  expected: Object.freeze({ _tag: 'Completes' as const, result: 42 }),
})
