#include <cstdint>
#include <cstdio>
#include <cstdlib>

static std::int32_t status;
extern "C" void fixture_write_renamed(std::int32_t *value) { *value = 42; }
extern "C" std::int32_t fixture_read_renamed(const std::int32_t *value) { return *value; }
extern "C" std::int32_t *fixture_alias_renamed(std::int32_t *value) { return value; }
extern "C" void fixture_nocapture(std::int32_t *value) { *value = 73; }
extern "C" std::int32_t fixture_operation_renamed() { status = 17; return -1; }
extern "C" std::int32_t *fixture_accessor_renamed() { return &status; }
extern "C" void fixture_intervening_renamed() { status = 29; }
extern "C" void fixture_stop() { std::exit(23); }
extern "C" std::int32_t silk_stop();
extern "C" void fixture_throw() { throw 42; }
extern "C" std::int32_t silk_contracts();
extern "C" void silk_throw();
extern "C" void silk_trap_report_v1(std::int32_t) { std::abort(); }
int main(int argc, char **) {
  if (argc > 2) return silk_stop();
  if (argc == 1) {
    const auto result = silk_contracts();
    if (result != 0) return result;
    std::puts("contracts-ok");
    return 0;
  }
  // A cleanup-only personality would continue the search to this handler. The Silk boundary
  // must terminate during the search, before this enclosing C++ catch is ever selected.
  try { silk_throw(); }
  catch (...) { std::puts("escaped-to-catch"); return 77; }
  std::puts("unexpected-return");
  return 78;
}
