#!/bin/sh
set -eu

revision=7768cd2b44049e040685d48318d13bfa7f7d32a8
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
checkout=$(mktemp -d "${TMPDIR:-/tmp}/silk-jul171-rustls.XXXXXX")
trap 'rm -rf "$checkout"' EXIT HUP INT TERM

git clone --quiet https://github.com/rustls/rustls.git "$checkout"
git -C "$checkout" checkout --quiet "$revision"
test "$(git -C "$checkout" rev-parse HEAD)" = "$revision"

cp "$script_dir/rustls_record_fixture.rs" "$checkout/rustls/src/jul171_record_fixture.rs"
printf '\n#[cfg(all(test, feature = "ring"))]\nmod jul171_record_fixture;\n' >> "$checkout/rustls/src/lib.rs"

cargo test \
  --quiet \
  --manifest-path "$checkout/Cargo.toml" \
  -p rustls \
  --lib \
  --no-default-features \
  --features ring,std \
  jul171_record_fixture \
  -- --nocapture
