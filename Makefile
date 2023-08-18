
prog :=tasm-lang

# Treat all warnings as errors
export RUSTFLAGS = -Dwarnings

# Set another target dir than default to avoid builds from `make`
# to invalidate cache from barebones use of `cargo` commands.
# The cache is cleared when a new `RUSTFLAGS` value is encountered,
# so to prevent the two builds from interfering, we use two dirs.
export CARGO_TARGET_DIR=./makefile-target

all: lint format build test bench-no-run

build:
	cargo build
	rustup check
	@echo "Update with \`rustup install stable\` if needed."

doc:
	cargo doc --no-deps
	xdg-open "target/doc/twenty_first/index.html"

check:
	cargo check

ctags:
	# Do `cargo install rusty-tags`
	# See https://github.com/dan-t/rusty-tags
	rusty-tags vi

format:
	cargo fmt --all -- --check

install:
	cp target/$(target)/$(prog) ~/bin/$(prog)$(extension)

lint:
	cargo clippy --all-targets -- -D warnings

# Get a stack trace upon kernel panic (may slow down implementation)
test: export RUST_BACKTRACE = 1
test:
	cargo test

bench:
	cargo bench

bench-no-run:
	cargo bench --no-run

help:
	@echo "usage: make [debug=1]"

clean:
	@echo "      ._.  ██    ██  ███  ██ ██ █████    ████ ██    █████  ███  ██  ██"
	@echo "    c/-|   ███  ███ ██ ██ ████  ██      ██    ██    ██    ██ ██ ███ ██"
	@echo "   c/--|   ████████ █████ ███   ███     ██    ██    ███   █████ ██████"
	@echo "   /  /|   ██ ██ ██ ██ ██ ████  ██      ██    ██    ██    ██ ██ ██ ███"
	@echo " mmm ' '   ██    ██ ██ ██ ██ ██ █████    ████ █████ █████ ██ ██ ██  ██"
	@rm -rf target
