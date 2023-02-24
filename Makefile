
prog :=tasm-lang

all: lint build test bench-no-run

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
	cargo fmt

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
