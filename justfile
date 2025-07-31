set shell := ["bash", "-cu"]

default:
    just --list

run:
    cargo run

test:
    cargo test

fmt:
    cargo fmt

review:
    cargo insta review

profile:
    cargo build --profile profile
    valgrind --tool=callgrind ./target/profile/scl
    heaptrack ./target/profile/scl

clean: clean-profile
    cargo clean

clean-profile:
    rm -f callgrind.out.*
    rm -f heaptrack.*
