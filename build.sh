#!/bin/sh

#build compiler for release
cargo build --release
strip targets/release/active_oberon

# Starting executing all UnitTests for project
cargo test