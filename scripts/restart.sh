#!/usr/bin/env bash

set -e

SKIP_WASM_BUILD= cargo check
WASM_BUILD_TYPE=release cargo run -- purge-chain --dev -y
WASM_BUILD_TYPE=release cargo run -- --dev