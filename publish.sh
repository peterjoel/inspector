#!/usr/bin/env bash -e

(cd core; cargo publish) || exit $?
sleep 10s
(cd macros; cargo publish) || exit $?
sleep 10s
(cd query; cargo publish) || exit $?
sleep 10s
(cd parse_pest; cargo publish) || exit $?
sleep 10s
cargo publish
