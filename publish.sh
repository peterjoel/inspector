#!/usr/bin/env bash -e

(cd core; cargo publish) || exit $?
sleep 25s
(cd macros; cargo publish) || exit $?
sleep 25s
(cd query; cargo publish) || exit $?
sleep 25s
(cd parse_pest; cargo publish) || exit $?
sleep 25s
(cargo publish) || exit $?
sleep 25s
(cd cli; cargo publish) || exit $?
