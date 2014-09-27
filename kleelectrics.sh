#!/bin/sh

_script="$(readlink -f ${BASH_SOURCE[0]})"
_base="$(dirname $_script)"

R -f "$_base/kleelectrics.r"
