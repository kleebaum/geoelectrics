#!/bin/sh
_script="$(readlink -f ${BASH_SOURCE[0]})"
_base="$(dirname $_script)"

echo $_base
 
R -f "$_base/kleelectrics.r"
