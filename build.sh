#!/bin/bash

set -ex

make

cp symmetric.pdf /output/
cp timings/* /output/
