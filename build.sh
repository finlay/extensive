#!/bin/bash

set -ex

make

cp timings/* /output/
