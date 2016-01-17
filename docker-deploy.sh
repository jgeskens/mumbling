#!/bin/bash

docker run --rm -ti -v `pwd`:/data/project -v /Users/jef/.stack:/root/.stack fpco/stack-build:lts-4.1 bash -c 'cd /data/project; stack build' \
&& cp .stack-work/dist/x86_64-linux/*/build/mumbling/mumbling .stack-work/image/usr/local/bin/mumbling \
&& docker build -t tutum.co/jgeskens/mumbling .stack-work/image \
&& docker push tutum.co/jgeskens/mumbling

