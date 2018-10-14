#!/bin/bash
set -e
CONTAINER_ID=`docker run -P -td crell:latest`
docker ps -a
docker logs -f $CONTAINER_ID