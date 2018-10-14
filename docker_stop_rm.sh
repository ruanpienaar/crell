#!/bin/bash
for i in `docker ps -a | grep crell:latest | awk '{print $1}'`; do
    docker stop $i
    docker rm $i
done
