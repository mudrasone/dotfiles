#!/bin/bash
docker build -t civiclabs:latest .
docker run -d --privileged=true -e TERM --name dev civiclabs:latest
docker exec -it dev bash
