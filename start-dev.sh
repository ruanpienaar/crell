#!/bin/sh
erl -name remotenode@localhost -setcookie remotenode -noshell -noinput -detached
rel/crell/bin/crell start
sleep 1
rel/crell/bin/crell attach