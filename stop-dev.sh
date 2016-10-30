#!/bin/sh
rel/crell/bin/crell ping
rel/crell/bin/crell stop
ps aux | grep beam | grep "remotenode@localhost" | awk '{print $2}' | xargs kill
