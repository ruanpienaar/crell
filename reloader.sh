#!/bin/bash
cp -r apps/crell/priv/* rel/crell/lib/crell-1/priv/
rebar compile && cp apps/crell/ebin/* rel/crell/lib/crell-1/ebin/ && cp apps/crell_web/ebin/* rel/crell/lib/crell_web-1/ebin/