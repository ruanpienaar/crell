#!/bin/bash
cp -r apps/crell/priv/* rel/crell/lib/crell-1/priv/
rebar compile && cp apps/crell/ebin/* rel/crell/lib/crell-1/ebin/