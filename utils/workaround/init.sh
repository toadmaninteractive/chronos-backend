#!/bin/bash

cd $(dirname $0)
mkdir server
rm -rf src
git clone --single-branch --branch main https://github.com/toadmaninteractive/chronos-backend src
. /usr/local/erlang/21.3/activate
cd src
export JSX_FORCE_MAPS=true
make update all install DESTDIR=../server
cd ..
cp server/etc/app.config.sample server/etc/app.config
cp server/etc/server.conf.sample server/etc/server.conf
cp server/etc/vm.args.sample server/etc/vm.args
server/bin/server start
