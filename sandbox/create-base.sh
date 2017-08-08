#!/bin/bash

NAME=bn-base

lxc image delete $NAME
lxc delete $NAME
lxc launch ubuntu:16.04 $NAME

echo "Waiting for container to boot..."
while [[ ! `lxc exec "$NAME" -- runlevel` =~ ^N ]]; do
    sleep 1
done

echo "Running apt..."
lxc exec $NAME -- bash <<END
echo "" > /etc/apt/apt.conf.d/20auto-upgrades

#dpkg --add-architecture i386
#apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
#add-apt-repository -y ppa:avsm/ppa

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get upgrade -y

apt-get install -y ruby python3
apt-get install -y libarchive-zip-perl libipc-system-perl
apt-get install -y clang valgrind build-essential
apt-get install -y openjdk-8-jdk
apt-get install -y libfuse-dev pkg-config
apt-get install -y wamerican libbsd-dev libgmp-dev

#apt-get install -y ocaml ocaml-native-compilers camlp4-extra opam \
#        nasm m4 gcc-multilib g++-multilib libc6-dev-i386 libc6-dev \
#        libc6 libc6-dbg libc6-dbg:i386

#su ubuntu -c "cd ~student ; opam init -y"
#su ubuntu -c "echo \". /home/student/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true\" >> ~student/.profile"
#su ubuntu -c "cd ~student ; opam install -y ounit extlib ocamlfind"

apt-get upgrade -y

END

echo "Stopping; Publishing..."
lxc stop $NAME
lxc publish $NAME --alias steno-base
lxc delete $NAME

