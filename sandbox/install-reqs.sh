#!/bin/bash

echo "" > /etc/apt/apt.conf.d/20auto-upgrades

dpkg --add-architecture i386
apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
add-apt-repository -y ppa:avsm/ppa

apt-get update -y
apt-get install -y openjdk-8-jdk gradle
apt-get install -y ruby2.3
apt-get install -y build-essential libarchive-zip-perl
#apt-get install -y --allow-unauthenticated sbt
apt-get install -y clang valgrind wamerican libbsd-dev libgmp-dev
apt-get install -y libfuse-dev pkg-config
apt-get install -y ocaml ocaml-native-compilers camlp4-extra opam valgrind \
        nasm clang m4 gcc-multilib g++-multilib libc6-dev-i386 libc6-dev \
        libc6 libc6-dbg libc6-dbg:i386
apt-get upgrade -y

adduser --disabled-password --gecos "" student

su student -c "cd ~student ; opam init -y"
su student -c "echo \". /home/student/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true\" >> ~student/.profile"
su student -c "cd ~student ; opam install -y ounit extlib ocamlfind"

