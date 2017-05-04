#!/bin/bash

lxc image delete bn-base

lxc launch ubuntu:16.04 bn-base

sleep 10
lxc file push install-reqs.sh bn-base/root/install-reqs.sh
lxc exec bn-base -- bash /root/install-reqs.sh
lxc stop bn-base --force

lxc publish bn-base --alias bn-base

lxc delete bn-base
