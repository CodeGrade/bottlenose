#!/usr/bin/perl
use 5.16.0;
use strict;
use warnings FATAL => 'all';

use Test::Simple tests => 2;

system("rm -f hello");
system("gcc -o hello hello.c");
my $rv = $? >> 8;

ok($rv == 0, "Compile OK");

my $hello = `./hello`;
chomp $hello;

ok($hello eq "Hello, World!", "Hello OK");

system("rm -f hello");

