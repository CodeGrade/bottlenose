#!/usr/bin/perl
use 5.16.0;

use Test::Simple tests => 1;

my $hello = `./hello`;
chomp $hello;
ok($hello eq "Hi!", "output correct");

