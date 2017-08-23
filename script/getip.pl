#!/usr/bin/perl
use 5.18.0;
use warnings FATAL => 'all';

my $route = `ip route get 8.8.8.8`;
$route =~ /dev\s+(\w+?)\s/ or die;
my $iface = $1;

my $addrs = `ip a show dev "$iface"`;
$addrs =~ /inet\s+(\d+\.\d+\.\d+\.\d+)[\s|\/]/ or die;

my $ip = $1;
say "$ip";
