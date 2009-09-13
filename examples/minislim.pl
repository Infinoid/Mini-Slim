#!/usr/bin/perl

use strict;
use warnings;
use blib;
use Mini::Slim;

return Mini::Slim->new(@ARGV)->server();
