#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';
use Test::More tests => 2;
BEGIN { use_ok 'PHP::Strings', ':bin2hex' };

# Good inputs
{
    eval { bin2hex(q{'x'} ) };
    like( $@, qr/Function `bin2hex' not implemented/, "Not implemented." );
}
