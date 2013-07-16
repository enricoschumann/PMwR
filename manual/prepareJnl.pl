## -*- truncate-lines: t; -*-
## Time-stamp: <2013-07-16 11:47:36 CEST (es)>

use warnings;
use strict;

while (<>) {
        print "$_" if /^\s*\|/;
}
