## -*- truncate-lines: t; -*-
## Time-stamp: <2013-07-13 14:27:24 CEST (es)>

use warnings;
use strict;

while (<>) {
    if (/^\s*\|/){
        print "$_";
    }
}
