#!/bin/sh -ve

. ./merge.inc.sh
setup
merge staging
merge production
end
