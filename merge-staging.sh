#!/bin/sh -ve

. ./merge.inc.sh
update master
merge deploy-staging master
end
