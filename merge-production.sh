#!/bin/sh -ve

. ./merge.inc.sh
update master
merge deploy-production master
end
