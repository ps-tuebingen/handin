#!/bin/sh -ve

. ./merge.inc.sh
update deploy-staging
merge deploy-production deploy-staging
end
