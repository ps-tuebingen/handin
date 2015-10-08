#!/bin/sh -v
git checkout deploy-staging
git merge master --ff-only
git checkout deploy-production
git merge master --ff-only
git checkout master
