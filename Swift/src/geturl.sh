#!/bin/sh
# Fetch a file from a url, trying wget, curl and fetch

WGET=${WGET:-wget}
CURL=${CURL:-curl}
FETCH=${FETCH:-fetch}

URL="$1"

if which ${WGET} > /dev/null ; then
    ${WGET} "$URL" || die "Could not download $URL with $WGET"
elif which ${CURL} > /dev/null ; then
    ${CURL} -O "$URL" || die "Could not download $URL with $CURL"
elif which ${FETCH} > /dev/null ; then
    ${FETCH} "$URL" || die "Could not download $URL with $FETCH"
else
    echo Could not find a utility to fetch from URLs.  Tried to locate echo wget, curl 
    echo and fetch. Utility must be on path, or specified with WGET, CURL or FETCH 
    echo environment variable.
    exit 1
fi
