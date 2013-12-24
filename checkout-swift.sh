#!/bin/sh
set -e
COG_SRC=$1
SWIFT_PATCH=$2
SWIFT_TAG=$3
COG_TAG=$4

if [ "$COG_SRC" = "" ] ; then
    COG_SRC=cogsrc
fi

if [ "$SWIFT_TAG" = "" ] ; then
    echo Need to provide Swift tag
    exit 1
fi

if [ "$COG_TAG" = "" ] ; then
    echo Need to provide CoG tag
    exit 1
fi

if [ -d ${COG_SRC} ] ; then rm -rf ${COG_SRC}; fi

echo Checking out CoG source

svn checkout https://cogkit.svn.sourceforge.net/svnroot/cogkit/${COG_TAG} ${COG_SRC}
echo Checking out Swift source 
CURR=`pwd`
cd ${COG_SRC}/modules
svn checkout https://svn.ci.uchicago.edu/svn/vdl2/${SWIFT_TAG} swift
cd "$CURR"

patch -d ${COG_SRC} -p0 < $SWIFT_PATCH
cd ${COG_SRC} 
xargs -d '\n' -n 1 --arg-file=../${SWIFT_PATCH}.deleted rm -rf
cd "$CURR"
