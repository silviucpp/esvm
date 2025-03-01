#!/usr/bin/env bash

ROOT=$(pwd)
DEPS_LOCATION=_build/deps
OS=$(uname -s)
KERNEL=$(echo $(lsb_release -ds 2>/dev/null || cat /etc/*release 2>/dev/null | head -n1 | awk '{print $1;}') | awk '{print $1;}')
CPUS=`getconf _NPROCESSORS_ONLN 2>/dev/null || sysctl -n hw.ncpu`

# libsvm

LIBSVM_DESTINATION=libsvm
LIBSVM_REPO=https://github.com/cjlin1/libsvm.git
LIBSVM_BRANCH=master
LIBSVM_TAG=v335
LIBSVM_SUCCESS=svm.cpp

fail_check()
{
    "$@"
    local status=$?
    if [ $status -ne 0 ]; then
        echo "error with $1" >&2
        exit 1
    fi
}

CheckoutLib()
{
    if [ -f "$DEPS_LOCATION/$4/$5" ]; then
        echo "$4 fork already exist. delete $DEPS_LOCATION/$4 for a fresh checkout ..."
    else
        #repo rev branch destination

        echo "repo=$1 tag=$2 branch=$3"

        mkdir -p $DEPS_LOCATION
        pushd $DEPS_LOCATION

        if [ ! -d "$4" ]; then
            fail_check git clone -b $3 $1 $4
        fi

        pushd $4
        fail_check git checkout $2
        CopyLibrary $4
        popd
        popd
    fi
}

CopyLibrary()
{
    fail_check cp svm.cpp ../../../c_src/svm.cc
    fail_check cp svm.h ../../../c_src/svm.h
}

CheckoutLib $LIBSVM_REPO $LIBSVM_TAG $LIBSVM_BRANCH $LIBSVM_DESTINATION $LIBSVM_SUCCESS
