#!/bin/bash

env | grep KAHAWAI
set -e

mkdir -p extract
export PGPASSWORD=${KAHAWAI_DBPASS}
psql -h ${KAHAWAI_DBHOST} -U ${KAHAWAI_DBUSER} ${KAHAWAI_DBNAME} --set ON_ERROR_STOP=1 -f data.sql

make -f make

if test -n "$(find . -maxdepth 1 -name '*.RData' -print -quit)"
then
    cp *.RData ${OUTPUTDIR}/.
fi

if test -n "$(find . -maxdepth 1 -name '*.pdf' -print -quit)"
then
    cp *.pdf ${OUTPUTDIR}/.
fi

if test -n "$(find . -maxdepth 1 -name '*.csv' -print -quit)"
then
    cp *.csv ${OUTPUTDIR}/.
fi

if test -n "$(find . -maxdepth 1 -name '*.png' -print -quit)"
then
    cp *.png ${OUTPUTDIR}/.
fi
