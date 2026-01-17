#!/bin/sh

bname=hs-fs-filter

bin=$( cabal exec -- which "${bname}" )

ex1_select_files(){
    echo -- printing files only --
    ls | "${bin}" --only-files
}

ex2_select_dirs(){
    echo -- printing dirs only --
    ls | "${bin}" --only-dirs
}

ex1_select_files
echo
ex2_select_dirs
