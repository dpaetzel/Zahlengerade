#!/usr/bin/env sh

distFolder="../Zahlengerade-$1"

rm -rf $distFolder

mkdir $distFolder

cp \
    example-input.yaml\
    Zahlengerade.bat\
    README.org\
    LICENSE\
    .stack-work/install/*/lts*/*/bin/zahlengerade.exe\
    $distFolder
