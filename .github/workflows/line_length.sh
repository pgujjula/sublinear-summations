#!/bin/bash

# SPDX-FileCopyrightText: Copyright Preetham Gujjula
# SPDX-License-Identifier: BSD-3-Clause

git ls-files |  \
    grep -v '^LICENSES/BSD-3-Clause.txt$\|^sublinear-summations.cabal$' | \
    xargs grep -n '.\{81\}' |
    grep .

if [ "$?" -eq "0" ]
then
  echo "Lines longer than 80 characters found"
  exit 0
else
  exit 0
fi
