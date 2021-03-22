#!/usr/bin/env bash
for i in testfiles/*.hs; do cat $i | cabal v2-run ; done