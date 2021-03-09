#!/usr/bin/env bash
for i in testfiles/*.txt; do cat $i | ./Main.o ; done
