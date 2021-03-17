#!/bin/bash
for i in testfiles/*.txt; do cat $i | ./Main ; done