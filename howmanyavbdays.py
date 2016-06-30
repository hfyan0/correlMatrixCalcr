#!/usr/local/bin/python
import sys

CNT=0
with open(sys.argv[1],'r') as f:
    for line in f:
        linecsv = line.strip().split(",")
        if linecsv[0] <= sys.argv[2]:
            CNT=CNT+1


print CNT
