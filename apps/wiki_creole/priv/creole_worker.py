#!/usr/bin/env python

import sys
import creoleparser

COMMAND_BREAK = "------wriaki-creole-break------"
Acc = ""

while 1:
    L = sys.stdin.readline()
    if L.strip() == COMMAND_BREAK:
        H = creoleparser.text2html(Acc)
        print H
        print COMMAND_BREAK
        sys.stdout.flush()
        Acc = ""
    elif L == "":
        break
    else:
        Acc += L

