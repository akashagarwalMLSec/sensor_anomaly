# -*- coding: utf-8 -*-
"""
Created on Fri Dec  7 19:51:18 2018

@author: moniy
"""

import math
import os
import random
import re
import sys



if __name__ == '__main__':

    N = int(input())


    if(N % 2 == 0):
        print("Even")
        if(N > 1 and N < 6):
            print("Not Weird-1,6")
        else:
            if(N > 7 and N < 21):
                print("Weird-6,20")
            else:
                print("Not Weird,>21")
    else:
        print("Weird-Odd")