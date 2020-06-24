# -*- coding: utf-8 -*-
"""
Created on Mon Dec 10 17:39:45 2018

@author: moniy
"""


# Enter your code here. Read input from STDIN. Print output to STDOUT
def printEven(string):
    even = ""
    for i in range(0,len(string),2):
        even = even + string[i]
    return(even)

def printOdd(string):
    odd = ""
    for i in range(1,len(string),2):
        odd = odd + string[i]
    return(odd)

print("Enter the number of test cases")
n = int(input())
print("Enter the test cases")
for i in range(n):
    string = str(input())
    even = printEven(string)
    odd = printOdd(string)
    print(even,odd)


a = 2437
b = 875
x = a
y = b
while(x != y):
    if(x > y):
        x = x-y
    if(x < y):
        y = y-x
print(x)




n=7
import math as m
def combi(s,c1,c3):
    if(c1<c3):
        a=m.factorial(c1)
        p = 1
        for x in range(s-c1,n+1):
            p=p*x
        print(p/a)
    if(c1>=c3):
        a=m.factorial(c3)
        p = 1
        for x in range(s-c3,n+1):
            p=p*x
        print(p/a)

def numberOfWays(n):
    c3=0
    cnt=0
    while(True):
        c1=n-c3*3
        print combi(c1+c3,c1,c3)
#        cnt = cnt + combi(n,c1,c3)
        #cnt=cnt+m.factorial(c1+c3)/m.factorial(c1)/m.factorial(c3)
        c3+=1
        if(c3*3>n):
            break;
    print(cnt)

combi(7,4,1)
numberOfWays(n)