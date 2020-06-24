# -*- coding: utf-8 -*-
"""
Created on Sun Dec  9 00:11:12 2018

@author: moniy
"""
class Person:
    def __init__(self,initialAge):
        if(initialAge < 0):
            print("Age is not valid,setting to 0")
            self.age=0
        else:
            self.age=initialAge

    def amIOld(self):
        if(self.age < 13):
            print("You are young.")
        elif(self.age > 12 and self.age < 18):
            print("You are a teenager.")
        else:
            print("You are old.")

    def yearPasses(self):
        self.age = self.age+1
        print(self.age)


age = int(input())
p = Person(age)
p.amIOld()
for j in range(0, 3):
    p.yearPasses()
p.amIOld()
print("")