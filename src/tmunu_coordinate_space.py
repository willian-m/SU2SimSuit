#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr  5 16:03:55 2018

@author: willian
"""
from numpy import fromfile as FF
from matplotlib import pyplot as plt

data_file_path="./output/StatisticalAverage12.dat"
error_file_path="./output/StatisticalError12.dat"

Ns=8
Nt=8

data_array=FF(data_file_path,
              dtype=float,
              count=Nt*Ns**3)
error_array=FF(error_file_path,
               dtype=float,
               count=Nt*Ns**3)

#Plot of a single slice in direction t. The parameters bellow determines where
#we will slice the lattice.
x=3
y=3
z=3
t=3

xSliceData=[float(data_array[x + y*Ns+ z*Ns**2 + t*Ns**3]) for t in range(Ns)]
xSliceError=[float(error_array[x + y*Ns+ z*Ns**2 + t*Ns**3]) for t in range(Ns)]


#Now we plot in function of x^2 = x4^2 + x1^2 + x2^2 + x3^2
plt.title(r'$\langle T_{01}(4,4,4,t)T_{02}(0) \rangle$')
plt.xlabel(r'$t$')
plt.errorbar([x for x in range(Ns)],xSliceData,yerr=xSliceError,fmt='o',capsize=10) 
plt.show()

x_squared=[]
data=[]
error=[]
for t in range(Nt):
    for z in range(Ns):
        for y in range(Ns):
            for x in range(Ns):
                x_squared.append(x**2+y**2+z**2+t**2)
                data.append(data_array[x + y*Ns+ z*Ns**2 + t*Ns**3])
                error.append(error_array[x + y*Ns+ z*Ns**2 + t*Ns**3])

plt.title(r'$\langle T_{01}(x^2)T_{02}(0) \rangle$')
plt.xlabel(r'$x^2$')
plt.errorbar(x_squared,data,yerr=error,fmt='o',capsize=4,elinewidth=1,ms=2)
plt.show()

#We will average points with the same x^2
#Lattice artifacts are expected to be present on these plots
x2 = []
avrgd_data=[]
avrgd_error=[]
data_array=data_array.tolist()
error_array=error_array.tolist()

for ref in x_squared:
    n = x_squared.count(ref)
    d=0
    e=0
    for i in range(n):
        ind = x_squared.index(ref)
        x_squared.pop(ind)
        d = d + data_array.pop(ind)
        e = e + error_array.pop(ind)
    avrgd_data.append(d/n)
    avrgd_error.append(e/n)
    x2.append(ref)


plt.title(r'$\langle T_{01}(x^2)T_{02}(0) \rangle_{x^2}$')
plt.xlabel(r'$x^2$')
plt.errorbar(x2,avrgd_data,yerr=avrgd_error,fmt='o',capsize=4,elinewidth=1,ms=3)
plt.show()