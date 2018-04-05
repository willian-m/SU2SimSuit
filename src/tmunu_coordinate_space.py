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

x=3
y=3
z=3
t=3

xSliceData=[float(data_array[x + y*Ns+ z*Ns**2 + t*Ns**3]) for t in range(Ns)]
xSliceError=[float(error_array[x + y*Ns+ z*Ns**2 + t*Ns**3]) for t in range(Ns)]

plt.title(r'$\langle T_{01}(4,4,4,t)T_{02}(0) \rangle$')
plt.xlabel(r'$t$')
plt.errorbar([x for x in range(Ns)],xSliceData,yerr=xSliceError,fmt='o',capsize=4) 
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
plt.errorbar(x_squared,data,yerr=error,fmt='o',capsize=4,elinewidth=1)
plt.show()
plt.title(r'$\langle T_{01}(x^2)T_{02}(0) \rangle$ - Error bars ommited')
plt.xlabel(r'$x^2$')
plt.plot(x_squared,data,'o',ms=1)
plt.show()