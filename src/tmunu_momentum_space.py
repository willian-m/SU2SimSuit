#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from numpy import fromfile as FF
from matplotlib import pyplot as plt

data_file_path="./output/StatisticalAverage12.dat"
error_file_path="./output/StatisticalError12.dat"
Ns=5
Nt=5

data_array=FF(data_file_path,
              dtype=complex,
              count=Nt*Ns**3)
error_array=FF(error_file_path,
               dtype=complex,
               count=Nt*Ns**3)

x=0
y=0
z=0
t=0

for t in range(Nt):
    xSliceData=[float(data_array[x + y*Ns+ z*Ns**2 + t*Ns**3]) for t in range(Ns)]
    xSliceError=[float(error_array[x + y*Ns+ z*Ns**2 + t*Ns**3]) for t in range(Ns)]
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

plt.errorbar(x_squared,data,yerr=error,fmt='o',capsize=4)