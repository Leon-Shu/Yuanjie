# -*- coding: utf-8 -*-
"""
Created on Sat Dec  5 20:29:51 2020
@author: jjieleo

For process the abaqus-fortran result.
1681 is the number of node.
(Total_Cycle_Num-1) is determined by the number of cycles required
Calling a Matlab program to plot a figure


"""
import matlab.engine
import time
eng = matlab.engine.start_matlab()

starttime = time.time()

fname = 'Out-Surfv.txt'
with open(fname, 'rb') as f:
    lines = f.readlines()

Total_Cycle_Num = len(lines)/1681
Coord = []
X = []
Y = []
Z = []

for i in range(int(Total_Cycle_Num-1)*1681,len(lines)):
    Coord.append(''.join(lines[i].decode()))
for i in range (0,len(Coord)):
    temp = Coord[i].split()
    X.append(temp[1])
    Y.append(temp[2])
    Z.append(temp[4])
    
del lines

f = open('TEMP.txt', 'w')
for i in range (0,len(X)):
    f.write(str(X[i])+' '+str(Y[i])+' '+str(Z[i])+'\n')
f.close()

fig = eng.mesh3D(1)

endtime = time.time()
print(endtime-starttime)
#fig.mesh3D(1)

    
