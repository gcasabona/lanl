import numpy as np
import matplotlib.pyplot as plt
import sys
import os
import argparse
import string

parser = argparse.ArgumentParser(description='Analyze StirTurbHelm output.')
parser.add_argument('directory', metavar='N', type=str, nargs='+',
                   help='directory to analyze')
#parser.add_argument('--sum', dest='accumulate', action='store_const',
#                   const=sum, default=max,
#                   help='sum the integers (default: find the max)')

args = parser.parse_args()
directorylist = args.directory

# Verbose flag ( = 1 for verbose output, 0 if not)

verbose = 1

for directory in directorylist:

    directory = os.path.join(directory, '')

    f1 = open("N1000.dat", 'r')
    f1 = f1.readlines()
    f2 = open("N2000.dat", 'r')
    f2 = f2.readlines()
    f4 = open("N4000.dat", 'r')
    f4 = f4.readlines()


    radius1 = []
    mass1 = []
    rho1 = []
    press1 = []
    
    radius2 = []
    mass2 = []
    rho2 = []
    press2 = []
    
    radius4 = []
    mass4 = []
    rho4 = []
    press4 = []
    
    radius16 = []
    mass16 = []
    rho16 = []
    press16 = []

    radiuserr = []
    masserr = []
    rhoerr = []
    presserr = []
    
    for line in f1:
        if line.strip().startswith('#'):
            if (verbose == 1):
                print (line)
        else:

                
                lst = line.split()


                radius1.append (float (lst[0]))
                mass1.append   (float (lst[1]))
                rho1.append    (float (lst[2]))
                press1.append  (float (lst[3]))

    radiusarr1 = np.array(radius1)
    massarr1 = np.array(mass1)
    rhoarr1 = np.array(rho1)
    pressarr1 = np.array(press1)

    radiusarr1 = radiusarr1[1::]
    massarr1 = massarr1[1::]
    rhoarr1 = rhoarr1[1::]
    pressarr1 = pressarr1[1::]
#    print('Radius', radiusarr1)
#    print('Mass', massarr1)
    print('Rho', rhoarr1)
    print(len(rhoarr1))
#    print('Pressure', pressarr1)

    for line in f2:
        if line.strip().startswith('#'):
            if (verbose == 1):
                print (line)
        else:

                lst = line.split()


                radius2.append (float (lst[0]))
                mass2.append   (float (lst[1]))
                rho2.append    (float (lst[2]))
                press2.append  (float (lst[3]))

     #           print(rho2)


    radiusarr2 = np.array(radius2)
    massarr2 = np.array(mass2)
    rhoarr2 = np.array(rho2)
    pressarr2 = np.array(press2)

    radiusarr2 = radiusarr2[1::2]
    massarr2 = massarr2[1::2]
    rhoarr2 = rhoarr2[1::2]
    pressarr2 = pressarr2[1::2]

    print(rhoarr2)
    print(len(rhoarr2))

    for line in f4:
        if line.strip().startswith('#'):
            if (verbose == 1):
                print (line)
        else:

                lst = line.split()


                radius4.append (float (lst[0]))
                mass4.append   (float (lst[1]))
                rho4.append    (float (lst[2]))
                press4.append  (float (lst[3]))


                
    radiusarr4 = np.array(radius4)
    massarr4 = np.array(mass4)
    rhoarr4 = np.array(rho4)
    pressarr4 = np.array(press4)
    
    radiusarr4 = radiusarr4[1::4]
    massarr4 = massarr4[1::4]
    rhoarr4 = rhoarr4[1::4]
    pressarr4 = pressarr4[1::4]
    
    print(rhoarr4)
    print(len(rhoarr4))

    
    np.seterr(invalid='ignore')
    rhoerr = np.log2((rhoarr1 - rhoarr2)/(rhoarr2 - rhoarr4))
   
    #print(rhoerr)
    #print(radiusarr2)
    #print(rhoarr2)

    #print(radiusarr4)
    #print(rhoarr4)
    plt.semilogy(radiusarr1, rhoerr)
    plt.savefig('conv_rho.png')
