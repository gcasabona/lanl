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

    f1 = open("n1000.dat", 'r')
    f1 = f1.readlines()
    f2 = open("n2000.dat", 'r')
    f2 = f2.readlines()
    f4 = open("n4000.dat", 'r')
    f4 = f4.readlines()


#    print(f1)
    #print(f2)
    #print(f4)

    radius1 = []
    mass1 = []
    rho1 = []
    
    radius2 = []
    mass2 = []
    rho2 = []
    
    radius4 = []
    mass4 = []
    rho4 = []
    
    radiuserr = []
    masserr = []
    rhoerr = []
    
    for line in f1:
        if line.strip().startswith('#'):
            if (verbose == 1):
                print (line)
        else:

                
                lst = line.split()


                radius1.append (float (lst[0]))
                mass1.append   (float (lst[1]))
                rho1.append    (float (lst[2]))

    radiusarr1 = np.array(radius1)
    massarr1 = np.array(mass1)
    rhoarr1 = np.array(rho1)

    radiusarr1 = radiusarr1[0::]
    massarr1 = massarr1[0::]
    rhoarr1 = rhoarr1[0::]
#    print('Radius', radiusarr1)
#    print('Mass', massarr1)
    print('Rho1', rhoarr1)
    print(len(rhoarr1))

    for line in f2:
        if line.strip().startswith('#'):
            if (verbose == 1):
                print (line)
        else:

                lst = line.split()


                radius2.append (float (lst[0]))
                mass2.append   (float (lst[1]))
                rho2.append    (float (lst[2]))

     #           print(rho2)


    radiusarr2 = np.array(radius2)
    massarr2 = np.array(mass2)
    rhoarr2 = np.array(rho2)

    radiusarr2 = radiusarr2[0::2]
    massarr2 = massarr2[0::2]
    rhoarr2 = rhoarr2[0::2]

    print('rho2', rhoarr2)
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


                
    radiusarr4 = np.array(radius4)
    massarr4 = np.array(mass4)
    rhoarr4 = np.array(rho4)
    
    radiusarr4 = radiusarr4[0::4]
    massarr4 = massarr4[0::4]
    rhoarr4 = rhoarr4[0::4]
    
    print('rho4', rhoarr4)
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
