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

    filename = "rho_v_mass.dat"
    f = open(filename, 'r')

    radius = []
    mass = []
    rho = []
    press = []

    for line in f:
        if line.strip().startswith('#'):
            if (verbose == 1):
                print (line)
        else:

                lst = line.split()

                radius.append (float (lst[0]))
                mass.append   (float (lst[1]))
                #rho.append    (float (lst[2]))
                #press.append  (float (lst[3]))

    radiusarr = np.array(radius)
    #radiusarr.sort()
    massarr = np.array(mass)
    #massarr.sort()
    #rhoarr = np.array(rho)
    #pressarr = np.array(press)



    plt.plot(radiusarr, massarr)
    plt.ylabel('mass')
    plt.xlabel('radius')
    plt.savefig('rho_v_mass.png')
 
