import numpy as np
import matplotlib.pyplot as plt
import sys
import os
import argparse
import string
import scipy
from scipy import interpolate
parser = argparse.ArgumentParser(description='Analyze StirTurbHelm output.')
parser.add_argument('directory', metavar='N', type=str, nargs='+',
                   help='directory to analyze')

args = parser.parse_args()
directorylist = args.directory

# Verbose flag ( = 1 for verbose output, 0 if not)

verbose = 1

for directory in directorylist:

    directory = os.path.join(directory, '')

    filename = "sfho_0.1MeV_beta.txt"
    f = open(filename, 'r')

    press = []
    rho = []
    efrac = []

    for line in f:
        if line.strip().startswith('#'):
            if (verbose == 1):
                print (line)
        else:

                lst = line.split()

                press.append  (float (lst[0]))
                rho.append    (float (lst[1]))
                efrac.append  (float (lst[2]))


    pressarr = np.array(press)*1.602e33
    rhoarr = 10**np.array(rho)


    #file = open("eos_cgs.txt", "w")
    #for index in range(len(press)):
    #    file.write(str(press[index]) + "  " + str(rho[index]) + "\n")
    #file.close()

    #data = np.column_stack([press, rho])
    #np.savetxt("/eos_cgs.txt", data) 


    #col_format = "{:<5}" * 2 + "\n"

    #with open("eos_cgs.txt", 'w') as pf:
    #    for x in zip(press, rho):
    #        os.write(col_format.format(*x))

    i1 = interpolate.interp1d(rhoarr, pressarr, kind='linear')
    #i2 = interpolate.interp1d(pressarr, rhoarr, kind='quadratic')
    #i3 = interpolate.interp1d(pressarr, rhoarr, kind='cubic')
    #pressint = np.linspace(pressarr[0], pressarr[-1], num=3000)
    #rhoint1 = i1(pressarr)
    #rhoint2 = i2(pressarr)
    #rhoint3 = i3(pressarr)
    rhoint = np.linspace(1e13, 1e14, 1000)
    pressint = i1(rhoint)

    plt.plot(rhoint, pressint, 'b-')
    plt.xlim(1e13, 1e14)
    plt.ylim(pressint[0], pressint[-1])
    plt.plot(rhoarr, pressarr, 'ro') 
    #plt.loglog(pressarr, rhoint2, 'g|')
    #plt.loglog(pressarr, rhoint3, 'c|')
    #plt.legend(
    plt.savefig('press_v_rho.png')
