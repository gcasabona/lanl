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

    filename = "N4000.dat"
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
                rho.append    (float (lst[2]))
                press.append  (float (lst[3]))

    radiusarr = np.array(radius)
    massarr = np.array(mass)
    rhoarr = np.array(rho)
    pressarr = np.array(press)



    fig, axis_1 = plt.subplots()
    axis_2 = axis_1.twinx()

    color = 'tab:blue'
    axis_1.set_xlabel('radius (cm)', fontsize = 12)
    #axis_1.set_ylabel('t_eddy/t_burn', color = color) 
    axis_1.set_ylabel('mass (g)', fontsize = 12 ,color = color)
    axis_1.plot(radiusarr, massarr ,color=color)
    axis_1.tick_params(axis='y',labelcolor = color)
    #axis_1.legend(loc = 'center left')
    color = 'tab:red'
    #axis_2.set_ylabel('c12_abundance', color = color)
    axis_2.set_ylabel('density (g cm^3)',fontsize =12, color = color)
    axis_2.plot(radiusarr, rhoarr, color = color)
    axis_2.tick_params(axis='y',labelcolor = color)
    #axis_2.legend(loc = 'center left')
    #fig.savefig('local_timescale_vs_local_c12.png',format = 'png', dpi = 1000)

    plt.savefig('radius_mass_rho.png')

    fig, axis_1 = plt.subplots()
    axis_2 = axis_1.twinx()

    color = 'tab:blue'
    axis_1.set_xlabel('radius (cm)', fontsize = 12)
    #axis_1.set_ylabel('t_eddy/t_burn', color = color) 
    axis_1.set_ylabel('mass (g)', fontsize = 12 ,color = color)
    axis_1.plot(radiusarr, massarr ,color=color)
    axis_1.tick_params(axis='y',labelcolor = color)
    #axis_1.legend(loc = 'center left')
    color = 'tab:red'
    #axis_2.set_ylabel('c12_abundance', color = color)
    axis_2.set_ylabel('pressure (dyne cm$^{-2})$',fontsize =12, color = color)
    axis_2.plot(radiusarr, pressarr, color = color)
    axis_2.tick_params(axis='y',labelcolor = color)
    #axis_2.legend(loc = 'center left')
    #fig.savefig('local_timescale_vs_local_c12.png',format = 'png', dpi = 1000)

    plt.savefig('radius_mass_pressure.png')
