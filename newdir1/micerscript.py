# An example for using these scripts. Will call degas2 executables as needed, so ensure that they are in your $PATH
# Also, make sure degas2/scripts is in your $PYTHONPATH
# To use as-is, run in your working directory. Otherwise, just use this as a template to copy into your own scripts
import os
import subprocess
import dg2d
import problem
import source
import defineback
import postprocess
import numpy as np
import scipy.interpolate as interpolate
import netCDF4 as nc
import sys

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# DUMMY DATA
# POPULATE THESE ARRAYS WITH APPROPRIATE PLASMA DATA
# Read tables, interpolate, etc. to build these tables.
#
# This script follows the convention that psi is normalized (0 = magnetic axis, 1 = separatrix)
psi_data = [0.0,1.0,2.0]
# Units of m^-3
ne_data = [1.0e20,1.0e19,1.0e18]
# Units of eV 
Te_data = [10000.0,100.0,1.0]
Ti_data = [10000.0,200.0,10.0]




'''
#reading in tables from fidasim 
Ti_data_or = []
fileIon = open('ionProfile.txt')
contentIon = fileIon.read()
stringsIon = contentIon.split()
for i in stringsIon: Ti_data_or.append(float(i))


multi  = 2
Ti_data = [x* multi for x in Ti_data_or]



Te_data = []
fileElectron = open('electronProfile.txt')
contentElectron = fileElectron.read()
stringsElectron = contentElectron.split()
for i in stringsElectron: Te_data.append(float(i))

ne_data = []
filene = open('neProfile.txt')
contentne = filene.read()
stringsne = contentne.split()
for i in stringsne: ne_data.append(float(i))

#note difference in psi written in scrip and rho imported from fidasim
psi_data = []
filerho = open('rhoVals.txt')
contentrho = filerho.read()
stringsrho = contentrho.split()
for i in stringsrho: psi_data.append(float(i))


omega_data = []
fileO= open('omegaProfile.txt')
contentO = fileO.read()
stringsO = contentO.split()
for i in stringsO: omega_data.append(float(i))

'''
#import matplotlib.pyplot as plt
#Fig, ax = plt.subplots()
#ax.plot(psi_data, Ti_data)
#ax.plot(psi_data, Te_data)
#ax.plot(psi_data, ne_data)
#plt.show()

#1/0

#print(omega_data)


# Units of rad/s. Gets converted to a toroidal velocity = R*omega
omega_data = [1000.0,1000.0,1000.0]
#
# File containing magnetic equilibrium data.
# Just an example (not for distribution). Replace with your own data.
#eqdsk_file = "g186414_03700_FwdBt.g"
eqdsk_file = "g189598_04200.g"
#
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Path to your DEGAS2 build directory
d2path = "$HOME/degasFresh/degas2/build_newviews"

# The number of flight samples
Nsample = 1000000
print('number of flight samples is ', Nsample)

# Recycling coefficient
recyc = 0.98
print('Recycling coefficient is ', recyc)

# Wall Material
material = "C"
print('Wall material is ',material)

# Wall temperature in Kelvin

# 
walltemp = 300.0
print('Wall temperature is ', walltemp)



# When refining mesh, this is the largest segment permitted along the wall
dlim_max = 0.02

p = problem.genStdProblem("C-D")
print("Running problemsetup...\n")
subprocess.run(d2path+"/bin/problemsetup",shell=True)

# dlim_max gives the maximum distance defined along limiter for triangulation, in meters. Roughly sets spatial resolution.

#George will change this to generate rho instead of psifunc
psifunc,nodes = dg2d.generateGeometryFromEFITfile(eqdsk_file,material,recyc_coef=recyc,Twall=walltemp,dlim_max=0.02,clockwise=True)

# At this point, the g file was used to generate psifunc, generating normalized psi as a function of R,Z

print("Running definegeometry2d...\n")
subprocess.run(d2path+"/bin/definegeometry2d dg2d.in",shell=True)

defineback.generate_plasma_file_through_psi(ne_data,Te_data,Ti_data,psifunc,psi_data=psi_data,rot_data=omega_data)

# This will produce a "puff" type source at a temperature of 300K uniformly around limiter at a strength of
# 1.0e24 nuclei per m^2 per s. Using Nsample flights and treating as a 
#sgroup = source.Source(Nsample,"puff","D",rootspecies="D",pufftemp=300.0,strength=1.0e24,stratum=3,segment="112", specify_flux=True)
# This treats the source as if it's recycling, same flux as above, energy distribution of produced neturals
# are determined by ions near the wall and the recycling properies of the PFC.
sgroup = source.Source(Nsample,"plate","D",rootspecies="D+",strength=1.0e24,stratum=3,segment="*")

source.write_db_input([sgroup])

print("Running defineback...\n")
subprocess.run(d2path+"/bin/defineback db.in",shell=True)

# Use supplied tally.in file
print("Running tallysetup...\n")
subprocess.run(d2path+"/bin/tallysetup",shell=True)

# You may or may not have compiled flighttest with MPI. Obey that here
print("Running flighttest...\n")
subprocess.run("mpirun -np 128 "+d2path+"/bin/flighttest",shell=True)
#subprocess.run(d2path+"/bin/flighttest",shell=True)


# Open the resulting output_ucd.silo with ParaView
subprocess.run(d2path+"/bin/ucd_plot polygons.nc",shell=True)

# How to query an output tally
# Look up the name given to what you're looking for in tally.in
# Pass this string to the function below.

spectrum,spectrum_errs = postprocess.get_output("MICER spectrum",with_err=True)

signal,signal_errs = postprocess.get_output("MICER total",with_err=True)

nn = postprocess.get_output("neutral density")


d=nc.Dataset("geometry.nc")
j=-1
for i in range(0,len(d["detector_name"])):
   # Use the same name here as in tally.in:
   if str(nc.chartostring(d["detector_name"][i])).strip() == "Halpha spectrum":
      j=i
if j == -1:
   print("ERROR: could not find the detector name")
   sys.exit(1)
min = d["detector_min"][j]
delta = d["detector_delta"][j]
num = d["detector_tab_index"][j]

print('wavelength range', min, min + delta*200)
print('number of bins', num)




#ax.plot(spectrum)
#plt.show()



