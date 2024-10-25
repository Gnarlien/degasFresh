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

import matplotlib.pyplot as plt
import numpy as np



import netCDF4 as nc
import sys
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
#max = d["detector_max"][j]
delta = d["detector_delta"][j]
num = d["detector_tab_index"][j]
maxi = min+delta*num

print('detector name is', d["detector_name"])


print('wavelength range', min, min + delta*num)
print('number of bins', num)

print(num)



spectrum,spectrum_errs = postprocess.get_output("MICER spectrum",with_err=True)

#wavelength min 6556, max = 6566

#wavelength = np.linspace(min,min+delta, len(spectrum[1]))
wavelength = np.linspace(min,maxi,len(spectrum[1]))

signal,signal_errs = postprocess.get_output("MICER total",with_err=True)

nn = postprocess.get_output("neutral density")


import matplotlib.pyplot as plt


#fig, ax = plt.subplots(4,4)
fig, ax = plt.subplots()
print(len(spectrum[1]))
for i in np.arange(0,len(spectrum)):
   plt.plot(wavelength,spectrum[i,:])
plt.show()

#ax.legend()
#print(shape(nn))
#ax.plot(nn)
#plt.show()
