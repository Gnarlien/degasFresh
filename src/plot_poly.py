#
# This routine reads a set of polygons in a single file,
# named on the command line and then plots them for inspection.
#
# E.g.:  python3 ./plot_poly.py polygons.txt
#
# Each polygon specification begins with a line:
# Polygon    label
# where "label" may be a number or string (no spaces) used
# to identify it.  The R, Z coordinates of each point in the
# polygon appear, space delimited, on subsequent lines with
# one point per line.
#
# As currently written no other blank or comment lines
# are allowed.
#
# Clicking on the plot will cause the label for the
# polygon at that location and the R, Z coordinates
# to be printed to stdout.  If the cursor extends over
# more than one polygon, multiple labels will be output.
#
import sys
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.collections import PolyCollection
import matplotlib.cm as cm
import matplotlib as mpl

infile = sys.argv[1]
polyfile = open(infile,"rt")
# At this point, verts is just an empty list.
verts=[]
labels=[]
npoints=0
npoly=-1
color_index=[]
for line in polyfile:
    if line.startswith(' Polygon'):
        labels.append(line.split()[1])
        if (npoints > 0):
            verts.append(poly)
        npoints=0
        npoly+=1
        #
        # This is the color function used in geomtesta
        # etc.; note that % is the modulo function.
        #
        color_index.append(3+(97*npoly % (255-3)))
    else:
        x=line.split()[0]
        y=line.split()[1]
        if npoints==0:
            # poly is an (npoints,2) ndarray object.
            poly=np.array([[x,y]])  
        else:
            poly=np.append(poly,[[x,y]],axis=0)
        npoints+=1
verts.append(poly)
#
# This sets up a (nearly trivial) normalizing function
# that converts the familiar 8 bit numbers to 0 --> 1.
#
thenorm=mpl.colors.Normalize(vmin=0,vmax=255)
#
# The actual color map itself.
#
thecmap=cm.rainbow
#
# This combines the above two together into a
# single unit that can map a scalar value into
# a "color".
#
themap=cm.ScalarMappable(norm=thenorm, cmap=thecmap)
#
# That color is then expressed via to to_rgba function, to which
# we can pass the computed color function values.
#
rgb_index=themap.to_rgba(color_index)

fig, ax = plt.subplots(1,1)
polyplot=PolyCollection(verts,color=rgb_index,picker=5)
ax.add_collection(polyplot)
ax.autoscale(True)
ax.set_aspect('equal')

def onpick(event):
    artist = event.artist
    xmouse, ymouse = event.mouseevent.xdata, event.mouseevent.ydata
    ind = event.ind
    print(' Polygon ',ind+1)
    print('R, Z of mouse: {:.4f},{:.4f}'.format(xmouse, ymouse))

def onclick(event):
    xmouse, ymouse = event.xdata, event.ydata
    print('x, y of mouse: {:.2f},{:.2f}'.format(xmouse, ymouse))

fig.canvas.mpl_connect('pick_event', onpick)

plt.show()



        
