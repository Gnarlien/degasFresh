      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      function particle_track(tmax,t,species_x,test_x,time_x,weight_x,po
     &s_x,cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,secto
     &r_next_x,velocity_x,type_x,author_x)
      
      use gi_mod
      
      use sc_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical particle_track
      integer species_x
      integer test_x
      REAL(kind=DOUBLE)time_x,weight_x,velocity_x(3)
      REAL(kind=DOUBLE)pos_x(3)
      integer cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x
      integer type_x,author_x
      REAL(kind=DOUBLE)tmax
      REAL(kind=DOUBLE)t
      REAL(kind=DOUBLE)pos_y(3)
      integer cell_y,zone_y,surface_y,cell_next_y,zone_next_y,sector_y,s
     &ector_next_y
      external track
      logical track
      particle_track=track(type_x,tmax,pos_x,velocity_x,cell_x,t,pos_y,c
     &ell_y,surface_y,cell_next_y,sector_y,sector_next_y)
      time_x=time_x+t
      zone_y=cells(4,cell_y)
      if(surface_y.NE.0)then
      zone_next_y=cells(4,cell_next_y)
      else
      zone_next_y=zone_y
      end if
      pos_x(1)=pos_y(1)
      pos_x(2)=pos_y(2)
      pos_x(3)=pos_y(3)
      
      cell_x=cell_y
      zone_x=zone_y
      surface_x=surface_y
      cell_next_x=cell_next_y
      zone_next_x=zone_next_y
      sector_x=sector_y
      sector_next_x=sector_next_y
      return
      end
      
      
