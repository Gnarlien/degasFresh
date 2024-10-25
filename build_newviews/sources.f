      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      subroutine sample_sources(est,is,number,species_x,test_x,time_x,we
     &ight_x,pos_x,cell_x,zone_x,surface_x,cell_next_x,zone_next_x,secto
     &r_x,sector_next_x,velocity_x,type_x,author_x,ran_index_x,ran_array
     &_x,kseg)
      
      use so_mod
      
      use sp_mod
      
      use pr_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer est,number
      integer is
      integer species_x
      integer test_x
      REAL(kind=DOUBLE)time_x,weight_x,velocity_x(3)
      REAL(kind=DOUBLE)pos_x(3)
      integer cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x
      integer type_x,author_x
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer kseg
      REAL(kind=DOUBLE)urd,vprob
      integer xseg
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      REAL(kind=DOUBLE)random
      external random
      external locate_point,check_location
      integer locate_point
      logical check_location
      external find_index
      REAL(kind=DOUBLE)find_index
      if(so_sampling.EQ.0)then
      urd=random(ran_index_x,ran_array_x(0))
      else if(so_sampling.EQ.1)then
      urd=mod(so_direct_delta+(0.5_DOUBLE)*(sqrt((5._DOUBLE))-(1.0_DOUBL
     &E))*REAL(number,DOUBLE),(1.0_DOUBLE))
      else
      if('Illegal value of so_sampling'.EQ.' ')continue
      end if
      if(source_segment_ptr_alias(source_base_ptr(is)).NE.2000000000)the
     &n
      kseg=int(source_num_segments(is)*urd)
      vprob=source_num_segments(is)*urd-kseg
      if(vprob.LT.source_segment_prob_alias(source_base_ptr(is)+kseg))th
     &en
      xseg=source_segment_ptr(source_base_ptr(is)+kseg)
      else
      xseg=source_segment_ptr_alias(source_base_ptr(is)+kseg)
      kseg=int_lookup(xseg,source_segment_ptr(source_base_ptr(is)),sourc
     &e_num_segments(is))-1
      end if
      else
      kseg=int(find_index(urd,source_segment_prob_alias(source_base_ptr(
     &is)),source_num_segments(is)))
      kseg=min(kseg,source_num_segments(is)-1)
      xseg=source_segment_ptr(source_base_ptr(is)+kseg)
      end if
      call set_source_x(est,is,xseg,kseg,ran_index_x,ran_array_x(0),pos_
     &x(1),cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,sect
     &or_next_x)
      call set_source_v(is,xseg,kseg,pos_x(1),cell_x,zone_x,surface_x,ce
     &ll_next_x,zone_next_x,sector_x,sector_next_x,ran_index_x,ran_array
     &_x(0),velocity_x(1))
      if(source_time_variation(is).EQ.0)then
      time_x=so_time_initial
      if(so_time_initialization.EQ.0)continue
      else if(source_time_variation(is).EQ.1)then
      if((so_time_dependent.EQ.1).AND.(so_time_final.GT.so_time_initial)
     &)continue
      urd=random(ran_index_x,ran_array_x(0))
      time_x=so_time_initial+urd*(so_time_final-so_time_initial)
      else
      if('Unsupported source time variation'.EQ.' ')continue
      end if
      if(source_type(is).NE.5)then
      species_x=string_lookup(species_sy(source_species(is)),species_sy,
     &sp_num)
      test_x=problem_species_test(species_x)
      if(species_m(species_x).EQ.0)then
      type_x=0
      else if(species_z(species_x).EQ.0)then
      type_x=1
      author_x=source_type(is)
      else
      type_x=2
      author_x=source_type(is)
      end if
      weight_x=(1.0_DOUBLE)
      else
      call init_snapshot_pt(is,xseg,kseg,species_x,test_x,time_x,weight_
     &x,pos_x(1),cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_
     &x,sector_next_x,velocity_x(1),type_x,author_x)
      end if
      weight_x=source_segment_rel_wt(source_base_ptr(is)+kseg)*source_we
     &ight_norm(is)
      if(weight_x.GT.(0.0_DOUBLE))continue
      return
      end
      subroutine set_source_x(est,isource,xseg,kseg,ran_index_r,ran_arra
     &y_r,pos_x,cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x
     &,sector_next_x)
      
      use gi_mod
      
      use so_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer isource
      integer est,xseg,kseg
      integer ran_index_r
      REAL(kind=DOUBLE)ran_array_r(0:100-1)
      REAL(kind=DOUBLE)pos_x(3)
      integer cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x
      REAL(kind=DOUBLE)random
      external random
      if((geometry_symmetry.EQ.1).OR.(geometry_symmetry.EQ.3).OR.(geomet
     &ry_symmetry.EQ.2).OR.(geometry_symmetry.EQ.4).OR.(geometry_symmetr
     &y.EQ.5).OR.(geometry_symmetry.EQ.6))continue
      if(source_geometry(isource).EQ.2)then
      call set_surface_source_x(est,isource,xseg,kseg,ran_index_r,ran_ar
     &ray_r(0),pos_x(1),cell_x,zone_x,surface_x,cell_next_x,zone_next_x,
     &sector_x,sector_next_x)
      else if(source_geometry(isource).EQ.3)then
      if(source_type(isource).NE.5)then
      call set_volume_source_x(est,isource,xseg,kseg,ran_index_r,ran_arr
     &ay_r(0),pos_x(1),cell_x,zone_x,surface_x,cell_next_x,zone_next_x,s
     &ector_x,sector_next_x)
      else
      call set_snapshot_source_x(est,isource,xseg,kseg,ran_index_r,ran_a
     &rray_r(0),pos_x(1),cell_x,zone_x,surface_x,cell_next_x,zone_next_x
     &,sector_x,sector_next_x)
      end if
      else
      if("Untreated source geometry".EQ." ")continue
      end if
      return
      end
      subroutine set_surface_source_x(est,isource,xseg,kseg,ran_index_r,
     &ran_array_r,pos_x,cell_x,zone_x,surface_x,cell_next_x,zone_next_x,
     &sector_x,sector_next_x)
      
      use gi_mod
      
      use sc_mod
      
      use zn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer isource
      integer est,xseg,kseg
      integer ran_index_r
      REAL(kind=DOUBLE)ran_array_r(0:100-1)
      REAL(kind=DOUBLE)pos_x(3)
      integer cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x
      integer zone
      REAL(kind=DOUBLE)urd,r0,r1,fr,urd_y,delta_y,y,delta
      REAL(kind=DOUBLE)deltax(3)
      REAL(kind=DOUBLE)xsrc(3)
      REAL(kind=DOUBLE)vzero(3)
      REAL(kind=DOUBLE)vone(3)
      REAL(kind=DOUBLE)vnorm(3)
      REAL(kind=DOUBLE)random
      external random
      save/sources_common/
      REAL(kind=DOUBLE)x_surface(3)
      common/sources_common/x_surface
      external locate_point,check_location
      integer locate_point
      logical check_location
      if(est.EQ.3)then
      urd=(0.5_DOUBLE)
      else
      urd=random(ran_index_r,ran_array_r(0))
      end if
      
      if((geometry_symmetry.EQ.2).OR.(geometry_symmetry.EQ.5).OR.(geomet
     &ry_symmetry.EQ.6))then
      r0=sector_points(1,0,xseg)
      r1=sector_points(1,1,xseg)
      if(sector_points(2,0,xseg).EQ.(0.0_DOUBLE))continue
      if(sector_points(2,1,xseg).EQ.(0.0_DOUBLE))continue
      if(r1.NE.r0)urd=(sqrt(r0**2+urd*(r1**2-r0**2))-r0)/(r1-r0)
      else
      if((geometry_symmetry.EQ.1).OR.(geometry_symmetry.EQ.4).OR.(geomet
     &ry_symmetry.EQ.3))continue
      end if
      deltax(1)=sector_points(1,1,xseg)-sector_points(1,0,xseg)
      deltax(2)=sector_points(2,1,xseg)-sector_points(2,0,xseg)
      deltax(3)=sector_points(3,1,xseg)-sector_points(3,0,xseg)
      
      xsrc(1)=sector_points(1,0,xseg)+deltax(1)*(urd)
      xsrc(2)=sector_points(2,0,xseg)+deltax(2)*(urd)
      xsrc(3)=sector_points(3,0,xseg)+deltax(3)*(urd)
      
      zone=sector_zone(xseg)
      if((geometry_symmetry.EQ.4).OR.(geometry_symmetry.EQ.5).OR.(geomet
     &ry_symmetry.EQ.6))then
      if(zone_min(2,zone).NE.zone_max(2,zone))continue
      if(est.EQ.3)then
      urd_y=(0.5_DOUBLE)
      else
      urd_y=random(ran_index_r,ran_array_r(0))
      end if
      delta_y=zone_max(2,zone)-zone_min(2,zone)
      y=zone_min(2,zone)+urd_y*delta_y
      if(xsrc(2).EQ.(0.0_DOUBLE))continue
      if(geometry_symmetry.EQ.4)then
      xsrc(1)=xsrc(1)
      xsrc(2)=y
      xsrc(3)=xsrc(3)
      else
      r0=xsrc(1)
      xsrc(1)=r0*cos(y)
      xsrc(2)=r0*sin(y)
      xsrc(3)=xsrc(3)
      end if
      end if
      
      vzero(1)=(0.0_DOUBLE)
      vzero(2)=(0.0_DOUBLE)
      vzero(3)=(0.0_DOUBLE)
      vone(1)=(0.0_DOUBLE)
      vone(2)=(0.0_DOUBLE)
      vone(3)=(1.0_DOUBLE)
      call surface_reflect(sector_surface(xseg),xsrc(1),vzero(1),vone(1)
     &,vnorm(1))
      x_surface(1)=xsrc(1)
      x_surface(2)=xsrc(2)
      x_surface(3)=xsrc(3)
      
      delta=(1.0e-8_DOUBLE)
90000 continue
      fr=delta/sqrt((vnorm(1)**2+vnorm(2)**2+vnorm(3)**2))
      pos_x(1)=xsrc(1)+vnorm(1)*(fr)
      pos_x(2)=xsrc(2)+vnorm(2)*(fr)
      pos_x(3)=xsrc(3)+vnorm(3)*(fr)
      
      zone_x=zone
      cell_x=314159265
      return
      end
      subroutine set_volume_source_x(est,isource,xseg,kseg,ran_index_r,r
     &an_array_r,pos_x,cell_x,zone_x,surface_x,cell_next_x,zone_next_x,s
     &ector_x,sector_next_x)
      
      use gi_mod
      
      use sc_mod
      
      use zn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer isource
      integer est,xseg,kseg
      integer ran_index_r
      REAL(kind=DOUBLE)ran_array_r(0:100-1)
      REAL(kind=DOUBLE)pos_x(3)
      integer cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x
      integer i
      REAL(kind=DOUBLE)urd_x,urd_y,urd_z,r0,r1,x0,y0,z0,delta_y
      external locate_point,check_location
      integer locate_point
      logical check_location
      REAL(kind=DOUBLE)random
      external random
      if(est.EQ.3)then
      pos_x(1)=zone_center(1,xseg)
      pos_x(2)=zone_center(2,xseg)
      pos_x(3)=zone_center(3,xseg)
      
      cell_x=locate_point(pos_x,zone_x)
      surface_x=0
      cell_next_x=0
      zone_next_x=0
      sector_x=0
      sector_next_x=0
      if((cell_x.GT.0).AND.(zone_x.EQ.xseg))continue
      else
      i=0
90000 continue
      i=i+1
      urd_x=random(ran_index_r,ran_array_r(0))
      urd_z=random(ran_index_r,ran_array_r(0))
      if((geometry_symmetry.EQ.2).OR.(geometry_symmetry.EQ.5).OR.(geomet
     &ry_symmetry.EQ.6))then
      r0=zone_min(1,xseg)
      r1=zone_max(1,xseg)
      if(r1.NE.r0)urd_x=(sqrt(r0**2+urd_x*(r1**2-r0**2))-r0)/(r1-r0)
      else
      if((geometry_symmetry.EQ.1).OR.(geometry_symmetry.EQ.4).OR.(geomet
     &ry_symmetry.EQ.3))continue
      end if
      x0=zone_min(1,xseg)+urd_x*(zone_max(1,xseg)-zone_min(1,xseg))
      z0=zone_min(3,xseg)+urd_z*(zone_max(3,xseg)-zone_min(3,xseg))
      if((geometry_symmetry.EQ.4).OR.(geometry_symmetry.EQ.5).OR.(geomet
     &ry_symmetry.EQ.6))then
      if(zone_min(2,xseg).NE.zone_max(2,xseg))continue
      if(est.EQ.3)then
      urd_y=(0.5_DOUBLE)
      else
      urd_y=random(ran_index_r,ran_array_r(0))
      end if
      delta_y=zone_max(2,xseg)-zone_min(2,xseg)
      y0=zone_min(2,xseg)+urd_y*delta_y
      else
      if(zone_min(2,xseg).EQ.zone_max(2,xseg))continue
      y0=zone_min(2,xseg)
      end if
      if((geometry_symmetry.EQ.1).OR.(geometry_symmetry.EQ.4).OR.(geomet
     &ry_symmetry.EQ.3))then
      pos_x(1)=x0
      pos_x(2)=y0
      pos_x(3)=z0
      else
      pos_x(1)=x0*cos(y0)
      pos_x(2)=x0*sin(y0)
      pos_x(3)=z0
      end if
      cell_x=locate_point(pos_x,zone_x)
      surface_x=0
      cell_next_x=0
      zone_next_x=0
      sector_x=0
      sector_next_x=0
      if(cell_x.LT.0.OR.zone_x.NE.xseg)goto 90000
      end if
      return
      end
      subroutine set_snapshot_source_x(est,isource,xseg,kseg,ran_index_r
     &,ran_array_r,pos_x,cell_x,zone_x,surface_x,cell_next_x,zone_next_x
     &,sector_x,sector_next_x)
      
      use so_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer isource
      integer est,xseg,kseg
      integer ran_index_r
      REAL(kind=DOUBLE)ran_array_r(0:100-1)
      REAL(kind=DOUBLE)pos_x(3)
      integer cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x
      integer iparam,seg_tot
      REAL(kind=DOUBLE)x1,x2,x3
      if(source_num_parameters(isource).GT.0)continue
      seg_tot=source_base_ptr(isource)+kseg
      do iparam=1,source_num_parameters(isource)
      if(source_parameters_list(source_parameters_base(isource)+iparam).
     &EQ.12)then
      x1=source_parameters_data(source_parameters_data_base(isource)+(se
     &g_tot-source_base_ptr(isource))*source_num_parameters(isource)+ipa
     &ram)
      else if(source_parameters_list(source_parameters_base(isource)+ipa
     &ram).EQ.13)then
      x2=source_parameters_data(source_parameters_data_base(isource)+(se
     &g_tot-source_base_ptr(isource))*source_num_parameters(isource)+ipa
     &ram)
      else if(source_parameters_list(source_parameters_base(isource)+ipa
     &ram).EQ.14)then
      x3=source_parameters_data(source_parameters_data_base(isource)+(se
     &g_tot-source_base_ptr(isource))*source_num_parameters(isource)+ipa
     &ram)
      end if
      end do
      pos_x(1)=x1
      pos_x(2)=x2
      pos_x(3)=x3
      if(source_num_iparameters(isource).GT.0)continue
      do iparam=1,source_num_iparameters(isource)
      if(source_iparameters_list(source_iparameters_base(isource)+iparam
     &).EQ.3)then
      cell_x=source_iparameters_data(source_iparameters_data_base(isourc
     &e)+(seg_tot-source_base_ptr(isource))*source_num_iparameters(isour
     &ce)+iparam)
      else if(source_iparameters_list(source_iparameters_base(isource)+i
     &param).EQ.4)then
      zone_x=source_iparameters_data(source_iparameters_data_base(isourc
     &e)+(seg_tot-source_base_ptr(isource))*source_num_iparameters(isour
     &ce)+iparam)
      end if
      end do
      surface_x=0
      cell_next_x=0
      zone_next_x=0
      sector_x=0
      sector_next_x=0
      return
      end
      subroutine set_source_v(isource,xseg,kseg,pos_x,cell_x,zone_x,surf
     &ace_x,cell_next_x,zone_next_x,sector_x,sector_next_x,ran_index_r,r
     &an_array_r,vsrc)
      
      use so_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer isource
      integer xseg,kseg
      REAL(kind=DOUBLE)pos_x(3)
      integer cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x
      integer ran_index_r
      REAL(kind=DOUBLE)ran_array_r(0:100-1)
      REAL(kind=DOUBLE)vsrc(3)
      if(source_type(isource).EQ.1)then
      call set_plate_source_v(isource,xseg,kseg,pos_x(1),cell_x,zone_x,s
     &urface_x,cell_next_x,zone_next_x,sector_x,sector_next_x,ran_index_
     &r,ran_array_r(0),vsrc(1))
      else if(source_type(isource).EQ.2)then
      call set_puff_source_v(isource,xseg,kseg,pos_x(1),cell_x,zone_x,su
     &rface_x,cell_next_x,zone_next_x,sector_x,sector_next_x,ran_index_r
     &,ran_array_r(0),vsrc(1))
      else if(source_type(isource).EQ.3)then
      call set_recomb_source_v(isource,xseg,kseg,pos_x(1),cell_x,zone_x,
     &surface_x,cell_next_x,zone_next_x,sector_x,sector_next_x,ran_index
     &_r,ran_array_r(0),vsrc(1))
      else if(source_type(isource).EQ.4)then
      call set_vol_source_v(isource,xseg,kseg,pos_x(1),cell_x,zone_x,sur
     &face_x,cell_next_x,zone_next_x,sector_x,sector_next_x,ran_index_r,
     &ran_array_r(0),vsrc(1))
      else if(source_type(isource).EQ.5)then
      call set_snapshot_source_v(isource,xseg,kseg,pos_x(1),cell_x,zone_
     &x,surface_x,cell_next_x,zone_next_x,sector_x,sector_next_x,ran_ind
     &ex_r,ran_array_r(0),vsrc(1))
      else if(source_type(isource).EQ.6)then
      call set_plt_e_bins_source_v(isource,xseg,kseg,pos_x(1),cell_x,zon
     &e_x,surface_x,cell_next_x,zone_next_x,sector_x,sector_next_x,ran_i
     &ndex_r,ran_array_r(0),vsrc(1))
      else
      if("Unsupported source type".EQ." ")continue
      end if
      return
      end
      subroutine set_plate_source_v(isource,xseg,kseg,pos_x,cell_x,zone_
     &x,surface_x,cell_next_x,zone_next_x,sector_x,sector_next_x,ran_ind
     &ex_r,ran_array_r,vsrc)
      
      use sc_mod
      
      use so_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer isource
      integer xseg,kseg
      REAL(kind=DOUBLE)pos_x(3)
      integer cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x
      integer ran_index_r
      REAL(kind=DOUBLE)ran_array_r(0:100-1)
      REAL(kind=DOUBLE)vsrc(3)
      integer j,seg_tot,iparam
      REAL(kind=DOUBLE)wa,ion_energy,e_ion_mult
      REAL(kind=DOUBLE)vgauss(1)
      if(source_num_parameters(isource).GT.0)continue
      seg_tot=source_base_ptr(isource)+kseg
      do iparam=1,source_num_parameters(isource)
      if(source_parameters_list(source_parameters_base(isource)+iparam).
     &EQ.6)then
      e_ion_mult=source_parameters_data(source_parameters_data_base(isou
     &rce)+(seg_tot-source_base_ptr(isource))*source_num_parameters(isou
     &rce)+iparam)
      end if
      end do
      wa=(0.0_DOUBLE)
      if(e_ion_mult.GT.(0.0_DOUBLE))then
      do j=1,3
      call random_gauss(vgauss,1,ran_index_r,ran_array_r(0))
      wa=wa+(vgauss(1)**2)
      end do
      else
      if(e_ion_mult.EQ.(0.0_DOUBLE))continue
      end if
      call plate_v_details(isource,xseg,kseg,pos_x(1),cell_x,zone_x,surf
     &ace_x,cell_next_x,zone_next_x,sector_x,sector_next_x,wa,vsrc(1),io
     &n_energy)
      return
      end
      subroutine plate_v_details(isource,xseg,kseg,pos_x,cell_x,zone_x,s
     &urface_x,cell_next_x,zone_next_x,sector_x,sector_next_x,wa,vsrc,io
     &n_energy)
      
      use so_mod
      
      use pr_mod
      
      use bk_mod
      
      use sc_mod
      
      use sp_mod
      
      use zn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer isource
      integer xseg,kseg
      REAL(kind=DOUBLE)wa
      REAL(kind=DOUBLE)pos_x(3)
      integer cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x
      REAL(kind=DOUBLE)vsrc(3)
      REAL(kind=DOUBLE)ion_energy
      integer seg_tot,iparam
      REAL(kind=DOUBLE)e_ion_delta,e_ion_sheath,e_ion_mult
      integer spion
      integer back
      if(source_geometry(isource).EQ.2)continue
      spion=source_root_species(isource)
      back=problem_species_background(spion)
      if((back.GT.0.AND.back.LE.pr_background_num))continue
      wa=(0.5_DOUBLE)*wa*background_temp(back,zone_pointer(zone_x))
      if(source_num_parameters(isource).GT.0)continue
      seg_tot=source_base_ptr(isource)+kseg
      do iparam=1,source_num_parameters(isource)
      if(source_parameters_list(source_parameters_base(isource)+iparam).
     &EQ.5)then
      e_ion_delta=source_parameters_data(source_parameters_data_base(iso
     &urce)+(seg_tot-source_base_ptr(isource))*source_num_parameters(iso
     &urce)+iparam)
      else if(source_parameters_list(source_parameters_base(isource)+ipa
     &ram).EQ.7)then
      e_ion_sheath=source_parameters_data(source_parameters_data_base(is
     &ource)+(seg_tot-source_base_ptr(isource))*source_num_parameters(is
     &ource)+iparam)
      else if(source_parameters_list(source_parameters_base(isource)+ipa
     &ram).EQ.6)then
      e_ion_mult=source_parameters_data(source_parameters_data_base(isou
     &rce)+(seg_tot-source_base_ptr(isource))*source_num_parameters(isou
     &rce)+iparam)
      end if
      end do
      ion_energy=e_ion_delta+e_ion_mult*wa+e_ion_sheath*species_z(spion)
      if(ion_energy.GE.(0.0_DOUBLE))continue
      call plate_v_vector(ion_energy,spion,xseg,vsrc(1))
      return
      end
      subroutine plate_v_vector(ion_energy,spion,xseg,vsrc)
      
      use sp_mod
      
      use sc_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)ion_energy
      integer xseg
      integer spion
      REAL(kind=DOUBLE)vsrc(3)
      REAL(kind=DOUBLE)ion_velocity
      REAL(kind=DOUBLE)vzero(3)
      REAL(kind=DOUBLE)vone(3)
      save/sources_common/
      REAL(kind=DOUBLE)x_surface(3)
      common/sources_common/x_surface
      ion_velocity=sqrt((2.0_DOUBLE)*ion_energy/species_m(spion))
      vzero(1)=(0.0_DOUBLE)
      vzero(2)=(0.0_DOUBLE)
      vzero(3)=(0.0_DOUBLE)
      vone(1)=(0.0_DOUBLE)
      vone(2)=(0.0_DOUBLE)
      vone(3)=-(1.0_DOUBLE)
      call surface_reflect(sector_surface(xseg),x_surface(1),vzero(1),vo
     &ne(1),vsrc(1))
      vsrc(1)=(ion_velocity)*vsrc(1)
      vsrc(2)=(ion_velocity)*vsrc(2)
      vsrc(3)=(ion_velocity)*vsrc(3)
      
      return
      end
      subroutine set_puff_source_v(isource,xseg,kseg,pos_x,cell_x,zone_x
     &,surface_x,cell_next_x,zone_next_x,sector_x,sector_next_x,ran_inde
     &x_r,ran_array_r,vsrc)
      
      use so_mod
      
      use sc_mod
      
      use sp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer isource
      integer xseg,kseg
      REAL(kind=DOUBLE)pos_x(3)
      integer cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x
      integer ran_index_r
      REAL(kind=DOUBLE)ran_array_r(0:100-1)
      REAL(kind=DOUBLE)vsrc(3)
      integer j,iparam
      REAL(kind=DOUBLE)vpuff,energy,p,xi,cos_theta,sin_theta,phi,cos_phi
     &,sin_phi
      REAL(kind=DOUBLE)vgauss(1)
      REAL(kind=DOUBLE)random
      external random
      REAL(kind=DOUBLE)vcos(3)
      p=(314159265.3589793238462_DOUBLE)
      if(source_num_gparameters(isource).GT.0)then
      do iparam=1,source_num_gparameters(isource)
      if(source_gparameters_list(source_gparameters_base(isource)+iparam
     &).EQ.2)p=source_gparameters_data(source_gparameters_base(isource)+
     &iparam)
      end do
      end if
      if((p.EQ.(1.0_DOUBLE)).OR.(p.EQ.(314159265.3589793238462_DOUBLE)))
     &then
      call random_cosdist(vcos,1,ran_index_r,ran_array_r(0))
      else
      xi=random(ran_index_r,ran_array_r(0))
      cos_theta=xi**((1.0_DOUBLE)/((1.0_DOUBLE)+p))
      sin_theta=sqrt((1.0_DOUBLE)-xi**((2.0_DOUBLE)/((1.0_DOUBLE)+p)))
      if(sin_theta.LT.(0.0_DOUBLE))sin_theta=(0.0_DOUBLE)
      if(sin_theta.GT.(1.0_DOUBLE))sin_theta=(1.0_DOUBLE)
      phi=random(ran_index_r,ran_array_r(0))
      phi=atan2((0.0_DOUBLE),-(1.0_DOUBLE))*((2.0_DOUBLE)*phi-(1.0_DOUBL
     &E))
      cos_phi=cos(phi)
      sin_phi=sin(phi)
      vcos(1)=sin_theta*cos_phi
      vcos(2)=sin_theta*sin_phi
      vcos(3)=cos_theta
      end if
      vpuff=(0.0_DOUBLE)
      do j=1,3
      call random_gauss(vgauss,1,ran_index_r,ran_array_r(0))
      vpuff=vpuff+(vgauss(1)**2)
      end do
      call puff_v_details(isource,xseg,kseg,pos_x(1),cell_x,zone_x,surfa
     &ce_x,cell_next_x,zone_next_x,sector_x,sector_next_x,vpuff,vcos,vsr
     &c,energy)
      return
      end
      subroutine puff_v_details(isource,xseg,kseg,pos_x,cell_x,zone_x,su
     &rface_x,cell_next_x,zone_next_x,sector_x,sector_next_x,vpuff,vcos,
     &vsrc,energy)
      
      use so_mod
      
      use sc_mod
      
      use sp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer isource
      integer xseg,kseg
      REAL(kind=DOUBLE)vpuff
      REAL(kind=DOUBLE)pos_x(3)
      integer cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x
      REAL(kind=DOUBLE)vcos(3)
      REAL(kind=DOUBLE)vsrc(3)
      REAL(kind=DOUBLE)energy
      integer iparam
      REAL(kind=DOUBLE)temp
      integer spsrc
      REAL(kind=DOUBLE)vzero(3)
      save/sources_common/
      REAL(kind=DOUBLE)x_surface(3)
      common/sources_common/x_surface
      if(source_geometry(isource).EQ.2)continue
      spsrc=source_species(isource)
      vzero(1)=(0.0_DOUBLE)
      vzero(2)=(0.0_DOUBLE)
      vzero(3)=(0.0_DOUBLE)
      call surface_reflect(sector_surface(xseg),x_surface(1),vzero(1),vc
     &os(1),vsrc(1))
      if(source_num_gparameters(isource).GT.0)continue
      do iparam=1,source_num_gparameters(isource)
      if(source_gparameters_list(source_gparameters_base(isource)+iparam
     &).EQ.1)temp=source_gparameters_data(source_gparameters_base(isourc
     &e)+iparam)
      end do
      if(temp.GT.(0.0_DOUBLE))continue
      energy=(0.5_DOUBLE)*vpuff*temp
      vpuff=sqrt((vpuff*temp)/species_m(spsrc))
      vsrc(1)=(vpuff)*vsrc(1)
      vsrc(2)=(vpuff)*vsrc(2)
      vsrc(3)=(vpuff)*vsrc(3)
      
      return
      end
      subroutine set_recomb_source_v(isource,xseg,kseg,pos_x,cell_x,zone
     &_x,surface_x,cell_next_x,zone_next_x,sector_x,sector_next_x,ran_in
     &dex_r,ran_array_r,vsrc)
      
      use so_mod
      
      use pr_mod
      
      use bk_mod
      
      use gi_mod
      
      use sp_mod
      
      use zn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer isource
      integer xseg,kseg
      REAL(kind=DOUBLE)pos_x(3)
      integer cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x
      integer ran_index_r
      REAL(kind=DOUBLE)ran_array_r(0:100-1)
      REAL(kind=DOUBLE)vsrc(3)
      integer j
      REAL(kind=DOUBLE)vion
      REAL(kind=DOUBLE)vgauss(1)
      integer spsrc
      REAL(kind=DOUBLE)vb_flow(3)
      integer spback
      if(source_geometry(isource).EQ.3)continue
      spsrc=source_species(isource)
      spback=problem_species_background(source_root_species(isource))
      if(spback.NE.0)continue
      if((geometry_symmetry.EQ.2.OR.geometry_symmetry.EQ.5.OR.geometry_s
     &ymmetry.EQ.6).AND.(background_coords.EQ.2).AND.(pos_x(1)**2+pos_x(
     &2)**2.GT.(0.0_DOUBLE)))then
      vb_flow(1)=(background_v(1,spback,zone_pointer(zone_x))*pos_x(1)-b
     &ackground_v(2,spback,zone_pointer(zone_x))*pos_x(2))/sqrt(pos_x(1)
     &**2+pos_x(2)**2)
      vb_flow(2)=(background_v(1,spback,zone_pointer(zone_x))*pos_x(2)+b
     &ackground_v(2,spback,zone_pointer(zone_x))*pos_x(1))/sqrt(pos_x(1)
     &**2+pos_x(2)**2)
      vb_flow(3)=background_v(3,spback,zone_pointer(zone_x))
      else
      if(.NOT.((geometry_symmetry.EQ.0).AND.(background_coords.EQ.2)))co
     &ntinue
      vb_flow(1)=background_v(1,spback,zone_pointer(zone_x))
      vb_flow(2)=background_v(2,spback,zone_pointer(zone_x))
      vb_flow(3)=background_v(3,spback,zone_pointer(zone_x))
      
      end if
      vion=sqrt(background_temp(spback,zone_pointer(zone_x))/species_m(s
     &psrc))
      do j=1,3
      call random_gauss(vgauss,1,ran_index_r,ran_array_r(0))
      vsrc(j)=vion*vgauss(1)+vb_flow(j)
      end do
      return
      end
      subroutine set_vol_source_v(isource,xseg,kseg,pos_x,cell_x,zone_x,
     &surface_x,cell_next_x,zone_next_x,sector_x,sector_next_x,ran_index
     &_r,ran_array_r,vsrc)
      
      use so_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer isource
      integer xseg,kseg
      REAL(kind=DOUBLE)pos_x(3)
      integer cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x
      integer ran_index_r
      REAL(kind=DOUBLE)ran_array_r(0:100-1)
      REAL(kind=DOUBLE)vsrc(3)
      REAL(kind=DOUBLE)temp
      REAL(kind=DOUBLE)v_gauss(3)
      if(source_geometry(isource).EQ.3)continue
      call random_gauss(v_gauss,3,ran_index_r,ran_array_r(0))
      call vol_source_v_details(isource,xseg,kseg,pos_x(1),cell_x,zone_x
     &,surface_x,cell_next_x,zone_next_x,sector_x,sector_next_x,v_gauss(
     &1),vsrc(1),temp)
      return
      end
      subroutine vol_source_v_details(isource,xseg,kseg,pos_x,cell_x,zon
     &e_x,surface_x,cell_next_x,zone_next_x,sector_x,sector_next_x,v_gau
     &ss,vsrc,temp)
      
      use so_mod
      
      use bk_mod
      
      use gi_mod
      
      use sp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer isource
      integer xseg,kseg
      REAL(kind=DOUBLE)pos_x(3)
      integer cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x
      REAL(kind=DOUBLE)v_gauss(3)
      REAL(kind=DOUBLE)vsrc(3)
      REAL(kind=DOUBLE)temp
      integer i,seg_tot,iparam
      REAL(kind=DOUBLE)v1,v2,v3,vion
      integer spsrc
      REAL(kind=DOUBLE)vb_flow(3)
      spsrc=source_species(isource)
      v1=(0.0_DOUBLE)
      v2=(0.0_DOUBLE)
      v3=(0.0_DOUBLE)
      if(source_num_parameters(isource).GT.0)continue
      seg_tot=source_base_ptr(isource)+kseg
      do iparam=1,source_num_parameters(isource)
      if(source_parameters_list(source_parameters_base(isource)+iparam).
     &EQ.4)then
      temp=source_parameters_data(source_parameters_data_base(isource)+(
     &seg_tot-source_base_ptr(isource))*source_num_parameters(isource)+i
     &param)
      if(temp.GT.(0.0_DOUBLE))continue
      else if(source_parameters_list(source_parameters_base(isource)+ipa
     &ram).EQ.1)then
      v1=source_parameters_data(source_parameters_data_base(isource)+(se
     &g_tot-source_base_ptr(isource))*source_num_parameters(isource)+ipa
     &ram)
      else if(source_parameters_list(source_parameters_base(isource)+ipa
     &ram).EQ.2)then
      v2=source_parameters_data(source_parameters_data_base(isource)+(se
     &g_tot-source_base_ptr(isource))*source_num_parameters(isource)+ipa
     &ram)
      else if(source_parameters_list(source_parameters_base(isource)+ipa
     &ram).EQ.3)then
      v3=source_parameters_data(source_parameters_data_base(isource)+(se
     &g_tot-source_base_ptr(isource))*source_num_parameters(isource)+ipa
     &ram)
      end if
      end do
      vsrc(1)=v1
      vsrc(2)=v2
      vsrc(3)=v3
      if((geometry_symmetry.EQ.2.OR.geometry_symmetry.EQ.5.OR.geometry_s
     &ymmetry.EQ.6).AND.(background_coords.EQ.2).AND.(pos_x(1)**2+pos_x(
     &2)**2.GT.(0.0_DOUBLE)))then
      vb_flow(1)=(vsrc(1)*pos_x(1)-vsrc(2)*pos_x(2))/sqrt(pos_x(1)**2+po
     &s_x(2)**2)
      vb_flow(2)=(vsrc(1)*pos_x(2)+vsrc(2)*pos_x(1))/sqrt(pos_x(1)**2+po
     &s_x(2)**2)
      vb_flow(3)=vsrc(3)
      else
      if(.NOT.((geometry_symmetry.EQ.0).AND.(background_coords.EQ.2)))co
     &ntinue
      vb_flow(1)=vsrc(1)
      vb_flow(2)=vsrc(2)
      vb_flow(3)=vsrc(3)
      
      end if
      vion=sqrt(temp/species_m(spsrc))
      do i=1,3
      vsrc(i)=vion*v_gauss(i)+vb_flow(i)
      end do
      return
      end
      subroutine set_snapshot_source_v(isource,xseg,kseg,pos_x,cell_x,zo
     &ne_x,surface_x,cell_next_x,zone_next_x,sector_x,sector_next_x,ran_
     &index_r,ran_array_r,vsrc)
      
      use so_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer isource
      integer xseg,kseg
      REAL(kind=DOUBLE)pos_x(3)
      integer cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x
      integer ran_index_r
      REAL(kind=DOUBLE)ran_array_r(0:100-1)
      REAL(kind=DOUBLE)vsrc(3)
      integer iparam,seg_tot
      REAL(kind=DOUBLE)v1,v2,v3
      if(source_num_parameters(isource).GT.0)continue
      seg_tot=source_base_ptr(isource)+kseg
      do iparam=1,source_num_parameters(isource)
      if(source_parameters_list(source_parameters_base(isource)+iparam).
     &EQ.9)then
      v1=source_parameters_data(source_parameters_data_base(isource)+(se
     &g_tot-source_base_ptr(isource))*source_num_parameters(isource)+ipa
     &ram)
      else if(source_parameters_list(source_parameters_base(isource)+ipa
     &ram).EQ.10)then
      v2=source_parameters_data(source_parameters_data_base(isource)+(se
     &g_tot-source_base_ptr(isource))*source_num_parameters(isource)+ipa
     &ram)
      else if(source_parameters_list(source_parameters_base(isource)+ipa
     &ram).EQ.11)then
      v3=source_parameters_data(source_parameters_data_base(isource)+(se
     &g_tot-source_base_ptr(isource))*source_num_parameters(isource)+ipa
     &ram)
      end if
      end do
      vsrc(1)=v1
      vsrc(2)=v2
      vsrc(3)=v3
      return
      end
      subroutine set_plt_e_bins_source_v(isource,xseg,kseg,pos_x,cell_x,
     &zone_x,surface_x,cell_next_x,zone_next_x,sector_x,sector_next_x,ra
     &n_index_r,ran_array_r,vsrc)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer isource
      integer xseg,kseg
      REAL(kind=DOUBLE)pos_x(3)
      integer cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x
      integer ran_index_r
      REAL(kind=DOUBLE)ran_array_r(0:100-1)
      REAL(kind=DOUBLE)vsrc(3)
      REAL(kind=DOUBLE)urd,ion_energy
      REAL(kind=DOUBLE)random
      external random
      urd=random(ran_index_r,ran_array_r(0))
      call plt_e_bins_v_details(isource,xseg,kseg,pos_x(1),cell_x,zone_x
     &,surface_x,cell_next_x,zone_next_x,sector_x,sector_next_x,urd,vsrc
     &(1),ion_energy)
      return
      end
      subroutine plt_e_bins_v_details(isource,xseg,kseg,pos_x,cell_x,zon
     &e_x,surface_x,cell_next_x,zone_next_x,sector_x,sector_next_x,urd,v
     &src,ion_energy)
      
      use so_mod
      
      use sp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer isource
      integer xseg,kseg
      REAL(kind=DOUBLE)urd
      REAL(kind=DOUBLE)pos_x(3)
      integer cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x
      REAL(kind=DOUBLE)vsrc(3)
      REAL(kind=DOUBLE)ion_energy
      integer iparam,num_bins,e_spacing,i_bin,seg_tot
      REAL(kind=DOUBLE)e_min,e_delta,ran,x_bin,e_i,e_im1,pdf_i
      REAL(kind=DOUBLE)e_prob(0:15)
      integer spion
      external find_index
      REAL(kind=DOUBLE)find_index
      if(source_num_giparameters(isource).GT.0)continue
      do iparam=1,source_num_giparameters(isource)
      if(source_giparameters_list(source_giparameters_base(isource)+ipar
     &am).EQ.1)then
      num_bins=source_giparameters_data(source_giparameters_base(isource
     &)+iparam)
      else if(source_giparameters_list(source_giparameters_base(isource)
     &+iparam).EQ.2)then
      e_spacing=source_giparameters_data(source_giparameters_base(isourc
     &e)+iparam)
      end if
      end do
      if((num_bins.GT.0).AND.(num_bins.LE.15))continue
      if(e_spacing.GT.0)continue
      if(source_num_gparameters(isource).GT.0)continue
      do iparam=1,source_num_gparameters(isource)
      if(source_gparameters_list(source_gparameters_base(isource)+iparam
     &).EQ.3)then
      e_min=source_gparameters_data(source_gparameters_base(isource)+ipa
     &ram)
      else if(source_gparameters_list(source_gparameters_base(isource)+i
     &param).EQ.4)then
      e_delta=source_gparameters_data(source_gparameters_base(isource)+i
     &param)
      end if
      end do
      if(e_spacing.EQ.1)then
      if(e_min.GT.(0.0_DOUBLE))continue
      end if
      if(e_delta.GT.(0.0_DOUBLE))continue
      if(source_num_parameters(isource).GT.0)continue
      seg_tot=source_base_ptr(isource)+kseg
      i_bin=0
      e_prob(i_bin)=(0.0_DOUBLE)
      do iparam=1,source_num_parameters(isource)
      if(source_parameters_list(source_parameters_base(isource)+iparam).
     &EQ.15)then
      i_bin=i_bin+1
      e_prob(i_bin)=source_parameters_data(source_parameters_data_base(i
     &source)+(seg_tot-source_base_ptr(isource))*source_num_parameters(i
     &source)+iparam)
      end if
      end do
      if(i_bin.GE.num_bins)continue
      if(urd.NE.(2.0e30_DOUBLE))then
      x_bin=find_index(urd,e_prob,num_bins+1)
      ion_energy=e_min+x_bin*e_delta
      if(e_spacing.EQ.2)then
      ion_energy=exp(ion_energy)
      else
      if(e_spacing.EQ.1)continue
      end if
      if(ion_energy.GT.(0.0_DOUBLE))continue
      else
      ion_energy=(0.0_DOUBLE)
      do i_bin=1,num_bins
      e_i=e_min+REAL(i_bin,DOUBLE)*e_delta
      e_im1=e_min+REAL(i_bin-1,DOUBLE)*e_delta
      if(i_bin.GT.1)then
      pdf_i=e_prob(i_bin)-e_prob(i_bin-1)
      else
      pdf_i=e_prob(1)
      end if
      if(e_spacing.EQ.1)then
      ion_energy=ion_energy+(pdf_i*(0.5_DOUBLE)*(e_i+e_im1))
      else if(e_spacing.EQ.2)then
      e_i=exp(e_i)
      e_im1=exp(e_im1)
      ion_energy=ion_energy+(pdf_i*(e_i-e_im1)/e_delta)
      else
      if('Unexpected energy bin spacing'.EQ.' ')continue
      end if
      end do
      end if
      if(source_geometry(isource).EQ.2)continue
      spion=source_root_species(isource)
      call plate_v_vector(ion_energy,spion,xseg,vsrc(1))
      return
      end
      subroutine init_snapshot_pt(isource,xseg,kseg,species_x,test_x,tim
     &e_x,weight_x,pos_x,cell_x,zone_x,surface_x,cell_next_x,zone_next_x
     &,sector_x,sector_next_x,velocity_x,type_x,author_x)
      
      use so_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer isource
      integer xseg,kseg
      integer species_x
      integer test_x
      REAL(kind=DOUBLE)time_x,weight_x,velocity_x(3)
      REAL(kind=DOUBLE)pos_x(3)
      integer cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x
      integer type_x,author_x
      integer iparam,seg_tot
      if(source_num_iparameters(isource).GT.0)continue
      seg_tot=source_base_ptr(isource)+kseg
      do iparam=1,source_num_iparameters(isource)
      if(source_iparameters_list(source_iparameters_base(isource)+iparam
     &).EQ.1)then
      species_x=source_iparameters_data(source_iparameters_data_base(iso
     &urce)+(seg_tot-source_base_ptr(isource))*source_num_iparameters(is
     &ource)+iparam)
      else if(source_iparameters_list(source_iparameters_base(isource)+i
     &param).EQ.2)then
      test_x=source_iparameters_data(source_iparameters_data_base(isourc
     &e)+(seg_tot-source_base_ptr(isource))*source_num_iparameters(isour
     &ce)+iparam)
      else if(source_iparameters_list(source_iparameters_base(isource)+i
     &param).EQ.5)then
      type_x=source_iparameters_data(source_iparameters_data_base(isourc
     &e)+(seg_tot-source_base_ptr(isource))*source_num_iparameters(isour
     &ce)+iparam)
      else if(source_iparameters_list(source_iparameters_base(isource)+i
     &param).EQ.6)then
      author_x=source_iparameters_data(source_iparameters_data_base(isou
     &rce)+(seg_tot-source_base_ptr(isource))*source_num_iparameters(iso
     &urce)+iparam)
      end if
      end do
      return
      end
      
      
