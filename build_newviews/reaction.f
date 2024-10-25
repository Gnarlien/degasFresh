      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      function find_rate(species_p,test_p,time_p,weight_p,pos_p,cell_p,z
     &one_p,surface_p,cell_next_p,zone_next_p,sector_p,sector_next_p,vel
     &ocity_p,type_p,author_p,back,pr_reac,ran_index_x,ran_array_x)
      
      use pr_mod
      
      use rd_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer inc
      REAL(kind=DOUBLE)find_rate
      integer species_p
      integer test_p
      REAL(kind=DOUBLE)time_p,weight_p,velocity_p(3)
      REAL(kind=DOUBLE)pos_p(3)
      integer cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,s
     &ector_next_p
      integer type_p,author_p
      integer back
      integer pr_reac
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      REAL(kind=DOUBLE)independent_parameters(20)
      external eval_data
      REAL(kind=DOUBLE)eval_data
      call set_indep_params(species_p,test_p,time_p,weight_p,pos_p(1),ce
     &ll_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,sector_next
     &_p,velocity_p(1),type_p,author_p,back,reaction_rate_num_rand(pr_re
     &ac),ran_index_x,ran_array_x(0),independent_parameters)
      if(independent_parameters(1).NE.(0.0_DOUBLE))then
      find_rate=eval_data(reaction_rate_eval_name(pr_reac),reaction_rate
     &_rank(pr_reac),reaction_rate_var(1,pr_reac),reaction_rate_tab_inde
     &x(1,pr_reac),reaction_rate_spacing(0,pr_reac),reaction_rate_min(1,
     &pr_reac),reaction_rate_delta(1,pr_reac),reaction_rate_tab(reaction
     &_rate_base(pr_reac)),independent_parameters,ran_index_x,ran_array_
     &x(0))
      find_rate=find_rate*(independent_parameters(1))
      else
      find_rate=(0.0_DOUBLE)
      endif
      return
      end
      subroutine set_indep_params(species_p,test_p,time_p,weight_p,pos_p
     &,cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,sector_n
     &ext_p,velocity_p,type_p,author_p,back,num_random,ran_index_x,ran_a
     &rray_x,independent_parameters)
      
      use zn_mod
      
      use bk_mod
      
      use gi_mod
      
      use sp_mod
      
      use pr_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer species_p
      integer test_p
      REAL(kind=DOUBLE)time_p,weight_p,velocity_p(3)
      REAL(kind=DOUBLE)pos_p(3)
      integer cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,s
     &ector_next_p
      integer type_p,author_p
      integer back
      integer num_random
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      REAL(kind=DOUBLE)vb_flow(3)
      REAL(kind=DOUBLE)vt_flow(3)
      REAL(kind=DOUBLE)vrel(3)
      integer back_test
      integer back_elec
      integer i
      REAL(kind=DOUBLE)independent_parameters(20)
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      if((geometry_symmetry.EQ.2.OR.geometry_symmetry.EQ.5.OR.geometry_s
     &ymmetry.EQ.6).AND.(background_coords.EQ.2).AND.(pos_p(1)**2+pos_p(
     &2)**2.GT.(0.0_DOUBLE)))then
      vb_flow(1)=(background_v(1,back,zone_pointer(zone_p))*pos_p(1)-bac
     &kground_v(2,back,zone_pointer(zone_p))*pos_p(2))/sqrt(pos_p(1)**2+
     &pos_p(2)**2)
      vb_flow(2)=(background_v(1,back,zone_pointer(zone_p))*pos_p(2)+bac
     &kground_v(2,back,zone_pointer(zone_p))*pos_p(1))/sqrt(pos_p(1)**2+
     &pos_p(2)**2)
      vb_flow(3)=background_v(3,back,zone_pointer(zone_p))
      else
      if(.NOT.((geometry_symmetry.EQ.0).AND.(background_coords.EQ.2)))co
     &ntinue
      vb_flow(1)=background_v(1,back,zone_pointer(zone_p))
      vb_flow(2)=background_v(2,back,zone_pointer(zone_p))
      vb_flow(3)=background_v(3,back,zone_pointer(zone_p))
      
      end if
      do i=1,20
      independent_parameters(i)=(0.0_DOUBLE)
      end do
      independent_parameters(1)=background_n(back,zone_pointer(zone_p))
      vrel(1)=velocity_p(1)-vb_flow(1)
      vrel(2)=velocity_p(2)-vb_flow(2)
      vrel(3)=velocity_p(3)-vb_flow(3)
      
      independent_parameters(2)=(0.5_DOUBLE)*species_m(species_p)*(vrel(
     &1)**2+vrel(2)**2+vrel(3)**2)
      independent_parameters(3)=background_temp(back,zone_pointer(zone_p
     &))
      independent_parameters(4)=(0.5_DOUBLE)*(vrel(1)**2+vrel(2)**2+vrel
     &(3)**2)
      independent_parameters(5)=background_temp(back,zone_pointer(zone_p
     &))/species_m(problem_background_sp(back))
      independent_parameters(6)=zone_p
      independent_parameters(8)=velocity_p(1)
      independent_parameters(9)=velocity_p(2)
      independent_parameters(10)=velocity_p(3)
      independent_parameters(11)=vb_flow(1)
      independent_parameters(12)=vb_flow(2)
      independent_parameters(13)=vb_flow(3)
      independent_parameters(14)=species_m(species_p)
      independent_parameters(15)=species_m(problem_background_sp(back))
      if(num_random.GT.0)then
      if(num_random.LE.1)continue
      call random_array(independent_parameters(7),num_random,ran_index_x
     &,ran_array_x(0))
      end if
      if((problem_species_background(species_p).GT.0.AND.problem_species
     &_background(species_p).LE.bk_num))then
      back_test=problem_species_background(species_p)
      if((geometry_symmetry.EQ.2.OR.geometry_symmetry.EQ.5.OR.geometry_s
     &ymmetry.EQ.6).AND.(background_coords.EQ.2).AND.(pos_p(1)**2+pos_p(
     &2)**2.GT.(0.0_DOUBLE)))then
      vt_flow(1)=(background_v(1,back_test,zone_pointer(zone_p))*pos_p(1
     &)-background_v(2,back_test,zone_pointer(zone_p))*pos_p(2))/sqrt(po
     &s_p(1)**2+pos_p(2)**2)
      vt_flow(2)=(background_v(1,back_test,zone_pointer(zone_p))*pos_p(2
     &)+background_v(2,back_test,zone_pointer(zone_p))*pos_p(1))/sqrt(po
     &s_p(1)**2+pos_p(2)**2)
      vt_flow(3)=background_v(3,back_test,zone_pointer(zone_p))
      else
      if(.NOT.((geometry_symmetry.EQ.0).AND.(background_coords.EQ.2)))co
     &ntinue
      vt_flow(1)=background_v(1,back_test,zone_pointer(zone_p))
      vt_flow(2)=background_v(2,back_test,zone_pointer(zone_p))
      vt_flow(3)=background_v(3,back_test,zone_pointer(zone_p))
      
      end if
      else
      vt_flow(1)=(0.0_DOUBLE)
      vt_flow(2)=(0.0_DOUBLE)
      vt_flow(3)=(0.0_DOUBLE)
      end if
      independent_parameters(17)=vt_flow(1)
      independent_parameters(18)=vt_flow(2)
      independent_parameters(19)=vt_flow(3)
      back_elec=problem_species_background(string_lookup('e',species_sy,
     &sp_num))
      if((back_elec.GT.0.AND.back_elec.LE.bk_num))continue
      independent_parameters(20)=background_temp(back_elec,zone_pointer(
     &zone_p))
      return
      end
      function eval_data(eval_name,rank,ind_params_label,tab_index,spaci
     &ng,min,delta,data_table,independent_parameters,ran_index_x,ran_arr
     &ay_x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)eval_data
      character*40 eval_name
      integer rank,ind_params_label(*),spacing(0:*),tab_index(*)
      REAL(kind=DOUBLE)min(*),delta(*),data_table(0:*),independent_param
     &eters(*)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer i
      REAL(kind=DOUBLE)ind_params_value(3)
      external interpolate1,interpolate2,interpolate3,interpolate4,inter
     &polate5
      REAL(kind=DOUBLE)interpolate1,interpolate2,interpolate3,interpolat
     &e4,interpolate5
      if(rank.GT.0)then
      do i=1,rank
      if(ind_params_label(i).LE.20)continue
      ind_params_value(i)=independent_parameters(ind_params_label(i))
      end do
      end if
      if(eval_name.EQ.'table')then
      if(rank.GT.0)then
      do i=1,rank
      if(spacing(i).EQ.2)ind_params_value(i)=log(ind_params_value(i))
      if(spacing(i).EQ.2.OR.spacing(i).EQ.1)then
      ind_params_value(i)=(ind_params_value(i)-min(i))/delta(i)
      else
      if('Unsupported spacing'.EQ.' ')continue
      end if
      end do
      end if
      if(rank.EQ.0)then
      eval_data=data_table(0)
      else if(rank.EQ.1)then
      eval_data=interpolate1(ind_params_value(1),data_table(0),tab_index
     &(1))
      else if(rank.EQ.2)then
      eval_data=interpolate2(ind_params_value,data_table(0),tab_index(1)
     &)
      else if(rank.EQ.3)then
      eval_data=interpolate3(ind_params_value,data_table(0),tab_index(1)
     &)
      else if(rank.EQ.4)then
      eval_data=interpolate4(ind_params_value,data_table(0),tab_index(1)
     &)
      else if(rank.EQ.5)then
      eval_data=interpolate5(ind_params_value,data_table(0),tab_index(1)
     &)
      end if
      if(spacing(0).EQ.2)eval_data=exp(eval_data)
      else
      if('Unsupported evaluation option'.EQ.' ')continue
      end if
      return
      end
      subroutine reaction_read(num)
      
      use xs_mod
      
      use rc_mod
      
      use pr_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer num
      integer fileid
      integer rank_ind_id
      integer rank_ind0_id
      integer dep_var_ind_id
      integer unit_string_id
      integer tag_string_id
      integer xs_symbol_string_id
      integer eval_name_id
      integer xs_data_size_id
      integer xs_data_ind_id
      integer xs_num_dep_var_id
      integer xs_rank_id
      integer xs_tab_index_id
      integer xs_data_base_id
      integer xs_data_inc_id
      integer xs_name_id
      integer xs_spacing_id
      integer xs_var_id
      integer xs_units_id
      integer xs_eval_name_id
      integer xs_min_id
      integer xs_max_id
      integer xs_mult_id
      integer xs_data_tab_id
      
      integer nc_dims(5),nc_corner(5),nc_edge(5),nc_stat,nc_size,nc_rank
     &,nc_attr,nc_type
      character*128 nc_dummy
      external ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf
     &_inq_format
      integer ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf_
     &inq_format
      integer old_size
      interface
      function mem_alloc_c1(size,l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1
      character(len=size),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_c2(size,l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2
      character(len=size),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_c3(size,l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3
      character(len=size),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_c4(size,l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,l4,u4
      character(len=size),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i1(l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      integer,dimension(:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i2(l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      integer,dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i3(l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      integer,dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i4(l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      integer,dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i5(l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      integer,dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r1(l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      REAL(kind=DOUBLE),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r2(l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      REAL(kind=DOUBLE),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r3(l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r4(l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      REAL(kind=DOUBLE),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r5(l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      REAL(kind=DOUBLE),dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_realloc_c1(p,size,l1,u1,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,nu
      character(len=size),dimension(:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_c2(p,size,l1,u1,l2,u2,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,nu
      character(len=size),dimension(:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_c3(p,size,l1,u1,l2,u2,l3,u3,nu,name)result(np
     &)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,nu
      character(len=size),dimension(:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_c4(p,size,l1,u1,l2,u2,l3,u3,l4,u4,nu,name)res
     &ult(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,l4,u4,nu
      character(len=size),dimension(:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i1(p,l1,u1,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,nu
      integer,dimension(:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i2(p,l1,u1,l2,u2,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,nu
      integer,dimension(:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i3(p,l1,u1,l2,u2,l3,u3,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,nu
      integer,dimension(:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i4(p,l1,u1,l2,u2,l3,u3,l4,u4,nu,name)result(n
     &p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,nu
      integer,dimension(:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu,name)re
     &sult(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu
      integer,dimension(:,:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r1(p,l1,u1,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,nu
      REAL(kind=DOUBLE),dimension(:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r2(p,l1,u1,l2,u2,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,nu
      REAL(kind=DOUBLE),dimension(:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r3(p,l1,u1,l2,u2,l3,u3,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,nu
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r4(p,l1,u1,l2,u2,l3,u3,l4,u4,nu,name)result(n
     &p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,nu
      REAL(kind=DOUBLE),dimension(:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu,name)re
     &sult(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu
      REAL(kind=DOUBLE),dimension(:,:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      subroutine mem_free_c1(p,size,l1,u1,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1
      character(len=size),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_c2(p,size,l1,u1,l2,u2,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2
      character(len=size),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_c3(p,size,l1,u1,l2,u2,l3,u3,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3
      character(len=size),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_c4(p,size,l1,u1,l2,u2,l3,u3,l4,u4,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,l4,u4
      character(len=size),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i1(p,l1,u1,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      integer,dimension(:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i2(p,l1,u1,l2,u2,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      integer,dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i3(p,l1,u1,l2,u2,l3,u3,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      integer,dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i4(p,l1,u1,l2,u2,l3,u3,l4,u4,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      integer,dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      integer,dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r1(p,l1,u1,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      REAL(kind=DOUBLE),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r2(p,l1,u1,l2,u2,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      REAL(kind=DOUBLE),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r3(p,l1,u1,l2,u2,l3,u3,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r4(p,l1,u1,l2,u2,l3,u3,l4,u4,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      REAL(kind=DOUBLE),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      REAL(kind=DOUBLE),dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      end interface
      
      
      fileid=ncopn(reaction_filename(problem_rc(num)),0,nc_stat)
      rank_ind_id=ncdid(fileid,'rank_ind',nc_stat)
      call ncdinq(fileid,rank_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((3)-(1)+1))continue
      rank_ind0_id=ncdid(fileid,'rank_ind0',nc_stat)
      call ncdinq(fileid,rank_ind0_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((3)-(0)+1))continue
      dep_var_ind_id=ncdid(fileid,'dep_var_ind',nc_stat)
      call ncdinq(fileid,dep_var_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((20)-(1)+1))continue
      unit_string_id=ncdid(fileid,'unit_string',nc_stat)
      call ncdinq(fileid,unit_string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((12)-(1)+1))continue
      tag_string_id=ncdid(fileid,'tag_string',nc_stat)
      call ncdinq(fileid,tag_string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((40)-(1)+1))continue
      xs_symbol_string_id=ncdid(fileid,'xs_symbol_string',nc_stat)
      call ncdinq(fileid,xs_symbol_string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((24)-(1)+1))continue
      eval_name_id=ncdid(fileid,'eval_name',nc_stat)
      call ncdinq(fileid,eval_name_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((40)-(1)+1))continue
      xs_data_size_id=ncvid(fileid,'xs_data_size',nc_stat)
      call ncvinq(fileid,xs_data_size_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      
      call ncvgt(fileid,xs_data_size_id,nc_corner,nc_edge,xs_data_size,n
     &c_stat)
      xs_data_ind_id=ncdid(fileid,'xs_data_ind',nc_stat)
      call ncdinq(fileid,xs_data_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((xs_data_size-1)-(0)+1))continue
      xs_num_dep_var_id=ncvid(fileid,'xs_num_dep_var',nc_stat)
      call ncvinq(fileid,xs_num_dep_var_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      
      call ncvgt(fileid,xs_num_dep_var_id,nc_corner,nc_edge,xs_num_dep_v
     &ar,nc_stat)
      xs_rank_id=ncvid(fileid,'xs_rank',nc_stat)
      call ncvinq(fileid,xs_rank_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_
     &attr,nc_stat)
      if(nc_dims(1).EQ.dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((20)-(1)+1)
      call ncvgt(fileid,xs_rank_id,nc_corner,nc_edge,xs_rank,nc_stat)
      xs_tab_index_id=ncvid(fileid,'xs_tab_index',nc_stat)
      call ncvinq(fileid,xs_tab_index_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      if(nc_dims(1).EQ.rank_ind_id)continue
      if(nc_dims(2).EQ.dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((20)-(1)+1)
      call ncvgt(fileid,xs_tab_index_id,nc_corner,nc_edge,xs_tab_index,n
     &c_stat)
      xs_data_base_id=ncvid(fileid,'xs_data_base',nc_stat)
      call ncvinq(fileid,xs_data_base_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      if(nc_dims(1).EQ.dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((20)-(1)+1)
      call ncvgt(fileid,xs_data_base_id,nc_corner,nc_edge,xs_data_base,n
     &c_stat)
      xs_data_inc_id=ncvid(fileid,'xs_data_inc',nc_stat)
      call ncvinq(fileid,xs_data_inc_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      if(nc_dims(1).EQ.dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((20)-(1)+1)
      call ncvgt(fileid,xs_data_inc_id,nc_corner,nc_edge,xs_data_inc,nc_
     &stat)
      xs_name_id=ncvid(fileid,'xs_name',nc_stat)
      call ncvinq(fileid,xs_name_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_
     &attr,nc_stat)
      if(nc_dims(1).EQ.xs_symbol_string_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((24)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,xs_name_id,nc_corner,nc_edge,xs_na
     &me)
      xs_spacing_id=ncvid(fileid,'xs_spacing',nc_stat)
      call ncvinq(fileid,xs_spacing_id,nc_dummy,nc_type,nc_rank,nc_dims,
     &nc_attr,nc_stat)
      if(nc_dims(1).EQ.tag_string_id)continue
      if(nc_dims(2).EQ.rank_ind0_id)continue
      if(nc_dims(3).EQ.dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((40)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((3)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((20)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,xs_spacing_id,nc_corner,nc_edge,xs
     &_spacing)
      xs_var_id=ncvid(fileid,'xs_var',nc_stat)
      call ncvinq(fileid,xs_var_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_a
     &ttr,nc_stat)
      if(nc_dims(1).EQ.tag_string_id)continue
      if(nc_dims(2).EQ.rank_ind0_id)continue
      if(nc_dims(3).EQ.dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((40)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((3)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((20)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,xs_var_id,nc_corner,nc_edge,xs_var
     &)
      xs_units_id=ncvid(fileid,'xs_units',nc_stat)
      call ncvinq(fileid,xs_units_id,nc_dummy,nc_type,nc_rank,nc_dims,nc
     &_attr,nc_stat)
      if(nc_dims(1).EQ.unit_string_id)continue
      if(nc_dims(2).EQ.rank_ind0_id)continue
      if(nc_dims(3).EQ.dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((12)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((3)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((20)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,xs_units_id,nc_corner,nc_edge,xs_u
     &nits)
      xs_eval_name_id=ncvid(fileid,'xs_eval_name',nc_stat)
      call ncvinq(fileid,xs_eval_name_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      if(nc_dims(1).EQ.eval_name_id)continue
      if(nc_dims(2).EQ.dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((40)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((20)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,xs_eval_name_id,nc_corner,nc_edge,
     &xs_eval_name)
      xs_min_id=ncvid(fileid,'xs_min',nc_stat)
      call ncvinq(fileid,xs_min_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_a
     &ttr,nc_stat)
      if(nc_dims(1).EQ.rank_ind_id)continue
      if(nc_dims(2).EQ.dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((20)-(1)+1)
      call ncvgt(fileid,xs_min_id,nc_corner,nc_edge,xs_min,nc_stat)
      xs_max_id=ncvid(fileid,'xs_max',nc_stat)
      call ncvinq(fileid,xs_max_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_a
     &ttr,nc_stat)
      if(nc_dims(1).EQ.rank_ind_id)continue
      if(nc_dims(2).EQ.dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((20)-(1)+1)
      call ncvgt(fileid,xs_max_id,nc_corner,nc_edge,xs_max,nc_stat)
      xs_mult_id=ncvid(fileid,'xs_mult',nc_stat)
      call ncvinq(fileid,xs_mult_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_
     &attr,nc_stat)
      if(nc_dims(1).EQ.rank_ind0_id)continue
      if(nc_dims(2).EQ.dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((3)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((20)-(1)+1)
      call ncvgt(fileid,xs_mult_id,nc_corner,nc_edge,xs_mult,nc_stat)
      xs_data_tab_id=ncvid(fileid,'xs_data_tab',nc_stat)
      call ncvinq(fileid,xs_data_tab_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      if(nc_dims(1).EQ.xs_data_ind_id)continue
      
      xs_data_tab =>mem_alloc_r1((0),(xs_data_size-1),'xs_data_tab')
      nc_corner(1)=1
      nc_edge(1)=((xs_data_size-1)-(0)+1)
      call ncvgt(fileid,xs_data_tab_id,nc_corner,nc_edge,xs_data_tab,nc_
     &stat)
      
      call ncclos(fileid,nc_stat)
      call xs_copy(num)
      call mem_free_r1(xs_data_tab,(0),(xs_data_size-1),'xs_data_tab')
      return
      end
      subroutine xs_copy(num)
      
      use xs_mod
      
      use pr_mod
      
      use rc_mod
      
      use rd_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer inc
      integer old_size
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      integer num
      integer i,j,jdep,k,rh_ind,num_random,i_var0
      integer spacing(0:20),var(20)
      REAL(kind=DOUBLE)table_entry,min(20),delta(20)
      character*40 eval_name
      interface
      function mem_alloc_c1(size,l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1
      character(len=size),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_c2(size,l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2
      character(len=size),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_c3(size,l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3
      character(len=size),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_c4(size,l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,l4,u4
      character(len=size),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i1(l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      integer,dimension(:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i2(l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      integer,dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i3(l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      integer,dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i4(l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      integer,dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i5(l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      integer,dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r1(l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      REAL(kind=DOUBLE),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r2(l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      REAL(kind=DOUBLE),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r3(l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r4(l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      REAL(kind=DOUBLE),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r5(l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      REAL(kind=DOUBLE),dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_realloc_c1(p,size,l1,u1,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,nu
      character(len=size),dimension(:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_c2(p,size,l1,u1,l2,u2,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,nu
      character(len=size),dimension(:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_c3(p,size,l1,u1,l2,u2,l3,u3,nu,name)result(np
     &)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,nu
      character(len=size),dimension(:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_c4(p,size,l1,u1,l2,u2,l3,u3,l4,u4,nu,name)res
     &ult(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,l4,u4,nu
      character(len=size),dimension(:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i1(p,l1,u1,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,nu
      integer,dimension(:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i2(p,l1,u1,l2,u2,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,nu
      integer,dimension(:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i3(p,l1,u1,l2,u2,l3,u3,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,nu
      integer,dimension(:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i4(p,l1,u1,l2,u2,l3,u3,l4,u4,nu,name)result(n
     &p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,nu
      integer,dimension(:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu,name)re
     &sult(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu
      integer,dimension(:,:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r1(p,l1,u1,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,nu
      REAL(kind=DOUBLE),dimension(:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r2(p,l1,u1,l2,u2,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,nu
      REAL(kind=DOUBLE),dimension(:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r3(p,l1,u1,l2,u2,l3,u3,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,nu
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r4(p,l1,u1,l2,u2,l3,u3,l4,u4,nu,name)result(n
     &p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,nu
      REAL(kind=DOUBLE),dimension(:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu,name)re
     &sult(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu
      REAL(kind=DOUBLE),dimension(:,:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      subroutine mem_free_c1(p,size,l1,u1,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1
      character(len=size),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_c2(p,size,l1,u1,l2,u2,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2
      character(len=size),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_c3(p,size,l1,u1,l2,u2,l3,u3,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3
      character(len=size),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_c4(p,size,l1,u1,l2,u2,l3,u3,l4,u4,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,l4,u4
      character(len=size),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i1(p,l1,u1,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      integer,dimension(:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i2(p,l1,u1,l2,u2,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      integer,dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i3(p,l1,u1,l2,u2,l3,u3,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      integer,dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i4(p,l1,u1,l2,u2,l3,u3,l4,u4,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      integer,dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      integer,dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r1(p,l1,u1,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      REAL(kind=DOUBLE),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r2(p,l1,u1,l2,u2,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      REAL(kind=DOUBLE),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r3(p,l1,u1,l2,u2,l3,u3,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r4(p,l1,u1,l2,u2,l3,u3,l4,u4,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      REAL(kind=DOUBLE),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      REAL(kind=DOUBLE),dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      end interface
      
      
      if(xs_name.NE.reaction_sy(problem_rc(num)))then
      write(6,*)' Warning: reaction ',reaction_sy(problem_rc(num)),' ref
     &ers to a reaction data file containing a different name, ',xs_name
      end if
      
      rh_ind=0
      reaction_rate_rank(num)=0
      reaction_rate_eval_name(num)='unknown'
      reaction_rate_num_rand(num)=0
      reaction_rate_spacing(0,num)=0
      do i=1,3
      reaction_rate_var(i,num)=0
      reaction_rate_spacing(i,num)=0
      reaction_rate_min(i,num)=(0.0_DOUBLE)
      reaction_rate_delta(i,num)=(0.0_DOUBLE)
      reaction_rate_tab_index(i,num)=0
      end do
      reaction_handling_num_rand(num)=0
      do j=1,19
      reaction_handling_rank(j,num)=0
      reaction_handling_eval_name(j,num)='unknown'
      reaction_handling_var0(j,num)=1
      reaction_handling_spacing(0,j,num)=0
      do i=1,3
      reaction_handling_var(i,j,num)=0
      reaction_handling_spacing(i,j,num)=0
      reaction_handling_min(i,j,num)=(0.0_DOUBLE)
      reaction_handling_delta(i,j,num)=(0.0_DOUBLE)
      reaction_handling_tab_index(i,j,num)=0
      end do
      end do
      
      do jdep=1,xs_num_dep_var
      if(xs_eval_name(jdep) (1:5).EQ.'table'.OR.xs_eval_name(jdep).EQ.'c
     &ramd')continue
      if(xs_eval_name(jdep) (1:5).EQ.'table')then
      eval_name='table'
      else
      eval_name=xs_eval_name(jdep)
      end if
      i=0
      if(xs_spacing(i,jdep).EQ.'linear')then
      spacing(i)=1
      else if(xs_spacing(i,jdep).EQ.'log')then
      spacing(i)=2
      else if(xs_spacing(i,jdep).EQ.'unknown')then
      spacing(i)=0
      end if
      num_random=0
      if(xs_rank(jdep).GT.0)then
      do i=1,xs_rank(jdep)
      if(xs_spacing(i,jdep).EQ.'linear')then
      spacing(i)=1
      else if(xs_spacing(i,jdep).EQ.'log')then
      spacing(i)=2
      else if(xs_spacing(i,jdep).EQ.'unknown')then
      spacing(i)=0
      end if
      if(xs_var(i,jdep).EQ.'density')then
      var(i)=1
      else if(xs_var(i,jdep).EQ.'temperature')then
      var(i)=3
      else if(xs_var(i,jdep).EQ.'energy')then
      var(i)=2
      else if(xs_var(i,jdep).EQ.'specific_temperature')then
      var(i)=5
      else if(xs_var(i,jdep).EQ.'specific_energy')then
      var(i)=4
      else if(xs_var(i,jdep).EQ.'plasma_zone')then
      var(i)=6
      else if(xs_var(i,jdep).EQ.'1st_random_number')then
      var(i)=7
      num_random=max(1,num_random)
      else if(xs_var(i,jdep).EQ.'v_test_1')then
      var(i)=8
      else if(xs_var(i,jdep).EQ.'v_test_2')then
      var(i)=9
      else if(xs_var(i,jdep).EQ.'v_test_3')then
      var(i)=10
      else if(xs_var(i,jdep).EQ.'v_flow_1')then
      var(i)=11
      else if(xs_var(i,jdep).EQ.'v_flow_2')then
      var(i)=12
      else if(xs_var(i,jdep).EQ.'v_flow_3')then
      var(i)=13
      else if(xs_var(i,jdep).EQ.'test_mass')then
      var(i)=14
      else if(xs_var(i,jdep).EQ.'back_mass')then
      var(i)=15
      else if(xs_var(i,jdep).EQ.'emitter_mass')then
      var(i)=16
      else if(xs_var(i,jdep).EQ.'v_flowb_1')then
      var(i)=17
      else if(xs_var(i,jdep).EQ.'v_flowb_2')then
      var(i)=18
      else if(xs_var(i,jdep).EQ.'v_flowb_3')then
      var(i)=19
      else if(xs_var(i,jdep).EQ.'elec_temperature')then
      var(i)=20
      end if
      if(spacing(i).EQ.1)then
      min(i)=xs_min(i,jdep)*xs_mult(i,jdep)
      delta(i)=(xs_max(i,jdep)-xs_min(i,jdep))*xs_mult(i,jdep)/REAL(xs_t
     &ab_index(i,jdep)-1,DOUBLE)
      else if(spacing(i).EQ.2)then
      min(i)=log(xs_min(i,jdep)*xs_mult(i,jdep))
      delta(i)=log(xs_max(i,jdep)/xs_min(i,jdep))/REAL(xs_tab_index(i,jd
     &ep)-1,DOUBLE)
      else
      min(i)=(0.0_DOUBLE)
      delta(i)=(0.0_DOUBLE)
      end if
      end do
      end if
      
      if(xs_var(0,jdep).EQ.'rate'.OR.xs_var(0,jdep).EQ.'reaction_rate')t
     &hen
      reaction_rate_eval_name(num)=eval_name
      reaction_rate_rank(num)=xs_rank(jdep)
      if(xs_rank(jdep).GT.0.OR.xs_rank(jdep).LE.3)continue
      reaction_rate_spacing(0,num)=spacing(0)
      reaction_rate_num_rand(num)=max(num_random,reaction_rate_num_rand(
     &num))
      do i=1,xs_rank(jdep)
      reaction_rate_tab_index(i,num)=xs_tab_index(i,jdep)
      reaction_rate_spacing(i,num)=spacing(i)
      reaction_rate_var(i,num)=var(i)
      reaction_rate_min(i,num)=min(i)
      reaction_rate_delta(i,num)=delta(i)
      end do
      if(xs_rank(jdep).LT.3)then
      do i=xs_rank(jdep)+1,3
      reaction_rate_tab_index(i,num)=1
      end do
      end if
      reaction_rate_base(num)=reaction_rate_size
      inc=1
      inc=inc*reaction_rate_tab_index(1,num)
      inc=inc*reaction_rate_tab_index(2,num)
      inc=inc*reaction_rate_tab_index(3,num)
      
      reaction_rate_tab =>mem_realloc_r1(reaction_rate_tab,(0),reaction_
     &rate_size-1,reaction_rate_size-1+inc,'reaction_rate_tab')
      reaction_rate_size=reaction_rate_size+inc
      else if(xs_var(0,jdep).EQ.'unknown')then
      goto 90010
      else
      rh_ind=rh_ind+1
      i_var0=string_lookup(xs_var(0,jdep),pr_var0_list,pr_var0_num)
      if(i_var0.GT.0)then
      reaction_handling_var0(rh_ind,num)=i_var0
      else
      pr_var0_num=pr_var0_num+1
      if(mod(((pr_var0_num)-(1)+1),100).EQ.1)then
      pr_var0_list =>mem_realloc_c1(pr_var0_list,(40),(1),(((int(((((pr_
     &var0_num)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((pr_var0_n
     &um)-(1)+1))+100-1)/100)*100)+(1)-1),'pr_var0_list')
      end if
      pr_var0_list(pr_var0_num)=xs_var(0,jdep)
      reaction_handling_var0(rh_ind,num)=pr_var0_num
      end if
      reaction_handling_eval_name(rh_ind,num)=eval_name
      reaction_handling_rank(rh_ind,num)=xs_rank(jdep)
      reaction_handling_spacing(0,rh_ind,num)=spacing(0)
      reaction_handling_num_rand(num)=max(num_random,reaction_handling_n
     &um_rand(num))
      if(xs_rank(jdep).GT.0)then
      do i=1,xs_rank(jdep)
      reaction_handling_tab_index(i,rh_ind,num)=xs_tab_index(i,jdep)
      reaction_handling_spacing(i,rh_ind,num)=spacing(i)
      reaction_handling_var(i,rh_ind,num)=var(i)
      reaction_handling_min(i,rh_ind,num)=min(i)
      reaction_handling_delta(i,rh_ind,num)=delta(i)
      end do
      end if
      if(xs_rank(jdep).LT.3)then
      do i=xs_rank(jdep)+1,3
      reaction_handling_tab_index(i,rh_ind,num)=1
      end do
      end if
      reaction_handling_base(rh_ind,num)=reaction_handling_size
      inc=1
      inc=inc*reaction_handling_tab_index(1,rh_ind,num)
      inc=inc*reaction_handling_tab_index(2,rh_ind,num)
      inc=inc*reaction_handling_tab_index(3,rh_ind,num)
      
      reaction_handling_tab =>mem_realloc_r1(reaction_handling_tab,(0),r
     &eaction_handling_size-1,reaction_handling_size-1+inc,'reaction_han
     &dling_tab')
      reaction_handling_size=reaction_handling_size+inc
      end if
      
      do k=0,xs_tab_index(3,jdep)-1
      do j=0,xs_tab_index(2,jdep)-1
      do i=0,xs_tab_index(1,jdep)-1
      table_entry=xs_data_tab(i+xs_tab_index(1,jdep)*(j+xs_tab_index(2,j
     &dep)*k)+xs_data_base(jdep))
      if(spacing(0).EQ.2)then
      if(table_entry.GT.(0.0_DOUBLE))then
      table_entry=log(table_entry*xs_mult(0,jdep))
      else
      table_entry=-(1000000.0_DOUBLE)
      end if
      else
      table_entry=table_entry*xs_mult(0,jdep)
      end if
      
      if(xs_var(0,jdep).EQ.'rate'.OR.xs_var(0,jdep).EQ.'reaction_rate')t
     &hen
      reaction_rate_tab(reaction_rate_base(num)+i+reaction_rate_tab_inde
     &x(1,num)*(j+reaction_rate_tab_index(2,num)*k))=table_entry
      else
      reaction_handling_tab(reaction_handling_base(rh_ind,num)+i+reactio
     &n_handling_tab_index(1,rh_ind,num)*(j+reaction_handling_tab_index(
     &2,rh_ind,num)*k))=table_entry
      end if
      end do
      end do
      end do
90010 continue
      end do
      return
      end
      subroutine init_reaction
      
      use pr_mod
      
      use rd_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer inc
      integer i
      interface
      function mem_alloc_c1(size,l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1
      character(len=size),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_c2(size,l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2
      character(len=size),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_c3(size,l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3
      character(len=size),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_c4(size,l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,l4,u4
      character(len=size),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i1(l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      integer,dimension(:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i2(l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      integer,dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i3(l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      integer,dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i4(l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      integer,dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i5(l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      integer,dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r1(l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      REAL(kind=DOUBLE),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r2(l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      REAL(kind=DOUBLE),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r3(l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r4(l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      REAL(kind=DOUBLE),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r5(l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      REAL(kind=DOUBLE),dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_realloc_c1(p,size,l1,u1,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,nu
      character(len=size),dimension(:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_c2(p,size,l1,u1,l2,u2,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,nu
      character(len=size),dimension(:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_c3(p,size,l1,u1,l2,u2,l3,u3,nu,name)result(np
     &)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,nu
      character(len=size),dimension(:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_c4(p,size,l1,u1,l2,u2,l3,u3,l4,u4,nu,name)res
     &ult(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,l4,u4,nu
      character(len=size),dimension(:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i1(p,l1,u1,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,nu
      integer,dimension(:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i2(p,l1,u1,l2,u2,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,nu
      integer,dimension(:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i3(p,l1,u1,l2,u2,l3,u3,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,nu
      integer,dimension(:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i4(p,l1,u1,l2,u2,l3,u3,l4,u4,nu,name)result(n
     &p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,nu
      integer,dimension(:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu,name)re
     &sult(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu
      integer,dimension(:,:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r1(p,l1,u1,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,nu
      REAL(kind=DOUBLE),dimension(:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r2(p,l1,u1,l2,u2,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,nu
      REAL(kind=DOUBLE),dimension(:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r3(p,l1,u1,l2,u2,l3,u3,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,nu
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r4(p,l1,u1,l2,u2,l3,u3,l4,u4,nu,name)result(n
     &p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,nu
      REAL(kind=DOUBLE),dimension(:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu,name)re
     &sult(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu
      REAL(kind=DOUBLE),dimension(:,:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      subroutine mem_free_c1(p,size,l1,u1,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1
      character(len=size),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_c2(p,size,l1,u1,l2,u2,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2
      character(len=size),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_c3(p,size,l1,u1,l2,u2,l3,u3,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3
      character(len=size),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_c4(p,size,l1,u1,l2,u2,l3,u3,l4,u4,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,l4,u4
      character(len=size),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i1(p,l1,u1,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      integer,dimension(:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i2(p,l1,u1,l2,u2,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      integer,dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i3(p,l1,u1,l2,u2,l3,u3,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      integer,dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i4(p,l1,u1,l2,u2,l3,u3,l4,u4,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      integer,dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      integer,dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r1(p,l1,u1,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      REAL(kind=DOUBLE),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r2(p,l1,u1,l2,u2,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      REAL(kind=DOUBLE),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r3(p,l1,u1,l2,u2,l3,u3,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r4(p,l1,u1,l2,u2,l3,u3,l4,u4,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      REAL(kind=DOUBLE),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      REAL(kind=DOUBLE),dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      end interface
      
      
      reaction_rate_eval_name =>mem_alloc_c1((40),(1),(pr_reaction_dim),
     &'reaction_rate_eval_name')
      reaction_rate_min =>mem_alloc_r2((1),(3),(1),(pr_reaction_dim),'re
     &action_rate_min')
      reaction_rate_delta =>mem_alloc_r2((1),(3),(1),(pr_reaction_dim),'
     &reaction_rate_delta')
      reaction_rate_rank =>mem_alloc_i1((1),(pr_reaction_dim),'reaction_
     &rate_rank')
      reaction_rate_spacing =>mem_alloc_i2((0),(3),(1),(pr_reaction_dim)
     &,'reaction_rate_spacing')
      reaction_rate_tab_index =>mem_alloc_i2((1),(3),(1),(pr_reaction_di
     &m),'reaction_rate_tab_index')
      reaction_rate_var =>mem_alloc_i2((1),(3),(1),(pr_reaction_dim),'re
     &action_rate_var')
      reaction_rate_num_rand =>mem_alloc_i1((1),(pr_reaction_dim),'react
     &ion_rate_num_rand')
      reaction_rate_size=0
      reaction_rate_base =>mem_alloc_i1((1),(pr_reaction_dim),'reaction_
     &rate_base')
      call init_base(reaction_rate_base,(((pr_reaction_dim)-(1)+1)))
      reaction_handling_eval_name =>mem_alloc_c2((40),(1),(19),(1),(pr_r
     &eaction_dim),'reaction_handling_eval_name')
      reaction_handling_min =>mem_alloc_r3((1),(3),(1),(19),(1),(pr_reac
     &tion_dim),'reaction_handling_min')
      reaction_handling_delta =>mem_alloc_r3((1),(3),(1),(19),(1),(pr_re
     &action_dim),'reaction_handling_delta')
      reaction_handling_rank =>mem_alloc_i2((1),(19),(1),(pr_reaction_di
     &m),'reaction_handling_rank')
      reaction_handling_spacing =>mem_alloc_i3((0),(3),(1),(19),(1),(pr_
     &reaction_dim),'reaction_handling_spacing')
      reaction_handling_tab_index =>mem_alloc_i3((1),(3),(1),(19),(1),(p
     &r_reaction_dim),'reaction_handling_tab_index')
      reaction_handling_var0 =>mem_alloc_i2((1),(19),(1),(pr_reaction_di
     &m),'reaction_handling_var0')
      reaction_handling_var =>mem_alloc_i3((1),(3),(1),(19),(1),(pr_reac
     &tion_dim),'reaction_handling_var')
      reaction_handling_num_rand =>mem_alloc_i1((1),(pr_reaction_dim),'r
     &eaction_handling_num_rand')
      reaction_handling_size=0
      reaction_handling_base =>mem_alloc_i2((1),(19),(1),(pr_reaction_di
     &m),'reaction_handling_base')
      call init_base(reaction_handling_base,(((19)-(1)+1)*((pr_reaction_
     &dim)-(1)+1)))
      if(pr_reaction_num.GT.0)then
      do i=1,pr_reaction_num
      call reaction_read(i)
      end do
      else
      reaction_rate_tab =>mem_realloc_r1(reaction_rate_tab,(0),reaction_
     &rate_size-1,reaction_rate_size-1+1,'reaction_rate_tab')
      reaction_rate_size=1
      reaction_handling_tab =>mem_realloc_r1(reaction_handling_tab,(0),r
     &eaction_handling_size-1,reaction_handling_size-1+1,'reaction_handl
     &ing_tab')
      reaction_handling_size=1
      end if
      return
      end
      
      
