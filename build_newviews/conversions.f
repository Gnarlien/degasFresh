      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      subroutine track_conversions(jscore,species_p,test_p,time_p,weight
     &_p,pos_p,cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,
     &sector_next_p,velocity_p,type_p,author_p,index_parameters,scoring_
     &data,converted_data)
      
      use pr_mod
      
      use tl_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer jscore
      integer species_p
      integer test_p
      REAL(kind=DOUBLE)time_p,weight_p,velocity_p(3)
      REAL(kind=DOUBLE)pos_p(3)
      integer cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,s
     &ector_next_p
      integer type_p,author_p
      integer index_parameters(0:100)
      REAL(kind=DOUBLE)scoring_data(280)
      REAL(kind=DOUBLE)converted_data(280)
      integer icv,j_tl_cv,k,ip,i,j
      REAL(kind=DOUBLE)cv_params(11),local_data(5),local_data_out(5)
      logical check_tally
      integer inc
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      do k=1,pr_var0_num
      converted_data(k)=scoring_data(k)
      end do
      
      if(tally_num_conversions(jscore).GT.0)continue
      call set_track_cv_params(jscore,index_parameters,species_p,test_p,
     &time_p,weight_p,pos_p(1),cell_p,zone_p,surface_p,cell_next_p,zone_
     &next_p,sector_p,sector_next_p,velocity_p(1),type_p,author_p,cv_par
     &ams)
      do j_tl_cv=1,tally_num_conversions(jscore)
      icv=tally_cv_ptr(j_tl_cv,jscore)
      if(tally_cv_type(icv).EQ.1)then
      i=0
      do ip=1,tally_cv_num_partners(icv)
      do j=1,tally_dep_var_dim(tally_cv_partners(ip,icv))
      i=i+1
      local_data(i)=converted_data(tally_dep_var(tally_cv_partners(ip,ic
     &v))+j-1+15*index_parameters(4))
      end do
      end do
      if(i.LE.5)continue
      call do_conversions(icv,cv_params,local_data,tally_dep_var_dim(tal
     &ly_cv_partners(1,icv)),local_data_out)
      
      i=0
      do j=1,tally_dep_var_dim(tally_cv_partners(1,icv))
      i=i+1
      converted_data(tally_dep_var(tally_cv_partners(1,icv))+j-1+15*inde
     &x_parameters(4))=local_data_out(i)
      end do
      end if
      end do
      return
      end
      subroutine final_conversions(type,scores)
      
      use pr_mod
      
      use tl_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer type
      REAL(kind=DOUBLE)scores(0:1,0:*)
      integer icv,jscore,ip,i,j,k,l,m,is,jv,dim1
      integer ind_val(5),index_parameters(0:100)
      REAL(kind=DOUBLE)check_data
      REAL(kind=DOUBLE)local_data(5),local_data_out(5),cv_params(11)
      logical check_tally
      integer inc
      do icv=1,nconversions
      if(tally_cv_type(icv).EQ.type)then
      jscore=tally_cv_partners(1,icv)
      if(jscore.GT.0)continue
      do i=0,100
      index_parameters(i)=(0.0_DOUBLE)
      end do
      
      if(5.EQ.5)continue
      do m=1,tally_tab_index(5,jscore)
      ind_val(5)=m-1
      index_parameters(tally_indep_var(5,jscore))=m
      do l=1,tally_tab_index(4,jscore)
      ind_val(4)=l-1
      index_parameters(tally_indep_var(4,jscore))=l
      do k=1,tally_tab_index(3,jscore)
      ind_val(3)=k-1
      index_parameters(tally_indep_var(3,jscore))=k
      do j=1,tally_tab_index(2,jscore)
      ind_val(2)=j-1
      index_parameters(tally_indep_var(2,jscore))=j
      do i=1,tally_tab_index(1,jscore)
      ind_val(1)=i-1
      index_parameters(tally_indep_var(1,jscore))=i
      call set_cv_params(jscore,index_parameters,cv_params)
      is=0
      check_data=(0.0_DOUBLE)
      dim1=tally_dep_var_dim(tally_cv_partners(1,icv))
      do ip=1,tally_cv_num_partners(icv)
      do jv=1,tally_dep_var_dim(tally_cv_partners(ip,icv))
      is=is+1
      local_data(is)=scores(0,tally_base(tally_cv_partners(ip,icv))+jv-1
     &+tally_dep_var_dim(tally_cv_partners(ip,icv))*(ind_val(1)+tally_ta
     &b_index(1,tally_cv_partners(ip,icv))*(ind_val(2)+tally_tab_index(2
     &,tally_cv_partners(ip,icv))*(ind_val(3)+tally_tab_index(3,tally_cv
     &_partners(ip,icv))*(ind_val(4)+tally_tab_index(4,tally_cv_partners
     &(ip,icv))*ind_val(5))))))
      check_data=check_data+(abs(local_data(is)))
      end do
      end do
      if(is.LE.5)continue
      if(check_data.GT.(0.0_DOUBLE))then
      call do_conversions(icv,cv_params,local_data,dim1,local_data_out)
      
      is=0
      do jv=1,dim1
      is=is+1
      scores(0,tally_base(tally_cv_partners(1,icv))+jv-1+tally_dep_var_d
     &im(tally_cv_partners(1,icv))*(ind_val(1)+tally_tab_index(1,tally_c
     &v_partners(1,icv))*(ind_val(2)+tally_tab_index(2,tally_cv_partners
     &(1,icv))*(ind_val(3)+tally_tab_index(3,tally_cv_partners(1,icv))*(
     &ind_val(4)+tally_tab_index(4,tally_cv_partners(1,icv))*ind_val(5))
     &))))=local_data_out(is)
      end do
      end if
      end do
      end do
      end do
      end do
      end do
      end if
      end do
      return
      end
      subroutine set_track_cv_params(jscore,index_parameters,species_p,t
     &est_p,time_p,weight_p,pos_p,cell_p,zone_p,surface_p,cell_next_p,zo
     &ne_next_p,sector_p,sector_next_p,velocity_p,type_p,author_p,cv_par
     &ams)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer jscore
      integer index_parameters(0:100)
      integer species_p
      integer test_p
      REAL(kind=DOUBLE)time_p,weight_p,velocity_p(3)
      REAL(kind=DOUBLE)pos_p(3)
      integer cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,s
     &ector_next_p
      integer type_p,author_p
      REAL(kind=DOUBLE)cv_params(11)
      integer i
      do i=1,11
      cv_params(i)=(0.0_DOUBLE)
      end do
      cv_params(5)=pos_p(1)
      cv_params(6)=pos_p(2)
      cv_params(7)=pos_p(3)
      return
      end
      subroutine set_cv_params(jscore,index_parameters,cv_params)
      
      use pr_mod
      
      use tl_mod
      
      use sp_mod
      
      use zn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer jscore
      integer index_parameters(0:100)
      REAL(kind=DOUBLE)cv_params(11)
      integer k
      integer zone
      logical check_tally
      integer inc
      logical check_zone
      
      do k=1,11
      cv_params(k)=(0.0_DOUBLE)
      end do
      
      cv_params(4)=(1.3332e-1_DOUBLE)
      cv_params(11)=(1.5_DOUBLE)
      if((index_parameters(3).GT.0.AND.index_parameters(3).LE.pr_test_nu
     &m))cv_params(1)=species_m(problem_test_sp(index_parameters(3)))
      if((index_parameters(4).GT.0.AND.index_parameters(4).LE.pr_backgro
     &und_num))then
      cv_params(2)=species_m(problem_background_sp(index_parameters(4)))
      else if((index_parameters(4)-pr_background_num.GT.0.AND.index_para
     &meters(4)-pr_background_num.LE.pr_test_num))then
      cv_params(2)=species_m(problem_test_sp(index_parameters(4)-pr_back
     &ground_num))
      end if
      zone=index_parameters(1)
      if(check_zone(zone))then
      cv_params(3)=zone_volume(zone)
      if(zone_type(zone).EQ.2)then
      cv_params(8)=zone_center(1,zone)
      cv_params(9)=zone_center(2,zone)
      cv_params(10)=zone_center(3,zone)
      end if
      end if
      
      return
      end
      subroutine do_conversions(icv,cv_params,local_data,dim,local_data_
     &out)
      
      use gi_mod
      
      use bk_mod
      
      use pr_mod
      
      use tl_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer icv,dim
      REAL(kind=DOUBLE)cv_params(11),local_data(5)
      REAL(kind=DOUBLE)local_data_out(5)
      integer i
      REAL(kind=DOUBLE)x(3)
      logical check_tally
      integer inc
      do i=1,5
      local_data_out(i)=(0.0_DOUBLE)
      end do
      if(tally_cv_action(icv).EQ.1)then
      if(cv_params(tally_cv_scalers(1,icv)).NE.(0.0_DOUBLE))then
      do i=1,dim
      local_data_out(i)=local_data(i)/cv_params(tally_cv_scalers(1,icv))
      end do
      else
      do i=1,dim
      local_data_out(i)=local_data(i)
      end do
      end if
      else if(tally_cv_action(icv).EQ.4)then
      if(tally_cv_ptr(1,tally_cv_partners(2,icv)).LT.icv)continue
      if(local_data(dim+1).NE.(0.0_DOUBLE))then
      do i=1,dim
      local_data_out(i)=local_data(i)/local_data(dim+1)
      end do
      else
      do i=1,dim
      local_data_out(i)=local_data(i)
      end do
      end if
      local_data_out(dim+1)=local_data(dim+1)
      else if(tally_cv_action(icv).EQ.3)then
      if(dim.EQ.3)continue
      x(1)=cv_params(8)
      x(2)=cv_params(9)
      x(3)=cv_params(10)
      if((geometry_symmetry.EQ.2.OR.geometry_symmetry.EQ.5.OR.geometry_s
     &ymmetry.EQ.6).AND.(background_coords.EQ.2).AND.(x(1)**2+x(2)**2.GT
     &.(0.0_DOUBLE)))then
      local_data_out(1)=(local_data(1)*x(1)-local_data(2)*x(2))/sqrt(x(1
     &)**2+x(2)**2)
      local_data_out(2)=(local_data(1)*x(2)+local_data(2)*x(1))/sqrt(x(1
     &)**2+x(2)**2)
      local_data_out(3)=local_data(3)
      else
      if(.NOT.((geometry_symmetry.EQ.0).AND.(background_coords.EQ.2)))co
     &ntinue
      local_data_out(1)=local_data(1)
      local_data_out(2)=local_data(2)
      local_data_out(3)=local_data(3)
      
      end if
      else if(tally_cv_action(icv).EQ.2)then
      if(dim.EQ.3)continue
      x(1)=cv_params(5)
      x(2)=cv_params(6)
      x(3)=cv_params(7)
      if((geometry_symmetry.EQ.2.OR.geometry_symmetry.EQ.5.OR.geometry_s
     &ymmetry.EQ.6).AND.(background_coords.EQ.2).AND.(x(1)**2+x(2)**2.GT
     &.(0.0_DOUBLE)))then
      local_data_out(1)=(local_data(1)*x(1)+local_data(2)*x(2))/sqrt(x(1
     &)**2+x(2)**2)
      local_data_out(2)=(-local_data(1)*x(2)+local_data(2)*x(1))/sqrt(x(
     &1)**2+x(2)**2)
      local_data_out(3)=local_data(3)
      else
      if(.NOT.((geometry_symmetry.EQ.0).AND.(background_coords.EQ.2)))co
     &ntinue
      local_data_out(1)=local_data(1)
      local_data_out(2)=local_data(2)
      local_data_out(3)=local_data(3)
      
      end if
      else
      if('Unsupported conversion'.EQ.' ')continue
      end if
      return
      end
      
      
