      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      function find_yield(species_p,test_p,time_p,weight_p,pos_p,cell_p,
     &zone_p,surface_p,cell_next_p,zone_next_p,sector_p,sector_next_p,ve
     &locity_p,type_p,author_p,ts_pmi,independent_parameters,ran_index_x
     &,ran_array_x)
      
      use pd_mod
      
      use pr_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)find_yield
      integer species_p
      integer test_p
      REAL(kind=DOUBLE)time_p,weight_p,velocity_p(3)
      REAL(kind=DOUBLE)pos_p(3)
      integer cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,s
     &ector_next_p
      integer type_p,author_p
      integer ts_pmi
      REAL(kind=DOUBLE)independent_parameters(10)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      external intersection_direction,pd_eval_data
      REAL(kind=DOUBLE)intersection_direction,pd_eval_data
      integer i
      REAL(kind=DOUBLE)ran
      integer pmi_sub
      pmi_sub=problem_pmi_cases(ts_pmi,test_p)
      
      if(pmi_yield_num_rand(pmi_sub).GT.0)then
      if(pmi_yield_num_rand(pmi_sub).LE.3)continue
      call random_array(independent_parameters(7),pmi_yield_num_rand(pmi
     &_sub),ran_index_x,ran_array_x(0))
      end if
      find_yield=pd_eval_data(pmi_yield_eval_name(pmi_sub),pmi_yield_ran
     &k(pmi_sub),pmi_yield_var(1,pmi_sub),pmi_yield_tab_index(1,pmi_sub)
     &,pmi_yield_spacing(0,pmi_sub),pmi_yield_min(1,pmi_sub),pmi_yield_d
     &elta(1,pmi_sub),pmi_yield_tab(0),pmi_yield_base(pmi_sub),independe
     &nt_parameters,ran_index_x,ran_array_x(0))
      return
      end
      subroutine pick_pmi(ts_pmi,independent_parameters,nprod,species_pr
     &od,test_prod,time_prod,weight_prod,pos_prod,cell_prod,zone_prod,su
     &rface_prod,cell_next_prod,zone_next_prod,sector_prod,sector_next_p
     &rod,velocity_prod,type_prod,author_prod,ran_index_x,ran_array_x)
      
      use pr_mod
      
      use pd_mod
      
      use pm_mod
      
      use sp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer ts_pmi
      REAL(kind=DOUBLE)independent_parameters(10)
      integer nprod
      integer species_prod(0:4)
      integer test_prod(0:4)
      REAL(kind=DOUBLE)time_prod(0:4),weight_prod(0:4),velocity_prod(3,0
     &:4)
      REAL(kind=DOUBLE)pos_prod(3,0:4)
      integer cell_prod(0:4),zone_prod(0:4),surface_prod(0:4),cell_next_
     &prod(0:4),zone_next_prod(0:4),sector_prod(0:4),sector_next_prod(0:
     &4)
      integer type_prod(0:4),author_prod(0:4)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer i
      REAL(kind=DOUBLE)ran
      REAL(kind=DOUBLE)v_temp(3)
      integer pmi_sub
      external intersection_direction
      REAL(kind=DOUBLE)intersection_direction
      pmi_sub=problem_pmi_cases(ts_pmi,test_prod(0))
      
      if(pmi_handling_num_rand(pmi_sub).GT.0)then
      if(pmi_handling_num_rand(pmi_sub).LE.3)continue
      call random_array(independent_parameters(7),pmi_handling_num_rand(
     &pmi_sub),ran_index_x,ran_array_x(0))
      end if
      
      if(pmi_type(problem_pmi_ref(pmi_sub)).EQ.'reflection')then
      call reflection(test_prod(0),ts_pmi,independent_parameters,nprod,s
     &pecies_prod(1),velocity_prod(1,1),weight_prod(1),ran_index_x,ran_a
     &rray_x(0))
      else if(pmi_type(problem_pmi_ref(pmi_sub)).EQ.'desorption')then
      call desorption(test_prod(0),ts_pmi,independent_parameters,nprod,s
     &pecies_prod(1),velocity_prod(1,1),weight_prod(1),ran_index_x,ran_a
     &rray_x(0))
      else if(pmi_type(problem_pmi_ref(pmi_sub)).EQ.'adsorption')then
      call adsorption(test_prod(0),ts_pmi,independent_parameters,nprod,s
     &pecies_prod(1),velocity_prod(1,1),weight_prod(1),ran_index_x,ran_a
     &rray_x(0))
      else
      if('Unsupported pmi type'.EQ.' ')continue
      end if
      if(nprod.GT.0)then
      do i=1,nprod
      call surface_reflect(surface_prod(0),pos_prod(1,0),velocity_prod(1
     &,0),velocity_prod(1,i),v_temp(1))
      velocity_prod(1,i)=v_temp(1)
      velocity_prod(2,i)=v_temp(2)
      velocity_prod(3,i)=v_temp(3)
      
      test_prod(i)=problem_species_test(species_prod(i))
      pos_prod(1,i)=pos_prod(1,0)
      pos_prod(2,i)=pos_prod(2,0)
      pos_prod(3,i)=pos_prod(3,0)
      
      cell_prod(i)=cell_prod(0)
      zone_prod(i)=zone_prod(0)
      surface_prod(i)=surface_prod(0)
      cell_next_prod(i)=cell_next_prod(0)
      zone_next_prod(i)=zone_next_prod(0)
      sector_prod(i)=sector_prod(0)
      sector_next_prod(i)=sector_next_prod(0)
      cell_prod(i)=cell_next_prod(0)
      cell_next_prod(i)=cell_prod(0)
      zone_prod(i)=zone_next_prod(0)
      zone_next_prod(i)=zone_prod(0)
      sector_prod(i)=sector_next_prod(0)
      sector_next_prod(i)=sector_prod(0)
      time_prod(i)=time_prod(0)
      weight_prod(i)=weight_prod(0)*weight_prod(i)
      author_prod(i)=6+pr_reaction_num+pmi_sub
      if(species_z(species_prod(i)).EQ.0)then
      type_prod(i)=1
      else
      type_prod(i)=2
      end if
      end do
      end if
      return
      end
      subroutine reflection(test,ts_pmi,independent_parameters,nprod,pro
     &d,v_prod,w_prod,ran_index_x,ran_array_x)
      
      use pd_mod
      
      use pm_mod
      
      use pr_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer test
      integer ts_pmi
      REAL(kind=DOUBLE)independent_parameters(10)
      integer nprod
      integer prod(4)
      REAL(kind=DOUBLE)v_prod(3,4)
      REAL(kind=DOUBLE)w_prod(4)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer pmi_sub
      pmi_sub=problem_pmi_cases(ts_pmi,test)
      if(pmi_product_num(problem_pmi_ref(pmi_sub)).EQ.1)continue
      if(problem_pmi_num_arrange(ts_pmi,test).EQ.1)continue
      if(pmi_generic(problem_pmi_ref(pmi_sub)).EQ.1)then
      if(problem_pmi_products(1,1,ts_pmi,test).EQ.problem_test_sp(test))
     &continue
      end if
      nprod=1
      prod(1)=problem_pmi_products(1,1,ts_pmi,test)
      call pd_eval_v_product(pmi_sub,independent_parameters,prod(1),v_pr
     &od(1,1),ran_index_x,ran_array_x(0))
      w_prod(1)=(1.0_DOUBLE)
      return
      end
      subroutine desorption(test,ts_pmi,independent_parameters,nprod,pro
     &d,v_prod,w_prod,ran_index_x,ran_array_x)
      
      use pd_mod
      
      use pr_mod
      
      use sp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer test
      integer ts_pmi
      REAL(kind=DOUBLE)independent_parameters(10)
      integer nprod
      integer prod(4)
      REAL(kind=DOUBLE)v_prod(3,4)
      REAL(kind=DOUBLE)w_prod(4)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer j,arr,wt_factor
      REAL(kind=DOUBLE)ran
      REAL(kind=DOUBLE)random
      external random
      integer pmi_sub
      pmi_sub=problem_pmi_cases(ts_pmi,test)
      nprod=1
      
      if(problem_pmi_num_arrange(ts_pmi,test).GE.1)continue
      ran=random(ran_index_x,ran_array_x(0))
      arr=ran*problem_pmi_num_arrange(ts_pmi,test)+1
      arr=max(arr,1)
      arr=min(arr,problem_pmi_num_arrange(ts_pmi,test))
      prod(1)=problem_pmi_products(1,arr,ts_pmi,test)
      call pd_eval_v_product(pmi_sub,independent_parameters,prod(1),v_pr
     &od(1,1),ran_index_x,ran_array_x(0))
      
      if((species_ncomp(problem_test_sp(test)).EQ.1).AND.(species_count(
     &1,species_generic(problem_test_sp(test))).EQ.1))then
      wt_factor=0
      do j=1,species_ncomp(species_generic(prod(1)))
      if(species_el(1,species_generic(problem_test_sp(test))).EQ.species
     &_el(j,species_generic(prod(1))))then
      wt_factor=wt_factor+(species_count(j,species_generic(prod(1))))
      end if
      end do
      else
      wt_factor=1
      end if
      if(wt_factor.GT.0)continue
      w_prod(1)=(1.0_DOUBLE)/wt_factor
      return
      end
      subroutine adsorption(test,ts_pmi,independent_parameters,nprod,pro
     &d,v_prod,w_prod,ran_index_x,ran_array_x)
      
      use pd_mod
      
      use pm_mod
      
      use sp_mod
      
      use pr_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer test
      integer ts_pmi
      REAL(kind=DOUBLE)independent_parameters(10)
      integer nprod
      integer prod(4)
      REAL(kind=DOUBLE)v_prod(3,4)
      REAL(kind=DOUBLE)w_prod(4)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer pmi_sub
      pmi_sub=problem_pmi_cases(ts_pmi,test)
      
      if(pmi_product_num(problem_pmi_ref(pmi_sub)).EQ.0)continue
      nprod=0
      prod(1)=0
      v_prod(1,1)=(0.0_DOUBLE)
      v_prod(2,1)=(0.0_DOUBLE)
      v_prod(3,1)=(0.0_DOUBLE)
      w_prod(1)=(0.0_DOUBLE)
      return
      end
      subroutine pd_eval_v_product(pmi_sub,independent_parameters,produc
     &t,v_product,ran_index_x,ran_array_x)
      
      use pd_mod
      
      use sp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer pmi_sub
      REAL(kind=DOUBLE)independent_parameters(10)
      integer product
      REAL(kind=DOUBLE)v_product(3)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer j
      REAL(kind=DOUBLE)energy,sqrt_energy,velocity,cos_theta,sin_theta,c
     &os_phi,sin_phi,ran
      logical cos_phi_set,cos_theta_set,v_set,vhat_set,v1_set,v2_set,v3_
     &set
      REAL(kind=DOUBLE)vhat(3)
      external pd_eval_data
      REAL(kind=DOUBLE)pd_eval_data
      REAL(kind=DOUBLE)random
      external random
      
      cos_phi_set=.FALSE.
      cos_theta_set=.FALSE.
      v_set=.FALSE.
      vhat_set=.FALSE.
      
      v1_set=.FALSE.
      v2_set=.FALSE.
      v3_set=.FALSE.
      do j=1,4
      if(pmi_handling_var0(j,pmi_sub).EQ.'energy_out')then
      energy=pd_eval_data(pmi_handling_eval_name(j,pmi_sub),pmi_handling
     &_rank(j,pmi_sub),pmi_handling_var(1,j,pmi_sub),pmi_handling_tab_in
     &dex(1,j,pmi_sub),pmi_handling_spacing(0,j,pmi_sub),pmi_handling_mi
     &n(1,j,pmi_sub),pmi_handling_delta(1,j,pmi_sub),pmi_handling_tab(0)
     &,pmi_handling_base(j,pmi_sub),independent_parameters,ran_index_x,r
     &an_array_x(0))
      velocity=sqrt((2.0_DOUBLE)*energy/species_m(product))
      v_set=.TRUE.
      else if(pmi_handling_var0(j,pmi_sub).EQ.'sqrt_energy_out')then
      sqrt_energy=pd_eval_data(pmi_handling_eval_name(j,pmi_sub),pmi_han
     &dling_rank(j,pmi_sub),pmi_handling_var(1,j,pmi_sub),pmi_handling_t
     &ab_index(1,j,pmi_sub),pmi_handling_spacing(0,j,pmi_sub),pmi_handli
     &ng_min(1,j,pmi_sub),pmi_handling_delta(1,j,pmi_sub),pmi_handling_t
     &ab(0),pmi_handling_base(j,pmi_sub),independent_parameters,ran_inde
     &x_x,ran_array_x(0))
      velocity=sqrt_energy*sqrt((2.0_DOUBLE)/species_m(product))
      v_set=.TRUE.
      else if(pmi_handling_var0(j,pmi_sub).EQ.'cos_polar_angle_out')then
      cos_theta=pd_eval_data(pmi_handling_eval_name(j,pmi_sub),pmi_handl
     &ing_rank(j,pmi_sub),pmi_handling_var(1,j,pmi_sub),pmi_handling_tab
     &_index(1,j,pmi_sub),pmi_handling_spacing(0,j,pmi_sub),pmi_handling
     &_min(1,j,pmi_sub),pmi_handling_delta(1,j,pmi_sub),pmi_handling_tab
     &(0),pmi_handling_base(j,pmi_sub),independent_parameters,ran_index_
     &x,ran_array_x(0))
      if(cos_theta.LE.(1.0_DOUBLE))continue
      sin_theta=sqrt((1.0_DOUBLE)-cos_theta**2)
      cos_theta_set=.TRUE.
      else if(pmi_handling_var0(j,pmi_sub).EQ.'cos_azi_angle_out')then
      cos_phi=pd_eval_data(pmi_handling_eval_name(j,pmi_sub),pmi_handlin
     &g_rank(j,pmi_sub),pmi_handling_var(1,j,pmi_sub),pmi_handling_tab_i
     &ndex(1,j,pmi_sub),pmi_handling_spacing(0,j,pmi_sub),pmi_handling_m
     &in(1,j,pmi_sub),pmi_handling_delta(1,j,pmi_sub),pmi_handling_tab(0
     &),pmi_handling_base(j,pmi_sub),independent_parameters,ran_index_x,
     &ran_array_x(0))
      if(abs(cos_phi).LE.(1.0_DOUBLE))continue
      sin_phi=sqrt((1.0_DOUBLE)-cos_phi**2)
      ran=random(ran_index_x,ran_array_x(0))
      if(ran.GT.0.5)sin_phi=-sin_phi
      cos_phi_set=.TRUE.
      else if(pmi_handling_var0(j,pmi_sub).EQ.'velocity_vector')then
      call pd_eval_vhat(pmi_handling_eval_name(j,pmi_sub),pmi_handling_r
     &ank(j,pmi_sub),pmi_handling_var(1,j,pmi_sub),pmi_handling_tab_inde
     &x(1,j,pmi_sub),pmi_handling_spacing(0,j,pmi_sub),pmi_handling_min(
     &1,j,pmi_sub),pmi_handling_delta(1,j,pmi_sub),pmi_handling_tab(0),p
     &mi_handling_base(j,pmi_sub),independent_parameters,ran_index_x,ran
     &_array_x(0),vhat(1))
      vhat_set=.TRUE.
      end if
      end do
      if((v_set.AND.cos_theta_set.AND.cos_phi_set).OR.(v_set.AND.vhat_se
     &t).OR.(v1_set.AND.v2_set.AND.v3_set))continue
      if(.NOT.vhat_set)then
      vhat(1)=sin_theta*cos_phi
      vhat(2)=sin_theta*sin_phi
      vhat(3)=cos_theta
      vhat_set=.TRUE.
      end if
      if((v_set.AND.vhat_set).OR.(v1_set.AND.v2_set.AND.v3_set))continue
      if(.NOT.v1_set)then
      v_product(1)=velocity*vhat(1)
      v1_set=.TRUE.
      end if
      if(.NOT.v2_set)then
      v_product(2)=velocity*vhat(2)
      v2_set=.TRUE.
      end if
      if(.NOT.v3_set)then
      v_product(3)=velocity*vhat(3)
      v3_set=.TRUE.
      end if
      if(v1_set.AND.v2_set.AND.v3_set)continue
      return
      end
      function pd_eval_data(eval_name,rank,ind_params_label,tab_index,sp
     &acing,min,delta,data_table,tab_base,independent_parameters,ran_ind
     &ex_x,ran_array_x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)pd_eval_data
      character*40 eval_name
      integer rank,ind_params_label(5),spacing(0:5),tab_index(5),tab_bas
     &e
      REAL(kind=DOUBLE)ind_params_value(5),min(5),delta(5),data_table(0:
     &*),independent_parameters(10)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer i
      external interpolate1,interpolate2,interpolate3,interpolate4,inter
     &polate5,pd_eval_fit
      REAL(kind=DOUBLE)interpolate1,interpolate2,interpolate3,interpolat
     &e4,interpolate5,pd_eval_fit
      if(rank.GT.0)then
      do i=1,rank
      if(ind_params_label(i).LE.10)continue
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
      pd_eval_data=data_table(0+tab_index(1)*(0+tab_index(2)*(0+tab_inde
     &x(3)*(0+tab_index(4)*0)))+tab_base)
      else if(rank.EQ.1)then
      pd_eval_data=interpolate1(ind_params_value(1),data_table(0+tab_ind
     &ex(1)*(0+tab_index(2)*(0+tab_index(3)*(0+tab_index(4)*0)))+tab_bas
     &e),tab_index(1))
      else if(rank.EQ.2)then
      pd_eval_data=interpolate2(ind_params_value,data_table(0+tab_index(
     &1)*(0+tab_index(2)*(0+tab_index(3)*(0+tab_index(4)*0)))+tab_base),
     &tab_index(1))
      else if(rank.EQ.3)then
      pd_eval_data=interpolate3(ind_params_value,data_table(0+tab_index(
     &1)*(0+tab_index(2)*(0+tab_index(3)*(0+tab_index(4)*0)))+tab_base),
     &tab_index(1))
      else if(rank.EQ.4)then
      pd_eval_data=interpolate4(ind_params_value,data_table(0+tab_index(
     &1)*(0+tab_index(2)*(0+tab_index(3)*(0+tab_index(4)*0)))+tab_base),
     &tab_index(1))
      else if(rank.EQ.5)then
      pd_eval_data=interpolate5(ind_params_value,data_table(0+tab_index(
     &1)*(0+tab_index(2)*(0+tab_index(3)*(0+tab_index(4)*0)))+tab_base),
     &tab_index(1))
      end if
      if(spacing(0).EQ.2)pd_eval_data=exp(pd_eval_data)
      else
      pd_eval_data=pd_eval_fit(eval_name,rank,ind_params_label,tab_index
     &(1),data_table(0+tab_index(1)*(0+tab_index(2)*(0+tab_index(3)*(0+t
     &ab_index(4)*0)))+tab_base),ind_params_value,ran_index_x,ran_array_
     &x(0))
      end if
      return
      end
      subroutine pd_eval_vhat(eval_name,rank,ind_params_label,tab_index,
     &spacing,min,delta,data_table,tab_base,independent_parameters,ran_i
     &ndex_x,ran_array_x,vhat)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character*40 eval_name
      integer rank,ind_params_label(5),spacing(0:5),tab_index(5),tab_bas
     &e
      REAL(kind=DOUBLE)ind_params_value(5),min(5),delta(5),data_table(0:
     &*),independent_parameters(10)
      REAL(kind=DOUBLE)vhat(3)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer i
      if(rank.GT.0)then
      do i=1,rank
      if(ind_params_label(i).LE.10)continue
      ind_params_value(i)=independent_parameters(ind_params_label(i))
      end do
      end if
      if(eval_name.NE.'table')continue
      call pd_eval_v_fit(eval_name,rank,ind_params_label,tab_index(1),da
     &ta_table(0+tab_index(1)*(0+tab_index(2)*(0+tab_index(3)*(0+tab_ind
     &ex(4)*0)))+tab_base),ind_params_value,ran_index_x,ran_array_x(0),v
     &hat(1))
      return
      end
      subroutine pd_eval_v_fit(eval_name,rank,ind_params_label,tab_index
     &,data_table,ind_params_value,ran_index_x,ran_array_x,vhat)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character*40 eval_name
      integer rank,ind_params_label(5),tab_index
      REAL(kind=DOUBLE)ind_params_value(5),data_table(0:*)
      REAL(kind=DOUBLE)vhat(3)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      if(eval_name.EQ.'cosine')then
      call cosine(rank,ind_params_label,tab_index,data_table(0),ind_para
     &ms_value,ran_index_x,ran_array_x(0),vhat(1))
      else if(eval_name.EQ.'specular')then
      call specular(rank,ind_params_label,tab_index,data_table(0),ind_pa
     &rams_value,ran_index_x,ran_array_x(0),vhat(1))
      else if(eval_name.EQ.'cosspec')then
      call cosspec(rank,ind_params_label,tab_index,data_table(0),ind_par
     &ams_value,ran_index_x,ran_array_x(0),vhat(1))
      else if(eval_name.EQ.'cosine_p')then
      call cosine_p(rank,ind_params_label,tab_index,data_table(0),ind_pa
     &rams_value,ran_index_x,ran_array_x(0),vhat(1))
      else if(eval_name.EQ.'maxwell_flux')then
      call maxwell_flux(rank,ind_params_label,tab_index,data_table(0),in
     &d_params_value,ran_index_x,ran_array_x(0),vhat(1))
      else
      if('Unsupported velocity distribution'.EQ.' ')continue
      end if
      return
      end
      function pd_eval_fit(eval_name,rank,ind_params_label,tab_index,dat
     &a_table,ind_params_value,ran_index_x,ran_array_x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)pd_eval_fit
      character*40 eval_name
      integer rank,ind_params_label(5),tab_index
      REAL(kind=DOUBLE)ind_params_value(5),data_table(0:*)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      external refl_ruzic
      REAL(kind=DOUBLE)refl_ruzic
      external e_maxwell
      REAL(kind=DOUBLE)e_maxwell
      if(eval_name.EQ.'refl_ruzic')then
      pd_eval_fit=refl_ruzic(rank,ind_params_label,tab_index,data_table(
     &0),ind_params_value,ran_index_x,ran_array_x(0))
      else if(eval_name.EQ.'e_maxwell')then
      pd_eval_fit=e_maxwell(rank,ind_params_label,tab_index,data_table(0
     &),ind_params_value,ran_index_x,ran_array_x(0))
      else
      if('Unsupported fit option'.EQ.' ')continue
      end if
      return
      end
      function refl_ruzic(rank,ind_params_label,tab_index,data_table,ind
     &_params_value,ran_index_x,ran_array_x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)refl_ruzic
      integer rank,ind_params_label(5),tab_index
      REAL(kind=DOUBLE)ind_params_value(5),data_table(0:*)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      REAL(kind=DOUBLE)energy_in,polar_angle,energy_min,energy_delta
      external interpolate1
      REAL(kind=DOUBLE)interpolate1
      
      if(rank.EQ.2)continue
      if(tab_index.EQ.48)continue
      if(ind_params_label(1).EQ.1)continue
      if(ind_params_label(2).EQ.5)continue
      energy_in=log(ind_params_value(1))
      polar_angle=ind_params_value(2)
      
      energy_min=log((1.60217733e-19_DOUBLE))
      energy_delta=(0.1_DOUBLE)*log((10.0_DOUBLE))
      
      energy_in=(energy_in-energy_min)/energy_delta
      
      refl_ruzic=interpolate1(energy_in,data_table(0),tab_index)
      
      refl_ruzic=refl_ruzic*((1.0_DOUBLE)+(0.5_DOUBLE)*sin(polar_angle))
      refl_ruzic=min(refl_ruzic,(0.9_DOUBLE))
      return
      end
      function e_maxwell(rank,ind_params_label,tab_index,data_table,ind_
     &params_value,ran_index_x,ran_array_x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)e_maxwell
      integer rank,ind_params_label(5),tab_index
      REAL(kind=DOUBLE)ind_params_value(5),data_table(0:*)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      REAL(kind=DOUBLE)ewall
      REAL(kind=DOUBLE)ran(1)
      integer i
      
      if(rank.EQ.1)continue
      if(ind_params_label(1).EQ.6)continue
      ewall=(1.5_DOUBLE)*ind_params_value(1)
      e_maxwell=(0.0_DOUBLE)
      do i=1,3
      call random_gauss(ran,1,ran_index_x,ran_array_x(0))
      e_maxwell=e_maxwell+(ran(1)*ran(1))
      end do
      e_maxwell=e_maxwell*(ewall)
      return
      end
      subroutine cosine(rank,ind_params_label,tab_index,data_table,ind_p
     &arams_value,ran_index_x,ran_array_x,vhat)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer rank,ind_params_label(5),tab_index
      REAL(kind=DOUBLE)ind_params_value(5),data_table(0:*)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      REAL(kind=DOUBLE)vhat(3)
      
      if(rank.EQ.0)continue
      call random_cosdist(vhat,1,ran_index_x,ran_array_x(0))
      return
      end
      subroutine specular(rank,ind_params_label,tab_index,data_table,ind
     &_params_value,ran_index_x,ran_array_x,vhat)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer rank,ind_params_label(5),tab_index
      REAL(kind=DOUBLE)ind_params_value(5),data_table(0:*)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      REAL(kind=DOUBLE)vhat(3)
      
      if(rank.EQ.1)continue
      if(ind_params_label(1).EQ.5)continue
      vhat(1)=sin(ind_params_value(1))
      vhat(2)=(0.0_DOUBLE)
      vhat(3)=cos(ind_params_value(1))
      return
      end
      subroutine cosspec(rank,ind_params_label,tab_index,data_table,ind_
     &params_value,ran_index_x,ran_array_x,vhat)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer rank,ind_params_label(5),tab_index
      REAL(kind=DOUBLE)ind_params_value(5),data_table(0:*)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      REAL(kind=DOUBLE)vhat(3)
      REAL(kind=DOUBLE)p,alpha,cos_alpha,sin_alpha,xi,sin_theta,sin_thet
     &a_hat,cos_theta_hat,phi,cos_phi,sin_phi
      REAL(kind=DOUBLE)random
      external random
      
      if(rank.EQ.1)continue
      if(tab_index.EQ.1)continue
      if(ind_params_label(1).EQ.5)continue
      p=data_table(0)
      alpha=ind_params_value(1)
      cos_alpha=cos(alpha)
      sin_alpha=sin(alpha)
      xi=random(ran_index_x,ran_array_x(0))
      sin_theta=sqrt((1.0_DOUBLE)-xi**((2.0_DOUBLE)/((1.0_DOUBLE)+p)))
      if(sin_theta.LT.(0.0_DOUBLE))sin_theta=(0.0_DOUBLE)
      if(sin_theta.GT.(1.0_DOUBLE))sin_theta=(1.0_DOUBLE)
      sin_theta_hat=cos_alpha*sin_theta
      cos_theta_hat=sqrt((1.0_DOUBLE)-sin_theta_hat**2)
      phi=random(ran_index_x,ran_array_x(0))
      phi=atan2((0.0_DOUBLE),-(1.0_DOUBLE))*((2.0_DOUBLE)*phi-(1.0_DOUBL
     &E))
      cos_phi=cos(phi)
      sin_phi=sin(phi)
      vhat(1)=sin_theta_hat*cos_phi*cos_alpha+cos_theta_hat*sin_alpha
      vhat(2)=sin_theta_hat*sin_phi
      vhat(3)=cos_theta_hat*cos_alpha-sin_theta_hat*cos_phi*sin_alpha
      return
      end
      subroutine cosine_p(rank,ind_params_label,tab_index,data_table,ind
     &_params_value,ran_index_x,ran_array_x,vhat)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer rank,ind_params_label(5),tab_index
      REAL(kind=DOUBLE)ind_params_value(5),data_table(0:*)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      REAL(kind=DOUBLE)vhat(3)
      REAL(kind=DOUBLE)p,xi,sin_theta,cos_theta,phi,cos_phi,sin_phi
      REAL(kind=DOUBLE)random
      external random
      
      if(rank.EQ.0)continue
      if(tab_index.EQ.1)continue
      p=data_table(0)
      xi=random(ran_index_x,ran_array_x(0))
      cos_theta=xi**((1.0_DOUBLE)/((1.0_DOUBLE)+p))
      sin_theta=sqrt((1.0_DOUBLE)-xi**((2.0_DOUBLE)/((1.0_DOUBLE)+p)))
      if(sin_theta.LT.(0.0_DOUBLE))sin_theta=(0.0_DOUBLE)
      if(sin_theta.GT.(1.0_DOUBLE))sin_theta=(1.0_DOUBLE)
      phi=random(ran_index_x,ran_array_x(0))
      phi=atan2((0.0_DOUBLE),-(1.0_DOUBLE))*((2.0_DOUBLE)*phi-(1.0_DOUBL
     &E))
      cos_phi=cos(phi)
      sin_phi=sin(phi)
      vhat(1)=sin_theta*cos_phi
      vhat(2)=sin_theta*sin_phi
      vhat(3)=cos_theta
      return
      end
      subroutine maxwell_flux(rank,ind_params_label,tab_index,data_table
     &,ind_params_value,ran_index_x,ran_array_x,vhat)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer rank,ind_params_label(5),tab_index
      REAL(kind=DOUBLE)ind_params_value(5),data_table(0:*)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      REAL(kind=DOUBLE)vhat(3)
      REAL(kind=DOUBLE)ran,log_ran
      REAL(kind=DOUBLE)random
      external random
      
      if(rank.EQ.0)continue
      call random_gauss(vhat,2,ran_index_x,ran_array_x(0))
      ran=random(ran_index_x,ran_array_x(0))
      log_ran=log(ran)
      vhat(3)=sqrt(-(log_ran+log_ran))
      return
      end
      subroutine pmi_read(num)
      
      use pf_mod
      
      use pm_mod
      
      use pr_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer num
      integer fileid
      integer old_size
      integer pf_rank_ind_id
      integer pf_rank_ind0_id
      integer pf_dep_var_ind_id
      integer pf_unit_string_id
      integer pf_tag_string_id
      integer pf_symbol_string_id
      integer pf_eval_string_id
      integer pf_data_size_id
      integer pf_data_ind_id
      integer pf_num_dep_var_id
      integer pf_rank_id
      integer pf_tab_index_id
      integer pf_data_base_id
      integer pf_data_inc_id
      integer pf_name_id
      integer pf_spacing_id
      integer pf_var_id
      integer pf_units_id
      integer pf_eval_name_id
      integer pf_min_id
      integer pf_max_id
      integer pf_mult_id
      integer pf_data_tab_id
      
      integer nc_dims(5),nc_corner(5),nc_edge(5),nc_stat,nc_size,nc_rank
     &,nc_attr,nc_type
      character*128 nc_dummy
      external ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf
     &_inq_format
      integer ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf_
     &inq_format
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
      
      
      fileid=ncopn(pmi_filename(problem_pmi_ref(num)),0,nc_stat)
      pf_rank_ind_id=ncdid(fileid,'pf_rank_ind',nc_stat)
      call ncdinq(fileid,pf_rank_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((5)-(1)+1))continue
      pf_rank_ind0_id=ncdid(fileid,'pf_rank_ind0',nc_stat)
      call ncdinq(fileid,pf_rank_ind0_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((5)-(0)+1))continue
      pf_dep_var_ind_id=ncdid(fileid,'pf_dep_var_ind',nc_stat)
      call ncdinq(fileid,pf_dep_var_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((5)-(1)+1))continue
      pf_unit_string_id=ncdid(fileid,'pf_unit_string',nc_stat)
      call ncdinq(fileid,pf_unit_string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((12)-(1)+1))continue
      pf_tag_string_id=ncdid(fileid,'pf_tag_string',nc_stat)
      call ncdinq(fileid,pf_tag_string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((20)-(1)+1))continue
      pf_symbol_string_id=ncdid(fileid,'pf_symbol_string',nc_stat)
      call ncdinq(fileid,pf_symbol_string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((24)-(1)+1))continue
      pf_eval_string_id=ncdid(fileid,'pf_eval_string',nc_stat)
      call ncdinq(fileid,pf_eval_string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((40)-(1)+1))continue
      pf_data_size_id=ncvid(fileid,'pf_data_size',nc_stat)
      call ncvinq(fileid,pf_data_size_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      
      call ncvgt(fileid,pf_data_size_id,nc_corner,nc_edge,pf_data_size,n
     &c_stat)
      pf_data_ind_id=ncdid(fileid,'pf_data_ind',nc_stat)
      call ncdinq(fileid,pf_data_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((pf_data_size-1)-(0)+1))continue
      pf_num_dep_var_id=ncvid(fileid,'pf_num_dep_var',nc_stat)
      call ncvinq(fileid,pf_num_dep_var_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      
      call ncvgt(fileid,pf_num_dep_var_id,nc_corner,nc_edge,pf_num_dep_v
     &ar,nc_stat)
      pf_rank_id=ncvid(fileid,'pf_rank',nc_stat)
      call ncvinq(fileid,pf_rank_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_
     &attr,nc_stat)
      if(nc_dims(1).EQ.pf_dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      call ncvgt(fileid,pf_rank_id,nc_corner,nc_edge,pf_rank,nc_stat)
      pf_tab_index_id=ncvid(fileid,'pf_tab_index',nc_stat)
      call ncvinq(fileid,pf_tab_index_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pf_rank_ind_id)continue
      if(nc_dims(2).EQ.pf_dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((5)-(1)+1)
      call ncvgt(fileid,pf_tab_index_id,nc_corner,nc_edge,pf_tab_index,n
     &c_stat)
      pf_data_base_id=ncvid(fileid,'pf_data_base',nc_stat)
      call ncvinq(fileid,pf_data_base_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pf_dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      call ncvgt(fileid,pf_data_base_id,nc_corner,nc_edge,pf_data_base,n
     &c_stat)
      pf_data_inc_id=ncvid(fileid,'pf_data_inc',nc_stat)
      call ncvinq(fileid,pf_data_inc_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pf_dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      call ncvgt(fileid,pf_data_inc_id,nc_corner,nc_edge,pf_data_inc,nc_
     &stat)
      pf_name_id=ncvid(fileid,'pf_name',nc_stat)
      call ncvinq(fileid,pf_name_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_
     &attr,nc_stat)
      if(nc_dims(1).EQ.pf_symbol_string_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((24)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,pf_name_id,nc_corner,nc_edge,pf_na
     &me)
      pf_spacing_id=ncvid(fileid,'pf_spacing',nc_stat)
      call ncvinq(fileid,pf_spacing_id,nc_dummy,nc_type,nc_rank,nc_dims,
     &nc_attr,nc_stat)
      if(nc_dims(1).EQ.pf_tag_string_id)continue
      if(nc_dims(2).EQ.pf_rank_ind0_id)continue
      if(nc_dims(3).EQ.pf_dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((20)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((5)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((5)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,pf_spacing_id,nc_corner,nc_edge,pf
     &_spacing)
      pf_var_id=ncvid(fileid,'pf_var',nc_stat)
      call ncvinq(fileid,pf_var_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_a
     &ttr,nc_stat)
      if(nc_dims(1).EQ.pf_tag_string_id)continue
      if(nc_dims(2).EQ.pf_rank_ind0_id)continue
      if(nc_dims(3).EQ.pf_dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((20)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((5)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((5)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,pf_var_id,nc_corner,nc_edge,pf_var
     &)
      pf_units_id=ncvid(fileid,'pf_units',nc_stat)
      call ncvinq(fileid,pf_units_id,nc_dummy,nc_type,nc_rank,nc_dims,nc
     &_attr,nc_stat)
      if(nc_dims(1).EQ.pf_unit_string_id)continue
      if(nc_dims(2).EQ.pf_rank_ind0_id)continue
      if(nc_dims(3).EQ.pf_dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((12)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((5)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((5)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,pf_units_id,nc_corner,nc_edge,pf_u
     &nits)
      pf_eval_name_id=ncvid(fileid,'pf_eval_name',nc_stat)
      call ncvinq(fileid,pf_eval_name_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pf_eval_string_id)continue
      if(nc_dims(2).EQ.pf_dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((40)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((5)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,pf_eval_name_id,nc_corner,nc_edge,
     &pf_eval_name)
      pf_min_id=ncvid(fileid,'pf_min',nc_stat)
      call ncvinq(fileid,pf_min_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_a
     &ttr,nc_stat)
      if(nc_dims(1).EQ.pf_rank_ind_id)continue
      if(nc_dims(2).EQ.pf_dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((5)-(1)+1)
      call ncvgt(fileid,pf_min_id,nc_corner,nc_edge,pf_min,nc_stat)
      pf_max_id=ncvid(fileid,'pf_max',nc_stat)
      call ncvinq(fileid,pf_max_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_a
     &ttr,nc_stat)
      if(nc_dims(1).EQ.pf_rank_ind_id)continue
      if(nc_dims(2).EQ.pf_dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((5)-(1)+1)
      call ncvgt(fileid,pf_max_id,nc_corner,nc_edge,pf_max,nc_stat)
      pf_mult_id=ncvid(fileid,'pf_mult',nc_stat)
      call ncvinq(fileid,pf_mult_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_
     &attr,nc_stat)
      if(nc_dims(1).EQ.pf_rank_ind0_id)continue
      if(nc_dims(2).EQ.pf_dep_var_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((5)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((5)-(1)+1)
      call ncvgt(fileid,pf_mult_id,nc_corner,nc_edge,pf_mult,nc_stat)
      pf_data_tab_id=ncvid(fileid,'pf_data_tab',nc_stat)
      call ncvinq(fileid,pf_data_tab_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pf_data_ind_id)continue
      
      pf_data_tab =>mem_alloc_r1((0),(pf_data_size-1),'pf_data_tab')
      nc_corner(1)=1
      nc_edge(1)=((pf_data_size-1)-(0)+1)
      call ncvgt(fileid,pf_data_tab_id,nc_corner,nc_edge,pf_data_tab,nc_
     &stat)
      
      call ncclos(fileid,nc_stat)
      call pf_copy(num)
      call mem_free_r1(pf_data_tab,(0),(pf_data_size-1),'pf_data_tab')
      return
      end
      subroutine pf_copy(num)
      
      use pf_mod
      
      use pr_mod
      
      use pm_mod
      
      use pd_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer old_size
      integer num
      integer i,j,jdep,k,l,m,ph_ind,num_random
      integer spacing(0:5),var(5)
      REAL(kind=DOUBLE)table_entry,min(5),delta(5)
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
      
      
      if(pf_name.NE.pmi_sy(problem_pmi_ref(num)))then
      write(6,*)'Warning: PMI ',pmi_sy(problem_pmi_ref(num)),' refers to
     & a PMI data file containing a different name, ',pf_name
      end if
      
      ph_ind=0
      pmi_yield_rank(num)=0
      pmi_yield_eval_name(num)='unknown'
      pmi_yield_num_rand(num)=0
      pmi_yield_spacing(0,num)=0
      do i=1,5
      pmi_yield_var(i,num)=0
      pmi_yield_spacing(i,num)=0
      pmi_yield_min(i,num)=(0.0_DOUBLE)
      pmi_yield_delta(i,num)=(0.0_DOUBLE)
      pmi_yield_tab_index(i,num)=0
      end do
      pmi_handling_num_rand(num)=0
      do j=1,4
      pmi_handling_rank(j,num)=0
      pmi_handling_eval_name(j,num)='unknown'
      pmi_handling_var0(j,num)='unknown'
      pmi_handling_spacing(0,j,num)=0
      do i=1,5
      pmi_handling_var(i,j,num)=0
      pmi_handling_spacing(i,j,num)=0
      pmi_handling_min(i,j,num)=(0.0_DOUBLE)
      pmi_handling_delta(i,j,num)=(0.0_DOUBLE)
      pmi_handling_tab_index(i,j,num)=0
      end do
      end do
      
      do jdep=1,pf_num_dep_var
      eval_name=pf_eval_name(jdep)
      i=0
      if(pf_spacing(i,jdep).EQ.'linear')then
      spacing(i)=1
      else if(pf_spacing(i,jdep).EQ.'log')then
      spacing(i)=2
      else if(pf_spacing(i,jdep).EQ.'unknown')then
      spacing(i)=0
      end if
      num_random=0
      if(pf_rank(jdep).GT.0)then
      do i=1,pf_rank(jdep)
      if(pf_spacing(i,jdep).EQ.'linear')then
      spacing(i)=1
      else if(pf_spacing(i,jdep).EQ.'log')then
      spacing(i)=2
      else if(pf_spacing(i,jdep).EQ.'unknown')then
      spacing(i)=0
      end if
      if(pf_var(i,jdep).EQ.'energy_in')then
      var(i)=1
      else if(pf_var(i,jdep).EQ.'vel_1_in')then
      var(i)=2
      else if(pf_var(i,jdep).EQ.'vel_2_in')then
      var(i)=3
      else if(pf_var(i,jdep).EQ.'vel_3_in')then
      var(i)=4
      else if(pf_var(i,jdep).EQ.'polar_angle_in')then
      var(i)=5
      else if(pf_var(i,jdep).EQ.'cos_polar_angle_in')then
      var(i)=10
      else if(pf_var(i,jdep).EQ.'t_wall')then
      var(i)=6
      else if(pf_var(i,jdep).EQ.'1st_random_number')then
      var(i)=7
      num_random=max(1,num_random)
      else if(pf_var(i,jdep).EQ.'2nd_random_number')then
      var(i)=8
      num_random=max(2,num_random)
      else if(pf_var(i,jdep).EQ.'3rd_random_number')then
      var(i)=9
      num_random=max(3,num_random)
      else
      if('Unknown independent variable'.EQ.' ')continue
      end if
      if(spacing(i).EQ.1)then
      min(i)=pf_min(i,jdep)*pf_mult(i,jdep)
      delta(i)=(pf_max(i,jdep)-pf_min(i,jdep))*pf_mult(i,jdep)/REAL(pf_t
     &ab_index(i,jdep)-1,DOUBLE)
      else if(spacing(i).EQ.2)then
      min(i)=log(pf_min(i,jdep)*pf_mult(i,jdep))
      delta(i)=log(pf_max(i,jdep)/pf_min(i,jdep))/REAL(pf_tab_index(i,jd
     &ep)-1,DOUBLE)
      else
      min(i)=(0.0_DOUBLE)
      delta(i)=(0.0_DOUBLE)
      end if
      end do
      end if
      
      if(pf_var(0,jdep).EQ.'reflection_coef'.OR.pf_var(0,jdep).EQ.'yield
     &')then
      pmi_yield_eval_name(num)=eval_name
      pmi_yield_rank(num)=pf_rank(jdep)
      if(pf_rank(jdep).GT.0.OR.pf_rank(jdep).LE.5)continue
      pmi_yield_spacing(0,num)=spacing(0)
      pmi_yield_num_rand(num)=max(num_random,pmi_yield_num_rand(num))
      do i=1,pf_rank(jdep)
      pmi_yield_tab_index(i,num)=pf_tab_index(i,jdep)
      pmi_yield_spacing(i,num)=spacing(i)
      pmi_yield_var(i,num)=var(i)
      pmi_yield_min(i,num)=min(i)
      pmi_yield_delta(i,num)=delta(i)
      end do
      if(pf_rank(jdep).LT.5)then
      do i=pf_rank(jdep)+1,5
      pmi_yield_tab_index(i,num)=1
      end do
      end if
      pmi_yield_base(num)=pmi_yield_size
      pd_inc=1
      pd_inc=pd_inc*pmi_yield_tab_index(1,num)
      pd_inc=pd_inc*pmi_yield_tab_index(2,num)
      pd_inc=pd_inc*pmi_yield_tab_index(3,num)
      pd_inc=pd_inc*pmi_yield_tab_index(4,num)
      pd_inc=pd_inc*pmi_yield_tab_index(5,num)
      
      pmi_yield_tab =>mem_realloc_r1(pmi_yield_tab,(0),pmi_yield_size-1,
     &pmi_yield_size-1+pd_inc,'pmi_yield_tab')
      pmi_yield_size=pmi_yield_size+pd_inc
      else if(pf_var(0,jdep).EQ.'unknown')then
      goto 90010
      else
      ph_ind=ph_ind+1
      pmi_handling_var0(ph_ind,num)=pf_var(0,jdep)
      pmi_handling_eval_name(ph_ind,num)=eval_name
      pmi_handling_rank(ph_ind,num)=pf_rank(jdep)
      pmi_handling_spacing(0,ph_ind,num)=spacing(0)
      pmi_handling_num_rand(num)=max(num_random,pmi_handling_num_rand(nu
     &m))
      if(pf_rank(jdep).GT.0)then
      do i=1,pf_rank(jdep)
      pmi_handling_tab_index(i,ph_ind,num)=pf_tab_index(i,jdep)
      pmi_handling_spacing(i,ph_ind,num)=spacing(i)
      pmi_handling_var(i,ph_ind,num)=var(i)
      pmi_handling_min(i,ph_ind,num)=min(i)
      pmi_handling_delta(i,ph_ind,num)=delta(i)
      end do
      end if
      if(pf_rank(jdep).LT.5)then
      do i=pf_rank(jdep)+1,5
      pmi_handling_tab_index(i,ph_ind,num)=1
      end do
      end if
      pmi_handling_base(ph_ind,num)=pmi_handling_size
      pd_inc=1
      pd_inc=pd_inc*pmi_handling_tab_index(1,ph_ind,num)
      pd_inc=pd_inc*pmi_handling_tab_index(2,ph_ind,num)
      pd_inc=pd_inc*pmi_handling_tab_index(3,ph_ind,num)
      pd_inc=pd_inc*pmi_handling_tab_index(4,ph_ind,num)
      pd_inc=pd_inc*pmi_handling_tab_index(5,ph_ind,num)
      
      pmi_handling_tab =>mem_realloc_r1(pmi_handling_tab,(0),pmi_handlin
     &g_size-1,pmi_handling_size-1+pd_inc,'pmi_handling_tab')
      pmi_handling_size=pmi_handling_size+pd_inc
      end if
      
      do m=0,pf_tab_index(5,jdep)-1
      do l=0,pf_tab_index(4,jdep)-1
      do k=0,pf_tab_index(3,jdep)-1
      do j=0,pf_tab_index(2,jdep)-1
      do i=0,pf_tab_index(1,jdep)-1
      table_entry=pf_data_tab(i+pf_tab_index(1,jdep)*(j+pf_tab_index(2,j
     &dep)*(k+pf_tab_index(3,jdep)*(l+pf_tab_index(4,jdep)*m)))+pf_data_
     &base(jdep))
      if(spacing(0).EQ.2)then
      if(table_entry.GT.(0.0_DOUBLE))then
      table_entry=log(table_entry*pf_mult(0,jdep))
      else
      table_entry=-(1000000.0_DOUBLE)
      end if
      else
      table_entry=table_entry*pf_mult(0,jdep)
      end if
      
      if(pf_var(0,jdep).EQ.'reflection_coef'.OR.pf_var(0,jdep).EQ.'yield
     &')then
      pmi_yield_tab(i+pmi_yield_tab_index(1,num)*(j+pmi_yield_tab_index(
     &2,num)*(k+pmi_yield_tab_index(3,num)*(l+pmi_yield_tab_index(4,num)
     &*m)))+pmi_yield_base(num))=table_entry
      else
      pmi_handling_tab(i+pmi_handling_tab_index(1,ph_ind,num)*(j+pmi_han
     &dling_tab_index(2,ph_ind,num)*(k+pmi_handling_tab_index(3,ph_ind,n
     &um)*(l+pmi_handling_tab_index(4,ph_ind,num)*m)))+pmi_handling_base
     &(ph_ind,num))=table_entry
      end if
      end do
      end do
      end do
      end do
      end do
90010 continue
      end do
      return
      end
      subroutine init_pmi
      
      use pr_mod
      
      use pd_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
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
      
      
      pmi_yield_min =>mem_alloc_r2((1),(5),(1),(pr_pmi_num),'pmi_yield_m
     &in')
      pmi_yield_delta =>mem_alloc_r2((1),(5),(1),(pr_pmi_num),'pmi_yield
     &_delta')
      pmi_yield_rank =>mem_alloc_i1((1),(pr_pmi_num),'pmi_yield_rank')
      pmi_yield_spacing =>mem_alloc_i2((0),(5),(1),(pr_pmi_num),'pmi_yie
     &ld_spacing')
      pmi_yield_eval_name =>mem_alloc_c1((40),(1),(pr_pmi_num),'pmi_yiel
     &d_eval_name')
      pmi_yield_tab_index =>mem_alloc_i2((1),(5),(1),(pr_pmi_num),'pmi_y
     &ield_tab_index')
      pmi_yield_var =>mem_alloc_i2((1),(5),(1),(pr_pmi_num),'pmi_yield_v
     &ar')
      pmi_yield_num_rand =>mem_alloc_i1((1),(pr_pmi_num),'pmi_yield_num_
     &rand')
      pmi_yield_size=0
      pmi_yield_base =>mem_alloc_i1((1),(pr_pmi_num),'pmi_yield_base')
      call init_base(pmi_yield_base,(((pr_pmi_num)-(1)+1)))
      pmi_handling_min =>mem_alloc_r3((1),(5),(1),(4),(1),(pr_pmi_num),'
     &pmi_handling_min')
      pmi_handling_delta =>mem_alloc_r3((1),(5),(1),(4),(1),(pr_pmi_num)
     &,'pmi_handling_delta')
      pmi_handling_rank =>mem_alloc_i2((1),(4),(1),(pr_pmi_num),'pmi_han
     &dling_rank')
      pmi_handling_spacing =>mem_alloc_i3((0),(5),(1),(4),(1),(pr_pmi_nu
     &m),'pmi_handling_spacing')
      pmi_handling_eval_name =>mem_alloc_c2((40),(1),(4),(1),(pr_pmi_num
     &),'pmi_handling_eval_name')
      pmi_handling_tab_index =>mem_alloc_i3((1),(5),(1),(4),(1),(pr_pmi_
     &num),'pmi_handling_tab_index')
      pmi_handling_var0 =>mem_alloc_c2((20),(1),(4),(1),(pr_pmi_num),'pm
     &i_handling_var0')
      pmi_handling_var =>mem_alloc_i3((1),(5),(1),(4),(1),(pr_pmi_num),'
     &pmi_handling_var')
      pmi_handling_num_rand =>mem_alloc_i1((1),(pr_pmi_num),'pmi_handlin
     &g_num_rand')
      pmi_handling_size=0
      pmi_handling_base =>mem_alloc_i2((1),(4),(1),(pr_pmi_num),'pmi_han
     &dling_base')
      call init_base(pmi_handling_base,(((4)-(1)+1)*((pr_pmi_num)-(1)+1)
     &))
      do i=1,pr_pmi_num
      call pmi_read(i)
      end do
      return
      end
      
      
