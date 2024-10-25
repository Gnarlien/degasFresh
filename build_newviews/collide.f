      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      subroutine pick_reaction(test,ts_reac,rate,independent_parameters,
     &nprod,prod,v_prod,w_prod,scoring_data,ran_index_x,ran_array_x)
      
      use rc_mod
      
      use pr_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer test
      integer ts_reac
      REAL(kind=DOUBLE)rate
      REAL(kind=DOUBLE)independent_parameters(20)
      integer nprod
      integer prod(4)
      REAL(kind=DOUBLE)v_prod(3,4)
      REAL(kind=DOUBLE)w_prod(4),scoring_data(280)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer pr_reac
      pr_reac=problem_test_reaction(ts_reac,test)
      if(reaction_type(problem_rc(pr_reac)).EQ.'chargex')then
      call chargex(test,ts_reac,rate,independent_parameters,nprod,prod,v
     &_prod(1,1),w_prod,scoring_data,ran_index_x,ran_array_x(0))
      else if(reaction_type(problem_rc(pr_reac)).EQ.'elastic')then
      call elastic(test,ts_reac,rate,independent_parameters,nprod,prod,v
     &_prod(1,1),w_prod,scoring_data,ran_index_x,ran_array_x(0))
      else if(reaction_type(problem_rc(pr_reac)).EQ.'bgk_elastic')then
      call bgk_elastic(test,ts_reac,rate,independent_parameters,nprod,pr
     &od,v_prod(1,1),w_prod,scoring_data,ran_index_x,ran_array_x(0))
      else if(reaction_type(problem_rc(pr_reac)).EQ.'dissoc')then
      call dissoc(test,ts_reac,rate,independent_parameters,nprod,prod,v_
     &prod(1,1),w_prod,scoring_data,ran_index_x,ran_array_x(0))
      else if(reaction_type(problem_rc(pr_reac)).EQ.'dissoc_rec')then
      call dissoc(test,ts_reac,rate,independent_parameters,nprod,prod,v_
     &prod(1,1),w_prod,scoring_data,ran_index_x,ran_array_x(0))
      else if(reaction_type(problem_rc(pr_reac)).EQ.'dissoc_cramd')then
      call dissoc_cramd(test,ts_reac,rate,independent_parameters,nprod,p
     &rod,v_prod(1,1),w_prod,scoring_data,ran_index_x,ran_array_x(0))
      else if(reaction_type(problem_rc(pr_reac)).EQ.'ionize')then
      call ionize(test,ts_reac,rate,independent_parameters,nprod,prod,v_
     &prod(1,1),w_prod,scoring_data,ran_index_x,ran_array_x(0))
      else if(reaction_type(problem_rc(pr_reac)).EQ.'excitation')then
      call excite(test,ts_reac,rate,independent_parameters,nprod,prod,v_
     &prod(1,1),w_prod,scoring_data,ran_index_x,ran_array_x(0))
      else if(reaction_type(problem_rc(pr_reac)).EQ.'deexcitation')then
      call excite(test,ts_reac,rate,independent_parameters,nprod,prod,v_
     &prod(1,1),w_prod,scoring_data,ran_index_x,ran_array_x(0))
      else if(reaction_type(problem_rc(pr_reac)).EQ.'ion_conv')then
      call ion_conv(test,ts_reac,rate,independent_parameters,nprod,prod,
     &v_prod(1,1),w_prod,scoring_data,ran_index_x,ran_array_x(0))
      else
      if('Unsupported reaction type'.EQ.' ')continue
      end if
      return
      end
      subroutine chargex(test,ts_reac,rate,independent_parameters,nprod,
     &prod,v_prod,w_prod,scoring_data,ran_index_x,ran_array_x)
      
      use pr_mod
      
      use rc_mod
      
      use rd_mod
      
      use sp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer inc
      REAL(kind=DOUBLE)random
      external random
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      integer test
      integer ts_reac
      REAL(kind=DOUBLE)rate
      REAL(kind=DOUBLE)independent_parameters(20)
      integer nprod
      integer prod(4)
      REAL(kind=DOUBLE)v_prod(3,4)
      REAL(kind=DOUBLE)w_prod(4),scoring_data(280)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer pr_reac
      integer back
      integer back_i
      integer test_i
      integer i,i_cx,i_sigv_max,jdep_cx,count,psp_i
      REAL(kind=DOUBLE)v_test(3)
      REAL(kind=DOUBLE)v_back(3)
      REAL(kind=DOUBLE)vb_flow(3)
      REAL(kind=DOUBLE)vt_plasma_frame(3)
      REAL(kind=DOUBLE)vb_therm(3)
      REAL(kind=DOUBLE)vrel(3)
      REAL(kind=DOUBLE)v_i(3)
      REAL(kind=DOUBLE)v_thermal,ran,sgvmax,cxsec,cx_v,m_i
      logical init
      external eval_data
      REAL(kind=DOUBLE)eval_data
      save init,i_sigv_max,i_cx
      data init/.TRUE./
      if(init)then
      i_sigv_max=string_lookup('sigv_max',pr_var0_list,pr_var0_num)
      i_cx=string_lookup('cross_section',pr_var0_list,pr_var0_num)
      if(i_sigv_max.GT.0.AND.i_cx.GT.0)continue
      init=.FALSE.
      end if
      pr_reac=problem_test_reaction(ts_reac,test)
      nprod=reaction_product_num(problem_rc(pr_reac))
      back=problem_test_background(ts_reac,test)
      count=0
      sgvmax=(0.0_DOUBLE)
      do i=1,19
      if(reaction_handling_var0(i,pr_reac).EQ.i_sigv_max)then
      sgvmax=eval_data(reaction_handling_eval_name(i,pr_reac),reaction_h
     &andling_rank(i,pr_reac),reaction_handling_var(1,i,pr_reac),reactio
     &n_handling_tab_index(1,i,pr_reac),reaction_handling_spacing(0,i,pr
     &_reac),reaction_handling_min(1,i,pr_reac),reaction_handling_delta(
     &1,i,pr_reac),reaction_handling_tab(reaction_handling_base(i,pr_rea
     &c)),independent_parameters,ran_index_x,ran_array_x(0))
      else if(reaction_handling_var0(i,pr_reac).EQ.i_cx)then
      jdep_cx=i
      end if
      end do
      
      do i=1,3
      v_test(i)=independent_parameters(8+i-1)
      vb_flow(i)=independent_parameters(11+i-1)
      end do
      vt_plasma_frame(1)=v_test(1)-vb_flow(1)
      vt_plasma_frame(2)=v_test(2)-vb_flow(2)
      vt_plasma_frame(3)=v_test(3)-vb_flow(3)
      
90000 continue
      count=count+1
      v_thermal=sqrt(independent_parameters(5))
      call random_gauss(vb_therm,3,ran_index_x,ran_array_x(0))
      do i=1,3
      vb_therm(i)=vb_therm(i)*(v_thermal)
      end do
      
      vrel(1)=vt_plasma_frame(1)-vb_therm(1)
      vrel(2)=vt_plasma_frame(2)-vb_therm(2)
      vrel(3)=vt_plasma_frame(3)-vb_therm(3)
      
      
      independent_parameters(2)=(0.5_DOUBLE)*independent_parameters(14)*
     &(vrel(1)**2+vrel(2)**2+vrel(3)**2)
      independent_parameters(4)=(0.5_DOUBLE)*(vrel(1)**2+vrel(2)**2+vrel
     &(3)**2)
      cxsec=eval_data(reaction_handling_eval_name(jdep_cx,pr_reac),react
     &ion_handling_rank(jdep_cx,pr_reac),reaction_handling_var(1,jdep_cx
     &,pr_reac),reaction_handling_tab_index(1,jdep_cx,pr_reac),reaction_
     &handling_spacing(0,jdep_cx,pr_reac),reaction_handling_min(1,jdep_c
     &x,pr_reac),reaction_handling_delta(1,jdep_cx,pr_reac),reaction_han
     &dling_tab(reaction_handling_base(jdep_cx,pr_reac)),independent_par
     &ameters,ran_index_x,ran_array_x(0))
      cx_v=cxsec*sqrt((vrel(1)**2+vrel(2)**2+vrel(3)**2))
      ran=random(ran_index_x,ran_array_x(0))
      if(cx_v.LT.ran*sgvmax)goto 90000
      v_back(1)=vb_therm(1)+vb_flow(1)
      v_back(2)=vb_therm(2)+vb_flow(2)
      v_back(3)=vb_therm(3)+vb_flow(3)
      
      if(problem_num_arrangements(ts_reac,test).EQ.1)continue
      if(nprod.EQ.2)continue
      do i=1,2
      prod(i)=problem_test_products(i,1,ts_reac,test)
      w_prod(i)=(1.0_DOUBLE)
      if(i.EQ.1)then
      v_prod(1,i)=v_back(1)
      v_prod(2,i)=v_back(2)
      v_prod(3,i)=v_back(3)
      
      else
      v_prod(1,i)=v_test(1)
      v_prod(2,i)=v_test(2)
      v_prod(3,i)=v_test(3)
      
      end if
      end do
      do i=1,2
      if(i.EQ.1)then
      m_i=species_m(problem_background_sp(back))
      psp_i=back
      v_i(1)=v_back(1)
      v_i(2)=v_back(2)
      v_i(3)=v_back(3)
      
      else if(i.EQ.2)then
      m_i=species_m(problem_test_sp(test))
      psp_i=(pr_background_num+test)
      v_i(1)=v_test(1)
      v_i(2)=v_test(2)
      v_i(3)=v_test(3)
      
      end if
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+(-m_i*rate)
      scoring_data(21+15*psp_i)=scoring_data(21+15*psp_i)+(-m_i*v_i(1)*r
     &ate)
      scoring_data(22+15*psp_i)=scoring_data(22+15*psp_i)+(-m_i*v_i(2)*r
     &ate)
      scoring_data(23+15*psp_i)=scoring_data(23+15*psp_i)+(-m_i*v_i(3)*r
     &ate)
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+(-(0.5_DOUBLE)
     &*m_i*(v_i(1)**2+v_i(2)**2+v_i(3)**2)*rate)
      end do
      do i=1,nprod
      test_i=problem_species_test(prod(i))
      if(test_i.NE.0)then
      m_i=species_m(problem_test_sp(test_i))
      psp_i=(pr_background_num+test_i)
      else
      back_i=problem_species_background(prod(i))
      if(back_i.NE.0)continue
      m_i=species_m(problem_background_sp(back_i))
      psp_i=back_i
      end if
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+(m_i*rate)
      scoring_data(21+15*psp_i)=scoring_data(21+15*psp_i)+(m_i*v_prod(1,
     &i)*rate)
      scoring_data(22+15*psp_i)=scoring_data(22+15*psp_i)+(m_i*v_prod(2,
     &i)*rate)
      scoring_data(23+15*psp_i)=scoring_data(23+15*psp_i)+(m_i*v_prod(3,
     &i)*rate)
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+((0.5_DOUBLE)*
     &m_i*(v_prod(1,i)**2+v_prod(2,i)**2+v_prod(3,i)**2)*rate)
      end do
      return
      end
      subroutine elastic(test,ts_reac,rate,independent_parameters,nprod,
     &prod,v_prod,w_prod,scoring_data,ran_index_x,ran_array_x)
      
      use pr_mod
      
      use rc_mod
      
      use rd_mod
      
      use sp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer inc
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      integer test
      integer ts_reac
      REAL(kind=DOUBLE)rate
      REAL(kind=DOUBLE)independent_parameters(20)
      integer nprod
      integer prod(4)
      REAL(kind=DOUBLE)v_prod(3,4)
      REAL(kind=DOUBLE)w_prod(4),scoring_data(280)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer pr_reac
      integer back
      integer back_i
      integer test_i
      integer i,rm_theta_prob,theta_count,coll_count,psp_i,i_sigv_max,i_
     &angle_min,i_cx,i_csang,jdep_cx
      REAL(kind=DOUBLE)v_test(3)
      REAL(kind=DOUBLE)v_back(3)
      REAL(kind=DOUBLE)vb_flow(3)
      REAL(kind=DOUBLE)vt_plasma_frame(3)
      REAL(kind=DOUBLE)vb_therm(3)
      REAL(kind=DOUBLE)vrel(3)
      REAL(kind=DOUBLE)v_i(3)
      REAL(kind=DOUBLE)v_thermal,sigvmax,cxsec,cx_v,m_i,angle,angle_min,
     &ran
      logical init
      external eval_data
      REAL(kind=DOUBLE)eval_data
      REAL(kind=DOUBLE)random
      external random
      save init,i_sigv_max,i_angle_min,i_cx,i_csang
      data init/.TRUE./
      if(init)then
      i_sigv_max=string_lookup('sigv_max',pr_var0_list,pr_var0_num)
      i_angle_min=string_lookup('angle_min',pr_var0_list,pr_var0_num)
      i_cx=string_lookup('cross_section',pr_var0_list,pr_var0_num)
      i_csang=string_lookup('scattering_angle',pr_var0_list,pr_var0_num)
      if(i_sigv_max.GT.0.AND.i_angle_min.GT.0.AND.i_cx.GT.0.AND.i_csang.
     &GT.0)continue
      init=.FALSE.
      end if
      pr_reac=problem_test_reaction(ts_reac,test)
      nprod=reaction_product_num(problem_rc(pr_reac))
      back=problem_test_background(ts_reac,test)
      sigvmax=(0.0_DOUBLE)
      do i=1,19
      if(reaction_handling_var0(i,pr_reac).EQ.i_sigv_max)then
      sigvmax=eval_data(reaction_handling_eval_name(i,pr_reac),reaction_
     &handling_rank(i,pr_reac),reaction_handling_var(1,i,pr_reac),reacti
     &on_handling_tab_index(1,i,pr_reac),reaction_handling_spacing(0,i,p
     &r_reac),reaction_handling_min(1,i,pr_reac),reaction_handling_delta
     &(1,i,pr_reac),reaction_handling_tab(reaction_handling_base(i,pr_re
     &ac)),independent_parameters,ran_index_x,ran_array_x(0))
      else if(reaction_handling_var0(i,pr_reac).EQ.i_angle_min)then
      angle_min=eval_data(reaction_handling_eval_name(i,pr_reac),reactio
     &n_handling_rank(i,pr_reac),reaction_handling_var(1,i,pr_reac),reac
     &tion_handling_tab_index(1,i,pr_reac),reaction_handling_spacing(0,i
     &,pr_reac),reaction_handling_min(1,i,pr_reac),reaction_handling_del
     &ta(1,i,pr_reac),reaction_handling_tab(reaction_handling_base(i,pr_
     &reac)),independent_parameters,ran_index_x,ran_array_x(0))
      else if(reaction_handling_var0(i,pr_reac).EQ.i_cx)then
      jdep_cx=i
      else if(reaction_handling_var0(i,pr_reac).EQ.i_csang)then
      rm_theta_prob=i
      end if
      end do
      
      do i=1,3
      v_test(i)=independent_parameters(8+i-1)
      vb_flow(i)=independent_parameters(11+i-1)
      end do
      vt_plasma_frame(1)=v_test(1)-vb_flow(1)
      vt_plasma_frame(2)=v_test(2)-vb_flow(2)
      vt_plasma_frame(3)=v_test(3)-vb_flow(3)
      
90000 continue
      v_thermal=sqrt(independent_parameters(5))
      call random_gauss(vb_therm,3,ran_index_x,ran_array_x(0))
      do i=1,3
      vb_therm(i)=vb_therm(i)*(v_thermal)
      end do
      
      vrel(1)=vt_plasma_frame(1)-vb_therm(1)
      vrel(2)=vt_plasma_frame(2)-vb_therm(2)
      vrel(3)=vt_plasma_frame(3)-vb_therm(3)
      
      
      independent_parameters(2)=(0.5_DOUBLE)*independent_parameters(14)*
     &(vrel(1)**2+vrel(2)**2+vrel(3)**2)
      independent_parameters(4)=(0.5_DOUBLE)*(vrel(1)**2+vrel(2)**2+vrel
     &(3)**2)
      cxsec=eval_data(reaction_handling_eval_name(jdep_cx,pr_reac),react
     &ion_handling_rank(jdep_cx,pr_reac),reaction_handling_var(1,jdep_cx
     &,pr_reac),reaction_handling_tab_index(1,jdep_cx,pr_reac),reaction_
     &handling_spacing(0,jdep_cx,pr_reac),reaction_handling_min(1,jdep_c
     &x,pr_reac),reaction_handling_delta(1,jdep_cx,pr_reac),reaction_han
     &dling_tab(reaction_handling_base(jdep_cx,pr_reac)),independent_par
     &ameters,ran_index_x,ran_array_x(0))
      cx_v=cxsec*sqrt((vrel(1)**2+vrel(2)**2+vrel(3)**2))
      
      ran=random(ran_index_x,ran_array_x(0))
      if(cx_v.LE.sigvmax.AND.cx_v.LT.ran*sigvmax)goto 90000
      angle=eval_data(reaction_handling_eval_name(rm_theta_prob,pr_reac)
     &,reaction_handling_rank(rm_theta_prob,pr_reac),reaction_handling_v
     &ar(1,rm_theta_prob,pr_reac),reaction_handling_tab_index(1,rm_theta
     &_prob,pr_reac),reaction_handling_spacing(0,rm_theta_prob,pr_reac),
     &reaction_handling_min(1,rm_theta_prob,pr_reac),reaction_handling_d
     &elta(1,rm_theta_prob,pr_reac),reaction_handling_tab(reaction_handl
     &ing_base(rm_theta_prob,pr_reac)),independent_parameters,ran_index_
     &x,ran_array_x(0))
      
      if(angle.LT.angle_min)angle=angle_min
      
      call product_velocities(angle,independent_parameters,vt_plasma_fra
     &me(1),vb_therm(1),vrel(1),v_prod(1,1),ran_index_x,ran_array_x(0))
      
      if(problem_num_arrangements(ts_reac,test).EQ.1)continue
      do i=1,nprod
      prod(i)=problem_test_products(i,1,ts_reac,test)
      w_prod(i)=(1.0_DOUBLE)
      v_prod(1,i)=v_prod(1,i)+vb_flow(1)
      v_prod(2,i)=v_prod(2,i)+vb_flow(2)
      v_prod(3,i)=v_prod(3,i)+vb_flow(3)
      
      end do
      v_back(1)=vb_therm(1)+vb_flow(1)
      v_back(2)=vb_therm(2)+vb_flow(2)
      v_back(3)=vb_therm(3)+vb_flow(3)
      
      
      do i=1,2
      if(i.EQ.1)then
      m_i=species_m(problem_background_sp(back))
      psp_i=back
      v_i(1)=v_back(1)
      v_i(2)=v_back(2)
      v_i(3)=v_back(3)
      
      else if(i.EQ.2)then
      m_i=species_m(problem_test_sp(test))
      psp_i=(pr_background_num+test)
      v_i(1)=v_test(1)
      v_i(2)=v_test(2)
      v_i(3)=v_test(3)
      
      end if
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+(-m_i*rate)
      scoring_data(21+15*psp_i)=scoring_data(21+15*psp_i)+(-m_i*v_i(1)*r
     &ate)
      scoring_data(22+15*psp_i)=scoring_data(22+15*psp_i)+(-m_i*v_i(2)*r
     &ate)
      scoring_data(23+15*psp_i)=scoring_data(23+15*psp_i)+(-m_i*v_i(3)*r
     &ate)
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+(-(0.5_DOUBLE)
     &*m_i*(v_i(1)**2+v_i(2)**2+v_i(3)**2)*rate)
      end do
      do i=1,nprod
      test_i=problem_species_test(prod(i))
      if(test_i.NE.0)then
      m_i=species_m(problem_test_sp(test_i))
      psp_i=(pr_background_num+test_i)
      else
      back_i=problem_species_background(prod(i))
      if(back_i.NE.0)continue
      m_i=species_m(problem_background_sp(back_i))
      psp_i=back_i
      end if
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+(m_i*rate)
      scoring_data(21+15*psp_i)=scoring_data(21+15*psp_i)+(m_i*v_prod(1,
     &i)*rate)
      scoring_data(22+15*psp_i)=scoring_data(22+15*psp_i)+(m_i*v_prod(2,
     &i)*rate)
      scoring_data(23+15*psp_i)=scoring_data(23+15*psp_i)+(m_i*v_prod(3,
     &i)*rate)
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+((0.5_DOUBLE)*
     &m_i*(v_prod(1,i)**2+v_prod(2,i)**2+v_prod(3,i)**2)*rate)
      end do
      return
      end
      subroutine bgk_elastic(test,ts_reac,rate,independent_parameters,np
     &rod,prod,v_prod,w_prod,scoring_data,ran_index_x,ran_array_x)
      
      use pr_mod
      
      use rc_mod
      
      use rd_mod
      
      use sp_mod
      
      use bk_mod
      
      use zn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer inc
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      integer test
      integer ts_reac
      integer prod(4)
      REAL(kind=DOUBLE)v_prod(3,4)
      REAL(kind=DOUBLE)w_prod(4),scoring_data(280)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer pr_reac
      integer back
      integer back_i
      REAL(kind=DOUBLE)v_test(3)
      REAL(kind=DOUBLE)v_back(3)
      REAL(kind=DOUBLE)vb_flow(3)
      REAL(kind=DOUBLE)vbt_flow(3)
      REAL(kind=DOUBLE)v_diff(3)
      REAL(kind=DOUBLE)v_ave(3)
      REAL(kind=DOUBLE)vt_plasma_frame(3)
      REAL(kind=DOUBLE)vb_therm(3)
      REAL(kind=DOUBLE)vrel(3)
      REAL(kind=DOUBLE)v_i(3)
      integer i,nprod,back_test,psp_i
      REAL(kind=DOUBLE)v_thermal,m_i,rate
      REAL(kind=DOUBLE)independent_parameters(20),ran(1)
      REAL(kind=DOUBLE)m_total,m_coef,temp_test,temp_back,temp_ave
      pr_reac=problem_test_reaction(ts_reac,test)
      nprod=reaction_product_num(problem_rc(pr_reac))
      back=problem_test_background(ts_reac,test)
      if(problem_test_sp(test).EQ.problem_background_sp(back))then
      do i=1,3
      vb_flow(i)=independent_parameters(11+i-1)
      end do
      v_thermal=sqrt(independent_parameters(5))
      else
      m_total=species_m(problem_test_sp(test))+species_m(problem_backgro
     &und_sp(back))
      m_coef=(2.0_DOUBLE)*species_m(problem_test_sp(test))*species_m(pro
     &blem_background_sp(back))/m_total**2
      back_test=problem_species_background(problem_test_sp(test))
      do i=1,3
      vb_flow(i)=independent_parameters(11+i-1)
      vbt_flow(i)=independent_parameters(17+i-1)
      v_ave(i)=(species_m(problem_background_sp(back_test))*vbt_flow(i)+
     &species_m(problem_background_sp(back))*vb_flow(i))/m_total
      end do
      v_diff(1)=vb_flow(1)-vbt_flow(1)
      v_diff(2)=vb_flow(2)-vbt_flow(2)
      v_diff(3)=vb_flow(3)-vbt_flow(3)
      
      vb_flow(1)=v_ave(1)
      vb_flow(2)=v_ave(2)
      vb_flow(3)=v_ave(3)
      
      temp_test=background_temp(back_test,zone_pointer(int(independent_p
     &arameters(6))))
      temp_back=background_temp(back,zone_pointer(int(independent_parame
     &ters(6))))
      temp_ave=temp_test+m_coef*((temp_back-temp_test)+species_m(problem
     &_background_sp(back))*(v_diff(1)**2+v_diff(2)**2+v_diff(3)**2)/(6.
     &_DOUBLE))
      v_thermal=sqrt(temp_ave/species_m(problem_test_sp(test)))
      endif
      do i=1,3
      call random_gauss(ran,1,ran_index_x,ran_array_x(0))
      vb_therm(i)=v_thermal*ran(1)
      end do
      do i=1,3
      v_test(i)=independent_parameters(8+i-1)
      end do
      m_i=species_m(problem_test_sp(test))
      psp_i=(pr_background_num+test)
      v_i(1)=v_test(1)
      v_i(2)=v_test(2)
      v_i(3)=v_test(3)
      
      scoring_data(21+15*psp_i)=scoring_data(21+15*psp_i)+(-m_i*v_i(1)*r
     &ate)
      scoring_data(22+15*psp_i)=scoring_data(22+15*psp_i)+(-m_i*v_i(2)*r
     &ate)
      scoring_data(23+15*psp_i)=scoring_data(23+15*psp_i)+(-m_i*v_i(3)*r
     &ate)
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+(-(0.5_DOUBLE)
     &*m_i*(v_i(1)**2+v_i(2)**2+v_i(3)**2)*rate)
      
      if(problem_num_arrangements(ts_reac,test).EQ.1)continue
      do i=1,nprod
      prod(i)=problem_test_products(i,1,ts_reac,test)
      if(i.EQ.1)then
      w_prod(i)=(0.0_DOUBLE)
      v_prod(1,i)=(0.0_DOUBLE)
      v_prod(2,i)=(0.0_DOUBLE)
      v_prod(3,i)=(0.0_DOUBLE)
      else if(i.EQ.2)then
      w_prod(i)=(1.0_DOUBLE)
      v_prod(1,i)=vb_therm(1)+vb_flow(1)
      v_prod(2,i)=vb_therm(2)+vb_flow(2)
      v_prod(3,i)=vb_therm(3)+vb_flow(3)
      
      endif
      end do
      v_i(1)=v_prod(1,2)
      v_i(2)=v_prod(2,2)
      v_i(3)=v_prod(3,2)
      
      scoring_data(21+15*psp_i)=scoring_data(21+15*psp_i)+(m_i*v_i(1)*ra
     &te)
      scoring_data(22+15*psp_i)=scoring_data(22+15*psp_i)+(m_i*v_i(2)*ra
     &te)
      scoring_data(23+15*psp_i)=scoring_data(23+15*psp_i)+(m_i*v_i(3)*ra
     &te)
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+((0.5_DOUBLE)*
     &m_i*(v_i(1)**2+v_i(2)**2+v_i(3)**2)*rate)
      return
      end
      subroutine dissoc(test,ts_reac,rate,independent_parameters,nprod,p
     &rod,v_prod,w_prod,scoring_data,ran_index_x,ran_array_x)
      
      use pr_mod
      
      use rc_mod
      
      use sp_mod
      
      use rd_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer inc
      REAL(kind=DOUBLE)random
      external random
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      integer test
      integer ts_reac
      REAL(kind=DOUBLE)rate
      REAL(kind=DOUBLE)independent_parameters(20)
      integer nprod
      integer prod(4)
      REAL(kind=DOUBLE)v_prod(3,4)
      REAL(kind=DOUBLE)w_prod(4),scoring_data(280)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer pr_reac
      integer back
      integer back_i
      integer test_i
      integer reac
      integer i,i_ediss,i_elediss,num_heavy_products,j_arr,psp_i,num_lin
     &es,i_e_rate,i_gen_rate
      integer product_index(2),lines(6),e_rates(6)
      REAL(kind=DOUBLE)v_test(3)
      REAL(kind=DOUBLE)v_iso(3)
      REAL(kind=DOUBLE)v_emitter(3)
      REAL(kind=DOUBLE)sum,ran,ediss,elediss,v_diss1,v_diss2,m_i
      REAL(kind=DOUBLE)mass(2)
      logical init,first_line
      character*40 line
      external eval_data
      REAL(kind=DOUBLE)eval_data
      save init,i_ediss,i_elediss,num_lines,lines,e_rates,i_gen_rate
      data init/.TRUE./
      if(init)then
      i_ediss=string_lookup('dissociation_energy',pr_var0_list,pr_var0_n
     &um)
      i_elediss=string_lookup('background_energy_loss_rate',pr_var0_list
     &,pr_var0_num)
      if(i_ediss.GT.0.AND.i_elediss.GT.0)continue
      num_lines=0
      do i=1,pr_var0_num
      if(pr_var0_list(i) (1:10).EQ.'wavelength')then
      num_lines=num_lines+1
      lines(num_lines)=i
      line=pr_var0_list(i) (11:)
      i_e_rate=string_lookup('emission_rate'//line,pr_var0_list,pr_var0_
     &num)
      if(i_e_rate.GT.0)continue
      e_rates(num_lines)=i_e_rate
      end if
      end do
      if(num_lines.GT.0)then
      i_gen_rate=string_lookup('emission_rate',pr_var0_list,pr_var0_num)
      end if
      init=.FALSE.
      end if
      pr_reac=problem_test_reaction(ts_reac,test)
      nprod=reaction_product_num(problem_rc(pr_reac))
      back=problem_test_background(ts_reac,test)
      sum=(0.0_DOUBLE)
      ran=random(ran_index_x,ran_array_x(0))
      do i=1,problem_num_arrangements(ts_reac,test)
      sum=sum+problem_prod_mult(i,ts_reac,test)
      if(sum.GE.ran)then
      j_arr=i
      goto 90007
      end if
      end do
90007 continue
      
      call random_isodist(v_iso,1,ran_index_x,ran_array_x(0))
      num_heavy_products=0
      do i=1,nprod
      prod(i)=problem_test_products(i,j_arr,ts_reac,test)
      if(species_sy(prod(i)).NE.'e')then
      num_heavy_products=num_heavy_products+1
      mass(num_heavy_products)=species_m(prod(i))
      product_index(num_heavy_products)=i
      end if
      end do
      reac=problem_rc(pr_reac)
      if(reaction_emitter(reac).NE.0)then
      if(reaction_emitter(reac).GT.0)then
      independent_parameters(16)=species_m(prod(reaction_emitter(reac)))
      else if(species_generic(reaction_reagent(-reaction_emitter(reac),r
     &eac)).EQ.species_generic(problem_test_sp(test)))then
      independent_parameters(16)=species_m(problem_test_sp(test))
      else if(species_generic(reaction_reagent(-reaction_emitter(reac),r
     &eac)).EQ.species_generic(problem_background_sp(back)))then
      if('background velocity not set'.EQ.' ')continue
      end if
      end if
      do i=1,19
      if(reaction_handling_var0(i,pr_reac).EQ.i_ediss)then
      ediss=eval_data(reaction_handling_eval_name(i,pr_reac),reaction_ha
     &ndling_rank(i,pr_reac),reaction_handling_var(1,i,pr_reac),reaction
     &_handling_tab_index(1,i,pr_reac),reaction_handling_spacing(0,i,pr_
     &reac),reaction_handling_min(1,i,pr_reac),reaction_handling_delta(1
     &,i,pr_reac),reaction_handling_tab(reaction_handling_base(i,pr_reac
     &)),independent_parameters,ran_index_x,ran_array_x(0))
      else if(reaction_handling_var0(i,pr_reac).EQ.i_elediss)then
      elediss=independent_parameters(1)*eval_data(reaction_handling_eval
     &_name(i,pr_reac),reaction_handling_rank(i,pr_reac),reaction_handli
     &ng_var(1,i,pr_reac),reaction_handling_tab_index(1,i,pr_reac),react
     &ion_handling_spacing(0,i,pr_reac),reaction_handling_min(1,i,pr_rea
     &c),reaction_handling_delta(1,i,pr_reac),reaction_handling_tab(reac
     &tion_handling_base(i,pr_reac)),independent_parameters,ran_index_x,
     &ran_array_x(0))
      else if(reaction_handling_var0(i,pr_reac).NE.1)then
      scoring_data(reaction_handling_var0(i,pr_reac))=eval_data(reaction
     &_handling_eval_name(i,pr_reac),reaction_handling_rank(i,pr_reac),r
     &eaction_handling_var(1,i,pr_reac),reaction_handling_tab_index(1,i,
     &pr_reac),reaction_handling_spacing(0,i,pr_reac),reaction_handling_
     &min(1,i,pr_reac),reaction_handling_delta(1,i,pr_reac),reaction_han
     &dling_tab(reaction_handling_base(i,pr_reac)),independent_parameter
     &s,ran_index_x,ran_array_x(0))
      end if
      end do
      if(num_lines.GT.0)then
      first_line=.TRUE.
      do i=1,num_lines
      if((i_gen_rate.GT.0).AND.first_line)then
      if(scoring_data(lines(i)).GT.(0.0_DOUBLE))then
      if(scoring_data(e_rates(i)).EQ.(0.0_DOUBLE))continue
      if(reaction_emitter(reac).NE.0)continue
      scoring_data(e_rates(i))=scoring_data(i_gen_rate)*independent_para
     &meters(1)
      first_line=.FALSE.
      end if
      else
      if(scoring_data(lines(i)).GT.(0.0_DOUBLE))then
      scoring_data(e_rates(i))=scoring_data(e_rates(i))*(independent_par
     &ameters(1))
      end if
      end if
      end do
      end if
      if(num_heavy_products.EQ.2)continue
      v_diss1=sqrt((4.0_DOUBLE)*ediss*(mass(2)/mass(1))/(mass(1)+mass(2)
     &))
      v_diss2=-v_diss1*mass(1)/mass(2)
      
      do i=1,3
      v_test(i)=independent_parameters(8+i-1)
      end do
      do i=1,nprod
      if(product_index(1).EQ.i)then
      v_prod(1,i)=v_test(1)+v_iso(1)*(v_diss1)
      v_prod(2,i)=v_test(2)+v_iso(2)*(v_diss1)
      v_prod(3,i)=v_test(3)+v_iso(3)*(v_diss1)
      
      if(problem_species_test(prod(i)).GT.0.AND.prod(i).EQ.prod(product_
     &index(2)))then
      w_prod(i)=(2.0_DOUBLE)
      else
      w_prod(i)=(1.0_DOUBLE)
      end if
      else if(product_index(2).EQ.i)then
      v_prod(1,i)=v_test(1)+v_iso(1)*(v_diss2)
      v_prod(2,i)=v_test(2)+v_iso(2)*(v_diss2)
      v_prod(3,i)=v_test(3)+v_iso(3)*(v_diss2)
      
      if(problem_species_test(prod(i)).GT.0.AND.prod(i).EQ.prod(product_
     &index(1)))then
      w_prod(i)=(0.0_DOUBLE)
      else
      w_prod(i)=(1.0_DOUBLE)
      end if
      else
      v_prod(1,i)=(0.0_DOUBLE)
      v_prod(2,i)=(0.0_DOUBLE)
      v_prod(3,i)=(0.0_DOUBLE)
      w_prod(i)=(0.0_DOUBLE)
      end if
      end do
      if(reaction_emitter(reac).NE.0)then
      if(reaction_emitter(reac).GT.0)then
      v_emitter(1)=v_prod(1,reaction_emitter(reac))
      v_emitter(2)=v_prod(2,reaction_emitter(reac))
      v_emitter(3)=v_prod(3,reaction_emitter(reac))
      
      else if(species_generic(reaction_reagent(-reaction_emitter(reac),r
     &eac)).EQ.species_generic(problem_test_sp(test)))then
      v_emitter(1)=v_test(1)
      v_emitter(2)=v_test(2)
      v_emitter(3)=v_test(3)
      
      else if(species_generic(reaction_reagent(-reaction_emitter(reac),r
     &eac)).EQ.species_generic(problem_background_sp(back)))then
      if('background velocity not set'.EQ.' ')continue
      end if
      scoring_data(8)=v_emitter(1)
      scoring_data(9)=v_emitter(2)
      scoring_data(10)=v_emitter(3)
      end if
      
      psp_i=back
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+(-species_m(pr
     &oblem_background_sp(back))*rate)
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+(-elediss)
      psp_i=(pr_background_num+test)
      m_i=species_m(problem_test_sp(test))
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+(-m_i*rate)
      scoring_data(21+15*psp_i)=scoring_data(21+15*psp_i)+(-m_i*v_test(1
     &)*rate)
      scoring_data(22+15*psp_i)=scoring_data(22+15*psp_i)+(-m_i*v_test(2
     &)*rate)
      scoring_data(23+15*psp_i)=scoring_data(23+15*psp_i)+(-m_i*v_test(3
     &)*rate)
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+(-(0.5_DOUBLE)
     &*m_i*(v_test(1)**2+v_test(2)**2+v_test(3)**2)*rate)
      do i=1,nprod
      test_i=problem_species_test(prod(i))
      if(test_i.NE.0)then
      m_i=species_m(problem_test_sp(test_i))*w_prod(i)
      psp_i=(pr_background_num+test_i)
      else
      back_i=problem_species_background(prod(i))
      if(back_i.NE.0)continue
      m_i=species_m(problem_background_sp(back_i))
      psp_i=back_i
      end if
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+(m_i*rate)
      scoring_data(21+15*psp_i)=scoring_data(21+15*psp_i)+(m_i*v_prod(1,
     &i)*rate)
      scoring_data(22+15*psp_i)=scoring_data(22+15*psp_i)+(m_i*v_prod(2,
     &i)*rate)
      scoring_data(23+15*psp_i)=scoring_data(23+15*psp_i)+(m_i*v_prod(3,
     &i)*rate)
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+((0.5_DOUBLE)*
     &m_i*(v_prod(1,i)**2+v_prod(2,i)**2+v_prod(3,i)**2)*rate)
      end do
      return
      end
      subroutine dissoc_cramd(test,ts_reac,rate,independent_parameters,n
     &prod,prod,v_prod,w_prod,scoring_data,ran_index_x,ran_array_x)
      
      use pr_mod
      
      use rc_mod
      
      use sp_mod
      
      use rd_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer inc
      REAL(kind=DOUBLE)random
      external random
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      integer test
      integer ts_reac
      REAL(kind=DOUBLE)rate
      REAL(kind=DOUBLE)independent_parameters(20)
      integer nprod
      integer prod(4)
      REAL(kind=DOUBLE)v_prod(3,4)
      REAL(kind=DOUBLE)w_prod(4),scoring_data(280)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer pr_reac
      integer back
      integer back_i
      integer i,i_eloss,i_iloss,i_hfrac,num_heavy_products,j_arr
      integer product_index(2)
      REAL(kind=DOUBLE)v_test(3)
      REAL(kind=DOUBLE)v_iso(3)
      REAL(kind=DOUBLE)sum,ran,eloss,iloss,hfrac,ediss,v_diss1,v_diss2,m
     &_i
      REAL(kind=DOUBLE)mass(2)
      logical init
      external eval_data
      REAL(kind=DOUBLE)eval_data
      save init,i_eloss,i_iloss,i_hfrac
      data init/.TRUE./
      if(init)then
      i_eloss=string_lookup('background_energy_loss_rate',pr_var0_list,p
     &r_var0_num)
      i_iloss=string_lookup('proton_energy_loss_rate',pr_var0_list,pr_va
     &r0_num)
      i_hfrac=string_lookup('H_fraction',pr_var0_list,pr_var0_num)
      if(i_eloss.GT.0.AND.i_iloss.GT.0.AND.i_hfrac.GT.0)continue
      init=.FALSE.
      end if
      pr_reac=problem_test_reaction(ts_reac,test)
      nprod=reaction_product_num(problem_rc(pr_reac))
      back=problem_test_background(ts_reac,test)
      
      sum=(0.0_DOUBLE)
      ran=random(ran_index_x,ran_array_x(0))
      do i=1,problem_num_arrangements(ts_reac,test)
      sum=sum+problem_prod_mult(i,ts_reac,test)
      if(sum.GE.ran)then
      j_arr=i
      goto 90007
      end if
      end do
90007 continue
      
      call random_isodist(v_iso,1,ran_index_x,ran_array_x(0))
      do i=1,19
      if(reaction_handling_var0(i,pr_reac).EQ.i_eloss)then
      eloss=independent_parameters(1)*eval_data(reaction_handling_eval_n
     &ame(i,pr_reac),reaction_handling_rank(i,pr_reac),reaction_handling
     &_var(1,i,pr_reac),reaction_handling_tab_index(1,i,pr_reac),reactio
     &n_handling_spacing(0,i,pr_reac),reaction_handling_min(1,i,pr_reac)
     &,reaction_handling_delta(1,i,pr_reac),reaction_handling_tab(reacti
     &on_handling_base(i,pr_reac)),independent_parameters,ran_index_x,ra
     &n_array_x(0))
      else if(reaction_handling_var0(i,pr_reac).EQ.i_iloss)then
      
      iloss=independent_parameters(1)*eval_data(reaction_handling_eval_n
     &ame(i,pr_reac),reaction_handling_rank(i,pr_reac),reaction_handling
     &_var(1,i,pr_reac),reaction_handling_tab_index(1,i,pr_reac),reactio
     &n_handling_spacing(0,i,pr_reac),reaction_handling_min(1,i,pr_reac)
     &,reaction_handling_delta(1,i,pr_reac),reaction_handling_tab(reacti
     &on_handling_base(i,pr_reac)),independent_parameters,ran_index_x,ra
     &n_array_x(0))
      else if(reaction_handling_var0(i,pr_reac).EQ.i_hfrac)then
      hfrac=eval_data(reaction_handling_eval_name(i,pr_reac),reaction_ha
     &ndling_rank(i,pr_reac),reaction_handling_var(1,i,pr_reac),reaction
     &_handling_tab_index(1,i,pr_reac),reaction_handling_spacing(0,i,pr_
     &reac),reaction_handling_min(1,i,pr_reac),reaction_handling_delta(1
     &,i,pr_reac),reaction_handling_tab(reaction_handling_base(i,pr_reac
     &)),independent_parameters,ran_index_x,ran_array_x(0))
      end if
      end do
      num_heavy_products=0
      do i=1,nprod
      prod(i)=problem_test_products(i,j_arr,ts_reac,test)
      if(species_sy(prod(i)).NE.'e')then
      num_heavy_products=num_heavy_products+1
      mass(num_heavy_products)=species_m(prod(i))
      product_index(num_heavy_products)=i
      if(species_z(prod(i)).EQ.0)then
      w_prod(i)=(2.0_DOUBLE)*hfrac
      else
      if(species_z(prod(i)).GT.0)continue
      w_prod(i)=(2.0_DOUBLE)*((1.0_DOUBLE)-hfrac)
      end if
      else
      w_prod(i)=(0.0_DOUBLE)
      end if
      end do
      
      ediss=(3._DOUBLE)*(1.60217733e-19_DOUBLE)
      if(num_heavy_products.EQ.2)continue
      v_diss1=sqrt((4.0_DOUBLE)*ediss*(mass(2)/mass(1))/(mass(1)+mass(2)
     &))
      v_diss2=-v_diss1*mass(1)/mass(2)
      
      do i=1,3
      v_test(i)=independent_parameters(8+i-1)
      end do
      do i=1,nprod
      if(product_index(1).EQ.i)then
      v_prod(1,i)=v_test(1)+v_iso(1)*(v_diss1)
      v_prod(2,i)=v_test(2)+v_iso(2)*(v_diss1)
      v_prod(3,i)=v_test(3)+v_iso(3)*(v_diss1)
      
      else if(product_index(2).EQ.i)then
      v_prod(1,i)=v_test(1)+v_iso(1)*(v_diss2)
      v_prod(2,i)=v_test(2)+v_iso(2)*(v_diss2)
      v_prod(3,i)=v_test(3)+v_iso(3)*(v_diss2)
      
      else
      v_prod(1,i)=(0.0_DOUBLE)
      v_prod(2,i)=(0.0_DOUBLE)
      v_prod(3,i)=(0.0_DOUBLE)
      end if
      end do
      
      return
      end
      subroutine ionize(test,ts_reac,rate,independent_parameters,nprod,p
     &rod,v_prod,w_prod,scoring_data,ran_index_x,ran_array_x)
      
      use pr_mod
      
      use rc_mod
      
      use sp_mod
      
      use rd_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer inc
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      integer test
      integer ts_reac
      REAL(kind=DOUBLE)rate
      REAL(kind=DOUBLE)independent_parameters(20)
      integer nprod
      integer prod(4)
      REAL(kind=DOUBLE)v_prod(3,4)
      REAL(kind=DOUBLE)w_prod(4),scoring_data(280)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer pr_reac
      integer back
      integer back_i
      integer test_i
      integer reac
      integer i,i_eioniz,num_lines,i_e_rate,i_gen_rate,psp_i
      integer lines(6),e_rates(6)
      REAL(kind=DOUBLE)eioniz,m_i
      REAL(kind=DOUBLE)v_test(3)
      REAL(kind=DOUBLE)v_emitter(3)
      character*40 line
      logical init,first_line
      external eval_data,species_add_check
      REAL(kind=DOUBLE)eval_data
      logical species_add_check
      save init,i_eioniz,num_lines,lines,e_rates,i_gen_rate
      data init/.TRUE./
      if(init)then
      i_eioniz=string_lookup('background_energy_loss_rate',pr_var0_list,
     &pr_var0_num)
      num_lines=0
      do i=1,pr_var0_num
      if(pr_var0_list(i) (1:10).EQ.'wavelength')then
      num_lines=num_lines+1
      lines(num_lines)=i
      line=pr_var0_list(i) (11:)
      i_e_rate=string_lookup('emission_rate'//line,pr_var0_list,pr_var0_
     &num)
      if(i_e_rate.GT.0)continue
      e_rates(num_lines)=i_e_rate
      end if
      end do
      if(num_lines.GT.0)then
      i_gen_rate=string_lookup('emission_rate',pr_var0_list,pr_var0_num)
      end if
      init=.FALSE.
      end if
      pr_reac=problem_test_reaction(ts_reac,test)
      nprod=reaction_product_num(problem_rc(pr_reac))
      back=problem_test_background(ts_reac,test)
      if(problem_num_arrangements(ts_reac,test).EQ.1)continue
      
      do i=1,3
      v_test(i)=independent_parameters(8+i-1)
      end do
      do i=1,nprod
      prod(i)=problem_test_products(i,1,ts_reac,test)
      if(species_sy(prod(i)).NE.'e')then
      v_prod(1,i)=v_test(1)
      v_prod(2,i)=v_test(2)
      v_prod(3,i)=v_test(3)
      
      w_prod(i)=(1.0_DOUBLE)
      else
      v_prod(1,i)=(0.0_DOUBLE)
      v_prod(2,i)=(0.0_DOUBLE)
      v_prod(3,i)=(0.0_DOUBLE)
      w_prod(i)=(0.0_DOUBLE)
      end if
      end do
      reac=problem_rc(pr_reac)
      if(reaction_emitter(reac).NE.0)then
      if(reaction_emitter(reac).GT.0)then
      v_emitter(1)=v_prod(1,reaction_emitter(reac))
      v_emitter(2)=v_prod(2,reaction_emitter(reac))
      v_emitter(3)=v_prod(3,reaction_emitter(reac))
      
      independent_parameters(16)=species_m(prod(reaction_emitter(reac)))
      else if(species_generic(reaction_reagent(-reaction_emitter(reac),r
     &eac)).EQ.species_generic(problem_test_sp(test)))then
      v_emitter(1)=v_test(1)
      v_emitter(2)=v_test(2)
      v_emitter(3)=v_test(3)
      
      independent_parameters(16)=species_m(problem_test_sp(test))
      else if(species_generic(reaction_reagent(-reaction_emitter(reac),r
     &eac)).EQ.species_generic(problem_background_sp(back)))then
      if('background velocity not set'.EQ.' ')continue
      end if
      scoring_data(8)=v_emitter(1)
      scoring_data(9)=v_emitter(2)
      scoring_data(10)=v_emitter(3)
      end if
      do i=1,19
      if(reaction_handling_var0(i,pr_reac).EQ.i_eioniz)then
      eioniz=independent_parameters(1)*eval_data(reaction_handling_eval_
     &name(i,pr_reac),reaction_handling_rank(i,pr_reac),reaction_handlin
     &g_var(1,i,pr_reac),reaction_handling_tab_index(1,i,pr_reac),reacti
     &on_handling_spacing(0,i,pr_reac),reaction_handling_min(1,i,pr_reac
     &),reaction_handling_delta(1,i,pr_reac),reaction_handling_tab(react
     &ion_handling_base(i,pr_reac)),independent_parameters,ran_index_x,r
     &an_array_x(0))
      else if(reaction_handling_var0(i,pr_reac).NE.1)then
      scoring_data(reaction_handling_var0(i,pr_reac))=eval_data(reaction
     &_handling_eval_name(i,pr_reac),reaction_handling_rank(i,pr_reac),r
     &eaction_handling_var(1,i,pr_reac),reaction_handling_tab_index(1,i,
     &pr_reac),reaction_handling_spacing(0,i,pr_reac),reaction_handling_
     &min(1,i,pr_reac),reaction_handling_delta(1,i,pr_reac),reaction_han
     &dling_tab(reaction_handling_base(i,pr_reac)),independent_parameter
     &s,ran_index_x,ran_array_x(0))
      end if
      end do
      if(num_lines.GT.0)then
      first_line=.TRUE.
      do i=1,num_lines
      if((i_gen_rate.GT.0).AND.first_line)then
      if((scoring_data(lines(i)).GT.(0.0_DOUBLE)).AND.(scoring_data(i_ge
     &n_rate).GT.(0.0_DOUBLE)))then
      if(scoring_data(e_rates(i)).EQ.(0.0_DOUBLE))continue
      if(reaction_emitter(reac).NE.0)continue
      scoring_data(e_rates(i))=scoring_data(i_gen_rate)*independent_para
     &meters(1)
      first_line=.FALSE.
      end if
      else
      if(scoring_data(lines(i)).GT.(0.0_DOUBLE))then
      scoring_data(e_rates(i))=scoring_data(e_rates(i))*(independent_par
     &ameters(1))
      end if
      end if
      end do
      end if
      psp_i=back
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+(-species_m(pr
     &oblem_background_sp(back))*rate)
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+(-eioniz)
      psp_i=(pr_background_num+test)
      m_i=species_m(problem_test_sp(test))
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+(-m_i*rate)
      scoring_data(21+15*psp_i)=scoring_data(21+15*psp_i)+(-m_i*v_test(1
     &)*rate)
      scoring_data(22+15*psp_i)=scoring_data(22+15*psp_i)+(-m_i*v_test(2
     &)*rate)
      scoring_data(23+15*psp_i)=scoring_data(23+15*psp_i)+(-m_i*v_test(3
     &)*rate)
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+(-(0.5_DOUBLE)
     &*m_i*(v_test(1)**2+v_test(2)**2+v_test(3)**2)*rate)
      do i=1,nprod
      prod(i)=problem_test_products(i,1,ts_reac,test)
      test_i=problem_species_test(prod(i))
      if(test_i.NE.0)then
      psp_i=(pr_background_num+test_i)
      else
      back_i=problem_species_background(prod(i))
      if(back_i.NE.0)continue
      psp_i=back_i
      end if
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+(species_m(pro
     &d(i))*rate)
      scoring_data(21+15*psp_i)=scoring_data(21+15*psp_i)+(species_m(pro
     &d(i))*v_prod(1,i)*rate)
      scoring_data(22+15*psp_i)=scoring_data(22+15*psp_i)+(species_m(pro
     &d(i))*v_prod(2,i)*rate)
      scoring_data(23+15*psp_i)=scoring_data(23+15*psp_i)+(species_m(pro
     &d(i))*v_prod(3,i)*rate)
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+((0.5_DOUBLE)*
     &species_m(prod(i))*(v_prod(1,i)**2+v_prod(2,i)**2+v_prod(3,i)**2)*
     &rate)
      end do
      
      return
      end
      subroutine excite(test,ts_reac,rate,independent_parameters,nprod,p
     &rod,v_prod,w_prod,scoring_data,ran_index_x,ran_array_x)
      
      use pr_mod
      
      use rc_mod
      
      use sp_mod
      
      use rd_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer inc
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      integer test
      integer ts_reac
      REAL(kind=DOUBLE)rate
      REAL(kind=DOUBLE)independent_parameters(20)
      integer nprod
      integer prod(4)
      REAL(kind=DOUBLE)v_prod(3,4)
      REAL(kind=DOUBLE)w_prod(4),scoring_data(280)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer pr_reac
      integer back
      integer back_i
      integer test_i
      integer reac
      integer i,i_eloss,i_esrc,num_lines,i_e_rate,i_gen_rate,psp_i
      integer lines(6),e_rates(6)
      REAL(kind=DOUBLE)eloss,esrc,m_i
      REAL(kind=DOUBLE)v_test(3)
      REAL(kind=DOUBLE)v_emitter(3)
      character*40 line
      logical init,first_line
      external eval_data,species_add_check
      REAL(kind=DOUBLE)eval_data
      logical species_add_check
      save init,i_eloss,i_esrc,num_lines,lines,e_rates,i_gen_rate
      data init/.TRUE./
      if(init)then
      i_eloss=string_lookup('background_energy_loss_rate',pr_var0_list,p
     &r_var0_num)
      i_esrc=string_lookup('background_energy_source_rate',pr_var0_list,
     &pr_var0_num)
      num_lines=0
      do i=1,pr_var0_num
      if(pr_var0_list(i) (1:10).EQ.'wavelength')then
      num_lines=num_lines+1
      lines(num_lines)=i
      line=pr_var0_list(i) (11:)
      i_e_rate=string_lookup('emission_rate'//line,pr_var0_list,pr_var0_
     &num)
      if(i_e_rate.GT.0)continue
      e_rates(num_lines)=i_e_rate
      end if
      end do
      if(num_lines.GT.0)then
      i_gen_rate=string_lookup('emission_rate',pr_var0_list,pr_var0_num)
      end if
      init=.FALSE.
      end if
      pr_reac=problem_test_reaction(ts_reac,test)
      nprod=reaction_product_num(problem_rc(pr_reac))
      back=problem_test_background(ts_reac,test)
      if(problem_num_arrangements(ts_reac,test).EQ.1)continue
      do i=1,3
      v_test(i)=independent_parameters(8+i-1)
      end do
      do i=1,nprod
      prod(i)=problem_test_products(i,1,ts_reac,test)
      if(species_sy(prod(i)).NE.'e')then
      v_prod(1,i)=v_test(1)
      v_prod(2,i)=v_test(2)
      v_prod(3,i)=v_test(3)
      
      w_prod(i)=(1.0_DOUBLE)
      else
      v_prod(1,i)=(0.0_DOUBLE)
      v_prod(2,i)=(0.0_DOUBLE)
      v_prod(3,i)=(0.0_DOUBLE)
      w_prod(i)=(0.0_DOUBLE)
      end if
      end do
      reac=problem_rc(pr_reac)
      if(reaction_emitter(reac).NE.0)then
      if(reaction_emitter(reac).GT.0)then
      v_emitter(1)=v_prod(1,reaction_emitter(reac))
      v_emitter(2)=v_prod(2,reaction_emitter(reac))
      v_emitter(3)=v_prod(3,reaction_emitter(reac))
      
      independent_parameters(16)=species_m(prod(reaction_emitter(reac)))
      else if(species_generic(reaction_reagent(-reaction_emitter(reac),r
     &eac)).EQ.species_generic(problem_test_sp(test)))then
      v_emitter(1)=v_test(1)
      v_emitter(2)=v_test(2)
      v_emitter(3)=v_test(3)
      
      independent_parameters(16)=species_m(problem_test_sp(test))
      else if(species_generic(reaction_reagent(-reaction_emitter(reac),r
     &eac)).EQ.species_generic(problem_background_sp(back)))then
      if('background velocity not set'.EQ.' ')continue
      end if
      scoring_data(8)=v_emitter(1)
      scoring_data(9)=v_emitter(2)
      scoring_data(10)=v_emitter(3)
      end if
      eloss=(0.0_DOUBLE)
      esrc=(0.0_DOUBLE)
      do i=1,19
      if(reaction_handling_var0(i,pr_reac).EQ.i_eloss)then
      eloss=independent_parameters(1)*eval_data(reaction_handling_eval_n
     &ame(i,pr_reac),reaction_handling_rank(i,pr_reac),reaction_handling
     &_var(1,i,pr_reac),reaction_handling_tab_index(1,i,pr_reac),reactio
     &n_handling_spacing(0,i,pr_reac),reaction_handling_min(1,i,pr_reac)
     &,reaction_handling_delta(1,i,pr_reac),reaction_handling_tab(reacti
     &on_handling_base(i,pr_reac)),independent_parameters,ran_index_x,ra
     &n_array_x(0))
      else if(reaction_handling_var0(i,pr_reac).EQ.i_esrc)then
      esrc=independent_parameters(1)*eval_data(reaction_handling_eval_na
     &me(i,pr_reac),reaction_handling_rank(i,pr_reac),reaction_handling_
     &var(1,i,pr_reac),reaction_handling_tab_index(1,i,pr_reac),reaction
     &_handling_spacing(0,i,pr_reac),reaction_handling_min(1,i,pr_reac),
     &reaction_handling_delta(1,i,pr_reac),reaction_handling_tab(reactio
     &n_handling_base(i,pr_reac)),independent_parameters,ran_index_x,ran
     &_array_x(0))
      else if(reaction_handling_var0(i,pr_reac).NE.1)then
      scoring_data(reaction_handling_var0(i,pr_reac))=eval_data(reaction
     &_handling_eval_name(i,pr_reac),reaction_handling_rank(i,pr_reac),r
     &eaction_handling_var(1,i,pr_reac),reaction_handling_tab_index(1,i,
     &pr_reac),reaction_handling_spacing(0,i,pr_reac),reaction_handling_
     &min(1,i,pr_reac),reaction_handling_delta(1,i,pr_reac),reaction_han
     &dling_tab(reaction_handling_base(i,pr_reac)),independent_parameter
     &s,ran_index_x,ran_array_x(0))
      end if
      end do
      if(num_lines.GT.0)then
      first_line=.TRUE.
      do i=1,num_lines
      if((i_gen_rate.GT.0).AND.first_line)then
      if((scoring_data(lines(i)).GT.(0.0_DOUBLE)).AND.(scoring_data(i_ge
     &n_rate).GT.(0.0_DOUBLE)))then
      if(scoring_data(e_rates(i)).EQ.(0.0_DOUBLE))continue
      if(reaction_emitter(reac).NE.0)continue
      scoring_data(e_rates(i))=scoring_data(i_gen_rate)*independent_para
     &meters(1)
      first_line=.FALSE.
      end if
      else
      if(scoring_data(lines(i)).GT.(0.0_DOUBLE))then
      scoring_data(e_rates(i))=scoring_data(e_rates(i))*(independent_par
     &ameters(1))
      end if
      end if
      end do
      end if
      psp_i=back
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+(-species_m(pr
     &oblem_background_sp(back))*rate)
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+((esrc-eloss))
      psp_i=(pr_background_num+test)
      m_i=species_m(problem_test_sp(test))
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+(-m_i*rate)
      scoring_data(21+15*psp_i)=scoring_data(21+15*psp_i)+(-m_i*v_test(1
     &)*rate)
      scoring_data(22+15*psp_i)=scoring_data(22+15*psp_i)+(-m_i*v_test(2
     &)*rate)
      scoring_data(23+15*psp_i)=scoring_data(23+15*psp_i)+(-m_i*v_test(3
     &)*rate)
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+(-(0.5_DOUBLE)
     &*m_i*(v_test(1)**2+v_test(2)**2+v_test(3)**2)*rate)
      do i=1,nprod
      prod(i)=problem_test_products(i,1,ts_reac,test)
      test_i=problem_species_test(prod(i))
      if(test_i.NE.0)then
      psp_i=(pr_background_num+test_i)
      else
      back_i=problem_species_background(prod(i))
      if(back_i.NE.0)continue
      psp_i=back_i
      end if
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+(species_m(pro
     &d(i))*rate)
      scoring_data(21+15*psp_i)=scoring_data(21+15*psp_i)+(species_m(pro
     &d(i))*v_prod(1,i)*rate)
      scoring_data(22+15*psp_i)=scoring_data(22+15*psp_i)+(species_m(pro
     &d(i))*v_prod(2,i)*rate)
      scoring_data(23+15*psp_i)=scoring_data(23+15*psp_i)+(species_m(pro
     &d(i))*v_prod(3,i)*rate)
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+((0.5_DOUBLE)*
     &species_m(prod(i))*(v_prod(1,i)**2+v_prod(2,i)**2+v_prod(3,i)**2)*
     &rate)
      end do
      return
      end
      subroutine ion_conv(test,ts_reac,rate,independent_parameters,nprod
     &,prod,v_prod,w_prod,scoring_data,ran_index_x,ran_array_x)
      
      use pr_mod
      
      use rc_mod
      
      use rd_mod
      
      use sp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer inc
      REAL(kind=DOUBLE)random
      external random
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      integer test
      integer ts_reac
      REAL(kind=DOUBLE)rate
      REAL(kind=DOUBLE)independent_parameters(20)
      integer nprod
      integer prod(4)
      REAL(kind=DOUBLE)v_prod(3,4)
      REAL(kind=DOUBLE)w_prod(4),scoring_data(280)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer pr_reac
      integer back
      integer back_i
      integer test_i
      integer i,count,psp_i
      REAL(kind=DOUBLE)v_test(3)
      REAL(kind=DOUBLE)v_back(3)
      REAL(kind=DOUBLE)vb_flow(3)
      REAL(kind=DOUBLE)vt_plasma_frame(3)
      REAL(kind=DOUBLE)vb_therm(3)
      REAL(kind=DOUBLE)vrel(3)
      REAL(kind=DOUBLE)v_i(3)
      REAL(kind=DOUBLE)v_thermal,m_i
      external eval_data
      REAL(kind=DOUBLE)eval_data
      pr_reac=problem_test_reaction(ts_reac,test)
      nprod=reaction_product_num(problem_rc(pr_reac))
      back=problem_test_background(ts_reac,test)
      count=0
      do i=1,3
      v_test(i)=independent_parameters(8+i-1)
      vb_flow(i)=independent_parameters(11+i-1)
      end do
      vt_plasma_frame(1)=v_test(1)-vb_flow(1)
      vt_plasma_frame(2)=v_test(2)-vb_flow(2)
      vt_plasma_frame(3)=v_test(3)-vb_flow(3)
      
90000 continue
      count=count+1
      v_thermal=sqrt(independent_parameters(5))
      call random_gauss(vb_therm,3,ran_index_x,ran_array_x(0))
      do i=1,3
      vb_therm(i)=vb_therm(i)*(v_thermal)
      end do
      vrel(1)=vt_plasma_frame(1)-vb_therm(1)
      vrel(2)=vt_plasma_frame(2)-vb_therm(2)
      vrel(3)=vt_plasma_frame(3)-vb_therm(3)
      
      v_back(1)=vb_therm(1)+vb_flow(1)
      v_back(2)=vb_therm(2)+vb_flow(2)
      v_back(3)=vb_therm(3)+vb_flow(3)
      
      if(problem_num_arrangements(ts_reac,test).EQ.1)continue
      if(nprod.EQ.2)continue
      do i=1,2
      prod(i)=problem_test_products(i,1,ts_reac,test)
      w_prod(i)=(1.0_DOUBLE)
      if(i.EQ.1)then
      v_prod(1,i)=v_back(1)
      v_prod(2,i)=v_back(2)
      v_prod(3,i)=v_back(3)
      
      else
      v_prod(1,i)=v_test(1)
      v_prod(2,i)=v_test(2)
      v_prod(3,i)=v_test(3)
      
      end if
      end do
      do i=1,2
      if(i.EQ.1)then
      m_i=species_m(problem_background_sp(back))
      psp_i=back
      v_i(1)=v_back(1)
      v_i(2)=v_back(2)
      v_i(3)=v_back(3)
      
      else if(i.EQ.2)then
      m_i=species_m(problem_test_sp(test))
      psp_i=(pr_background_num+test)
      v_i(1)=v_test(1)
      v_i(2)=v_test(2)
      v_i(3)=v_test(3)
      
      end if
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+(-m_i*rate)
      scoring_data(21+15*psp_i)=scoring_data(21+15*psp_i)+(-m_i*v_i(1)*r
     &ate)
      scoring_data(22+15*psp_i)=scoring_data(22+15*psp_i)+(-m_i*v_i(2)*r
     &ate)
      scoring_data(23+15*psp_i)=scoring_data(23+15*psp_i)+(-m_i*v_i(3)*r
     &ate)
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+(-(0.5_DOUBLE)
     &*m_i*(v_i(1)**2+v_i(2)**2+v_i(3)**2)*rate)
      end do
      do i=1,nprod
      test_i=problem_species_test(prod(i))
      if(test_i.NE.0)then
      m_i=species_m(problem_test_sp(test_i))
      psp_i=(pr_background_num+test_i)
      else
      back_i=problem_species_background(prod(i))
      if(back_i.NE.0)continue
      m_i=species_m(problem_background_sp(back_i))
      psp_i=back_i
      end if
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+(m_i*rate)
      scoring_data(21+15*psp_i)=scoring_data(21+15*psp_i)+(m_i*v_prod(1,
     &i)*rate)
      scoring_data(22+15*psp_i)=scoring_data(22+15*psp_i)+(m_i*v_prod(2,
     &i)*rate)
      scoring_data(23+15*psp_i)=scoring_data(23+15*psp_i)+(m_i*v_prod(3,
     &i)*rate)
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+((0.5_DOUBLE)*
     &m_i*(v_prod(1,i)**2+v_prod(2,i)**2+v_prod(3,i)**2)*rate)
      end do
      return
      end
      subroutine product_velocities(angle,independent_parameters,v_test,
     &v_back,vrel,v_prod,ran_index_x,ran_array_x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)angle
      REAL(kind=DOUBLE)independent_parameters(20)
      REAL(kind=DOUBLE)v_test(3)
      REAL(kind=DOUBLE)v_back(3)
      REAL(kind=DOUBLE)vrel(3)
      REAL(kind=DOUBLE)v_prod(3,4)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      REAL(kind=DOUBLE)sin_eps,cos_eps,cos_chi2,sin_chi2,m_tot,vrel_sq,v
     &rel_diff,vrel_mag,aps_perp
      REAL(kind=DOUBLE)ran,phi
      integer i
      external apse
      REAL(kind=DOUBLE)apse
      REAL(kind=DOUBLE)random
      external random
      ran=random(ran_index_x,ran_array_x(0))
      phi=atan2((0.0_DOUBLE),-(1.0_DOUBLE))*((2.0_DOUBLE)*ran-(1.0_DOUBL
     &E))
      sin_eps=sin(phi)
      cos_eps=cos(phi)
      vrel_sq=(vrel(1)**2+vrel(2)**2+vrel(3)**2)
      vrel_mag=sqrt(vrel_sq)
      cos_chi2=cos((0.5_DOUBLE)*angle)
      sin_chi2=sin((0.5_DOUBLE)*angle)
      m_tot=(1.0_DOUBLE)/(independent_parameters(14)+independent_paramet
     &ers(15))
      vrel_diff=sqrt((1.0_DOUBLE)-vrel(1)**2/vrel_sq)
      if(vrel_diff.EQ.(0.0_DOUBLE))vrel_diff=(1.e-15_DOUBLE)
      do i=1,3
      aps_perp=apse(i,cos_chi2,vrel(1),vrel_sq,vrel_mag,vrel_diff,cos_ep
     &s,sin_eps)
      v_prod(i,1)=v_back(i)+(2.0_DOUBLE)*independent_parameters(14)*m_to
     &t*sin_chi2*(vrel(i)*sin_chi2+vrel_mag*aps_perp)
      v_prod(i,2)=v_test(i)-(2.0_DOUBLE)*independent_parameters(15)*m_to
     &t*sin_chi2*(vrel(i)*sin_chi2+vrel_mag*aps_perp)
      end do
      return
      end
      function apse(i,cos_chi2,vrel,vrel_sq,vrel_mag,vrel_diff,cos_eps,s
     &in_eps)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)apse
      integer i
      REAL(kind=DOUBLE)cos_chi2,cos_eps,sin_eps,vrel_sq,vrel_mag,vrel_di
     &ff
      REAL(kind=DOUBLE)vrel(3)
      REAL(kind=DOUBLE)coef_a,coef_b
      if(i.EQ.1)then
      apse=cos_chi2*vrel_diff*cos_eps
      elseif(i.EQ.2)then
      coef_a=vrel(2)*vrel(1)*cos_eps+vrel(3)*vrel_mag*sin_eps
      coef_b=vrel_sq*vrel_diff
      apse=-cos_chi2*coef_a/coef_b
      else
      coef_a=vrel(3)*vrel(1)*cos_eps-vrel(2)*vrel_mag*sin_eps
      coef_b=vrel_sq*vrel_diff
      apse=-cos_chi2*coef_a/coef_b
      endif
      return
      end
      subroutine pick_track_reaction(test,ts_reac,rate,independent_param
     &eters,scoring_data,ran_index_x,ran_array_x)
      
      use rc_mod
      
      use pr_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer test
      integer ts_reac
      REAL(kind=DOUBLE)rate
      REAL(kind=DOUBLE)independent_parameters(20)
      REAL(kind=DOUBLE)scoring_data(280)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer pr_reac
      pr_reac=problem_test_reaction(ts_reac,test)
      if(reaction_type(problem_rc(pr_reac)).EQ.'chargex')then
      call track_chargex(test,ts_reac,rate,independent_parameters,scorin
     &g_data,ran_index_x,ran_array_x(0))
      else if(reaction_type(problem_rc(pr_reac)).EQ.'elastic')then
      call track_elastic(test,ts_reac,rate,independent_parameters,scorin
     &g_data,ran_index_x,ran_array_x(0))
      else if(reaction_type(problem_rc(pr_reac)).EQ.'dissoc')then
      call track_dissoc(test,ts_reac,rate,independent_parameters,scoring
     &_data,ran_index_x,ran_array_x(0))
      else if(reaction_type(problem_rc(pr_reac)).EQ.'dissoc_rec')then
      call track_dissoc(test,ts_reac,rate,independent_parameters,scoring
     &_data,ran_index_x,ran_array_x(0))
      else if(reaction_type(problem_rc(pr_reac)).EQ.'dissoc_cramd')then
      call track_dissoc_cramd(test,ts_reac,rate,independent_parameters,s
     &coring_data,ran_index_x,ran_array_x(0))
      else if(reaction_type(problem_rc(pr_reac)).EQ.'ionize')then
      call track_ionize(test,ts_reac,rate,independent_parameters,scoring
     &_data,ran_index_x,ran_array_x(0))
      else if(reaction_type(problem_rc(pr_reac)).EQ.'ionize_suppress')th
     &en
      call track_ionize(test,ts_reac,rate,independent_parameters,scoring
     &_data,ran_index_x,ran_array_x(0))
      else if(reaction_type(problem_rc(pr_reac)).EQ.'excitation')then
      call track_excite(test,ts_reac,rate,independent_parameters,scoring
     &_data,ran_index_x,ran_array_x(0))
      else if(reaction_type(problem_rc(pr_reac)).EQ.'deexcitation')then
      call track_excite(test,ts_reac,rate,independent_parameters,scoring
     &_data,ran_index_x,ran_array_x(0))
      else
      if('Unsupported reaction type'.EQ.' ')continue
      end if
      return
      end
      subroutine track_chargex(test,ts_reac,rate,independent_parameters,
     &scoring_data,ran_index_x,ran_array_x)
      
      use rc_mod
      
      use rd_mod
      
      use pr_mod
      
      use sp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer inc
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      integer test
      integer ts_reac
      REAL(kind=DOUBLE)rate
      REAL(kind=DOUBLE)independent_parameters(20)
      REAL(kind=DOUBLE)scoring_data(280)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer pr_reac
      integer back
      integer i,j,i_0_1,i_0_2,nprod,back_i,test_i,psp_i
      REAL(kind=DOUBLE)ni_1up,ni_2up2,up2,up,u_abs,uprime_abs,m_i,e_src,
     &e_eff,i_term,i_term_org,i_term_lim
      REAL(kind=DOUBLE)u(3)
      REAL(kind=DOUBLE)uprime(3)
      REAL(kind=DOUBLE)vb_flow(3)
      REAL(kind=DOUBLE)mom_src(3)
      REAL(kind=DOUBLE)vr_i(3)
      logical init
      external eval_data
      REAL(kind=DOUBLE)eval_data
      save init,i_0_1,i_0_2
      data init/.TRUE./
      if(init)then
      i_0_1=string_lookup('I_0_1*up',pr_var0_list,pr_var0_num)
      i_0_2=string_lookup('I_0_2*up^2',pr_var0_list,pr_var0_num)
      if(i_0_1.GT.0.AND.i_0_2.GT.0)continue
      init=.FALSE.
      end if
      pr_reac=problem_test_reaction(ts_reac,test)
      nprod=reaction_product_num(problem_rc(pr_reac))
      back=problem_test_background(ts_reac,test)
      ni_1up=(314159265.3589793238462_DOUBLE)
      ni_2up2=(314159265.3589793238462_DOUBLE)
      do i=1,19
      if(reaction_handling_var0(i,pr_reac).EQ.i_0_1)then
      ni_1up=independent_parameters(1)*eval_data(reaction_handling_eval_
     &name(i,pr_reac),reaction_handling_rank(i,pr_reac),reaction_handlin
     &g_var(1,i,pr_reac),reaction_handling_tab_index(1,i,pr_reac),reacti
     &on_handling_spacing(0,i,pr_reac),reaction_handling_min(1,i,pr_reac
     &),reaction_handling_delta(1,i,pr_reac),reaction_handling_tab(react
     &ion_handling_base(i,pr_reac)),independent_parameters,ran_index_x,r
     &an_array_x(0))
      else if(reaction_handling_var0(i,pr_reac).EQ.i_0_2)then
      ni_2up2=independent_parameters(1)*eval_data(reaction_handling_eval
     &_name(i,pr_reac),reaction_handling_rank(i,pr_reac),reaction_handli
     &ng_var(1,i,pr_reac),reaction_handling_tab_index(1,i,pr_reac),react
     &ion_handling_spacing(0,i,pr_reac),reaction_handling_min(1,i,pr_rea
     &c),reaction_handling_delta(1,i,pr_reac),reaction_handling_tab(reac
     &tion_handling_base(i,pr_reac)),independent_parameters,ran_index_x,
     &ran_array_x(0))
      end if
      end do
      if(ni_1up.NE.(314159265.3589793238462_DOUBLE).AND.ni_2up2.NE.(3141
     &59265.3589793238462_DOUBLE))continue
      do i=1,3
      u(i)=independent_parameters(8+i-1)
      vb_flow(i)=independent_parameters(11+i-1)
      end do
      up2=(2.0_DOUBLE)*independent_parameters(5)
      up=sqrt(up2)
      u_abs=sqrt((u(1)**2+u(2)**2+u(3)**2))
      uprime(1)=u(1)-vb_flow(1)
      uprime(2)=u(2)-vb_flow(2)
      uprime(3)=u(3)-vb_flow(3)
      
      uprime_abs=sqrt((uprime(1)**2+uprime(2)**2+uprime(3)**2))
      i_term_org=ni_1up/uprime_abs-(0.5_DOUBLE)*up2*rate/uprime_abs**2
      i_term_lim=(2.0_DOUBLE)*ni_2up2/((3._DOUBLE)*up2)
      if(i_term_org.LT.(0.0_DOUBLE))then
      i_term=i_term_lim
      if(uprime_abs.LT.(0.5_DOUBLE)*up)continue
      else
      i_term=i_term_org
      end if
      do i=1,2
      if(i.EQ.1)then
      m_i=species_m(problem_background_sp(back))
      psp_i=back
      do j=1,3
      mom_src(j)=rate*u(j)-i_term*uprime(j)
      end do
      vr_i(1)=mom_src(1)
      vr_i(2)=mom_src(2)
      vr_i(3)=mom_src(3)
      
      e_src=(0.5_DOUBLE)*u_abs**2*rate-(u(1)*uprime(1)+u(2)*uprime(2)+u(
     &3)*uprime(3))*i_term+(0.5_DOUBLE)*ni_2up2
      e_eff=e_src
      else if(i.EQ.2)then
      m_i=species_m(problem_test_sp(test))
      psp_i=(pr_background_num+test)
      vr_i(1)=(rate)*u(1)
      vr_i(2)=(rate)*u(2)
      vr_i(3)=(rate)*u(3)
      
      e_eff=(0.5_DOUBLE)*u_abs**2*rate
      end if
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+(-m_i*rate)
      scoring_data(21+15*psp_i)=scoring_data(21+15*psp_i)+(-m_i*vr_i(1))
      scoring_data(22+15*psp_i)=scoring_data(22+15*psp_i)+(-m_i*vr_i(2))
      scoring_data(23+15*psp_i)=scoring_data(23+15*psp_i)+(-m_i*vr_i(3))
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+(-m_i*e_eff)
      end do
      if(nprod.EQ.2)continue
      do i=1,2
      if(i.EQ.1)then
      vr_i(1)=mom_src(1)
      vr_i(2)=mom_src(2)
      vr_i(3)=mom_src(3)
      
      e_eff=e_src
      else
      vr_i(1)=(rate)*u(1)
      vr_i(2)=(rate)*u(2)
      vr_i(3)=(rate)*u(3)
      
      e_eff=(0.5_DOUBLE)*u_abs**2*rate
      end if
      test_i=problem_species_test(problem_test_products(i,1,ts_reac,test
     &))
      if(test_i.NE.0)then
      m_i=species_m(problem_test_sp(test_i))
      psp_i=(pr_background_num+test_i)
      else
      back_i=problem_species_background(problem_test_products(i,1,ts_rea
     &c,test))
      if(back_i.NE.0)continue
      m_i=species_m(problem_background_sp(back_i))
      psp_i=back_i
      end if
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+(m_i*rate)
      scoring_data(21+15*psp_i)=scoring_data(21+15*psp_i)+(m_i*vr_i(1))
      scoring_data(22+15*psp_i)=scoring_data(22+15*psp_i)+(m_i*vr_i(2))
      scoring_data(23+15*psp_i)=scoring_data(23+15*psp_i)+(m_i*vr_i(3))
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+(m_i*e_eff)
      end do
      return
      end
      subroutine track_elastic(test,ts_reac,rate,independent_parameters,
     &scoring_data,ran_index_x,ran_array_x)
      
      use rc_mod
      
      use rd_mod
      
      use pr_mod
      
      use sp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer inc
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      integer test
      integer ts_reac
      REAL(kind=DOUBLE)rate
      REAL(kind=DOUBLE)independent_parameters(20)
      REAL(kind=DOUBLE)scoring_data(280)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer pr_reac
      integer back
      integer i,i_1_0,i_1_1,i_1_2,psp_i
      REAL(kind=DOUBLE)ni_0,ni_1up,ni_2up2,up2,up,m_test,m_back,mom_src,
     &uprime_abs,usq,e_src
      REAL(kind=DOUBLE)u(3)
      REAL(kind=DOUBLE)uprime(3)
      REAL(kind=DOUBLE)vb_flow(3)
      logical init
      external eval_data
      REAL(kind=DOUBLE)eval_data
      save init,i_1_0,i_1_1,i_1_2
      data init/.TRUE./
      if(init)then
      i_1_0=string_lookup('I_1_0',pr_var0_list,pr_var0_num)
      i_1_1=string_lookup('I_1_1*up',pr_var0_list,pr_var0_num)
      i_1_2=string_lookup('I_1_2*up^2',pr_var0_list,pr_var0_num)
      if(i_1_0.GT.0.AND.i_1_1.GT.0.AND.i_1_2.GT.0)continue
      init=.FALSE.
      end if
      pr_reac=problem_test_reaction(ts_reac,test)
      back=problem_test_background(ts_reac,test)
      ni_0=(314159265.3589793238462_DOUBLE)
      ni_1up=(314159265.3589793238462_DOUBLE)
      ni_2up2=(314159265.3589793238462_DOUBLE)
      do i=1,19
      if(reaction_handling_var0(i,pr_reac).EQ.i_1_0)then
      ni_0=independent_parameters(1)*eval_data(reaction_handling_eval_na
     &me(i,pr_reac),reaction_handling_rank(i,pr_reac),reaction_handling_
     &var(1,i,pr_reac),reaction_handling_tab_index(1,i,pr_reac),reaction
     &_handling_spacing(0,i,pr_reac),reaction_handling_min(1,i,pr_reac),
     &reaction_handling_delta(1,i,pr_reac),reaction_handling_tab(reactio
     &n_handling_base(i,pr_reac)),independent_parameters,ran_index_x,ran
     &_array_x(0))
      else if(reaction_handling_var0(i,pr_reac).EQ.i_1_1)then
      ni_1up=independent_parameters(1)*eval_data(reaction_handling_eval_
     &name(i,pr_reac),reaction_handling_rank(i,pr_reac),reaction_handlin
     &g_var(1,i,pr_reac),reaction_handling_tab_index(1,i,pr_reac),reacti
     &on_handling_spacing(0,i,pr_reac),reaction_handling_min(1,i,pr_reac
     &),reaction_handling_delta(1,i,pr_reac),reaction_handling_tab(react
     &ion_handling_base(i,pr_reac)),independent_parameters,ran_index_x,r
     &an_array_x(0))
      else if(reaction_handling_var0(i,pr_reac).EQ.i_1_2)then
      ni_2up2=independent_parameters(1)*eval_data(reaction_handling_eval
     &_name(i,pr_reac),reaction_handling_rank(i,pr_reac),reaction_handli
     &ng_var(1,i,pr_reac),reaction_handling_tab_index(1,i,pr_reac),react
     &ion_handling_spacing(0,i,pr_reac),reaction_handling_min(1,i,pr_rea
     &c),reaction_handling_delta(1,i,pr_reac),reaction_handling_tab(reac
     &tion_handling_base(i,pr_reac)),independent_parameters,ran_index_x,
     &ran_array_x(0))
      end if
      end do
      if(ni_0.NE.(314159265.3589793238462_DOUBLE).AND.ni_1up.NE.(3141592
     &65.3589793238462_DOUBLE).AND.ni_2up2.NE.(314159265.3589793238462_D
     &OUBLE))continue
      do i=1,3
      u(i)=independent_parameters(8+i-1)
      vb_flow(i)=independent_parameters(11+i-1)
      end do
      up2=(2.0_DOUBLE)*independent_parameters(5)
      up=sqrt(up2)
      usq=(u(1)*u(1)+u(2)*u(2)+u(3)*u(3))
      uprime(1)=u(1)-vb_flow(1)
      uprime(2)=u(2)-vb_flow(2)
      uprime(3)=u(3)-vb_flow(3)
      
      uprime_abs=sqrt((uprime(1)**2+uprime(2)**2+uprime(3)**2))
      m_test=independent_parameters(14)
      m_back=independent_parameters(15)
      psp_i=back
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+((0.0_DOUBLE))
      mom_src=(m_test*m_back/(m_test+m_back))*(-(0.5_DOUBLE)*up2*ni_0/up
     &rime_abs**2+ni_1up/uprime_abs)
      scoring_data(21+15*psp_i)=scoring_data(21+15*psp_i)+(mom_src*uprim
     &e(1))
      scoring_data(22+15*psp_i)=scoring_data(22+15*psp_i)+(mom_src*uprim
     &e(2))
      scoring_data(23+15*psp_i)=scoring_data(23+15*psp_i)+(mom_src*uprim
     &e(3))
      e_src=((2.0_DOUBLE)*m_test*m_back/(m_test+m_back)**2)*(-(0.5_DOUBL
     &E)*m_back*ni_2up2+(0.5_DOUBLE)*(m_test+m_back)*(u(1)*uprime(1)+u(2
     &)*uprime(2)+u(3)*uprime(3))*(ni_1up/uprime_abs-(0.5_DOUBLE)*up2*ni
     &_0/uprime_abs**2))
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+(e_src)
      psp_i=(pr_background_num+test)
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+((0.0_DOUBLE))
      scoring_data(21+15*psp_i)=scoring_data(21+15*psp_i)+(-mom_src*upri
     &me(1))
      scoring_data(22+15*psp_i)=scoring_data(22+15*psp_i)+(-mom_src*upri
     &me(2))
      scoring_data(23+15*psp_i)=scoring_data(23+15*psp_i)+(-mom_src*upri
     &me(3))
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+(-e_src)
      
      return
      end
      subroutine track_dissoc(test,ts_reac,rate,independent_parameters,s
     &coring_data,ran_index_x,ran_array_x)
      
      use pr_mod
      
      use rc_mod
      
      use sp_mod
      
      use rd_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer inc
      REAL(kind=DOUBLE)random
      external random
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      integer test
      integer ts_reac
      REAL(kind=DOUBLE)rate
      REAL(kind=DOUBLE)independent_parameters(*)
      REAL(kind=DOUBLE)scoring_data(280)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer pr_reac
      integer back
      integer back_i
      integer test_i
      integer reac
      integer prod(4)
      integer i,i_ediss,i_elediss,nprod,num_heavy_products,j_arr,k,psp_i
     &,num_lines,i_e_rate,i_gen_rate,i_line
      integer product_index(2),lines(6),e_rates(6)
      REAL(kind=DOUBLE)v_test(3)
      REAL(kind=DOUBLE)v_emitter(3)
      REAL(kind=DOUBLE)ediss,elediss,mass_total,m_i,this_line
      REAL(kind=DOUBLE)mass(2)
      logical init,first_line
      character*40 line
      external eval_data
      REAL(kind=DOUBLE)eval_data
      save init,i_ediss,i_elediss,num_lines,lines,e_rates,i_gen_rate
      data init/.TRUE./
      if(init)then
      i_ediss=string_lookup('dissociation_energy',pr_var0_list,pr_var0_n
     &um)
      i_elediss=string_lookup('background_energy_loss_rate',pr_var0_list
     &,pr_var0_num)
      if(i_ediss.GT.0.AND.i_elediss.GT.0)continue
      num_lines=0
      do i=1,pr_var0_num
      if(pr_var0_list(i) (1:10).EQ.'wavelength')then
      num_lines=num_lines+1
      lines(num_lines)=i
      line=pr_var0_list(i) (11:)
      i_e_rate=string_lookup('emission_rate'//line,pr_var0_list,pr_var0_
     &num)
      if(i_e_rate.GT.0)continue
      e_rates(num_lines)=i_e_rate
      end if
      end do
      if(num_lines.GT.0)then
      i_gen_rate=string_lookup('emission_rate',pr_var0_list,pr_var0_num)
      end if
      init=.FALSE.
      end if
      pr_reac=problem_test_reaction(ts_reac,test)
      back=problem_test_background(ts_reac,test)
      nprod=reaction_product_num(problem_rc(pr_reac))
      do i=1,19
      if(reaction_handling_var0(i,pr_reac).EQ.i_ediss)then
      ediss=eval_data(reaction_handling_eval_name(i,pr_reac),reaction_ha
     &ndling_rank(i,pr_reac),reaction_handling_var(1,i,pr_reac),reaction
     &_handling_tab_index(1,i,pr_reac),reaction_handling_spacing(0,i,pr_
     &reac),reaction_handling_min(1,i,pr_reac),reaction_handling_delta(1
     &,i,pr_reac),reaction_handling_tab(reaction_handling_base(i,pr_reac
     &)),independent_parameters,ran_index_x,ran_array_x(0))
      else if(reaction_handling_var0(i,pr_reac).EQ.i_elediss)then
      elediss=independent_parameters(1)*eval_data(reaction_handling_eval
     &_name(i,pr_reac),reaction_handling_rank(i,pr_reac),reaction_handli
     &ng_var(1,i,pr_reac),reaction_handling_tab_index(1,i,pr_reac),react
     &ion_handling_spacing(0,i,pr_reac),reaction_handling_min(1,i,pr_rea
     &c),reaction_handling_delta(1,i,pr_reac),reaction_handling_tab(reac
     &tion_handling_base(i,pr_reac)),independent_parameters,ran_index_x,
     &ran_array_x(0))
      else if(reaction_handling_var0(i,pr_reac).NE.1)then
      scoring_data(reaction_handling_var0(i,pr_reac))=eval_data(reaction
     &_handling_eval_name(i,pr_reac),reaction_handling_rank(i,pr_reac),r
     &eaction_handling_var(1,i,pr_reac),reaction_handling_tab_index(1,i,
     &pr_reac),reaction_handling_spacing(0,i,pr_reac),reaction_handling_
     &min(1,i,pr_reac),reaction_handling_delta(1,i,pr_reac),reaction_han
     &dling_tab(reaction_handling_base(i,pr_reac)),independent_parameter
     &s,ran_index_x,ran_array_x(0))
      end if
      end do
      do i=1,3
      v_test(i)=independent_parameters(8+i-1)
      end do
      reac=problem_rc(pr_reac)
      if(reaction_emitter(reac).NE.0)then
      if(reaction_emitter(reac).GT.0)then
      v_emitter(1)=v_test(1)
      v_emitter(2)=v_test(2)
      v_emitter(3)=v_test(3)
      
      else if(species_generic(reaction_reagent(-reaction_emitter(reac),r
     &eac)).EQ.species_generic(problem_test_sp(test)))then
      v_emitter(1)=v_test(1)
      v_emitter(2)=v_test(2)
      v_emitter(3)=v_test(3)
      
      else if(species_generic(reaction_reagent(-reaction_emitter(reac),r
     &eac)).EQ.species_generic(problem_background_sp(back)))then
      if('background velocity not set'.EQ.' ')continue
      end if
      scoring_data(8)=v_emitter(1)
      scoring_data(9)=v_emitter(2)
      scoring_data(10)=v_emitter(3)
      end if
      psp_i=back
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+(-species_m(pr
     &oblem_background_sp(back))*rate)
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+(-elediss)
      psp_i=(pr_background_num+test)
      m_i=species_m(problem_test_sp(test))
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+(-m_i*rate)
      scoring_data(21+15*psp_i)=scoring_data(21+15*psp_i)+(-m_i*v_test(1
     &)*rate)
      scoring_data(22+15*psp_i)=scoring_data(22+15*psp_i)+(-m_i*v_test(2
     &)*rate)
      scoring_data(23+15*psp_i)=scoring_data(23+15*psp_i)+(-m_i*v_test(3
     &)*rate)
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+(-(0.5_DOUBLE)
     &*m_i*(v_test(1)**2+v_test(2)**2+v_test(3)**2)*rate)
      do j_arr=1,problem_num_arrangements(ts_reac,test)
      num_heavy_products=0
      mass_total=(0.0_DOUBLE)
      do i=1,nprod
      prod(i)=problem_test_products(i,j_arr,ts_reac,test)
      test_i=problem_species_test(prod(i))
      if(test_i.NE.0)then
      psp_i=(pr_background_num+test_i)
      else
      back_i=problem_species_background(prod(i))
      if(back_i.NE.0)continue
      psp_i=back_i
      end if
      scoring_data(20+15*psp_i)=scoring_data(20+15*psp_i)+(species_m(pro
     &d(i))*rate*problem_prod_mult(j_arr,ts_reac,test))
      if(species_sy(prod(i)).NE.'e')then
      num_heavy_products=num_heavy_products+1
      mass(num_heavy_products)=species_m(prod(i))
      mass_total=mass_total+(species_m(prod(i)))
      product_index(num_heavy_products)=i
      end if
      end do
      if(num_heavy_products.EQ.2)continue
      if(reaction_emitter(reac).NE.0)then
      if(reaction_emitter(reac).GT.0)then
      independent_parameters(16)=species_m(prod(reaction_emitter(reac)))
      if(species_sy(prod(reaction_emitter(reac))).NE.'e')continue
      else if(species_generic(reaction_reagent(-reaction_emitter(reac),r
     &eac)).EQ.species_generic(problem_test_sp(test)))then
      independent_parameters(16)=species_m(problem_test_sp(test))
      else if(species_generic(reaction_reagent(-reaction_emitter(reac),r
     &eac)).EQ.species_generic(problem_background_sp(back)))then
      if('background velocity not set'.EQ.' ')continue
      end if
      end if
      if(num_lines.GT.0)then
      first_line=.TRUE.
      do i_line=1,num_lines
      do i=1,19
      if(reaction_handling_var0(i,pr_reac).EQ.lines(i_line))then
      this_line=eval_data(reaction_handling_eval_name(i,pr_reac),reactio
     &n_handling_rank(i,pr_reac),reaction_handling_var(1,i,pr_reac),reac
     &tion_handling_tab_index(1,i,pr_reac),reaction_handling_spacing(0,i
     &,pr_reac),reaction_handling_min(1,i,pr_reac),reaction_handling_del
     &ta(1,i,pr_reac),reaction_handling_tab(reaction_handling_base(i,pr_
     &reac)),independent_parameters,ran_index_x,ran_array_x(0))
      if(this_line.GT.(0.0_DOUBLE))then
      if(reaction_emitter(reac).NE.0)continue
      if((i_gen_rate.GT.0).AND.first_line)then
      scoring_data(e_rates(i_line))=scoring_data(e_rates(i_line))+(scori
     &ng_data(i_gen_rate)*independent_parameters(1)*problem_prod_mult(j_
     &arr,ts_reac,test))
      first_line=.FALSE.
      else
      if(scoring_data(lines(i_line)).GT.(0.0_DOUBLE))then
      scoring_data(e_rates(i_line))=scoring_data(e_rates(i_line))*((inde
     &pendent_parameters(1)*problem_prod_mult(j_arr,ts_reac,test)))
      end if
      end if
      end if
      end if
      end do
      end do
      end if
      do k=1,num_heavy_products
      test_i=problem_species_test(problem_test_products(product_index(k)
     &,j_arr,ts_reac,test))
      if(test_i.NE.0)then
      psp_i=(pr_background_num+test_i)
      else
      back_i=problem_species_background(problem_test_products(product_in
     &dex(k),j_arr,ts_reac,test))
      if(back_i.NE.0)continue
      psp_i=back_i
      end if
      scoring_data(21+15*psp_i)=scoring_data(21+15*psp_i)+(mass(k)*v_tes
     &t(1)*rate*problem_prod_mult(j_arr,ts_reac,test))
      scoring_data(22+15*psp_i)=scoring_data(22+15*psp_i)+(mass(k)*v_tes
     &t(2)*rate*problem_prod_mult(j_arr,ts_reac,test))
      scoring_data(23+15*psp_i)=scoring_data(23+15*psp_i)+(mass(k)*v_tes
     &t(3)*rate*problem_prod_mult(j_arr,ts_reac,test))
      scoring_data(24+15*psp_i)=scoring_data(24+15*psp_i)+(((0.5_DOUBLE)
     &*mass(k)*(v_test(1)**2+v_test(2)**2+v_test(3)**2)+(2.0_DOUBLE)*edi
     &ss*((1.0_DOUBLE)-mass(k)/mass_total))*rate*problem_prod_mult(j_arr
     &,ts_reac,test))
      end do
      end do
      return
      end
      subroutine track_dissoc_cramd(test,ts_reac,rate,independent_parame
     &ters,scoring_data,ran_index_x,ran_array_x)
      
      use pr_mod
      
      use rc_mod
      
      use sp_mod
      
      use rd_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer inc
      REAL(kind=DOUBLE)random
      external random
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      integer test
      integer ts_reac
      REAL(kind=DOUBLE)rate
      REAL(kind=DOUBLE)independent_parameters(*)
      REAL(kind=DOUBLE)scoring_data(280)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer pr_reac
      integer back
      integer back_i
      integer prod(4)
      integer i,i_eloss,i_iloss,i_hfrac,num_heavy_products,j_arr,k,nprod
      integer product_index(2)
      REAL(kind=DOUBLE)v_test(3)
      REAL(kind=DOUBLE)eloss,iloss,hfrac,ediss,mass_total
      REAL(kind=DOUBLE)mass(2)
      logical init
      external eval_data
      REAL(kind=DOUBLE)eval_data
      save init,i_eloss,i_iloss,i_hfrac
      data init/.TRUE./
      if(init)then
      i_eloss=string_lookup('background_energy_loss_rate',pr_var0_list,p
     &r_var0_num)
      i_iloss=string_lookup('proton_energy_loss_rate',pr_var0_list,pr_va
     &r0_num)
      i_hfrac=string_lookup('H_fraction',pr_var0_list,pr_var0_num)
      if(i_eloss.GT.0.AND.i_iloss.GT.0.AND.i_hfrac.GT.0)continue
      init=.FALSE.
      end if
      pr_reac=problem_test_reaction(ts_reac,test)
      nprod=reaction_product_num(problem_rc(pr_reac))
      back=problem_test_background(ts_reac,test)
      
      do i=1,19
      if(reaction_handling_var0(i,pr_reac).EQ.i_eloss)then
      eloss=independent_parameters(1)*eval_data(reaction_handling_eval_n
     &ame(i,pr_reac),reaction_handling_rank(i,pr_reac),reaction_handling
     &_var(1,i,pr_reac),reaction_handling_tab_index(1,i,pr_reac),reactio
     &n_handling_spacing(0,i,pr_reac),reaction_handling_min(1,i,pr_reac)
     &,reaction_handling_delta(1,i,pr_reac),reaction_handling_tab(reacti
     &on_handling_base(i,pr_reac)),independent_parameters,ran_index_x,ra
     &n_array_x(0))
      else if(reaction_handling_var0(i,pr_reac).EQ.i_iloss)then
      iloss=independent_parameters(1)*eval_data(reaction_handling_eval_n
     &ame(i,pr_reac),reaction_handling_rank(i,pr_reac),reaction_handling
     &_var(1,i,pr_reac),reaction_handling_tab_index(1,i,pr_reac),reactio
     &n_handling_spacing(0,i,pr_reac),reaction_handling_min(1,i,pr_reac)
     &,reaction_handling_delta(1,i,pr_reac),reaction_handling_tab(reacti
     &on_handling_base(i,pr_reac)),independent_parameters,ran_index_x,ra
     &n_array_x(0))
      else if(reaction_handling_var0(i,pr_reac).EQ.i_hfrac)then
      hfrac=eval_data(reaction_handling_eval_name(i,pr_reac),reaction_ha
     &ndling_rank(i,pr_reac),reaction_handling_var(1,i,pr_reac),reaction
     &_handling_tab_index(1,i,pr_reac),reaction_handling_spacing(0,i,pr_
     &reac),reaction_handling_min(1,i,pr_reac),reaction_handling_delta(1
     &,i,pr_reac),reaction_handling_tab(reaction_handling_base(i,pr_reac
     &)),independent_parameters,ran_index_x,ran_array_x(0))
      end if
      end do
      
      ediss=(3._DOUBLE)*(1.60217733e-19_DOUBLE)
      
      if(species_ncomp(problem_test_sp(test)).EQ.1)continue
      if(species_count(1,problem_test_sp(test)).EQ.2)continue
      return
      end
      subroutine track_ionize(test,ts_reac,rate,independent_parameters,s
     &coring_data,ran_index_x,ran_array_x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer test
      integer ts_reac
      REAL(kind=DOUBLE)rate
      REAL(kind=DOUBLE)independent_parameters(20)
      REAL(kind=DOUBLE)scoring_data(280)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer nprod
      REAL(kind=DOUBLE)w_prod(4)
      integer prod(4)
      REAL(kind=DOUBLE)v_prod(3,4)
      
      call ionize(test,ts_reac,rate,independent_parameters,nprod,prod,v_
     &prod(1,1),w_prod,scoring_data,ran_index_x,ran_array_x(0))
      return
      end
      subroutine track_excite(test,ts_reac,rate,independent_parameters,s
     &coring_data,ran_index_x,ran_array_x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer test
      integer ts_reac
      REAL(kind=DOUBLE)rate
      REAL(kind=DOUBLE)independent_parameters(20)
      REAL(kind=DOUBLE)scoring_data(280)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer nprod
      REAL(kind=DOUBLE)w_prod(4)
      integer prod(4)
      REAL(kind=DOUBLE)v_prod(3,4)
      
      call excite(test,ts_reac,rate,independent_parameters,nprod,prod,v_
     &prod(1,1),w_prod,scoring_data,ran_index_x,ran_array_x(0))
      return
      end
      subroutine recombination_data(est,number_x,source_x,source_kseg_x,
     &source_xseg_x,source_type_x,source_root_sp_x,species_origin_x,test
     &_origin_x,time_origin_x,weight_origin_x,pos_origin_x,cell_origin_x
     &,zone_origin_x,surface_origin_x,cell_next_origin_x,zone_next_origi
     &n_x,sector_origin_x,sector_next_origin_x,velocity_origin_x,type_or
     &igin_x,author_origin_x,ran_index_x,ran_array_x,species_stack_x,tes
     &t_stack_x,time_stack_x,weight_stack_x,pos_stack_x,cell_stack_x,zon
     &e_stack_x,surface_stack_x,cell_next_stack_x,zone_next_stack_x,sect
     &or_stack_x,sector_next_stack_x,velocity_stack_x,type_stack_x,autho
     &r_stack_x,pointer_x,scoring_data)
      
      use pr_mod
      
      use bk_mod
      
      use gi_mod
      
      use rc_mod
      
      use sp_mod
      
      use rd_mod
      
      use zn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer inc
      external locate_point,check_location
      integer locate_point
      logical check_location
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      integer est
      integer number_x,source_x,source_kseg_x,source_xseg_x,source_type_
     &x,source_root_sp_x,pointer_x
      integer species_origin_x
      integer test_origin_x
      REAL(kind=DOUBLE)time_origin_x,weight_origin_x,velocity_origin_x(3
     &)
      REAL(kind=DOUBLE)pos_origin_x(3)
      integer cell_origin_x,zone_origin_x,surface_origin_x,cell_next_ori
     &gin_x,zone_next_origin_x,sector_origin_x,sector_next_origin_x
      integer type_origin_x,author_origin_x
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer species_stack_x(40)
      integer test_stack_x(40)
      REAL(kind=DOUBLE)time_stack_x(40),weight_stack_x(40),velocity_stac
     &k_x(3,40)
      REAL(kind=DOUBLE)pos_stack_x(3,40)
      integer cell_stack_x(40),zone_stack_x(40),surface_stack_x(40),cell
     &_next_stack_x(40),zone_next_stack_x(40),sector_stack_x(40),sector_
     &next_stack_x(40)
      integer type_stack_x(40),author_stack_x(40)
      REAL(kind=DOUBLE)scoring_data(280)
      integer back_2
      integer back_1
      integer bk_rc_is
      integer pr_reac
      integer sptest
      integer reac
      REAL(kind=DOUBLE)pos_zone_loc(3)
      integer cell_zone_loc,zone_zone_loc,surface_zone_loc,cell_next_zon
     &e_loc,zone_next_zone_loc,sector_zone_loc,sector_next_zone_loc
      REAL(kind=DOUBLE)vb_flow(3)
      REAL(kind=DOUBLE)v_src(3)
      integer i,i_e_loss,i_e_src,num_lines,i_e_rate,i_gen_rate,psp_1,psp
     &_2,xseg
      integer lines(6),e_rates(6)
      REAL(kind=DOUBLE)rate,e_loss,e_e_loss,e_e_src
      REAL(kind=DOUBLE)independent_parameters(20)
      character*40 line
      logical init,first_line
      external eval_data,find_rate
      REAL(kind=DOUBLE)eval_data,find_rate
      save init,i_e_loss,i_e_src,num_lines,lines,e_rates,i_gen_rate
      data init/.TRUE./
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
      
      
      REAL(kind=DOUBLE)random
      external random
      if(init)then
      i_e_loss=string_lookup('background_energy_loss_rate',pr_var0_list,
     &pr_var0_num)
      i_e_src=string_lookup('background_energy_source_rate',pr_var0_list
     &,pr_var0_num)
      num_lines=0
      do i=1,pr_var0_num
      if(pr_var0_list(i) (1:10).EQ.'wavelength')then
      num_lines=num_lines+1
      lines(num_lines)=i
      line=pr_var0_list(i) (11:)
      i_e_rate=string_lookup('emission_rate'//line,pr_var0_list,pr_var0_
     &num)
      if(i_e_rate.GT.0)continue
      e_rates(num_lines)=i_e_rate
      end if
      end do
      if(num_lines.GT.0)then
      i_gen_rate=string_lookup('emission_rate',pr_var0_list,pr_var0_num)
      end if
      init=.FALSE.
      end if
      sptest=species_stack_x(pointer_x)
      bk_rc_is=0
      do i=1,pr_bkrc_num
      if(problem_bkrc_products(1,i).EQ.sptest)then
      if(bk_rc_is.EQ.0)continue
      if(problem_species_background(source_root_sp_x).EQ.problem_bkrc_re
     &agents(2,i))continue
      bk_rc_is=i
      end if
      end do
      if(bk_rc_is.NE.0)continue
      back_1=problem_bkrc_reagents(1,bk_rc_is)
      back_2=problem_bkrc_reagents(2,bk_rc_is)
      pr_reac=problem_background_reaction(bk_rc_is)
      reac=problem_rc(pr_reac)
      rate=find_rate(species_stack_x(pointer_x),test_stack_x(pointer_x),
     &time_stack_x(pointer_x),weight_stack_x(pointer_x),pos_stack_x(1,po
     &inter_x),cell_stack_x(pointer_x),zone_stack_x(pointer_x),surface_s
     &tack_x(pointer_x),cell_next_stack_x(pointer_x),zone_next_stack_x(p
     &ointer_x),sector_stack_x(pointer_x),sector_next_stack_x(pointer_x)
     &,velocity_stack_x(1,pointer_x),type_stack_x(pointer_x),author_stac
     &k_x(pointer_x),back_1,problem_background_reaction(bk_rc_is),ran_in
     &dex_x,ran_array_x(0))
      if(rate.GT.(0.0_DOUBLE))continue
      call set_indep_params(species_stack_x(pointer_x),test_stack_x(poin
     &ter_x),time_stack_x(pointer_x),weight_stack_x(pointer_x),pos_stack
     &_x(1,pointer_x),cell_stack_x(pointer_x),zone_stack_x(pointer_x),su
     &rface_stack_x(pointer_x),cell_next_stack_x(pointer_x),zone_next_st
     &ack_x(pointer_x),sector_stack_x(pointer_x),sector_next_stack_x(poi
     &nter_x),velocity_stack_x(1,pointer_x),type_stack_x(pointer_x),auth
     &or_stack_x(pointer_x),back_1,reaction_rate_num_rand(pr_reac),ran_i
     &ndex_x,ran_array_x(0),independent_parameters)
      
      xseg=source_xseg_x
      if(reaction_emitter(reac).NE.0)then
      if(reaction_emitter(reac).EQ.1)continue
      independent_parameters(16)=species_m(sptest)
      if(est.EQ.3)then
      if(xseg.EQ.int(independent_parameters(6)))continue
      pos_zone_loc(1)=zone_center(1,xseg)
      pos_zone_loc(2)=zone_center(2,xseg)
      pos_zone_loc(3)=zone_center(3,xseg)
      
      cell_zone_loc=locate_point(pos_zone_loc,zone_zone_loc)
      surface_zone_loc=0
      if((geometry_symmetry.EQ.2.OR.geometry_symmetry.EQ.5.OR.geometry_s
     &ymmetry.EQ.6).AND.(background_coords.EQ.2).AND.(pos_zone_loc(1)**2
     &+pos_zone_loc(2)**2.GT.(0.0_DOUBLE)))then
      vb_flow(1)=(background_v(1,back_2,zone_pointer(zone_zone_loc))*pos
     &_zone_loc(1)-background_v(2,back_2,zone_pointer(zone_zone_loc))*po
     &s_zone_loc(2))/sqrt(pos_zone_loc(1)**2+pos_zone_loc(2)**2)
      vb_flow(2)=(background_v(1,back_2,zone_pointer(zone_zone_loc))*pos
     &_zone_loc(2)+background_v(2,back_2,zone_pointer(zone_zone_loc))*po
     &s_zone_loc(1))/sqrt(pos_zone_loc(1)**2+pos_zone_loc(2)**2)
      vb_flow(3)=background_v(3,back_2,zone_pointer(zone_zone_loc))
      else
      if(.NOT.((geometry_symmetry.EQ.0).AND.(background_coords.EQ.2)))co
     &ntinue
      vb_flow(1)=background_v(1,back_2,zone_pointer(zone_zone_loc))
      vb_flow(2)=background_v(2,back_2,zone_pointer(zone_zone_loc))
      vb_flow(3)=background_v(3,back_2,zone_pointer(zone_zone_loc))
      
      end if
      scoring_data(11)=vb_flow(1)
      scoring_data(12)=vb_flow(2)
      scoring_data(13)=vb_flow(3)
      scoring_data(14)=sqrt(background_temp(back_2,zone_pointer(zone_zon
     &e_loc))/species_m(problem_background_sp(back_2)))
      v_src(1)=vb_flow(1)
      v_src(2)=vb_flow(2)
      v_src(3)=vb_flow(3)
      
      e_loss=(1.5_DOUBLE)*background_temp(back_2,zone_pointer(zone_zone_
     &loc))+(0.5_DOUBLE)*species_m(problem_background_sp(back_2))*(vb_fl
     &ow(1)**2+vb_flow(2)**2+vb_flow(3)**2)
      else if(est.EQ.2)then
      do i=1,3
      v_src(i)=independent_parameters(8+i-1)
      end do
      scoring_data(8)=v_src(1)
      scoring_data(9)=v_src(2)
      scoring_data(10)=v_src(3)
      e_loss=(0.5_DOUBLE)*species_m(problem_background_sp(back_2))*(v_sr
     &c(1)**2+v_src(2)**2+v_src(3)**2)
      end if
      end if
      e_e_loss=(0.0_DOUBLE)
      e_e_src=(0.0_DOUBLE)
      do i=1,19
      
      if(reaction_handling_var0(i,pr_reac).EQ.i_e_loss)then
      e_e_loss=independent_parameters(1)*eval_data(reaction_handling_eva
     &l_name(i,pr_reac),reaction_handling_rank(i,pr_reac),reaction_handl
     &ing_var(1,i,pr_reac),reaction_handling_tab_index(1,i,pr_reac),reac
     &tion_handling_spacing(0,i,pr_reac),reaction_handling_min(1,i,pr_re
     &ac),reaction_handling_delta(1,i,pr_reac),reaction_handling_tab(rea
     &ction_handling_base(i,pr_reac)),independent_parameters,ran_index_x
     &,ran_array_x(0))
      else if(reaction_handling_var0(i,pr_reac).EQ.i_e_src)then
      e_e_src=independent_parameters(1)*eval_data(reaction_handling_eval
     &_name(i,pr_reac),reaction_handling_rank(i,pr_reac),reaction_handli
     &ng_var(1,i,pr_reac),reaction_handling_tab_index(1,i,pr_reac),react
     &ion_handling_spacing(0,i,pr_reac),reaction_handling_min(1,i,pr_rea
     &c),reaction_handling_delta(1,i,pr_reac),reaction_handling_tab(reac
     &tion_handling_base(i,pr_reac)),independent_parameters,ran_index_x,
     &ran_array_x(0))
      
      else if(reaction_handling_var0(i,pr_reac).NE.1)then
      scoring_data(reaction_handling_var0(i,pr_reac))=eval_data(reaction
     &_handling_eval_name(i,pr_reac),reaction_handling_rank(i,pr_reac),r
     &eaction_handling_var(1,i,pr_reac),reaction_handling_tab_index(1,i,
     &pr_reac),reaction_handling_spacing(0,i,pr_reac),reaction_handling_
     &min(1,i,pr_reac),reaction_handling_delta(1,i,pr_reac),reaction_han
     &dling_tab(reaction_handling_base(i,pr_reac)),independent_parameter
     &s,ran_index_x,ran_array_x(0))
      end if
      end do
      
      if(num_lines.GT.0)then
      first_line=.TRUE.
      do i=1,num_lines
      if((i_gen_rate.GT.0).AND.first_line)then
      if((scoring_data(lines(i)).GT.(0.0_DOUBLE)).AND.(scoring_data(i_ge
     &n_rate).GT.(0.0_DOUBLE)))then
      if(scoring_data(e_rates(i)).EQ.(0.0_DOUBLE))continue
      if(reaction_emitter(reac).NE.0)continue
      scoring_data(e_rates(i))=scoring_data(i_gen_rate)*independent_para
     &meters(1)/rate
      first_line=.FALSE.
      end if
      else
      if(scoring_data(lines(i)).GT.(0.0_DOUBLE))then
      scoring_data(e_rates(i))=scoring_data(e_rates(i))*(independent_par
     &ameters(1)/rate)
      end if
      end if
      end do
      end if
      
      psp_1=back_1
      psp_2=back_2
      scoring_data(20+15*psp_1)=scoring_data(20+15*psp_1)+(-species_m(pr
     &oblem_background_sp(back_1)))
      scoring_data(20+15*psp_2)=scoring_data(20+15*psp_2)+(-species_m(pr
     &oblem_background_sp(back_2)))
      scoring_data(21+15*psp_2)=scoring_data(21+15*psp_2)+(-species_m(pr
     &oblem_background_sp(back_2))*v_src(1))
      scoring_data(22+15*psp_2)=scoring_data(22+15*psp_2)+(-species_m(pr
     &oblem_background_sp(back_2))*v_src(2))
      scoring_data(23+15*psp_2)=scoring_data(23+15*psp_2)+(-species_m(pr
     &oblem_background_sp(back_2))*v_src(3))
      scoring_data(24+15*psp_1)=scoring_data(24+15*psp_1)+((e_e_src-e_e_
     &loss)/rate)
      scoring_data(24+15*psp_2)=scoring_data(24+15*psp_2)+(-e_loss)
      psp_2=(pr_background_num+test_stack_x(pointer_x))
      scoring_data(20+15*psp_2)=scoring_data(20+15*psp_2)+(species_m(spt
     &est))
      scoring_data(21+15*psp_2)=scoring_data(21+15*psp_2)+(species_m(spt
     &est)*v_src(1))
      scoring_data(22+15*psp_2)=scoring_data(22+15*psp_2)+(species_m(spt
     &est)*v_src(2))
      scoring_data(23+15*psp_2)=scoring_data(23+15*psp_2)+(species_m(spt
     &est)*v_src(3))
      scoring_data(24+15*psp_2)=scoring_data(24+15*psp_2)+(e_loss)
      return
      end
      
      
