      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      subroutine init_psi_interp(g_file)
      
      
      
      
      
      
      
      
      
      
      use bspline
      
      use ef_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character*96 g_file
      integer open_stat,g_fit,diskin4
      character*40 g_title
      character*8 g_date
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
      
      
      external get_env,wall_time,cpu_time,get_pid,arg_count,set_cwd
      logical get_env,set_cwd
      integer get_pid,arg_count
      REAL(kind=DOUBLE)cpu_time,wall_time
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      REAL(kind=DOUBLE),dimension(:),pointer::fg
      REAL(kind=DOUBLE),dimension(:),pointer::pg
      REAL(kind=DOUBLE),dimension(:),pointer::ffg
      REAL(kind=DOUBLE),dimension(:),pointer::ppg
      REAL(kind=DOUBLE),dimension(:),pointer::rr
      REAL(kind=DOUBLE),dimension(:),pointer::zz
      REAL(kind=DOUBLE),dimension(:),pointer::psis
      REAL(kind=DOUBLE),dimension(:,:),pointer::pfm
      diskin4=20+4
      open(unit=diskin4,file=g_file,status='old',form='formatted',iostat
     &=open_stat)
      if(open_stat.EQ.0)continue
      read(diskin4,'(a40,a8,3(i4))')g_title,g_date,g_fit,ef_nr,ef_nz
      fg =>mem_alloc_r1((1),(ef_nr),'fg')
      pg =>mem_alloc_r1((1),(ef_nr),'pg')
      ffg =>mem_alloc_r1((1),(ef_nr),'ffg')
      ppg =>mem_alloc_r1((1),(ef_nr),'ppg')
      rr =>mem_alloc_r1((1),(ef_nr),'rr')
      zz =>mem_alloc_r1((1),(ef_nz),'zz')
      psis =>mem_alloc_r1((1),(ef_nr),'psis')
      pfm =>mem_alloc_r2((1),(ef_nr),(1),(ef_nz),'pfm')
      call read_efit(diskin4,ef_nr,ef_nz,rr,zz,psis,fg,pg,ffg,ppg,pfm,ef
     &_psilim,ef_rma,ef_zma)
      ef_psi_knot =>mem_alloc_r1((1),(ef_nr+3),'ef_psi_knot')
      ef_r_knot =>mem_alloc_r1((1),(ef_nr+5),'ef_r_knot')
      ef_z_knot =>mem_alloc_r1((1),(ef_nz+5),'ef_z_knot')
      ef_psi_bscoef =>mem_alloc_r1((1),(ef_nr*ef_nz),'ef_psi_bscoef')
      ef_i_bscoef =>mem_alloc_r1((1),(ef_nr),'ef_i_bscoef')
      call dbsnak(ef_nr,psis,3,ef_psi_knot)
      ef_min_r=rr(1)
      ef_max_r=rr(ef_nr)
      ef_min_z=zz(1)
      ef_max_z=zz(ef_nz)
      call dbsnak(ef_nr,rr,5,ef_r_knot)
      call dbsnak(ef_nz,zz,5,ef_z_knot)
      CALL dbs2IN(ef_nr,rr,ef_nz,zz,pfm,ef_nr,5,5,ef_r_knot,ef_z_knot,ef
     &_psi_bscoef)
      call dbsint(ef_nr,psis,fg,3,ef_psi_knot,ef_i_bscoef)
      call test_interp(ef_nr,ef_nz,pfm,psis,fg)
      return
      end
      subroutine test_interp(nr,nz,pfm,psis,fg)
      
      use ef_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer nr,nz
      REAL(kind=DOUBLE)pfm(nr,nz),psis(nr),fg(nr)
      integer ir,iz
      REAL(kind=DOUBLE)maxerr,toterr,ztest,rtest,psi_spline,psi_value,er
     &ror,avgerr,psi_test,i_spline
      external psi_interp,i_interp
      REAL(kind=DOUBLE)psi_interp,i_interp
      maxerr=(0.0_DOUBLE)
      toterr=(0.0_DOUBLE)
      do iz=1,ef_nz
      ztest=ef_min_z+((ef_max_z-ef_min_z)/REAL(ef_nz-1,DOUBLE))*REAL(iz-
     &1,DOUBLE)
      do ir=1,ef_nr
      rtest=ef_min_r+((ef_max_r-ef_min_r)/REAL(ef_nr-1,DOUBLE))*REAL(ir-
     &1,DOUBLE)
      psi_value=pfm(ir,iz)
      psi_spline=psi_interp(rtest,ztest,0,0)
      error=abs(psi_spline-psi_value)/max(abs(psi_value),(1.e-10_DOUBLE)
     &)
      if(error.GT.maxerr)maxerr=error
      toterr=toterr+(error)
      end do
      end do
      avgerr=toterr/REAL(ef_nr*ef_nz,DOUBLE)
      maxerr=(0.0_DOUBLE)
      toterr=(0.0_DOUBLE)
      do ir=1,ef_nr
      psi_test=psis(ir)
      i_spline=i_interp(psi_test,0)
      error=abs(i_spline-fg(ir))/max(abs(fg(ir)),(1.e-10_DOUBLE))
      if(error.GT.maxerr)maxerr=error
      toterr=toterr+(error)
      enddo
      avgerr=toterr/REAL(ef_nr,DOUBLE)
      return
      end
      subroutine read_efit(nunit,nr,nz,rr,zz,psis,fg,pg,ffg,ppg,pfm,psil
     &im,rma,zma)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer nunit,nr,nz
      REAL(kind=DOUBLE)psilim,rma,zma
      REAL(kind=DOUBLE)rr(nr),zz(nz),psis(nr),fg(nr),pg(nr),ffg(nr),ppg(
     &nr),pfm(nr,nz)
      integer i,j,ir,iz
      REAL(kind=DOUBLE)rdim,zdim,rcntc,redge,zmsmid,psimin,btorc,dummy
      read(nunit,'(5e16.9)')rdim,zdim,rcntc,redge,zmsmid
      read(nunit,'(5e16.9)')rma,zma,psimin,psilim,btorc
      read(nunit,'(5e16.9)')dummy,dummy,dummy,dummy,dummy
      read(nunit,'(5e16.9)')dummy,dummy,dummy,dummy,dummy
      read(nunit,'(5e16.9)') (fg(i),i=1,nr)
      read(nunit,'(5e16.9)') (pg(i),i=1,nr)
      read(nunit,'(5e16.9)') (ffg(i),i=1,nr)
      read(nunit,'(5e16.9)') (ppg(i),i=1,nr)
      read(nunit,'(5e16.9)') ((pfm(i,j),i=1,nr),j=1,nz)
      close(unit=nunit)
      do ir=1,nr
      rr(ir)=(REAL(ir-1,DOUBLE)/(REAL(nr-1,DOUBLE)))*rdim+redge
      psis(ir)=(REAL(ir-1,DOUBLE)/(REAL(nr-1,DOUBLE)))*(psilim-psimin)
      do iz=1,nz
      pfm(ir,iz)=pfm(ir,iz)-psimin
      end do
      end do
      do iz=1,nz
      zz(iz)=((REAL(iz-1,DOUBLE)-(REAL(nz-1,DOUBLE)/(2.0_DOUBLE)))/(REAL
     &(nz-1,DOUBLE)))*zdim+zmsmid
      end do
      psilim=psilim-psimin
      return
      end
      function psi_interp(r_in,z_in,r_der,z_der)
      use bspline
      
      use ef_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)psi_interp
      integer r_der,z_der
      REAL(kind=DOUBLE)r_in,z_in
      if((r_in.GE.ef_min_r).AND.(r_in.LE.ef_max_r))continue
      if((z_in.GE.ef_min_z).AND.(z_in.LE.ef_max_z))continue
      psi_interp=dbs2dr(r_der,z_der,r_in,z_in,5,5,ef_r_knot,ef_z_knot,ef
     &_nr,ef_nz,ef_psi_bscoef)
      return
      end
      function i_interp(psi_in,i_der)
      use bspline
      
      use ef_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)i_interp
      integer i_der
      REAL(kind=DOUBLE)psi_in
      REAL(kind=DOUBLE)psi
      if(psi_in.LE.ef_psilim)then
      psi=psi_in
      else
      psi=ef_psilim
      end if
      i_interp=dbsder(i_der,psi,3,ef_psi_knot,ef_nr,ef_i_bscoef)
      return
      end
      subroutine bvec_interp(r,z,b)
      use bspline
      
      use ef_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)r,z
      REAL(kind=DOUBLE)b(3)
      REAL(kind=DOUBLE)psi,dpsi_dr,dpsi_dz,fi
      external psi_interp,i_interp
      REAL(kind=DOUBLE)psi_interp,i_interp
      psi=psi_interp(r,z,0,0)
      dpsi_dr=psi_interp(r,z,1,0)
      dpsi_dz=psi_interp(r,z,0,1)
      fi=i_interp(psi,0)
      b(1)=-dpsi_dz/r
      b(2)=fi/r
      b(3)=dpsi_dr/r
      return
      end
      subroutine follow_field(order,x,l_org,delta_phi,x_dest,l_dest)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer order
      REAL(kind=DOUBLE)x(3)
      REAL(kind=DOUBLE)l_org,delta_phi
      REAL(kind=DOUBLE)x_dest(3)
      REAL(kind=DOUBLE)l_dest
      REAL(kind=DOUBLE)phi0,r_org,z_org,br,bphi,bz,r_dest,z_dest,hh,r_mi
     &d,z_mid,h6,dr1,dz1,r_tmp,z_tmp,dr2,dz2,dr3,dz3,dr4,dz4,phi,r,z,bto
     &t,l_mid,dl1,l_tmp,dl2,dl3,dl4
      REAL(kind=DOUBLE)b(3)
      phi0=atan2(x(2),x(1))
      if(phi0.GT.atan2((0.0_DOUBLE),-(1.0_DOUBLE)))phi0=phi0-(2.0_DOUBLE
     &)*atan2((0.0_DOUBLE),-(1.0_DOUBLE))
      r_org=sqrt(x(1)**2+x(2)**2)
      z_org=x(3)
      call bvec_interp(r_org,z_org,b(1))
      br=b(1)
      bphi=b(2)
      bz=b(3)
      btot=sqrt(b(1)**2+b(2)**2+b(3)**2)
      if(order.EQ.1)then
      r_dest=r_org+(br/bphi)*(r_org*delta_phi)
      z_dest=z_org+(bz/bphi)*(r_org*delta_phi)
      l_dest=l_org+(btot/bphi)*(r_org*delta_phi)
      else if(order.EQ.2)then
      hh=(0.5_DOUBLE)*delta_phi
      r_mid=r_org+(br/bphi)*(r_org*hh)
      z_mid=z_org+(bz/bphi)*(r_org*hh)
      l_mid=l_org+(btot/bphi)*(r_org*hh)
      call bvec_interp(r_mid,z_mid,b(1))
      br=b(1)
      bphi=b(2)
      bz=b(3)
      btot=sqrt(b(1)**2+b(2)**2+b(3)**2)
      r_dest=r_org+(br/bphi)*(r_mid*delta_phi)
      z_dest=z_org+(bz/bphi)*(r_mid*delta_phi)
      l_dest=l_org+(btot/bphi)*(r_mid*delta_phi)
      else
      hh=(0.5_DOUBLE)*delta_phi
      h6=delta_phi/(6._DOUBLE)
      dr1=(br/bphi)*r_org
      r_tmp=r_org+hh*dr1
      dz1=(bz/bphi)*r_org
      z_tmp=z_org+hh*dz1
      dl1=(btot/bphi)*r_org
      l_tmp=l_org+hh*dl1
      call bvec_interp(r_tmp,z_tmp,b(1))
      br=b(1)
      bphi=b(2)
      bz=b(3)
      btot=sqrt(b(1)**2+b(2)**2+b(3)**2)
      dr2=(br/bphi)*r_tmp
      r_tmp=r_org+hh*dr2
      dz2=(bz/bphi)*r_tmp
      z_tmp=z_org+hh*dz2
      dl2=(btot/bphi)*r_tmp
      l_tmp=l_org+hh*dl2
      call bvec_interp(r_tmp,z_tmp,b(1))
      br=b(1)
      bphi=b(2)
      bz=b(3)
      btot=sqrt(b(1)**2+b(2)**2+b(3)**2)
      dr3=(br/bphi)*r_tmp
      r_tmp=r_org+delta_phi*dr3
      dz3=(bz/bphi)*r_tmp
      z_tmp=z_org+delta_phi*dz3
      dl3=(btot/bphi)*r_tmp
      l_tmp=l_org+delta_phi*dl3
      call bvec_interp(r_tmp,z_tmp,b(1))
      br=b(1)
      bphi=b(2)
      bz=b(3)
      btot=sqrt(b(1)**2+b(2)**2+b(3)**2)
      dr4=(br/bphi)*r_tmp
      r_dest=r_org+h6*(dr1+(2.0_DOUBLE)*dr2+(2.0_DOUBLE)*dr3+dr4)
      dz4=(bz/bphi)*r_tmp
      z_dest=z_org+h6*(dz1+(2.0_DOUBLE)*dz2+(2.0_DOUBLE)*dz3+dz4)
      dl4=(btot/bphi)*r_tmp
      l_dest=l_org+h6*(dl1+(2.0_DOUBLE)*dl2+(2.0_DOUBLE)*dl3+dl4)
      end if
      phi=phi0+delta_phi
      x_dest(1)=r_dest*cos(phi)
      x_dest(2)=r_dest*sin(phi)
      x_dest(3)=z_dest
      return
      end
      
      
