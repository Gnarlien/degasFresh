      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      function surface_eval(coeff,sigma,x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)surface_eval
      REAL(kind=DOUBLE)coeff(10)
      integer sigma
      REAL(kind=DOUBLE)x(3)
      
      surface_eval=sigma*(x(3)*(coeff(7)*x(3)+coeff(9)*x(2)+coeff(10)*x(
     &1)+coeff(4))+x(2)*(coeff(6)*x(2)+coeff(8)*x(1)+coeff(3))+x(1)*(coe
     &ff(5)*x(1)+coeff(2))+coeff(1))
      return
      end
      subroutine surface_gradient(coeff,sigma,x,grad)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)coeff(10)
      integer sigma
      REAL(kind=DOUBLE)x(3)
      REAL(kind=DOUBLE)grad(3)
      grad(1)=sigma*(coeff(10)*x(3)+coeff(8)*x(2)+2*coeff(5)*x(1)+coeff(
     &2))
      grad(2)=sigma*(coeff(9)*x(3)+2*coeff(6)*x(2)+coeff(8)*x(1)+coeff(3
     &))
      grad(3)=sigma*(2*coeff(7)*x(3)+coeff(9)*x(2)+coeff(10)*x(1)+coeff(
     &4))
      return
      end
      function in_surface(coeff,sigma,x,surface_val,f)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical in_surface
      REAL(kind=DOUBLE)coeff(10)
      integer sigma
      REAL(kind=DOUBLE)x(3),f
      logical surface_val
      REAL(kind=DOUBLE)grad(3),f0
      external surface_eval,surface_gradient
      REAL(kind=DOUBLE)surface_eval
      call surface_gradient(coeff,sigma,x,grad)
      if(surface_val)then
      f0=f
      else
      f0=surface_eval(coeff,sigma,x)
      end if
      in_surface=abs(f0).LE.(1.0e-8_DOUBLE)*sqrt((grad(1)**2+grad(2)**2+
     &grad(3)**2))
      return
      end
      function in_surface_dump(coeff,sigma,x,surface_val,f)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical in_surface_dump
      REAL(kind=DOUBLE)coeff(10)
      integer sigma
      REAL(kind=DOUBLE)x(3),f
      logical surface_val
      REAL(kind=DOUBLE)grad(3),f0
      external surface_eval,surface_gradient
      REAL(kind=DOUBLE)surface_eval
      call surface_gradient(coeff,sigma,x,grad)
      if(surface_val)then
      f0=f
      else
      f0=surface_eval(coeff,sigma,x)
      end if
      write(0,*)' In in_surface_dump'
      write(0,*)' f0 = ',f
      write(0,*)' grad = ',grad
      write(0,*)' vc_abs(grad) = ',sqrt((grad(1)**2+grad(2)**2+grad(3)**
     &2))
      write(0,*)' surface_args = ',coeff,sigma
      in_surface_dump=abs(f0).LE.(1.0e-8_DOUBLE)*sqrt((grad(1)**2+grad(2
     &)**2+grad(3)**2))
      return
      end
      function inside_surface(coeff,sigma,x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical inside_surface
      REAL(kind=DOUBLE)coeff(10)
      integer sigma
      REAL(kind=DOUBLE)x(3)
      REAL(kind=DOUBLE)f
      external in_surface,surface_eval
      REAL(kind=DOUBLE)surface_eval
      logical in_surface
      inside_surface=.TRUE.
      f=surface_eval(coeff,sigma,x)
      if(f.GE.(0.0_DOUBLE))return
      inside_surface=in_surface(coeff,sigma,x,.TRUE.,f)
      return
      end
      function inside_cell(nface,nsurface,nsurfaces,surf_list,surface_co
     &effs,x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical inside_cell
      integer nface,nsurface,nsurfaces,surf_list(nsurface)
      REAL(kind=DOUBLE)surface_coeffs(10,nsurfaces)
      REAL(kind=DOUBLE)x(3)
      integer n
      external surface_eval
      REAL(kind=DOUBLE)surface_eval
      inside_cell=.FALSE.
      do n=1,nsurface
      if(surface_eval(surface_coeffs(1,abs(surf_list(n))),sign(1,surf_li
     &st(n)),x).LT.(0.0_DOUBLE))return
      end do
      inside_cell=.TRUE.
      return
      end
      function sloppy_inside(nface,nsurface,nsurfaces,surf_list,surface_
     &coeffs,x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical sloppy_inside
      integer nface,nsurface,nsurfaces,surf_list(nsurface)
      REAL(kind=DOUBLE)surface_coeffs(10,nsurfaces)
      REAL(kind=DOUBLE)x(3)
      REAL(kind=DOUBLE)f0
      integer n
      external surface_eval,in_surface
      REAL(kind=DOUBLE)surface_eval
      logical in_surface
      sloppy_inside=.FALSE.
      do n=1,nface
      f0=surface_eval(surface_coeffs(1,abs(surf_list(n))),sign(1,surf_li
     &st(n)),x)
      if(f0.LT.(0.0_DOUBLE))then
      if(.NOT.in_surface(surface_coeffs(1,abs(surf_list(n))),sign(1,surf
     &_list(n)),x,.TRUE.,f0))return
      end if
      end do
      do n=nface+1,nsurface
      if(surface_eval(surface_coeffs(1,abs(surf_list(n))),sign(1,surf_li
     &st(n)),x).LT.(0.0_DOUBLE))return
      end do
      sloppy_inside=.TRUE.
      return
      end
      function face_in_cell(nface,nsurface,nsurfaces,surf_list,surface_c
     &oeffs,face)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical face_in_cell
      integer nface,nsurface,nsurfaces,surf_list(nsurface)
      REAL(kind=DOUBLE)surface_coeffs(10,nsurfaces)
      integer face
      integer n
      face_in_cell=.TRUE.
      do n=1,nface
      if(surf_list(n).EQ.face)return
      end do
      face_in_cell=.FALSE.
      return
      end
      function inside_cell_a(nface,nsurface,nsurfaces,surf_list,surface_
     &coeffs,face,x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical inside_cell_a
      integer nface,nsurface,nsurfaces,surf_list(nsurface)
      REAL(kind=DOUBLE)surface_coeffs(10,nsurfaces)
      integer face
      REAL(kind=DOUBLE)x(3)
      integer n
      external surface_eval
      REAL(kind=DOUBLE)surface_eval
      inside_cell_a=.FALSE.
      do n=1,nsurface
      if(surf_list(n).NE.face)then
      if(surface_eval(surface_coeffs(1,abs(surf_list(n))),sign(1,surf_li
     &st(n)),x).LT.(0.0_DOUBLE))return
      end if
      end do
      inside_cell_a=.TRUE.
      return
      end
      function sloppy_inside_a(nface,nsurface,nsurfaces,surf_list,surfac
     &e_coeffs,face,x,dump)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical sloppy_inside_a
      integer nface,nsurface,nsurfaces,surf_list(nsurface)
      REAL(kind=DOUBLE)surface_coeffs(10,nsurfaces)
      integer face
      REAL(kind=DOUBLE)x(3)
      logical dump
      integer n
      REAL(kind=DOUBLE)f0
      external surface_eval,in_surface,in_surface_dump
      REAL(kind=DOUBLE)surface_eval
      logical in_surface,in_surface_dump
      sloppy_inside_a=.FALSE.
      do n=1,nface
      if(dump)then
      write(0,*)' In sloppy_inside_a'
      write(0,*)' n = ',n
      write(0,*)' surf_list[n] = ',surf_list(n)
      end if
      if(surf_list(n).NE.face)then
      f0=surface_eval(surface_coeffs(1,abs(surf_list(n))),sign(1,surf_li
     &st(n)),x)
      if(dump)then
      write(0,*)' f0 = ',f0
      end if
      if(f0.LT.(0.0_DOUBLE))then
      if(.NOT.dump)then
      if(.NOT.in_surface(surface_coeffs(1,abs(surf_list(n))),sign(1,surf
     &_list(n)),x,.TRUE.,f0))return
      else
      if(.NOT.in_surface_dump(surface_coeffs(1,abs(surf_list(n))),sign(1
     &,surf_list(n)),x,.TRUE.,f0))then
      write(0,*)' in_surface came back false for n = ',n
      return
      end if
      end if
      end if
      end if
      end do
      do n=nface+1,nsurface
      if(surface_eval(surface_coeffs(1,abs(surf_list(n))),sign(1,surf_li
     &st(n)),x).LT.(0.0_DOUBLE))return
      end do
      sloppy_inside_a=.TRUE.
      return
      end
      function on_face(nface,nsurface,nsurfaces,surf_list,surface_coeffs
     &,face,x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical on_face
      integer nface,nsurface,nsurfaces,surf_list(nsurface)
      REAL(kind=DOUBLE)surface_coeffs(10,nsurfaces)
      integer face
      REAL(kind=DOUBLE)x(3)
      external in_surface,inside_cell_a,face_in_cell
      logical in_surface,inside_cell_a,face_in_cell
      if(face_in_cell(nface,nsurface,nsurfaces,surf_list,surface_coeffs,
     &face))continue
      on_face=in_surface(surface_coeffs(1,abs(face)),sign(1,face),x,.FAL
     &SE.,(0.0_DOUBLE)).AND.inside_cell_a(nface,nsurface,nsurfaces,surf_
     &list,surface_coeffs,face,x)
      return
      end
      function sloppy_on(nface,nsurface,nsurfaces,surf_list,surface_coef
     &fs,face,x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical sloppy_on
      integer nface,nsurface,nsurfaces,surf_list(nsurface)
      REAL(kind=DOUBLE)surface_coeffs(10,nsurfaces)
      integer face
      REAL(kind=DOUBLE)x(3)
      external in_surface,sloppy_inside_a,face_in_cell
      logical in_surface,sloppy_inside_a,face_in_cell
      if(face_in_cell(nface,nsurface,nsurfaces,surf_list,surface_coeffs,
     &face))continue
      sloppy_on=in_surface(surface_coeffs(1,abs(face)),sign(1,face),x,.F
     &ALSE.,(0.0_DOUBLE)).AND.sloppy_inside_a(nface,nsurface,nsurfaces,s
     &urf_list,surface_coeffs,face,x,.FALSE.)
      return
      end
      function cell_compare(nface,nsurface,nsurfaces,surf_list,surface_c
     &oeffs,x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer cell_compare
      integer nface,nsurface,nsurfaces,surf_list(nsurface)
      REAL(kind=DOUBLE)surface_coeffs(10,nsurfaces)
      REAL(kind=DOUBLE)x(3)
      integer n
      REAL(kind=DOUBLE)f0
      logical on
      external surface_eval,in_surface
      REAL(kind=DOUBLE)surface_eval
      logical in_surface
      cell_compare=-1
      do n=nface+1,nsurface
      if(surface_eval(surface_coeffs(1,abs(surf_list(n))),sign(1,surf_li
     &st(n)),x).LT.(0.0_DOUBLE))return
      end do
      on=.FALSE.
      do n=1,nface
      f0=surface_eval(surface_coeffs(1,abs(surf_list(n))),sign(1,surf_li
     &st(n)),x)
      if(in_surface(surface_coeffs(1,abs(surf_list(n))),sign(1,surf_list
     &(n)),x,.TRUE.,f0))f0=(0.0_DOUBLE)
      if(f0.LT.(0.0_DOUBLE))return
      if(f0.EQ.(0.0_DOUBLE))on=.TRUE.
      end do
      if(on)then
      cell_compare=0
      else
      cell_compare=1
      end if
      return
      end
      function surface_intersect(coeff,sigma,x0,v)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)surface_intersect
      REAL(kind=DOUBLE)coeff(10)
      integer sigma
      REAL(kind=DOUBLE)x0(3),v(3)
      REAL(kind=DOUBLE)a,h,c,temp(3),disc
      external surface_eval
      REAL(kind=DOUBLE)surface_eval
      a=sigma*(v(3)*(coeff(7)*v(3)+coeff(9)*v(2)+coeff(10)*v(1))+v(2)*(c
     &oeff(6)*v(2)+coeff(8)*v(1))+coeff(5)*v(1)**2)
      call surface_gradient(coeff,sigma,x0,temp)
      h=(0.5_DOUBLE)*(temp(1)*v(1)+temp(2)*v(2)+temp(3)*v(3))
      c=surface_eval(coeff,sigma,x0)
      surface_intersect=(1.0e16_DOUBLE)
      if(a.NE.(0.0_DOUBLE))then
      disc=h**2-a*c
      if(disc.LE.(0.0_DOUBLE))then
      if(a.LT.(0.0_DOUBLE))then
      surface_intersect=-h/a
      end if
      else
      disc=sqrt(disc)
      if(h.LT.(0.0_DOUBLE))then
      surface_intersect=-c/(h-disc)
      else
      if(a.LT.(0.0_DOUBLE))surface_intersect=-(h+disc)/a
      end if
      end if
      else if(h.NE.(0.0_DOUBLE))then
      if(h.LT.(0.0_DOUBLE))surface_intersect=-c/((2.0_DOUBLE)*h)
      end if
      return
      end
      function surface_intersection_direction(coeff,sigma,x,v)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)surface_intersection_direction
      REAL(kind=DOUBLE)coeff(10)
      integer sigma
      REAL(kind=DOUBLE)x(3),v(3)
      REAL(kind=DOUBLE)grad(3)
      external surface_gradient
      call surface_gradient(coeff,sigma,x,grad)
      surface_intersection_direction=-(grad(1)*v(1)+grad(2)*v(2)+grad(3)
     &*v(3))/(sqrt((v(1)**2+v(2)**2+v(3)**2))*sqrt((grad(1)**2+grad(2)**
     &2+grad(3)**2)))
      if(surface_intersection_direction.LT.-(1.0_DOUBLE))surface_interse
     &ction_direction=-(1.0_DOUBLE)
      if(surface_intersection_direction.GT.(1.0_DOUBLE))surface_intersec
     &tion_direction=(1.0_DOUBLE)
      if(abs(surface_intersection_direction).LE.(1.0_DOUBLE))continue
      return
      end
      function intersection_direction(face,x,v)
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)intersection_direction
      integer face
      REAL(kind=DOUBLE)x(3),v(3)
      external surface_intersection_direction
      REAL(kind=DOUBLE)surface_intersection_direction
      intersection_direction=surface_intersection_direction(surface_coef
     &fs(1,abs(face)),sign(1,face),x,v)
      return
      end
      subroutine surface_specular(face,x,v,v1)
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer face
      REAL(kind=DOUBLE)x(3),v(3)
      REAL(kind=DOUBLE)v1(3)
      REAL(kind=DOUBLE)grad(3),vperp
      REAL(kind=DOUBLE)vector_temp(3)
      external surface_gradient
      call surface_gradient(surface_coeffs(1,abs(face)),sign(1,face),x,g
     &rad)
      vector_temp(1)=sqrt((grad(1)**2+grad(2)**2+grad(3)**2))
      if(vector_temp(1).GT.(0.0_DOUBLE))continue
      grad(1)=((1.0_DOUBLE)/vector_temp(1))*grad(1)
      grad(2)=((1.0_DOUBLE)/vector_temp(1))*grad(2)
      grad(3)=((1.0_DOUBLE)/vector_temp(1))*grad(3)
      
      vperp=(grad(1)*v(1)+grad(2)*v(2)+grad(3)*v(3))
      v1(1)=v(1)+grad(1)*((-(1.0_DOUBLE)+sign((1.0_DOUBLE),vperp))*vperp
     &)
      v1(2)=v(2)+grad(2)*((-(1.0_DOUBLE)+sign((1.0_DOUBLE),vperp))*vperp
     &)
      v1(3)=v(3)+grad(3)*((-(1.0_DOUBLE)+sign((1.0_DOUBLE),vperp))*vperp
     &)
      
      return
      end
      subroutine surface_reflect(face,x,v,w,v1)
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer face
      REAL(kind=DOUBLE)x(3),v(3),w(3)
      REAL(kind=DOUBLE)v1(3)
      REAL(kind=DOUBLE)e(3,3),e1(3),e2(3),e3(3),vunit(3)
      REAL(kind=DOUBLE)vperp
      equivalence(e1,e(1,1)),(e2,e(1,2)),(e3,e(1,3))
      REAL(kind=DOUBLE)vector_temp(3)
      integer i
      external surface_gradient
      call surface_gradient(surface_coeffs(1,abs(face)),sign(1,face),x,e
     &3)
      vector_temp(1)=sqrt((e3(1)**2+e3(2)**2+e3(3)**2))
      if(vector_temp(1).GT.(0.0_DOUBLE))continue
      e3(1)=((1.0_DOUBLE)/vector_temp(1))*e3(1)
      e3(2)=((1.0_DOUBLE)/vector_temp(1))*e3(2)
      e3(3)=((1.0_DOUBLE)/vector_temp(1))*e3(3)
      
      if(sqrt((v(1)**2+v(2)**2+v(3)**2)).GT.(0.0_DOUBLE))then
      vector_temp(1)=sqrt((v(1)**2+v(2)**2+v(3)**2))
      if(vector_temp(1).GT.(0.0_DOUBLE))continue
      vunit(1)=((1.0_DOUBLE)/vector_temp(1))*v(1)
      vunit(2)=((1.0_DOUBLE)/vector_temp(1))*v(2)
      vunit(3)=((1.0_DOUBLE)/vector_temp(1))*v(3)
      
      vperp=abs((1.0_DOUBLE)+(e3(1)*vunit(1)+e3(2)*vunit(2)+e3(3)*vunit(
     &3)))
      else
      vperp=(1.0_DOUBLE)
      end if
      e1(1)=v(1)+e3(1)*(-(e3(1)*v(1)+e3(2)*v(2)+e3(3)*v(3)))
      e1(2)=v(2)+e3(2)*(-(e3(1)*v(1)+e3(2)*v(2)+e3(3)*v(3)))
      e1(3)=v(3)+e3(3)*(-(e3(1)*v(1)+e3(2)*v(2)+e3(3)*v(3)))
      
      if((sqrt((e1(1)**2+e1(2)**2+e1(3)**2)).LE.sqrt((v(1)**2+v(2)**2+v(
     &3)**2))*(4._DOUBLE)*(5*EPSILON((0.0_DOUBLE)))).OR.(vperp.LE.(4._DO
     &UBLE)*(5*EPSILON((0.0_DOUBLE)))))then
      e2(1)=(1.0_DOUBLE)
      e2(2)=(0.0_DOUBLE)
      e2(3)=(0.0_DOUBLE)
      vector_temp(1)=e3(2)*e2(3)-e3(3)*e2(2)
      vector_temp(2)=e3(3)*e2(1)-e3(1)*e2(3)
      e1(3)=e3(1)*e2(2)-e3(2)*e2(1)
      e1(1)=vector_temp(1)
      e1(2)=vector_temp(2)
      if(sqrt((e1(1)**2+e1(2)**2+e1(3)**2)).LE.sqrt((v(1)**2+v(2)**2+v(3
     &)**2))*(3._DOUBLE)*(5*EPSILON((0.0_DOUBLE))))then
      e(2,2)=(1.0_DOUBLE)
      vector_temp(1)=e3(2)*e2(3)-e3(3)*e2(2)
      vector_temp(2)=e3(3)*e2(1)-e3(1)*e2(3)
      e1(3)=e3(1)*e2(2)-e3(2)*e2(1)
      e1(1)=vector_temp(1)
      e1(2)=vector_temp(2)
      end if
      if(sqrt((v(1)**2+v(2)**2+v(3)**2)).GT.(0.0_DOUBLE))then
      e1(1)=(sqrt((v(1)**2+v(2)**2+v(3)**2)))*e1(1)
      e1(2)=(sqrt((v(1)**2+v(2)**2+v(3)**2)))*e1(2)
      e1(3)=(sqrt((v(1)**2+v(2)**2+v(3)**2)))*e1(3)
      
      end if
      end if
      if(sqrt((v(1)**2+v(2)**2+v(3)**2)).GT.(0.0_DOUBLE))then
      if(sqrt((e1(1)**2+e1(2)**2+e1(3)**2))/sqrt((v(1)**2+v(2)**2+v(3)**
     &2)).GT.sqrt((5*EPSILON((0.0_DOUBLE)))))continue
      end if
      vector_temp(1)=sqrt((e1(1)**2+e1(2)**2+e1(3)**2))
      if(vector_temp(1).GT.(0.0_DOUBLE))continue
      e1(1)=((1.0_DOUBLE)/vector_temp(1))*e1(1)
      e1(2)=((1.0_DOUBLE)/vector_temp(1))*e1(2)
      e1(3)=((1.0_DOUBLE)/vector_temp(1))*e1(3)
      
      vector_temp(1)=e3(2)*e1(3)-e3(3)*e1(2)
      vector_temp(2)=e3(3)*e1(1)-e3(1)*e1(3)
      e2(3)=e3(1)*e1(2)-e3(2)*e1(1)
      e2(1)=vector_temp(1)
      e2(2)=vector_temp(2)
      do i=1,3
      v1(i)=w(1)*e(i,1)+w(2)*e(i,2)+w(3)*e(i,3)
      end do
      return
      end
      function cell_intersect(nface,nsurface,nsurfaces,surf_list,surface
     &_coeffs,x0,v,exit_face)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)cell_intersect
      integer nface,nsurface,nsurfaces,surf_list(nsurface)
      REAL(kind=DOUBLE)surface_coeffs(10,nsurfaces)
      REAL(kind=DOUBLE)x0(3),v(3)
      integer exit_face
      REAL(kind=DOUBLE)t
      integer n,ns
      external surface_intersect
      REAL(kind=DOUBLE)surface_intersect
      cell_intersect=(1.0e16_DOUBLE)
      do n=1,nface
      t=surface_intersect(surface_coeffs(1,abs(surf_list(n))),sign(1,sur
     &f_list(n)),x0,v)
      if(t.LT.cell_intersect)then
      cell_intersect=t
      ns=n
      end if
      end do
      cell_intersect=max((0.0_DOUBLE),cell_intersect)
      if(cell_intersect.LT.(1.0e16_DOUBLE))continue
      exit_face=surf_list(ns)
      return
      end
      function new_cell(exit_face,x)
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer new_cell
      integer exit_face
      REAL(kind=DOUBLE)x(3)
      integer n,s,i,j,face
      external inside_cell_a,sloppy_inside_a
      logical inside_cell_a,sloppy_inside_a
      face=-exit_face
      new_cell=0
      n=surfaces((sign(1,face)+1)/2,1,abs(face))
      s=surfaces((sign(1,face)+1)/2,0,abs(face))
      if(n.EQ.0)return
      do i=s,s+n-1
      j=neighbors(i)
      if(j.GT.0)then
      if(inside_cell_a(cells(2,j),cells(3,j),nsurfaces,boundaries(cells(
     &1,j)),surface_coeffs(1,1),face,x))then
      new_cell=j
      return
      end if
      end if
      end do
      do i=s,s+n-1
      j=neighbors(i)
      if(j.GT.0)then
      if(sloppy_inside_a(cells(2,j),cells(3,j),nsurfaces,boundaries(cell
     &s(1,j)),surface_coeffs(1,1),face,x,.FALSE.))then
      new_cell=j
      return
      end if
      end if
      end do
      if(.NOT.(new_cell.GT.0))then
      write(0,*)' new_cell <= 0 '
      write(0,*)' x = ',x
      write(0,*)' exit_face = ',exit_face
      write(0,*)' new_cell = ',new_cell
      write(0,*)' n = ',n
      write(0,*)' s = ',s
      do i=s,s+n-1
      j=neighbors(i)
      write(0,*)' For i = ',i,' j = ',j
      if(j.GT.0)then
      if(sloppy_inside_a(cells(2,j),cells(3,j),nsurfaces,boundaries(cell
     &s(1,j)),surface_coeffs(1,1),face,x,.TRUE.))then
      write(0,*)' WHOA! This did not work above!'
      end if
      end if
      end do
      if(new_cell.GT.0)continue
      end if
      return
      end
      function track(type,tmax,x0,v,cell,t,x,cell1,exit_face,cell2,secto
     &r1,sector2)
      
      use gi_mod
      
      use sc_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical track
      REAL(kind=DOUBLE)tmax,x0(3),v(3)
      integer type,cell
      REAL(kind=DOUBLE)t,x(3)
      integer cell1,exit_face,cell2,sector1,sector2
      integer j,k,k1,tx,i,start,count
      REAL(kind=DOUBLE)dt,tleft,t1,v1(3),temp(3)
      external cell_intersect,new_cell,in_surface
      REAL(kind=DOUBLE)cell_intersect
      integer new_cell
      logical in_surface
      if(type.EQ.0.OR.type.EQ.1.OR.type.EQ.2)continue
      if(cell.GT.0)continue
      j=cell
      x(1)=x0(1)
      x(2)=x0(2)
      x(3)=x0(3)
      
      if(type.EQ.2)then
      v1(1)=(0.0_DOUBLE)
      v1(2)=(0.0_DOUBLE)
      v1(3)=(0.0_DOUBLE)
      else
      v1(1)=v(1)
      v1(2)=v(2)
      v1(3)=v(3)
      
      end if
      t1=(0.0_DOUBLE)
      sector1=0
      sector2=0
90000 continue
      cell1=j
      tleft=tmax-t1
      if(sqrt((v1(1)**2+v1(2)**2+v1(3)**2)).GT.(0.0_DOUBLE))then
      dt=min(tleft,cell_intersect(cells(2,cell1),cells(3,cell1),nsurface
     &s,boundaries(cells(1,cell1)),surface_coeffs(1,1),x,v1,k))
      else
      dt=tleft
      end if
      t1=t1+dt
      x(1)=x(1)+v1(1)*(dt)
      x(2)=x(2)+v1(2)*(dt)
      x(3)=x(3)+v1(3)*(dt)
      
      if(dt.EQ.tleft)then
      t=tmax
      exit_face=0
      cell2=j
      track=.TRUE.
      return
      end if
      
      count=surface_sectors((sign(1,k)+1)/2,1,abs(k))
      if(count.GT.0)then
      start=surface_sectors((sign(1,k)+1)/2,0,abs(k))
      do i=0,count-1
      if(cells(4,cell1).EQ.sector_zone(sectors(start+i)))then
      sector1=sectors(start+i)
      if(sector_surface(sector1).EQ.k)continue
      goto 90008
      end if
      end do
90008 continue
      end if
      if(type.NE.0)then
      
      k1=surfaces_tx_ind((sign(1,k)+1)/2,0,abs(k))
      if(k1.NE.0)then
      tx=surfaces_tx_ind((sign(1,k)+1)/2,1,abs(k))
      k=k1
      temp(1)=x(1)
      temp(2)=x(2)
      temp(3)=x(3)
      
      call transform(surfaces_tx_mx(1,1,tx),temp,x)
      temp(1)=v1(1)
      temp(2)=v1(2)
      temp(3)=v1(3)
      
      call transform_velocity(surfaces_tx_mx(1,1,tx),temp,v1)
      if(type.NE.2)then
      v(1)=v1(1)
      v(2)=v1(2)
      v(3)=v1(3)
      
      end if
      if(in_surface(surface_coeffs(1,abs(k)),sign(1,k),x,.FALSE.,(0.0_DO
     &UBLE)))continue
      end if
      end if
      j=new_cell(k,x)
      
      count=surface_sectors((sign(1,-k)+1)/2,1,abs(k))
      if(count.GT.0)then
      start=surface_sectors((sign(1,-k)+1)/2,0,abs(k))
      do i=0,count-1
      if(cells(4,j).EQ.sector_zone(sectors(start+i)))then
      sector2=sectors(start+i)
      if(sector_surface(sector2).EQ.-k)continue
      goto 90009
      end if
      end do
90009 continue
      end if
      
      if(cells(4,cell1).EQ.cells(4,j).AND.sector1.EQ.0.AND.sector2.EQ.0)
     &goto 90000
      t=t1
      exit_face=k
      cell2=j
      track=.FALSE.
      return
      end
      function cell_enter(cell,x0,v,enter_face)
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)cell_enter
      integer cell
      REAL(kind=DOUBLE)x0(3),v(3)
      integer enter_face
      REAL(kind=DOUBLE)x(3),v1(3),t
      integer n,ns
      external surface_intersect,cell_intersect,inside_cell,inside_cell_
     &a,inside_surface
      REAL(kind=DOUBLE)surface_intersect,cell_intersect
      logical inside_cell,inside_cell_a,inside_surface
      cell_enter=(1.0e16_DOUBLE)
      ns=0
      if(inside_cell(cells(2,cell),cells(3,cell),nsurfaces,boundaries(ce
     &lls(1,cell)),surface_coeffs(1,1),x0))then
      v1(1)=(-(1.0_DOUBLE))*v(1)
      v1(2)=(-(1.0_DOUBLE))*v(2)
      v1(3)=(-(1.0_DOUBLE))*v(3)
      
      cell_enter=-cell_intersect(cells(2,cell),cells(3,cell),nsurfaces,b
     &oundaries(cells(1,cell)),surface_coeffs(1,1),x0,v1,enter_face)
      else
      do n=cells(1,cell),cells(1,cell)+cells(2,cell)-1
      if(inside_surface(surface_coeffs(1,abs(-boundaries(n))),sign(1,-bo
     &undaries(n)),x0))then
      t=surface_intersect(surface_coeffs(1,abs(-boundaries(n))),sign(1,-
     &boundaries(n)),x0,v)
      x(1)=x0(1)+v(1)*(t)
      x(2)=x0(2)+v(2)*(t)
      x(3)=x0(3)+v(3)*(t)
      
      if(inside_cell_a(cells(2,cell),cells(3,cell),nsurfaces,boundaries(
     &cells(1,cell)),surface_coeffs(1,1),boundaries(n),x))then
      if(t.LT.cell_enter)then
      cell_enter=t
      ns=n
      end if
      end if
      end if
      end do
      if(ns.GT.0)enter_face=ns
      end if
      return
      end
      function locate_point(x,zone)
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer locate_point
      REAL(kind=DOUBLE)x(3)
      integer zone
      integer k,c,cell1,cell2,sector1,sector2
      REAL(kind=DOUBLE)x0(3),x1(3),v(3),t,tmax
      logical done
      external track,new_cell,cell_enter
      REAL(kind=DOUBLE)cell_enter
      integer new_cell
      logical track
      external sloppy_inside
      logical sloppy_inside
      v(1)=(1.0_DOUBLE)
      v(2)=(0.0_DOUBLE)
      v(3)=(0.0_DOUBLE)
      t=cell_enter(0,x,v,k)
      if(t.GT.(1.0e-8_DOUBLE))then
      locate_point=-1
      goto 90007
      end if
      x0(1)=x(1)+v(1)*(t)
      x0(2)=x(2)+v(2)*(t)
      x0(3)=x(3)+v(3)*(t)
      
      c=new_cell(-k,x0)
      if(c.GT.0)continue
      if(t.GE.(0.0_DOUBLE))then
      locate_point=c
      goto 90007
      end if
      tmax=-t
90000 continue
      done=track(0,tmax,x0,v,c,t,x1,cell1,k,cell2,sector1,sector2)
      if(done)then
      locate_point=cell1
      goto 90007
      end if
      x0(1)=x1(1)
      x0(2)=x1(2)
      x0(3)=x1(3)
      
      tmax=tmax-t
      c=cell2
      goto 90000
90007 continue
      if(locate_point.GT.0)then
      if(sloppy_inside(cells(2,locate_point),cells(3,locate_point),nsurf
     &aces,boundaries(cells(1,locate_point)),surface_coeffs(1,1),x))cont
     &inue
      zone=cells(4,locate_point)
      else
      zone=-1
      end if
      return
      end
      function check_point_x(x)
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical check_point_x
      REAL(kind=DOUBLE)x(3)
      integer j,l
      logical interior,onface
      external sloppy_inside,inside_cell,cell_compare
      logical sloppy_inside,inside_cell
      integer cell_compare
      check_point_x=.FALSE.
      interior=.FALSE.
      onface=.FALSE.
      if(.NOT.sloppy_inside(cells(2,0),cells(3,0),nsurfaces,boundaries(c
     &ells(1,0)),surface_coeffs(1,1),x))then
      do j=1,ncells
      if(inside_cell(cells(2,j),cells(3,j),nsurfaces,boundaries(cells(1,
     &j)),surface_coeffs(1,1),x))return
      end do
      check_point_x=.TRUE.
      else
      do j=1,ncells
      l=cell_compare(cells(2,j),cells(3,j),nsurfaces,boundaries(cells(1,
     &j)),surface_coeffs(1,1),x)
      if(l.GE.0)then
      if(interior)return
      if(l.GT.0)then
      if(onface)return
      interior=.TRUE.
      else
      onface=.TRUE.
      end if
      end if
      end do
      check_point_x=onface.OR.interior
      end if
      return
      end
      function check_location(pos_x,cell_x,zone_x,surface_x,cell_next_x,
     &zone_next_x,sector_x,sector_next_x)
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical check_location
      REAL(kind=DOUBLE)pos_x(3)
      integer cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x
      external sloppy_inside,face_in_cell,in_surface
      logical sloppy_inside,face_in_cell,in_surface
      check_location=.FALSE.
      if(cell_x.LE.0.OR.cell_x.GT.ncells)return
      if(zone_x.LE.0)return
      if(zone_x.NE.cells(4,cell_x))return
      if(.NOT.sloppy_inside(cells(2,cell_x),cells(3,cell_x),nsurfaces,bo
     &undaries(cells(1,cell_x)),surface_coeffs(1,1),pos_x))return
      if(surface_x.NE.0)then
      if(abs(surface_x).GT.nsurfaces)return
      if(cell_next_x.LE.0.OR.cell_x.GT.ncells)return
      if(zone_next_x.LE.0)return
      if(zone_next_x.NE.cells(4,cell_next_x))return
      if(.NOT.sloppy_inside(cells(2,cell_next_x),cells(3,cell_next_x),ns
     &urfaces,boundaries(cells(1,cell_next_x)),surface_coeffs(1,1),pos_x
     &))return
      if(.NOT.in_surface(surface_coeffs(1,abs(surface_x)),sign(1,surface
     &_x),pos_x,.FALSE.,(0.0_DOUBLE)))return
      if(.NOT.face_in_cell(cells(2,cell_x),cells(3,cell_x),nsurfaces,bou
     &ndaries(cells(1,cell_x)),surface_coeffs(1,1),surface_x))return
      if(.NOT.face_in_cell(cells(2,cell_next_x),cells(3,cell_next_x),nsu
     &rfaces,boundaries(cells(1,cell_next_x)),surface_coeffs(1,1),-surfa
     &ce_x))return
      end if
      check_location=.TRUE.
      return
      end
      function check_zone(x)
      
      use zn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical check_zone
      integer x
      check_zone=.FALSE.
      if(x.LE.0.OR.x.GT.zn_num)return
      if(zone_type(x).LE.0.OR.zone_type(x).GT.4)return
      check_zone=.TRUE.
      return
      end
      subroutine read_geometry
      
      use gi_mod
      
      use zn_mod
      
      use rf_mod
      
      use sc_mod
      
      use de_mod
      
      use mp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer nc_dims(5),nc_corner(5),nc_edge(5),nc_stat,nc_size,nc_rank
     &,nc_attr,nc_type
      character*128 nc_dummy
      external ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf
     &_inq_format
      integer ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf_
     &inq_format
      integer ncells_id
      integer nsurfaces_id
      integer nboundaries_id
      integer nneighbors_id
      integer ntransforms_id
      integer geometry_symmetry_id
      integer cell_info_ind_id
      integer cell_ind_id
      integer surface_ind_id
      integer boundary_ind_id
      integer neighbor_ind_id
      integer neg_pos_id
      integer surface_info_ind_id
      integer surface_tx_ind_id
      integer tx_ind_1_id
      integer tx_ind_2_id
      integer transform_ind_id
      integer coeff_ind_id
      integer universal_cell_min_id
      integer universal_cell_max_id
      integer universal_cell_vol_id
      integer cells_id
      integer surfaces_id
      integer surfaces_tx_ind_id
      integer surfaces_tx_mx_id
      integer surface_sectors_id
      integer boundaries_id
      integer neighbors_id
      integer surface_coeffs_id
      integer surface_points_id
      
      integer zn_num_id
      integer zone_type_ind_id
      integer zone_index_ind_id
      integer zone_ind_id
      integer zone_type_num_id
      integer zone_type_id
      integer zone_index_id
      integer zone_index_min_id
      integer zone_index_max_id
      integer zone_pointer_id
      integer zone_volume_id
      integer zone_center_id
      integer zone_min_id
      integer zone_max_id
      
      integer vector_id
      integer string_id
      
      include 'mpif.h'
      integer mpi_err
      integer mpi_status
      dimension mpi_status(MPI_STATUS_SIZE)
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
      
      
      integer nsectors_id
      integer sector_ind_id
      integer sector_neg_pos_ind_id
      integer sector_type_ind_id
      integer strata_id
      integer sector_strata_segment_id
      integer sectors_id
      integer sector_zone_id
      integer sector_surface_id
      integer sector_points_id
      integer sector_type_pointer_id
      integer sc_vacuum_num_id
      integer vacuum_ind_id
      integer vacuum_sector_id
      integer sc_plasma_num_id
      integer plasma_ind_id
      integer plasma_sector_id
      integer sc_target_num_id
      integer target_ind_id
      integer target_sector_id
      integer target_material_id
      integer target_temperature_id
      integer target_recyc_coef_id
      integer sc_wall_num_id
      integer wall_ind_id
      integer wall_sector_id
      integer wall_material_id
      integer wall_temperature_id
      integer wall_recyc_coef_id
      integer sc_exit_num_id
      integer exit_ind_id
      integer exit_sector_id
      integer sc_diagnostic_grps_id
      integer sc_diag_name_string_id
      integer diag_grp_ind_id
      integer sc_diag_max_bins_id
      integer diagnostic_grp_name_id
      integer diagnostic_num_sectors_id
      integer diagnostic_var_id
      integer diagnostic_tab_index_id
      integer diagnostic_min_id
      integer diagnostic_delta_id
      integer diagnostic_spacing_id
      integer diagnostic_grp_base_id
      integer sc_diag_size_id
      integer sc_diag_ind_id
      integer diagnostic_sector_tab_id
      
      integer de_symbol_string_id
      integer de_name_string_id
      integer de_grps_id
      integer de_grp_ind_id
      integer de_max_bins_id
      integer de_zone_frags_dim_id
      integer de_zone_frags_ind_id
      integer de_zone_frags_size_id
      integer detector_name_id
      integer detector_num_views_id
      integer detector_var_id
      integer detector_tab_index_id
      integer detector_min_id
      integer detector_delta_id
      integer detector_spacing_id
      integer detector_total_views_id
      integer de_tot_view_ind_id
      integer de_start_end_ind_id
      integer de_view_points_id
      integer de_view_algorithm_id
      integer de_view_halfwidth_id
      integer de_zone_frags_id
      integer de_zone_frags_start_id
      integer de_zone_frags_num_id
      integer de_zone_frags_zones_id
      integer de_zone_frags_min_zn_id
      integer de_zone_frags_max_zn_id
      integer de_view_base_id
      integer de_view_size_id
      integer de_view_ind_id
      integer de_view_tab_id
      
      integer fileid
      character*96 tempfile
      if((mpi_rank.EQ.mpi_degas2_root))then
      tempfile=filenames_array(4)
      if(tempfile.NE.'undefined')continue
      fileid=ncopn(tempfile,0,nc_stat)
      vector_id=ncdid(fileid,'vector',nc_stat)
      call ncdinq(fileid,vector_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((3)-(1)+1))continue
      string_id=ncdid(fileid,'string',nc_stat)
      call ncdinq(fileid,string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((300)-(1)+1))continue
      
      ncells_id=ncvid(fileid,'ncells',nc_stat)
      call ncvinq(fileid,ncells_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_a
     &ttr,nc_stat)
      
      call ncvgt(fileid,ncells_id,nc_corner,nc_edge,ncells,nc_stat)
      nsurfaces_id=ncvid(fileid,'nsurfaces',nc_stat)
      call ncvinq(fileid,nsurfaces_id,nc_dummy,nc_type,nc_rank,nc_dims,n
     &c_attr,nc_stat)
      
      call ncvgt(fileid,nsurfaces_id,nc_corner,nc_edge,nsurfaces,nc_stat
     &)
      nboundaries_id=ncvid(fileid,'nboundaries',nc_stat)
      call ncvinq(fileid,nboundaries_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      
      call ncvgt(fileid,nboundaries_id,nc_corner,nc_edge,nboundaries,nc_
     &stat)
      nneighbors_id=ncvid(fileid,'nneighbors',nc_stat)
      call ncvinq(fileid,nneighbors_id,nc_dummy,nc_type,nc_rank,nc_dims,
     &nc_attr,nc_stat)
      
      call ncvgt(fileid,nneighbors_id,nc_corner,nc_edge,nneighbors,nc_st
     &at)
      ntransforms_id=ncvid(fileid,'ntransforms',nc_stat)
      call ncvinq(fileid,ntransforms_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      
      call ncvgt(fileid,ntransforms_id,nc_corner,nc_edge,ntransforms,nc_
     &stat)
      geometry_symmetry_id=ncvid(fileid,'geometry_symmetry',nc_stat)
      call ncvinq(fileid,geometry_symmetry_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,geometry_symmetry_id,nc_corner,nc_edge,geometry_
     &symmetry,nc_stat)
      cell_info_ind_id=ncdid(fileid,'cell_info_ind',nc_stat)
      call ncdinq(fileid,cell_info_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((4)-(1)+1))continue
      cell_ind_id=ncdid(fileid,'cell_ind',nc_stat)
      call ncdinq(fileid,cell_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((ncells)-(0)+1))continue
      surface_ind_id=ncdid(fileid,'surface_ind',nc_stat)
      call ncdinq(fileid,surface_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((nsurfaces)-(1)+1))continue
      boundary_ind_id=ncdid(fileid,'boundary_ind',nc_stat)
      call ncdinq(fileid,boundary_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((nboundaries)-(1)+1))continue
      neighbor_ind_id=ncdid(fileid,'neighbor_ind',nc_stat)
      call ncdinq(fileid,neighbor_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((nneighbors)-(0)+1))continue
      neg_pos_id=ncdid(fileid,'neg_pos',nc_stat)
      call ncdinq(fileid,neg_pos_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((1)-(0)+1))continue
      surface_info_ind_id=ncdid(fileid,'surface_info_ind',nc_stat)
      call ncdinq(fileid,surface_info_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((1)-(0)+1))continue
      surface_tx_ind_id=ncdid(fileid,'surface_tx_ind',nc_stat)
      call ncdinq(fileid,surface_tx_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((1)-(0)+1))continue
      tx_ind_1_id=ncdid(fileid,'tx_ind_1',nc_stat)
      call ncdinq(fileid,tx_ind_1_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((3)-(1)+1))continue
      tx_ind_2_id=ncdid(fileid,'tx_ind_2',nc_stat)
      call ncdinq(fileid,tx_ind_2_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((4)-(1)+1))continue
      transform_ind_id=ncdid(fileid,'transform_ind',nc_stat)
      call ncdinq(fileid,transform_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((ntransforms)-(0)+1))continue
      coeff_ind_id=ncdid(fileid,'coeff_ind',nc_stat)
      call ncdinq(fileid,coeff_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((10)-(1)+1))continue
      universal_cell_min_id=ncvid(fileid,'universal_cell_min',nc_stat)
      call ncvinq(fileid,universal_cell_min_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.vector_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      call ncvgt(fileid,universal_cell_min_id,nc_corner,nc_edge,universa
     &l_cell_min,nc_stat)
      universal_cell_max_id=ncvid(fileid,'universal_cell_max',nc_stat)
      call ncvinq(fileid,universal_cell_max_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.vector_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      call ncvgt(fileid,universal_cell_max_id,nc_corner,nc_edge,universa
     &l_cell_max,nc_stat)
      universal_cell_vol_id=ncvid(fileid,'universal_cell_vol',nc_stat)
      call ncvinq(fileid,universal_cell_vol_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,universal_cell_vol_id,nc_corner,nc_edge,universa
     &l_cell_vol,nc_stat)
      cells_id=ncvid(fileid,'cells',nc_stat)
      call ncvinq(fileid,cells_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_at
     &tr,nc_stat)
      if(nc_dims(1).EQ.cell_info_ind_id)continue
      if(nc_dims(2).EQ.cell_ind_id)continue
      
      cells =>mem_alloc_i2((1),(4),(0),(ncells),'cells')
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((ncells)-(0)+1)
      call ncvgt(fileid,cells_id,nc_corner,nc_edge,cells,nc_stat)
      surfaces_id=ncvid(fileid,'surfaces',nc_stat)
      call ncvinq(fileid,surfaces_id,nc_dummy,nc_type,nc_rank,nc_dims,nc
     &_attr,nc_stat)
      if(nc_dims(1).EQ.neg_pos_id)continue
      if(nc_dims(2).EQ.surface_info_ind_id)continue
      if(nc_dims(3).EQ.surface_ind_id)continue
      
      surfaces =>mem_alloc_i3((0),(1),(0),(1),(1),(nsurfaces),'surfaces'
     &)
      nc_corner(1)=1
      nc_edge(1)=((1)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((1)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((nsurfaces)-(1)+1)
      call ncvgt(fileid,surfaces_id,nc_corner,nc_edge,surfaces,nc_stat)
      surfaces_tx_ind_id=ncvid(fileid,'surfaces_tx_ind',nc_stat)
      call ncvinq(fileid,surfaces_tx_ind_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.neg_pos_id)continue
      if(nc_dims(2).EQ.surface_tx_ind_id)continue
      if(nc_dims(3).EQ.surface_ind_id)continue
      
      surfaces_tx_ind =>mem_alloc_i3((0),(1),(0),(1),(1),(nsurfaces),'su
     &rfaces_tx_ind')
      nc_corner(1)=1
      nc_edge(1)=((1)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((1)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((nsurfaces)-(1)+1)
      call ncvgt(fileid,surfaces_tx_ind_id,nc_corner,nc_edge,surfaces_tx
     &_ind,nc_stat)
      surfaces_tx_mx_id=ncvid(fileid,'surfaces_tx_mx',nc_stat)
      call ncvinq(fileid,surfaces_tx_mx_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.tx_ind_1_id)continue
      if(nc_dims(2).EQ.tx_ind_2_id)continue
      if(nc_dims(3).EQ.transform_ind_id)continue
      
      surfaces_tx_mx =>mem_alloc_r3((1),(3),(1),(4),(0),(ntransforms),'s
     &urfaces_tx_mx')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((ntransforms)-(0)+1)
      call ncvgt(fileid,surfaces_tx_mx_id,nc_corner,nc_edge,surfaces_tx_
     &mx,nc_stat)
      surface_sectors_id=ncvid(fileid,'surface_sectors',nc_stat)
      call ncvinq(fileid,surface_sectors_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.neg_pos_id)continue
      if(nc_dims(2).EQ.surface_info_ind_id)continue
      if(nc_dims(3).EQ.surface_ind_id)continue
      
      surface_sectors =>mem_alloc_i3((0),(1),(0),(1),(1),(nsurfaces),'su
     &rface_sectors')
      nc_corner(1)=1
      nc_edge(1)=((1)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((1)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((nsurfaces)-(1)+1)
      call ncvgt(fileid,surface_sectors_id,nc_corner,nc_edge,surface_sec
     &tors,nc_stat)
      boundaries_id=ncvid(fileid,'boundaries',nc_stat)
      call ncvinq(fileid,boundaries_id,nc_dummy,nc_type,nc_rank,nc_dims,
     &nc_attr,nc_stat)
      if(nc_dims(1).EQ.boundary_ind_id)continue
      
      boundaries =>mem_alloc_i1((1),(nboundaries),'boundaries')
      nc_corner(1)=1
      nc_edge(1)=((nboundaries)-(1)+1)
      call ncvgt(fileid,boundaries_id,nc_corner,nc_edge,boundaries,nc_st
     &at)
      neighbors_id=ncvid(fileid,'neighbors',nc_stat)
      call ncvinq(fileid,neighbors_id,nc_dummy,nc_type,nc_rank,nc_dims,n
     &c_attr,nc_stat)
      if(nc_dims(1).EQ.neighbor_ind_id)continue
      
      neighbors =>mem_alloc_i1((0),(nneighbors),'neighbors')
      nc_corner(1)=1
      nc_edge(1)=((nneighbors)-(0)+1)
      call ncvgt(fileid,neighbors_id,nc_corner,nc_edge,neighbors,nc_stat
     &)
      surface_coeffs_id=ncvid(fileid,'surface_coeffs',nc_stat)
      call ncvinq(fileid,surface_coeffs_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.coeff_ind_id)continue
      if(nc_dims(2).EQ.surface_ind_id)continue
      
      surface_coeffs =>mem_alloc_r2((1),(10),(1),(nsurfaces),'surface_co
     &effs')
      nc_corner(1)=1
      nc_edge(1)=((10)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((nsurfaces)-(1)+1)
      call ncvgt(fileid,surface_coeffs_id,nc_corner,nc_edge,surface_coef
     &fs,nc_stat)
      surface_points_id=ncvid(fileid,'surface_points',nc_stat)
      call ncvinq(fileid,surface_points_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.vector_id)continue
      if(nc_dims(2).EQ.neg_pos_id)continue
      if(nc_dims(3).EQ.surface_ind_id)continue
      
      surface_points =>mem_alloc_r3((1),(3),(0),(1),(1),(nsurfaces),'sur
     &face_points')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((1)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((nsurfaces)-(1)+1)
      call ncvgt(fileid,surface_points_id,nc_corner,nc_edge,surface_poin
     &ts,nc_stat)
      
      zn_num_id=ncvid(fileid,'zn_num',nc_stat)
      call ncvinq(fileid,zn_num_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_a
     &ttr,nc_stat)
      
      call ncvgt(fileid,zn_num_id,nc_corner,nc_edge,zn_num,nc_stat)
      zone_type_ind_id=ncdid(fileid,'zone_type_ind',nc_stat)
      call ncdinq(fileid,zone_type_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((4)-(1)+1))continue
      zone_index_ind_id=ncdid(fileid,'zone_index_ind',nc_stat)
      call ncdinq(fileid,zone_index_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((4)-(1)+1))continue
      zone_ind_id=ncdid(fileid,'zone_ind',nc_stat)
      call ncdinq(fileid,zone_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((zn_num)-(1)+1))continue
      zone_type_num_id=ncvid(fileid,'zone_type_num',nc_stat)
      call ncvinq(fileid,zone_type_num_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      if(nc_dims(1).EQ.zone_type_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      call ncvgt(fileid,zone_type_num_id,nc_corner,nc_edge,zone_type_num
     &,nc_stat)
      zone_type_id=ncvid(fileid,'zone_type',nc_stat)
      call ncvinq(fileid,zone_type_id,nc_dummy,nc_type,nc_rank,nc_dims,n
     &c_attr,nc_stat)
      if(nc_dims(1).EQ.zone_ind_id)continue
      
      zone_type =>mem_alloc_i1((1),(zn_num),'zone_type')
      nc_corner(1)=1
      nc_edge(1)=((zn_num)-(1)+1)
      call ncvgt(fileid,zone_type_id,nc_corner,nc_edge,zone_type,nc_stat
     &)
      zone_index_id=ncvid(fileid,'zone_index',nc_stat)
      call ncvinq(fileid,zone_index_id,nc_dummy,nc_type,nc_rank,nc_dims,
     &nc_attr,nc_stat)
      if(nc_dims(1).EQ.zone_index_ind_id)continue
      if(nc_dims(2).EQ.zone_ind_id)continue
      
      zone_index =>mem_alloc_i2((1),(4),(1),(zn_num),'zone_index')
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((zn_num)-(1)+1)
      call ncvgt(fileid,zone_index_id,nc_corner,nc_edge,zone_index,nc_st
     &at)
      zone_index_min_id=ncvid(fileid,'zone_index_min',nc_stat)
      call ncvinq(fileid,zone_index_min_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.zone_index_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      call ncvgt(fileid,zone_index_min_id,nc_corner,nc_edge,zone_index_m
     &in,nc_stat)
      zone_index_max_id=ncvid(fileid,'zone_index_max',nc_stat)
      call ncvinq(fileid,zone_index_max_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.zone_index_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      call ncvgt(fileid,zone_index_max_id,nc_corner,nc_edge,zone_index_m
     &ax,nc_stat)
      zone_pointer_id=ncvid(fileid,'zone_pointer',nc_stat)
      call ncvinq(fileid,zone_pointer_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      if(nc_dims(1).EQ.zone_ind_id)continue
      
      zone_pointer =>mem_alloc_i1((1),(zn_num),'zone_pointer')
      nc_corner(1)=1
      nc_edge(1)=((zn_num)-(1)+1)
      call ncvgt(fileid,zone_pointer_id,nc_corner,nc_edge,zone_pointer,n
     &c_stat)
      zone_volume_id=ncvid(fileid,'zone_volume',nc_stat)
      call ncvinq(fileid,zone_volume_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      if(nc_dims(1).EQ.zone_ind_id)continue
      
      zone_volume =>mem_alloc_r1((1),(zn_num),'zone_volume')
      nc_corner(1)=1
      nc_edge(1)=((zn_num)-(1)+1)
      call ncvgt(fileid,zone_volume_id,nc_corner,nc_edge,zone_volume,nc_
     &stat)
      zone_center_id=ncvid(fileid,'zone_center',nc_stat)
      call ncvinq(fileid,zone_center_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      if(nc_dims(1).EQ.vector_id)continue
      if(nc_dims(2).EQ.zone_ind_id)continue
      
      zone_center =>mem_alloc_r2((1),(3),(1),(zn_num),'zone_center')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((zn_num)-(1)+1)
      call ncvgt(fileid,zone_center_id,nc_corner,nc_edge,zone_center,nc_
     &stat)
      zone_min_id=ncvid(fileid,'zone_min',nc_stat)
      call ncvinq(fileid,zone_min_id,nc_dummy,nc_type,nc_rank,nc_dims,nc
     &_attr,nc_stat)
      if(nc_dims(1).EQ.vector_id)continue
      if(nc_dims(2).EQ.zone_ind_id)continue
      
      zone_min =>mem_alloc_r2((1),(3),(1),(zn_num),'zone_min')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((zn_num)-(1)+1)
      call ncvgt(fileid,zone_min_id,nc_corner,nc_edge,zone_min,nc_stat)
      zone_max_id=ncvid(fileid,'zone_max',nc_stat)
      call ncvinq(fileid,zone_max_id,nc_dummy,nc_type,nc_rank,nc_dims,nc
     &_attr,nc_stat)
      if(nc_dims(1).EQ.vector_id)continue
      if(nc_dims(2).EQ.zone_ind_id)continue
      
      zone_max =>mem_alloc_r2((1),(3),(1),(zn_num),'zone_max')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((zn_num)-(1)+1)
      call ncvgt(fileid,zone_max_id,nc_corner,nc_edge,zone_max,nc_stat)
      
      nsectors_id=ncvid(fileid,'nsectors',nc_stat)
      call ncvinq(fileid,nsectors_id,nc_dummy,nc_type,nc_rank,nc_dims,nc
     &_attr,nc_stat)
      
      call ncvgt(fileid,nsectors_id,nc_corner,nc_edge,nsectors,nc_stat)
      sector_ind_id=ncdid(fileid,'sector_ind',nc_stat)
      call ncdinq(fileid,sector_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((nsectors)-(0)+1))continue
      sector_neg_pos_ind_id=ncdid(fileid,'sector_neg_pos_ind',nc_stat)
      call ncdinq(fileid,sector_neg_pos_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((1)-(0)+1))continue
      sector_type_ind_id=ncdid(fileid,'sector_type_ind',nc_stat)
      call ncdinq(fileid,sector_type_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((17)-(1)+1))continue
      strata_id=ncvid(fileid,'strata',nc_stat)
      call ncvinq(fileid,strata_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_a
     &ttr,nc_stat)
      if(nc_dims(1).EQ.sector_ind_id)continue
      
      strata =>mem_alloc_i1((0),(nsectors),'strata')
      nc_corner(1)=1
      nc_edge(1)=((nsectors)-(0)+1)
      call ncvgt(fileid,strata_id,nc_corner,nc_edge,strata,nc_stat)
      sector_strata_segment_id=ncvid(fileid,'sector_strata_segment',nc_s
     &tat)
      call ncvinq(fileid,sector_strata_segment_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.sector_ind_id)continue
      
      sector_strata_segment =>mem_alloc_i1((0),(nsectors),'sector_strata
     &_segment')
      nc_corner(1)=1
      nc_edge(1)=((nsectors)-(0)+1)
      call ncvgt(fileid,sector_strata_segment_id,nc_corner,nc_edge,secto
     &r_strata_segment,nc_stat)
      sectors_id=ncvid(fileid,'sectors',nc_stat)
      call ncvinq(fileid,sectors_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_
     &attr,nc_stat)
      if(nc_dims(1).EQ.sector_ind_id)continue
      
      sectors =>mem_alloc_i1((0),(nsectors),'sectors')
      nc_corner(1)=1
      nc_edge(1)=((nsectors)-(0)+1)
      call ncvgt(fileid,sectors_id,nc_corner,nc_edge,sectors,nc_stat)
      sector_zone_id=ncvid(fileid,'sector_zone',nc_stat)
      call ncvinq(fileid,sector_zone_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      if(nc_dims(1).EQ.sector_ind_id)continue
      
      sector_zone =>mem_alloc_i1((0),(nsectors),'sector_zone')
      nc_corner(1)=1
      nc_edge(1)=((nsectors)-(0)+1)
      call ncvgt(fileid,sector_zone_id,nc_corner,nc_edge,sector_zone,nc_
     &stat)
      sector_surface_id=ncvid(fileid,'sector_surface',nc_stat)
      call ncvinq(fileid,sector_surface_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.sector_ind_id)continue
      
      sector_surface =>mem_alloc_i1((0),(nsectors),'sector_surface')
      nc_corner(1)=1
      nc_edge(1)=((nsectors)-(0)+1)
      call ncvgt(fileid,sector_surface_id,nc_corner,nc_edge,sector_surfa
     &ce,nc_stat)
      sector_points_id=ncvid(fileid,'sector_points',nc_stat)
      call ncvinq(fileid,sector_points_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      if(nc_dims(1).EQ.vector_id)continue
      if(nc_dims(2).EQ.sector_neg_pos_ind_id)continue
      if(nc_dims(3).EQ.sector_ind_id)continue
      
      sector_points =>mem_alloc_r3((1),(3),(0),(1),(0),(nsectors),'secto
     &r_points')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((1)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((nsectors)-(0)+1)
      call ncvgt(fileid,sector_points_id,nc_corner,nc_edge,sector_points
     &,nc_stat)
      sector_type_pointer_id=ncvid(fileid,'sector_type_pointer',nc_stat)
      call ncvinq(fileid,sector_type_pointer_id,nc_dummy,nc_type,nc_rank
     &,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.sector_type_ind_id)continue
      if(nc_dims(2).EQ.sector_ind_id)continue
      
      sector_type_pointer =>mem_alloc_i2((1),(17),(0),(nsectors),'sector
     &_type_pointer')
      nc_corner(1)=1
      nc_edge(1)=((17)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((nsectors)-(0)+1)
      call ncvgt(fileid,sector_type_pointer_id,nc_corner,nc_edge,sector_
     &type_pointer,nc_stat)
      sc_vacuum_num_id=ncvid(fileid,'sc_vacuum_num',nc_stat)
      call ncvinq(fileid,sc_vacuum_num_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      
      call ncvgt(fileid,sc_vacuum_num_id,nc_corner,nc_edge,sc_vacuum_num
     &,nc_stat)
      vacuum_ind_id=ncdid(fileid,'vacuum_ind',nc_stat)
      call ncdinq(fileid,vacuum_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((sc_vacuum_num)-(0)+1))continue
      vacuum_sector_id=ncvid(fileid,'vacuum_sector',nc_stat)
      call ncvinq(fileid,vacuum_sector_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      if(nc_dims(1).EQ.vacuum_ind_id)continue
      
      vacuum_sector =>mem_alloc_i1((0),(sc_vacuum_num),'vacuum_sector')
      nc_corner(1)=1
      nc_edge(1)=((sc_vacuum_num)-(0)+1)
      call ncvgt(fileid,vacuum_sector_id,nc_corner,nc_edge,vacuum_sector
     &,nc_stat)
      sc_plasma_num_id=ncvid(fileid,'sc_plasma_num',nc_stat)
      call ncvinq(fileid,sc_plasma_num_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      
      call ncvgt(fileid,sc_plasma_num_id,nc_corner,nc_edge,sc_plasma_num
     &,nc_stat)
      plasma_ind_id=ncdid(fileid,'plasma_ind',nc_stat)
      call ncdinq(fileid,plasma_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((sc_plasma_num)-(0)+1))continue
      plasma_sector_id=ncvid(fileid,'plasma_sector',nc_stat)
      call ncvinq(fileid,plasma_sector_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      if(nc_dims(1).EQ.plasma_ind_id)continue
      
      plasma_sector =>mem_alloc_i1((0),(sc_plasma_num),'plasma_sector')
      nc_corner(1)=1
      nc_edge(1)=((sc_plasma_num)-(0)+1)
      call ncvgt(fileid,plasma_sector_id,nc_corner,nc_edge,plasma_sector
     &,nc_stat)
      sc_target_num_id=ncvid(fileid,'sc_target_num',nc_stat)
      call ncvinq(fileid,sc_target_num_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      
      call ncvgt(fileid,sc_target_num_id,nc_corner,nc_edge,sc_target_num
     &,nc_stat)
      target_ind_id=ncdid(fileid,'target_ind',nc_stat)
      call ncdinq(fileid,target_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((sc_target_num)-(0)+1))continue
      target_sector_id=ncvid(fileid,'target_sector',nc_stat)
      call ncvinq(fileid,target_sector_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      if(nc_dims(1).EQ.target_ind_id)continue
      
      target_sector =>mem_alloc_i1((0),(sc_target_num),'target_sector')
      nc_corner(1)=1
      nc_edge(1)=((sc_target_num)-(0)+1)
      call ncvgt(fileid,target_sector_id,nc_corner,nc_edge,target_sector
     &,nc_stat)
      target_material_id=ncvid(fileid,'target_material',nc_stat)
      call ncvinq(fileid,target_material_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.target_ind_id)continue
      
      target_material =>mem_alloc_i1((0),(sc_target_num),'target_materia
     &l')
      nc_corner(1)=1
      nc_edge(1)=((sc_target_num)-(0)+1)
      call ncvgt(fileid,target_material_id,nc_corner,nc_edge,target_mate
     &rial,nc_stat)
      target_temperature_id=ncvid(fileid,'target_temperature',nc_stat)
      call ncvinq(fileid,target_temperature_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.target_ind_id)continue
      
      target_temperature =>mem_alloc_r1((0),(sc_target_num),'target_temp
     &erature')
      nc_corner(1)=1
      nc_edge(1)=((sc_target_num)-(0)+1)
      call ncvgt(fileid,target_temperature_id,nc_corner,nc_edge,target_t
     &emperature,nc_stat)
      target_recyc_coef_id=ncvid(fileid,'target_recyc_coef',nc_stat)
      call ncvinq(fileid,target_recyc_coef_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.target_ind_id)continue
      
      target_recyc_coef =>mem_alloc_r1((0),(sc_target_num),'target_recyc
     &_coef')
      nc_corner(1)=1
      nc_edge(1)=((sc_target_num)-(0)+1)
      call ncvgt(fileid,target_recyc_coef_id,nc_corner,nc_edge,target_re
     &cyc_coef,nc_stat)
      sc_wall_num_id=ncvid(fileid,'sc_wall_num',nc_stat)
      call ncvinq(fileid,sc_wall_num_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      
      call ncvgt(fileid,sc_wall_num_id,nc_corner,nc_edge,sc_wall_num,nc_
     &stat)
      wall_ind_id=ncdid(fileid,'wall_ind',nc_stat)
      call ncdinq(fileid,wall_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((sc_wall_num)-(0)+1))continue
      wall_sector_id=ncvid(fileid,'wall_sector',nc_stat)
      call ncvinq(fileid,wall_sector_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      if(nc_dims(1).EQ.wall_ind_id)continue
      
      wall_sector =>mem_alloc_i1((0),(sc_wall_num),'wall_sector')
      nc_corner(1)=1
      nc_edge(1)=((sc_wall_num)-(0)+1)
      call ncvgt(fileid,wall_sector_id,nc_corner,nc_edge,wall_sector,nc_
     &stat)
      wall_material_id=ncvid(fileid,'wall_material',nc_stat)
      call ncvinq(fileid,wall_material_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      if(nc_dims(1).EQ.wall_ind_id)continue
      
      wall_material =>mem_alloc_i1((0),(sc_wall_num),'wall_material')
      nc_corner(1)=1
      nc_edge(1)=((sc_wall_num)-(0)+1)
      call ncvgt(fileid,wall_material_id,nc_corner,nc_edge,wall_material
     &,nc_stat)
      wall_temperature_id=ncvid(fileid,'wall_temperature',nc_stat)
      call ncvinq(fileid,wall_temperature_id,nc_dummy,nc_type,nc_rank,nc
     &_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.wall_ind_id)continue
      
      wall_temperature =>mem_alloc_r1((0),(sc_wall_num),'wall_temperatur
     &e')
      nc_corner(1)=1
      nc_edge(1)=((sc_wall_num)-(0)+1)
      call ncvgt(fileid,wall_temperature_id,nc_corner,nc_edge,wall_tempe
     &rature,nc_stat)
      wall_recyc_coef_id=ncvid(fileid,'wall_recyc_coef',nc_stat)
      call ncvinq(fileid,wall_recyc_coef_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.wall_ind_id)continue
      
      wall_recyc_coef =>mem_alloc_r1((0),(sc_wall_num),'wall_recyc_coef'
     &)
      nc_corner(1)=1
      nc_edge(1)=((sc_wall_num)-(0)+1)
      call ncvgt(fileid,wall_recyc_coef_id,nc_corner,nc_edge,wall_recyc_
     &coef,nc_stat)
      sc_exit_num_id=ncvid(fileid,'sc_exit_num',nc_stat)
      call ncvinq(fileid,sc_exit_num_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      
      call ncvgt(fileid,sc_exit_num_id,nc_corner,nc_edge,sc_exit_num,nc_
     &stat)
      exit_ind_id=ncdid(fileid,'exit_ind',nc_stat)
      call ncdinq(fileid,exit_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((sc_exit_num)-(0)+1))continue
      exit_sector_id=ncvid(fileid,'exit_sector',nc_stat)
      call ncvinq(fileid,exit_sector_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      if(nc_dims(1).EQ.exit_ind_id)continue
      
      exit_sector =>mem_alloc_i1((0),(sc_exit_num),'exit_sector')
      nc_corner(1)=1
      nc_edge(1)=((sc_exit_num)-(0)+1)
      call ncvgt(fileid,exit_sector_id,nc_corner,nc_edge,exit_sector,nc_
     &stat)
      sc_diagnostic_grps_id=ncvid(fileid,'sc_diagnostic_grps',nc_stat)
      call ncvinq(fileid,sc_diagnostic_grps_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,sc_diagnostic_grps_id,nc_corner,nc_edge,sc_diagn
     &ostic_grps,nc_stat)
      sc_diag_name_string_id=ncdid(fileid,'sc_diag_name_string',nc_stat)
      call ncdinq(fileid,sc_diag_name_string_id,nc_dummy,nc_size,nc_stat
     &)
      if(nc_size.EQ.((40)-(1)+1))continue
      diag_grp_ind_id=ncdid(fileid,'diag_grp_ind',nc_stat)
      call ncdinq(fileid,diag_grp_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((sc_diagnostic_grps)-(0)+1))continue
      sc_diag_max_bins_id=ncvid(fileid,'sc_diag_max_bins',nc_stat)
      call ncvinq(fileid,sc_diag_max_bins_id,nc_dummy,nc_type,nc_rank,nc
     &_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,sc_diag_max_bins_id,nc_corner,nc_edge,sc_diag_ma
     &x_bins,nc_stat)
      diagnostic_grp_name_id=ncvid(fileid,'diagnostic_grp_name',nc_stat)
      call ncvinq(fileid,diagnostic_grp_name_id,nc_dummy,nc_type,nc_rank
     &,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.sc_diag_name_string_id)continue
      if(nc_dims(2).EQ.diag_grp_ind_id)continue
      
      diagnostic_grp_name =>mem_alloc_c1((40),(0),(sc_diagnostic_grps),'
     &diagnostic_grp_name')
      nc_corner(1)=1
      nc_edge(1)=((40)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((sc_diagnostic_grps)-(0)+1)
      nc_stat=nf_get_vara_text(fileid,diagnostic_grp_name_id,nc_corner,n
     &c_edge,diagnostic_grp_name)
      diagnostic_num_sectors_id=ncvid(fileid,'diagnostic_num_sectors',nc
     &_stat)
      call ncvinq(fileid,diagnostic_num_sectors_id,nc_dummy,nc_type,nc_r
     &ank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.diag_grp_ind_id)continue
      
      diagnostic_num_sectors =>mem_alloc_i1((0),(sc_diagnostic_grps),'di
     &agnostic_num_sectors')
      nc_corner(1)=1
      nc_edge(1)=((sc_diagnostic_grps)-(0)+1)
      call ncvgt(fileid,diagnostic_num_sectors_id,nc_corner,nc_edge,diag
     &nostic_num_sectors,nc_stat)
      diagnostic_var_id=ncvid(fileid,'diagnostic_var',nc_stat)
      call ncvinq(fileid,diagnostic_var_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.diag_grp_ind_id)continue
      
      diagnostic_var =>mem_alloc_i1((0),(sc_diagnostic_grps),'diagnostic
     &_var')
      nc_corner(1)=1
      nc_edge(1)=((sc_diagnostic_grps)-(0)+1)
      call ncvgt(fileid,diagnostic_var_id,nc_corner,nc_edge,diagnostic_v
     &ar,nc_stat)
      diagnostic_tab_index_id=ncvid(fileid,'diagnostic_tab_index',nc_sta
     &t)
      call ncvinq(fileid,diagnostic_tab_index_id,nc_dummy,nc_type,nc_ran
     &k,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.diag_grp_ind_id)continue
      
      diagnostic_tab_index =>mem_alloc_i1((0),(sc_diagnostic_grps),'diag
     &nostic_tab_index')
      nc_corner(1)=1
      nc_edge(1)=((sc_diagnostic_grps)-(0)+1)
      call ncvgt(fileid,diagnostic_tab_index_id,nc_corner,nc_edge,diagno
     &stic_tab_index,nc_stat)
      diagnostic_min_id=ncvid(fileid,'diagnostic_min',nc_stat)
      call ncvinq(fileid,diagnostic_min_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.diag_grp_ind_id)continue
      
      diagnostic_min =>mem_alloc_r1((0),(sc_diagnostic_grps),'diagnostic
     &_min')
      nc_corner(1)=1
      nc_edge(1)=((sc_diagnostic_grps)-(0)+1)
      call ncvgt(fileid,diagnostic_min_id,nc_corner,nc_edge,diagnostic_m
     &in,nc_stat)
      diagnostic_delta_id=ncvid(fileid,'diagnostic_delta',nc_stat)
      call ncvinq(fileid,diagnostic_delta_id,nc_dummy,nc_type,nc_rank,nc
     &_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.diag_grp_ind_id)continue
      
      diagnostic_delta =>mem_alloc_r1((0),(sc_diagnostic_grps),'diagnost
     &ic_delta')
      nc_corner(1)=1
      nc_edge(1)=((sc_diagnostic_grps)-(0)+1)
      call ncvgt(fileid,diagnostic_delta_id,nc_corner,nc_edge,diagnostic
     &_delta,nc_stat)
      diagnostic_spacing_id=ncvid(fileid,'diagnostic_spacing',nc_stat)
      call ncvinq(fileid,diagnostic_spacing_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.diag_grp_ind_id)continue
      
      diagnostic_spacing =>mem_alloc_i1((0),(sc_diagnostic_grps),'diagno
     &stic_spacing')
      nc_corner(1)=1
      nc_edge(1)=((sc_diagnostic_grps)-(0)+1)
      call ncvgt(fileid,diagnostic_spacing_id,nc_corner,nc_edge,diagnost
     &ic_spacing,nc_stat)
      diagnostic_grp_base_id=ncvid(fileid,'diagnostic_grp_base',nc_stat)
      call ncvinq(fileid,diagnostic_grp_base_id,nc_dummy,nc_type,nc_rank
     &,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.diag_grp_ind_id)continue
      
      diagnostic_grp_base =>mem_alloc_i1((0),(sc_diagnostic_grps),'diagn
     &ostic_grp_base')
      nc_corner(1)=1
      nc_edge(1)=((sc_diagnostic_grps)-(0)+1)
      call ncvgt(fileid,diagnostic_grp_base_id,nc_corner,nc_edge,diagnos
     &tic_grp_base,nc_stat)
      sc_diag_size_id=ncvid(fileid,'sc_diag_size',nc_stat)
      call ncvinq(fileid,sc_diag_size_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      
      call ncvgt(fileid,sc_diag_size_id,nc_corner,nc_edge,sc_diag_size,n
     &c_stat)
      sc_diag_ind_id=ncdid(fileid,'sc_diag_ind',nc_stat)
      call ncdinq(fileid,sc_diag_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((sc_diag_size-1)-(0)+1))continue
      diagnostic_sector_tab_id=ncvid(fileid,'diagnostic_sector_tab',nc_s
     &tat)
      call ncvinq(fileid,diagnostic_sector_tab_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.sc_diag_ind_id)continue
      
      diagnostic_sector_tab =>mem_alloc_i1((0),(sc_diag_size-1),'diagnos
     &tic_sector_tab')
      nc_corner(1)=1
      nc_edge(1)=((sc_diag_size-1)-(0)+1)
      call ncvgt(fileid,diagnostic_sector_tab_id,nc_corner,nc_edge,diagn
     &ostic_sector_tab,nc_stat)
      
      de_symbol_string_id=ncdid(fileid,'de_symbol_string',nc_stat)
      call ncdinq(fileid,de_symbol_string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((24)-(1)+1))continue
      de_name_string_id=ncdid(fileid,'de_name_string',nc_stat)
      call ncdinq(fileid,de_name_string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((100)-(1)+1))continue
      de_grps_id=ncvid(fileid,'de_grps',nc_stat)
      call ncvinq(fileid,de_grps_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_
     &attr,nc_stat)
      
      call ncvgt(fileid,de_grps_id,nc_corner,nc_edge,de_grps,nc_stat)
      de_grp_ind_id=ncdid(fileid,'de_grp_ind',nc_stat)
      call ncdinq(fileid,de_grp_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((de_grps)-(0)+1))continue
      de_max_bins_id=ncvid(fileid,'de_max_bins',nc_stat)
      call ncvinq(fileid,de_max_bins_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      
      call ncvgt(fileid,de_max_bins_id,nc_corner,nc_edge,de_max_bins,nc_
     &stat)
      de_zone_frags_dim_id=ncvid(fileid,'de_zone_frags_dim',nc_stat)
      call ncvinq(fileid,de_zone_frags_dim_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,de_zone_frags_dim_id,nc_corner,nc_edge,de_zone_f
     &rags_dim,nc_stat)
      de_zone_frags_ind_id=ncdid(fileid,'de_zone_frags_ind',nc_stat)
      call ncdinq(fileid,de_zone_frags_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((de_zone_frags_dim)-(1)+1))continue
      de_zone_frags_size_id=ncvid(fileid,'de_zone_frags_size',nc_stat)
      call ncvinq(fileid,de_zone_frags_size_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,de_zone_frags_size_id,nc_corner,nc_edge,de_zone_
     &frags_size,nc_stat)
      detector_name_id=ncvid(fileid,'detector_name',nc_stat)
      call ncvinq(fileid,detector_name_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      if(nc_dims(1).EQ.de_name_string_id)continue
      if(nc_dims(2).EQ.de_grp_ind_id)continue
      
      detector_name =>mem_alloc_c1((100),(0),(de_grps),'detector_name')
      nc_corner(1)=1
      nc_edge(1)=((100)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((de_grps)-(0)+1)
      nc_stat=nf_get_vara_text(fileid,detector_name_id,nc_corner,nc_edge
     &,detector_name)
      detector_num_views_id=ncvid(fileid,'detector_num_views',nc_stat)
      call ncvinq(fileid,detector_num_views_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.de_grp_ind_id)continue
      
      detector_num_views =>mem_alloc_i1((0),(de_grps),'detector_num_view
     &s')
      nc_corner(1)=1
      nc_edge(1)=((de_grps)-(0)+1)
      call ncvgt(fileid,detector_num_views_id,nc_corner,nc_edge,detector
     &_num_views,nc_stat)
      detector_var_id=ncvid(fileid,'detector_var',nc_stat)
      call ncvinq(fileid,detector_var_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      if(nc_dims(1).EQ.de_grp_ind_id)continue
      
      detector_var =>mem_alloc_i1((0),(de_grps),'detector_var')
      nc_corner(1)=1
      nc_edge(1)=((de_grps)-(0)+1)
      call ncvgt(fileid,detector_var_id,nc_corner,nc_edge,detector_var,n
     &c_stat)
      detector_tab_index_id=ncvid(fileid,'detector_tab_index',nc_stat)
      call ncvinq(fileid,detector_tab_index_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.de_grp_ind_id)continue
      
      detector_tab_index =>mem_alloc_i1((0),(de_grps),'detector_tab_inde
     &x')
      nc_corner(1)=1
      nc_edge(1)=((de_grps)-(0)+1)
      call ncvgt(fileid,detector_tab_index_id,nc_corner,nc_edge,detector
     &_tab_index,nc_stat)
      detector_min_id=ncvid(fileid,'detector_min',nc_stat)
      call ncvinq(fileid,detector_min_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      if(nc_dims(1).EQ.de_grp_ind_id)continue
      
      detector_min =>mem_alloc_r1((0),(de_grps),'detector_min')
      nc_corner(1)=1
      nc_edge(1)=((de_grps)-(0)+1)
      call ncvgt(fileid,detector_min_id,nc_corner,nc_edge,detector_min,n
     &c_stat)
      detector_delta_id=ncvid(fileid,'detector_delta',nc_stat)
      call ncvinq(fileid,detector_delta_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.de_grp_ind_id)continue
      
      detector_delta =>mem_alloc_r1((0),(de_grps),'detector_delta')
      nc_corner(1)=1
      nc_edge(1)=((de_grps)-(0)+1)
      call ncvgt(fileid,detector_delta_id,nc_corner,nc_edge,detector_del
     &ta,nc_stat)
      detector_spacing_id=ncvid(fileid,'detector_spacing',nc_stat)
      call ncvinq(fileid,detector_spacing_id,nc_dummy,nc_type,nc_rank,nc
     &_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.de_grp_ind_id)continue
      
      detector_spacing =>mem_alloc_i1((0),(de_grps),'detector_spacing')
      nc_corner(1)=1
      nc_edge(1)=((de_grps)-(0)+1)
      call ncvgt(fileid,detector_spacing_id,nc_corner,nc_edge,detector_s
     &pacing,nc_stat)
      detector_total_views_id=ncvid(fileid,'detector_total_views',nc_sta
     &t)
      call ncvinq(fileid,detector_total_views_id,nc_dummy,nc_type,nc_ran
     &k,nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,detector_total_views_id,nc_corner,nc_edge,detect
     &or_total_views,nc_stat)
      de_tot_view_ind_id=ncdid(fileid,'de_tot_view_ind',nc_stat)
      call ncdinq(fileid,de_tot_view_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((detector_total_views)-(0)+1))continue
      de_start_end_ind_id=ncdid(fileid,'de_start_end_ind',nc_stat)
      call ncdinq(fileid,de_start_end_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((1)-(0)+1))continue
      de_view_points_id=ncvid(fileid,'de_view_points',nc_stat)
      call ncvinq(fileid,de_view_points_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.vector_id)continue
      if(nc_dims(2).EQ.de_start_end_ind_id)continue
      if(nc_dims(3).EQ.de_tot_view_ind_id)continue
      
      de_view_points =>mem_alloc_r3((1),(3),(0),(1),(0),(detector_total_
     &views),'de_view_points')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((1)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((detector_total_views)-(0)+1)
      call ncvgt(fileid,de_view_points_id,nc_corner,nc_edge,de_view_poin
     &ts,nc_stat)
      de_view_algorithm_id=ncvid(fileid,'de_view_algorithm',nc_stat)
      call ncvinq(fileid,de_view_algorithm_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.de_tot_view_ind_id)continue
      
      de_view_algorithm =>mem_alloc_i1((0),(detector_total_views),'de_vi
     &ew_algorithm')
      nc_corner(1)=1
      nc_edge(1)=((detector_total_views)-(0)+1)
      call ncvgt(fileid,de_view_algorithm_id,nc_corner,nc_edge,de_view_a
     &lgorithm,nc_stat)
      de_view_halfwidth_id=ncvid(fileid,'de_view_halfwidth',nc_stat)
      call ncvinq(fileid,de_view_halfwidth_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.de_tot_view_ind_id)continue
      
      de_view_halfwidth =>mem_alloc_r1((0),(detector_total_views),'de_vi
     &ew_halfwidth')
      nc_corner(1)=1
      nc_edge(1)=((detector_total_views)-(0)+1)
      call ncvgt(fileid,de_view_halfwidth_id,nc_corner,nc_edge,de_view_h
     &alfwidth,nc_stat)
      de_zone_frags_id=ncvid(fileid,'de_zone_frags',nc_stat)
      call ncvinq(fileid,de_zone_frags_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      if(nc_dims(1).EQ.de_zone_frags_ind_id)continue
      
      de_zone_frags =>mem_alloc_r1((1),(de_zone_frags_dim),'de_zone_frag
     &s')
      nc_corner(1)=1
      nc_edge(1)=((de_zone_frags_dim)-(1)+1)
      call ncvgt(fileid,de_zone_frags_id,nc_corner,nc_edge,de_zone_frags
     &,nc_stat)
      de_zone_frags_start_id=ncvid(fileid,'de_zone_frags_start',nc_stat)
      call ncvinq(fileid,de_zone_frags_start_id,nc_dummy,nc_type,nc_rank
     &,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.de_tot_view_ind_id)continue
      
      de_zone_frags_start =>mem_alloc_i1((0),(detector_total_views),'de_
     &zone_frags_start')
      nc_corner(1)=1
      nc_edge(1)=((detector_total_views)-(0)+1)
      call ncvgt(fileid,de_zone_frags_start_id,nc_corner,nc_edge,de_zone
     &_frags_start,nc_stat)
      de_zone_frags_num_id=ncvid(fileid,'de_zone_frags_num',nc_stat)
      call ncvinq(fileid,de_zone_frags_num_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.de_tot_view_ind_id)continue
      
      de_zone_frags_num =>mem_alloc_i1((0),(detector_total_views),'de_zo
     &ne_frags_num')
      nc_corner(1)=1
      nc_edge(1)=((detector_total_views)-(0)+1)
      call ncvgt(fileid,de_zone_frags_num_id,nc_corner,nc_edge,de_zone_f
     &rags_num,nc_stat)
      de_zone_frags_zones_id=ncvid(fileid,'de_zone_frags_zones',nc_stat)
      call ncvinq(fileid,de_zone_frags_zones_id,nc_dummy,nc_type,nc_rank
     &,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.de_zone_frags_ind_id)continue
      
      de_zone_frags_zones =>mem_alloc_i1((1),(de_zone_frags_dim),'de_zon
     &e_frags_zones')
      nc_corner(1)=1
      nc_edge(1)=((de_zone_frags_dim)-(1)+1)
      call ncvgt(fileid,de_zone_frags_zones_id,nc_corner,nc_edge,de_zone
     &_frags_zones,nc_stat)
      de_zone_frags_min_zn_id=ncvid(fileid,'de_zone_frags_min_zn',nc_sta
     &t)
      call ncvinq(fileid,de_zone_frags_min_zn_id,nc_dummy,nc_type,nc_ran
     &k,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.de_tot_view_ind_id)continue
      
      de_zone_frags_min_zn =>mem_alloc_i1((0),(detector_total_views),'de
     &_zone_frags_min_zn')
      nc_corner(1)=1
      nc_edge(1)=((detector_total_views)-(0)+1)
      call ncvgt(fileid,de_zone_frags_min_zn_id,nc_corner,nc_edge,de_zon
     &e_frags_min_zn,nc_stat)
      de_zone_frags_max_zn_id=ncvid(fileid,'de_zone_frags_max_zn',nc_sta
     &t)
      call ncvinq(fileid,de_zone_frags_max_zn_id,nc_dummy,nc_type,nc_ran
     &k,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.de_tot_view_ind_id)continue
      
      de_zone_frags_max_zn =>mem_alloc_i1((0),(detector_total_views),'de
     &_zone_frags_max_zn')
      nc_corner(1)=1
      nc_edge(1)=((detector_total_views)-(0)+1)
      call ncvgt(fileid,de_zone_frags_max_zn_id,nc_corner,nc_edge,de_zon
     &e_frags_max_zn,nc_stat)
      de_view_base_id=ncvid(fileid,'de_view_base',nc_stat)
      call ncvinq(fileid,de_view_base_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      if(nc_dims(1).EQ.de_grp_ind_id)continue
      
      de_view_base =>mem_alloc_i1((0),(de_grps),'de_view_base')
      nc_corner(1)=1
      nc_edge(1)=((de_grps)-(0)+1)
      call ncvgt(fileid,de_view_base_id,nc_corner,nc_edge,de_view_base,n
     &c_stat)
      de_view_size_id=ncvid(fileid,'de_view_size',nc_stat)
      call ncvinq(fileid,de_view_size_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      
      call ncvgt(fileid,de_view_size_id,nc_corner,nc_edge,de_view_size,n
     &c_stat)
      de_view_ind_id=ncdid(fileid,'de_view_ind',nc_stat)
      call ncdinq(fileid,de_view_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((de_view_size-1)-(0)+1))continue
      de_view_tab_id=ncvid(fileid,'de_view_tab',nc_stat)
      call ncvinq(fileid,de_view_tab_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      if(nc_dims(1).EQ.de_view_ind_id)continue
      
      de_view_tab =>mem_alloc_i1((0),(de_view_size-1),'de_view_tab')
      nc_corner(1)=1
      nc_edge(1)=((de_view_size-1)-(0)+1)
      call ncvgt(fileid,de_view_tab_id,nc_corner,nc_edge,de_view_tab,nc_
     &stat)
      
      call ncclos(fileid,nc_stat)
      endif
      
      call MPI_bcast(ncells,1,MPI_INTEGER,mpi_degas2_root,comm_world_dup
     &,mpi_err)
      call MPI_bcast(nsurfaces,1,MPI_INTEGER,mpi_degas2_root,comm_world_
     &dup,mpi_err)
      call MPI_bcast(nboundaries,1,MPI_INTEGER,mpi_degas2_root,comm_worl
     &d_dup,mpi_err)
      call MPI_bcast(nneighbors,1,MPI_INTEGER,mpi_degas2_root,comm_world
     &_dup,mpi_err)
      call MPI_bcast(ntransforms,1,MPI_INTEGER,mpi_degas2_root,comm_worl
     &d_dup,mpi_err)
      call MPI_bcast(geometry_symmetry,1,MPI_INTEGER,mpi_degas2_root,com
     &m_world_dup,mpi_err)
      call MPI_bcast(universal_cell_min,(((3)-(1)+1)),MPI_DOUBLE_PRECISI
     &ON,mpi_degas2_root,comm_world_dup,mpi_err)
      call MPI_bcast(universal_cell_max,(((3)-(1)+1)),MPI_DOUBLE_PRECISI
     &ON,mpi_degas2_root,comm_world_dup,mpi_err)
      call MPI_bcast(universal_cell_vol,1,MPI_DOUBLE_PRECISION,mpi_degas
     &2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      cells =>mem_alloc_i2((1),(4),(0),(ncells),'cells')
      endif
      call MPI_bcast(cells,(((4)-(1)+1)*((ncells)-(0)+1)),MPI_INTEGER,mp
     &i_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      surfaces =>mem_alloc_i3((0),(1),(0),(1),(1),(nsurfaces),'surfaces'
     &)
      endif
      call MPI_bcast(surfaces,(((1)-(0)+1)*((1)-(0)+1)*((nsurfaces)-(1)+
     &1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      surfaces_tx_ind =>mem_alloc_i3((0),(1),(0),(1),(1),(nsurfaces),'su
     &rfaces_tx_ind')
      endif
      call MPI_bcast(surfaces_tx_ind,(((1)-(0)+1)*((1)-(0)+1)*((nsurface
     &s)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      surfaces_tx_mx =>mem_alloc_r3((1),(3),(1),(4),(0),(ntransforms),'s
     &urfaces_tx_mx')
      endif
      call MPI_bcast(surfaces_tx_mx,(((3)-(1)+1)*((4)-(1)+1)*((ntransfor
     &ms)-(0)+1)),MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mp
     &i_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      surface_sectors =>mem_alloc_i3((0),(1),(0),(1),(1),(nsurfaces),'su
     &rface_sectors')
      endif
      call MPI_bcast(surface_sectors,(((1)-(0)+1)*((1)-(0)+1)*((nsurface
     &s)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      boundaries =>mem_alloc_i1((1),(nboundaries),'boundaries')
      endif
      call MPI_bcast(boundaries,(((nboundaries)-(1)+1)),MPI_INTEGER,mpi_
     &degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      neighbors =>mem_alloc_i1((0),(nneighbors),'neighbors')
      endif
      call MPI_bcast(neighbors,(((nneighbors)-(0)+1)),MPI_INTEGER,mpi_de
     &gas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      surface_coeffs =>mem_alloc_r2((1),(10),(1),(nsurfaces),'surface_co
     &effs')
      endif
      call MPI_bcast(surface_coeffs,(((10)-(1)+1)*((nsurfaces)-(1)+1)),M
     &PI_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      surface_points =>mem_alloc_r3((1),(3),(0),(1),(1),(nsurfaces),'sur
     &face_points')
      endif
      call MPI_bcast(surface_points,(((3)-(1)+1)*((1)-(0)+1)*((nsurfaces
     &)-(1)+1)),MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_
     &err)
      
      call MPI_bcast(zn_num,1,MPI_INTEGER,mpi_degas2_root,comm_world_dup
     &,mpi_err)
      call MPI_bcast(zone_type_num,(((4)-(1)+1)),MPI_INTEGER,mpi_degas2_
     &root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      zone_type =>mem_alloc_i1((1),(zn_num),'zone_type')
      endif
      call MPI_bcast(zone_type,(((zn_num)-(1)+1)),MPI_INTEGER,mpi_degas2
     &_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      zone_index =>mem_alloc_i2((1),(4),(1),(zn_num),'zone_index')
      endif
      call MPI_bcast(zone_index,(((4)-(1)+1)*((zn_num)-(1)+1)),MPI_INTEG
     &ER,mpi_degas2_root,comm_world_dup,mpi_err)
      call MPI_bcast(zone_index_min,(((4)-(1)+1)),MPI_INTEGER,mpi_degas2
     &_root,comm_world_dup,mpi_err)
      call MPI_bcast(zone_index_max,(((4)-(1)+1)),MPI_INTEGER,mpi_degas2
     &_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      zone_pointer =>mem_alloc_i1((1),(zn_num),'zone_pointer')
      endif
      call MPI_bcast(zone_pointer,(((zn_num)-(1)+1)),MPI_INTEGER,mpi_deg
     &as2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      zone_volume =>mem_alloc_r1((1),(zn_num),'zone_volume')
      endif
      call MPI_bcast(zone_volume,(((zn_num)-(1)+1)),MPI_DOUBLE_PRECISION
     &,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      zone_center =>mem_alloc_r2((1),(3),(1),(zn_num),'zone_center')
      endif
      call MPI_bcast(zone_center,(((3)-(1)+1)*((zn_num)-(1)+1)),MPI_DOUB
     &LE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      zone_min =>mem_alloc_r2((1),(3),(1),(zn_num),'zone_min')
      endif
      call MPI_bcast(zone_min,(((3)-(1)+1)*((zn_num)-(1)+1)),MPI_DOUBLE_
     &PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      zone_max =>mem_alloc_r2((1),(3),(1),(zn_num),'zone_max')
      endif
      call MPI_bcast(zone_max,(((3)-(1)+1)*((zn_num)-(1)+1)),MPI_DOUBLE_
     &PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      
      call MPI_bcast(nsectors,1,MPI_INTEGER,mpi_degas2_root,comm_world_d
     &up,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      strata =>mem_alloc_i1((0),(nsectors),'strata')
      endif
      call MPI_bcast(strata,(((nsectors)-(0)+1)),MPI_INTEGER,mpi_degas2_
     &root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      sector_strata_segment =>mem_alloc_i1((0),(nsectors),'sector_strata
     &_segment')
      endif
      call MPI_bcast(sector_strata_segment,(((nsectors)-(0)+1)),MPI_INTE
     &GER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      sectors =>mem_alloc_i1((0),(nsectors),'sectors')
      endif
      call MPI_bcast(sectors,(((nsectors)-(0)+1)),MPI_INTEGER,mpi_degas2
     &_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      sector_zone =>mem_alloc_i1((0),(nsectors),'sector_zone')
      endif
      call MPI_bcast(sector_zone,(((nsectors)-(0)+1)),MPI_INTEGER,mpi_de
     &gas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      sector_surface =>mem_alloc_i1((0),(nsectors),'sector_surface')
      endif
      call MPI_bcast(sector_surface,(((nsectors)-(0)+1)),MPI_INTEGER,mpi
     &_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      sector_points =>mem_alloc_r3((1),(3),(0),(1),(0),(nsectors),'secto
     &r_points')
      endif
      call MPI_bcast(sector_points,(((3)-(1)+1)*((1)-(0)+1)*((nsectors)-
     &(0)+1)),MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_er
     &r)
      if(mpi_rank.NE.mpi_degas2_root)then
      sector_type_pointer =>mem_alloc_i2((1),(17),(0),(nsectors),'sector
     &_type_pointer')
      endif
      call MPI_bcast(sector_type_pointer,(((17)-(1)+1)*((nsectors)-(0)+1
     &)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      call MPI_bcast(sc_vacuum_num,1,MPI_INTEGER,mpi_degas2_root,comm_wo
     &rld_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      vacuum_sector =>mem_alloc_i1((0),(sc_vacuum_num),'vacuum_sector')
      endif
      call MPI_bcast(vacuum_sector,(((sc_vacuum_num)-(0)+1)),MPI_INTEGER
     &,mpi_degas2_root,comm_world_dup,mpi_err)
      call MPI_bcast(sc_plasma_num,1,MPI_INTEGER,mpi_degas2_root,comm_wo
     &rld_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      plasma_sector =>mem_alloc_i1((0),(sc_plasma_num),'plasma_sector')
      endif
      call MPI_bcast(plasma_sector,(((sc_plasma_num)-(0)+1)),MPI_INTEGER
     &,mpi_degas2_root,comm_world_dup,mpi_err)
      call MPI_bcast(sc_target_num,1,MPI_INTEGER,mpi_degas2_root,comm_wo
     &rld_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      target_sector =>mem_alloc_i1((0),(sc_target_num),'target_sector')
      endif
      call MPI_bcast(target_sector,(((sc_target_num)-(0)+1)),MPI_INTEGER
     &,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      target_material =>mem_alloc_i1((0),(sc_target_num),'target_materia
     &l')
      endif
      call MPI_bcast(target_material,(((sc_target_num)-(0)+1)),MPI_INTEG
     &ER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      target_temperature =>mem_alloc_r1((0),(sc_target_num),'target_temp
     &erature')
      endif
      call MPI_bcast(target_temperature,(((sc_target_num)-(0)+1)),MPI_DO
     &UBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      target_recyc_coef =>mem_alloc_r1((0),(sc_target_num),'target_recyc
     &_coef')
      endif
      call MPI_bcast(target_recyc_coef,(((sc_target_num)-(0)+1)),MPI_DOU
     &BLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      call MPI_bcast(sc_wall_num,1,MPI_INTEGER,mpi_degas2_root,comm_worl
     &d_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      wall_sector =>mem_alloc_i1((0),(sc_wall_num),'wall_sector')
      endif
      call MPI_bcast(wall_sector,(((sc_wall_num)-(0)+1)),MPI_INTEGER,mpi
     &_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      wall_material =>mem_alloc_i1((0),(sc_wall_num),'wall_material')
      endif
      call MPI_bcast(wall_material,(((sc_wall_num)-(0)+1)),MPI_INTEGER,m
     &pi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      wall_temperature =>mem_alloc_r1((0),(sc_wall_num),'wall_temperatur
     &e')
      endif
      call MPI_bcast(wall_temperature,(((sc_wall_num)-(0)+1)),MPI_DOUBLE
     &_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      wall_recyc_coef =>mem_alloc_r1((0),(sc_wall_num),'wall_recyc_coef'
     &)
      endif
      call MPI_bcast(wall_recyc_coef,(((sc_wall_num)-(0)+1)),MPI_DOUBLE_
     &PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      call MPI_bcast(sc_exit_num,1,MPI_INTEGER,mpi_degas2_root,comm_worl
     &d_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      exit_sector =>mem_alloc_i1((0),(sc_exit_num),'exit_sector')
      endif
      call MPI_bcast(exit_sector,(((sc_exit_num)-(0)+1)),MPI_INTEGER,mpi
     &_degas2_root,comm_world_dup,mpi_err)
      call MPI_bcast(sc_diagnostic_grps,1,MPI_INTEGER,mpi_degas2_root,co
     &mm_world_dup,mpi_err)
      call MPI_bcast(sc_diag_max_bins,1,MPI_INTEGER,mpi_degas2_root,comm
     &_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      diagnostic_grp_name =>mem_alloc_c1((40),(0),(sc_diagnostic_grps),'
     &diagnostic_grp_name')
      endif
      call MPI_bcast(diagnostic_grp_name,(((40)-(1)+1)*((sc_diagnostic_g
     &rps)-(0)+1)),MPI_CHARACTER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      diagnostic_num_sectors =>mem_alloc_i1((0),(sc_diagnostic_grps),'di
     &agnostic_num_sectors')
      endif
      call MPI_bcast(diagnostic_num_sectors,(((sc_diagnostic_grps)-(0)+1
     &)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      diagnostic_var =>mem_alloc_i1((0),(sc_diagnostic_grps),'diagnostic
     &_var')
      endif
      call MPI_bcast(diagnostic_var,(((sc_diagnostic_grps)-(0)+1)),MPI_I
     &NTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      diagnostic_tab_index =>mem_alloc_i1((0),(sc_diagnostic_grps),'diag
     &nostic_tab_index')
      endif
      call MPI_bcast(diagnostic_tab_index,(((sc_diagnostic_grps)-(0)+1))
     &,MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      diagnostic_min =>mem_alloc_r1((0),(sc_diagnostic_grps),'diagnostic
     &_min')
      endif
      call MPI_bcast(diagnostic_min,(((sc_diagnostic_grps)-(0)+1)),MPI_D
     &OUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      diagnostic_delta =>mem_alloc_r1((0),(sc_diagnostic_grps),'diagnost
     &ic_delta')
      endif
      call MPI_bcast(diagnostic_delta,(((sc_diagnostic_grps)-(0)+1)),MPI
     &_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      diagnostic_spacing =>mem_alloc_i1((0),(sc_diagnostic_grps),'diagno
     &stic_spacing')
      endif
      call MPI_bcast(diagnostic_spacing,(((sc_diagnostic_grps)-(0)+1)),M
     &PI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      diagnostic_grp_base =>mem_alloc_i1((0),(sc_diagnostic_grps),'diagn
     &ostic_grp_base')
      endif
      call MPI_bcast(diagnostic_grp_base,(((sc_diagnostic_grps)-(0)+1)),
     &MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      call MPI_bcast(sc_diag_size,1,MPI_INTEGER,mpi_degas2_root,comm_wor
     &ld_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      diagnostic_sector_tab =>mem_alloc_i1((0),(sc_diag_size-1),'diagnos
     &tic_sector_tab')
      endif
      call MPI_bcast(diagnostic_sector_tab,(((sc_diag_size-1)-(0)+1)),MP
     &I_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      
      call MPI_bcast(de_grps,1,MPI_INTEGER,mpi_degas2_root,comm_world_du
     &p,mpi_err)
      call MPI_bcast(de_max_bins,1,MPI_INTEGER,mpi_degas2_root,comm_worl
     &d_dup,mpi_err)
      call MPI_bcast(de_zone_frags_dim,1,MPI_INTEGER,mpi_degas2_root,com
     &m_world_dup,mpi_err)
      call MPI_bcast(de_zone_frags_size,1,MPI_INTEGER,mpi_degas2_root,co
     &mm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      detector_name =>mem_alloc_c1((100),(0),(de_grps),'detector_name')
      endif
      call MPI_bcast(detector_name,(((100)-(1)+1)*((de_grps)-(0)+1)),MPI
     &_CHARACTER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      detector_num_views =>mem_alloc_i1((0),(de_grps),'detector_num_view
     &s')
      endif
      call MPI_bcast(detector_num_views,(((de_grps)-(0)+1)),MPI_INTEGER,
     &mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      detector_var =>mem_alloc_i1((0),(de_grps),'detector_var')
      endif
      call MPI_bcast(detector_var,(((de_grps)-(0)+1)),MPI_INTEGER,mpi_de
     &gas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      detector_tab_index =>mem_alloc_i1((0),(de_grps),'detector_tab_inde
     &x')
      endif
      call MPI_bcast(detector_tab_index,(((de_grps)-(0)+1)),MPI_INTEGER,
     &mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      detector_min =>mem_alloc_r1((0),(de_grps),'detector_min')
      endif
      call MPI_bcast(detector_min,(((de_grps)-(0)+1)),MPI_DOUBLE_PRECISI
     &ON,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      detector_delta =>mem_alloc_r1((0),(de_grps),'detector_delta')
      endif
      call MPI_bcast(detector_delta,(((de_grps)-(0)+1)),MPI_DOUBLE_PRECI
     &SION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      detector_spacing =>mem_alloc_i1((0),(de_grps),'detector_spacing')
      endif
      call MPI_bcast(detector_spacing,(((de_grps)-(0)+1)),MPI_INTEGER,mp
     &i_degas2_root,comm_world_dup,mpi_err)
      call MPI_bcast(detector_total_views,1,MPI_INTEGER,mpi_degas2_root,
     &comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      de_view_points =>mem_alloc_r3((1),(3),(0),(1),(0),(detector_total_
     &views),'de_view_points')
      endif
      call MPI_bcast(de_view_points,(((3)-(1)+1)*((1)-(0)+1)*((detector_
     &total_views)-(0)+1)),MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_wor
     &ld_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      de_view_algorithm =>mem_alloc_i1((0),(detector_total_views),'de_vi
     &ew_algorithm')
      endif
      call MPI_bcast(de_view_algorithm,(((detector_total_views)-(0)+1)),
     &MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      de_view_halfwidth =>mem_alloc_r1((0),(detector_total_views),'de_vi
     &ew_halfwidth')
      endif
      call MPI_bcast(de_view_halfwidth,(((detector_total_views)-(0)+1)),
     &MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      de_zone_frags =>mem_alloc_r1((1),(de_zone_frags_dim),'de_zone_frag
     &s')
      endif
      call MPI_bcast(de_zone_frags,(((de_zone_frags_dim)-(1)+1)),MPI_DOU
     &BLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      de_zone_frags_start =>mem_alloc_i1((0),(detector_total_views),'de_
     &zone_frags_start')
      endif
      call MPI_bcast(de_zone_frags_start,(((detector_total_views)-(0)+1)
     &),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      de_zone_frags_num =>mem_alloc_i1((0),(detector_total_views),'de_zo
     &ne_frags_num')
      endif
      call MPI_bcast(de_zone_frags_num,(((detector_total_views)-(0)+1)),
     &MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      de_zone_frags_zones =>mem_alloc_i1((1),(de_zone_frags_dim),'de_zon
     &e_frags_zones')
      endif
      call MPI_bcast(de_zone_frags_zones,(((de_zone_frags_dim)-(1)+1)),M
     &PI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      de_zone_frags_min_zn =>mem_alloc_i1((0),(detector_total_views),'de
     &_zone_frags_min_zn')
      endif
      call MPI_bcast(de_zone_frags_min_zn,(((detector_total_views)-(0)+1
     &)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      de_zone_frags_max_zn =>mem_alloc_i1((0),(detector_total_views),'de
     &_zone_frags_max_zn')
      endif
      call MPI_bcast(de_zone_frags_max_zn,(((detector_total_views)-(0)+1
     &)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      de_view_base =>mem_alloc_i1((0),(de_grps),'de_view_base')
      endif
      call MPI_bcast(de_view_base,(((de_grps)-(0)+1)),MPI_INTEGER,mpi_de
     &gas2_root,comm_world_dup,mpi_err)
      call MPI_bcast(de_view_size,1,MPI_INTEGER,mpi_degas2_root,comm_wor
     &ld_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      de_view_tab =>mem_alloc_i1((0),(de_view_size-1),'de_view_tab')
      endif
      call MPI_bcast(de_view_tab,(((de_view_size-1)-(0)+1)),MPI_INTEGER,
     &mpi_degas2_root,comm_world_dup,mpi_err)
      
      return
      end
      subroutine transform(m,x,y)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)m(3,4),x(3)
      REAL(kind=DOUBLE)y(3)
      integer i
      REAL(kind=DOUBLE)temp(3)
      do i=1,3
      temp(i)=m(i,1)*x(1)+m(i,2)*x(2)+m(i,3)*x(3)+m(i,4)
      end do
      do i=1,3
      y(i)=temp(i)
      end do
      return
      end
      subroutine transform_velocity(m,v,w)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)m(3,4),v(3)
      REAL(kind=DOUBLE)w(3)
      integer i
      REAL(kind=DOUBLE)temp(3)
      do i=1,3
      temp(i)=m(i,1)*v(1)+m(i,2)*v(2)+m(i,3)*v(3)
      end do
      do i=1,3
      w(i)=temp(i)
      end do
      return
      end
      
      
