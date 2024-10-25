      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      subroutine unit_plane(coeff)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)coeff(10)
      integer i
      do i=1,10
      coeff(i)=(0.0_DOUBLE)
      end do
      coeff(4)=(1.0_DOUBLE)
      return
      end
      subroutine unit_sphere(coeff)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)coeff(10)
      integer i
      do i=1,10
      coeff(i)=(0.0_DOUBLE)
      end do
      coeff(1)=-(1.0_DOUBLE)
      coeff(5)=(1.0_DOUBLE)
      coeff(6)=(1.0_DOUBLE)
      coeff(7)=(1.0_DOUBLE)
      return
      end
      subroutine unit_cylinder(coeff)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)coeff(10)
      integer i
      do i=1,10
      coeff(i)=(0.0_DOUBLE)
      end do
      coeff(1)=-(1.0_DOUBLE)
      coeff(5)=(1.0_DOUBLE)
      coeff(6)=(1.0_DOUBLE)
      return
      end
      subroutine unit_cone(coeff)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)coeff(10)
      integer i
      do i=1,10
      coeff(i)=(0.0_DOUBLE)
      end do
      coeff(5)=(1.0_DOUBLE)
      coeff(6)=(1.0_DOUBLE)
      coeff(7)=-(1.0_DOUBLE)
      return
      end
      subroutine plane(x0,a,coeff)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)x0(3),a(3)
      REAL(kind=DOUBLE)coeff(10)
      REAL(kind=DOUBLE)m(3,4)
      external geom_translate,geom_rotatea,unit_plane,init_identity
      call unit_plane(coeff)
      call init_identity(m)
      call geom_translate(x0,m)
      call geom_rotatea(a,m)
      call transform_face(coeff,m,coeff)
      return
      end
      subroutine sphere(x0,r,coeff)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)x0(3),r
      REAL(kind=DOUBLE)coeff(10)
      REAL(kind=DOUBLE)m(3,4)
      external geom_scale,geom_translate,unit_sphere,init_identity
      if(r.GT.(2.0_DOUBLE)*(1.0e-8_DOUBLE))continue
      call unit_sphere(coeff)
      call init_identity(m)
      call geom_translate(x0,m)
      call geom_scale(r,m)
      call transform_face(coeff,m,coeff)
      return
      end
      subroutine planea(x1,x2,x3,coeff)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)x1(3),x2(3),x3(3)
      REAL(kind=DOUBLE)coeff(10)
      REAL(kind=DOUBLE)t(3,3)
      REAL(kind=DOUBLE)vector_temp(3)
      integer i,k,l,comp,orientation
      external vector_compare
      integer vector_compare
      t(1,1)=x1(1)
      t(2,1)=x1(2)
      t(3,1)=x1(3)
      
      t(1,2)=x2(1)
      t(2,2)=x2(2)
      t(3,2)=x2(3)
      
      t(1,3)=x3(1)
      t(2,3)=x3(2)
      t(3,3)=x3(3)
      
      orientation=1
      do k=2,1,-1
      do l=1,k
      comp=vector_compare(t(1,l),t(1,l+1))
      if(comp.NE.0)continue
      if(comp.GT.0)then
      vector_temp(1)=t(1,l)
      t(1,l)=t(1,l+1)
      t(1,l+1)=vector_temp(1)
      vector_temp(2)=t(2,l)
      t(2,l)=t(2,l+1)
      t(2,l+1)=vector_temp(2)
      vector_temp(3)=t(3,l)
      t(3,l)=t(3,l+1)
      t(3,l+1)=vector_temp(3)
      
      orientation=-orientation
      end if
      end do
      end do
      if(vector_compare(t(1,1),t(1,2)).LT.0.AND.vector_compare(t(1,2),t(
     &1,3)).LT.0)continue
      t(1,2)=t(1,2)-t(1,1)
      t(2,2)=t(2,2)-t(2,1)
      t(3,2)=t(3,2)-t(3,1)
      
      t(1,3)=t(1,3)-t(1,1)
      t(2,3)=t(2,3)-t(2,1)
      t(3,3)=t(3,3)-t(3,1)
      
      vector_temp(1)=sqrt((t(1,2)**2+t(2,2)**2+t(3,2)**2))
      if(vector_temp(1).GT.(0.0_DOUBLE))continue
      t(1,2)=((1.0_DOUBLE)/vector_temp(1))*t(1,2)
      t(2,2)=((1.0_DOUBLE)/vector_temp(1))*t(2,2)
      t(3,2)=((1.0_DOUBLE)/vector_temp(1))*t(3,2)
      
      vector_temp(1)=sqrt((t(1,3)**2+t(2,3)**2+t(3,3)**2))
      if(vector_temp(1).GT.(0.0_DOUBLE))continue
      t(1,3)=((1.0_DOUBLE)/vector_temp(1))*t(1,3)
      t(2,3)=((1.0_DOUBLE)/vector_temp(1))*t(2,3)
      t(3,3)=((1.0_DOUBLE)/vector_temp(1))*t(3,3)
      
      vector_temp(1)=t(2,2)*t(3,3)-t(3,2)*t(2,3)
      vector_temp(2)=t(3,2)*t(1,3)-t(1,2)*t(3,3)
      t(3,2)=t(1,2)*t(2,3)-t(2,2)*t(1,3)
      t(1,2)=vector_temp(1)
      t(2,2)=vector_temp(2)
      if(sqrt((t(1,2)**2+t(2,2)**2+t(3,2)**2)).GE.(1.0e-10_DOUBLE))conti
     &nue
      call plane(t(1,1),t(1,2),coeff)
      if(orientation.LT.0)then
      do i=1,10
      coeff(i)=-coeff(i)
      end do
      end if
      
      return
      end
      subroutine cylinder(x0,a,r,coeff)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)x0(3),a(3),r
      REAL(kind=DOUBLE)coeff(10)
      REAL(kind=DOUBLE)m(3,4)
      REAL(kind=DOUBLE)vector_temp(3)
      external unit_cylinder,init_identity,geom_translate,geom_rotatea
      if(r.GT.(2.0_DOUBLE)*(1.0e-8_DOUBLE))continue
      call unit_cylinder(coeff)
      call init_identity(m)
      call geom_translate(x0,m)
      call geom_rotatea(a,m)
      call geom_scale(r,m)
      call transform_face(coeff,m,coeff)
      return
      end
      subroutine conea(x1,x2,coeff,surf_type,apex)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)x1(3),x2(3)
      REAL(kind=DOUBLE)coeff(10),apex(3)
      integer surf_type
      REAL(kind=DOUBLE)x0(3),a(3),m(3,4),xa1(3),xa2(3),c(3),slope
      integer orientation,i
      REAL(kind=DOUBLE)vector_temp(3)
      external unit_cone,init_identity,geom_translate,geom_scalea,transf
     &orm,transform_face,vector_compare
      integer vector_compare
      orientation=vector_compare(x1,x2)
      if(orientation.NE.0)continue
      do i=1,3
      xa1(i)=x1(i)
      xa2(i)=x2(i)
      end do
      if(orientation.LT.0)then
      vector_temp(1)=xa1(1)
      xa1(1)=xa2(1)
      xa2(1)=vector_temp(1)
      vector_temp(2)=xa1(2)
      xa1(2)=xa2(2)
      xa2(2)=vector_temp(2)
      vector_temp(3)=xa1(3)
      xa1(3)=xa2(3)
      xa2(3)=vector_temp(3)
      
      end if
      xa1(1)=sqrt(xa1(1)**2+xa1(2)**2)
      xa1(2)=(0.0_DOUBLE)
      xa2(1)=sqrt(xa2(1)**2+xa2(2)**2)
      xa2(2)=(0.0_DOUBLE)
      a(1)=(0.0_DOUBLE)
      a(2)=(0.0_DOUBLE)
      a(3)=(1.0_DOUBLE)
      if(sqrt((xa1(1)-xa2(1))**2+(xa1(3)-xa2(3))**2).GT.(2.0_DOUBLE)*(1.
     &0e-8_DOUBLE))continue
      if(abs(xa1(3)-xa2(3)).LE.min((1.0e-8_DOUBLE),sqrt((2.0_DOUBLE)*(1.
     &0e-10_DOUBLE))*abs(xa1(1)-xa2(1))))then
      c(1)=(0.0_DOUBLE)
      c(2)=(0.0_DOUBLE)
      c(3)=(0.5_DOUBLE)*(x1(3)+x2(3))
      call plane(c,a,coeff)
      if(xa1(1).LT.xa2(1))orientation=-orientation
      surf_type=0
      apex(1)=(0.0_DOUBLE)
      apex(2)=(0.0_DOUBLE)
      apex(3)=c(3)
      else if(abs(xa1(1)-xa2(1)).LE.min((1.0e-8_DOUBLE),sqrt((2.0_DOUBLE
     &)*(1.0e-10_DOUBLE))*abs(xa1(3)-xa2(3))))then
      x0(1)=(0.0_DOUBLE)
      x0(2)=(0.0_DOUBLE)
      x0(3)=(0.0_DOUBLE)
      call cylinder(x0,a,(0.5_DOUBLE)*(xa1(1)+xa2(1)),coeff)
      if(xa2(3).LT.xa1(3))orientation=-orientation
      surf_type=2
      apex(1)=(0.0_DOUBLE)
      apex(2)=(0.0_DOUBLE)
      apex(3)=(1.0e16_DOUBLE)
      else
      slope=(xa2(3)-xa1(3))/(xa2(1)-xa1(1))
      apex(1)=(0.0_DOUBLE)
      apex(2)=(0.0_DOUBLE)
      apex(3)=xa1(3)-slope*xa1(1)
      c(1)=(1.0_DOUBLE)
      c(2)=(1.0_DOUBLE)
      c(3)=slope
      call init_identity(m)
      call geom_translate(apex,m)
      call geom_scalea(c,m)
      call unit_cone(coeff)
      call transform_face(coeff,m,coeff)
      if(xa2(3).LT.xa1(3))orientation=-orientation
      surf_type=1
      end if
      if(orientation.LT.0)then
      do i=1,10
      coeff(i)=-coeff(i)
      end do
      end if
      return
      end
      subroutine cone(x0,a,x1,x2,coeff,surf_type,apex)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)x0(3),a(3),x1(3),x2(3)
      REAL(kind=DOUBLE)coeff(10),apex(3)
      integer surf_type
      REAL(kind=DOUBLE)m(3,4),xa1(3),xa2(3),tempa(3)
      external conea,init_identity,geom_translate,geom_rotatea,transform
     &,transform_face
      call init_identity(m)
      call geom_translate(x0,m)
      call geom_rotatea(a,m)
      call transform(m,x1,xa1)
      call transform(m,x2,xa2)
      call conea(xa1,xa2,coeff,surf_type,apex)
      call transform_face(coeff,m,coeff)
      call invert(m,m)
      tempa(1)=apex(1)
      tempa(2)=apex(2)
      tempa(3)=apex(3)
      
      call transform(m,tempa,apex)
      return
      end
      subroutine coneb(x0,a,x1,x2,coeff)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)x0(3),a(3),x1(3),x2(3)
      REAL(kind=DOUBLE)coeff(10)
      REAL(kind=DOUBLE)m(3,4),xa1(3),xa2(3),xm(3),c(3),z1,slope
      integer orientation,i
      REAL(kind=DOUBLE)vector_temp(3)
      external unit_cone,init_identity,geom_translate,geom_rotatea,geom_
     &scalea,transform,transform_face,vector_compare
      integer vector_compare
      orientation=vector_compare(x1,x2)
      if(orientation.NE.0)continue
      call init_identity(m)
      call geom_translate(x0,m)
      call geom_rotatea(a,m)
      call transform(m,x1,xa1)
      call transform(m,x2,xa2)
      if(orientation.LT.0)then
      vector_temp(1)=xa1(1)
      xa1(1)=xa2(1)
      xa2(1)=vector_temp(1)
      vector_temp(2)=xa1(2)
      xa1(2)=xa2(2)
      xa2(2)=vector_temp(2)
      vector_temp(3)=xa1(3)
      xa1(3)=xa2(3)
      xa2(3)=vector_temp(3)
      
      end if
      xa1(1)=sqrt(xa1(1)**2+xa1(2)**2)
      xa1(2)=(0.0_DOUBLE)
      xa2(1)=sqrt(xa2(1)**2+xa2(2)**2)
      xa2(2)=(0.0_DOUBLE)
      if(sqrt((xa1(1)-xa2(1))**2+(xa1(3)-xa2(3))**2).GT.(2.0_DOUBLE)*(1.
     &0e-8_DOUBLE))continue
      if(abs(xa1(1)-xa2(1)).LE.(1.0e-8_DOUBLE))then
      call cylinder(x0,a,(0.5_DOUBLE)*(xa1(1)+xa2(1)),coeff)
      if(xa2(3).LT.xa1(3))orientation=-orientation
      else if(abs(xa1(3)-xa2(3)).LE.(1.0e-8_DOUBLE))then
      do i=1,3
      xm(i)=(0.5_DOUBLE)*(x1(i)+x2(i))
      end do
      call plane(xm,a,coeff)
      if(xa1(1).LT.xa2(1))orientation=-orientation
      else
      slope=(xa2(3)-xa1(3))/(xa2(1)-xa1(1))
      z1=xa1(3)-slope*xa1(1)
      vector_temp(1)=sqrt((a(1)**2+a(2)**2+a(3)**2))
      if(vector_temp(1).GT.(0.0_DOUBLE))continue
      xm(1)=((1.0_DOUBLE)/vector_temp(1))*a(1)
      xm(2)=((1.0_DOUBLE)/vector_temp(1))*a(2)
      xm(3)=((1.0_DOUBLE)/vector_temp(1))*a(3)
      
      xm(1)=x0(1)+xm(1)*(z1)
      xm(2)=x0(2)+xm(2)*(z1)
      xm(3)=x0(3)+xm(3)*(z1)
      
      c(1)=(1.0_DOUBLE)
      c(2)=(1.0_DOUBLE)
      c(3)=slope
      call unit_cone(coeff)
      call init_identity(m)
      call geom_translate(xm,m)
      call geom_rotatea(a,m)
      call geom_scalea(c,m)
      call transform_face(coeff,m,coeff)
      if(xa2(3).LT.xa1(3))orientation=-orientation
      end if
      if(orientation.LT.0)then
      do i=1,10
      coeff(i)=-coeff(i)
      end do
      end if
      return
      end
      
      
