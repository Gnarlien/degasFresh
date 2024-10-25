      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      subroutine invert(m,n)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)m(3,4)
      REAL(kind=DOUBLE)n(3,4)
      integer i,j
      REAL(kind=DOUBLE)det,temp(3,4)
      external determinant
      REAL(kind=DOUBLE)determinant
      det=determinant(m)
      if(det.NE.(0.0_DOUBLE))continue
      temp(1,1)=m(2,2)*m(3,3)-m(2,3)*m(3,2)
      temp(1,2)=m(1,3)*m(3,2)-m(1,2)*m(3,3)
      temp(1,3)=m(1,2)*m(2,3)-m(1,3)*m(2,2)
      temp(1,4)=(m(1,3)*m(2,2)-m(1,2)*m(2,3))*m(3,4)+(m(1,2)*m(2,4)-m(1,
     &4)*m(2,2))*m(3,3)+(m(1,4)*m(2,3)-m(1,3)*m(2,4))*m(3,2)
      temp(2,1)=m(2,3)*m(3,1)-m(2,1)*m(3,3)
      temp(2,2)=m(1,1)*m(3,3)-m(1,3)*m(3,1)
      temp(2,3)=m(1,3)*m(2,1)-m(1,1)*m(2,3)
      temp(2,4)=(m(1,1)*m(2,3)-m(1,3)*m(2,1))*m(3,4)+(m(1,4)*m(2,1)-m(1,
     &1)*m(2,4))*m(3,3)+(m(1,3)*m(2,4)-m(1,4)*m(2,3))*m(3,1)
      temp(3,1)=m(2,1)*m(3,2)-m(2,2)*m(3,1)
      temp(3,2)=m(1,2)*m(3,1)-m(1,1)*m(3,2)
      temp(3,3)=m(1,1)*m(2,2)-m(1,2)*m(2,1)
      temp(3,4)=(m(1,2)*m(2,1)-m(1,1)*m(2,2))*m(3,4)+(m(1,1)*m(2,4)-m(1,
     &4)*m(2,1))*m(3,2)+(m(1,4)*m(2,2)-m(1,2)*m(2,4))*m(3,1)
      do i=1,3
      do j=1,4
      n(i,j)=temp(i,j)/det
      end do
      end do
      return
      end
      subroutine transform_face(coeff,m,new_coeff)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)coeff(10),m(3,4)
      REAL(kind=DOUBLE)new_coeff(10)
      REAL(kind=DOUBLE)temp(10)
      integer i
      temp(1)=m(3,4)**2*coeff(7)+m(3,4)*coeff(4)+m(2,4)*m(3,4)*coeff(9)+
     &m(2,4)**2*coeff(6)+m(2,4)*coeff(3)+m(1,4)*m(3,4)*coeff(10)+m(1,4)*
     &m(2,4)*coeff(8)+m(1,4)**2*coeff(5)+m(1,4)*coeff(2)+coeff(1)
      temp(2)=2*m(3,1)*m(3,4)*coeff(7)+m(3,1)*coeff(4)+(m(2,1)*m(3,4)+m(
     &2,4)*m(3,1))*coeff(9)+2*m(2,1)*m(2,4)*coeff(6)+m(2,1)*coeff(3)+(m(
     &1,1)*m(3,4)+m(1,4)*m(3,1))*coeff(10)+(m(1,1)*m(2,4)+m(1,4)*m(2,1))
     &*coeff(8)+2*m(1,1)*m(1,4)*coeff(5)+m(1,1)*coeff(2)
      temp(3)=2*m(3,2)*m(3,4)*coeff(7)+m(3,2)*coeff(4)+(m(2,2)*m(3,4)+m(
     &2,4)*m(3,2))*coeff(9)+2*m(2,2)*m(2,4)*coeff(6)+m(2,2)*coeff(3)+(m(
     &1,2)*m(3,4)+m(1,4)*m(3,2))*coeff(10)+(m(1,2)*m(2,4)+m(1,4)*m(2,2))
     &*coeff(8)+2*m(1,2)*m(1,4)*coeff(5)+m(1,2)*coeff(2)
      temp(4)=2*m(3,3)*m(3,4)*coeff(7)+m(3,3)*coeff(4)+(m(2,3)*m(3,4)+m(
     &2,4)*m(3,3))*coeff(9)+2*m(2,3)*m(2,4)*coeff(6)+m(2,3)*coeff(3)+(m(
     &1,3)*m(3,4)+m(1,4)*m(3,3))*coeff(10)+(m(1,3)*m(2,4)+m(1,4)*m(2,3))
     &*coeff(8)+2*m(1,3)*m(1,4)*coeff(5)+m(1,3)*coeff(2)
      temp(5)=m(3,1)**2*coeff(7)+m(2,1)*m(3,1)*coeff(9)+m(2,1)**2*coeff(
     &6)+m(1,1)*m(3,1)*coeff(10)+m(1,1)*m(2,1)*coeff(8)+m(1,1)**2*coeff(
     &5)
      temp(6)=m(3,2)**2*coeff(7)+m(2,2)*m(3,2)*coeff(9)+m(2,2)**2*coeff(
     &6)+m(1,2)*m(3,2)*coeff(10)+m(1,2)*m(2,2)*coeff(8)+m(1,2)**2*coeff(
     &5)
      temp(7)=m(3,3)**2*coeff(7)+m(2,3)*m(3,3)*coeff(9)+m(2,3)**2*coeff(
     &6)+m(1,3)*m(3,3)*coeff(10)+m(1,3)*m(2,3)*coeff(8)+m(1,3)**2*coeff(
     &5)
      temp(8)=2*m(3,1)*m(3,2)*coeff(7)+(m(2,1)*m(3,2)+m(2,2)*m(3,1))*coe
     &ff(9)+2*m(2,1)*m(2,2)*coeff(6)+(m(1,1)*m(3,2)+m(1,2)*m(3,1))*coeff
     &(10)+(m(1,1)*m(2,2)+m(1,2)*m(2,1))*coeff(8)+2*m(1,1)*m(1,2)*coeff(
     &5)
      temp(9)=2*m(3,2)*m(3,3)*coeff(7)+(m(2,2)*m(3,3)+m(2,3)*m(3,2))*coe
     &ff(9)+2*m(2,2)*m(2,3)*coeff(6)+(m(1,2)*m(3,3)+m(1,3)*m(3,2))*coeff
     &(10)+(m(1,2)*m(2,3)+m(1,3)*m(2,2))*coeff(8)+2*m(1,2)*m(1,3)*coeff(
     &5)
      temp(10)=2*m(3,1)*m(3,3)*coeff(7)+(m(2,1)*m(3,3)+m(2,3)*m(3,1))*co
     &eff(9)+2*m(2,1)*m(2,3)*coeff(6)+(m(1,1)*m(3,3)+m(1,3)*m(3,1))*coef
     &f(10)+(m(1,1)*m(2,3)+m(1,3)*m(2,1))*coeff(8)+2*m(1,1)*m(1,3)*coeff
     &(5)
      do i=1,10
      new_coeff(i)=temp(i)
      end do
      return
      end
      subroutine geom_rotate(theta,v,m)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)theta,v(3)
      REAL(kind=DOUBLE)m(3,4)
      integer i,j,k
      REAL(kind=DOUBLE)vh(3),c,s,temp(3,4)
      REAL(kind=DOUBLE)vector_temp(3)
      vector_temp(1)=sqrt((v(1)**2+v(2)**2+v(3)**2))
      if(vector_temp(1).GT.(0.0_DOUBLE))continue
      vh(1)=((1.0_DOUBLE)/vector_temp(1))*v(1)
      vh(2)=((1.0_DOUBLE)/vector_temp(1))*v(2)
      vh(3)=((1.0_DOUBLE)/vector_temp(1))*v(3)
      
      c=cos(theta)
      s=(2.0_DOUBLE)*sin((0.5_DOUBLE)*theta)**2
      do i=1,3
      do j=1,3
      temp(i,j)=s*vh(i)*vh(j)
      end do
      temp(i,i)=c+temp(i,i)
      temp(i,4)=(0.0_DOUBLE)
      end do
      
      s=-sin(theta)
      do i=1,3
      j=mod(i,3)+1
      k=mod(j,3)+1
      temp(i,k)=temp(i,k)+s*vh(j)
      temp(i,j)=temp(i,j)-s*vh(k)
      end do
      call compose(temp,m,m)
      return
      end
      subroutine geom_rotatea(a,m)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)a(3)
      REAL(kind=DOUBLE)m(3,4)
      REAL(kind=DOUBLE)theta,z(3),v(3)
      REAL(kind=DOUBLE)vector_temp(3)
      if(sqrt((a(1)**2+a(2)**2+a(3)**2)).GT.(0.0_DOUBLE))continue
      theta=atan2(sqrt(a(1)**2+a(2)**2),a(3))
      z(1)=(0.0_DOUBLE)
      z(2)=(0.0_DOUBLE)
      z(3)=(1.0_DOUBLE)
      vector_temp(1)=z(2)*a(3)-z(3)*a(2)
      vector_temp(2)=z(3)*a(1)-z(1)*a(3)
      v(3)=z(1)*a(2)-z(2)*a(1)
      v(1)=vector_temp(1)
      v(2)=vector_temp(2)
      if(sqrt((v(1)**2+v(2)**2+v(3)**2)).EQ.(0.0_DOUBLE))then
      v(1)=(1.0_DOUBLE)
      end if
      call geom_rotate(theta,v,m)
      return
      end
      subroutine reflect_plane(coeff,m)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)coeff(10)
      REAL(kind=DOUBLE)m(3,4)
      integer i,j
      if(coeff(5).EQ.(0.0_DOUBLE).AND.coeff(6).EQ.(0.0_DOUBLE).AND.coeff
     &(7).EQ.(0.0_DOUBLE).AND.coeff(8).EQ.(0.0_DOUBLE).AND.coeff(9).EQ.(
     &0.0_DOUBLE).AND.coeff(10).EQ.(0.0_DOUBLE))continue
      m(1,1)=coeff(4)**2+coeff(3)**2-coeff(2)**2
      m(1,2)=-(2.0_DOUBLE)*coeff(2)*coeff(3)
      m(1,3)=-(2.0_DOUBLE)*coeff(2)*coeff(4)
      m(1,4)=-(2.0_DOUBLE)*coeff(1)*coeff(2)
      m(2,1)=-(2.0_DOUBLE)*coeff(2)*coeff(3)
      m(2,2)=coeff(4)**2-coeff(3)**2+coeff(2)**2
      m(2,3)=-(2.0_DOUBLE)*coeff(3)*coeff(4)
      m(2,4)=-(2.0_DOUBLE)*coeff(1)*coeff(3)
      m(3,1)=-(2.0_DOUBLE)*coeff(2)*coeff(4)
      m(3,2)=-(2.0_DOUBLE)*coeff(3)*coeff(4)
      m(3,3)=-coeff(4)**2+coeff(3)**2+coeff(2)**2
      m(3,4)=-(2.0_DOUBLE)*coeff(1)*coeff(4)
      if(coeff(4)**2+coeff(3)**2+coeff(2)**2.GT.(0.0_DOUBLE))continue
      do i=1,3
      do j=1,4
      m(i,j)=m(i,j)/(coeff(4)**2+coeff(3)**2+coeff(2)**2)
      end do
      end do
      return
      end
      function vector_compare(a,b)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer vector_compare
      REAL(kind=DOUBLE)a(3),b(3)
      integer i
      do i=1,3
      if(a(i).LT.b(i))then
      vector_compare=-1
      return
      else if(a(i).GT.b(i))then
      vector_compare=1
      return
      end if
      end do
      vector_compare=0
      return
      end
      function surface_equal(coeff1,coeff2,orientation)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical surface_equal
      REAL(kind=DOUBLE)coeff1(10),coeff2(10)
      integer orientation
      REAL(kind=DOUBLE)temp
      logical start
      integer i
      start=.TRUE.
      surface_equal=.FALSE.
      do i=1,10
      if(start)then
      if(coeff1(i).EQ.coeff2(i))then
      if(coeff1(i).NE.(0.0_DOUBLE))then
      temp=(1.0_DOUBLE)
      start=.FALSE.
      end if
      else if(coeff1(i).EQ.-coeff2(i))then
      temp=-(1.0_DOUBLE)
      start=.FALSE.
      else
      return
      end if
      else
      if(coeff1(i).NE.temp*coeff2(i))then
      return
      end if
      end if
      end do
      if(.NOT.start)continue
      orientation=int(temp)
      surface_equal=.TRUE.
      return
      end
      
      subroutine compose(m,n,t)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)m(3,4),n(3,4)
      REAL(kind=DOUBLE)t(3,4)
      integer i,j
      REAL(kind=DOUBLE)temp(3,4)
      do i=1,3
      do j=1,4
      temp(i,j)=m(i,1)*n(1,j)+m(i,2)*n(2,j)+m(i,3)*n(3,j)
      end do
      temp(i,4)=temp(i,4)+m(i,4)
      end do
      do i=1,3
      do j=1,4
      t(i,j)=temp(i,j)
      end do
      end do
      return
      end
      function determinant(m)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)determinant
      REAL(kind=DOUBLE)m(3,4)
      determinant=m(1,1)*(m(2,2)*m(3,3)-m(2,3)*m(3,2))-m(1,2)*(m(2,1)*m(
     &3,3)-m(2,3)*m(3,1))+m(1,3)*(m(2,1)*m(3,2)-m(2,2)*m(3,1))
      return
      end
      subroutine init_identity(m)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)m(3,4)
      integer i,j
      do i=1,3
      do j=1,4
      m(i,j)=(0.0_DOUBLE)
      end do
      m(i,i)=(1.0_DOUBLE)
      end do
      return
      end
      subroutine geom_translate(x0,m)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)x0(3)
      REAL(kind=DOUBLE)m(3,4)
      integer i,j
      REAL(kind=DOUBLE)temp(3,4)
      do i=1,3
      do j=1,3
      temp(i,j)=(0.0_DOUBLE)
      end do
      temp(i,i)=(1.0_DOUBLE)
      temp(i,4)=-x0(i)
      end do
      call compose(temp,m,m)
      return
      end
      subroutine geom_scale(a,m)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)a
      REAL(kind=DOUBLE)m(3,4)
      integer i,j
      REAL(kind=DOUBLE)temp(3,4)
      if(a.NE.(0.0_DOUBLE))continue
      do i=1,3
      do j=1,4
      temp(i,j)=(0.0_DOUBLE)
      end do
      temp(i,i)=(1.0_DOUBLE)/a
      end do
      call compose(temp,m,m)
      return
      end
      subroutine geom_scalea(a,m)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)a(3)
      REAL(kind=DOUBLE)m(3,4)
      integer i,j
      REAL(kind=DOUBLE)temp(3,4)
      do i=1,3
      if(a(i).NE.(0.0_DOUBLE))continue
      do j=1,4
      temp(i,j)=(0.0_DOUBLE)
      end do
      temp(i,i)=(1.0_DOUBLE)/a(i)
      end do
      call compose(temp,m,m)
      return
      end
      subroutine surface_normalize(coeff)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)coeff(10)
      REAL(kind=DOUBLE)temp
      integer i
      temp=(0.0_DOUBLE)
      do i=1,10
      temp=max(temp,abs(coeff(i)))
      end do
      if(temp.GT.(0.0_DOUBLE))continue
      do i=1,10
      coeff(i)=coeff(i)/temp
      end do
      return
      end
      
      
