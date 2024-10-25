      
      
      
      
      
      
      subroutine random_gauss(y,n,ran_index_x,ran_array_x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer n
      REAL(kind=DOUBLE)y(0:n-1)
      integer i
      REAL(kind=DOUBLE)pi,theta,z
      REAL(kind=DOUBLE)random
      external random_array,random
      data pi/3.14159265358979323846264338328_DOUBLE/
      REAL(kind=SINGLE)ys(0:n-1)
      REAL(kind=SINGLE)spi,stheta,sz
      REAL(kind=SINGLE)srandom
      external srandom_array,srandom
      data spi/3.14159265358979323846264338328/
      if(n.LE.0)return
      call random_array(y,n,ran_index_x,ran_array_x(0))
      if(n.GT.1)then
      do i=0,int(n/2)-1
      theta=pi*((2.0_DOUBLE)*y(i)-(1.0_DOUBLE))
      z=sqrt(-(2.0_DOUBLE)*log(y(i+1)))
      y(i)=z*cos(theta)
      y(i+1)=z*sin(theta)
      end do
      end if
      if(mod(n,2).EQ.0)return
      theta=pi*((2.0_DOUBLE)*y(n-1)-(1.0_DOUBLE))
      z=sqrt(-(2.0_DOUBLE)*log(random(ran_index_x,ran_array_x(0))))
      y(n-1)=z*cos(theta)
      return
      entry srandom_gauss(ys,n,ran_index_x,ran_array_x)
      if(n.LE.0)return
      call srandom_array(ys,n,ran_index_x,ran_array_x(0))
      if(n.GT.1)then
      do i=0,int(n/2)-1
      stheta=spi*(2.0*ys(i)-1.0)
      sz=sqrt(-2.0*log(ys(i+1)))
      ys(i)=sz*cos(stheta)
      ys(i+1)=sz*sin(stheta)
      end do
      end if
      if(mod(n,2).EQ.0)return
      stheta=spi*(2.0*ys(n-1)-1.0)
      sz=sqrt(-2.0*log(srandom(ran_index_x,ran_array_x(0))))
      ys(n-1)=sz*cos(stheta)
      return
      end
      subroutine random_isodist(v,n,ran_index_x,ran_array_x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer n
      REAL(kind=DOUBLE)v(0:3*n-1)
      integer i
      REAL(kind=DOUBLE)pi,costheta,phi
      external random_array
      data pi/3.14159265358979323846264338328_DOUBLE/
      REAL(kind=SINGLE)vs(0:3*n-1)
      REAL(kind=SINGLE)spi,scostheta,sphi
      external srandom_array
      data spi/3.14159265358979323846264338328/
      if(n.LE.0)return
      call random_array(v(n),2*n,ran_index_x,ran_array_x(0))
      do i=0,n-1
      costheta=(2.0_DOUBLE)*v(n+2*i)-(1.0_DOUBLE)
      phi=pi*((2.0_DOUBLE)*v(n+2*i+1)-(1.0_DOUBLE))
      v(3*i)=cos(phi)*sqrt((1.0_DOUBLE)-costheta**2)
      v(3*i+1)=sin(phi)*sqrt((1.0_DOUBLE)-costheta**2)
      v(3*i+2)=costheta
      end do
      return
      entry srandom_isodist(vs,n,ran_index_x,ran_array_x)
      if(n.LE.0)return
      call srandom_array(vs(n),2*n,ran_index_x,ran_array_x(0))
      do i=0,n-1
      scostheta=2.0*vs(n+2*i)-1.0
      sphi=spi*(2.0*vs(n+2*i+1)-1.0)
      vs(3*i)=cos(sphi)*sqrt(1.0-scostheta**2)
      vs(3*i+1)=sin(sphi)*sqrt(1.0-scostheta**2)
      vs(3*i+2)=scostheta
      end do
      return
      end
      subroutine random_cosdist(v,n,ran_index_x,ran_array_x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer n
      REAL(kind=DOUBLE)v(0:3*n-1)
      integer i
      REAL(kind=DOUBLE)pi,costheta2,phi
      external random_array
      data pi/3.14159265358979323846264338328_DOUBLE/
      REAL(kind=SINGLE)vs(0:2*n-1)
      REAL(kind=SINGLE)spi,scostheta2,sphi
      external srandom_array
      data spi/3.14159265358979323846264338328/
      if(n.LE.0)return
      call random_array(v(n),2*n,ran_index_x,ran_array_x(0))
      do i=0,n-1
      costheta2=v(n+2*i)
      phi=pi*((2.0_DOUBLE)*v(n+2*i+1)-(1.0_DOUBLE))
      v(3*i)=cos(phi)*sqrt((1.0_DOUBLE)-costheta2)
      v(3*i+1)=sin(phi)*sqrt((1.0_DOUBLE)-costheta2)
      v(3*i+2)=sqrt(costheta2)
      end do
      return
      entry srandom_cosdist(vs,n,ran_index_x,ran_array_x)
      if(n.LE.0)return
      call srandom_array(vs(n),2*n,ran_index_x,ran_array_x(0))
      do i=0,n-1
      scostheta2=vs(n+2*i)
      sphi=spi*(2.0*vs(n+2*i+1)-1.0)
      vs(3*i)=cos(sphi)*sqrt(1.0-scostheta2)
      vs(3*i+1)=sin(sphi)*sqrt(1.0-scostheta2)
      vs(3*i+2)=sqrt(scostheta2)
      end do
      return
      end
      
      
