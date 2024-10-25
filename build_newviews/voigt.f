      
      
      
      
      
      function voigt(xarg,y)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)voigt
      REAL(kind=DOUBLE)xarg,y
      integer i,n
      REAL(kind=DOUBLE)fact_term,zn,x,s,t,xser,yser,xn,yn,xnew,ynew,x2,y
     &2,r,f,g,h
      REAL(kind=DOUBLE)coef_r1(30),coef_r2(6),coef_r3(4)
      logical init
      save init,coef_r1,coef_r2,coef_r3
      data init/.TRUE./
      if(init)then
      fact_term=(1.0_DOUBLE)
      coef_r1(1)=(1.0_DOUBLE)
      do i=2,30
      zn=REAL(i-1,DOUBLE)
      fact_term=fact_term*(-(1.0_DOUBLE)/zn)
      coef_r1(i)=fact_term/((2.0_DOUBLE)*zn+(1.0_DOUBLE))
      end do
      coef_r2(1)=(0.46131350_DOUBLE)
      coef_r2(2)=(0.19016350_DOUBLE)
      coef_r2(3)=(0.09999216_DOUBLE)
      coef_r2(4)=(1.78449270_DOUBLE)
      coef_r2(5)=(0.002883894_DOUBLE)
      coef_r2(6)=(5.52534370_DOUBLE)
      coef_r3(1)=(0.51242424_DOUBLE)
      coef_r3(2)=(0.27525810_DOUBLE)
      coef_r3(3)=(0.05176536_DOUBLE)
      coef_r3(4)=(2.72474500_DOUBLE)
      init=.FALSE.
      end if
      if(y.GE.(0.0_DOUBLE))continue
      if(xarg.LT.(0.0_DOUBLE))then
      x=-xarg
      else
      x=xarg
      end if
      s=x*x-y*y
      t=(2.0_DOUBLE)*x*y
      if(x.LT.(3.0_DOUBLE).AND.y.LT.(1.8_DOUBLE))then
      xser=y
      yser=-x
      xn=y
      yn=-x
      x2=-s
      y2=-t
      n=int((6.842_DOUBLE)*x+(8.0_DOUBLE))
      n=min(n,29)
      n=max(n,18)
      do i=1,n
      xnew=xn*x2-yn*y2
      ynew=y2*xn+yn*x2
      xser=xser+(xnew*coef_r1(i+1))
      yser=yser+(ynew*coef_r1(i+1))
      xn=xnew
      yn=ynew
      end do
      voigt=exp(-s)*(cos(-t)*((1.0_DOUBLE)-((2.0_DOUBLE)/sqrt(atan2((0.0
     &_DOUBLE),-(1.0_DOUBLE))))*xser)+((2.0_DOUBLE)/sqrt(atan2((0.0_DOUB
     &LE),-(1.0_DOUBLE))))*sin(-t)*yser)
      else if(x.LT.(5.0_DOUBLE).AND.y.LT.(5.0_DOUBLE))then
      r=t*t
      t=t*x
      f=s-coef_r2(6)
      g=s-coef_r2(4)
      h=s-coef_r2(2)
      voigt=coef_r2(1)*((t-h*y)/(h*h+r))+coef_r2(3)*((t-g*y)/(g*g+r))+co
     &ef_r2(5)*((t-f*y)/(f*f+r))
      else
      r=t*t
      t=t*x
      f=s-coef_r3(2)
      g=s-coef_r3(4)
      voigt=coef_r3(1)*((t-f*y)/(f*f+r))+coef_r3(3)*((t-g*y)/(g*g+r))
      end if
      end
      
      
