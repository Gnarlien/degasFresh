      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module el_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer el_num
      character(len=(16)),dimension(:),pointer::element_name
      character(len=(3)),dimension(:),pointer::element_sy
      integer,dimension(:),pointer::element_z
      REAL(kind=DOUBLE),dimension(:),pointer::element_m
      character(len=(300))element_version
      end module el_mod
      
      
