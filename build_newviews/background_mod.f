      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module bk_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer bk_num
      integer background_coords
      REAL(kind=DOUBLE),dimension(:,:),pointer::background_n
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::background_v
      REAL(kind=DOUBLE),dimension(:,:),pointer::background_temp
      end module bk_mod
      
      
