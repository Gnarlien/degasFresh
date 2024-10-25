      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module ma_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer ma_num
      character(len=(32)),dimension(:),pointer::materials_name
      character(len=(8)),dimension(:),pointer::materials_sy
      character(len=(300))materials_version
      end module ma_mod
      
      
