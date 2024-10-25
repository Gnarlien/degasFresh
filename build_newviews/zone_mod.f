      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module zn_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer zn_num
      integer zone_type_num((1):(4))
      integer,dimension(:),pointer::zone_type
      integer,dimension(:,:),pointer::zone_index
      integer zone_index_min((1):(4))
      integer zone_index_max((1):(4))
      integer,dimension(:),pointer::zone_pointer
      REAL(kind=DOUBLE),dimension(:),pointer::zone_volume
      REAL(kind=DOUBLE),dimension(:,:),pointer::zone_center
      REAL(kind=DOUBLE),dimension(:,:),pointer::zone_min
      REAL(kind=DOUBLE),dimension(:,:),pointer::zone_max
      character(len=(300))zone_version
      end module zn_mod
      
      
