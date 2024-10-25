      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module sp_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer sp_num
      character(len=(32)),dimension(:),pointer::species_name
      character(len=(8)),dimension(:),pointer::species_sy
      integer,dimension(:),pointer::species_z
      REAL(kind=DOUBLE),dimension(:),pointer::species_m
      integer,dimension(:),pointer::species_ncomp
      integer,dimension(:),pointer::species_generic
      integer,dimension(:),pointer::species_multiplicity
      integer,dimension(:,:),pointer::species_el
      integer,dimension(:,:),pointer::species_count
      character(len=(300))species_version
      end module sp_mod
      
      
