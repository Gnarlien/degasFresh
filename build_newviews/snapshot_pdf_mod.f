      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module sn_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer sn_particles_dim
      integer sn_number_particles
      character(len=(34))sn_seed_decimal
      REAL(kind=DOUBLE),dimension(:,:),pointer::sn_particles_float
      integer,dimension(:,:),pointer::sn_particles_int
      end module sn_mod
      
      
