      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module mp_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer comm_world_dup
      integer mpi_degas2_root
      integer mpi_size
      integer mpi_rank
      integer mpi_nslaves
      integer mpi_nlevels
      integer,dimension(:),pointer::mpi_slave_map
      integer,dimension(:),pointer::mpi_meta_slave_comms
      integer,dimension(:),pointer::mpi_meta_slave_groups
      end module mp_mod
      
      
