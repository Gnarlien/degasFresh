      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module ef_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer ef_nr
      integer ef_nz
      integer ef_lim_msegments
      REAL(kind=DOUBLE),dimension(:),pointer::ef_psi_knot
      REAL(kind=DOUBLE),dimension(:),pointer::ef_r_knot
      REAL(kind=DOUBLE),dimension(:),pointer::ef_z_knot
      REAL(kind=DOUBLE),dimension(:),pointer::ef_psi_bscoef
      REAL(kind=DOUBLE),dimension(:),pointer::ef_i_bscoef
      REAL(kind=DOUBLE),dimension(:),pointer::ef_lim_r
      REAL(kind=DOUBLE),dimension(:),pointer::ef_lim_z
      REAL(kind=DOUBLE),dimension(:),pointer::ef_lim_psi
      integer,dimension(:),pointer::ef_lim_cross_times
      integer,dimension(:,:),pointer::ef_lim_cross_wall
      integer,dimension(:,:),pointer::ef_lim_cross_node
      REAL(kind=DOUBLE)ef_min_r
      REAL(kind=DOUBLE)ef_max_r
      REAL(kind=DOUBLE)ef_min_z
      REAL(kind=DOUBLE)ef_max_z
      REAL(kind=DOUBLE)ef_psilim
      REAL(kind=DOUBLE)ef_rma
      REAL(kind=DOUBLE)ef_zma
      end module ef_mod
      
      
