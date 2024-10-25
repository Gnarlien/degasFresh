      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module gi_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer ncells
      integer nsurfaces
      integer nboundaries
      integer nneighbors
      integer ntransforms
      integer geometry_symmetry
      REAL(kind=DOUBLE)universal_cell_min((1):(3))
      REAL(kind=DOUBLE)universal_cell_max((1):(3))
      REAL(kind=DOUBLE)universal_cell_vol
      integer,dimension(:,:),pointer::cells
      integer,dimension(:,:,:),pointer::surfaces
      integer,dimension(:,:,:),pointer::surfaces_tx_ind
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::surfaces_tx_mx
      integer,dimension(:,:,:),pointer::surface_sectors
      integer,dimension(:),pointer::boundaries
      integer,dimension(:),pointer::neighbors
      REAL(kind=DOUBLE),dimension(:,:),pointer::surface_coeffs
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::surface_points
      end module gi_mod
      
      
