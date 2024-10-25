      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module sc_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer nsectors
      integer,dimension(:),pointer::strata
      integer,dimension(:),pointer::sector_strata_segment
      integer,dimension(:),pointer::sectors
      integer,dimension(:),pointer::sector_zone
      integer,dimension(:),pointer::sector_surface
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::sector_points
      integer,dimension(:,:),pointer::sector_type_pointer
      integer sc_vacuum_num
      integer,dimension(:),pointer::vacuum_sector
      integer sc_plasma_num
      integer,dimension(:),pointer::plasma_sector
      integer sc_target_num
      integer,dimension(:),pointer::target_sector
      integer,dimension(:),pointer::target_material
      REAL(kind=DOUBLE),dimension(:),pointer::target_temperature
      REAL(kind=DOUBLE),dimension(:),pointer::target_recyc_coef
      integer sc_wall_num
      integer,dimension(:),pointer::wall_sector
      integer,dimension(:),pointer::wall_material
      REAL(kind=DOUBLE),dimension(:),pointer::wall_temperature
      REAL(kind=DOUBLE),dimension(:),pointer::wall_recyc_coef
      integer sc_exit_num
      integer,dimension(:),pointer::exit_sector
      integer sc_diagnostic_grps
      integer sc_diag_max_bins
      character(len=(40)),dimension(:),pointer::diagnostic_grp_name
      integer,dimension(:),pointer::diagnostic_num_sectors
      integer,dimension(:),pointer::diagnostic_var
      integer,dimension(:),pointer::diagnostic_tab_index
      REAL(kind=DOUBLE),dimension(:),pointer::diagnostic_min
      REAL(kind=DOUBLE),dimension(:),pointer::diagnostic_delta
      integer,dimension(:),pointer::diagnostic_spacing
      integer,dimension(:),pointer::diagnostic_grp_base
      integer sc_diag_size
      integer,dimension(:),pointer::diagnostic_sector_tab
      end module sc_mod
      
      
