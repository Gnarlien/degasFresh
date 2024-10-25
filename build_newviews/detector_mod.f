      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module de_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer de_grps
      integer de_max_bins
      integer de_zone_frags_dim
      integer de_zone_frags_size
      character(len=(100)),dimension(:),pointer::detector_name
      integer,dimension(:),pointer::detector_num_views
      integer,dimension(:),pointer::detector_var
      integer,dimension(:),pointer::detector_tab_index
      REAL(kind=DOUBLE),dimension(:),pointer::detector_min
      REAL(kind=DOUBLE),dimension(:),pointer::detector_delta
      integer,dimension(:),pointer::detector_spacing
      integer detector_total_views
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::de_view_points
      integer,dimension(:),pointer::de_view_algorithm
      REAL(kind=DOUBLE),dimension(:),pointer::de_view_halfwidth
      REAL(kind=DOUBLE),dimension(:),pointer::de_zone_frags
      integer,dimension(:),pointer::de_zone_frags_start
      integer,dimension(:),pointer::de_zone_frags_num
      integer,dimension(:),pointer::de_zone_frags_zones
      integer,dimension(:),pointer::de_zone_frags_min_zn
      integer,dimension(:),pointer::de_zone_frags_max_zn
      integer,dimension(:),pointer::de_view_base
      integer de_view_size
      integer,dimension(:),pointer::de_view_tab
      end module de_mod
      
      
