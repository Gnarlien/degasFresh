      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module g2_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer g2_num_polygons
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::g2_polygon_xz
      integer,dimension(:,:),pointer::g2_polygon_segment
      integer,dimension(:),pointer::g2_polygon_points
      integer,dimension(:),pointer::g2_polygon_zone
      integer,dimension(:),pointer::g2_polygon_stratum
      end module g2_mod
      
      
