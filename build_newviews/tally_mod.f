      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module tl_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer tl_num
      integer nconversions
      integer tally_size
      integer,dimension(:),pointer::tally_type_num
      integer,dimension(:),pointer::tally_type
      integer,dimension(:),pointer::tally_geometry
      integer,dimension(:),pointer::tally_geometry_ptr
      integer,dimension(:),pointer::tally_base
      integer,dimension(:),pointer::tally_type_base
      integer,dimension(:),pointer::tally_rank
      integer,dimension(:),pointer::tally_dep_var_dim
      integer,dimension(:,:),pointer::tally_indep_var
      integer,dimension(:,:),pointer::tally_tab_index
      character(len=(80)),dimension(:),pointer::tally_name
      integer,dimension(:),pointer::tally_dep_var
      REAL(kind=DOUBLE),dimension(:,:),pointer::tally_est_test
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::tally_est_reaction
      integer,dimension(:),pointer::tally_num_conversions
      integer,dimension(:,:),pointer::tally_cv_ptr
      integer,dimension(:),pointer::tally_cv_action
      integer,dimension(:),pointer::tally_cv_type
      integer,dimension(:),pointer::tally_cv_num_partners
      integer,dimension(:,:),pointer::tally_cv_scalers
      integer,dimension(:,:),pointer::tally_cv_partners
      character(len=(40))tally_var_list((0):(100))
      character(len=(300))tally_version
      end module tl_mod
      
      
