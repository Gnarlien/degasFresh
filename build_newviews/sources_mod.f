      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module so_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer so_grps
      integer so_seg_tot
      integer so_restart
      integer so_spaced_seeds
      integer so_seed_spacing
      integer so_sampling
      integer so_time_dependent
      integer so_time_initialization
      REAL(kind=DOUBLE)so_time_initial
      REAL(kind=DOUBLE)so_time_final
      REAL(kind=DOUBLE)so_rel_wt_min
      REAL(kind=DOUBLE)so_rel_wt_max
      REAL(kind=DOUBLE)so_wt_norm_min
      REAL(kind=DOUBLE)so_wt_norm_max
      integer so_gparams_list_size
      integer so_gparams_list_dim
      integer so_params_list_size
      integer so_params_list_dim
      integer so_params_data_size
      integer so_params_data_dim
      integer so_giparams_list_size
      integer so_giparams_list_dim
      integer so_iparams_list_size
      integer so_iparams_list_dim
      integer so_iparams_data_size
      integer so_iparams_data_dim
      character(len=(34))so_seed_decimal
      character(len=(10))source_name((1):(6))
      integer,dimension(:),pointer::source_base_ptr
      integer,dimension(:),pointer::source_num_segments
      integer,dimension(:),pointer::source_type
      integer,dimension(:),pointer::source_geometry
      integer,dimension(:),pointer::source_num_flights
      integer,dimension(:),pointer::source_num_checkpoints
      integer,dimension(:),pointer::source_species
      integer,dimension(:),pointer::source_root_species
      integer,dimension(:),pointer::source_time_variation
      integer,dimension(:),pointer::source_num_gparameters
      integer,dimension(:),pointer::source_num_parameters
      integer,dimension(:),pointer::source_gparameters_list
      integer,dimension(:),pointer::source_parameters_list
      integer,dimension(:),pointer::source_gparameters_base
      integer,dimension(:),pointer::source_parameters_base
      integer,dimension(:),pointer::source_parameters_data_base
      REAL(kind=DOUBLE),dimension(:),pointer::source_gparameters_data
      REAL(kind=DOUBLE),dimension(:),pointer::source_parameters_data
      integer,dimension(:),pointer::source_num_giparameters
      integer,dimension(:),pointer::source_num_iparameters
      integer,dimension(:),pointer::source_giparameters_list
      integer,dimension(:),pointer::source_iparameters_list
      integer,dimension(:),pointer::source_giparameters_base
      integer,dimension(:),pointer::source_iparameters_base
      integer,dimension(:),pointer::source_iparameters_data_base
      integer,dimension(:),pointer::source_giparameters_data
      integer,dimension(:),pointer::source_iparameters_data
      REAL(kind=DOUBLE),dimension(:),pointer::source_total_current
      REAL(kind=DOUBLE),dimension(:),pointer::source_weight_norm
      REAL(kind=DOUBLE),dimension(:),pointer::source_scale_factor
      integer,dimension(:),pointer::source_segment_ptr
      REAL(kind=DOUBLE),dimension(:),pointer::source_current
      REAL(kind=DOUBLE),dimension(:),pointer::source_segment_rel_wt
      REAL(kind=DOUBLE),dimension(:),pointer::source_segment_prob_alias
      integer,dimension(:),pointer::source_segment_ptr_alias
      REAL(kind=DOUBLE)so_direct_delta
      end module so_mod
      
      
