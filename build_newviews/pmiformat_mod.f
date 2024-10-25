      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module pf_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer pf_data_size
      integer pf_num_dep_var
      integer pf_rank((1):(5))
      integer pf_tab_index((1):(5),(1):(5))
      integer pf_data_base((1):(5))
      integer pf_data_inc((1):(5))
      character(len=(24))pf_name
      character(len=(20))pf_spacing((0):(5),(1):(5))
      character(len=(20))pf_var((0):(5),(1):(5))
      character(len=(12))pf_units((0):(5),(1):(5))
      character(len=(40))pf_eval_name((1):(5))
      REAL(kind=DOUBLE)pf_min((1):(5),(1):(5))
      REAL(kind=DOUBLE)pf_max((1):(5),(1):(5))
      REAL(kind=DOUBLE)pf_mult((0):(5),(1):(5))
      REAL(kind=DOUBLE),dimension(:),pointer::pf_data_tab
      character(len=(300))pf_version
      end module pf_mod
      
      
