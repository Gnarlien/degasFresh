      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module xs_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer xs_data_size
      integer xs_num_dep_var
      integer xs_rank((1):(20))
      integer xs_tab_index((1):(3),(1):(20))
      integer xs_data_base((1):(20))
      integer xs_data_inc((1):(20))
      character(len=(24))xs_name
      character(len=(40))xs_spacing((0):(3),(1):(20))
      character(len=(40))xs_var((0):(3),(1):(20))
      character(len=(12))xs_units((0):(3),(1):(20))
      character(len=(40))xs_eval_name((1):(20))
      REAL(kind=DOUBLE)xs_min((1):(3),(1):(20))
      REAL(kind=DOUBLE)xs_max((1):(3),(1):(20))
      REAL(kind=DOUBLE)xs_mult((0):(3),(1):(20))
      REAL(kind=DOUBLE),dimension(:),pointer::xs_data_tab
      character(len=(300))xsection_version
      end module xs_mod
      
      
