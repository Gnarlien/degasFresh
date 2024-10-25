      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module pd_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer pmi_yield_size
      integer pmi_handling_size
      REAL(kind=DOUBLE),dimension(:,:),pointer::pmi_yield_min
      REAL(kind=DOUBLE),dimension(:,:),pointer::pmi_yield_delta
      integer,dimension(:),pointer::pmi_yield_rank
      integer,dimension(:,:),pointer::pmi_yield_spacing
      character(len=(40)),dimension(:),pointer::pmi_yield_eval_name
      integer,dimension(:,:),pointer::pmi_yield_tab_index
      integer,dimension(:,:),pointer::pmi_yield_var
      integer,dimension(:),pointer::pmi_yield_num_rand
      integer,dimension(:),pointer::pmi_yield_base
      REAL(kind=DOUBLE),dimension(:),pointer::pmi_yield_tab
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::pmi_handling_min
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::pmi_handling_delta
      integer,dimension(:,:),pointer::pmi_handling_rank
      integer,dimension(:,:,:),pointer::pmi_handling_spacing
      character(len=(40)),dimension(:,:),pointer::pmi_handling_eval_name
      integer,dimension(:,:,:),pointer::pmi_handling_tab_index
      character(len=(20)),dimension(:,:),pointer::pmi_handling_var0
      integer,dimension(:,:,:),pointer::pmi_handling_var
      integer,dimension(:),pointer::pmi_handling_num_rand
      integer,dimension(:,:),pointer::pmi_handling_base
      REAL(kind=DOUBLE),dimension(:),pointer::pmi_handling_tab
      integer pd_inc
      end module pd_mod
      
      
