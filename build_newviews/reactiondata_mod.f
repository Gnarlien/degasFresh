      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module rd_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer reaction_rate_size
      integer reaction_handling_size
      REAL(kind=DOUBLE),dimension(:,:),pointer::reaction_rate_min
      REAL(kind=DOUBLE),dimension(:,:),pointer::reaction_rate_delta
      integer,dimension(:),pointer::reaction_rate_rank
      integer,dimension(:,:),pointer::reaction_rate_spacing
      character(len=(40)),dimension(:),pointer::reaction_rate_eval_name
      integer,dimension(:,:),pointer::reaction_rate_tab_index
      integer,dimension(:,:),pointer::reaction_rate_var
      integer,dimension(:),pointer::reaction_rate_num_rand
      integer,dimension(:),pointer::reaction_rate_base
      REAL(kind=DOUBLE),dimension(:),pointer::reaction_rate_tab
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::reaction_handling_min
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::reaction_handling_delt
     &a
      integer,dimension(:,:),pointer::reaction_handling_rank
      integer,dimension(:,:,:),pointer::reaction_handling_spacing
      character(len=(40)),dimension(:,:),pointer::reaction_handling_eval
     &_name
      integer,dimension(:,:,:),pointer::reaction_handling_tab_index
      integer,dimension(:,:),pointer::reaction_handling_var0
      integer,dimension(:,:,:),pointer::reaction_handling_var
      integer,dimension(:),pointer::reaction_handling_num_rand
      integer,dimension(:,:),pointer::reaction_handling_base
      REAL(kind=DOUBLE),dimension(:),pointer::reaction_handling_tab
      end module rd_mod
      
      
