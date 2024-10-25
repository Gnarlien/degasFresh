      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module rc_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer rc_num
      character(len=(80)),dimension(:),pointer::reaction_name
      character(len=(32)),dimension(:),pointer::reaction_type
      character(len=(24)),dimension(:),pointer::reaction_sy
      integer,dimension(:),pointer::reaction_emitter
      integer,dimension(:),pointer::reaction_reagent_num
      integer,dimension(:),pointer::reaction_generic
      integer,dimension(:),pointer::reaction_product_num
      integer,dimension(:,:),pointer::reaction_reagent
      integer,dimension(:,:),pointer::reaction_product
      character(len=(96)),dimension(:),pointer::reaction_filename
      character(len=(300))reaction_version
      end module rc_mod
      
      
