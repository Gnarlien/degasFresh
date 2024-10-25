      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module pm_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer pm_num
      integer pm_ignore
      character(len=(80)),dimension(:),pointer::pmi_name
      character(len=(32)),dimension(:),pointer::pmi_type
      character(len=(24)),dimension(:),pointer::pmi_sy
      integer,dimension(:),pointer::pmi_reagent
      integer,dimension(:),pointer::pmi_materials
      integer,dimension(:),pointer::pmi_generic
      integer,dimension(:),pointer::pmi_product_num
      integer,dimension(:,:),pointer::pmi_product
      character(len=(96)),dimension(:),pointer::pmi_filename
      character(len=(300))pmi_version
      end module pm_mod
      
      
