      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      module pr_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer pr_test_num
      integer pr_background_num
      integer pr_ex_test_num
      integer pr_ex_test_dim
      integer pr_reaction_num
      integer pr_reaction_dim
      integer pr_bkrc_num
      integer pr_bkrc_dim
      integer pr_exrc_num
      integer pr_exrc_dim
      integer pr_materials_num
      integer pr_pmi_num
      integer pr_var0_num
      integer,dimension(:),pointer::problem_species_test
      integer,dimension(:),pointer::problem_species_background
      integer,dimension(:),pointer::problem_materials_sub
      integer,dimension(:),pointer::problem_test_sp
      integer,dimension(:),pointer::problem_background_sp
      integer,dimension(:),pointer::problem_ex_test_sp
      integer,dimension(:),pointer::problem_materials_ref
      integer,dimension(:),pointer::problem_rc
      integer,dimension(:),pointer::problem_reaction_num
      integer,dimension(:,:),pointer::problem_test_reaction
      integer,dimension(:,:),pointer::problem_test_background
      integer,dimension(:,:),pointer::problem_num_arrangements
      integer,dimension(:,:,:,:),pointer::problem_test_products
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::problem_prod_mult
      integer,dimension(:),pointer::problem_background_reaction
      integer,dimension(:,:),pointer::problem_bkrc_reagents
      integer,dimension(:,:),pointer::problem_bkrc_products
      integer,dimension(:),pointer::problem_external_reaction
      integer,dimension(:),pointer::problem_external_test_reaction_num
      integer,dimension(:,:),pointer::problem_external_test_reaction
      integer,dimension(:,:),pointer::problem_external_test_background
      integer,dimension(:,:,:),pointer::problem_exrc_products
      integer,dimension(:),pointer::problem_pmi_ref
      integer,dimension(:),pointer::problem_pmi_sub
      integer,dimension(:),pointer::problem_pmi_case_num
      integer,dimension(:,:),pointer::problem_pmi_cases
      integer,dimension(:,:),pointer::problem_pmi_num_arrange
      integer,dimension(:,:,:,:),pointer::problem_pmi_products
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::problem_pmi_prod_mult
      character(len=(40)),dimension(:),pointer::pr_var0_list
      character(len=(300))problem_version
      end module pr_mod
      
      module ps_mod
      implicit none
      integer,parameter,private::SINGLE=kind(0.0),DOUBLE=selected_real_k
     &ind(12)
      save
      integer num_generics
      integer,dimension(:),pointer::generics
      integer,dimension(:),pointer::num_equiv
      integer,dimension(:,:),pointer::equivalents
      end module ps_mod
      
      
