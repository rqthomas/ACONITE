      module aconite_type

!-----------------------------------------------------------------------
!BOP
!
! !MODULE: aconite_type
!
! !DESCRIPTION:

!
! !USES:

! !PUBLIC TYPES:
      implicit none
      private 
      !public :: initvars
!
! !REVISION HISTORY:
!


      type,public :: state_type
         real,pointer :: leafC  
         real,pointer :: woodC
         real,pointer :: rootC
         real, pointer :: leafN
         real, pointer :: woodN
         real, pointer :: rootN 
         real ,pointer :: labileC
         real , pointer :: labileN
         real ,pointer :: labileC_bud
         real, pointer :: labileN_bud
         real ,pointer :: labileC_Ra
         real, pointer :: litterC
         real, pointer :: litterN
         real, pointer :: soilC
         real, pointer :: soilN
         real, pointer :: cwdC
         real, pointer :: cwdN
         real, pointer :: nh4
         real, pointer :: no3
         real,pointer :: psid
         real,pointer :: rtot
         real,pointer :: rooting_depth
         real,pointer :: root_length
         real,pointer :: root_surface_area
         real, pointer :: rootx
         real, pointer :: lai
         real, pointer :: totvegc
         real,pointer :: totvegn
         real,pointer :: totvegc_prev
         real,pointer :: totvegn_prev
         integer,pointer :: rstep
         real,pointer :: Tave
          real, pointer :: Tday
          real, pointer :: Tnight
          real, pointer :: DayLength
          real, pointer :: NightLength
          real, pointer :: dayspan
          real, pointer :: totalN
          real, pointer :: totalN_prev
          real,pointer :: totalC
          real,pointer :: totalC_prev
          real,pointer :: litterC_prev
          real,pointer :: cwdC_prev
          real,pointer :: soilC_prev
          real,pointer :: litterN_prev
          real,pointer :: cwdN_prev
          real,pointer :: soilN_prev
          real,pointer :: minN_prev
          real,pointer :: leafCN
          real,pointer :: woodCN
          real,pointer :: rootCN
          real,pointer :: target_leafCN
          real,pointer :: target_rootCN
          real,pointer :: target_leafC
          real,pointer :: target_leafN
          real,pointer :: target_rootC
          real,pointer :: target_rootN
          real,pointer :: GDDTot
          real,pointer :: LeafTurnoverTarget
          real,pointer :: Nuptake_downreg
          real,pointer :: Cuptake_downreg
          real,pointer :: MaxNstore
          real,pointer :: MaxCstore   
          real,pointer :: maxleafC
          real,pointer :: hitmaxleafC
          real,pointer :: maxrootC
          real,pointer :: hitmaxrootC
          real,pointer :: maxRaC  
          real,pointer :: maxRaC_integ  
          real,pointer :: min_wood_deficit
          real,pointer :: min_root_deficit
          real,pointer :: prev_labileC
          real,pointer :: prev_labileN
          real,pointer :: a_leafC
          real,pointer :: a_woodC
          real,pointer :: a_rootC
          real,pointer :: a_NfixC
          real,pointer :: a_Ra_grow
          real,pointer :: annual_leafC
          real,pointer :: annual_woodC
          real,pointer :: annual_rootC
          real, pointer :: annual_leafN
          real, pointer :: annual_woodN
          real, pointer :: annual_rootN
          real ,pointer :: annual_labileC
          real, pointer :: annual_labileC_bud
          real, pointer :: annual_labileC_Ra
          real, pointer :: annual_labileN
          real, pointer :: annual_labileN_bud
          real, pointer :: annual_targetleafCN
          real, pointer :: annual_targetrootCN
          real, pointer :: annual_litterC
          real, pointer :: annual_litterN
          real, pointer :: annual_soilC
          real, pointer :: annual_soilN
          real, pointer :: annual_cwdC
          real, pointer :: annual_cwdN
          real, pointer :: annual_nh4
          real, pointer :: annual_no3
          real, pointer :: annual_lai
          real, pointer :: annual_totvegc
          real,pointer :: annual_totvegn
          real,pointer :: annual_leafCN
          real,pointer :: annual_woodCN
          real,pointer :: annual_rootCN
          real,pointer :: annual_Nuptake_downreg
          real,pointer :: annual_Cuptake_downreg
          real,pointer :: annual_MaxNstore
          real,pointer :: annual_MaxCstore
          real,pointer :: annual_a_leafC
          real,pointer :: annual_a_woodC
          real,pointer :: annual_a_rootC
          real,pointer :: annual_a_NfixC
          real,pointer :: integ_N_surplus
          real,pointer :: integ_C_surplus
          real,pointer :: debug
          real,pointer :: debug2
          real,pointer :: wood_requirement
          real,pointer :: hitNfix
          real,pointer :: leafN_deficit
          real,pointer :: total_N_deficit
          real, pointer :: maxleafN
      end type state_type

      type,public :: marg_type   
          real,pointer :: Creturn_leafC
          real,pointer :: Nreturn_leafC
          real,pointer :: Creturn_rootC
          real,pointer :: Nreturn_rootC
          real,pointer :: Creturn_Raexcess
          real,pointer :: Nreturn_Raexcess
          real,pointer :: Creturn_leafN
          real,pointer :: Nreturn_leafN
          real,pointer :: Creturn_rootN
          real,pointer :: Nreturn_rootN
          real,pointer :: Creturn_leafCN
          real,pointer :: Nreturn_rootCN
          real,pointer :: GPP_leafC
          real,pointer :: Rm_leafC
          real,pointer :: Rg_leafC
          real,pointer :: GPP_leafN
          real,pointer :: addN_leafN
          real,pointer :: Rm_leafN
          real,pointer :: GPP_leafCN
          real,pointer :: Rm_leafCN
          real,pointer :: Rm_rootC
          real,pointer :: Rg_rootC
          real,pointer :: Nuptake_rootC  
          real,pointer :: Rm_rootN
          real,pointer :: Nuptake_rootN   
          real,pointer :: addN_rootN             
          real,pointer :: Nuptake_rootCN    
          real,pointer :: Rg_woodC 
          real,pointer :: integ_Creturn_leafC
          real,pointer :: integ_Nreturn_leafC
          real,pointer :: integ_Creturn_rootC
          real,pointer :: integ_Nreturn_rootC
          real,pointer :: integ_Creturn_Raexcess
          real,pointer :: integ_Nreturn_Raexcess
          real,pointer :: integ_Creturn_leafN
          real,pointer :: integ_Nreturn_leafN
          real,pointer :: integ_Creturn_rootN
          real,pointer :: integ_Nreturn_rootN
          real,pointer :: integ_Creturn_leafCN
          real,pointer :: integ_Nreturn_rootCN
          real,pointer :: integ_GPP_leafC
          real,pointer :: integ_Rm_leafC
          real,pointer :: integ_Rg_leafC
          real,pointer :: integ_GPP_leafN
          real,pointer :: integ_addN_leafN
          real,pointer :: integ_Rm_leafN
          real,pointer :: integ_GPP_leafCN
          real,pointer :: integ_Rm_leafCN
          real,pointer :: integ_Rm_rootC
          real,pointer :: integ_Rg_rootC
          real,pointer :: integ_Nuptake_rootC  
          real,pointer :: integ_Rm_rootN
          real,pointer :: integ_Nuptake_rootN   
          real,pointer :: integ_addN_rootN             
          real,pointer :: integ_Nuptake_rootCN    
          real,pointer :: integ_Rg_woodC 
          real,pointer :: annual_Creturn_leafC
          real,pointer :: annual_Nreturn_leafC
          real,pointer :: annual_Creturn_rootC
          real,pointer :: annual_Nreturn_rootC
          real,pointer :: annual_Creturn_Raexcess
          real,pointer :: annual_Nreturn_Raexcess
          real,pointer :: annual_Creturn_leafN
          real,pointer :: annual_Nreturn_leafN
          real,pointer :: annual_Creturn_rootN
          real,pointer :: annual_Nreturn_rootN
          real,pointer :: annual_Creturn_leafCN
          real,pointer :: annual_Nreturn_rootCN
          real,pointer :: annual_GPP_leafC
          real,pointer :: annual_Rm_leafC
          real,pointer :: annual_Rg_leafC
          real,pointer :: annual_GPP_leafN
          real,pointer :: annual_addN_leafN
          real,pointer :: annual_Rm_leafN
          real,pointer :: annual_GPP_leafCN
          real,pointer :: annual_Rm_leafCN
          real,pointer :: annual_Rm_rootC
          real,pointer :: annual_Rg_rootC
          real,pointer :: annual_Nuptake_rootC  
          real,pointer :: annual_Rm_rootN
          real,pointer :: annual_Nuptake_rootN   
          real,pointer :: annual_addN_rootN             
          real,pointer :: annual_Nuptake_rootCN    
          real,pointer :: annual_Rg_woodC
          real,pointer :: mean_leafC
          real,pointer :: mean_leafN
          real,pointer :: mean_rootC
          real,pointer :: mean_rootN
          real,pointer :: day_count_leaf
          real,pointer :: day_count_root       
          real,pointer :: integ_hitmaxleafC
          real,pointer :: integ_hitmaxrootC
          real,pointer :: integ_hitwood_requirement
          real,pointer :: annual_cstore_ratio
          real,pointer :: integ_cstore_ratio
          real,pointer :: annual_nstore_ratio
          real,pointer :: integ_nstore_ratio
          real,pointer :: integ_total_N_deficit
      end type marg_type

      type,public :: flux_type
         real,pointer :: gpp
         real, pointer :: npp
         real,pointer :: NEE
         real,pointer :: Ra_main
         real,pointer :: Ra_retrans
         real,pointer :: Ra_grow
         real,pointer :: Ra_myc
         real,pointer :: Ra_total
         real,pointer :: Rh_total
         real,pointer :: Ra_excessC
         real,pointer :: a_leafC
         real,pointer :: a_rootC
         real,pointer :: a_woodC
         real,pointer :: a_labileC
         real,pointer :: a_labileC_bud_2leaf
         real,pointer :: a_labileN_bud_2leaf
         real,pointer :: a_labileC_bud
         real,pointer :: a_labileN_bud
         real,pointer :: a_labileC_2Ra
         real,pointer :: t_leafC
         real,pointer :: t_rootC
         real,pointer :: t_woodC
         real,pointer :: t_cwdC
         real,pointer :: t_litterC
         real,pointer :: t_soilC
         real,pointer :: a_leafN
         real,pointer :: a_rootN
         real,pointer :: a_woodN
         real,pointer :: a_labileN
         real,pointer :: t_leafN
         real,pointer :: t_rootN
         real,pointer :: t_woodN
         real,pointer :: t_cwdN
         real,pointer :: t_litterN
         real,pointer :: t_soilN
         real,pointer :: no3_uptake
         real,pointer :: nh4_uptake
         real,pointer :: no3_immob
         real,pointer :: nh4_immob
         real,pointer :: net_nmin
         real,pointer :: nitr
         real,pointer :: leachN
         real,pointer :: leachDON
         real,pointer :: Nfix
         real,pointer :: ndep_nh4
         real,pointer :: ndep_no3
         real,pointer :: retransN
         real,pointer :: annual_GPP
         real,pointer :: annual_NPP
         real,pointer :: annual_NEE
         real,pointer :: annual_Rh
         real,pointer :: annual_Nuptake
         real,pointer :: annual_immob
         real,pointer :: annual_netNmin
         real,pointer :: annual_leachN
         real,pointer :: annual_nitr
         real,pointer :: annual_Ndep
         real,pointer :: annual_Nfix
         real,pointer :: annual_litterfallC
         real,pointer :: annual_litterfallN
         real,pointer :: annual_Ra
         real,pointer :: annual_Ra_grow
         real,pointer :: annual_Ra_main
         real,pointer :: annual_Ra_excessC
         real,pointer :: annual_retransN
         real,pointer :: annual_a_leafC
         real,pointer :: annual_a_woodC
         real,pointer :: annual_a_rootC
         real,pointer :: annual_a_leafN
         real,pointer :: annual_a_woodN
         real,pointer :: annual_a_rootN
         real,pointer :: annual_t_leafC
         real,pointer :: annual_t_woodC
         real,pointer :: annual_t_rootC
         real,pointer :: annual_t_leafN
         real,pointer :: annual_t_woodN
         real,pointer :: annual_t_rootN
      
      end type flux_type

      type,public :: param_type
         real,pointer :: root_radius
         real,pointer :: bufferNH4
         real,pointer :: diffNH4
         real,pointer :: bufferNO3
         real,pointer :: diffNO3
         real,pointer :: imax
         real,pointer :: rooting_depth
         real,pointer :: vo
         real,pointer :: km
         real,pointer :: a1
         real,pointer :: a2
         real,pointer :: a3
         real,pointer :: a4
         real,pointer :: a5
         real,pointer :: a6 
         real,pointer :: a7
         real,pointer :: a8
         real,pointer :: a9
         real,pointer :: a10
         real,pointer :: a11
         real,pointer :: lca  
         real,pointer :: t_leaf
         real,pointer :: t_wood
         real,pointer :: t_root
         real,pointer :: t_cwd
         real,pointer :: trans_frac
         real,pointer :: t_litter
         real,pointer :: mCUE
         real,pointer :: t_soil
         real,pointer :: Ra_per_N
         real,pointer :: Ra_grow
         real,pointer :: nitr_rate
         real,pointer :: leach_rate
         real,pointer :: soilCN
         real,pointer :: leafCN
         real,pointer :: woodCN
         real,pointer :: rootCN
         real,pointer :: C_conc
         real,pointer :: root_density 
         real,pointer :: Ra_Q10
         real,pointer :: Rh_Q10
         real,pointer :: GDDStart
         real,pointer :: SenescStart
         real,pointer :: Clabile_prop
         real,pointer :: Nlabile_prop
         real,pointer :: RaClabile_prop
         real,pointer :: Nturn_dep_loss
         real,pointer :: add_C
         real,pointer :: t_excessC
         real,pointer :: Nfix_per_gC
         real,pointer :: leaf_resp_A
         real,pointer :: leaf_resp_B
         real,pointer :: root_resp_A
         real,pointer :: root_resp_B
         real,pointer :: MaxallocAdjust
         real,pointer :: Minleaf2woodAlloc
         real,pointer :: LeafC2budCprop
         real,pointer :: leaf2root_ratio
         real,pointer :: use_reich
         real,pointer :: stocks_close_prop
         real,pointer :: growth_potential_param
         real,pointer :: leaf_shed_rate
         
         
      end type param_type

      type, public :: clim_type
         real, pointer :: tmin(:)
         real, pointer :: tmax(:)
         real, pointer :: doy(:)
         real, pointer :: CO2(:)
         real, pointer :: prec(:)
         real, pointer :: year(:)
         real, pointer :: NO3dep(:)
         real, pointer :: NH4dep(:) 
         real, pointer :: rad(:)
      end type clim_type

      type, public :: site_type
          real, pointer :: Lat 
          integer, pointer :: seasonal
      end type site_type

      type,public :: input_output_type
          character(len=200),pointer :: clim_in 
          character(len=200),pointer :: param_in
          character(len=200),pointer :: site_in
          character(len=200),pointer :: day_out
          character(len=200),pointer :: annual_out
          character(len=200),pointer :: restart_in
          character(len=200),pointer :: restart_out
          integer, pointer :: sim_length
          integer, pointer :: clim_length
          integer, pointer :: print_year_start
          integer, pointer :: print_year_end
          integer, pointer :: annual_state_doy
          integer, pointer :: cold_start
      end type input_output_type

      type(state_type), public, target, save :: state  
      type(marg_type), public, target, save :: marg
      type(clim_type), public, target, save :: clim
      type(flux_type), public, target, save :: flux
      type(site_type), public, target, save :: site   
      type(param_type), public, target, save :: param
      type(input_output_type), public, target, save :: io

      end module aconite_type
