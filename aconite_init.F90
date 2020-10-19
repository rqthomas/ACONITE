      module aconite_init

!-----------------------------------------------------------------------
!BOP
!
! !MODULE: aconite_init 
!
! !DESCRIPTION:
!
! !USES:

     use aconite_type
! !PUBLIC TYPES:
      implicit none
      save
      public :: initvars
      private :: init_state
      private :: init_flux
      private :: init_site
      private :: init_clim
      private :: init_param
      public :: init_io
      public :: init_marg
!
! !REVISION HISTORY:
!
! !OTHER LOCAL VARIABLES:

!EOP
!-----------------------------------------------------------------------

     contains


!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: initvars
!
! !INTERFACE:
      subroutine initvars()

! !USES:
!
! !ARGUMENTS:
      implicit none

!EOP
!
! LOCAL VARAIBLES:

!------------------------------------------------------------------------
       call init_io(io)
       call init_site(site)
       !call init_clim(clim)
       call init_param(param)
       call init_state(state)
       call init_flux(flux)
       call init_param(param)
       call init_marg(marg)
       end subroutine initvars

!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_state
!
! !INTERFACE:
      subroutine init_state(state)
!
! !DESCRIPTION:
!
! !ARGUMENTS:
      implicit none
      type (state_type), intent(inout):: state
!
! !REVISION HISTORY:
!
!EOP
!------------------------------------------------------------------------
      
          
          allocate(state%leafC)  
          allocate(state%woodC)
          allocate(state%rootC)
          allocate(state%leafN)
          allocate(state%woodN)
          allocate(state%rootN)
          allocate(state%labileC)
          allocate(state%labileC_bud)
          allocate(state%labileC_Ra)
          allocate(state%labileN)
          allocate(state%labileN_bud)
          allocate(state%totvegc)
          allocate(state%totvegn)
          allocate(state%totvegc_prev)
          allocate(state%totvegn_prev)
          allocate(state%litterC)
          allocate(state%litterN)
          allocate(state%soilC)
          allocate(state%soilN)
          allocate(state%cwdC)
          allocate(state%cwdN)
          allocate(state%nh4)
          allocate(state%no3) 
          allocate(state%psid)
          allocate(state%rtot)
          allocate(state%rooting_depth)
          allocate(state%root_length)
          allocate(state%root_surface_area)
          allocate(state%rootx)
          allocate(state%lai)
          allocate(state%rstep)
          allocate(state%Tave)
          allocate(state%Tday)
          allocate(state%Tnight)
          allocate(state%DayLength)
          allocate(state%NightLength)
          allocate(state%dayspan)
          allocate(state%totalN)
          allocate(state%totalN_prev)
          allocate(state%totalC)
          allocate(state%totalC_prev)
          allocate(state%litterC_prev)
          allocate(state%cwdc_prev)
          allocate(state%soilC_prev)
          allocate(state%litterN_prev)
          allocate(state%cwdN_prev)
          allocate(state%soilN_prev)
          allocate(state%minN_prev) 
          allocate(state%leafCN)
          allocate(state%woodCN)
          allocate(state%rootCN)
          allocate(state%target_leafCN)
          allocate(state%target_rootCN)
          allocate(state%target_leafC)
          allocate(state%target_leafN)
          allocate(state%target_rootC)
          allocate(state%target_rootN)
          allocate(state%GDDTot)
          allocate(state%LeafTurnoverTarget)
          allocate(state%Nuptake_downreg)
          allocate(state%Cuptake_downreg)
          allocate(state%MaxNstore)
          allocate(state%MaxCstore)
          allocate(state%maxleafC)
          allocate(state%hitmaxleafC)
          allocate(state%maxrootC)
          allocate(state%hitmaxrootC)
          allocate(state%maxRaC)
          allocate(state%maxRaC_integ)
          allocate(state%min_wood_deficit)
          allocate(state%min_root_deficit)
          allocate(state%prev_labileC)
          allocate(state%prev_labileN)
          allocate(state%annual_leafC)
          allocate(state%annual_woodC)
          allocate(state%annual_rootC)
          allocate(state%annual_leafN)
          allocate(state%annual_woodN)
          allocate(state%annual_rootN)
          allocate(state%annual_labileC)
          allocate(state%annual_labileC_bud)
          allocate(state%annual_labileC_Ra)
          allocate(state%annual_labileN)
          allocate(state%annual_labileN_bud)
          allocate(state%annual_targetleafCN)
          allocate(state%annual_targetrootCN)
          allocate(state%annual_litterC)
          allocate(state%annual_litterN) 
          allocate(state%annual_soilC)
          allocate(state%annual_soilN) 
          allocate(state%annual_cwdC)
          allocate(state%annual_cwdN) 
          allocate(state%annual_nh4) 
          allocate(state%annual_no3)
          allocate(state%annual_lai)
          allocate(state%annual_totvegc) 
          allocate(state%annual_totvegn) 
          allocate(state%annual_leafCN)
          allocate(state%annual_woodCN)
          allocate(state%annual_rootCN)
          allocate(state%annual_Nuptake_downreg)
          allocate(state%annual_Cuptake_downreg)
          allocate(state%annual_MaxNstore)
          allocate(state%annual_MaxCstore)
          allocate(state%a_leafC)
          allocate(state%a_woodC)
          allocate(state%a_rootC)
          allocate(state%a_NfixC)
          allocate(state%a_Ra_grow)
          allocate(state%annual_a_leafC)
          allocate(state%annual_a_woodC)
          allocate(state%annual_a_rootC)
          allocate(state%annual_a_NfixC)
          allocate(state%debug)
          allocate(state%debug2)
          allocate(state%wood_requirement)
          allocate(state%leafN_deficit)
          allocate(state%total_N_deficit)
          allocate(state%maxleafN)


          state%leafC= 100.		!100
          state%woodC =10000.		!5000
          state%rootC= 200.		!100
          state%leafN= state%leafC/param%leafCN 
          state%woodN= state%woodC/param%woodCN
          state%rootN= state%rootC/param%rootCN 
          state%labileC= 10.
          state%labileN= 4.
          state%labileC_bud=25.
          state%labileC_Ra= 150.
          state%labileN_bud= state%labileC_bud/param%leafCN 
          state%totvegc_prev=state%leafC+state%woodC+state%rootC+state%labileC+ state%labileC_bud + &
               state%labileC_Ra
          state%totvegn_prev=state%leafN+state%woodN+state%rootN+state%labileN+state%labileN_bud
          state%totvegc=0.0
          state%totvegn=0.0
          state%litterC=1800.
          state%litterN=100.
          state%soilC= 10000
          state%soilN= state%soilC/param%soilCN
          state%cwdC= 500
          state%cwdN= state%cwdC/param%woodCN
          state%totalC_prev = state%totvegc_prev + state%litterC + state%cwdC + state%soilC  
          state%nh4=2.1
          state%no3=0.7
          state%totalN_prev = state%totvegn_prev + state%litterN + state%cwdN+ state%soilN + state%nh4 + state%no3
          state%psid=-2.5
          state%rtot=0.1
          state%rooting_depth =0
          state%root_length =0
          state%root_surface_area =0
          state%rootx =0
          state%lai = 0
          state%rstep = 0 
          state%Tave = 0
          state%Tday = 0.0
          state%Tnight = 0.0
          state%DayLength = 0.0
          state%NightLength = 0.0
          state%dayspan = 0.0
          state%litterC_prev=state%litterC
          state%cwdc_prev=state%cwdC
          state%soilC_prev = state%soilC
          state%litterN_prev=state%litterN
          state%cwdN_prev=state%cwdN
          state%soilN_prev = state%soilN
          state%minN_prev = state%nh4 + state%no3
          state%leafCN = state%leafC/state%leafN
          state%woodCN = state%woodC/state%woodN
          state%rootCN = state%rootC/state%rootN
          state%target_leafCN = param%leafCN
          state%target_rootCN = state%rootCN
          state%GDDTot = 0.0
          state%LeafTurnoverTarget = 0.0
          state%Nuptake_downreg = 1.0
          state%Cuptake_downreg = 1.0
          state%MaxNstore = state%woodN * param%Nlabile_prop
          state%MaxCstore = state%rootC * param%Clabile_prop
          state%maxleafC = state%leafC !state%labileC_bud*2.0
          state%hitmaxleafC = 1
          state%maxrootC = state%rootC
          state%hitmaxrootC = 1
          state%maxRaC = 100.
          state%maxRaC_integ = 0.0
          state%min_wood_deficit = 0.0
          state%min_root_deficit = 0.0
          state%prev_labileC = state%labileC
          state%prev_labileN = state%labileN
          state%a_leafC = 0.0
          state%a_woodC = 0.0
          state%a_rootC = 0.0
          state%a_NfixC = 0.0
          state%a_Ra_grow = 0.0
          state%annual_leafC = 0.0
          state%annual_woodC = 0.0
          state%annual_rootC = 0.0
          state%annual_leafN = 0.0
          state%annual_woodN = 0.0
          state%annual_rootN = 0.0
          state%annual_labileC = 0.0
          state%annual_labileC_bud = 0.0
          state%annual_labileC_Ra = 0.0
          state%annual_labileN = 0.0
          state%annual_labileN_bud = 0.0
          state%annual_targetleafCN = 0.0
          state%annual_targetrootCN = 0.0
          state%annual_litterC = 0.0
          state%annual_litterN = 0.0
          state%annual_soilC = 0.0
          state%annual_soilN = 0.0
          state%annual_cwdC = 0.0
          state%annual_cwdN = 0.0
          state%annual_nh4 = 0.0
          state%annual_no3 = 0.0
          state%annual_lai = 0.0
          state%annual_totvegc = 0.0
          state%annual_totvegn = 0.0
          state%annual_leafCN = 0.0
          state%annual_woodCN = 0.0
          state%annual_rootCN = 0.0
          state%annual_Nuptake_downreg = 0.0
          state%annual_Cuptake_downreg = 0.0
          state%annual_MaxNstore = 0.0
          state%annual_MaxCstore = 0.0
          state%annual_a_leafC = 0.21
          state%annual_a_woodC = 0.47
          state%annual_a_rootC = 0.32
          state%annual_a_NfixC = 0.0
          state%debug = 0.0
          state%debug2 = 0.0
          state%wood_requirement = 0.0
          state%leafN_deficit = 0.0
          state%total_N_deficit = 0.0
          state%maxleafN=0.0

      end subroutine init_state

!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_state
!
! !INTERFACE:
      subroutine init_marg(marg)
!
! !DESCRIPTION:
!
! !ARGUMENTS:
      implicit none
      type (marg_type), intent(inout):: marg
!
! !REVISION HISTORY:
!
!EOP
!------------------------------------------------------------------------
      

          allocate(marg%Creturn_leafC)
          allocate(marg%Nreturn_leafC)
          allocate(marg%Creturn_rootC)
          allocate(marg%Nreturn_rootC)
          allocate(marg%Creturn_Raexcess)
          allocate(marg%Nreturn_Raexcess)
          allocate(marg%Creturn_leafN)
          allocate(marg%Nreturn_leafN)
          allocate(marg%Creturn_rootN)
          allocate(marg%Nreturn_rootN)
          allocate(marg%Creturn_leafCN)
          allocate(marg%Nreturn_rootCN)
          allocate(marg%GPP_leafC)
          allocate(marg%Rm_leafC)
          allocate(marg%Rg_leafC)
          allocate(marg%GPP_leafN)
          allocate(marg%addN_leafN)
          allocate(marg%Rm_leafN)
          allocate(marg%GPP_leafCN)
          allocate(marg%Rm_leafCN)
          allocate(marg%Rm_rootC)
          allocate(marg%Rg_rootC)
          allocate(marg%Nuptake_rootC)  
          allocate(marg%Rm_rootN)
          allocate(marg%Nuptake_rootN)   
          allocate(marg%addN_rootN)             
          allocate(marg%Nuptake_rootCN)    
          allocate(marg%Rg_woodC)
          allocate(marg%Nreturn_Raexcess) 
          allocate(marg%integ_Creturn_leafC)
          allocate(marg%integ_Nreturn_leafC)
          allocate(marg%integ_Creturn_rootC)
          allocate(marg%integ_Nreturn_rootC)
          allocate(marg%integ_Creturn_Raexcess)
          allocate(marg%integ_Nreturn_Raexcess)
          allocate(marg%integ_Creturn_leafN)
          allocate(marg%integ_Nreturn_leafN)
          allocate(marg%integ_Creturn_rootN)
          allocate(marg%integ_Nreturn_rootN)
          allocate(marg%integ_Creturn_leafCN)
          allocate(marg%integ_Nreturn_rootCN)
          allocate(marg%integ_GPP_leafC)
          allocate(marg%integ_Rm_leafC)
          allocate(marg%integ_Rg_leafC)
          allocate(marg%integ_GPP_leafN)
          allocate(marg%integ_addN_leafN)
          allocate(marg%integ_Rm_leafN)
          allocate(marg%integ_GPP_leafCN)
          allocate(marg%integ_Rm_leafCN)
          allocate(marg%integ_Rm_rootC)
          allocate(marg%integ_Rg_rootC)
          allocate(marg%integ_Nuptake_rootC)  
          allocate(marg%integ_Rm_rootN)
          allocate(marg%integ_Nuptake_rootN)   
          allocate(marg%integ_addN_rootN)             
          allocate(marg%integ_Nuptake_rootCN)    
          allocate(marg%integ_Rg_woodC)
          allocate(marg%integ_Nreturn_Raexcess) 
          allocate(marg%annual_Creturn_leafC)
          allocate(marg%annual_Nreturn_leafC)
          allocate(marg%annual_Creturn_rootC)
          allocate(marg%annual_Nreturn_rootC)
          allocate(marg%annual_Creturn_Raexcess)
          allocate(marg%annual_Nreturn_Raexcess)
          allocate(marg%annual_Creturn_leafN)
          allocate(marg%annual_Nreturn_leafN)
          allocate(marg%annual_Creturn_rootN)
          allocate(marg%annual_Nreturn_rootN)
          allocate(marg%annual_Creturn_leafCN)
          allocate(marg%annual_Nreturn_rootCN)
          allocate(marg%annual_GPP_leafC)
          allocate(marg%annual_Rm_leafC)
          allocate(marg%annual_Rg_leafC)
          allocate(marg%annual_GPP_leafN)
          allocate(marg%annual_addN_leafN)
          allocate(marg%annual_Rm_leafN)
          allocate(marg%annual_GPP_leafCN)
          allocate(marg%annual_Rm_leafCN)
          allocate(marg%annual_Rm_rootC)
          allocate(marg%annual_Rg_rootC)
          allocate(marg%annual_Nuptake_rootC)  
          allocate(marg%annual_Rm_rootN)
          allocate(marg%annual_Nuptake_rootN)   
          allocate(marg%annual_addN_rootN)             
          allocate(marg%annual_Nuptake_rootCN)    
          allocate(marg%annual_Rg_woodC)
          allocate(marg%annual_Nreturn_Raexcess) 
          allocate(marg%mean_leafC)
          allocate(marg%mean_leafN)
          allocate(marg%mean_rootC)
          allocate(marg%mean_rootN)
          allocate(marg%day_count_leaf)  
          allocate(marg%day_count_root)
          allocate(marg%integ_hitmaxleafC)
          allocate(marg%integ_hitmaxrootC)   
          allocate(marg%integ_hitwood_requirement)   
          allocate(marg%annual_cstore_ratio)
          allocate(marg%integ_cstore_ratio)
          allocate(marg%annual_nstore_ratio)
          allocate(marg%integ_nstore_ratio)
          allocate(marg%integ_total_N_deficit)
                    
          marg%Creturn_leafC=0.0
          marg%Nreturn_leafC=0.0
          marg%Creturn_rootC=0.0
          marg%Nreturn_rootC=0.0
          marg%Creturn_Raexcess=0.0
          marg%Nreturn_Raexcess=0.0
          marg%Creturn_leafN=0.0
          marg%Nreturn_leafN=0.0
          marg%Creturn_rootN=0.0
          marg%Nreturn_rootN=0.0
          marg%Creturn_leafCN=0.0
          marg%Nreturn_rootCN=0.0
          marg%GPP_leafC=0.0
          marg%Rm_leafC=0.0
          marg%Rg_leafC=0.0
          marg%GPP_leafN=0.0
          marg%addN_leafN=0.0
          marg%Rm_leafN=0.0
          marg%GPP_leafCN=0.0
          marg%Rm_leafCN=0.0
          marg%Rm_rootC=0.0
          marg%Rg_rootC=0.0
          marg%Nuptake_rootC=0.0  
          marg%Rm_rootN=0.0
          marg%Nuptake_rootN=0.0   
          marg%addN_rootN=0.0             
          marg%Nuptake_rootCN=0.0    
          marg%Rg_woodC=0.0
          marg%Nreturn_Raexcess=0.0 
          marg%integ_Creturn_leafC=0.0
          marg%integ_Nreturn_leafC=0.0
          marg%integ_Creturn_rootC=0.0
          marg%integ_Nreturn_rootC=0.0
          marg%integ_Creturn_Raexcess=0.0
          marg%integ_Nreturn_Raexcess=0.0
          marg%integ_Creturn_leafN=0.0
          marg%integ_Nreturn_leafN=0.0
          marg%integ_Creturn_rootN=0.0
          marg%integ_Nreturn_rootN=0.0
          marg%integ_Creturn_leafCN=0.0
          marg%integ_Nreturn_rootCN=0.0
          marg%integ_GPP_leafC=0.0
          marg%integ_Rm_leafC=0.0
          marg%integ_Rg_leafC=0.0
          marg%integ_GPP_leafN=0.0
          marg%integ_addN_leafN=0.0
          marg%integ_Rm_leafN=0.0
          marg%integ_GPP_leafCN=0.0
          marg%integ_Rm_leafCN=0.0
          marg%integ_Rm_rootC=0.0
          marg%integ_Rg_rootC=0.0
          marg%integ_Nuptake_rootC=0.0  
          marg%integ_Rm_rootN=0.0
          marg%integ_Nuptake_rootN=0.0   
          marg%integ_addN_rootN=0.0             
          marg%integ_Nuptake_rootCN=0.0    
          marg%integ_Rg_woodC=0.0
          marg%integ_Nreturn_Raexcess=0.0 
          marg%annual_Creturn_leafC=0.0
          marg%annual_Nreturn_leafC=0.0
          marg%annual_Creturn_rootC=0.0
          marg%annual_Nreturn_rootC=0.0
          marg%annual_Creturn_Raexcess=0.0
          marg%annual_Nreturn_Raexcess=0.0
          marg%annual_Creturn_leafN=0.0
          marg%annual_Nreturn_leafN=0.0
          marg%annual_Creturn_rootN=0.0
          marg%annual_Nreturn_rootN=0.0
          marg%annual_Creturn_leafCN=0.0
          marg%annual_Nreturn_rootCN=0.0
          marg%annual_GPP_leafC=0.0
          marg%annual_Rm_leafC=0.0
          marg%annual_Rg_leafC=0.0
          marg%annual_GPP_leafN=0.0
          marg%annual_addN_leafN=0.0
          marg%annual_Rm_leafN=0.0
          marg%annual_GPP_leafCN=0.0
          marg%annual_Rm_leafCN=0.0
          marg%annual_Rm_rootC=0.0
          marg%annual_Rg_rootC=0.0
          marg%annual_Nuptake_rootC  =0.0
          marg%annual_Rm_rootN=0.0
          marg%annual_Nuptake_rootN   =0.0
          marg%annual_addN_rootN    =0.0         
          marg%annual_Nuptake_rootCN =0.0   
          marg%annual_Rg_woodC=0.0
          marg%annual_Nreturn_Raexcess=0.0 
          marg%mean_leafC=0.0
          marg%mean_leafN=0.0
          marg%mean_rootC=0.0
          marg%mean_rootN=0.0
          marg%day_count_leaf  =0.0 
          marg%day_count_root = 0.0
          marg%integ_hitmaxleafC =1
          marg%integ_hitmaxrootC = 1 
          marg%integ_hitwood_requirement = 1    
          marg%annual_cstore_ratio = 0.0
          marg%integ_cstore_ratio =0.0
          marg%annual_nstore_ratio = 0.0
          marg%integ_nstore_ratio = 0.0
          marg%integ_total_N_deficit = 0.0
          
      end subroutine init_marg

!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_site
!
! !INTERFACE:
      subroutine init_site(site)!
! !DESCRIPTION:
!
! !ARGUMENTS:
      implicit none
      type (site_type), intent(inout):: site
!
! !REVISION HISTORY:
!
!EOP
!------------------------------------------------------------------------
      allocate(site%Lat)
      allocate(site%seasonal)
      
      site%Lat = -3.0 
      site%seasonal = 0


      end subroutine init_site


!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_clim
!
! !INTERFACE:
      subroutine init_clim(clim)
!
! !DESCRIPTION:
!
! !ARGUMENTS:
      implicit none
      type (clim_type), intent(inout):: clim
!
! !REVISION HISTORY:
!
!EOP
!------------------------------------------------------------------------
      allocate(clim%tmin(1))
      allocate(clim%tmax(1))
      allocate(clim%doy(1))
      allocate(clim%CO2(1))
      allocate(clim%rad(1))
      allocate(clim%prec(1))
      allocate(clim%year(1))
      allocate(clim%NO3dep(1))
      allocate(clim%NH4dep(1))

      end subroutine init_clim
!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_flux
!
! !INTERFACE:
      subroutine init_flux(flux)
!
! !DESCRIPTION:
!
! !ARGUMENTS:
      implicit none
      type (flux_type), intent(inout):: flux
!
! !REVISION HISTORY:
!
!EOP
!------------------------------------------------------------------------

      allocate(flux%gpp)
      allocate(flux%npp)
      allocate(flux%NEE)
      allocate(flux%Ra_main)
      allocate(flux%Ra_retrans)
      allocate(flux%Ra_grow)
      allocate(flux%Ra_myc)
      allocate(flux%Ra_excessC)
      allocate(flux%Ra_total)
      allocate(flux%Rh_total)
      allocate(flux%a_leafC)
      allocate(flux%a_rootC)
      allocate(flux%a_woodC)
      allocate(flux%a_labileC)
      allocate(flux%a_labileC_bud_2leaf)
      allocate(flux%a_labileN_bud_2leaf)
      allocate(flux%a_labileC_bud)
      allocate(flux%a_labileN_bud)
      allocate(flux%a_labileC_2Ra)
      allocate(flux%t_leafC)
      allocate(flux%t_rootC)
      allocate(flux%t_woodC)
      allocate(flux%t_cwdC)
      allocate(flux%t_litterC)
      allocate(flux%t_soilC)
      allocate(flux%a_leafN)
      allocate(flux%a_rootN)
      allocate(flux%a_woodN)
      allocate(flux%a_labileN)
      allocate(flux%t_leafN)
      allocate(flux%t_rootN)
      allocate(flux%t_woodN)
      allocate(flux%t_cwdN)
      allocate(flux%t_litterN)
      allocate(flux%t_soilN)
      allocate(flux%no3_uptake)
      allocate(flux%nh4_uptake)
      allocate(flux%no3_immob)
      allocate(flux%nh4_immob)
      allocate(flux%net_nmin)
      allocate(flux%nitr)
      allocate(flux%leachN)
      allocate(flux%leachDON)
      allocate(flux%Nfix)
      allocate(flux%Ndep_nh4)
      allocate(flux%Ndep_no3)
      allocate(flux%retransN)
      allocate(flux%annual_GPP)
      allocate(flux%annual_NPP)
      allocate(flux%annual_NEE)
      allocate(flux%annual_Rh)
      allocate(flux%annual_Nuptake)
      allocate(flux%annual_immob)
      allocate(flux%annual_netNmin)
      allocate(flux%annual_nitr)
      allocate(flux%annual_leachN)
      allocate(flux%annual_Ndep)
      allocate(flux%annual_Nfix)
      allocate(flux%annual_litterfallC)
      allocate(flux%annual_litterfallN)
      allocate(flux%annual_Ra)
      allocate(flux%annual_Ra_grow)
      allocate(flux%annual_Ra_main)
      allocate(flux%annual_Ra_excessC)
      allocate(flux%annual_retransN)
      allocate(flux%annual_a_leafC)
      allocate(flux%annual_a_woodC)
      allocate(flux%annual_a_rootC)
      allocate(flux%annual_a_leafN)
      allocate(flux%annual_a_woodN)
      allocate(flux%annual_a_rootN)
      allocate(flux%annual_t_leafC)
      allocate(flux%annual_t_woodC)
      allocate(flux%annual_t_rootC)
      allocate(flux%annual_t_leafN)
      allocate(flux%annual_t_woodN)
      allocate(flux%annual_t_rootN)

      flux%gpp=0.0
      flux%npp=0.0
      flux%NEE = 0.0
      flux%Ra_main=0.0
      flux%Ra_retrans=0.0
      flux%Ra_grow=0.0
      flux%Ra_myc=0.0
      flux%Ra_excessC = 0.0
      flux%Ra_total = 0.0
      flux%Rh_total = 0.0
      flux%a_leafC=0.0
      flux%a_rootC=0.0
      flux%a_woodC=0.0
      flux%a_labileC=0.0
      flux%a_labileC_bud_2leaf=0.0
      flux%a_labileN_bud_2leaf =0.0
      flux%a_labileC_bud = 0.0
      flux%a_labileN_bud = 0.0
      flux%a_labileC_2Ra = 0.0
      flux%t_leafC=0.0
      flux%t_rootC=0.0
      flux%t_woodC=0.0
      flux%t_cwdC=0.0
      flux%t_litterC=0.0
      flux%t_soilC=0.0
      flux%a_leafN=0.0
      flux%a_rootN=0.0
      flux%a_woodN=0.0
      flux%a_labileN=0.0
      flux%t_leafN=0.0
      flux%t_rootN=0.0
      flux%t_woodN=0.0
      flux%t_cwdN=0.0
      flux%t_litterN=0.0
      flux%t_soilN=0.0
      flux%no3_uptake=0.0
      flux%nh4_uptake=0.0
      flux%no3_immob=0.0
      flux%nh4_immob=0.0
      flux%net_nmin=0.0
      flux%nitr=0.0
      flux%leachN=0.0
      flux%leachDON = 0.0
      flux%Nfix=0.0
      flux%Ndep_nh4 = 0.0
      flux%Ndep_no3 = 0.0
      flux%retransN =0.0
      flux%annual_GPP = 0.0
      flux%annual_NPP = 0.0
      flux%annual_NEE = 0.0
      flux%annual_Rh = 0.0
      flux%annual_Nuptake = 0.0
      flux%annual_immob = 0.0
      flux%annual_netNmin = 0.0
      flux%annual_nitr = 0.0
      flux%annual_leachN = 0.0
      flux%annual_Ndep= 0.0
      flux%annual_Nfix= 0.0
      flux%annual_litterfallC = 0.0
      flux%annual_litterfallN = 0.0
      flux%annual_Ra = 0.0
      flux%annual_Ra_grow = 0.0
      flux%annual_Ra_main = 0.0
      flux%annual_Ra_excessC = 0.0
      flux%annual_retransN = 0.0
      flux%annual_a_leafC = 0.0
      flux%annual_a_woodC = 0.0
      flux%annual_a_rootC = 0.0
      flux%annual_a_leafN = 0.0
      flux%annual_a_woodN = 0.0
      flux%annual_a_rootN = 0.0
      flux%annual_t_leafC = 0.0
      flux%annual_t_woodC = 0.0
      flux%annual_t_rootC = 0.0
      flux%annual_t_leafN = 0.0
      flux%annual_t_woodN = 0.0
      flux%annual_t_rootN = 0.0

      end subroutine init_flux

!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_param
!
! !INTERFACE:
      subroutine init_param(param)
!
! !DESCRIPTION:
!
! !ARGUMENTS:
      implicit none
      type (param_type), intent(inout):: param
!
! !REVISION HISTORY:
!
!EOP
!------------------------------------------------------------------------

      allocate(param%root_radius)
      allocate(param%bufferNH4)
      allocate(param%diffNH4)
      allocate(param%bufferNO3)
      allocate(param%diffNO3)
      allocate(param%imax)
      allocate(param%rooting_depth)
      allocate(param%vo)
      allocate(param%km)
      allocate(param%a1)
      allocate(param%a2)
      allocate(param%a3)
      allocate(param%a4)
      allocate(param%a5)
      allocate(param%a6)
      allocate(param%a7)
      allocate(param%a8)
      allocate(param%a9)
      allocate(param%a10)
      allocate(param%a11)
      allocate(param%lca)
      allocate(param%t_leaf)
      allocate(param%t_wood)
      allocate(param%t_root)
      allocate(param%t_excessC)
      allocate(param%t_cwd)
      allocate(param%trans_frac)
      allocate(param%t_litter)
      allocate(param%mCUE)
      allocate(param%t_soil) 
      allocate(param%Ra_per_N)  !NOT CURRENTLY USED
      allocate(param%Ra_grow)
      allocate(param%nitr_rate)
      allocate(param%leach_rate)
      allocate(param%soilCN)
      allocate(param%leafCN)  !NOT A PARAMETER
      allocate(param%woodCN)
      allocate(param%rootCN)
      allocate(param%C_conc)
      allocate(param%root_density)
      allocate(param%Ra_Q10)
      allocate(param%Rh_Q10)
      allocate(param%GDDStart)
      allocate(param%SenescStart)
      allocate(param%Clabile_prop)
      allocate(param%Nlabile_prop)
      allocate(param%RaClabile_prop)
      allocate(param%Nturn_dep_loss)
      allocate(param%add_C)  !NOT AN IMPORTANT PARAMETER
      allocate(param%Nfix_per_gC) 
      allocate(param%leaf_resp_A)
      allocate(param%leaf_resp_B)
      allocate(param%root_resp_A)  !SAME AS LEAF RESP
      allocate(param%root_resp_B)  !SAME AS LEAF RESP
      allocate(param%MaxAllocAdjust)
      allocate(param%Minleaf2woodAlloc)
      allocate(param%LeafC2budCprop)
      allocate(param%leaf2root_ratio)
      allocate(param%use_reich)
      allocate(param%stocks_close_prop)
      allocate(param%growth_potential_param)
      allocate(param%leaf_shed_rate)

      param%root_radius=(0.3*0.001) !meters
      param%bufferNH4=10. !Soil buffere power (unitless)
      param%diffNH4=1*(10e-11) ! Effective diffusion coefficient m-2 s-1
      param%bufferNO3=2*(10e-10) !Soil buffer power (unitless)
      param%diffNO3 = 0.5 ! Effective diffusion coefficient m-2 s-1
      param%imax=4*(10e-5) !Maximal N influx rate per root length mmol m-2 s-1
      param%rooting_depth = 1.0  ! rooting depth in meters
      param%vo = 1E-09!inward radial velocity at the root surface (ms-1) - functionof ET 
      param%km=15. !N uptake half saturation mmol/m3
      param%C_conc = 0.5  ! g C / g DW 
      param%root_density = 640000 ! m2 / gDW of root 
      param%a1= 6.0 !ACM parameter
      param%a2= 0.01569 !ACM parameter
      param%a3=  4.223 !ACM parameter
      param%a4= 208.86 !ACM parameter
      param%a5=  0.04531 !ACM parameter
      param%a6= 0.378 !ACM parameter
      param%a7= 7.193 !ACM parameter
      param%a8= 0.0111 !ACM parameter
      param%a9= 2.10 !ACM parameter
      param%a10= 0.7897 !ACM parameter
      param%a11 = 0.5 ! ACM LAI vs N 
      param%lca = 40. ! m2 / gC leaf
      param%t_leaf =1./(365.) !day-1
      param%t_wood=1./(365.*50.) !day-1
      param%t_root=1./(365.*0.7) ! day-1
      param%t_excessC = 0.1
      param%t_cwd = 1./(3.*365.) !day-1
      param%trans_frac=0.5 !proportion of leafN retranslocated
      param%t_litter=1./(365.) ! day-1
      param%mCUE=0.5 !microbial carbon use efficiency (proportion)
      param%t_soil=1./(365.*10.) ! day-1
      param%nitr_rate = 0.01 !g N gNH4-1 day-1
      param%leach_rate = 0.01! g N gN03-1 day-1
      param%Nturn_dep_loss = 0.02
      param%Ra_Q10 = 1.5
      param%Rh_Q10 = 2.0
      param%Ra_per_N = 0.0548 !0.0548 !(2.525e-6)*(60.*60.*24.)!0.0548 ! 0.2181 ! 0.0548 ! (2.525e-6)*(60.*60.*24.) ! g C gN -1
      param%leaf_resp_A=0.833 !Reich et al. Eco Letters Table 1 Organ= All, Plant group = All
      param%leaf_resp_B=1.268  !Reich et al. Eco Letters Table 1 Organ= All, Plant group = All
      param%root_resp_A=0.833  !Reich et al. Eco Letters Table 1 Organ= All, Plant group = All
      param%root_resp_B=1.268  !Reich et al. Eco Letters Table 1 Organ=All, Plant group = All
      param%Ra_grow = 0.25 !proportion
      param%soilCN = 15.  ! Soil C:N
      param%leafCN = 25.
      param%woodCN = 500.
      param%rootCN = 45.
      param%GDDStart = 100
      param%SenescStart = 288
      param%Clabile_prop = 0.25
      param%Nlabile_prop = 0.10
      param%RaClabile_prop = 0.03
      param%add_C = 1.0
      param%Nfix_per_gC = 1.0/9.0
      param%MaxAllocAdjust = 0.05
      param%Minleaf2woodAlloc = 1.0
      param%LeafC2budCprop = 0.5
      param%leaf2root_ratio = 1.5
      param%use_reich = 1.0
      param%stocks_close_prop = 0.95
      param%growth_potential_param = 0.07
      param%leaf_shed_rate = 0.5

      end subroutine init_param  
!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_io
!
! !INTERFACE:
      subroutine init_io(io)
!
! !DESCRIPTION:
!
! !ARGUMENTS:
      implicit none
      type (input_output_type), intent(inout):: io
!
! !REVISION HISTORY:
!
!EOP
!------------------------------------------------------------------------

      allocate(io%clim_in)
      allocate(io%param_in)
      allocate(io%site_in)
      allocate(io%day_out)
      allocate(io%annual_out)
      allocate(io%restart_in)
      allocate(io%restart_out)
      allocate(io%sim_length)
      allocate(io%clim_length)
      allocate(io%print_year_start)
      allocate(io%print_year_end)
      allocate(io%annual_state_doy)
      allocate(io%cold_start)
      end subroutine init_io


!------------------------------------------------------------------------


      end module aconite_init 
