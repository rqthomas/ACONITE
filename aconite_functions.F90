      module aconite_functions


! !PUBLIC MEMBER FUNCTIONS:
      public :: Nupt
      public :: Photosyn
      public :: BalanceCheck 
      public :: year_update
      public :: marginal_integrator
      public :: reich_resp
      public :: marginals
      public :: allocation_decision
      public :: zero_fluxes
!
! !REVISION HISTORY:
!
!EOP
!-----------------------------------------------------------------------

     contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: 
!
! !INTERFACE:  N uptake routine
      REAL FUNCTION Nupt(Ntype, rootC, rootN, woodC,avail_N,downreg, gpp_in, temp_response)
!
! !DESCRIPTION:
!
! !USES:
     use aconite_type
     implicit none 
!
! !ARGUMENTS:
!
! !CALLED FROM:
!
! !REVISION HISTORY:
!
! !LOCAL VARIABLES:
! local pointers to implicit in scalars
      real ::  rootC, rootN, woodC, gpp_in, temp_response,avail_N,downreg
      integer :: Ntype
!
!
! local pointers to implicit out scalars
!
!
! !OTHER LOCAL VARIABLES:
      real :: rooting_depth, pgamma, rootx, vo, delta, cav,root_length,root_surface_area
      real :: co, alpha, Up,Imax, tav
     real, parameter :: pi = 3.141592653589793239
!EOP
!-----------------------------------------------------------------------
    
        tav = (clim%tmax(state%rstep) + clim%tmin(state%rstep))/2.

        !some parameters  differ for NO3 vs NH4
        rooting_depth = param%rooting_depth
        !root length (m)  [param%C_density = gC m-3 root]
        root_length=((rootC/param%C_conc)/param%root_density)/(pi*param%root_radius**2)
        root_surface_area = 2.* pi * param%root_radius * root_length!root surface area, m2
        rootx = SQRT(rooting_depth/(pi*root_length))!mean half distance between roots (m)

        if(root_surface_area > 0 .AND. avail_N > 0. .and.tav.gt.0. .AND. gpp_in > 0) then
            vo = param%vo * gpp_in		!transpiration is assumed proportional to GPP    
            IF(ntype.eq.1)THEN!NH4
               !parameter related to buffer capacity and diffusion coefficient
               pgamma = param%root_radius*Vo/(param%bufferNH4*param%diffNH4)
            ELSE!NO3
               !buffer = soil buffer power;  diff = effective diffusion coefficient
               pgamma = param%root_radius*Vo/(param%bufferNo3*param%diffNo3)
            ENDIF

            !intermediate variable
            delta = 2./(2-pgamma)*(((rootx/param%root_radius)**(2-pgamma)-1)/((rootx/param%root_radius)**2-1))
            cav = avail_n/rooting_depth*1000/14
            	
            Imax = temp_response*param%imax

            co = 1./(2.*delta*Vo)*(-imax+delta*imax+Cav*Vo-delta*param%km*Vo+ & 
                SQRT(4.*Cav*delta*param%km*Vo**2.+(-imax+delta*imax+Cav*Vo-delta*param%km*Vo)**2.))
            
            !concentration at root surface (mmol m-3)
            alpha = co/(param%km + co)!root absorbing power (m s-1)
            Up= downreg*root_surface_area * alpha * Imax    !uptake rate (mmol s-1)
            
            Nupt = Up * 86400. * 14./1000.!scale to g d-1

        else
            Nupt = 0.0
        endif

      end function Nupt 
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Aggregrated Canopy Model
!
! !INTERFACE:
    real function Photosyn(Nit,LAI,rstep)
!

! !DESCRIPTION:
!
! !USES:
     use aconite_type
     implicit none 
!
! !ARGUMENTS:
!
! !CALLED FROM:
!
! !REVISION HISTORY:
!
! !LOCAL VARIABLES:
! local pointers to implicit in scalars
	real :: lai, nit
!
! local pointers to implicit out scalars
!
! !OTHER LOCAL VARIABLES:
	real :: trange
	real :: pp, qq, gs, ci, e0, cps,tav, lai_n_ratio
        integer :: rstep
   
!EOP
!-----------------------------------------------------------------------

      !soilmoist%psid and %rtot - soil hydraulic variables, fixed in current version
      tav = (clim%tmax(rstep) + clim%tmin(rstep))/2.
      if(clim%tmin(rstep).gt.0.)THEN	
         lai_n_ratio = lai/nit
         trange = clim%tmax(rstep) - clim%tmin(rstep)! daily temperature range (C)
         gs = abs(state%psid)**param%a10 / ((param%a6* state%rtot + 0.5*trange))
         pp = nit / gs * param%a1 * exp(param%a8 * clim%tmax(rstep)) * (lai_n_ratio/(lai_n_ratio + (param%a11)))
         qq = param%a3 - param%a4
         ci = 0.5 * (clim%co2(rstep) + qq - pp + ((clim%co2(rstep) + qq - pp)**2 - 4 * &
            (clim%co2(rstep) * qq - pp * param%a3)) **0.5)
         e0 = param%a7 * lai**2/(lai**2 + param%a9)
         cps = e0* clim%rad(rstep) * gs * (clim%co2(rstep) - ci)/(e0 * clim%rad(rstep) + &
             gs * (clim%co2(rstep)-ci))
         Photosyn = cps * (param%a2 * state%DayLength + param%a5)
      else 
         Photosyn = 0.
      endif
      
      end function Photosyn

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Respiration from Reich et al. 2008
!
! !INTERFACE:
    real function reich_resp(tissueC,tissueN,reich_paramA,reich_paramB)
!
! !DESCRIPTION:
!
! !USES:
     use aconite_type
     implicit none 
!
! !ARGUMENTS:
!
! !CALLED FROM:
!
! !REVISION HISTORY:
!
! !LOCAL VARIABLES:
! local pointers to implicit in scalars
        real :: tissueC, tissueN,reich_paramA,reich_paramB
!
! local pointers to implicit out scalars
!
! !OTHER LOCAL VARIABLES:
        real :: total_biomass,mmolesN_per_biomass,nmolesC_per_biomass_per_sec,gC_per_biomass_per_sec,gC_per_sec
!EOP
!-----------------------------------------------------------------------

       if(tissueC > 0.0 .and. tissueN > 0.0) then 
          total_biomass = tissueC/param%C_conc
          mmolesN_per_biomass = ((tissueN/14.)*1000)/total_biomass
          nmolesC_per_biomass_per_sec =EXP(reich_paramA+reich_paramB*LOG(mmolesN_per_biomass))
          gC_per_biomass_per_sec =(nmolesC_per_biomass_per_sec/1000000000.)*12.
          gC_per_sec = gC_per_biomass_per_sec * total_biomass
          reich_resp = gC_per_sec * (60.*60.*24.) 
       else
          reich_resp = 0.0
      endif
      end function reich_resp

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Check C and N Balance (daily) 
!
! !INTERFACE:
    integer function BalanceCheck()
!
! !DESCRIPTION:
!
! !USES:
     use aconite_type
     implicit none 
!
! !ARGUMENTS:
!
! !CALLED FROM:
!
! !REVISION HISTORY:
!
! !LOCAL VARIABLES:
! local pointers to implicit in scalars
!
! local pointers to implicit out scalars
!
! !OTHER LOCAL VARIABLES:
      real :: delta_stateC
      real :: delta_stateN
      real :: N_flux
      real :: vegc_flux
      real :: total_Nalloc
      integer :: pass
!EOP
!-----------------------------------------------------------------------

          delta_stateC = state%totalC - state%totalC_prev
          delta_stateN = state%totalN - state%totalN_prev
          N_flux = flux%Nfix + (flux%ndep_nh4 + flux%ndep_no3) - flux%leachN - flux%leachDON
          pass = 1
          if(abs(N_flux - delta_stateN) > 0.05) then
             print *,'N not in balance',(delta_stateN - N_flux)
             print *,'Vegetation balance',((state%totvegn -state%totvegn_prev)- &
                  ((flux%no3_uptake + flux%nh4_uptake) - (flux%t_woodN + flux%t_leafN + flux%t_rootN)))
             pass = 0 
          endif
          if(abs(flux%NEE - delta_stateC) > 0.1) then
             print *,'C not in balance',delta_stateC, flux%NEE,state%totalC, state%totalC_prev
             print *,'Vegetation balance',(state%totvegc - state%totvegc_prev), &
                  (flux%NPP - (flux%t_woodC + flux%t_leafC+ flux%t_rootC))
             pass = 0
          endif
          state%totalC_prev = state%totalC
          state%totalN_prev = state%totalN
          state%totvegc_prev = state%totvegc
          state%totvegn_prev = state%totvegn

           BalanceCheck = pass    
      end function BalanceCheck 

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Uptake annual integrated fluxes 
!
! !INTERFACE:
    subroutine year_update(new_year,year_count)
!
! !DESCRIPTION:
!
! !USES:
     use aconite_type
     implicit none 
!
! !ARGUMENTS:
!
! !CALLED FROM:
!
! !REVISION HISTORY:
!
! !LOCAL VARIABLES:
! local pointers to implicit in scalars
!
! local pointers to implicit out scalars
!
! !OTHER LOCAL VARIABLES:
      integer :: new_year
      integer :: year_count
!EOP
!-----------------------------------------------------------------------

      if(new_year ==  1) then
              flux% annual_GPP= 0.0
               flux%annual_NPP =  0.0
               flux%annual_NEE =  0.0
               flux%annual_Rh =  0.0
               flux%annual_Nuptake = 0.0
               flux%annual_immob = 0.0
               flux%annual_netNmin = 0.0
               flux%annual_nitr = 0.0
               flux%annual_leachN =  0.0
               flux%annual_Ndep = 0.0
               flux%annual_Nfix = 0.0
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
               state%GDDTot = 0.0

      else
          flux%annual_GPP = flux%annual_GPP + flux%GPP
          flux%annual_NPP = flux%annual_NPP + flux%NPP
          flux%annual_NEE = flux%annual_NEE + flux%NEE
          flux%annual_Rh = flux%annual_Rh + flux%Rh_total
          flux%annual_Nuptake = flux%annual_Nuptake + flux%no3_uptake + &
               flux%nh4_uptake
          flux%annual_immob =flux%annual_immob + flux%nh4_immob + &
               flux%no3_immob
          flux%annual_netNmin = flux%annual_netNmin + flux%net_nmin
          flux%annual_nitr = flux%annual_nitr + flux%nitr
          flux%annual_leachN = flux%annual_leachN + flux%leachN + flux%leachDON
          flux%annual_Ndep = flux%annual_Ndep + flux%ndep_nh4 + &
              flux%ndep_no3
          flux%annual_Nfix = flux%annual_Nfix + flux%Nfix
          flux%annual_litterfallC = flux%annual_litterfallC  + &
              flux%t_woodC + flux%t_leafC + flux%t_rootC
          flux%annual_litterfallN = flux%annual_litterfallN  + &
              flux%t_woodN + flux%t_leafN + flux%t_rootN
          flux%annual_Ra = flux%annual_Ra + flux%Ra_total
          flux%annual_Ra_grow = flux%annual_Ra_grow + flux%Ra_grow
          flux%annual_Ra_main = flux%annual_Ra_main + flux%Ra_main
          flux%annual_Ra_excessC = flux%annual_Ra_excessC + flux%Ra_excessC
          flux%annual_retransN = flux%annual_retransN + flux%retransN
          flux%annual_a_leafC = flux%annual_a_leafC  + flux%a_labileC_bud_2leaf
          flux%annual_a_woodC = flux%annual_a_woodC + flux%a_woodC
          flux%annual_a_rootC = flux%annual_a_rootC + flux%a_rootC
          flux%annual_a_leafN = flux%annual_a_leafN  + flux%a_labileN_bud_2leaf
          flux%annual_a_woodN = flux%annual_a_woodN + flux%a_woodN
          flux%annual_a_rootN = flux%annual_a_rootN + flux%a_rootN
          flux%annual_t_leafC = flux%annual_t_leafC + flux%t_leafC
          flux%annual_t_woodC = flux%annual_t_woodC + flux%t_woodC
          flux%annual_t_rootC = flux%annual_t_rootC + flux%t_rootC
          flux%annual_t_leafN = flux%annual_t_leafN + flux%t_leafN
          flux%annual_t_woodN = flux%annual_t_woodN + flux%t_woodN
          flux%annual_t_rootN = flux%annual_t_rootN + flux%t_rootN
        
          if(year_count == io%annual_state_doy) then
              state%annual_leafC = state%leafC
              state%annual_woodC = state%woodC
              state%annual_rootC = state%rootC
              state%annual_leafN = state%leafN
              state%annual_woodN = state%woodN
              state%annual_rootN = state%rootN
              state%annual_labileC = state%labileC
              state%annual_labileN = state%labileN
              state%annual_labileC_bud = state%labileC_bud
              state%annual_labileC_Ra = state%labileC_Ra
              state%annual_labileN_bud = state%labileN_bud
              state%annual_targetleafCN = state%target_leafCN
              state%annual_targetrootCN = state%target_rootCN
              state%annual_litterC = state%litterC
              state%annual_litterN = state%litterN
              state%annual_soilC = state%soilC
              state%annual_soilN = state%soilN
              state%annual_cwdC = state%cwdC
              state%annual_cwdN = state%cwdN
              state%annual_nh4 = state%nh4
              state%annual_no3 = state%no3
              state%annual_lai = state%lai
              state%annual_totvegc = state%totvegc
              state%annual_totvegn = state%totvegn
              state%annual_leafCN = state%leafCN
              state%annual_woodCN = state%woodCN
              state%annual_rootCN = state%rootCN
              state%annual_Nuptake_downreg = state%Nuptake_downreg
              state%annual_Cuptake_downreg = state%Cuptake_downreg
              state%annual_MaxNstore = state%MaxNstore
              state%annual_MaxCstore = state%MaxCstore
              state%annual_a_leafC = state%a_leafC
              state%annual_a_woodC = state%a_woodC
              state%annual_a_rootC = state%a_rootC
              state%annual_a_NfixC = state%a_NfixC
              marg%annual_Creturn_leafC = marg%Creturn_leafC
              marg%annual_Nreturn_leafC = marg%Nreturn_leafC
              marg%annual_Creturn_rootC =marg%Creturn_rootC
              marg%annual_Nreturn_rootC = marg%Nreturn_rootC
              marg%annual_Creturn_Raexcess =marg%Creturn_Raexcess
              marg%annual_Nreturn_Raexcess = marg%Nreturn_Raexcess
              marg%annual_Creturn_leafN = marg%Creturn_leafN
              marg%annual_Nreturn_leafN = marg%Nreturn_leafN
              marg%annual_Creturn_rootN = marg%Creturn_rootN
              marg%annual_Nreturn_rootN =marg%Nreturn_rootN
          endif          
          
       endif
      end subroutine year_update 

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: integrated marginal returns
!
! !INTERFACE:
    subroutine marginal_integrator()

!
! !DESCRIPTION:
!
! !USES:
     use aconite_type
     implicit none 
!
! !ARGUMENTS:
!
! !CALLED FROM:
!
! !REVISION HISTORY:
!
! !LOCAL VARIABLES:
! local pointers to implicit in scalars
!
! local pointers to implicit out scalars
!
! !OTHER LOCAL VARIABLES:
     real :: leaf_horizon,root_horizon, leafc_adjust, tmp
     real :: rootc_adjust,tmpmaxleafC,leafN_deficit, leafn_adjust
!EOP
!-----------------------------------------------------------------------


   if(state%leafC > 0) then
			  
      marg%annual_GPP_leafC = marg%annual_GPP_leafC + marg%GPP_leafC
      marg%annual_Rm_leafC = marg%annual_Rm_leafC + marg%Rm_leafC
      marg%annual_Rg_leafC = marg%annual_Rg_leafC + marg%Rg_leafC
      marg%annual_GPP_leafN = marg%annual_GPP_leafN + marg%GPP_leafN
      marg%annual_addN_leafN = marg%annual_addN_leafN + marg%addN_leafN
      marg%annual_Rm_leafN = marg%annual_Rm_leafN + marg%Rm_leafN
      marg%annual_GPP_leafCN = marg%annual_GPP_leafCN + marg%GPP_leafCN
      marg%annual_Rm_leafCN = marg%annual_Rm_leafCN + marg%Rm_leafCN       
      marg%day_count_leaf = marg%day_count_leaf + 1   
              	
    else
              
      marg%annual_GPP_leafC = marg%annual_GPP_leafC + 0.0
      marg%annual_Rm_leafC = marg%annual_Rm_leafC + 0.0
      marg%annual_Rg_leafC = marg%annual_Rg_leafC + 0.0
      marg%annual_GPP_leafN = marg%annual_GPP_leafN + 0.0
      marg%annual_addN_leafN = marg%annual_addN_leafN + 0.0
      marg%annual_Rm_leafN = marg%annual_Rm_leafN + 0.0
      marg%annual_GPP_leafCN = marg%annual_GPP_leafCN + 0.0
      marg%annual_Rm_leafCN = marg%annual_Rm_leafCN + 0.0 
       
    endif
            

   marg%day_count_root = marg%day_count_root + 1          
   marg%annual_Rm_rootC = marg%annual_Rm_rootC + marg%Rm_rootC
   marg%annual_Rg_rootC = marg%annual_Rg_rootC + marg%Rg_rootC
   marg%annual_Nuptake_rootC = marg%annual_Nuptake_rootC + marg%Nuptake_rootC
   marg%annual_Rm_rootN = marg%annual_Rm_rootN + marg%Rm_rootN
   marg%annual_Nuptake_rootN = marg%annual_Nuptake_rootN + marg%Nuptake_rootN   
   marg%annual_addN_rootN = marg%annual_addN_rootN + marg%addN_rootN
   marg%annual_Nuptake_rootCN = marg%annual_Nuptake_rootCN + marg%Nuptake_rootCN 
   marg%annual_Nreturn_Raexcess = marg%annual_Nreturn_Raexcess + marg%Nreturn_Raexcess
   marg%annual_cstore_ratio = marg%annual_cstore_ratio + (state%labileC/state%MaxCstore)
   marg%annual_nstore_ratio = marg%annual_nstore_ratio + (state%labileN/state%MaxNstore)
   marg%annual_Nreturn_Raexcess = marg%annual_Nreturn_Raexcess + marg%Nreturn_Raexcess
              
   marg%mean_leafC = marg%mean_leafC + state%leafC 
   marg%mean_leafN = marg%mean_leafN + state%leafN
   marg%mean_rootC = marg%mean_rootC + state%rootC
   marg%mean_rootN = marg%mean_rootN + state%rootN
                               
   state%hitmaxleafC = max(state%hitmaxleafC,(state%leafC/state%maxleafC))
   state%hitmaxrootC = max(state%hitmaxrootC,(state%rootC/state%maxrootC)) 
         
   if(clim%doy(state%rstep) == 365) then  !ANNUAL UPDATE
          	  
       marg%integ_GPP_leafC = marg%annual_GPP_leafC
       marg%integ_Rm_leafC = marg%annual_Rm_leafC
       marg%integ_Rg_leafC = (marg%annual_Rg_leafC/marg%day_count_leaf)
       marg%integ_GPP_leafN =marg%annual_GPP_leafN
       marg%integ_Rm_leafN =marg%annual_Rm_leafN
       marg%integ_GPP_leafCN = marg%annual_GPP_leafCN
       marg%integ_Rm_leafCN = marg%annual_Rm_leafCN
       marg%integ_Rm_rootC = marg%annual_Rm_rootC
       marg%integ_Rg_rootC =  marg%annual_Rg_rootC/marg%day_count_root         
       marg%integ_Nuptake_rootC =marg%annual_Nuptake_rootC        
       marg%integ_Rm_rootN = marg%annual_Rm_rootN
       marg%integ_Nuptake_rootN =marg%annual_Nuptake_rootN   
       marg%integ_Nuptake_rootCN = marg%annual_Nuptake_rootCN
       marg%integ_Rg_woodC = marg%annual_Rg_woodC
              
       marg%integ_Nreturn_Raexcess = marg%annual_Nreturn_Raexcess/marg%day_count_root
              
       marg%integ_addN_leafN = marg%annual_addN_leafN/marg%day_count_leaf
       marg%integ_addN_rootN = marg%annual_addN_rootN/marg%day_count_root              
       marg%mean_leafC = marg%mean_leafC/marg%day_count_leaf
       marg%mean_leafN = marg%mean_leafN/marg%day_count_leaf
       marg%mean_rootC = marg%mean_rootC/marg%day_count_root
       marg%mean_rootN = marg%mean_rootN/marg%day_count_root
       
       marg%integ_cstore_ratio = marg%annual_cstore_ratio/marg%day_count_root  
       marg%annual_cstore_ratio = 0.0
       marg%integ_nstore_ratio = marg%annual_nstore_ratio/marg%day_count_root  
       marg%annual_nstore_ratio = 0.0
        
       marg%integ_hitmaxleafC = state%hitmaxleafC
       marg%integ_hitmaxrootC = state%hitmaxrootC
       
       leafN_deficit = (((marg%integ_hitmaxleafC*state%maxleafC)/state%leafCN) - state%leafN_deficit)&
       				/((marg%integ_hitmaxleafC*state%maxleafC)/state%leafCN)
   	   leafN_deficit = max(leafN_deficit,0.0)
   	   state%debug = state%leafN_deficit
    
       marg%integ_hitwood_requirement = (state%wood_requirement - state%min_wood_deficit)/state%wood_requirement              

       state%hitmaxleafC = 0
       state%hitmaxrootC = 0
       state%wood_requirement = 0.0
       
       ! THIS IS WHERE THE MARGINAL RETURNS ARE CALCULATED   
              
       if(param%t_leaf>(1/365.) .and. site%seasonal == 1.0) then
          leaf_horizon = 1.0
       else
          leaf_horizon = (param%t_leaf*365.)
       endif
      
       root_horizon = (param%t_root*365.)


     
       marg%integ_Creturn_leafC = (marg%integ_GPP_leafC - marg%integ_Rm_leafC) &
               /leaf_horizon - marg%integ_Rg_leafC
                   
       marg%integ_Nreturn_leafC = 0.0
       marg%integ_Creturn_leafN = (marg%integ_GPP_leafN - marg%integ_Rm_leafN)/leaf_horizon
       marg%integ_Nreturn_leafN = -marg%integ_addN_leafN
       marg%integ_Creturn_leafCN = (marg%integ_GPP_leafCN - marg%integ_Rm_leafCN) &
                /leaf_horizon -  marg%integ_Rg_leafC
       marg%integ_Creturn_rootC = -marg%integ_Rm_rootC/root_horizon - marg%integ_Rg_rootC
       marg%integ_Nreturn_rootC = marg%integ_Nuptake_rootC/root_horizon
       marg%integ_Creturn_rootN = -marg%integ_Rm_rootN/root_horizon
       marg%integ_Nreturn_rootN = (marg%integ_Nuptake_rootN/root_horizon)- marg%integ_addN_rootN
       marg%integ_Nreturn_rootCN = (marg%integ_Nuptake_rootCN/root_horizon) - marg%integ_addN_rootN
               
       if(state%maxleafN==0.0) then
       		state%maxleafN = state%maxleafC/state%leafCN
       endif
 
       if(marg%integ_Creturn_leafCN > param%add_C) then
           leafc_adjust = min(param%MaxallocAdjust,(param%MaxallocAdjust*(marg%integ_Creturn_leafCN - param%add_C)))
       else
           leafc_adjust = min(param%MaxallocAdjust, (param%MaxallocAdjust*(param%add_C - marg%integ_Creturn_leafCN)))
       endif
   
   	   print *, leaf_adjust
   	   
       if(marg%integ_hitmaxleafC > 0.99 .and. (marg%integ_Creturn_leafC-param%add_C) >= 0.0) then
           if(state%min_wood_deficit == 0.0 .and. marg%integ_hitmaxrootC > 0.99) then
               state%maxleafC = state%maxleafC *  (1+leafc_adjust)
           else
               state%maxleafC = state%maxleafC*(1-leafc_adjust) 
           endif
       endif
		
       if(marg%integ_hitmaxleafC > 0.99 .and. marg%integ_Creturn_leafC-param%add_C < 0.0)then
           state%maxleafC = state%maxleafC*(1-leafc_adjust)
       endif
             
       if(marg%integ_hitmaxleafC <= 0.99  .and. (leafN_deficit > 0.99 .or. marg%integ_Creturn_leafC-param%add_C < 0.0)) then
            state%maxleafC = state%maxleafC*(1-leafc_adjust)          
       endif
       
       leafn_adjust = leafc_adjust

       if(marg%integ_hitmaxleafC > 0.99 .and. leafN_deficit > 0.99 .and. marg%integ_Creturn_leafN > 0.0) then
       			 if(state%min_wood_deficit == 0.0 .and. marg%integ_hitmaxrootC > 0.99) then
       			        		state%maxleafN = state%maxleafN*(1+leafn_adjust)                 		
       			 else
                     state%maxleafN = state%maxleafN              
                 endif  
       	endif
       	
       	if(marg%integ_hitmaxleafC > 0.99 .and. leafN_deficit <= 0.99) then
       	     state%maxleafN = state%maxleafN*(1-leafn_adjust)
       	endif
       	
        if(marg%integ_hitmaxleafC <= 0.99 .and. leafN_deficit > 0.99 .and. marg%integ_Creturn_leafN > 0.0) then
       		state%maxleafN = state%maxleafN*(1+leafn_adjust)		
       	endif  
       	
        if(marg%integ_hitmaxleafC <= 0.99 .and. leafN_deficit <= 0.99 .and. marg%integ_Creturn_leafN > 0.0) then
       		state%maxleafN = state%maxleafN*(1-leafn_adjust)
       	endif            	
       	
       	if(marg%integ_Creturn_leafN <= 0.0) then
       		state%maxleafN = state%maxleafN*(1-leafn_adjust)
       	endif
       	
       	! ADJUST ROOT C

       rootc_adjust = leafc_adjust
       
	   if(marg%integ_hitmaxrootC > 1.00) then
			state%maxrootC = state%maxrootC * marg%integ_hitmaxrootC
	   endif
			
       if(marg%integ_hitmaxrootC > 0.99 .and. marg%integ_hitmaxrootC  <= 1.00 .and. &
       state%min_wood_deficit == 0.0 .and. marg%integ_Nreturn_rootC > marg%integ_Nreturn_Raexcess &
       .and.  marg%integ_Nreturn_rootCN > marg%integ_addN_rootN .and. leafN_deficit <= 0.99) then
            state%maxrootC = state%maxrootC * (1+rootc_adjust)          				  
       endif
             
       if(marg%integ_hitmaxrootC  > 0.99 .and. &
       (marg%integ_Nreturn_rootC <= marg%integ_Nreturn_Raexcess &
       .or. marg%integ_Nreturn_rootCN <= marg%integ_addN_rootN)) then            
            state%maxrootC= state%maxrootC * (1-rootc_adjust)       			
       endif
             
       if(marg%integ_hitmaxrootC < 0.99) then    
            state%maxrootC = state%maxrootC * (1-rootc_adjust)
       endif
       
       if(marg%integ_hitmaxrootC > 0.99 .and. leafN_deficit > 0.99) then
       		  state%maxrootC = state%maxrootC * (1-rootc_adjust)	     	
       	endif
    
        state%maxrootC = max((state%maxleafC * param%leaf2root_ratio),state%maxrootC)
        state%maxrootC =state%maxleafC * param%leaf2root_ratio

        state%maxleafC = max(20.,state%maxleafC)
        state%maxleafN = max((20./35.),state%maxleafN)
        if(state%maxleafC == 20.) then
        	state%maxrootC = state%maxleafC *param%leaf2root_ratio
        endif

		state%target_leafCN =  state%maxleafC/state%maxleafN     
		state%target_leafCN = max(state%target_leafCN,0.0)

  
        state%prev_labileC = state%labileC     
        state%prev_labileN = state%labileN
        state%min_wood_deficit = 0.0
        state%leafN_deficit = 0.0       
                                        
        marg%annual_GPP_leafC = 0.0
        marg%annual_Rm_leafC = 0.0
        marg%annual_Rg_leafC = 0.0
        marg%annual_GPP_leafN = 0.0
        marg%annual_addN_leafN = 0.0
        marg%annual_Rm_leafN = 0.0
        marg%annual_GPP_leafCN = 0.0
        marg%annual_Rm_leafCN = 0.0
                           
        marg%annual_Rm_rootC = 0.0
        marg%annual_Rg_rootC= 0.0
        marg%annual_Nuptake_rootC = 0.0
        marg%annual_Rm_rootN = 0.0
        marg%annual_Nuptake_rootN = 0.0
        marg%annual_addN_rootN = 0.0
        marg%annual_Nuptake_rootCN = 0.0
        marg%annual_Rg_woodC = 0.0
        marg%annual_Nreturn_Raexcess = 0.0
        marg%day_count_leaf = 0.0
        marg%day_count_root = 0.0
    endif

          
    end subroutine marginal_integrator
     
!-----------------------------------------------------------------------
!BOP
!

! !IROUTINE: Zero_fluxes () 
!
! !INTERFACE:
    subroutine zero_fluxes()
!
! !DESCRIPTION:
!
! !USES:
     use aconite_type
     implicit none 
!
! !ARGUMENTS:
!
! !CALLED FROM:
!
! !REVISION HISTORY:
!
! !LOCAL VARIABLES:
! local pointers to implicit in scalars
!
! local pointers to implicit out scalars
!
! !OTHER LOCAL VARIABLES:

!EOP
!-----------------------------------------------------------------------

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
      flux%Ndep_no3 = 0.0
      flux%retransN =0.0
      
     end subroutine zero_fluxes
!-----------------------------------------------------------------------

! !IROUTINE: Marginal returns on allocation () 
!
! !INTERFACE:
    SUBROUTINE marginals(avail_NH4,avail_NO3,Ra_temp_resp)

! !DESCRIPTION:
!
! !USES:
     use aconite_type
     implicit none 
!
! !ARGUMENTS:
!
! !CALLED FROM:
!
! !REVISION HISTORY:
!
! !LOCAL VARIABLES:
! local pointers to implicit in scalars
     real:: avail_NH4,avail_NO3,Ra_temp_resp
!
! local pointers to implicit out scalars
     real :: marginal_nh4_uptake,marginal_no3_uptake,add_lai
     real ::  Nuptake_leafN_adjust,Cuptake_leafN_adjust
!

     
     Nuptake_leafN_adjust = 1.0  ! This are where we can adjust the leaf/root N by 
     Cuptake_leafN_adjust = 1.0 ! Ndownreg or Cdownreg in the respiration equations
                                 ! set to 1 means that all the N is contributing to 
                                 ! respiration
 
     marg%mean_leafC = state%leafC
     marg%mean_leafN = state%leafN
     marg%mean_rootC = state%rootC
     marg%mean_rootN = state%rootN

!LEAF C
     !increase leafC by 1g m-2
     add_lai = (state%leafC+param%add_C) / param%lca 
     marg%GPP_leafC = Photosyn(state%leafN, add_lai, state%rstep)*state%Cuptake_downreg - flux%GPP	
     marg%Rg_leafC = param%add_C*(1.+param%Ra_grow)
     if(param%use_reich == 0) then
        marg%Rm_leafC = 0.0
     else
        marg%Rm_leafC = (reich_resp(state%leafC+param%add_C,state%leafN*Cuptake_leafN_adjust,param%leaf_resp_A,&
             param%leaf_resp_B)*Ra_temp_resp - reich_resp(state%leafC,state%leafN*Cuptake_leafN_adjust, &
             param%leaf_resp_A,param%leaf_resp_B)*Ra_temp_resp)
    endif


!LEAF N
	if(state%leafC == 0.0) then
	   	marg%addN_leafN = param%add_C/state%target_leafCN
	else
        marg%addN_leafN =(param%add_C/state%leafCN) !increase leafN 
    endif
    marg%GPP_leafN = Photosyn((state%leafN + marg%addN_leafN), state%lai, state%rstep)*state%Cuptake_downreg - flux%GPP
    if(param%use_reich == 0) then
       	 marg%Rm_leafN = marg%addN_leafN * param%Ra_per_N*Ra_temp_resp
    else
        marg%Rm_leafN = (reich_resp(state%leafC,(state%leafN+marg%addN_leafN)*Cuptake_leafN_adjust,param%leaf_resp_A,&
             param%leaf_resp_B)*Ra_temp_resp -reich_resp(state%leafC,state%leafN*Cuptake_leafN_adjust,param%leaf_resp_A,&
             param%leaf_resp_B)*Ra_temp_resp)
    endif

        
! LEAF C AND N 
        
    marg%GPP_leafCN = Photosyn((state%leafN + marg%addN_leafN), add_lai, state%rstep)*state%Cuptake_downreg - flux%GPP
        
    if(param%use_reich == 0) then
        marg%Rm_leafCN = marg%addN_leafN * param%Ra_per_N*Ra_temp_resp
    else
        marg%Rm_leafCN = (reich_resp(state%leafC+param%add_C,(state%leafN+marg%addN_leafN)* &
             Cuptake_leafN_adjust,param%leaf_resp_A,param%leaf_resp_B)*Ra_temp_resp- &
             reich_resp(state%leafC,state%leafN*Cuptake_leafN_adjust,param%leaf_resp_A,param%leaf_resp_B)&
             *Ra_temp_resp)
    endif
    
!ROOTC	
    if(param%use_reich == 0) then
       marg%Rm_rootC = 0.0
    else
       marg%Rm_rootC =(reich_resp(state%rootC+param%add_C,state%rootN*state%Nuptake_downreg, &
            param%leaf_resp_A,param%leaf_resp_B)*Ra_temp_resp - &
            reich_resp(state%rootC,state%rootN*state%Nuptake_downreg,param%leaf_resp_A,param%leaf_resp_B)*Ra_temp_resp)
    endif
    marg%Rg_rootC= param%add_C*(1.+param%Ra_grow)
    if(avail_NH4>0)then     !there is NH4 to take up
        marginal_nh4_uptake = Nupt(1, (state%rootC + param%add_C), state%rootN, state%woodC,avail_nh4+flux%nh4_uptake,&
           	state%Nuptake_downreg,flux%GPP,Ra_temp_resp)-flux%nh4_uptake
    else
           marginal_nh4_uptake = 0.
    endif
    if(avail_NO3>0)then	!there is N03 to take up
        marginal_no3_uptake =Nupt(2, (state%rootC + param%add_C), state%rootN, state%woodC,avail_no3+flux%no3_uptake,&
           	state%Nuptake_downreg,flux%GPP,Ra_temp_resp)-flux%no3_uptake 
 	else
        marginal_no3_uptake = 0.	
    endif   	
    marg%Nuptake_rootC = marginal_nh4_uptake+marginal_no3_uptake
 
 
    !ROOT N
	marg%addN_rootN =(param%add_C/state%rootCN) !increase rootN 
	if(param%use_reich == 0) then
	    marg%Rm_rootN = marg%addN_rootN * param%Ra_per_N*Ra_temp_resp
	else
	    marg%Rm_rootN =(reich_resp(state%rootC,(state%rootN + marg%addN_rootN)*Nuptake_leafN_adjust, &
            param%leaf_resp_A,param%leaf_resp_B)*Ra_temp_resp - &
            reich_resp(state%rootC,state%rootN*Nuptake_leafN_adjust,param%leaf_resp_A,param%leaf_resp_B)*Ra_temp_resp)
    endif
        
	if(avail_NH4>0)then     !there is NH4 to take up
        marginal_nh4_uptake = Nupt(1, state%rootC, (state%rootN + marg%addN_rootN), state%woodC,&
        	avail_nh4+flux%nh4_uptake,state%Nuptake_downreg,flux%GPP,Ra_temp_resp)-flux%nh4_uptake
    else
        marginal_nh4_uptake = 0.
    endif

    if(avail_NO3>0)then	!there is N03 to take up
        marginal_no3_uptake = Nupt(2, state%rootC, (state%rootN + marg%addN_rootN), state%woodC,&
           	avail_no3+flux%no3_uptake,state%Nuptake_downreg,flux%GPP,Ra_temp_resp)-flux%no3_uptake
    else
        marginal_no3_uptake = 0.
    endif
    marg%Nuptake_rootN = marginal_nh4_uptake+marginal_no3_uptake
        
        
    ! ROOT CN
    if(avail_NH4>0)then     !there is NH4 to take up
        marginal_nh4_uptake = Nupt(1, state%rootC+param%add_C, (state%rootN + marg%addN_rootN), state%woodC,&
        	avail_nh4+flux%nh4_uptake,state%Nuptake_downreg,flux%GPP,Ra_temp_resp)-flux%nh4_uptake
    else
        marginal_nh4_uptake = 0.
    endif

    if(avail_NO3>0)then	!there is N03 to take up
        marginal_no3_uptake = Nupt(2, state%rootC + param%add_C, (state%rootN + marg%addN_rootN), state%woodC,&
           	avail_no3+flux%no3_uptake,state%Nuptake_downreg,flux%GPP,Ra_temp_resp)-flux%no3_uptake
    else
        marginal_no3_uptake = 0.
    endif 
    marg%Nuptake_rootCN = marginal_nh4_uptake+marginal_no3_uptake     
         
    !N FIXATION
    !Labile C 
	marg%Creturn_Raexcess = - param%add_C
    !Labile N
    marg%Nreturn_Raexcess = param%add_C * param%Nfix_per_gC*state%Nuptake_downreg*Ra_temp_resp


    end subroutine marginals


!!-----------------------------------------------------------------------

      end module aconite_functions 