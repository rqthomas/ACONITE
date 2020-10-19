      module aconite_ecosystem


! !PUBLIC MEMBER FUNCTIONS:
      public :: ecosystem_dynamics 
 
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
! !INTERFACE:
      subroutine ecosystem_dynamics(rstep,year_count)
      
  
!
! !DESCRIPTION:
!
! !USES:
     use aconite_type 
     use aconite_functions
     implicit none 
! !ARGUMENTS:
!
! !CALLED FROM:
!
! !REVISION HISTORY:
!
! !LOCAL VARIABLES:
! local pointers to implicit in scalars
!
!
! local pointers to implicit out scalars
!
!
! !OTHER LOCAL VARIABLES:
      real :: r
      real :: z, z2
      real :: decl
      real :: LatRad
      real :: h
      real :: TA
      real :: AC
      real :: hr
      real :: es
      real :: delta
      real :: emean
      real :: GDD
      integer :: rstep
      real :: total_immob
      real :: litter_to_atm
      real :: litter_to_soil
      integer :: year_count
      integer :: pass
      real :: avail_nh4
      real :: avail_no3
      real :: growth_potential
      real :: avail_C,avail_N
      real :: Ra_temp_resp,Rh_temp_resp
      real :: potN,labileC_bud2labile_Ra,labileN_bud2labileN
      real :: tmp_a_woodN,tmp_a_woodC,tmp_leafC,tmp_leafN,instant_Creturn_leafCN
      real, parameter :: pi = 3.141592653589793239
!EOP
!-----------------------------------------------------------------------


!---CALCULATE ATMOSPHERIC ENVIRONMENT----------------------------------

      state%Tave =  (clim%tmin(rstep) + clim%tmax(rstep)) / 2.0
      state%Tday = (clim%tmax(rstep) + state%Tave) / 2.0;
      LatRad = site%Lat * (2.0 * pi) / 360.0;
      r = 1 - (0.0167 * cos(0.0172 * (clim%doy(rstep) - 3)));
      z = 0.39785 * sin(4.868961 + 0.017203 * clim%doy(rstep) + & 
         0.033446 *sin(6.224111 + 0.017202 * clim%doy(rstep)));
      if (abs(z) < 0.7) then
          decl = atan(z / (sqrt(1.0 - (z**2))));
      else
          decl = pi / 2.0 - atan(sqrt(1 - z**2) / z);
      endif
      if (abs(LatRad) >= (pi/2.0)) then
          if (site%Lat < 0) then
              LatRad = (-1.0) * (pi/2.0 - 0.01);
          else
              LatRad = (1.0) * (pi/2.0 - 0.01);
          endif
      endif
      z2 = -tan(decl) * tan(LatRad);

      if (z2 >= 1.0) then
         h = 0;
      elseif (z2 <= -1.0) then
         h = pi;
      else
         TA = abs(z2);
         if (TA < 0.7) then
             AC = 1.570796 - atan(TA / sqrt(1.0 - (TA**2)));
         else
             AC = atan(sqrt(1 - (TA**2)) / TA);
         endif
         if (z2 < 0) then
             h = pi-AC;
         else
             h = AC;
        endif
      endif
      hr = 2.0 * (h*24.0) / (2.0*pi);
      state%DayLength = (hr);
      state%NightLength = (24.0 - hr);
     
      GDD = state%Tave - 8
      if (GDD < 0) then
          GDD = 0
      endif
      state%GDDTot = state%GDDTot + GDD

!---------------------------------------------------------------------
! CALCULATE ECOSYSTEM FLUXES

       if(year_count == 1) then
          call year_update(1,year_count)     
       endif

!--- PHENOLOGY ----------------------
      if(site%seasonal == 1) then			!seasonal growing season
          !in growing season, so grow
          if((state%GDDTot >= param%GDDStart) .AND. (clim%doy(rstep) < param%SenescStart)) then   
              growth_potential = param%growth_potential_param    
          else
              growth_potential = 0.0	
          endif

     else					!full year growing season
          growth_potential = param%growth_potential_param
     endif
     
!--- PLANT STRUCTURE ------------------------ 

      state%lai = state%leafC / param%lca
    
!----- RESPIRATION TEMPERATURE RESPONSE ---------

      Ra_temp_resp = param%Ra_Q10**((state%Tave-20)/10)
      Rh_temp_resp = param%Rh_Q10**((state%Tave-20)/10) 
      
!-----TISSUE TURNOVER ------------------------

      if(site%seasonal == 1) then				!is the ecosystem in a seasonal climate?
          if(clim%doy(rstep) >= param%SenescStart) then	!after sensence date, drop leaves
                 if(param%t_leaf > (1/365.)) then ! SEASONAL BROADLEAF
                     if(state%leafC < (state%maxleafC * (1-param%stocks_close_prop))) then
                     	flux%t_leafC = state%leafC
              			flux%t_leafN = state%leafN * (1 - param%trans_frac)
              			flux%retransN = (state%leafN * param%trans_frac)
                     else
              			flux%t_leafC = state%leafC * param%leaf_shed_rate
              			flux%t_leafN = (state%leafN * param%leaf_shed_rate)*(1 - param%trans_frac) 
              			flux%retransN = (state%leafN * param%leaf_shed_rate) *param%trans_frac     			
                    endif
                 else   

                    if(clim%doy(rstep) >= param%SenescStart .and. clim%doy(rstep) < (param%SenescStart +1)) then !SEASONAL EVERGREEN
                       state%LeafTurnoverTarget = state%leafC - state%leafC*(365.*param%t_leaf)
                    elseif(state%leafC > state%LeafTurnoverTarget) then 
                        flux%t_leafC = (state%leafC - state%LeafTurnoverTarget)*param%leaf_shed_rate
                        flux%t_leafN = flux%t_leafC * (state%leafN/state%leafC)*(1 - param%trans_frac) 
                        flux%retransN = flux%t_leafC * (state%leafN/state%leafC)*param%trans_frac
                    else
                        flux%t_leafC = 0.0
                        flux%t_leafN = 0.0
                    endif
                 endif		
          endif
      else  ! NON-SEASONAL EVERGREEN
          flux%t_leafC = (state%leafC*param%t_leaf)
          flux%t_leafN = (state%leafN * param%t_leaf)*(1 - param%trans_frac)
          flux%retransN =  (state%leafN * param%t_leaf)*(param%trans_frac)
      endif
      
      flux%t_woodC = state%woodC * param%t_wood
      flux%t_rootC = state%rootC * param%t_root
      
      flux%t_woodN = state%woodN * param%t_wood
      flux%t_rootN = state%rootN * param%t_root
      flux%t_cwdC = state%cwdC * param%t_cwd * Rh_temp_resp
      flux%t_cwdN = state%cwdN * param%t_cwd * Rh_temp_resp 

!--- LITTER AND SOIL CALCULATIONS --------------------------------

      flux%t_litterC = state%litterC * param%t_litter * Rh_temp_resp 
      litter_to_atm = flux%t_litterC*(1-param%mCUE)
      litter_to_soil = flux%t_litterC*(param%mCUE)
      flux%t_litterN = state%litterN * param%t_litter * Rh_temp_resp
      total_immob = ((litter_to_soil)/param%soilCN) - flux%t_litterN
      if(total_immob < 0) then
          flux%nh4_immob = ((litter_to_soil)/param%soilCN) -flux%t_litterN 
          flux%no3_immob = 0.0
      else
          flux%nh4_immob = (state%nh4/(state%nh4+state%no3))*total_immob
          flux%no3_immob = (state%no3/(state%nh4+state%no3))*total_immob
      endif
      flux%t_soilC =  state%soilC * param%t_soil* Rh_temp_resp 
      flux%t_soilN =  state%soilN * param%t_soil* Rh_temp_resp 
      flux%leachDON = flux%t_soilN*param%Nturn_dep_loss 
      flux%t_soilN = flux%t_soilN - flux%leachDON    

      avail_nh4 = state%nh4
      avail_no3 = state%no3

      if(avail_nh4 >= flux%nh4_immob)then
          avail_nh4 = avail_nh4 - flux%nh4_immob
      else
          flux%nh4_immob = avail_nh4
          avail_nh4 = 0
      endif

      if(avail_no3 >= flux%no3_immob)then
          avail_no3 = avail_no3 - flux%no3_immob
      else
          flux%no3_immob = avail_no3
          avail_no3 = 0
      endif

      if((flux%no3_immob + flux%nh4_immob) < total_immob) then
           IF(total_immob.lt.0)THEN
      	       flux%Rh_total = litter_to_atm + flux%t_soilC
      	   ENDIF
      
         flux%t_litterC = flux%t_litterC * ((flux%no3_immob +flux%nh4_immob)/total_immob)
         litter_to_atm = flux%t_litterC * (1.-param%mCUE)
         litter_to_soil = flux%t_litterC * (param%mCUE)
         flux%t_litterN = flux%t_litterN * ((flux%no3_immob +flux%nh4_immob)/total_immob)
      endif

      flux%Rh_total = litter_to_atm + flux%t_soilC
        
!------ PHOTOSYNTHESIS (GPP) ------
        
	   if(state%labileC > state%maxCstore) then
	   		state%Cuptake_downreg = max(0.0,1-((state%labileC-state%maxCstore)/state%maxCstore))
	   else
	        state%Cuptake_downreg = 1.0
	   endif

      if(state%lai > 0) then
          flux%GPP = Photosyn((state%leafN), state%lai, state%rstep)*state%Cuptake_downreg
      else
          flux%GPP = 0.0
      endif 
      
!----  PLANT N UPTAKE ----------

      if(state%labileN > state%maxNstore) then
	   		state%Nuptake_downreg = 0.0 
	   else
	        state%Nuptake_downreg = 1.0 - (state%labileN/state%maxNstore)
	   endif

      
      flux%nh4_uptake = Nupt(1, state%rootC, state%rootN, state%woodC,avail_nh4,state%Nuptake_downreg,flux%GPP,Ra_temp_resp)

      if(avail_nh4 >= flux%nh4_uptake) then
         avail_nh4 = avail_nh4 - flux%nh4_uptake
      else
         flux%nh4_uptake = avail_nh4
         avail_nh4 = 0.0
      endif
      
      flux%no3_uptake = Nupt(2, state%rootC, state%rootN, state%woodC,avail_no3,state%Nuptake_downreg,flux%GPP,Ra_temp_resp)
     
      if(avail_no3 >= flux%no3_uptake) then
         avail_no3 = avail_no3 - flux%no3_uptake
      else
         flux%no3_uptake = avail_no3
         avail_no3 = 0.0
      endif

!---- ALLOCATION ------------------------------

      CALL marginals(avail_NH4,avail_NO3,Ra_temp_resp)
      CALL marginal_integrator 
      
      state%target_rootCN = param%rootCN  ! SET HERE FOR NOW UNTIL WE GET DYNAMIC ROOT CN
      avail_C = max(state%labileC,0.0)
      avail_N = max(state%labileN,0.0)

      ! STEP 1: ALLOCATE ALL BUDC AND BUDN IN FIRST DAY OF SPRING LEAF OUT 
			
      if(param%t_leaf > (1.0/365.0)) then		!Deciduous 
          instant_Creturn_leafCN = (marg%GPP_leafCN - marg%Rm_leafCN)* &
             (param%SenescStart - clim%doy(state%rstep)) -  marg%Rg_leafC
      else	!evergreen 
          instant_Creturn_leafCN = (marg%GPP_leafCN - marg%Rm_leafCN) &
             /param%t_leaf -  marg%Rg_leafC           
      endif			

       
	  if(growth_potential > 0.0) then
	  	    
	  	tmp_leafC = state%leafC
	  
	 	if(instant_Creturn_leafCN >= param%add_C .or. state%leafC == 0.0) then

	      flux%a_labileC_bud_2leaf = min(state%labileC_bud, (state%maxleafC-state%leafC))
          flux%a_labileN_bud_2leaf = min(state%labileN_bud,  &
          	 ((state%labileN_bud/state%labileC_bud)*flux%a_labileC_bud_2leaf))
          state%min_wood_deficit = state%min_wood_deficit+ &
          	  flux%a_labileC_bud_2leaf*param%Minleaf2woodAlloc
          state%wood_requirement = state%wood_requirement + flux%a_labileC_bud_2leaf*param%Minleaf2woodAlloc

      	
         tmp_leafC = state%leafC + flux%a_labileC_bud_2leaf
         tmp_leafN = state%leafN + flux%a_labileN_bud_2leaf
      	 
    	endif
	  
     	  if(avail_N < 0.0) avail_N = 0.0
          if(avail_C < 0.0) avail_C = 0.0
     
     	if(tmp_leafC < state%maxleafC .AND. state%labileC_bud < state%maxleafC*param%leafC2budCprop) then 
         	! Continue filling the buds
         	flux%a_labileC_bud = min((avail_C * growth_potential)*(1-param%Ra_grow),(state%maxleafC - tmp_leafC))
         	flux%a_labileC_bud = max(0.0,flux%a_labileC_bud)
         	!CLOSE LEAFC IS CLOSE TO MAXLEAFC ADD LEAFC TO REACH MAXLEAFC
         	if((tmp_leafC+flux%a_labileC_bud) > state%maxleafC*param%stocks_close_prop .AND. &
         	tmp_leafC < state%maxleafC .AND. avail_C > (state%maxleafC-tmp_leafC)) then
            	flux%a_labileC_bud =  (state%maxleafC - tmp_leafC)
         	endif
                  
         	! CHECK THAT THERE IS ENOUGH N
         	potN = flux%a_labileC_bud / state%target_leafCN
         	if(potN > avail_N*growth_potential) then
            	flux%a_labileC_bud = avail_N*growth_potential * state%target_leafCN
         	endif
         	flux%Ra_grow = flux%a_labileC_bud*(param%Ra_grow)
         	flux%a_labileN_bud = flux%a_labileC_bud / state%target_leafCN
         	state%leafN_deficit = state%leafN_deficit + potN - flux%a_labileN_bud
         	state%total_N_deficit = state%total_N_deficit + potN - flux%a_labileN_bud

         	avail_C = avail_C - flux%a_labileC_bud - flux%Ra_grow
         	avail_N = avail_N - flux%a_labileN_bud
         	
          if(avail_N < 0.0) avail_N = 0.0
          if(avail_C < 0.0) avail_C = 0.0
          
          if(state%labileC_Ra < state%maxRaC) then
            	flux%a_labileC_2Ra = min((state%maxRaC-state%labileC_Ra),(avail_C))
            	avail_C = avail_C - flux%a_labileC_2Ra
          endif

     		!IF MAXLEAFC IS MET OR LEAF ALLOCATION DOESN'T HAVE A POSITIVE RETURN        
     	else
     	          ! START REFILLING THE RESPIRATION POOL

            if(avail_N < 0.0) avail_N = 0.0
          if(avail_C < 0.0) avail_C = 0.0
          
          if(state%labileC_Ra < state%maxRaC) then
            	flux%a_labileC_2Ra = min((state%maxRaC-state%labileC_Ra),(avail_C))
            	avail_C = avail_C - flux%a_labileC_2Ra
          endif
          
          if(avail_N < 0.0) avail_N = 0.0
          if(avail_C < 0.0) avail_C = 0.0

      	  ! START REFILLING BUDS  
        	if(state%labileC_bud < state%maxleafC*param%leafC2budCprop) then
            	if((avail_C * growth_potential)*(1-param%Ra_grow) < (state%maxleafC* &
            	param%leafC2budCprop - state%labileC_bud)) then
          			flux%a_labileC_bud = (avail_C * growth_potential)*(1-param%Ra_grow)
        					
            	else
          			flux%a_labileC_bud = (state%maxleafC*param%leafC2budCprop - state%labileC_bud)
          		endif

    			potN = flux%a_labileC_bud / state%target_leafCN
          		if(potN > avail_N*growth_potential) then
          			flux%a_labileC_bud = avail_N*growth_potential * state%target_leafCN
          		endif
          		flux%Ra_grow = flux%a_labileC_bud*(param%Ra_grow)
          		flux%a_labileN_bud = flux%a_labileC_bud / state%target_leafCN
          		avail_C = avail_C - flux%a_labileC_bud - flux%Ra_grow
          		state%leafN_deficit = state%leafN_deficit + potN - flux%a_labileN_bud   
          		state%total_N_deficit = state%total_N_deficit + potN - flux%a_labileN_bud         		
            	avail_N = avail_N - flux%a_labileN_bud
            	flux%Ra_grow = flux%Ra_grow+flux%a_labileC_bud*param%Ra_grow

        	endif
 


          if(avail_N < 0.0) avail_N = 0.0
          if(avail_C < 0.0) avail_C = 0.0

        	if(state%min_wood_deficit > 0.0) then
            	flux%a_woodC = min(avail_C*growth_potential*(1-param%Ra_grow),state%min_wood_deficit)
            	potN = flux%a_woodC / param%woodCN
  
            	if(potN > avail_N*growth_potential) then
          			flux%a_woodC = avail_N*growth_potential * param%woodCN

            	endif
            	flux%a_woodN = flux%a_woodC / param%woodCN
            	avail_C = avail_C - flux%a_woodC - flux%a_woodC*param%Ra_grow
            	flux%Ra_grow = flux%Ra_grow+flux%a_woodC*param%Ra_grow
            	state%min_wood_deficit = state%min_wood_deficit - flux%a_woodC
            	state%total_N_deficit = state%total_N_deficit + potN - flux%a_woodN
            	avail_N = avail_N - flux%a_woodN  
        	endif
     	
 
          if(avail_N < 0.0) avail_N = 0.0
          if(avail_C < 0.0) avail_C = 0.0

        	!START TRYING TO REACH MAX ROOT C      
        	if(state%rootC < state%maxrootC) then
            	flux%a_rootC = min((avail_C*growth_potential*(1-param%Ra_grow)), &
              	(state%maxrootC-state%rootC))
            	potN = flux%a_rootC / state%target_rootCN
            	if(potN > avail_N*growth_potential) then	
            	     
          		endif
            	flux%a_rootN = flux%a_rootC / state%target_rootCN
            	avail_C = avail_C - flux%a_rootC - flux%a_rootC*param%Ra_grow
            	flux%Ra_grow = flux%Ra_grow+flux%a_rootC*param%Ra_grow
            	state%total_N_deficit = state%total_N_deficit + potN - flux%a_rootN
            	avail_N = avail_N - flux%a_rootN  
         	endif
         	
          if(avail_N < 0.0) avail_N = 0.0
          if(avail_C < 0.0) avail_C = 0.0


        	if(state%leafC >= state%maxleafC*param%stocks_close_prop .and. &
      		state%rootC >= state%maxrootC*param%stocks_close_prop .AND. &
        	state%labileC_Ra >= state%maxRaC*param%stocks_close_prop .AND. &
        	state%min_wood_deficit == 0 .AND. avail_C > state%maxCstore) then
            	tmp_a_woodC = (avail_C-state%maxCstore)*growth_potential*(1-param%Ra_grow)
            	potN = tmp_a_woodC / param%woodCN
            	if(potN > avail_N*growth_potential) then
          			tmp_a_woodC = avail_N*growth_potential * param%woodCN     			
          		endif
            	tmp_a_woodN = tmp_a_woodC / param%woodCN
            	avail_C = avail_C - tmp_a_woodC - tmp_a_woodC*param%Ra_grow
            	flux%Ra_grow = flux%Ra_grow+tmp_a_woodC*param%Ra_grow
            	flux%a_woodC = flux%a_woodC + tmp_a_woodC
            	flux%a_woodN = flux%a_woodN + tmp_a_woodN                 
            	avail_N = avail_N - tmp_a_woodN 
             	avail_C = avail_C - tmp_a_woodC

        	endif
        	
        	if(avail_N < 0.0) avail_N = 0.0
            if(avail_C < 0.0) avail_C = 0.0
      	
        	if(avail_C > state%maxCstore .and. avail_N < state%maxNstore) then
        			flux%Ra_excessC =  (avail_C - state%maxCstore)*growth_potential* param%t_excessC
      	    		flux%nfix = flux%Ra_excessC*param%Nfix_per_gC*Ra_temp_resp*state%Nuptake_downreg
      	  		    avail_C = avail_C - flux%Ra_excessC
      	  endif        	

     	endif
     
     endif
     

     !FILL RESPIRATION IF IT DIPS BELOW MAX 
     if(growth_potential == 0.0 .AND. state%labileC_Ra < state%maxRaC .AND. avail_C > 0) then   
        flux%a_labileC_2Ra = min((state%maxRaC-state%labileC_Ra),(avail_C))

        avail_C = avail_C - flux%a_labileC_2Ra
     endif 
     
       
!----  NITRIFICATION -------------

      flux%nitr = avail_nh4 * param%nitr_rate * Rh_temp_resp 
      if(avail_nh4 >=flux%nitr) then
         avail_nh4 = avail_nh4 - flux%nitr 
      else
         flux%nitr = avail_nh4
         avail_nh4 = 0.0
      endif
             
!------- LEACHING -----------------

      flux%leachN = avail_no3 * param%leach_rate
      if(avail_no3 >= flux%leachN) then
        avail_no3 = avail_no3 - flux%leachN
      else
        flux%leachN = avail_no3
        avail_no3 = 0.0
      endif

!------ N INPUTS --------------------

      flux%ndep_nh4 = clim%NH4dep(rstep)
      flux%ndep_no3 = clim%NO3dep(rstep)
!----- AUTOTROPHIC RESPIRATION --------------

      if(param%use_reich == 0) then
          flux%Ra_Main = (state%leafN + state%rootN)*param%Ra_per_N*Ra_temp_resp
      else
          flux%Ra_Main = reich_resp(state%leafC,state%leafN,param%leaf_resp_A,&
             param%leaf_resp_B)*Ra_temp_resp + reich_resp(state%rootC,state%rootN,&
             param%leaf_resp_A,param%leaf_resp_B)*Ra_temp_resp
      endif
      flux%Ra_retrans = 0.0 !NOT CURRENTLY USED

          
!--- MOVEMENT BETWEEN STORAGE POOLS IF LABILE C GOES NEGATIVE------
      ! THIS PREVENTS THE RESPIRATION POOL FROM GOING NEGATIVE BY USING SOME OF THE BUD C
      ! IT "PUNISHES" THE VEGETATION FOR AN ABNORMALLY LONG WINTER

      if((state%labileC_Ra + flux%a_labileC_2Ra - flux%Ra_main) < 0) then

           flux%a_labileC_2Ra = flux%a_labileC_2Ra + min(flux%Ra_main,state%labileC)          
           if(state%labileC_bud > -(state%labileC_Ra + flux%a_labileC_2Ra - flux%Ra_main)) then
           		labileC_bud2labile_Ra = -(state%labileC_Ra + flux%a_labileC_2Ra - flux%Ra_main)
           		labileN_bud2labileN = labileC_bud2labile_Ra * (state%labileN_bud/state%labileC_bud)
           elseif(state%labileC_bud < -(state%labileC_Ra + flux%a_labileC_2Ra - flux%Ra_main).and. state%labileC_bud > 0.0 ) then
                 print *,'DEAD! - All labile C pools are 0 ',state%labileC
                 labileC_bud2labile_Ra = 0.0
                 labileN_bud2labileN = 0.0
           endif
      else
           labileC_bud2labile_Ra = 0.0
           labileN_bud2labileN = 0.0  
      endif

!------ PRODUCTIVITY CALCULATIONS ------------

      ! NOTE: Ra_total is diagnostic because the individual fluxes are now 
      ! taken out of there respective labile pools
      flux%Ra_total = flux%Ra_Main + flux%Ra_Grow + flux%Ra_excessC 
      flux%NPP = flux%GPP - flux%Ra_total 
      flux%NEE = flux%npp - flux%Rh_total 

!-------UPDATE STATE VARIABLES---------------------------

      !plant tissues
      state%leafC   = state%leafC + flux%a_labileC_bud_2leaf - &
                      flux%t_leafC            
      state%woodC   = state%woodC + flux%a_woodC - flux%t_woodC
      state%rootC   = state%rootC + flux%a_rootC - flux%t_rootC

      state%leafN   = state%leafN  + flux%a_labileN_bud_2leaf-  &
                      flux%t_leafN - flux%retransN
                      

      state%woodN   = state%woodN + flux%a_woodN - flux%t_woodN
      state%rootN   = state%rootN + flux%a_rootN - flux%t_rootN

      ! LABILE POOLS
      state%labileC = state%labileC + flux%GPP - flux%Ra_excessC - &
                      flux%a_labileC_bud  - flux%a_woodC - &
                      flux%a_rootC -flux%Ra_grow - flux%a_labileC_2Ra
                                      
      state%labileC_bud = state%labileC_bud + flux%a_labileC_bud - &
                      flux%a_labileC_bud_2leaf - labileC_bud2labile_Ra
                  
      state%labileC_Ra = state%labileC_Ra + flux%a_labileC_2Ra - &
                     flux%Ra_main + labileC_bud2labile_Ra
                         
      state%labileN = state%labileN + flux%no3_uptake + flux%nh4_uptake + & 
                     flux%retransN + flux%Nfix - flux%a_labileN_bud - &
                     flux%a_woodN - flux%a_rootN + labileN_bud2labileN  
                     
      state%labileN_bud = state%labileN_bud + flux%a_labileN_bud - &
                      flux%a_labileN_bud_2leaf - labileN_bud2labileN  
                                               
      !dead organic matter
      state%litterC = state%litterC + flux%t_leafC + flux%t_rootC + &
                flux%t_cwdC - flux%t_litterC
      state%litterN = state%litterN + flux%t_leafN + flux%t_rootN + &
                flux%t_cwdN - flux%t_litterN 
         
      state%soilC = state%soilC + litter_to_soil - flux%t_soilC 
      state%soilN = state%soilN + flux%t_litterN - flux%t_soilN + &
                flux%nh4_immob + flux%no3_immob-flux%leachDON 
         
      state%cwdC = state%cwdC + flux%t_woodC - flux%t_cwdC
      state%cwdN = state%cwdN + flux%t_woodN - flux%t_cwdN

      !inorganic pools
      state%nh4 = state%nh4 + flux%ndep_nh4 + flux%t_soilN - &
               flux%nh4_uptake - flux%nh4_immob - flux%nitr 
                
      state%no3 = state%no3 + flux%ndep_no3 + flux%nitr - & 
                flux%no3_uptake - flux%no3_immob - flux%leachN     

! ---- UPDATE SUMMARY VARIABLES --------
    
      state%totvegc = state%leafC + state%woodC + state%rootC + & 
               state%labileC + state%labileC_bud + state%labileC_Ra
      state%totvegn = state%leafN + state%woodN + state%rootN + &
               state%labileN + state%labileN_bud
      state%totalC = state%totvegc + state%litterC +state%cwdC + &
               state%soilC  
      state%totalN = state%totvegn + state%litterN + state%cwdN +&
               state%soilN + state%nh4 + state%no3         
      flux%net_nmin =(flux%t_soilN -flux%nh4_immob - flux%no3_immob)


      ! SET CURRENT VEGETATION C:N RATIOS

      if(state%leafN > 0) then
        state%leafCN = state%leafC/state%leafN
      else
      	state%leafCN = state%target_leafCN
      endif

      state%woodCN = state%woodC/state%woodN
      state%rootCN = state%rootC/state%rootN 
      
      !SET MAX STORE SIZES
      state%maxNstore = (state%rootC + state%woodC) * param%Nlabile_prop
      state%MaxCstore = (state%rootC + state%woodC) * param%Clabile_prop
      state%maxRaC =    (state%rootC + state%woodC) * param%RaClabile_prop
    	  
      call year_update(0,year_count)

      pass = BalanceCheck()

      end subroutine ecosystem_dynamics

end module aconite_ecosystem 