      module aconite_io

!-----------------------------------------------------------------------
!BOP
!
! !MODULE: 
!
! !DESCRIPTION:
!
! !USES:

     use aconite_type
! !PUBLIC TYPES:
      implicit none
      public :: read_climate
      public :: write_restart
      public :: read_restart
      public :: write_daily_output
      public :: write_annual_output
      public :: read_parameters
      public :: read_namelist
      public :: read_site_data
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
! !IROUTINE: 
!
! !INTERFACE:
      subroutine read_climate(clim,clim_length)

! !USES:
!
! !ARGUMENTS:
      implicit none
!
! LOCAL VARAIBLES:
   integer :: clim_length
   real,pointer :: climate_data(:,:)
   !real :: climate_data(11,10585)
   !real :: climate_data(11,5)
   integer:: nlines,i
   type (clim_type), intent(inout):: clim  
!EOP
!------------------------------------------------------------------------

     allocate(climate_data(10,clim_length))
     open(11,file = io%clim_in, status = 'old')
     
     nlines = clim_length

     do i = 1,nlines
         read(11,*) climate_data(1,i),climate_data(2,i),climate_data(3,i),climate_data(4,i),climate_data(5,i),&
         climate_data(6,i),climate_data(7,i),climate_data(8,i),climate_data(9,i),climate_data(10,i)
     end do
     close(11)

      allocate(clim%tmin(nlines))
      allocate(clim%tmax(nlines))
      allocate(clim%doy(nlines))
      allocate(clim%CO2(nlines))
      allocate(clim%rad(nlines))
      allocate(clim%prec(nlines))
      allocate(clim%year(nlines))
      allocate(clim%NO3dep(nlines))
      allocate(clim%NH4dep(nlines))

     clim%year(:) = climate_data(2,:)
     clim%doy(:) = climate_data(3,:)
     clim%rad(:) = climate_data(4,:)
     clim%prec(:) = climate_data(5,:)
     clim%tmax(:) = climate_data(6,:)
     clim%tmin(:) = climate_data(7,:)
     clim%NH4dep(:) = (climate_data(8,:)/365)
     clim%NO3dep(:) = climate_data(9,:)/365
     clim%CO2(:) = climate_data(10,:)
     deallocate(climate_data)
   
     
    end subroutine read_climate

!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: 
!
! !INTERFACE:
      subroutine read_parameters

! !USES:
!
! !ARGUMENTS:
      implicit none
!
! LOCAL VARAIBLES:
   real,pointer :: param_in(:)
   character(len=200) fname
   integer:: params,i
!EOP
!------------------------------------------------------------------------
     params = 65
     allocate(param_in(params))
     open(11,file = io%param_in, status = 'OLD')
     do i = 1,params
         read(11,*) param_in(i)
     end do
     close(11)

      param%root_radius= param_in(1)
      param%bufferNH4= param_in(2)
      param%diffNH4= param_in(3)
      param%bufferNO3= param_in(4)
      param%diffNO3 = param_in(5)
      param%imax= param_in(6)
      param%rooting_depth = param_in(7)
      param%vo = param_in(8)
      param%km= param_in(9)
      param%C_conc = param_in(10)
      param%root_density = param_in(11)
      param%a1= param_in(12)
      param%a2= param_in(13)
      param%a3= param_in(14)
      param%a4= param_in(15)
      param%a5= param_in(16)
      param%a6= param_in(17)
      param%a7= param_in(18)
      param%a8= param_in(19)
      param%a9= param_in(20)
      param%a10= param_in(21)
      param%lca = param_in(22)
      param%t_leaf = param_in(23)
      param%t_wood= param_in(24)
      param%t_root= param_in(25)
      param%t_excessC = param_in(26)
      param%t_cwd = param_in(27)
      param%trans_frac= param_in(28)
      param%t_litter= param_in(29)
      param%mCUE= param_in(30)
      param%t_soil= param_in(31)
      param%nitr_rate = param_in(32)
      param%leach_rate = param_in(33)
      param%Nturn_dep_loss = param_in(34)
      param%Ra_Q10 = param_in(35)
      param%Rh_Q10 = param_in(36)
      param%Ra_per_N = param_in(37)
      param%leaf_resp_A= param_in(38)
      param%leaf_resp_B= param_in(39)
      param%root_resp_A= param_in(40) !SAME AS LEAF
      param%root_resp_B= param_in(41) !SAME AS LEAF 
      param%Ra_grow= param_in(42)
      !param_in(43) ! NOT BEING USED
      param%soilCN = param_in(44)
      param%leafCN = param_in(45)
      param%woodCN = param_in(46)
      param%rootCN = param_in(47)
      param%GDDStart = param_in(48)
      param%SenescStart = param_in(49)
      param%Clabile_prop = param_in(50)
      param%Nlabile_prop = param_in(51)
      param%RaClabile_prop = param_in(52)
      param%add_C = param_in(53)
      param%Nfix_per_gC = param_in(54)
      !param_in(55) NOT BEING USED
      param%MaxAllocAdjust = param_in(56) 
      param%Minleaf2woodAlloc = param_in(57)
      param%LeafC2budCprop = param_in(58)
      !param_in(59) !NOT BEING USED
      param%leaf2root_ratio = param_in(60) 
      param%use_reich = param_in(61)
      param%stocks_close_prop = param_in(62)
      param%a11 = param_in(63)
      param%growth_potential_param  = param_in(64)
      param%leaf_shed_rate = param_in(65)
      
     if(io%cold_start == 1) then
        state%leafN = state%leafC/param%leafCN
        state%target_leafCN = param%leafCN
        state%labileN_bud = state%labileC_bud/param%leafCN
        state%rootN = state%rootC/param%rootCN
        state%target_rootCN = param%rootCN
        state%woodN = state%woodC/param%woodCN
        state%cwdN= state%cwdC/param%woodCN
        state%soilN = state%soilC/param%soilCN
        state%totvegn_prev=state%leafN+state%woodN+state%rootN+state%labileN+state%labileN_bud
        state%totalN_prev = state%totvegn_prev + state%litterN + state%cwdN+ state%soilN + state%nh4 + state%no3
        state%MaxNstore = state%woodN * param%Nlabile_prop
        state%cwdN_prev=state%cwdN
        state%soilN_prev = state%soilN
        state%litterN_prev=state%litterN
        state%leafCN = state%leafC/state%leafN
        state%woodCN = state%woodC/state%woodN
        state%rootCN = state%rootC/state%rootN
     endif
  deallocate(param_in)
    end subroutine read_parameters

!------------------------------------------------------------------------

!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: 
!
! !INTERFACE:
      subroutine read_site_data()

! !USES:
!
! !ARGUMENTS:
      implicit none
!
! LOCAL VARAIBLES:
   real,pointer :: site_in(:)
   character(len=200) fname
   integer:: site_params,i
!EOP
!------------------------------------------------------------------------

     site_params = 2
     allocate(site_in(site_params))
     open(11,file = io%site_in, status = 'OLD')
     do i = 1,site_params
         read(11,*) site_in(i)
     end do
     close(11)

      site%Lat = site_in(1)
      site%seasonal = site_in(2)

     deallocate(site_in)
    end subroutine read_site_data

!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: 
!
! !INTERFACE:
      subroutine write_restart()

! !USES:
! !ARGUMENTS:
      implicit none
!
! LOCAL VARAIBLES:
   character(len=200) fname
   integer:: nlines,i
   real :: restart_out(102)
!EOP
!------------------------------------------------------------------------

		  restart_out(1)= state%leafC*0.05 
          restart_out(2)=state%woodC*0.05 
          restart_out(3)= state%rootC*0.05
          restart_out(4) = state%leafN*0.05
          restart_out(5) = state%woodN*0.05
          restart_out(6) = state%rootN*0.05 
          restart_out(7) = state%labileC*0.05 
          restart_out(8) = state%labileN*0.05 
          restart_out(9)= state%labileC_bud*0.05
          restart_out(10) = state%labileC_Ra*0.05 
          restart_out(11) = state%labileN_bud*0.05 
          restart_out(12) = state%litterC
          restart_out(13) = state%litterN
          restart_out(14) = state%soilC 
          restart_out(15) = state%soilN
          restart_out(16) = state%cwdC
          restart_out(17) = state%cwdN
          restart_out(18) = state%nh4
          restart_out(19) = state%no3
          restart_out(20) = state%psid
          restart_out(21) = state%rtot
          restart_out(22) =  state%Nuptake_downreg
          restart_out(23) = state%Cuptake_downreg  
          restart_out(24) = state%maxleafC*0.05
          restart_out(25) = state%hitmaxleafC 
          restart_out(26) = state%maxrootC*0.05
          restart_out(27) = state%hitmaxrootC
          restart_out(28) = state%maxRaC*0.05
          restart_out(29) = state%min_wood_deficit
          restart_out(30) = marg%integ_Creturn_leafC
          restart_out(31) = marg%integ_Nreturn_leafC
          restart_out(32) = marg%integ_Creturn_rootC
          restart_out(33)= marg%integ_Nreturn_rootC
          restart_out(34) = marg%integ_Creturn_Raexcess
          restart_out(35) = marg%integ_Nreturn_Raexcess
          restart_out(36) = marg%integ_Creturn_leafN
          restart_out(37) = marg%integ_Nreturn_leafN
          restart_out(38) = marg%integ_Creturn_rootN
          restart_out(39)= marg%integ_Nreturn_rootN
          restart_out(40) = 0.0 !NOT USED
          restart_out(41) = 0.0 !NOT USED
          restart_out(42) = 0.0 !NOT USED
          restart_out(43) = 0.0 !NOT USED
          restart_out(44) = marg%integ_Creturn_leafCN
          restart_out(45) = marg%integ_Nreturn_rootCN
          restart_out(46) = marg%integ_GPP_leafC
          restart_out(47) = marg%integ_Rm_leafC
          restart_out(48) = marg%integ_Rg_leafC
          restart_out(49) = marg%integ_GPP_leafN
          restart_out(50) = marg%integ_addN_leafN
          restart_out(51) = marg%integ_Rm_leafN
          restart_out(52) = marg%integ_GPP_leafCN
          restart_out(53) = marg%integ_Rm_leafCN
          restart_out(54) = marg%integ_Rm_rootC
          restart_out(55) = marg%integ_Rg_rootC
          restart_out(56)  = marg%integ_Nuptake_rootC
          restart_out(57) = marg%integ_Rm_rootN
          restart_out(58) = marg%integ_Nuptake_rootN
          restart_out(59) = marg%integ_addN_rootN        
          restart_out(60) = marg%integ_Nuptake_rootCN   
          restart_out(61) = 0.0 !NOT USED
          restart_out(62) = marg%integ_Nreturn_Raexcess
          restart_out(63) = marg%annual_Creturn_leafC
          restart_out(64) = marg%annual_Nreturn_leafC
          restart_out(65) = marg%annual_Creturn_rootC
          restart_out(66) = marg%annual_Nreturn_rootC
          restart_out(67) = marg%annual_Creturn_Raexcess
          restart_out(68) = marg%annual_Nreturn_Raexcess
          restart_out(69) = marg%annual_Creturn_leafN
          restart_out(70) = marg%annual_Nreturn_leafN
          restart_out(71) = marg%annual_Creturn_rootN
          restart_out(72) = marg%annual_Nreturn_rootN
          restart_out(73) = 0.0 !NOT USED
          restart_out(74) = 0.0 !NOT USED
          restart_out(75) = 0.0 !NOT USED
          restart_out(76) = 0.0 !NOT USED
          restart_out(77) = marg%annual_Creturn_leafCN
          restart_out(78) = marg%annual_Nreturn_rootCN
          restart_out(79) = marg%annual_GPP_leafC
          restart_out(80) = marg%annual_Rm_leafC
          restart_out(81) = marg%annual_Rg_leafC
          restart_out(82) = marg%annual_GPP_leafN
          restart_out(83) = marg%annual_addN_leafN
          restart_out(84) = marg%annual_Rm_leafN
          restart_out(85) = marg%annual_GPP_leafCN
          restart_out(86) = marg%annual_Rm_leafCN
          restart_out(87) = marg%annual_Rm_rootC
          restart_out(88) = marg%annual_Rg_rootC
          restart_out(89) = marg%annual_Nuptake_rootC
          restart_out(90) = marg%annual_Rm_rootN
          restart_out(91) = marg%annual_Nuptake_rootN
          restart_out(92) = marg%annual_addN_rootN       
          restart_out(93) = marg%annual_Nuptake_rootCN
          restart_out(94) = 0.0 !NOT USED
          restart_out(95) = marg%annual_Nreturn_Raexcess
          restart_out(96) = marg%mean_leafC
          restart_out(97) = marg%mean_leafN
          restart_out(98) = marg%mean_rootC
          restart_out(99) = marg%mean_rootN
          restart_out(100) = marg%day_count_leaf
          restart_out(101) = marg%integ_hitmaxleafC
          restart_out(102) = marg%integ_hitmaxrootC
 
        OPEN (7, FILE = io%restart_out, STATUS = 'UNKNOWN')
       WRITE(7,*) restart_out
       CLOSE(7)         

      end subroutine write_restart
!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: 
!
! !INTERFACE:
      subroutine read_restart()

! !USES:
!
! !ARGUMENTS:
      implicit none
!
! LOCAL VARAIBLES:
   character(len=200) fname
   integer:: nlines,i
   real :: restart_in(102)
!EOP
!------------------------------------------------------------------------

    open(7,file = io%restart_in, STATUS = 'OLD')
    read(7,*) restart_in(:)
    close(7)

          state%leafC= restart_in(1)
          state%woodC =restart_in(2)
          state%rootC= restart_in(3)
          state%leafN= restart_in(4)
          state%woodN= restart_in(5)
          state%rootN= restart_in(6) 
          state%labileC= restart_in(7)
          state%labileN= restart_in(8)
          state%labileC_bud=restart_in(9)
          state%labileC_Ra= restart_in(10)
          state%labileN_bud= restart_in(11)
          state%totvegc_prev=state%leafC+state%woodC+state%rootC+state%labileC+ state%labileC_bud + &
               state%labileC_Ra
          state%totvegn_prev=state%leafN+state%woodN+state%rootN+state%labileN+state%labileN_bud
          state%litterC=restart_in(12)
          state%litterN=restart_in(13)
          state%soilC= restart_in(14)
          state%soilN= restart_in(15)
          state%cwdC= restart_in(16)
          state%cwdN= restart_in(17)
          state%totalC_prev = state%totvegc_prev + state%litterC + state%cwdC + state%soilC  
          state%nh4=restart_in(18)
          state%no3=restart_in(19)
          state%totalN_prev = state%totvegn_prev + state%litterN + state%cwdN+ state%soilN + state%nh4 + state%no3
          state%psid=restart_in(20)
          state%rtot=restart_in(21)
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
          state%Nuptake_downreg = restart_in(22)
          state%Cuptake_downreg = restart_in(23)
          state%MaxNstore = state%woodN * param%Nlabile_prop
          state%MaxCstore = state%rootC * param%Clabile_prop
          state%maxleafC = max(restart_in(24),state%leafC)
          state%hitmaxleafC = restart_in(25)
          state%maxrootC = max(restart_in(26),state%rootC)
          state%hitmaxrootC = restart_in(27)
          state%maxRaC = restart_in(28)
          state%maxRaC =  max(restart_in(28),state%labileC_Ra)
          state%min_wood_deficit = restart_in(29)
          marg%integ_Creturn_leafC=restart_in(30)
          marg%integ_Nreturn_leafC=restart_in(31)
          marg%integ_Creturn_rootC=restart_in(32)
          marg%integ_Nreturn_rootC=restart_in(33)
          marg%integ_Creturn_Raexcess=restart_in(34)
          marg%integ_Nreturn_Raexcess=restart_in(35)
          marg%integ_Creturn_leafN=restart_in(36)
          marg%integ_Nreturn_leafN=restart_in(37)
          marg%integ_Creturn_rootN=restart_in(38)
          marg%integ_Nreturn_rootN=restart_in(39)
          !restart_in(40)  NOT CURRENTLY USED
          !restart_in(41)  NOT CURRENTLY USED
          !restart_in(42)  NOT CURRENTLY USED
          !restart_in(43)  NOT CURRENTLY USED
          marg%integ_Creturn_leafCN=restart_in(44)
          marg%integ_Nreturn_rootCN=restart_in(45)
          marg%integ_GPP_leafC=restart_in(46)
          marg%integ_Rm_leafC=restart_in(47)
          marg%integ_Rg_leafC=restart_in(48)
          marg%integ_GPP_leafN=restart_in(49)
          marg%integ_addN_leafN=restart_in(50)
          marg%integ_Rm_leafN=restart_in(51)
          marg%integ_GPP_leafCN=restart_in(52)
          marg%integ_Rm_leafCN=restart_in(53)
          marg%integ_Rm_rootC=restart_in(54)
          marg%integ_Rg_rootC=restart_in(55)
          marg%integ_Nuptake_rootC=restart_in(56) 
          marg%integ_Rm_rootN=restart_in(57)
          marg%integ_Nuptake_rootN=restart_in(58)  
          marg%integ_addN_rootN=restart_in(59)             
          marg%integ_Nuptake_rootCN=restart_in(60)    
          !restart_in(61)  NOT CURRENTLY USED
          marg%integ_Nreturn_Raexcess=restart_in(62)
          marg%annual_Creturn_leafC=restart_in(63)
          marg%annual_Nreturn_leafC=restart_in(64)
          marg%annual_Creturn_rootC=restart_in(65)
          marg%annual_Nreturn_rootC=restart_in(66)
          marg%annual_Creturn_Raexcess=restart_in(67)
          marg%annual_Nreturn_Raexcess=restart_in(68)
          marg%annual_Creturn_leafN=restart_in(69)
          marg%annual_Nreturn_leafN=restart_in(70)
          marg%annual_Creturn_rootN=restart_in(71)
          marg%annual_Nreturn_rootN=restart_in(72)
          !restart_in(73)  NOT CURRENTLY USED
          !restart_in(74)  NOT CURRENTLY USED
          !restart_in(75)  NOT CURRENTLY USED
          !restart_in(76)  NOT CURRENTLY USED
          marg%annual_Creturn_leafCN=restart_in(77)
          marg%annual_Nreturn_rootCN=restart_in(78)
          marg%annual_GPP_leafC=restart_in(79)
          marg%annual_Rm_leafC=restart_in(80)
          marg%annual_Rg_leafC=restart_in(81)
          marg%annual_GPP_leafN=restart_in(82)
          marg%annual_addN_leafN=restart_in(83)
          marg%annual_Rm_leafN=restart_in(84)
          marg%annual_GPP_leafCN=restart_in(85)
          marg%annual_Rm_leafCN=restart_in(86)
          marg%annual_Rm_rootC=restart_in(87)
          marg%annual_Rg_rootC=restart_in(88)
          marg%annual_Nuptake_rootC  =restart_in(89)
          marg%annual_Rm_rootN=restart_in(90)
          marg%annual_Nuptake_rootN   =restart_in(91)
          marg%annual_addN_rootN    =restart_in(92)         
          marg%annual_Nuptake_rootCN =restart_in(93)  
          !restart_in(94) NOT CURRENTLY USED
          marg%annual_Nreturn_Raexcess=restart_in(95) 
          marg%mean_leafC=restart_in(96)
          marg%mean_leafN=restart_in(97)
          marg%mean_rootC=restart_in(98)
          marg%mean_rootN=restart_in(99)
          marg%day_count_leaf  =restart_in(100)
          marg%integ_hitmaxleafC =restart_in(101)
          marg%integ_hitmaxrootC = restart_in(102) 

      end subroutine read_restart

!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: 
!
! !INTERFACE:
      subroutine write_daily_output(model_year,mstep)

! !USES:
!
! !ARGUMENTS:
      implicit none
!
! LOCAL VARAIBLES:
   character(len=200) fname
   real :: daily_out(101)
   integer :: model_year
   integer :: mstep
!EOP
!------------------------------------------------------------------------

daily_out(1) = state%lai
daily_out(2) = state%leafC
daily_out(3) = state%woodC
daily_out(4) = state%rootC
daily_out(5) = state%labileC
daily_out(6) = state%leafN
daily_out(7) = state%woodN
daily_out(8) = state%rootN
daily_out(9) = state%labileN
daily_out(10) = state%totvegc
daily_out(11) = state%totvegn
daily_out(12) = state%cwdC
daily_out(13) = state%cwdN
daily_out(14) = state%litterC
daily_out(15) = state%litterN
daily_out(16) = state%soilC
daily_out(17) = state%soilN
daily_out(18) = state%nh4
daily_out(19) = state%no3
daily_out(20) = flux%GPP
daily_out(21) = flux%NPP
daily_out(22) = flux%NEE
daily_out(23) = flux%Rh_total
daily_out(24) = flux%no3_uptake + flux%nh4_uptake
daily_out(25) = flux%nh4_immob + flux%no3_immob
daily_out(26) = flux%net_Nmin
daily_out(27) = flux%leachN
daily_out(28) = state%leafCN
daily_out(29) = state%woodCN
daily_out(30) = state%rootCN
daily_out(31) = flux%a_leafC
daily_out(32) = flux%a_woodC
daily_out(33) = flux%a_rootC
daily_out(34) = flux%a_leafN
daily_out(35) = flux%a_woodN
daily_out(36) = flux%a_rootN
daily_out(37) = flux%Nfix
daily_out(38) = flux%nitr
daily_out(39) = flux%t_leafC
daily_out(40) = flux%t_woodC
daily_out(41) = flux%t_rootC
daily_out(42) = flux%t_leafN
daily_out(43) = flux%t_woodN
daily_out(44) = flux%t_rootN
daily_out(45) = flux%Ra_total
daily_out(46) = flux%Ra_grow
daily_out(47) = flux%Ra_main
daily_out(48) = flux%Ra_excessC
daily_out(49) = flux%retransN
daily_out(50) = state%MaxCstore
daily_out(51) = state%MaxNstore
daily_out(52) = state%Nuptake_downreg 
daily_out(53) = 0.0 !NOT USED
daily_out(54) = 0.0 ! NOT USED
daily_out(55) = 0.0 ! NOT USED
daily_out(56) = marg%Creturn_leafC
daily_out(57) = marg%Nreturn_leafC
daily_out(58) = marg%Creturn_rootC
daily_out(59) = marg%Nreturn_rootC
daily_out(60) = marg%Creturn_Raexcess
daily_out(61) = marg%Nreturn_Raexcess
daily_out(62) = marg%Creturn_leafN
daily_out(63) = marg%Nreturn_leafN
daily_out(64) = marg%Creturn_rootN
daily_out(65) = marg%Nreturn_rootN
daily_out(66) = 0.0 !NOT USED
daily_out(67) = 0.0 ! NOT USED
daily_out(68) = 0.0 !NOT USED
daily_out(69) = 0.0 ! NOTE USED
daily_out(70) = marg%integ_Creturn_leafC
daily_out(71) = marg%integ_Nreturn_leafC
daily_out(72) = marg%integ_Creturn_rootC
daily_out(73) = marg%integ_Nreturn_rootC
daily_out(74) = marg%integ_Creturn_Raexcess
daily_out(75) = marg%integ_Nreturn_Raexcess
daily_out(76) = marg%integ_Creturn_leafN
daily_out(77) = marg%integ_Nreturn_leafN
daily_out(78) = marg%integ_Creturn_rootN
daily_out(79) = marg%integ_Nreturn_rootN
daily_out(80) = 0.0 ! NOT USED
daily_out(81) = 0.0 ! NOT USED
daily_out(82) = 0.0 ! NOT USED
daily_out(83) = 0.0 ! NOT USED
daily_out(84) = state%a_leafC
daily_out(85) = state%a_woodC
daily_out(86) = state%a_rootC
daily_out(87) = state%a_NfixC
daily_out(88) = 0.0 ! NOT USED
daily_out(89) = state%debug2
daily_out(90) = state%debug
daily_out(91) = marg%integ_Creturn_leafCN
daily_out(92) = marg%integ_Nreturn_rootCN
daily_out(93) = state%Cuptake_downreg
daily_out(94) = state%labileC_bud
daily_out(95) = state%labileN_bud
daily_out(96) = state%labileC_Ra
daily_out(97) = state%target_leafCN
daily_out(98) = state%target_rootCN
daily_out(99) = state%maxleafC
daily_out(100) = state%maxrootC
daily_out(101) = flux%a_labileC_bud_2leaf

open(7,file = io%day_out, status='OLD',ACCESS = 'APPEND')
write(7,'(101(F12.4))') daily_out(:)
close(7)
end subroutine write_daily_output


!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: 
!
! !INTERFACE:
      subroutine write_annual_output(rstep,model_year,mstep)

! !USES:
!
! !ARGUMENTS:
      implicit none
!
! LOCAL VARAIBLES:
   character(len=200) fname
   character(len=500) header_row
   real :: annual_out(100)
   integer :: rstep
   integer :: model_year
   integer :: mstep
!EOP
 !------------------------------------------------------------------------
annual_out(1) = state%annual_lai 
annual_out(2) = state%annual_leafC
annual_out(3) = state%annual_woodC
annual_out(4) = state%annual_rootC
annual_out(5) = state%annual_labileC
annual_out(6) = state%annual_leafN
annual_out(7) = state%annual_woodN
annual_out(8) = state%annual_rootN
annual_out(9) = state%annual_labileN
annual_out(10) = state%annual_totvegc
annual_out(11) = state%annual_totvegn
annual_out(12) = state%annual_cwdC
annual_out(13) = state%annual_cwdN
annual_out(14) = state%annual_litterC
annual_out(15) = state%annual_litterN
annual_out(16) = state%annual_soilC
annual_out(17) = state%annual_soilN
annual_out(18) = state%annual_nh4
annual_out(19) = state%annual_no3
annual_out(20) = flux%annual_GPP
annual_out(21) = flux%annual_NPP
annual_out(22) = flux%annual_NEE
annual_out(23) = flux%annual_Rh
annual_out(24) = flux%annual_Nuptake 
annual_out(25) = flux%annual_immob
annual_out(26) = flux%annual_netNmin
annual_out(27) = flux%annual_leachN
annual_out(28) = state%annual_leafCN
annual_out(29) = state%annual_woodCN
annual_out(30) = state%annual_rootCN
annual_out(31) = flux%annual_a_leafC
annual_out(32) = flux%annual_a_woodC
annual_out(33) = flux%annual_a_rootC
annual_out(34) = flux%annual_a_leafN
annual_out(35) = flux%annual_a_woodN
annual_out(36) = flux%annual_a_rootN
annual_out(37) = flux%annual_Nfix
annual_out(38) = flux%annual_nitr
annual_out(39) = flux%annual_t_leafC
annual_out(40) = flux%annual_t_woodC
annual_out(41) = flux%annual_t_rootC
annual_out(42) = flux%annual_t_leafN
annual_out(43) = flux%annual_t_woodN
annual_out(44) = flux%annual_t_rootN
annual_out(45) = flux%annual_Ra
annual_out(46) = flux%annual_Ra_grow
annual_out(47) = flux%annual_Ra_main
annual_out(48) = flux%annual_Ra_excessC
annual_out(49) = flux%annual_retransN
annual_out(50) = state%annual_MaxCstore
annual_out(51) = state%annual_MaxNstore
annual_out(52) = state%annual_Nuptake_downreg
annual_out(53) = 0.0 ! NOT USED
annual_out(54) = 0.0 ! NOT USED
annual_out(55) = 0.0 ! NOT USED
annual_out(56) = marg%annual_Creturn_leafC
annual_out(57) = marg%annual_Nreturn_leafC
annual_out(58) = marg%annual_Creturn_rootC
annual_out(59) = marg%annual_Nreturn_rootC
annual_out(60) = marg%annual_Creturn_Raexcess
annual_out(61) = marg%annual_Nreturn_Raexcess
annual_out(62) = marg%annual_Creturn_leafN
annual_out(63) = marg%annual_Nreturn_leafN
annual_out(64) = marg%annual_Creturn_rootN
annual_out(65) = marg%annual_Nreturn_rootN
annual_out(66) = 0.0 ! NOT USED
annual_out(67) = 0.0 ! NOT USED
annual_out(68) = 0.0 ! NOT USED
annual_out(69) = 0.0 ! NOT USED
annual_out(70) = marg%integ_Creturn_leafC
annual_out(71) = marg%integ_Nreturn_leafC
annual_out(72) = marg%integ_Creturn_rootC
annual_out(73) = marg%integ_Nreturn_rootC
annual_out(74) = marg%integ_Creturn_Raexcess
annual_out(75) = marg%integ_Nreturn_Raexcess
annual_out(76) = marg%integ_Creturn_leafN
annual_out(77) = marg%integ_Nreturn_leafN
annual_out(78) = marg%integ_Creturn_rootN
annual_out(79) = marg%integ_Nreturn_rootN
annual_out(80) = 0.0 ! NOT USED
annual_out(81) = 0.0 ! NOT USED
annual_out(82) = 0.0 ! NOT USED
annual_out(83) = 0.0 ! NOT USED
annual_out(84) = state%annual_a_leafC
annual_out(85) = state%annual_a_woodC
annual_out(86) = state%annual_a_rootC
annual_out(87) = state%annual_a_NfixC
annual_out(88) = 0.0 ! NOT USED
annual_out(89) = state%debug2
annual_out(90) = state%debug
annual_out(91) = marg%integ_Creturn_leafCN
annual_out(92) = marg%integ_Nreturn_rootCN
annual_out(93) = state%Cuptake_downreg
annual_out(94) = state%annual_labileC_bud
annual_out(95) = state%annual_labileN_bud
annual_out(96) = state%annual_labileC_Ra
annual_out(97) = state%annual_targetleafCN
annual_out(98) = state%annual_targetrootCN
annual_out(99) = state%maxleafC
annual_out(100) = state%maxrootC

open(7,file = io%annual_out, status='OLD',ACCESS = 'APPEND')
write(7,'(100(G12.3))') annual_out(:)
close(7)
end subroutine write_annual_output

!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: 
!
! !INTERFACE:
      subroutine read_namelist()

! !USES:
!
! !ARGUMENTS:
      implicit none
!
! LOCAL VARAIBLES:
   character(len=200) clim_file
   character(len=200) param_file
   character(len=200) day_out
   character(len=200) annual_out
   character(len=200) restart_in
   character(len=200) restart_out
   character(len=200) site_in
   integer :: sim_length
   integer :: clim_length
   integer :: print_year_start
   integer :: print_year_end
   integer :: annual_state_doy
   integer :: cold_start
   namelist /ACONITE_IN/CLIM_FILE,PARAM_FILE,DAY_OUT,ANNUAL_OUT,RESTART_IN,RESTART_OUT,SITE_IN,&
       SIM_LENGTH,CLIM_LENGTH,PRINT_YEAR_START,PRINT_YEAR_END,ANNUAL_STATE_DOY,COLD_START
      
!EOP
!------------------------------------------------------------------------
open(8,file='aconite_namelist', status='OLD')
read(8,nml = ACONITE_IN) 
    io%clim_in = clim_file
    io%param_in = param_file
    io%day_out = day_out
    io%annual_out = annual_out
    io%site_in = site_in
    io%restart_in = restart_in
    io%restart_out = restart_out
    io%sim_length = sim_length
    io%clim_length = clim_length
    io%print_year_start = print_year_start
    io%print_year_end = print_year_end
    io%annual_state_doy = annual_state_doy
    io%cold_start = cold_start
close(8)
if(io%clim_in == '') then
   print *, 'WARNING: NEEDS CLIMATE DATA'
endif
if(io%param_in == '') then
    print *,'NO PARAMETER FILE LISTED: USING DEFAULTS'
endif
if(io%site_in == '') then
    print *,'NO SITE FILE LISTED: USING DEFAULTS'
endif
if(io%restart_in == '') then
    print *,'NO RESTART FILE LISTED: USING DEFAULTS'
endif

if(io%day_out /= '') then
   open(5,file=io%day_out,status='REPLACE')
   close(5)

endif

open(5,file = io%annual_out,status='REPLACE')
close(5)

end subroutine read_namelist

end module aconite_io
