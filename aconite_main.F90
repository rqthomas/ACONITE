      program main
      use aconite_init
      use aconite_io
      use aconite_ecosystem
      use aconite_functions
      implicit none

      integer :: CN_Mode
      integer :: rstep
      integer :: mstep
      integer :: model_year
      integer :: in_spinup
      integer :: year_count

      call initvars
      call read_namelist

      !read climate data
      call read_climate(clim,io%clim_length) 
      !read parameters, site, and disturbance
      if (io%param_in /= '') then
      call read_parameters()
      endif

      if (io%site_in /= '') then
      call read_site_data()
      endif

      !read restart
      if (io%restart_in /= '') then
          call read_restart()
      endif
     
      model_year = 0

      do mstep = 1,(io%sim_length*365)
           state%rstep = state%rstep + 1

           if(state%rstep == (io%clim_length+1))then
               state%rstep = 1
           endif
           year_count = year_count + 1
          
           call ecosystem_dynamics(state%rstep,year_count)

           if (io%day_out /= '' .AND. mstep >= (io%print_year_start*365) .AND. mstep < (io%print_year_end*365)) then
              
              call write_daily_output(model_year,mstep)
           endif
           
           if(year_count == 365 .AND. mstep >= (io%print_year_start*365) .AND. mstep < (io%print_year_end*365)) then   
              
              call write_annual_output(state%rstep,model_year,mstep)
              if(io%restart_out /= '') then
              call write_restart()
              endif
           endif  
           
           call zero_fluxes()         
           if(year_count == 365) then
              print *, 'model year',(mstep/365.0)
              year_count = 0 
           endif

      enddo

      end program main
