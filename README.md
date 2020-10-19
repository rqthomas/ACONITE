# ACONITE
Analysis of Carbon and Nitrogen Interactions in Terrestrial Ecosystems

Thomas, R.Q., and M. Williams. 2014. A model using marginal efficiency of investment to analyze carbon and nitrogen interactions in terrestrial ecosystems (ACONITE Version 1). Geoscientific Model Development 7: 2015–2037. https://doi.org/10.5194/gmd-7-2015-2014

## How to ACONITE

1) Compile the code. An example of a makefile is provided ('makefile'). example command: 'make -f makefile

2) Modify the aconite_namelist to customize the read.  The following files and inputs are required
	`CLIM_FILE`='climate_Manaus_1999.csv'  :  This is the daily climate inputs 
	`PARAM_FILE`='parameter_tropical.txt'  :  This is the parameter file 
	`DAY_OUT`=''			     :  Enter a file name here if you want daily outputs
	`ANNUAL_OUT`='annual out.txt'	     :  Enter a file name here if you want annual outputs.  Stocks will be output on the day of year specified by the 'ANNUAL_STATE_DOY' input below  
	`RESTART_IN`='restart_tropical.txt'    :   Enter the initial conditions file.  If blank it will use the hard coded defaults which aren't guaranteed to work.  
	`RESTART_OUT`='restart_test.txt'	     :  Enter a file name if you want the model save an initial condition file for future use.
	`SITE_IN`='site_details_Manaus.txt'    : This is the site level information.  
	`SIM_LENGTH`=2000			     : This is the total number of years in the simulation.  
	`CLIM_LENGTH`=365			     : This is the total number of days in the climate input file.  
	`PRINT_YEAR_START`=1		     : This is the year that output is first saved.  
	`PRINT_YEAR_END`=2000		     : This is the year that output is last saved.  
	`ANNUAL_STATE_DOY`=200		     : This is the day of year that stocks are saved in the annual output.  
	`COLD_START`=0			     : This overwrites the initial stock variables so that C:N are consistent with the parameterized C:N ratios. Rarely used.

3) Run the compiled code.  ('./run_aconite')



