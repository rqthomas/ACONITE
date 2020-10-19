
# This is an commentary line in a makefile
# # Start of the makefile
run_aconite: aconite_main.o aconite_ecosystem.o aconite_functions.o aconite_io.o aconite_init.o aconite_type.o 
	gfortran -o run_aconite aconite_type.o aconite_init.o aconite_io.o aconite_functions.o aconite_ecosystem.o aconite_main.o  
aconite_type.mod: aconite_type.o aconite_type.F90
	gfortran -c aconite_type.F90
aconite_type.o: aconite_type.F90
	gfortran -c aconite_type.F90
aconite_init.mod: aconite_init.o aconite_init.F90
	gfortran -c aconite_init.F90
aconite_init.o: aconite_type.mod aconite_init.F90
	gfortran -c aconite_init.F90
aconite_io.mod: aconite_io.o aconite_io.F90
	gfortran -c aconite_io.F90
aconite_io.o: aconite_type.mod aconite_init.mod aconite_io.F90
	gfortran -c aconite_io.F90
aconite_functions.mod: aconite_functions.o aconite_functions.F90
	gfortran -c aconite_functions.F90
aconite_functions.o: aconite_type.mod aconite_init.mod aconite_functions.F90
	gfortran -c aconite_functions.F90
aconite_ecosystem.mod: aconite_ecosystem.o aconite_ecosystem.F90
	gfortran -c aconite_ecosystem.F90
aconite_ecosystem.o: aconite_type.mod aconite_init.mod aconite_functions.mod aconite_ecosystem.F90
	gfortran -c aconite_ecosystem.F90
aconite_main.o: aconite_ecosystem.mod aconite_init.mod aconite_io.mod aconite_main.F90
	gfortran -c aconite_main.F90
clean:
	rm -rf *.o *.mod *.MOD
# End of the makefile
