fortran: main.o subroutines.o
	gfortran -o project-f90 main.o subroutines.o

main.o: main.f90 subroutines.o
	gfortran -c main.f90

subroutines.o: subroutines.f90
	gfortran -c subroutines.f90

ifort: imain.o isubroutines.o
	ifort -o project-f90 main.o subroutines.o

imain.o: main.f90 isubroutines.o
	ifort -c main.f90

isubroutines.o: subroutines.f90
	ifort -c subroutines.f90

.PHONY: clean

clean:
	rm -f *.o core
	rm -f *.mod core
	rm project-f90
