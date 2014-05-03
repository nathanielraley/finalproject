all: 
	$(MAKE) -C c c
	$(MAKE) -C fortran fortran

intel:
	$(MAKE) -C c icc
	$(MAKE) -C fortran ifort
c:
	$(MAKE) -C c c

c icc:
	$(MAKE) -C c icc

fortran ifort: 
	$(MAKE) -C fortran ifort

fortran:
	$(MAKE) -C fortran fortran

.PHONY: clean

clean:
	$(MAKE) -C c clean
	$(MAKE) -C fortran clean
