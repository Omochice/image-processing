FC = gfortran

# LIPS = ../lips/*.o
# SP = ../lips/spider.a
LIBS = pnm_tools.o filtering.o queue.o posterization.o gradation_processing.o

preview: preview.f90 $(LIBS)
	$(FC) $+ -o $@

pnm_tools.o: pnm_tools.f90
	$(FC) $+ -c

filtering.o: filtering.f90 queue.o 
	$(FC) $+ -c 

queue.o: queue.f08
	$(FC) $+ -c 

queue.f08: get-queue.sh
	bash $+

posterization.o: posterization.f08
	$(FC) $+ -c

gradation_processing.o: gradation_processing.f90
	$(FC) $+ -c

all: clean preview

clean:
	rm *.o *.mod preview
