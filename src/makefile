FC = gfortran

LIPS = ../lips/*.o
SP = ../lips/spider.a
LIBS = save_pnm.o display_img.o laplacian.o
LIBS = pnm_tools.o filtering.o queue.o

main: $(LIBS)
	$(FC) run.f90 $(LIPS) $(SP) $(LIBS) -o run 

pnm_tools.o:
	$(FC) $(@:.o=.f90) -c -o $@

filtering.o: queue.o
	$(FC) $(@:.o=.f90) -c -o $@

queue.o:
	$(FC) $(@:.o=.f08) -c -o $@

clean:
	rm *.o run
