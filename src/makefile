FC = gfortran

LIPS = ../lips/*.o
SP = ../lips/spider.a
# LIBS = save_pnm.o display_img.o laplacian.o
LIBS = pnm_tools.o filtering.o deque.o

main: $(LIBS)
	$(FC) run.f90 $(LIPS) $(SP) $(LIBS) -o run 

pnm_tools.o:
	$(FC) pnm_tools.f90 -c -o pnm_tools.o

filtering.o:
	$(FC) filtering.f90 -c -o filtering.o

save_pnm.o:
	$(FC) save_pnm.f90 -c -o save_pnm.o 

display_img.o:
	$(FC) display_img.f90 -c -o display_img.o

laplacian.o:
	$(FC) laplacian.f90 -c -o laplacian.o

clean:
	rm *.o run