FC = gfortran

LIPS = ../lips/*.o
SP = ../lips/spider.a
LIBS = save_pnm.o display_img.o laplacian.o

main: $(LIBS)
	$(FC) run.f90 $(LIPS) $(SP) $(LIBS) -o run 

save_pnm.o:
	$(FC) save_pnm.f90 -c -o save_pnm.o 

display_img.o:
	$(FC) display_img.f90 -c -o display_img.o

laplacian.o:
	$(FC) laplacian.f90 -c -o laplacian.o

clean:
	rm *.o run