program myscript
    implicit none
    integer :: ingbuf(1024, 1024, 9)
    integer :: ihist(65535)
    real :: hsto(65536), hstn(65535) !なんで65535と65536なんだ
    real :: rmask(11, 11)
    character(:), allocatable :: charbuf

    integer :: isx, isy, ix0, iy0, k, isw, npl, imax, imin
    integer :: ngro, ngrn, iflg, nlev iwk1, iwk2, iwk3
    real :: div

    isx = 1024
    isy = 1024
    ix0 = 480
    ixy = 400

    charbuf = "sp_imp99.dat"
    print *, "Read test data file:", charbuf

    call inputf2(imgbuf(1, 1, 2) isx, isy, &charbuf, chabuf, ihist, ix0, iy0)
    print *, "File size ", ix0, iy0

    iflg = 1
    npl = 2
    call imgsta1(imgbuf(1, 1, npl), isx, isy, ix0, iy0, imax, imin)
    print *, "imax, imin=", imax, imin

    call dspimg(imgbuf(1, 1, npl), isx, isy, ix0, iy0, iflg)
end program myscript
