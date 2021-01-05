program read_file
  implicit none    !未宣言変数の使用禁止
  character(len=100) :: filename
  character(len=2) :: pnm_type
  integer :: img_width, img_height, img_depth
  integer :: i = 0
  integer :: iostatus = 1
  integer, allocatable :: img_array(:, :)
  integer, allocatable :: output_img(:, :)

  call get_command_argument(1, filename)
! <-- load pnm -->
  open (10, file=filename, status="old", action="read", position="rewind")
  read (10, *, iostat=iostatus) pnm_type
  read (10, *, iostat=iostatus) img_width, img_height
  read (10, *, iostat=iostatus) img_depth
  print *, "img configs..."
  print *, "width: ", img_width
  print *, "height: ", img_height
  print *, "format: ", pnm_type

  allocate (img_array(img_height, img_width))
  allocate (output_img(img_height, img_width))
  i = 0
  do
    i = i + 1
    read (10, *, iostat=iostatus) img_array(:, i)
    if (iostatus < 0) exit                         !最終行ならループを抜ける
  end do
  close (10)
  print *, img_array(1, 5)

  call save_pnm(img_array, pnm_type, img_width, img_height, img_depth, "output.pgm")

  call dspimg(img_array, img_width, img_height, img_width/2, img_height/2, 1) !/2しないと1/4で出力される

  print *, "noise rejection"
  call ITEN1(img_array, output_img, output_img, img_width, img_height, 3)
!   print *, output_img
  call dspimg(output_img, img_width, img_height, img_width/2, img_height/2, 1) !/2しないと1/4で出力される

  print *, "laplasian"
  call lapf01(img_array, output_img, img_width, img_height, img_width, img_height)
  call dspimg(output_img, img_width, img_height, img_width/2, img_height/2, 1) !/2しないと1/4で出力される

end program

subroutine save_pnm(img_array, header, width, height, depth, filename)
  implicit none
  integer, dimension(height, width) :: img_array
  character(len=2) :: header
  integer :: width, height, depth, i
  character(len=*) :: filename

  open (18, file=filename, status="replace")
  write (18, "(A)") header
  write (18, *) width, height
  write (18, *) depth
  do i = 1, height
    write (18, *) img_array(:, i)
  end do
  close (18)
end subroutine save_pnm

! function load_pnm(filename) result(img_array)
!   implicit none
!   character(len=*), intent(in) :: filename
!   character(len=2) :: pnm_type
!   integer :: img_width, img_height, img_depth
!   integer :: i = 0
!   integer :: iostatus = 1
!   integer, allocatable :: img_array(:, :)

!   open (10, file=filename, status="old", action="read", position="rewind")
!   read (10, *, iostat=iostatus) pnm_type
!   read (10, *, iostat=iostatus) img_width, img_height
!   read (10, *, iostat=iostatus) img_depth
!   allocate (img_array(img_height, img_width))
!   i = 0
!   do
!     i = i + 1
!     read (10, *, iostat=iostatus) img_array(:, i)
!     if (iostatus < 0) exit                         !最終行ならループを抜ける
!   end do
!   close (10)

! end function load_pnm
