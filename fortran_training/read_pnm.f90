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
  allocate (img_array(img_height, img_width))
  allocate (output_img(img_height, img_width))
  i = 0
  do
    i = i + 1
    read (10, *, iostat=iostatus) img_array(:, i)
    if (iostatus < 0) exit                         !最終行ならループを抜ける
  end do
  close (10)


  print *, "noise rejection"
  call ITEN1(img_array(1,1), output_img(1,1), output_img(1,1), img_width, img_height, 3)
  print *, output_img
  call dspimg(img_array(1,1), img_width, img_height, img_width/2, img_height/2, 1) !/2しないと1/4で出力される
  call dspimg(output_img(1,1), img_width, img_height, img_width/2, img_height/2, 1) !/2しないと1/4で出力される

end program

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
