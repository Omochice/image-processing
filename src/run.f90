program read_file
  use filtering
  use pnm_tools
  implicit none    !未宣言変数の使用禁止
  character(len=100) :: filename
  character(len=2) :: pnm_type
  integer :: img_width, img_height, img_depth
  integer :: i = 0
  integer :: iostatus = 1
  integer, allocatable :: img_array(:, :)
  integer, allocatable :: output_img(:, :)
  integer, allocatable :: workspace(:, :)

  call get_command_argument(1, filename)
! <-- load pnm -->
  open (10, file=filename, status="old", action="read", position="rewind")
  read (10, *, iostat=iostatus) pnm_type
  read (10, *, iostat=iostatus) img_width, img_height
  read (10, *, iostat=iostatus) img_depth
  print *, "img configs..."
  print *, "format: ", pnm_type
  print *, "w, h: ", img_width, img_height

  allocate (img_array(img_width, img_height))
  allocate (output_img(img_width, img_height))
  allocate (workspace(img_width, img_height))

  do i = 1, img_height
    read (10, *, iostat=iostatus) img_array(:, i)
    if (iostatus < 0) exit                         !最終行ならループを抜ける
  end do
  close (10)

  call display_img(transpose(img_array), pnm_type, img_depth)

!   print *, "noise rejection"
!   call ITEN1(img_array, output_img, output_img, img_width, img_height, 3)
!   call display_img(transpose(output_img), pnm_type, img_depth)

!   print *, "laplasian"
! !   call lapf01(img_array, output_img, img_width, img_height, img_width, img_height)
!   call laplacian(img_array, output_img)
!   call display_img(transpose(output_img), pnm_type, img_depth)

!   print *, "gaussian"
  call gaussian(img_array, output_img)
!   call display_img(transpose(output_img), pnm_type, img_depth)

  print *, "sobel"
!   call egsb2(output_img, workspace, img_array, img_width, img_height, 1)
!   call display_img(transpose(workspace), pnm_type, img_depth)

  call sobel(output_img, workspace)
  call display_img(transpose(workspace), pnm_type, img_depth)

!   print *, "edge"
!   call egrb(img_array, output_img, img_width, img_height, 1)
!   call display_img(transpose(output_img), pnm_type, img_depth)

end program
