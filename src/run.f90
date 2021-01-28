program read_file
  use filtering
  use pnm_tools
  implicit none    !未宣言変数の使用禁止
  character(len=100) :: filename
  character(len=2) :: pnm_type
  integer :: img_width, img_height, img_depth
  integer :: i = 0
  integer :: iostatus = 1
  integer :: w, h
  integer, allocatable :: img_array(:, :)
  integer, allocatable :: output_img(:, :)
  integer, allocatable :: tmp(:, :)

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
  allocate (tmp(img_width, img_height))

  do i = 1, img_height
    read (10, *, iostat=iostatus) img_array(:, i)
    if (iostatus < 0) exit                         !最終行ならループを抜ける
  end do
  close (10)

  call display_img(transpose(img_array), pnm_type, img_depth)

  call canny_edge_detection(img_array, tmp)
  call bilateral(img_array, output_img, 0.3, 7)

  do w = 1, img_width
    do h = 1, img_height
      if (tmp(h, w) == 255) then
        output_img(h, w) = 0
      end if
    end do
  end do

  ! call display_img(transpose(output_img), pnm_type, img_depth)
  call save_pnm(transpose(output), pnm_type, img_depth, filename(:len_trim(filename) - 4)//"birateral_with_edge.pgm")

end program
