subroutine save_pnm(img_array, header, width, height, depth, filename)
  !!! Save array as pnm image.
  !!!
  !!! input:
  !!!   img_array (integer, 2D): image array. have pix value.
  !!!   header (character, len=2): pnm format. ex. P2
  !!!   width (integer): image width.
  !!!   height (integer): image height.
  !!!   depth (integer): image depth.
  !!!   filename (character): use as the filename of saved image.

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
    write (18, *) img_array(i, :)
  end do
  close (18)
end subroutine save_pnm

