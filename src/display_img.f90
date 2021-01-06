subroutine display_img(img, header, width, height, depth)
  !!! Display array img.
  !!! save array as pnm image named "output.pgm" then show via imagemagick.
  !!! input:
  !!!   img (integer, 2D): have pix value.
  !!!   header (character, len=2): pnm format. ex. P2
  !!!   width (integer): image width.
  !!!   height (integer): image height.
  !!!   depth (integer): image depth. ex. 255

  implicit none
  integer, dimension(height, width) :: img
  character(len=2) :: header
  integer :: width, height, depth

  call save_pnm(img, header, width, height, depth, "output.pgm")
  call system("display output.pgm")
end subroutine display_img

