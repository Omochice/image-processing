module pnm_tools
  implicit none

contains
  subroutine save_pnm(img_array, header, depth, filename)
  !!! Save array as pnm image.
  !!!
  !!! input:
  !!!   img_array (integer, 2D): image array. have pix value.
  !!!   header (character, len=2): pnm format. ex. P2
  !!!   depth (integer): image depth.
  !!!   filename (character): use as the filename of saved image.

    implicit none
    integer, dimension(:, :), intent(in) :: img_array
    character(len=2), intent(in) :: header
    character(len=*), intent(in) :: filename
    integer, intent(in) :: depth
    integer :: width, height, i
    integer, dimension(2) :: img_shape

    img_shape = shape(img_array)
    height = img_shape(1)
    width = img_shape(2)

    open (18, file=filename, status="replace")
    write (18, "(A)") header
    write (18, *) width, height
    write (18, *) depth
    do i = 1, height
      write (18, *) img_array(i, :)
    end do
    close (18)
  end subroutine save_pnm

  subroutine display_img(img, header, depth)
  !!! Display array img.
  !!! save array as pnm image named "output.pgm" then show via imagemagick.
  !!! input:
  !!!   img (integer, 2D): have pix value.
  !!!   header (character, len=2): pnm format. ex. P2
  !!!   depth (integer): image depth. ex. 255

    implicit none
    integer, dimension(:, :), intent(in) :: img
    character(len=2), intent(in) :: header
    integer, intent(in) :: depth

    call save_pnm(img, header, depth, "output.pgm")
    call system("display output.pgm")
  end subroutine display_img

end module pnm_tools
