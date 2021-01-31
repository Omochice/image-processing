module pnm_tools
  implicit none

contains
  function load_pnm(filename) result(img_array)
    !!! load_pnm
    !!!
    !!!   input:
    !!!     filename(character): input filename.
    !!!   output:
    !!!     img_array(integer, 3D): image array.
    !!!                             even if the image is grayscale/monochrome, the array is 3D.
    implicit none
    character(*), intent(in) :: filename
    integer, allocatable, dimension(:, :, :) :: img_array
    character(len=2) :: pnm_type
    integer :: i, j
    integer :: width, height, max_value, n_layer
    integer :: iostatus = 1
    integer, allocatable :: row(:), div_row(:), mod_row(:)

    open (10, file=filename, status="old", action="read", position="rewind")
    read (10, *, iostat=iostatus) pnm_type
    read (10, *, iostat=iostatus) width, height

    if (pnm_type == "P1") then
      n_layer = 1
    else if (pnm_type == "P2") then
      n_layer = 1
      read (10, *, iostat=iostatus) max_value
    else if (pnm_type == "P3") then
      n_layer = 3
      read (10, *, iostat=iostatus) max_value
    else
      print *, "pnm header must be in (P1, P2, P3), the header is ", pnm_type
      stop 1 ! how raise error?
    end if

    allocate (img_array(n_layer, height, width))

    allocate (row(width*n_layer))
    div_row = ([(i, i=0, (width*n_layer) - 1)]/n_layer) + 1 ! 1, 1, 1, 2, 2, 2, ...
    mod_row = (mod([(i, i=0, (width*n_layer) - 1)], n_layer)) + 1 ! 1, 2, 3, 1, 2, 3, ...
    do i = 1, height
      read (10, *, iostat=iostatus) row
      do j = 1, size(row)
        img_array(mod_row(j), i, div_row(j)) = row(j)
      end do
      if (iostatus < 0) then
        exit
      end if
    end do
    close (10)
    deallocate (row, div_row, mod_row)
  end function load_pnm

  subroutine save_pnm(img_array, maximum_value, filename)
    !!! Save array as pnm image.
    !!\!
    !!! input:
    !!!   img_array (integer, 2D): image array. have pix value.
    !!!   maximum_value (integer): image maximum_value.
    !!!   filename (character): use as the filename of saved image.

    implicit none
    integer, dimension(:, :, :), intent(in) :: img_array
    character(len=*), intent(in) :: filename
    integer, intent(in) :: maximum_value
    character(len=2) :: header
    integer :: width, height, n_layer
    integer :: i, j
    integer, dimension(3) :: img_shape
    integer, allocatable :: row(:)

    img_shape = shape(img_array)
    n_layer = img_shape(1)
    height = img_shape(2)
    width = img_shape(3)

    if (n_layer == 3) then
      header = "P3"
    else if (n_layer == 1 .and. maximum_value /= 2) then
      header = "P2"
    else if (n_layer == 1 .and. maximum_value == 2) then
      header = "P1"
    else
      print *, "the number of leyer must be 1 or 3. the number is ", n_layer
      stop 1
    end if

    open (18, file=filename, status="replace")
    write (18, "(A)") header
    write (18, *) width, height
    write (18, *) maximum_value
    do i = 1, height
      if (n_layer == 1) then
        row = img_array(1, i, :)
      else
        row = [(img_array(1, i, j), img_array(2, i, j), img_array(3, i, j), j=1, width)]
      end if
      write (18, *) row
      ! write (18, *) img_array(j, i, :)
    end do
    close (18)
  end subroutine save_pnm

  subroutine display_img(img, maximum_value)
    !!! Display array img.
    !!! save array as pnm image named "output.pgm" then show via imagemagick.
    !!! input:
    !!!   img (integer, 2D): have pix value.
    !!!   maximum_value (integer): image maximum_value. ex. 255

    implicit none
    integer, dimension(:, :, :), intent(in) :: img
    integer, intent(in) :: maximum_value

    call save_pnm(img, maximum_value, "output.pnm")
    call system("display output.pnm")
  end subroutine display_img

end module pnm_tools
