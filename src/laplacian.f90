subroutine laplacian(img, output, width, height)
  !!! laplacian filtering (8 neighborhood)
  !!! input:
  !!!   img(integer, 2D): array. have pix value.
  !!!   output(integer, 2D): array for output.
  !!!   width(integer): image width.
  !!!   height(integer): image height.

  implicit none
  integer, dimension(width, height), intent(in) :: img
  integer, dimension(width, height), intent(out) :: output
  integer :: width, height
  integer :: filter(3, 3) = reshape((/1, 1, 1, 1, -8, 1, 1, 1, 1/), shape(filter))
  integer :: w, h

  do w = 1, width 
    do h = 1, height 
      if (w == 1 .or. w == width .or. h == 1 .or. h == height) then
        output(h, w) = img(h, w)
      else
        output(h, w) = max(0, min(255, sum(img(h - 1:h + 1, w - 1:w + 1)*filter)), 0)
      end if
    end do
  end do
end subroutine laplacian