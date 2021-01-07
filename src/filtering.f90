module filtering
  implicit none

contains
  subroutine laplacian(img, output, depth)
  !!! laplacian filtering (8 neighborhood)
  !!! input:
  !!!   img(integer, 2D): array. have pix value.
  !!!   output(integer, 2D): array for output. same size as img.
  !!!   depth(integer): image depth.

    implicit none
    integer, intent(in), dimension(:, :) :: img
    integer, intent(inout), dimension(:, :) :: output
    integer, intent(in) :: depth
    integer, parameter, dimension(3, 3) :: filter = reshape((/1, 1, 1, 1, -8, 1, 1, 1, 1/), shape(filter))
    integer, dimension(2) :: img_shape
    integer :: w, h, width, height

    img_shape = shape(img)
    height = img_shape(1)
    width = img_shape(2)

    do h = 1, height
      do w = 1, width
        if (w == 1 .or. w == width .or. h == 1 .or. h == height) then
          output(h, w) = img(h, w)
        else
          output(h, w) = max(0, min(depth, sum(img(h - 1:h + 1, w - 1:w + 1)*filter)), 0)
        end if
      end do
    end do
  end subroutine laplacian

end module filtering

