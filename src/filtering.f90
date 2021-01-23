module filtering
  implicit none

contains
  recursive subroutine fill_edge(img, n_around)
    implicit none
    integer, intent(inout), dimension(:, :) :: img
    integer, intent(in) :: n_around
    integer, dimension(2) :: img_shape
    integer :: width, height, w, h

    img_shape = shape(img)
    height = img_shape(1)
    width = img_shape(2)

    if (n_around > 0) then
      do w = n_around + 1, width - n_around
        img(n_around, w) = img(n_around + 1, w)
        img(height - n_around + 1, w) = img(height - n_around, w)
      end do

      do h = n_around + 1, height - n_around
        img(h, n_around) = img(h, n_around + 1)
        img(h, width - n_around + 1) = img(h, width - n_around)
      end do

      img(n_around, n_around) = img(n_around + 1, n_around + 1)
      img(n_around, width - n_around + 1) = img(n_around + 1, width - n_around)
      img(height - n_around + 1, n_around) = img(height - n_around, n_around + 1)
      img(height - n_around + 1, width - n_around + 1) = img(height - n_around, width - n_around)

      call fill_edge(img, n_around - 1)
    end if
  end subroutine fill_edge

  subroutine laplacian(img, output, depth)
  !!! laplacian filtering (8 neighborhood)
  !!! input:
  !!!   img(integer, 2D): array. have pix value.
  !!!   output(integer, 2D): array for output. same size as img.
  !!!   depth(integer, aptional): max value of a pix. default to 255.

    implicit none
    integer, intent(in), dimension(:, :) :: img
    integer, intent(inout), dimension(:, :) :: output
    integer, intent(in), optional :: depth
    integer, parameter, dimension(3, 3) :: filter = reshape((/1, 1, 1, &
                                                              1, -8, 1, &
                                                              1, 1, 1/), shape(filter))
    integer, dimension(2) :: img_shape
    integer :: w, h, width, height, d

    img_shape = shape(img)
    height = img_shape(1)
    width = img_shape(2)

    if (present(depth)) then
      d = depth
    else
      d = 255
    end if

    do w = 1, width
      do h = 1, height
        if (w == 1 .or. w == width .or. h == 1 .or. h == height) then
          output(h, w) = img(h, w)
        else
          output(h, w) = max(0, min(d, sum(img(h - 1:h + 1, w - 1:w + 1)*filter)), 0)
        end if
      end do
    end do
    call fill_edge(output, 1)
  end subroutine laplacian

  subroutine gaussian(img, output, depth)
  !!! gaussian filtering (24 neighborhood)
  !!! input:
  !!!   img(integer, 2D): array. have pix value.
  !!!   output(integer, 2D): array for output. same size as img.
  !!!   depth(integer, aptional): max value of a pix. default to 255.

    implicit none
    integer, intent(in), dimension(:, :) :: img
    integer, intent(inout), dimension(:, :) ::  output
    integer, intent(in), optional :: depth

    integer, parameter, dimension(5, 5) :: filter = reshape((/1, 4, 6, 4, 1, &
                                                              4, 16, 24, 16, 4, &
                                                              6, 24, 36, 24, 6, &
                                                              4, 16, 24, 16, 4, &
                                                              1, 4, 6, 4, 1/), shape(filter))
    integer, dimension(2) :: img_shape
    integer :: w, h, width, height, d

    img_shape = shape(img)
    height = img_shape(1)
    width = img_shape(2)
    if (present(depth)) then
      d = depth
    else
      d = 255
    end if

    do w = 1, width
      do h = 1, height
        if (w < 3 .or. w > width - 1 .or. h < 3 .or. h > height - 1) then
          output(h, w) = img(h, w)
        else
          output(h, w) = max(0, min(d, sum(img(h - 2:h + 2, w - 2:w + 2)*filter/256)), 0)
        end if
      end do
    end do
    call fill_edge(output, 2)
  end subroutine gaussian

  subroutine sobel(img, output, depth)
  !!! sobel filter (sqrt version)
  !!! input:
  !!!   img(integer, 2D): input image.
  !!!   output(integer, 2d): output image.
  !!!   depth(integer, optional): max value of a pix. default to 255

    implicit none
    integer, intent(in), dimension(:, :) :: img
    integer, intent(inout), dimension(:, :) ::  output
    integer, intent(in), optional ::depth

    real, allocatable, dimension(:, :) :: workspace
    integer, parameter, dimension(3, 3) :: filter_x = reshape((/-1, 0, 1, &
                                                                -2, 0, 2, &
                                                                -1, 0, 1/), shape(filter_x))
    integer, parameter, dimension(3, 3) :: filter_y = reshape((/-1, -2, -1, &
                                                                0, 0, 0, &
                                                                1, 2, 1/), shape(filter_y))
    real(8), parameter :: PI = 4*atan(1.0_8)
    integer, dimension(2) :: img_shape
    integer :: width, height, w, h, d
    real :: sx, sy

    img_shape = shape(img)
    height = img_shape(1)
    width = img_shape(2)

    allocate (workspace(height, width))

    if (present(depth)) then
      d = depth
    else
      d = 255
    end if

    workspace = 0.0
    do w = 2, width - 1
      do h = 2, height - 1
        sx = sum(img(h - 1:h + 1, w - 1:w + 1)*filter_x)
        sy = sum(img(h - 1:h + 1, w - 1:w + 1)*filter_y)
        output(h, w) = max(0, min(d, int(sqrt(sx**2 + sy**2))))
        ! if (sx /= 0.0) then
        !     output(h, w) = atan2(sy, sx)
        ! else if (sy < 0.0) then
        !     output(h, w) = -1 * (PI /2)
        ! else if (sy == 0.0) then
        !     output(h, w) = 0
        ! else if (sy > 0.0) then
        !     output(h, w) = PI /2
        ! end if
      end do
    end do
    call fill_edge(output, 1)
  end subroutine sobel

end module filtering

