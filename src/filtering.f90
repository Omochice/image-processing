module filtering
  use mod_queue
  implicit none
  real(8), parameter :: PI = 4*atan(1.0_8)

contains
  recursive subroutine fill_edge(img, n_around)
    implicit none
    integer, intent(inout), dimension(:, :, :) :: img
    integer, intent(in) :: n_around
    integer :: img_shape(3)
    integer :: depth, width, height, d, w, h

    img_shape = shape(img)
    depth = img_shape(1)
    height = img_shape(2)
    width = img_shape(3)

    do d = 1, depth
      if (n_around > 0) then
        do w = n_around + 1, width - n_around
          img(d, n_around, w) = img(d, n_around + 1, w)
          img(d, height - n_around + 1, w) = img(d, height - n_around, w)
        end do

        do h = n_around + 1, height - n_around
          img(d, h, n_around) = img(d, h, n_around + 1)
          img(d, h, width - n_around + 1) = img(d, h, width - n_around)
        end do

        img(d, n_around, n_around) = img(d, n_around + 1, n_around + 1)
        img(d, n_around, width - n_around + 1) = img(d, n_around + 1, width - n_around)
        img(d, height - n_around + 1, n_around) = img(d, height - n_around, n_around + 1)
        img(d, height - n_around + 1, width - n_around + 1) = img(d, height - n_around, width - n_around)

        call fill_edge(img, n_around - 1)
      end if
    end do
  end subroutine fill_edge

  function laplacian(img, maximum_value) result(output)
  !!! laplacian filtering (8 neighborhood)
  !!! input:
  !!!   img(integer, 3D): array. have pix value.
  !!!   maximum_value(integer): max value of a pix. default to 255.

    implicit none
    integer, intent(in) :: img(:, :, :)
    integer, intent(in) :: maximum_value
    integer, allocatable :: output(:, :, :)
    integer, parameter, dimension(3, 3) :: filter = reshape((/1, 1, 1, &
                                                              1, -8, 1, &
                                                              1, 1, 1/), shape(filter))
    integer :: img_shape(3)
    integer :: depth, height, width, d, h, w

    img_shape = shape(img)
    depth = img_shape(1)
    height = img_shape(2)
    width = img_shape(3)
    allocate (output(depth, height, width))

    do w = 2, width - 1
      do h = 2, height - 1
        do d = 1, depth
          output(d, h, w) = min(maximum_value, &
                                max(0, &
                                    sum(img(d, h - 1:h + 1, w - 1:w + 1)*filter)))
        end do
      end do
    end do
    call fill_edge(output, 1)
  end function laplacian

  function gaussian(img, maximum_value, n_times) result(output)
  !!! gaussian filtering (24 neighborhood)
  !!! input:
  !!!   img(integer, 3D): array. have pix value.
  !!!   maximum_value(integer, optional): max value of a pix. default to 255.
  !!!   n_times(integer): number of time apply filter.

    implicit none
    integer, intent(in) :: img(:, :, :)
    integer, intent(in) :: maximum_value
    integer, intent(in) :: n_times
    integer, allocatable :: output(:, :, :), tmp(:, :, :)

    integer, parameter, dimension(5, 5) :: filter = reshape((/1, 4, 6, 4, 1, &
                                                              4, 16, 24, 16, 4, &
                                                              6, 24, 36, 24, 6, &
                                                              4, 16, 24, 16, 4, &
                                                              1, 4, 6, 4, 1/), shape(filter))
    integer :: img_shape(3), depth, height, width, d, h, w, n_time

    img_shape = shape(img)
    depth = img_shape(1)
    height = img_shape(2)
    width = img_shape(3)
    allocate (output(depth, height, width))
    allocate (tmp(depth, height, width))

    tmp(:, :, :) = img(:, :, :)

    do n_time = 1, n_times
      do w = 3, width - 2
        do h = 2, height - 2
          do d = 1, depth
            output(d, h, w) = min(maximum_value, &
                                  max(0, &
                                      sum(tmp(d, h - 2:h + 2, w - 2:w + 2)*filter/sum(filter))))
          end do
        end do
      end do
      tmp(:, :, :) = output(:, :, :) ! write back
    end do
    call fill_edge(output, n_around=2)

  end function gaussian

  function sobel(img, maximum_value, is_canny) result(output)
  !!! sobel filter (sqrt version)
  !!! input:
  !!!   img(integer, 3D): input image.
  !!!   maximum_value(integer, optional): max value of a pix. default to 255
  !!!   is_canny(logocal, optional): is used in canny adge detection

    implicit none
    integer, intent(in) :: img(:, :, :)
    integer, intent(in) :: maximum_value
    logical, intent(in), optional :: is_canny
    integer, allocatable :: output(:, :, :)
    real, allocatable :: edge_directions(:, :, :)
    integer, parameter, dimension(3, 3) :: kernel_x = reshape((/-1, -2, -1, &
                                                                0, 0, 0, &
                                                                1, 2, 1/), shape(kernel_x))
    integer, parameter, dimension(3, 3) :: kernel_y = reshape((/-1, 0, 1, &
                                                                -2, 0, 2, &
                                                                -1, 0, 1/), shape(kernel_y))
    integer ::depth, height, width, d, h, w, img_shape(3)
    real, allocatable :: sx(:, :, :), sy(:, :, :)

    img_shape = shape(img)
    depth = img_shape(1)
    height = img_shape(2)
    width = img_shape(3)

    allocate (output(depth, height, width))
    allocate (edge_directions(depth, height, width))
    allocate (sx(depth, height, width))
    allocate (sy(depth, height, width))

    output = 0
    edge_directions = 0.0

    do w = 2, width - 1
      do h = 2, height - 1
        do d = 1, depth
          sx(d, h, w) = sum(img(d, h - 1:h + 1, w - 1:w + 1)*kernel_x)
          sy(d, h, w) = sum(img(d, h - 1:h + 1, w - 1:w + 1)*kernel_y)
        end do
      end do
    end do

    output = min(maximum_value, max(0, int(sqrt(sx**2 + sy**2))))
    print *, shape(output)
    edge_directions = atan2(sy, sx)*180/PI  ! sx == 0 .and. sy == 0の処理がないけど動いている
    deallocate (sx)
    deallocate (sy)

    call fill_edge(output, 1)

    if (present(is_canny) .and. is_canny) then
      call non_maximun_supperssion(output, edge_directions)
      call hysteresis(output, maximum_value)
    end if
  end function sobel

  function canny_edge_detection(img, maximum_value) result(output)
  !!! apply canny edge detection
  !!! the method is
  !!! 1. apply gaussian filtering
  !!! 2. apply sobel filtering
  !!! 3. non-maximum supperssion
  !!! 4. edge tracking by hysteresis
  !!!
  !!! inputs:
  !!!   img(integer, 2D): image array
    implicit none
    integer, intent(in) :: img(:, :, :)
    integer, intent(in) :: maximum_value
    integer, allocatable :: output(:, :, :)

    integer, allocatable::tmp(:, :, :)
    integer :: depth, height, width, d, h, w, img_shape(3)

    img_shape = shape(img)
    depth = img_shape(1)
    height = img_shape(2)
    width = img_shape(3)
    if (depth /= 1) then
      print *, "n_layer must be 1, the n_layer is ", depth
      stop 1
    end if
    allocate (tmp(depth, height, width))
    tmp = gaussian(img, maximum_value, n_times=1)
    output = sobel(tmp, maximum_value, is_canny=.true.)
    deallocate (tmp)
  end function canny_edge_detection

  subroutine non_maximun_supperssion(edge_magnitudes, edge_ways)
    !!! Perform non-maximum_supperssin
    !!!
    !!! inputs:
    !!!   edge_magnitudes(integer, 2D): edge magnitude array
    !!!   edge_ways(real, 2D): edge directions array

    implicit none
    integer, intent(inout) :: edge_magnitudes(:, :, :)
    real, intent(inout):: edge_ways(:, :, :)
    integer, allocatable :: tmp_image(:, :, :)
    integer :: depth, width, height, d, w, h, img_shape(3)
    real :: way, edges(3)

    img_shape = shape(edge_magnitudes)
    depth = img_shape(1)
    height = img_shape(2)
    width = img_shape(3)

    allocate (tmp_image(depth, height, width))
    tmp_image = 0

    ! 量子化
    forall (d=1:depth, h=1:height, w=1:width, (-1*PI/8 <= edge_ways(d, h, w) &
                                               .and. edge_ways(d, h, w) < PI/8)) edge_ways(d, h, w) = 0
    forall (d=1:depth, h=1:height, w=1:width, (7*PI/8 <= edge_ways(d, h, w) &
                                               .and. edge_ways(d, h, w) < PI)) edge_ways(d, h, w) = 0
    forall (d=1:depth, h=1:height, w=1:width, (-1*PI <= edge_ways(d, h, w) &
                                               .and. edge_ways(d, h, w) < -7*PI/8)) edge_ways(d, h, w) = 0
    forall (d=1:depth, h=1:height, w=1:width, (PI/8 <= edge_ways(d, h, w) &
                                               .and. edge_ways(d, h, w) < 3*PI/8)) edge_ways(d, h, w) = 45.0
    forall (d=1:depth, h=1:height, w=1:width, (-7*PI/8 <= edge_ways(d, h, w) &
                                               .and. edge_ways(d, h, w) < -5*PI/8)) edge_ways(d, h, w) = 45.0
    forall (d=1:depth, h=1:height, w=1:width, (3*PI/8 <= edge_ways(d, h, w) &
                                               .and. edge_ways(d, h, w) < 5*PI/8)) edge_ways(d, h, w) = 90.0
    forall (d=1:depth, h=1:height, w=1:width, (-5*PI/8 <= edge_ways(d, h, w) &
                                               .and. edge_ways(d, h, w) < -3*PI/8)) edge_ways(d, h, w) = 90.0
    forall (d=1:depth, h=1:height, w=1:width, (5*PI/8 <= edge_ways(d, h, w) &
                                               .and. edge_ways(d, h, w) < 7*PI/8)) edge_ways(d, h, w) = 135.0
    forall (d=1:depth, h=1:height, w=1:width, (-3*PI/8 <= edge_ways(d, h, w) &
                                               .and. edge_ways(d, h, w) < -1*PI/8)) edge_ways(d, h, w) = 135.0

    do w = 2, width - 1
      do h = 2, height - 1
        do d = 1, depth
          way = edge_ways(d, h, w)
          if (way == 0.0) then
            edges = edge_magnitudes(d, h, w - 1:w + 1)
          else if (way == 45.0) then
            edges = [edge_magnitudes(d, h - 1, w + 1), edge_magnitudes(d, h, w), edge_magnitudes(d, h + 1, w - 1)]
          else if (way == 90.0) then
            edges = edge_magnitudes(d, h - 1:h + 1, w)
          else
            edges = [edge_magnitudes(d, h + 1, w + 1), edge_magnitudes(d, h, w), edge_magnitudes(d, h - 1, w - 1)]
          end if

          ! if ((way >= -1*PI/8 .and. way < PI/8) .or. (way < -7*PI/8 .or. way >= 7*PI/8)) then
          !   edges = edge_magnitudes(d, h, w - 1:w + 1)
          ! else if ((way >= PI/8 .and. way < 3*PI/8) .or. (way >= -7*PI/8 .and. way < -5*PI/8)) then
          !   edges = (/edge_magnitudes(d, h + 1, w - 1), edge_magnitudes(d, h, w), edge_magnitudes(d, h - 1, w + 1)/)
          ! else if ((way >= 3*PI/8 .and. way < 5*PI/8) .or. (way >= -5*PI/8 .and. way < -3*PI/8)) then
          !   edges = edge_magnitudes(d, h - 1:h + 1, w)
          ! else
          !   edges = (/edge_magnitudes(d, h - 1, w - 1), edge_magnitudes(d, h, w), edge_magnitudes(d, h + 1, w + 1)/)
          ! end if
          !
          ! if (edge_magnitudes(d, h, w) /= maxval(edges)) then
          if (edges(2) < edges(1) .or. edges(2) < edges(3)) then
            !   edge_magnitudes(h, w) = 0
            tmp_image(d, h, w) = 0
          else
            tmp_image(d, h, w) = edge_magnitudes(d, h, w)
          end if
        end do
      end do
    end do
    edge_magnitudes(:, :, :) = tmp_image
  end subroutine non_maximun_supperssion

  subroutine hysteresis(img, maximum_value)
    implicit none
    integer, intent(inout) :: img(:, :, :)
    integer, intent(in) :: maximum_value
    type(t_queue) :: queue
    integer :: depth, height, width, d, h, w, low_threshold, high_threshold, img_shape(3), dw, dh

    low_threshold = 75
    high_threshold = 150
    img_shape = shape(img)
    depth = img_shape(1)
    height = img_shape(2)
    width = img_shape(3)

    do w = 1, width
      do h = 1, height
        do d = 1, depth
          if (img(d, h, w) < low_threshold) then
            img(d, h, w) = 0
          else if (img(d, h, w) >= high_threshold) then
            img(d, h, w) = maximum_value
            call enqueue(queue, d)
            call enqueue(queue, h)
            call enqueue(queue, w)
          end if
        end do
      end do
    end do

    do while (size_of(queue) > 0)
      d = dequeue(queue)
      h = dequeue(queue)
      w = dequeue(queue)
      do dw = -1, 1
        do dh = -1, 1
          if (dw == 0 .and. dh == 0) then
            ! center is not target
          else if (h + dh < 1 .or. h + dh > height .or. w + dw < 1 .or. w + dw > width) then
            ! the index is out of img array
          else if (img(d, h + dh, w + dw) == 0) then
            ! the pix is not edge
          else if (img(d, h + dh, w + dw) == maximum_value) then
            ! the pix is already edge
          else
            img(d, h + dh, w + dw) = maximum_value
            call enqueue(queue, d)
            call enqueue(queue, h + dh)
            call enqueue(queue, w + dw)
          end if
        end do
      end do
    end do
    img = img/maximum_value*maximum_value
  end subroutine hysteresis

  function bilateral(img, sigma, maximum_value, n_times) result(output)
  !!! apply bilateral filter
  !!!
  !!! inputs:
  !!!   img(integer, 3D): image array.
  !!!   sigma(real): use in gaussian distribution.
  !!!   n_times: number of time apply filter.

    implicit none
    integer, intent(in):: img(:, :, :)
    real, intent(in) :: sigma
    integer, intent(in) :: maximum_value
    integer, intent(in) :: n_times
    integer, allocatable :: output(:, :, :)
    integer :: depth, height, width, d, h, w, img_shape(3), n_time, i
    real :: gaussian_dist(0:255**2), tmp_array(25), weighted_filter(5, 5), window(5, 5), center
    real, allocatable :: tmp(:, :, :)
    real, parameter, dimension(5, 5) :: filter = reshape((/1, 4, 6, 4, 1, &
                                                           4, 16, 24, 16, 4, &
                                                           6, 24, 36, 24, 6, &
                                                           4, 16, 24, 16, 4, &
                                                           1, 4, 6, 4, 1/), shape(filter))

    img_shape = shape(img)
    depth = img_shape(1)
    height = img_shape(2)
    width = img_shape(3)
    allocate (output(depth, height, width))
    allocate (tmp(depth, height, width))

    gaussian_dist = [(exp(-(real(i)/255)**2/(2*sigma**2)), i=0, 255**2)] ! TODO make this small
    tmp = real(img)
    output(:, :, :) = tmp(:, :, :)

    do n_time = 1, n_times
      do w = 3, width - 2
        do h = 3, height - 2
          do d = 1, depth
            window = tmp(d, h - 2:h + 2, w - 2:w + 2)
            center = tmp(d, h, w)
            tmp_array = reshape(abs(window - center)**2, shape(tmp_array))
            forall (i=1:size(tmp_array)) tmp_array(i) = gaussian_dist(int(tmp_array(i)))
            tmp_array = [(gaussian_dist(int(tmp_array(i))), i=1, size(tmp_array))]
            weighted_filter = filter*reshape(tmp_array, shape(filter))
            output(d, h, w) = min(maximum_value, &
                                  max(0, &
                                      nint(sum(window*weighted_filter)/sum(weighted_filter))))
          end do
        end do
      end do
      tmp(:, :, :) = output(:, :, :)  ! write back
    end do
  end function bilateral
end module filtering
