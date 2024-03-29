module filtering
  use mod_queue
  implicit none
  real(8), parameter :: PI = 4*atan(1.0_8)

contains
  pure function filtering_(img, filter, maximum_value, fill) result(filtered)
    !! apply filter
    implicit none

    !! Arguments
    integer, intent(in) :: img(:, :, :)
      !! image array. has pixel values.
    integer, intent(in) :: maximum_value
      !! max pixel value.
    integer, intent(in) :: filter(:, :)
      !! filter array.
    logical, intent(in), optional :: fill
      !! Whether does fill edge.
    integer, allocatable :: filtered(:, :, :)
      !! filtered image. same size as the argument img.

    integer :: depth, height, width, d, h, w, img_shape(3), filter_shape(2), dx, dy, sum_of_filter
    integer, allocatable :: window(:, :)

    img_shape = shape(img)
    depth = img_shape(1)
    height = img_shape(2)
    width = img_shape(3)
    allocate (filtered(depth, height, width))
    filtered(:, :, :) = img(:, :, :)

    filter_shape = shape(filter)
    allocate (window(filter_shape(1), filter_shape(2)))

    dx = filter_shape(2)/2
    dy = filter_shape(1)/2
    if (sum(filter) == 0) then
      sum_of_filter = 1
    else
      sum_of_filter = sum(filter)
    end if

    do w = dx + 1, width - dx
      do h = dy + 1, height - dy
        do d = 1, depth
          window = img(d, h - dy:h + dy, w - dx:w + dx)
          filtered(d, h, w) = min(maximum_value, &
                                  max(0, &
                                      sum(window*filter)/sum_of_filter))
        end do
      end do
    end do
    if (present(fill) .and. fill) then
      call fill_edge(filtered, n_around=dx)
    end if
  end function filtering_

  pure recursive subroutine fill_edge(img, n_around)
    !! fill edges with the same color as closest pix.
    implicit none

    integer, intent(inout), dimension(:, :, :) :: img
      !! image array. has pixel values.
    integer, intent(in) :: n_around
      !! The pix of fill edge.
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

  pure function laplacian(img, maximum_value) result(output)
    !! laplacian filtering (8 neighborhood)
    implicit none

    integer, intent(in) :: img(:, :, :)
      !! Image array. has pixel values.
    integer, intent(in) :: maximum_value
      !! max value of a pix. default to 255.
    integer, allocatable :: output(:, :, :)
      !! filtered image array.
    integer, parameter, dimension(3, 3) :: filter = reshape((/1, 1, 1, &
                                                              1, -8, 1, &
                                                              1, 1, 1/), shape(filter))
    output = filtering_(img, filter, maximum_value, fill=.true.)
  end function laplacian

  pure function gaussian(img, maximum_value, n_times) result(output)
    !! gaussian filtering (24 neighborhood)
    implicit none

    integer, intent(in) :: img(:, :, :)
      !! Image array has pixel values.
    integer, intent(in) :: maximum_value
      !! Max value of a pix. default to 255.
    integer, intent(in) :: n_times
      !! Number of time apply filter.
    integer, allocatable :: output(:, :, :)
      !! Filtered image array.

    integer, parameter, dimension(5, 5) :: filter = reshape((/1, 4, 6, 4, 1, &
                                                              4, 16, 24, 16, 4, &
                                                              6, 24, 36, 24, 6, &
                                                              4, 16, 24, 16, 4, &
                                                              1, 4, 6, 4, 1/), shape(filter))
    integer :: img_shape(3), n_time

    img_shape = shape(img)
    allocate (output(img_shape(1), img_shape(2), img_shape(3)))

    output(:, :, :) = img(:, :, :)

    do n_time = 1, n_times
      output = filtering_(output, filter, maximum_value, fill=.false.)
    end do
    call fill_edge(output, n_around=2)

  end function gaussian

  function sobel(img, maximum_value, is_canny) result(output)
    !! sobel filter (sqrt version)
    implicit none

    integer, intent(in) :: img(:, :, :)
      !! Image array has pixel values.
    integer, intent(in) :: maximum_value
      !! Max value of a pix. default to 255
    logical, intent(in), optional :: is_canny
      !! Whether is used in canny adge detection
    integer, allocatable :: output(:, :, :)
      !! Filtered image array
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
    !! apply canny edge detection <br/>
    !! the method is: <br/>
    !! 1. apply gaussian filtering <br/>
    !! 2. apply sobel filtering <br/>
    !! 3. non-maximum supperssion <br/>
    !! 4. edge tracking by hysteresis <br/>
    implicit none

    integer, intent(in) :: img(:, :, :)
      !! Image array has pixel values.
    integer, intent(in) :: maximum_value
      !! The max value of pixel.
    integer, allocatable :: output(:, :, :)
      !! Edge array

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

  pure subroutine non_maximun_supperssion(edge_magnitudes, edge_ways)
    !! Perform non-maximum_supperssin
    implicit none

    integer, intent(inout) :: edge_magnitudes(:, :, :)
      !! Edge magnitude array.
    real, intent(inout):: edge_ways(:, :, :)
      !! Edge directions array
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
    !! Edge tracking by hysteresis
    implicit none

    integer, intent(inout) :: img(:, :, :)
      !! Image array has pixel value.
    integer, intent(in) :: maximum_value
      !! The max value of pixel.
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

  pure function bilateral(img, sigma, maximum_value, n_times) result(output)
    !! apply bilateral filter
    implicit none

    integer, intent(in):: img(:, :, :)
      !! Image array has pixel values.
    real, intent(in) :: sigma
      !! use in gaussian distribution.
    integer, intent(in) :: maximum_value
      !! The max value of pixel.
    integer, intent(in) :: n_times
      !! Number of time to apply filter.
    integer, allocatable :: output(:, :, :)
      !! The image applied bilateral filter.
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

  pure function emboss(img, maximum_value) result(filtered_img)
    !! apply emboss filter
    implicit none

    integer, intent(in) :: img(:, :, :)
      !! Image array has pixel value.
    integer, intent(in) :: maximum_value
      !! The max value of pixel.

    integer, allocatable :: filtered_img(:, :, :)
      !! The image applied emboss filter.
    integer, parameter :: filter(3, 3) = reshape([-2, -1, 0, &
                                                  -1, 1, 1, &
                                                  0, 1, 2], shape(filter))

    filtered_img = filtering_(img, filter, maximum_value, fill=.false.)
  end function emboss
end module filtering
