module filtering
    use mod_queue
    implicit none
    real(8), parameter :: PI = 4*atan(1.0_8)

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

        do w = 3, width - 2
            do h = 3, height - 2
                output(h, w) = max(0, min(d, sum(img(h - 2:h + 2, w - 2:w + 2)*filter/256)), 0)
            end do
        end do
        call fill_edge(output, 2)
    end subroutine gaussian

    subroutine sobel(img, output, depth, is_canny)
  !!! sobel filter (sqrt version)
  !!! input:
  !!!   img(integer, 2D): input image.
  !!!   output(integer, 2d): output image.
  !!!   depth(integer, optional): max value of a pix. default to 255
  !!!   is_canny(logocal, optional): is used in canny adge detection

        implicit none
        integer, intent(in), dimension(:, :) :: img
        integer, intent(inout), dimension(:, :) ::  output
        integer, intent(in), optional ::depth
        logical, intent(in), optional :: is_canny

        real, allocatable, dimension(:, :) :: edge_directions
        integer, parameter, dimension(3, 3) :: kernel_x = reshape((/-1, -2, -1, &
                                                                    0, 0, 0, &
                                                                    1, 2, 1/), shape(kernel_x))
        integer, parameter, dimension(3, 3) :: kernel_y = reshape((/-1, 0, 1, &
                                                                    -2, 0, 2, &
                                                                    -1, 0, 1/), shape(kernel_y))
        integer :: width, height, w, h, d, img_shape(2)
        real :: sx, sy

        img_shape = shape(img)
        height = img_shape(1)
        width = img_shape(2)

        allocate (edge_directions(height, width))

        if (present(depth)) then
            d = depth
        else
            d = 255
        end if

        output = 0
        edge_directions = 0.0
        do w = 2, width - 1
            do h = 2, height - 1
                sx = sum(img(h - 1:h + 1, w - 1:w + 1)*kernel_x)
                sy = sum(img(h - 1:h + 1, w - 1:w + 1)*kernel_y)
                output(h, w) = max(0, min(d, int(sqrt(sx**2 + sy**2))))
                if (sx == 0.0 .and. sy == 0.0) then
                    edge_directions = 0
                else
                    edge_directions(h, w) = atan2(sy, sx)
                end if
            end do
        end do
        call fill_edge(output, 1)

        if (present(is_canny) .and. is_canny) then
            call non_maximun_supperssion(output, edge_directions)
            call hysteresis(output)
        end if

        deallocate (edge_directions)
    end subroutine sobel

    subroutine canny_edge_detection(img, output)
  !!! apply canny edge detection
  !!! the method is
  !!! 1. apply gaussian filtering
  !!! 2. apply sobel filtering
  !!! 3. non-maximum supperssion
  !!! 4. edge tracking by hysteresis
  !!!
  !!! inputs:
  !!!   img(integer, 2D): image array
  !!!   output(integer, 2D): output array
        implicit none
        integer, intent(in), dimension(:, :) :: img
        integer, intent(inout), dimension(:, :):: output

        integer, allocatable, dimension(:, :) ::tmp
        integer :: img_shape(2)

        img_shape = shape(img)
        allocate (tmp(img_shape(1), img_shape(2)))
        call gaussian(img, tmp)
        call sobel(tmp, output, is_canny=.true.)
        deallocate (tmp)
    end subroutine canny_edge_detection

    subroutine non_maximun_supperssion(edge_magnitudes, edge_ways)
    !!! Perform non-maximum_supperssin
    !!!
    !!! inputs:
    !!!   edge_magnitudes(integer, 2D): edge magnitude array
    !!!   edge_ways(real, 2D): edge directions array

        implicit none
        integer, intent(inout), dimension(:, :) :: edge_magnitudes
        real, intent(in), dimension(:, :) :: edge_ways
        integer, allocatable, dimension(:, :) :: tmp_image
        integer :: width, height, w, h, img_shape(2)
        real :: way, edges(3)

        img_shape = shape(edge_magnitudes)
        height = img_shape(1)
        width = img_shape(2)

        allocate (tmp_image(height, width))
        tmp_image = 0

        do w = 2, width - 1
            do h = 2, height - 1
                way = edge_ways(h, w)

                if ((way >= -1*PI/8 .and. way < PI/8) .or. (way < -7*PI/8 .or. way >= 7*PI/8)) then
                    edges = edge_magnitudes(h - 1:h + 1, w)
                else if ((way >= PI/8 .and. way < 3*PI/8) .or. (way >= -7*PI/8 .and. way < -5*PI/8)) then
                    edges = (/edge_magnitudes(h - 1, w - 1), edge_magnitudes(h, w), edge_magnitudes(h + 1, w + 1)/)
                else if ((way >= 3*PI/8 .and. way < 5*PI/8) .or. (way >= -5*PI/8 .and. way < -3*PI/8)) then
                    edges = edge_magnitudes(h, w - 1:w + 1)
                else
                    edges = (/edge_magnitudes(h + 1, w - 1), edge_magnitudes(h, w), edge_magnitudes(h - 1, w + 1)/)
                end if

                if (edge_magnitudes(h, w) /= maxval(edges)) then
                    !   edge_magnitudes(h, w) = 0
                    tmp_image(h, w) = 0
                else
                    tmp_image(h, w) = edge_magnitudes(h, w)
                end if
            end do
        end do
        edge_magnitudes(:, :) = tmp_image
    end subroutine non_maximun_supperssion

    subroutine hysteresis(img, depth)
        implicit none
        integer, intent(inout), dimension(:, :) :: img
        integer, intent(in), optional :: depth
        type(t_queue) :: queue
        integer :: w, h, width, height, low_threshold, high_threshold, img_shape(2), d
        integer ::dw, dh

        low_threshold = 50
        high_threshold = 100
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
                if (img(h, w) < low_threshold) then
                    img(h, w) = 0
                else if (img(h, w) >= high_threshold) then
                    img(h, w) = d
                    call enqueue(queue, h)
                    call enqueue(queue, w)
                end if
            end do
        end do

        ! do while (queue%num > 0)
        do while (size_of(queue) > 0)
            h = dequeue(queue)
            w = dequeue(queue)
            do dw = -1, 1
                do dh = -1, 1
                    if (dw == 0 .and. dh == 0) then
                        ! center is not target
                    else if (h + dh < 1 .or. h + dh > height .or. w + dw < 1 .or. w + dw > width) then
                        ! the index is out of img array
                    else if (img(h + dh, w + dw) == 0) then
                        ! the pix is not edge
                    else if (img(h + dh, w + dw) == d) then
                        ! the pix is already edge
                    else
                        img(h + dh, w + dw) = d
                        call enqueue(queue, h + dh)
                        call enqueue(queue, w + dw)
                    end if
                end do
            end do
        end do
    end subroutine hysteresis

    subroutine bilateral(img, output, sigma)
  !!! apply bilateral filter
  !!!
  !!! inputs:
  !!!   img(integer, 2D): image array.
  !!!   output(integer, 2D): ouput array.
  !!!   sigma(real): use in gaussian distribution.

        implicit none
        integer, intent(in), dimension(:, :) :: img
        integer, intent(out), dimension(:, :) :: output
        real, intent(in) :: sigma
        integer :: i, w, h, width, height, center, img_shape(2), window(5, 5)
        real :: gaussian_dist(0:255), tmp_array(25), weighted_filter(5, 5)
        real, parameter, dimension(5, 5) :: filter = reshape((/1, 4, 6, 4, 1, &
                                                               4, 16, 24, 16, 4, &
                                                               6, 24, 36, 24, 6, &
                                                               4, 16, 24, 16, 4, &
                                                               1, 4, 6, 4, 1/), shape(filter))

        img_shape = shape(img)
        height = img_shape(1)
        width = img_shape(2)

        gaussian_dist = (/(exp(-(real(i)/255)**2/(2*sigma**2)), i=0, 255)/)

        do w = 3, width - 2
            do h = 3, height - 2
                window = img(h - 2:h + 2, w - 2:w + 2)
                center = img(h, w)
                tmp_array = reshape(abs(window - center), shape(tmp_array))
                tmp_array = (/(gaussian_dist(int(tmp_array(i))), i=1, size(tmp_array))/)
                weighted_filter = filter*reshape(tmp_array, shape(filter))
                output(h, w) = max(0, min(255, &
                                          nint(sum(window*weighted_filter)/sum(weighted_filter)) &
                                          ))
            end do
        end do
        call fill_edge(output, 2)
    end subroutine bilateral
end module filtering

