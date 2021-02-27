module gradation_processing
  implicit none
contains
  pure function make_histogram(layer, maximum_number) result(hist)
    implicit none
    integer, intent(in) :: layer(:, :)
    integer, intent(in) :: maximum_number
    integer :: hist(0:maximum_number), layer_shape(2), h, w
    hist = 0

    layer_shape = shape(layer)
    do w = 1, layer_shape(2)
      do h = 1, layer_shape(1)
        hist(layer(h, w)) = hist(layer(h, w)) + 1
      end do
    end do
  end function make_histogram

  pure function linear_translation(img, maximum_value, low_threshold, high_threshold) result(translated)
    implicit none
    integer, intent(in) :: img(:, :, :)
    integer, intent(in), optional :: maximum_value, low_threshold, high_threshold
    integer :: d, w, h, img_shape(3), i, v_max, v_min, m
    integer, allocatable :: lut(:), translated(:, :, :)

    if (present(maximum_value)) then
      m = 255
    else
      m = maximum_value
    end if
    img_shape = shape(img)
    allocate (translated(img_shape(1), img_shape(2), img_shape(3)))
    allocate (lut(0:maximum_value))

    do d = 1, img_shape(1)
      if (present(low_threshold)) then
        v_min = low_threshold
      else
        v_min = minval(img(d, :, :))
      end if
      if (present(high_threshold)) then
        v_max = high_threshold
      else
        v_max = maxval(img(d, :, :))
      end if
      do i = 0, maximum_value
        lut(i) = min(maximum_value, &
                     max(0, &
                         nint(((real(i) - v_min)/(v_max - v_min))*maximum_value)))
      end do
      do w = 1, img_shape(3)
        do h = 1, img_shape(2)
          translated(d, h, w) = lut(img(d, h, w))
        end do
      end do
    end do
  end function linear_translation

  pure function brightness_translation(img, shift, maximum_value) result(translated)
    implicit none
    integer, intent(in) :: img(:, :, :)
    integer, intent(in) :: shift
    integer, intent(in), optional :: maximum_value
    integer :: img_shape(3), m
    integer, allocatable :: translated(:, :, :)

    if (present(maximum_value)) then
      m = maximum_value
    else
      m = 255
    end if

    img_shape = shape(img)
    translated = img

    translated = min(m, &
                     max(0, &
                         translated + shift))
  end function brightness_translation

  pure function contrast_translation(img, maximum_value, K) result(translated)
    implicit none
    integer, intent(in) :: img(:, :, :)
    integer, intent(in) :: maximum_value
    real, intent(in) :: K
    integer ::img_shape(3), d, h, w, i
    integer, allocatable :: translated(:, :, :), lut(:)

    allocate (lut(0:maximum_value))

    do i = 0, maximum_value
      lut(i) = min(maximum_value, &
                   max(0, &
                       nint(K*(i - maximum_value/2) + maximum_value/2)))
    end do

    img_shape = shape(img)
    allocate (translated(img_shape(1), img_shape(2), img_shape(3)))
    do w = 1, img_shape(3)
      do h = 1, img_shape(2)
        do d = 1, img_shape(1)
          translated(d, h, w) = lut(img(d, h, w))
        end do
      end do
    end do
  end function contrast_translation

  pure function gamma_correction(img, maximum_value, g) result(translated)
    implicit none
    integer, intent(in) :: img(:, :, :)
    integer, intent(in) :: maximum_value
    real, intent(in) :: g
    integer, allocatable :: translated(:, :, :)
    integer :: lut(0:maximum_value), img_shape(3), d, h, w, i

    img_shape = shape(img)
    allocate (translated(img_shape(1), img_shape(2), img_shape(3)))

    do i = 0, maximum_value
      lut(i) = nint(maximum_value*(real(i)/maximum_value)**(1/g))
    end do

    do w = 1, img_shape(3)
      do h = 1, img_shape(2)
        do d = 1, img_shape(1)
          translated(d, h, w) = lut(img(d, h, w))
        end do
      end do
    end do
  end function gamma_correction

  pure function histogram_equalization(img, maximum_value) result(translated)
    implicit none
    integer, intent(in) :: img(:, :, :)
    integer, intent(in) :: maximum_value
    integer, allocatable :: translated(:, :, :), hist(:), layer(:, :)
    integer :: d, h, w, img_shape(3), i

    img_shape = shape(img)
    allocate (translated(img_shape(1), img_shape(2), img_shape(3)))

    do d = 1, img_shape(1)
      layer = img(d, :, :)
      hist = make_histogram(layer, maximum_value)
      do i = 1, size(hist)
        hist(i) = hist(i - 1) + hist(i)
      end do
      hist = nint(hist*(maxval(layer)/real(img_shape(2)*img_shape(3))))
      do w = 1, img_shape(3)
        do h = 1, img_shape(2)
          translated(d, h, w) = hist(img(d, h, w))
        end do
      end do
    end do
  end function histogram_equalization
end module
