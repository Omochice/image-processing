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
    integer, intent(inout), optional :: maximum_value, low_threshold, high_threshold
    integer :: d, w, h, img_shape(3), i, v_max, v_min,
    integer, allocatable :: lut(:), translated(:, :, :)

    if (.not. present(maximum_value)) then
      maximum_value = 255
    end if
    img_shape = shape(img)
    allocate (translated(img_shape(1), img_shape(2), img_shape(3)))

    do d = 1, img_shape(1)
      if (present(low_threshold)) then
        v_min = low_threshold
      else
        v_min = min(img(d, :, :))
      end if
      if (present(high_threshold)) then
        v_max = high_threshold
      else
        v_max = max(img(d, :, :))
      end if
      allocate (lut(v_min:v_max))
      do i = v_min, v_max
        lut(i) = ((i - v_min)/(v_max - v_min))*maximum_value
      end do
      do w = 1, img_shape(3)
        do h = 1, img_shape(2)
          translated(d, h, w) = lut(img(d, h, w))
        end do
      end do
      deallocate (lut)
    end do
  end function linear_translation

  pure function brightness_translation(img, shift, maximum_value) result(translated)
    implicit none
    integer, intent(in) :: img(:, :, :)
    integer, intent(in) :: shift
    integer, intent(inout), optional :: maximum_value
    integer :: img_shape(3)
    integer, allocatable :: translated(:, :, :)

    if (.not. present(maximum_value)) then
      maximum_value = 255
    end if

    img_shape = shape(img)
    translated = img

    translated = min(maximum_value, &
                     max(0, &
                         translated + shift))
  end function brightness_translation
end module
