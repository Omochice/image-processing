module posterization
  implicit none

contains
  pure function make_histogram(layer, maximum) result(hist)
    implicit none
    integer, intent(in) :: layer(:, :)
    integer, intent(in) :: maximum
    integer :: hist(0:maximum), layer_shape(2), h, w
    hist = 0
    layer_shape = shape(layer)
    do w = 1, layer_shape(2)
      do h = 1, layer_shape(1)
        hist(layer(h, w)) = hist(layer(h, w)) + 1
      end do
    end do
  end function make_histogram

  pure function to_binary(img, threshold) result(rst)
    implicit none
    integer, intent(in) :: img(:, :, :)
    integer, intent(in) :: threshold
    integer, allocatable :: rst(:, :, :)

    rst = img
    where (img(1, :, :) >= threshold)
      rst(1, :, :) = 1
    else where
      rst(1, :, :) = 0
    end where
  end function to_binary

  pure function otsu(img, maximum) result(rst)
    implicit none
    integer, intent(in) :: img(1, :, :)
    integer, intent(in) :: maximum
    integer, allocatable :: rst(1, :, :), hist(:)
    integer :: i, th, nums(0:maximum)
    real ::  n_pix_class1, n_pix_class2, ave_class1, ave_class2, varianse, thresold_to_variance(2)

    hist = make_histogram(img, maximum)
    ! numsとthresold_to_varianceを配列の宣言時に代入するのはエラーになる
    nums = [(i, i=0, maximum)]
    thresold_to_variance = [0, -10]

    do th = 0, maximum
      n_pix_class1 = real(sum(hist(:th)))
      n_pix_class2 = real(sum(hist(th + 1:)))

      if (n_pix_class1 == 0) then
        ave_class1 = 0
      else
        ave_class1 = sum(hist(:th)*nums(:th))/n_pix_class1
      end if

      if (n_pix_class2 == 0) then
        ave_class2 = 0
      else
        ave_class2 = sum(hist(th + 1:)*nums(th + 1:))/n_pix_class2
      end if

      varianse = n_pix_class1*n_pix_class2*(ave_class1 - ave_class2)**2

      if (varianse > thresold_to_variance(2)) then
        thresold_to_variance = [real(th), varianse]
      end if
    end do

    rst = to_binary(img, int(thresold_to_variance(1)))
  end function otsu

end module posterization
