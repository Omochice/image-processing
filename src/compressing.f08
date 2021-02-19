module compressing
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
    integer, intent(in) :: img(:, :)
    integer, intent(in) :: threshold
    integer, allocatable :: rst(:, :)

    rst = img
    where (img(:, :) >= threshold)
      rst(:, :) = 1
    else where
      rst(:, :) = 0
    end where
  end function to_binary

  pure function otsu(img, maximum) result(rst)
    implicit none
    integer, intent(in) :: img(:, :)
    integer, intent(in) :: maximum
    integer, allocatable :: rst(:, :), hist(:)
    integer :: i, th, n1, n2, nums(0:maximum)
    real :: ave1, ave2, sigma
    real :: s_max(2)

    hist = make_histogram(img, maximum)
    ! numsとs_maxを配列の宣言時に代入するのはエラーになる
    nums = [(i, i=0, maximum)]
    s_max = [0, -10]

    do th = 0, maximum
      n1 = sum(hist(:th))
      n2 = sum(hist(th + 1:))

      if (n1 == 0) then
        ave1 = 0
      else
        ave1 = sum(hist(:th)*nums(:th))/real(n1)
      end if

      if (n2 == 0) then
        ave2 = 0
      else
        ave2 = sum(hist(th + 1:)*nums(th + 1:))/real(n2)
      end if

      sigma = n1*n2*(ave1 - ave2)**2

      if (sigma > s_max(2)) then
        s_max = [real(th), sigma]
      end if
    end do

    rst = to_binary(img, int(s_max(1)))
  end function otsu

end module compressing
