module posterization
  implicit none

contains
  pure function make_histogram(layer, maximum) result(hist)
    implicit none
    integer, intent(in) :: layer(:, :)
    integer, intent(in) :: maximum
    integer :: hist(0:maximum), i
    hist = 0
    do concurrent(i=0:maximum)
      block
        hist(i) = count(layer == i)
      end block
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
    integer, intent(in) :: img(:, :, :)
    integer, intent(in) :: maximum
    integer, allocatable :: rst(:, :, :), hist(:)
    integer :: i, th, best_th, rescale_rate
    real :: ave_class1, ave_class2, nums(0:maximum), var, best_var, &
            n_pix_class1, n_pix_class2

    hist = make_histogram(img(1, :, :), maximum)
    rescale_rate = size(img)**0.5
    ! numsとthresold_to_varを配列の宣言時に代入するのはエラーになる
    nums = [(real(i), i=0, maximum)]
    best_th = 0
    best_var = -1.0

    do concurrent(th=0:maximum)
      block
        n_pix_class1 = sum(hist(:th))/rescale_rate
        n_pix_class2 = sum(hist(th + 1:))/rescale_rate

        if (n_pix_class1 == 0) then
          ave_class1 = 0
        else
          ave_class1 = sum(hist(:th)*nums(:th))/n_pix_class1/rescale_rate
        end if

        if (n_pix_class2 == 0) then
          ave_class2 = 0
        else
          ave_class2 = sum(hist(th + 1:)*nums(th + 1:))/n_pix_class2/rescale_rate
        end if

        var = n_pix_class1*n_pix_class2*((ave_class1 - ave_class2)**2)

        if (var > best_var) then
          best_th = th
          best_var = var
        end if
      end block
    end do

    rst = to_binary(img, best_th)
  end function otsu

  pure function quantize(img, n_split, maximum) result(rst)
    implicit none
    integer, intent(in) :: img(:, :, :)
    integer, intent(in) :: n_split
    integer, intent(in), optional :: maximum
    integer, allocatable :: rst(:, :, :)
    integer :: m

    if (present(maximum)) then
      m = maximum
    else
      m = 255
    end if

    rst = img/(m/n_split + 1)
  end function quantize

  pure function dither(img, maximum) result(rst)
    implicit none
    integer, intent(in) :: img(:, :, :)
    integer, intent(in) :: maximum
    integer, allocatable :: rst(:, :, :)
    integer :: dither_mtx(4, 4)
    integer :: height, width, h, w, h_range(2), w_range(2), dh, dw, img_shape(3)
    integer, allocatable :: targets(:, :)

    dither_mtx = reshape([0, 12, 3, 15, &
                          8, 4, 11, 7, &
                          2, 14, 1, 13, &
                          10, 6, 9, 5]*((maximum + 1)/16), shape(dither_mtx))
    img_shape = shape(img)
    height = img_shape(2)
    width = img_shape(3)
    allocate (rst(1, height, width))

    do w = 0, ceiling(width/4.0)
      do h = 0, ceiling(height/4.0)
        h_range = [h*4 + 1, min(height, (h + 1)*4)]
        w_range = [w*4 + 1, min(width, (w + 1)*4)]

        targets = img(1, h_range(1):h_range(2), w_range(1):w_range(2))
        do concurrent(dh=1:h_range(2) - h_range(1) + 1, dw=1:w_range(2) - w_range(1) + 1)
          block
            if (dither_mtx(dh, dw) > targets(dh, dw)) then
              targets(dh, dw) = 0
            else
              targets(dh, dw) = 1
            end if
          end block
        end do
        rst(1, h_range(1):h_range(2), w_range(1):w_range(2)) = targets
      end do
    end do

  end function dither

end module posterization
