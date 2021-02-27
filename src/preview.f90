program preview
  use pnm_tools
  use filtering
  use gradation_processing
  use posterization
  implicit none
  character(len=100) :: filename
  integer, allocatable :: img(:, :, :)
  integer :: maximum_value = 255

  call get_command_argument(1, filename)

  img = load_pnm(filename)

  call display_img(img, maximum_value)
  ! filtering.f90
  call display_img(laplacian(img, maximum_value), maximum_value)
  call display_img(gaussian(img, maximum_value, 3), maximum_value)
  call display_img(sobel(img, maximum_value, is_canny=.false.), maximum_value)
  ! call display_img(canny_edge_detection(img, maximum_value), maximum_value)
  call display_img(bilateral(img, 0.3, maximum_value, 3), maximum_value)
  call display_img(emboss(img, maximum_value), maximum_value)
  ! gradation_processing.f90
  call display_img(linear_translation(img, maximum_value), maximum_value)
  call display_img(brightness_translation(img, 45, maximum_value), maximum_value)
  call display_img(contrast_translation(img, maximum_value, 0.3), maximum_value)
  call display_img(gamma_correction(img, maximum_value, 3.0), maximum_value)
  call display_img(histogram_equalization(img, maximum_value), maximum_value)
  ! posterization.f08
  call display_img(to_binary(img, 45), 1)
  call display_img(otsu(img, maximum_value), 1)
  call display_img(adaptive_threshold(img, maximum_value, 15), 1)
  call display_img(quantize(img, 8, maximum_value), 8)
  call display_img(dither(img, maximum_value), 1)
  call display_img(error_diffusion(img, maximum_value), 1)
end program preview
