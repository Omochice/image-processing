# Image processing

Simple image processing by fortran.

## How to use

```console
$ git clone (this repository)
$ cd image image-processing/src
$ make
$ make generate-pnm 
$ ./preview ../img/Lenna.pnm
```

## Modules 

### pnm_tools

* `load_pnm`
* `save_pnm`
* `display_img`

### filtering

* `laplacian`
* `gaussian`
* `sobel`
* `canny_edge_detection`
* `bilateral`
* `emboss`

### gradation_processing

* `linear_translation`
* `brightness_translation`
* `contrast_translation`
* `gamma_correction`
* `histogram_equalization`

### posterization

* `to_binary`
* `otsu`
* `adaptive_threshold`
* `quantize`
* `dither`
* `error_diffusion`
