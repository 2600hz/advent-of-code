Idea:

* Create an inverted 1 color depth [PBM](https://www.lifewire.com/pbm-file-2622169) image for each line.
* Get the diff using ImageMagick: `compare image1 image2 -compose src diff.pbm`
* Create a 90 degrees rotated and then mirrored version of the image.
* Get the lowest 0 bit position between both images.
* Convert the bit position to coordinates. That will be the result.
