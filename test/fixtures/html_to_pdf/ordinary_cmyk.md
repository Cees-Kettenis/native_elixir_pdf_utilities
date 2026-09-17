# Ordinary CMYK fixture

`ordinary_cmyk.jpg` is a 40 by 40 solid-red JPEG generated with libjpeg-turbo.
The encoder used `JCS_CMYK`, four components, quality 95, and
`write_Adobe_marker = FALSE`. Every input pixel was C=0, M=255, Y=255, K=0.
No image or profile was copied from an external source.

The stored samples are ordinary, non-inverted CMYK. The PDF rasterization test
checks the known red output alongside the existing Adobe CMYK and YCCK fixtures.
