(defpackage :jpg
  (:use :cl)
  (:export "COLOURP" "JPG" "ALPHAP" "FILTER-TYPE" "BYTE-ARRAYS" "LOAD-JPG" "WIDTH" "HEIGHT" "BPP" "COLOUR-TYPE"))

(defpackage :png
  (:use :cl)
  (:export "PNG" "COLOUR-TYPE" "ALPHAP" "BPP" "FILTER-TYPE" "LOAD-PNG" "MAKE-BYTE-ARRAYS" "PIXEL-COMPONENTS" "BYTES-PER-PIXEL"))

(defpackage :svg
  (:use :cl)
  (:export "LOAD-SVG" "SVG" "RENDER-SVG-TO-PDF-DRAWING-COMMANDS"))
