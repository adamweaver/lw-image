(in-package :jpg)
 
(defclass jpg ()
  ((data :initarg :data :accessor data)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (bpp :initarg :bpp :accessor bpp)
   (colour-type :initarg :colour-type :accessor colour-type)))
 
(defmethod colourp ((jpg jpg))
  (string/= (colour-type jpg) "/DeviceGray"))
 
(defmethod alphap ((jpg jpg))
  nil)
 
(defmethod filter-type ((jpg jpg))
  "/DCTDecode")
 
(defmethod byte-arrays ((jpg jpg))
  (list (data jpg) nil))
 
(defun load-jpg (vector)
  (let ((i 0) (o 0) (out (make-array (length vector) :element-type '(unsigned-byte 8) :initial-element 0)) width height colour)
    (labels ((copy-marker (length)
               (replace out vector :start1 o :end1 (+ o length) :start2 i)
               (incf o length)
               (incf i length))
 
             (read-marker ()
               ;; JPEGs can contain a lot of fields that choke a PDF reader
               ;; So we strip out any markers that aren't the basic requirements.
               ;; Implied in https://pdfa.org/norm-refs/5116.DCT_Filter.pdf page 9
               (case (aref vector (1+ i))
                 ;;SOI
                 (#xd8 (copy-marker 2))
                 ;;SOF0
                 (#xc0 (read-sof0))
                 ;; DHT RST0 RST1 RST2 RST3 RST4 RST5 RST6 RST7 DQT  DRI  APP2
                 ((#xc4 #xd0 #xd1 #xd2 #xd3 #xd4 #xd5 #xd6 #xd7 #xdb #xdd #xe2) (copy-marker (+ 2 (read-int16 (+ i 2)))))
                 ;; SOS
                 (#xda (copy-marker (- (length vector) i)))
                 ;; Ignore other markers
                 (t (incf i (+ 2 (read-int16 (+ i 2)))))))
 
             (colourtype (i)
               (case i
                 (3 "/DeviceRGB")
                 (4 "/DeviceCMYK")
                 (t "/DeviceGray")))
 
             (read-int16 (idx)
               (logior (ash (aref vector idx) 8) (aref vector (1+ idx))))
 
             (read-sof0 ()
               (setf width (read-int16 (+ i 7))
                     height (read-int16 (+ i 5))
                     colour (colourtype (aref vector (+ i 9))))
               (copy-marker (+ 2 (read-int16 (+ i 2))))))
 
      (loop while (and (< i (length vector)) (= (aref vector i) #xff)) do (read-marker))
      (make-instance 'jpg :width width :height height :bpp 8 :colour-type colour :data (adjust-array out o)))))
