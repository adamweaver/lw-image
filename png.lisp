(in-package :png)

(defstruct chunk
  type length data crc)

(defclass png ()
  ((width
    :initarg :width :reader width)
   (height
    :initarg :height :reader height)
   (depth
    :initarg :depth :reader depth)
   (ctype
    :initarg :ctype :reader ctype)
   (compression-method
    :initarg :compression-method :reader compression-method)
   (filter-method
    :initarg :filter-method :reader filter-method)
   (interlace-method
    :initarg :interlace-method :reader interlace-method)
   (pixel-dimensions
    :initarg :pixel-dimensions :reader pixel-dimensons)
   (palette
    :initarg :palette :reader palette)
   (transparency
    :initarg :transparency :reader transparency)
   (background
    :initarg :background :reader background)
   (raw-data
    :initarg :raw-data :accessor raw-data)
   (data
    :initarg :data :accessor data)
   (compressed
    :initarg :compressed :accessor compressed-data)
   (byte-arrays
    :initform nil :accessor byte-arrays)))

(defmethod colour-type ((png png))
  (if (find (ctype png) '(:rgb :palette :rgba) :test #'eq)
      "/DeviceRGB"
      "/DeviceGray"))

(defmethod alphap ((png png))
  (or (eq (ctype png) :greya)
      (eq (ctype png) :rgba)
      (transparency png)))

(defmethod bpp ((png png))
  (if (eq (ctype png) :palette)
      8
      (depth png)))

(defmethod filter-type ((png png))
  "/FlateDecode")

(defun load-png (vector)
  (labels ((make-colour-type (int)
             (and (<= 0 int 6) (aref #(:grey nil :rgb :palette :greya nil :rgba) int)))
           
           (png-p ()
             (null (mismatch #(137 80 78 71 13 10 26 10) vector :end2 8)))
           
           (read-unsigned-long (i)
             (make-unsigned-long (aref vector i) (aref vector (1+ i)) (aref vector (+ i 2)) (aref vector (+ i 3))))
           
           (make-unsigned-long (b1 b2 b3 b4)
             (dpb b1 (byte 8 24) (dpb b2 (byte 8 16) (dpb b3 (byte 8 8) b4)))))
    
    (unless (png-p) (error "Not a valid PNG file"))
    (let (ihdr idats plte bkgd phys trns)
      (loop with i = 8
            for len = (read-unsigned-long i)
            for type = (map 'string #'code-char (nsubseq vector (+ i 4) (+ i 8)))
            for vec = (nsubseq vector (+ 8 i) (+ 8 i len))
            for crc = (read-unsigned-long (+ 8 i len))
            for chunk = (make-chunk :type type :data vec :length len :crc crc)
            do (incf i (+ 12 len))
               (cond ((string= type "IHDR") (setf ihdr chunk))
                     ((string= type "IDAT") (push chunk idats))
                     ((string= type "PLTE") (setf plte chunk))
                     ((string= type "bKGD") (setf bkgd chunk))
                     ((string= type "pHYs") (setf phys chunk))
                     ((string= type "tRNS") (setf trns chunk))
                     ((string= type "IEND") (loop-finish))))
      
      (let* ((ihdr (chunk-data ihdr))
             (png (make-instance 'png
                                 :width (make-unsigned-long (aref ihdr 0) (aref ihdr 1) (aref ihdr 2) (aref ihdr 3))
                                 :height (make-unsigned-long (aref ihdr 4) (aref ihdr 5) (aref ihdr 6) (aref ihdr 7))
                                 :depth (aref ihdr 8)
                                 :ctype (make-colour-type (aref ihdr 9))
                                 :compression-method (aref ihdr 10)
                                 :filter-method (aref ihdr 11)
                                 :interlace-method (aref ihdr 12)
                                 :palette plte
                                 :transparency trns
                                 :pixel-dimensions phys
                                 :background bkgd
                                 :data (make-array (reduce #'+ idats :key #'chunk-length :initial-value 0) :element-type '(unsigned-byte 8)))))
        
        (loop with pos = 0
              for idat in (nreverse idats)
              for len = (chunk-length idat)
              do (replace (data png) (chunk-data idat) :start1 pos)
                 (incf pos len))
        
        (when (zerop (compression-method png))
          (setf (data png) (crypto:decompress (data png))))
        
        (setf (byte-arrays png) (case (ctype png)
                                  (:palette (make-palette-byte-arrays png))
                                  (:grey (make-byte-arrays png))
                                  (:rgb (make-byte-arrays png))
                                  (:greya (make-alpha-byte-arrays png 1))
                                  (:rgba (make-alpha-byte-arrays png 3))))
        png))))

(defun make-palette-byte-arrays (png)
  "Inflate our palette into an RGB of 8-bits each component array"
  (let ((data (deinterlace png))
        (width (width png))
        (colour (make-array (* (width png) (height png) 3) :element-type '(unsigned-byte 8)))
        (palette (chunk-data (palette png)))
        (transparency (when (transparency png) (chunk-data (transparency png))))
        (alpha (when (transparency png) (make-array (* (width png) (height png)) :element-type '(unsigned-byte 8) :initial-element 255))))
    
    (loop with bpp = (depth png)
          with stride = (ceiling (* width bpp) 8)
          for y below (height png)
          do (loop for x below width
                   for byte = (floor (* x bpp) 8)
                   for nybble = (mod (* x bpp) 8)
                   for idx = (ldb (byte bpp (- 8 bpp nybble)) (aref data (+ byte (* y stride))))
                   do (replace colour palette :start1 (+ (* x 3) (* y 3 width)) :start2 (* 3 idx) :end2 (* 3 (1+ idx)))
                      (when alpha
                        (setf (aref alpha (+ x (* y width))) (if (< idx (length transparency)) (aref transparency idx) 255)))))
    
    (list (crypto:compress colour) (when alpha (crypto:compress alpha)))))

(defun make-byte-arrays (png)
  (let ((data (deinterlace png)) (trans (when (transparency png) (chunk-data (transparency png)))))
    (list (crypto:compress data)
          (when trans (loop for i below (length trans) by 2 collect (logior (ash (aref trans i) 8) (aref trans (1+ i))))))))

(defun make-alpha-byte-arrays (png components)
  (let ((data (deinterlace png))
        (width (width png))
        (colour (make-array (* (width png) (height png) components (floor (depth png) 8)) :element-type '(unsigned-byte 8)))
        (alpha (make-array (* (width png) (height png) (floor (depth png) 8)) :element-type '(unsigned-byte 8))))
    
    (loop with c-len = (/ (* (depth png) components) 8)
          with a-len = (/ (depth png) 8)
          with len = (+ c-len a-len)
          for y below (height png)
          do (loop for x below width
                   for o-start = (+ (* x c-len) (* y width c-len))
                   for i-start = (+ (* x len) (* y width len))
                   do (replace colour data :start1 o-start :start2 i-start :end2 (+ i-start c-len))
                      (replace alpha data :start1 (+ (* x a-len) (* y width a-len)) :start2 (+ i-start c-len) :end2 (+ i-start len))))
    
    (list (crypto:compress colour) (crypto:compress alpha))))

(defun deinterlace (png)
  (if (zerop (interlace-method png))
      (defilter (data png) (height png) (bytes-per-pixel png))
      (error "Interlaced PNG not yet supported")))

(defun defilter (in height bpp)
  "Turn our array of HEIGHT x (1+ WIDTH) into a HEIGHT x WIDTH array, applying the filter function to each scanline"
  (labels ((defilter (filter x left top top-left)
             (ldb (byte 8 0)
                  (case filter
                    (0 x)
                    (1 (+ x left))
                    (2 (+ x top))
                    (3 (+ x (ldb (byte 8 0) (floor (+ left top) 2))))
                    (4 (+ x (predictor left top top-left))))))
           
           (predictor (a b c)
             (let* ((p (- (+ a b) c))
                    (pa (abs (- p a)))
                    (pb (abs (- p b)))
                    (pc (abs (- p c))))
               (cond ((and (<= pa pb) (<= pa pc)) a)
                     ((<= pb pc) b)
                     (t c)))))
    
    (loop with stride = (/ (length in) height)
          with width = (1- stride)
          with out = (make-array (* width height) :element-type '(unsigned-byte 8))
          for y below height
          for filter = (aref in (* y stride))
          do (loop for x below width
                   do (setf (aref out (+ x (* y width)))
                            (defilter filter
                                (aref in (+ 1 x (* y stride)))
                              (if (< x bpp) 0 (aref out (+ (- x bpp) (* y width))))
                              (if (zerop y) 0 (aref out (+ x (* (1- y) width))))
                              (if (or (< x bpp) (zerop y)) 0 (aref out (+ (- x bpp) (* (1- y) width)))))))
          finally (return out))))

(defun pixel-components (png)
  (case (ctype png)
    ((:grey :palette) 1)
    (:greya 2)
    (:rgb 3)
    (:rgba 4)))

(defun bytes-per-pixel (png)
  (ceiling (* (depth png) (pixel-components png)) 8))

