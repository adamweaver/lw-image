(in-package :svg)

(defclass svg ()
  ((data :initarg :data :reader data)
   (width :initarg :width :reader width)
   (height :initarg :height :reader height)))

(defparameter *cmds* nil)
(defparameter *coords* (vector 0 0))

(defun load-svg (src)
  (labels ((attr! (x name)
             (let ((value (xml:has-attr x #'conv:float! name 0)))
               (multiple-value-bind (real frac) (truncate value)
                 (if (zerop frac)
                     real
                     value))))
           
           (attr$ (x name)
             (xml:has-attr x #'string! name))
           
           (line-join (v)
             (or (position v #("butt" "round" "square") :test #'string-equal) 0))
           
           (line-cap (v)
             (or (position v #("miter" "round" "bevel") :test #'string-equal) 0))
           
           (collect (x)
             (let ((tag (xml:node-tag x)))
               (cond ((string-equal tag "svg") (make-svg x))
                     ((string-equal tag "path") (make-path x))
                     (t nil))))

           (lex-number (string start)
             (let* ((e (position-if-not #'digit-char-p string :start (1+ start)))
                    (whole (or (ignore-errors (parse-integer string :start start :end e)) 0)))
               (if (or (null e) (char/= (char string e) #\.))
                   (values whole e)
                   (let ((f (position-if-not #'digit-char-p string :start (1+ e))))
                     (values (+ whole (/ (parse-integer string :start (1+ e) :end f) (expt 10 (- (or f (length string)) e 1)))) f)))))
           
           (lex (string)
             (loop for i below (length string)
                   for c = (char string i)
                   nconc (cond ((or (digit-char-p c) (char= c #\-))
                                (multiple-value-bind (num o) (lex-number string i)
                                  (setf i (if o (1- o) (length string))) (list num)))
                               ((char= c #\.)
                                (let ((e (position-if-not #'digit-char-p string :start (1+ i))))
                                  (prog1 (list (/ (parse-integer string :start (1+ i) :end e) (expt 10 (- (or e (length string)) i 1))))
                                    (setf i (if e (1- e) (length string))))))
                               ((or (char= c #\Tab) (char= c #\Space) (char= c #\Return) (char= c #\Linefeed)) nil)
                               (t (list c)))))
           
           (clamp (number)
             (min 1.0D0 (max -1.0D0 number)))
           
           (convert-arc-to-bezier (prev-pt radius-x radius-y rotation large-arc-flag sweep-flag next-pt-x next-pt-y)
             ;; https://raw.githubusercontent.com/josh-frank/path-normalizer/refs/heads/master/arcToCubicBeziers.js
             (unless (or radius-x radius-y)
               (let* ((tau (* 2 pi)) (sinphi (sin (/ (* rotation tau) 360))) (cosphi (cos (/ (* rotation tau) 360)))
                      (pxp (+ (* cosphi 1/2 (- (car prev-pt) next-pt-x)) (* sinphi 1/2 (- (cdr prev-pt) next-pt-y))))
                      (pyp (+ (* (- sinphi) 1/2 (- (car prev-pt) next-pt-x)) (* cosphi 1/2 (- (cdr prev-pt) next-pt-y)))))
                 (unless (and (zerop pxp) (zerop pyp))
                   (setf radius-x (abs radius-x) radius-y (abs radius-y))
                   (let ((lam (+ (/ (* pxp pxp) (* radius-x radius-x)) (/ (* pyp pyp) (* radius-y radius-y)))))
                     (when (> lam 1)
                       (setf radius-x (* radius-x (sqrt lam)) radius-y (* radius-y (sqrt lam)))))
                   (destructuring-bind (centre-x centre-y a1 a2)
                       (get-arc-centre prev-pt next-pt-x next-pt-y radius-x radius-y large-arc-flag sweep-flag sinphi cosphi pxp pyp)
                     (let ((ratio (/ (abs a2) (/ tau 4))))
                       (when (< (abs (- 1.0 ratio)) 0.000001) (setf ratio 1.0))
                       (let ((segments (max (ceiling ratio) 1)))
                         (setf a2 (/ a2 segments))
                         (let ((curves (loop for i below segments collect (approximate-unit-arc a1 a2) do (incf a1 a2))))
                           (loop for c in curves
                                 nconc (map-to-ellipse (car c) radius-x radius-y cosphi sinphi centre-x centre-y)
                                 nconc (map-to-ellipse (cadr c) radius-x radius-y cosphi sinphi centre-x centre-y)
                                 nconc (map-to-ellipse (caddr c) radius-x radius-y cosphi sinphi centre-x centre-y))))))))))
           
           (map-to-ellipse (coords rx ry cosphi sinphi centre-x centre-y)
             (setf (car coords) (* (car coords) rx)
                   (cdr coords) (* (cdr coords) ry))
             (list (+ (- (* cosphi (car coords)) (* sinphi (cdr coords))) centre-x)
                   (+ (+ (* sinphi (car coords)) (* cosphi (cdr coords))) centre-y)))
           
           (approximate-unit-arc (a1 a2)
             (let ((a (cond ((= a2  1.5707963267948966)  0.551915024494)
                            ((= a2 -1.5707963267948966) -0.551915024494)
                            (t (* 4/3 (tan (/ a2 4)))))))
               (let ((x1 (cos a1)) (y1 (sin a1)) (x2 (cos (+ a1 a2))) (y2 (+ a1 a2)))
                 (list (cons (- x1 (* a y1)) (+ y1 (* a x1)))
                       (cons (+ x2 (* a y2)) (- y2 (* a x2)))
                       (cons x2 y2)))))
           
           (vector-angle (ux uy vx vy)
             (let ((sign (if (minusp (- (* ux uy) (* uy vx))) -1 1)))
               (* sign (acos (clamp (+ (* ux vx) (* uy vy)))))))
           
           (get-arc-centre (prev-pt cx cy rx ry large-arc-flag sweep-flag sinphi cosphi pxp pyp)
             (let ((rxsq (* rx rx)) (rysq (* ry ry)) (pxpsq (* pxp pxp)) (pypsq (* pyp pyp)) (tau (* 2 pi)))
               (let ((radicant (max (- (* rxsq rysq) (* rxsq pypsq) (* rysq pxpsq)) 0)))
                 (setf radicant (/ radicant (+ (* rxsq pypsq) (* rysq pxpsq)))
                       radicant (* (sqrt radicant) (if (eq large-arc-flag sweep-flag) -1 1)))
                 (let* ((centre-xp (/ (* radicant rx) (* ry pyp)))
                        (centre-yp (/ (* radicant (- ry)) (* rx pyp)))
                        (centre-x (+ (- (* cosphi centre-xp) (* sinphi centre-yp)) (/ (+ (car prev-pt) cx) 2)))
                        (centre-y (+ (* sinphi centre-xp) (* cosphi centre-yp) (/ (+ (cdr prev-pt) cy) 2)))
                        (vx1 (/ (- pxp centre-xp) rx))
                        (vy1 (/ (- pyp centre-yp) ry))
                        (vx2 (/ (- (- pxp) centre-xp) rx))
                        (vy2 (/ (- (- pyp) centre-yp) ry))
                        (a1 (vector-angle 1 0 vx1 vy1))
                        (a2 (vector-angle vx1 vy1 vx2 vy2)))
                   (when (and (null sweep-flag) (plusp a2)) (decf a2 tau))
                   (when (and sweep-flag (minusp a2)) (incf a2 tau))
                   (list centre-x centre-y a1 a2)))))
           
           (next ()
             (pop *cmds*))
           
           (translate ()
             ;; SVGs use both relative (lowercase path commands) and absolute coordinates (uppercase path commands)
             ;; But PDFs only know about absolute coordinates
             (loop for c = (next)
                   for uc = (and c (upper-case-p c))
                   for off-x = (if uc 0 (aref *coords* 0))
                   for off-y = (if uc 0 (aref *coords* 1))
                   while c
                   nconc (case (char-downcase c)
                           ;; Absolute / Relative move
                           (#\m (list :move
                                      (setf (aref *coords* 0) (+ off-x (next)))
                                      (setf (aref *coords* 1) (+ off-y (next)))))
                           ;; Absolute / Relative line
                           (#\l (loop while (numberp (car *cmds*))
                                      collect :line
                                      collect (setf (aref *coords* 0) (+ off-x (next)))
                                      collect (setf (aref *coords* 1) (+ off-y (next)))))
                           ;; Horizontal line
                           (#\h (loop while (numberp (car *cmds*))
                                      collect :line
                                      collect (setf (aref *coords* 0) (+ off-x (next)))
                                      collect (aref *coords* 1)))
                           ;; Vertical line
                           (#\v (loop while (numberp (car *cmds*))
                                      collect :line
                                      collect (aref *coords* 0)
                                      collect (setf (aref *coords* 1) (+ off-y (next)))))
                           ;; Cubic bezier
                           (#\c (loop while (numberp (car *cmds*))
                                      collect :curve3
                                      collect (+ (next) off-x)
                                      collect (+ (next) off-y)
                                      collect (+ (next) off-x)
                                      collect (+ (next) off-y)
                                      collect (setf (aref *coords* 0) (+ off-x (next)))
                                      collect (setf (aref *coords* 1) (+ off-y (next)))))
                           ;; Smooth curve bezier
                           (#\s (loop while (numberp (car *cmds*))
                                      collect :curve2
                                      collect (+ (next) off-x)
                                      collect (+ (next) off-y)
                                      collect (setf (aref *coords* 0) (+ off-x (next)))
                                      collect (setf (aref *coords* 1) (+ off-y (next)))))
                           ;; Arc
                           ((#\A #\a)
                            (loop while (numberp (car *cmds*))
                                  nconc (let ((x (aref *coords* 0)) (y (aref *coords* 1)) (rx (next)) (ry (next)) (angle (next))
                                              (large-arc-flag (plusp (next))) (sweep-flag (plusp (next))) (end-x (next)) (end-y (next)))
                                          (lw:when-let (curves (convert-arc-to-bezier (cons (setf (aref *coords* 0) (+ end-x off-x))
                                                                                            (setf (aref *coords* 1) (+ end-y off-y)))
                                                                                      rx ry angle large-arc-flag sweep-flag x y))
                                            (cons :curve3 curves)))))
                           
                           ((#\Z #\z) (list :stroke)))))
           
           (make-path (x)
             (let ((*cmds* (lex (attr$ x "d"))) (*coords* (vector 0 0)))
               (nconc (list :push) (translate) (list :stroke :fill :pop))))
           
           (make-svg (x)
             (make-instance 'svg :width (attr! x "width")
                                 :height (attr! x "height")
                                 :data (apply #'nconc
                                              (list* :line-join (line-join (attr$ x "stroke-linejoin"))
                                                     :line-cap (line-cap (attr$ x "stroke-linecap"))
                                                     :width (attr! x "stroke-width")
                                                     :viewbox (lex (attr$ x "viewBox")))
                                              (map 'list #'collect (xml:node-content x))))))
    
    (collect (xml:decode src))))


(defun render-svg-to-pdf-drawing-commands (colour svg x y w h)
  (let ((src (data svg)) (sx (/ w (width svg))) (sy (/ h (height svg))))
    (labels ((next ()
               (pop src))
             
             (num ()
               (next))
             
             (% (val)
               (cond ((or (stringp val) (integerp val)) val)
                     ((integerp val) val)
                     (t (multiple-value-bind (whole frac) (truncate val)
                          (if (zerop frac)
                              whole
                              (float val))))))
             
             (colour* ()
               (if (minusp (car src))
                   (progn (next) (next) (next) colour)
                   (list (float (/ (num) 255)) (float (/ (num) 255)) (float (/ (num) 255)) "RG")))
             
             (%rect (&rest rest)
               (let ((x1 (num)) (y1 (num)) (w1 (num)) (h1 (num)))
                 (list* (sx (+ x1 x)) (sy (+ y1 y)) (sx w1) (sy h1) rest)))
             
             (sx (xx)
               (float (+ x (* xx sx))))
             
             (sy (yy)
               (float (+ y (* yy sy))))
             
             (sz (v)
               (float (* v (max sx sy)))))
      
      (format nil "q ~{~D ~}RG ~{~A ~}Q~%"
              colour
              (mapcar #'% (loop while src
                                nconc (case (next)
                                        (:bg (append (colour*) (list  "rg")))
                                        (:close (list "h"))
                                        (:curve2 (list (sx (num)) (sy (num)) (sx (num)) (sy (num)) "v"))
                                        (:curve3 (list (sx (num)) (sy (num)) (sx (num)) (sy (num)) (sx (num)) (sy (num)) "c"))
                                        (:fg (append (colour*) (list "RG")))
                                        (:fill (list "F"))
                                        (:line (list (sx (num)) (sy (num)) "l"))
                                        (:line-join (list (num) "j"))
                                        (:line-cap (list (num) "J"))
                                        (:move (list (sx (num)) (sy (num)) "m"))
                                        (:pop (list "Q"))
                                        (:push (list #.(format nil "~%q")))
                                        (:rect (%rect "re"))
                                        (:stroke (list "S"))
                                        (:viewbox #+nil (%rect "re" "W" "n"))
                                        (:width (list (sz (num)) "w")))))))))

