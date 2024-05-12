(in-package #:minskyclogtron)

(defparameter *width* 1024)
(defparameter *height* 1024)

(defconstant +dmask+ #o777777)
(defconstant +ov+ #o1000000)

(defparameter *test-word* '(7 8 8 8 3 2))
(defparameter *first-time* t)

(declaim (inline 18bit->num add18 cma18 sar18 sub18))
(declaim (ftype (function (fixnum) fixnum) cma18 18bit->num))

(defun cma18 (ac)
  (declare (optimize (speed 3) (debug 0))
           (type fixnum ac))
  (logxor +dmask+ ac))

(declaim (ftype (function (fixnum fixnum) fixnum) add18 sub18 sar18))

(defun add18 (ac mb)
  (declare (optimize (speed 3) (debug 0))
           (type fixnum ac mb))
  (incf ac mb)
  (cond ((not (zerop (logand ac +ov+)))
         (logand (1+ ac) +dmask+))
        ((= ac +dmask+) 0)
        (t ac)))

(defun sub18 (ac mb)
  (declare (optimize (speed 3) (debug 0))
           (type fixnum ac mb))
  (setf ac (cma18 ac))
  (incf ac mb)
  (when (not (zerop (logand ac +ov+)))
    (setf ac (logand (1+ ac) +dmask+)))
  (cma18 ac))

(declaim (type (simple-array fixnum) *shift-masks-hi*))
(defparameter *shift-masks-hi*
  (make-array 19 :element-type 'fixnum
                 :initial-contents (loop for sh from 0 to 18
                                         collect (ash (1- (ash 1 sh))
                                                      (- 18 sh)))))

(defun sar18 (ac sh)
  (declare (optimize (speed 3) (debug 0))
           (type fixnum ac sh))
  (assert (<= 0 sh 18))
  (let ((bits (ash ac (- sh))))
    (if (zerop (ldb (byte 1 17) ac))
        bits
        (logior (aref *shift-masks-hi* sh) bits))))

(defun 18bit->num (18bit)
  (declare (optimize (speed 3) (debug 0))
           (type fixnum 18bit))
  (if (zerop (ldb (byte 1 17) 18bit))
      (ldb (byte 17 0) 18bit)
      (- (ldb (byte 17 0) (cma18 18bit)))))

;;;; '(4 4 8 8 3 3)
;;;; '(7 8 8 8 3 2)

(defun gen-minskytron-pars (&optional data values)
  (make-array 12
              :initial-contents (if data
                                    (append (coerce (subseq data 0 6) 'list)
                                            (or values (list #o757777 0 0 #o040000 #o020000 0)))
                                    (let ((data (if *first-time*
                                                    (prog1 *test-word*
                                                      (setf *first-time* nil))
                                                    (loop repeat 6 collect (1+ (random 8))))))
                                      (format *debug-io* "TEST WORD: ~S~%" data)
                                      (append data
                                              (or values (list #o757777 0 0 #o040000 #o020000 0)))))))

(defun gen-minskytron (points data &optional (max-iter 1024))
  (declare (optimize (speed 3) (safety 1) (debug 0))
           (type fixnum max-iter))
  (flet ((draw-point (idx x y)
           (declare (type fixnum x y idx))
           (let* ((x (ash (18bit->num x) -8))
                  (y (ash (18bit->num y) -8))
                  (px (float x 1f0))
                  (py (float y 1f0))
                  (idx (* 2 idx)))
             (declare (type fixnum idx x y)
                      (type single-float px py))
             ;;(format *debug-io* "~2D:~D,~D~%" idx px py)
             (setf (aref points idx) px
                   (aref points (1+ idx)) py))))
    (destructuring-bind (sh0 sh1 sh2 sh3 sh4 sh5 xa ya xb yb xc yc) (coerce data 'list)
      (declare (type fixnum sh0 sh1 sh2 sh3 sh4 sh5 xa ya xb yb xc yc))
      (loop for i fixnum from 0 below (or max-iter 2048)
            for idx from 0 by 3
            do (setf ya (add18 ya (sar18 (add18 xa xb) sh0))
                     xa (sub18 xa (sar18 (sub18 ya yb) sh1))
                     yb (add18 yb (sar18 (sub18 xb xc) sh2))
                     xb (sub18 xb (sar18 (sub18 yb yc) sh3))
                     yc (add18 yc (sar18 (sub18 xc xa) sh4))
                     xc (sub18 xc (sar18 (sub18 yc ya) sh5)))
               (draw-point idx xa ya)
               (draw-point (+ 1 idx) xb yb)
               (draw-point (+ 2 idx) xc yc)
            finally (setf (aref data 6) xa
                          (aref data 7) ya
                          (aref data 8) xb
                          (aref data 9) yb
                          (aref data 10) xc
                          (aref data 11) yc)))))

;;;; 7 8 8 5 1 2
;;;; 8 6 4 7 4 2
;;;; 5 6 7 8 4 5
;;;; 6 6 2 4 7 8
;;;; 1 4 7 7 3 5
;;;; 5 5 3 3 8 8
;;;; 4 4 7 7 0 2
;;;; 4 3 7 7 0 2
