(defpackage #:minskyclogtron
  (:use #:cl #:clog #:clog-webgl)
  (:export #:start))

(in-package :minskyclogtron)

(defclass gl-object ()
  ((webgl :initarg :webgl :reader webgl)
   (vbo :reader vbo)
   (vao :reader vao)
   (program :initarg :program :accessor program)
   (xy :initarg :xy :accessor xy)))

(defmethod initialize-instance :after ((instance gl-object) &rest initargs &key &allow-other-keys)
  (with-slots (vbo vao) instance
    (let ((webgl (getf initargs :webgl)))
      (setf vao (create-vertex-array webgl)
            vbo (create-webgl-buffer webgl)))))

(defgeneric draw (gl-object &optional texture)
  (:documentation "Draws a GL-OBJECT"))

(defclass minskytron (gl-object)
  ((num-points :initform 128 :initarg :num-points
               :accessor minskytron-num-points)
   (fb :reader minskytron-fb)
   (bf :reader minskytron-bf)
   (points :initarg :points :accessor minskytron-points)
   (words :initarg :words :accessor minskytron-words)
   (u-model :initarg :model :accessor minskytron-u-model)
   (u-view :initarg :view :accessor minskytron-u-view)
   (u-proy :initarg :proy :accessor minskytron-u-proy)
   (u-color :initarg :color :accessor minskytron-u-color)
   (u-size :initarg :size :accessor minskytron-u-size)
   (data :initarg :data :accessor minskytron-data)))

(defmethod initialize-instance :after ((instance minskytron) &rest initargs &key &allow-other-keys)
  (with-slots (fb bf) instance
    (let ((webgl (getf initargs :webgl)))
      (setf fb (create-webgl-frame-buffer webgl)
            bf (create-webgl-texture webgl)))))

(defparameter *minskytron-v-shader* "#version 300 es
in vec2 posicion;
out vec3 Color;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;
uniform vec3 color;
uniform float size;

void main() {
  gl_PointSize=size;
  Color = color;
  gl_Position = proj*view*model*vec4(posicion, 0.0, 1.0);
}")

(defparameter *minskytron-f-shader* "#version 300 es
precision highp float;
in vec3 Color;
out vec4 outColor;

void main() {
  vec2 coord = 2.0 * gl_PointCoord - 1.0;
  if ( dot(coord, coord) > 1.0 )
    discard;
  outColor = vec4(Color, 1.0);
}")

(defun compile-program (webgl vertex-shader fragment-shader)
  (let ((program (compile-webgl-program webgl
                                        (compile-shader-source webgl :VERTEX_SHADER vertex-shader)
                                        (compile-shader-source webgl :FRAGMENT_SHADER fragment-shader))))
    (use-program program)
    program))

(defun make-minskytron (webgl num-points width height)
  (let* ((program (compile-program webgl *minskytron-v-shader* *minskytron-f-shader*))
         (med-width (/ width 2f0))
         (med-height (/ height 2f0))
         (*first-time* t)
         (m (make-instance 'minskytron
                           :webgl webgl
                           :num-points num-points
                           :program program
                           :xy (attribute-location program "posicion")
                           :points (make-array (+ (* num-points 6)
                                                  36) ;; 36 = 18*2; son los 18=3*6 indicadores de bits
                                               :element-type 'single-float
                                               :initial-element 0.0f0)
                           :model (uniform-location program "model")
                           :view (uniform-location program "view")
                           :proy (uniform-location program "proj")
                           :color (uniform-location program "color")
                           :size (uniform-location program "size")
                           :data (gen-minskytron-pars))))
    (uniform-matrix webgl 4 (minskytron-u-view m) nil (coerce (rtg-math.matrix4:identity) 'list))
    (uniform-matrix webgl 4 (minskytron-u-proy m) nil (coerce (kit.math:ortho-matrix (- med-width) med-width
                                                                                     (- med-height) med-height
                                                                                     0.0001 3000.0)
                                                              'list))
    (uniform-matrix webgl 4 (minskytron-u-model m) nil (coerce (rtg-math.matrix4:identity) 'list))
    (bind-vertex-array (vao m))
    (bind-buffer (vbo m) :ARRAY_BUFFER)
    (enable-vertex-attribute-array webgl (xy m))
    (vertex-attribute-pointer webgl (xy m) 2 :FLOAT nil 8 0)
    (bind-frame-buffer (minskytron-fb m) :DRAW_FRAMEBUFFER)
    (bind-texture (minskytron-bf m) :TEXTURE_2D)
    (texture-image-2d webgl :TEXTURE_2D 0 :RGBA width height 0 :RGBA :UNSIGNED_BYTE nil)
    (tex-parameteri webgl :TEXTURE_2D :TEXTURE_MIN_FILTER :LINEAR)
    (tex-parameteri webgl :TEXTURE_2D :TEXTURE_MAG_FILTER :LINEAR)
    (bind-null-texture webgl :TEXTURE_2D)
    (frame-buffer-texture-2d webgl :FRAMEBUFFER :COLOR_ATTACHMENT0 :TEXTURE_2D
                             (minskytron-bf m) 0)
    (bind-frame-buffer (minskytron-fb m) :FRAMEBUFFER)
    (clear-color webgl 0.0f0 0.0f0 0.0f0 1.0f0)
    (clear-webgl webgl :COLOR_BUFFER_BIT)
    m))

(defmethod draw ((obj minskytron) &optional texture)
  (declare (ignore texture))
  (let ((high-bits 0)
        (low-bits 0)
        (idx-bits (* 6 (minskytron-num-points obj))))
    (labels ((add-bit-points (points)
               (loop for px in points
                     do (setf (aref (minskytron-points obj)      idx-bits) px
                              (aref (minskytron-points obj) (1+ idx-bits)) -488f0)
                        (incf idx-bits 2)))
             (draw-words (words)
               (loop with high-points = nil and low-points = nil
                     for word across words
                     for w from 0 by 56
                     do (loop for i from 2 downto 0
                              for pos-x = (+ w (* -15 i) 200f0)
                              if (= 1 (ldb (byte 1 i) (1- word))) do
                                (incf high-bits)
                                (push pos-x high-points)
                              else do
                                (incf low-bits)
                                (push pos-x low-points))
                     finally (add-bit-points high-points)
                             (add-bit-points low-points))))
      (use-program (program obj))
      (bind-vertex-array (vao obj))
      (bind-frame-buffer (minskytron-fb obj) :FRAMEBUFFER)
      (clear-webgl (webgl obj) :COLOR_BUFFER_BIT)
      (gen-minskytron (minskytron-points obj)
                      (minskytron-data obj)
                      (minskytron-num-points obj))
      (draw-words (subseq (minskytron-data obj) 0 6))
      (bind-buffer (vbo obj) :ARRAY_BUFFER)
      (buffer-data (vbo obj) (coerce (minskytron-points obj) 'list) "Float32Array" :STATIC_DRAW)
      (let ((num-points (* 3 (minskytron-num-points obj))))
        (uniformf (webgl obj) (minskytron-u-color obj) 0.3 0.9 0.8)
        (uniformf (webgl obj) (minskytron-u-size obj) 2.0)
        (draw-arrays (webgl obj) :POINTS 0 num-points)
        (uniformf (webgl obj) (minskytron-u-color obj) 0.1 1.0 0.25)
        (uniformf (webgl obj) (minskytron-u-size obj) 14.0)
        (draw-arrays (webgl obj) :POINTS num-points high-bits)
        (uniformf (webgl obj) (minskytron-u-color obj) 0.05 0.25 0.125)
        (draw-arrays (webgl obj) :POINTS (+ high-bits num-points) low-bits))))
  (minskytron-bf obj))

(defun minskytron-reset (minskytron)
  (clear-color (webgl minskytron) 0.0 0.0 0.0 1.0)
  (clear-webgl (webgl minskytron) :COLOR_BUFFER_BIT)
  (clear-color (webgl minskytron) 0.0 0.0 0.0 0.05)
  (let ((*first-time* t))
    (setf (minskytron-data minskytron) (gen-minskytron-pars))))

(defun minskytron-restart (minskytron)
  (clear-color (webgl minskytron) 0.0 0.0 0.0 1.0)
  (clear-webgl (webgl minskytron) :COLOR_BUFFER_BIT)
  (clear-color (webgl minskytron) 0.0 0.0 0.0 0.05)
  (setf (minskytron-data minskytron) (gen-minskytron-pars (minskytron-data minskytron))))

(defparameter *quad*
  (make-array 16 :element-type 'single-float
                 :initial-contents '(-1.0  1.0 0.0 1.0
                                     1.0  1.0 1.0 1.0
                                     1.0 -1.0 1.0 0.0
                                     -1.0 -1.0 0.0 0.0)))

(defparameter *quad-elems*
  (make-array 6 :element-type 'fixnum
                :initial-contents '(0 1 2
                                    2 3 0)))

(defparameter *quad-v-shader*
  "#version 300 es
layout (location = 0) in vec2 aPos;
layout (location = 1) in vec2 aTexCoords;

out vec2 TexCoords;

void main()
{
    gl_Position = vec4(aPos.x, aPos.y, 0.0, 1.0);
    TexCoords = aTexCoords;
}")

(defparameter *quad-f-shader*
  "#version 300 es
precision highp float;
out vec4 FragColor;

in vec2 TexCoords;

uniform sampler2D screenTexture;

void main()
{
    FragColor = texture(screenTexture, TexCoords);
}")

(defclass quad (gl-object)
  ((ebo :reader quad-ebo)
   (tex-coords :initarg :tex-coords :accessor quad-tex-coords)))

(defmethod initialize-instance :after ((instance quad) &rest initargs &key &allow-other-keys)
  (with-slots (vao ebo vbo) instance
    (bind-vertex-array vao)
    (let ((webgl (getf initargs :webgl))
          (xy (getf initargs :xy))
          (tex-coords (getf initargs :tex-coords)))
      (setf ebo (create-webgl-buffer webgl))
      (bind-buffer vbo :ARRAY_BUFFER)
      (bind-buffer ebo :ELEMENT_ARRAY_BUFFER)
      (buffer-data vbo (coerce *quad* 'list) "Float32Array" :STATIC_DRAW)
      (buffer-data ebo (coerce *quad-elems* 'list) "Uint16Array" :STATIC_DRAW)
      (vertex-attribute-pointer webgl xy 2 :FLOAT nil 16 0)
      (vertex-attribute-pointer webgl tex-coords 2 :FLOAT nil 16 8)
      (enable-vertex-attribute-array webgl xy)
      (enable-vertex-attribute-array webgl tex-coords))))

(defun make-quad (webgl)
  (let ((program (compile-program webgl *quad-v-shader* *quad-f-shader*)))
    (make-instance 'quad
                   :webgl webgl
                   :program program
                   :xy (attribute-location program "aPos")
                   :tex-coords (attribute-location program "aTexCoords"))))

(defmethod draw ((obj quad) &optional texture)
  (use-program (program obj))
  (bind-canvas-frame-buffer (webgl obj) :framebuffer)
  (bind-vertex-array (vao obj))
  (when texture
    (bind-texture texture :TEXTURE_2D))
  (draw-elements (webgl obj) :TRIANGLES 6 :UNSIGNED_SHORT 0))

(defclass byte-switches (clog-element)
  ((num-switch :initarg :num-switch :initform 0 :accessor num-switch)
   (byte-value :initarg :byte-value :initform 7 :accessor byte-value)
   (checkboxes :initarg :checkboxes :accessor checkboxes)))

(defgeneric create-byte-switches (clog-obj &key num-switch value hidden class html-id
                                             auto-place on-change)
  (:documentation "Create a new CLOG-Byte-switches as child of CLOG-OBJ if
:AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ. If hidden is
true visiblep is set to nil."))

;;;; GUI

(defmethod create-byte-switches ((obj clog-obj) &key (num-switch 0) (value 0) hidden class html-id
                                                  (auto-place t) on-change)
  (loop with value = (1- value)
        with div = (change-class (create-div obj :class "border"
                                                 :hidden hidden :class class
                                                 :html-id html-id
                                                 :auto-place auto-place)
                                 'byte-switches
                                 :num-switch num-switch
                                 :byte-value value)
        with checkboxes = nil
        for i from 2 downto 0
        for bit = (if (= 1 (ldb (byte 1 i) value)) t nil)
        initially (setf (display div) :flex
                        (align-items div) :center
                        (justify-content div) :center)
                  (set-padding div "0.75ex" "0.2em" "0.25em" "1ex")
        for div-sh = (create-div div :class "form-check form-switch")
        for ch = (create-form-element div-sh :checkbox :class "form-check-input")
        do (setf (checkedp ch) bit)
           (push ch checkboxes)
           (when on-change
             (set-on-change ch (let ((i i))
                                 (lambda (obj)
                                   (funcall on-change div obj i)))))
        finally (setf (checkboxes div) (reverse checkboxes))
                (return div)))

(defgeneric update (cl-obj))

(defmethod update ((obj byte-switches))
  (with-slots (checkboxes byte-value) obj
    (loop for i from 2 downto 0
          for ch in checkboxes
          ;; do (setf (checkedp ch) (if (= 1 (ldb (byte 1 i) byte-value)) t nil))
          do (jquery-execute ch (format nil "prop('checked', ~A)"
                                        (if (= 1 (ldb (byte 1 i) byte-value)) "true" "false"))))))

(defun create-on-change (minskytron)
  (lambda (obj checkbox byte-pos)
    (setf (ldb (byte 1 byte-pos) (byte-value obj)) (if (checkedp checkbox) 1 0)
          (elt (minskytron-data minskytron) (num-switch obj)) (1+ (byte-value obj)))))

(defun create-on-restart (minskytron)
  (lambda (obj)
    (declare (ignore obj))
    (minskytron-restart minskytron)))

(defun create-on-reset (minskytron switches)
  (lambda (obj)
    (declare (ignore obj))
    (minskytron-reset minskytron)
    (loop for value across (subseq (minskytron-data minskytron) 0 6)
          for sh in switches
          do (setf (byte-value sh) (1- value))
             (update sh))))

(defun on-new-window (body)
  (setf (title (html-document body)) "Minskytron")
  (load-css (html-document body)
            "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css")
  (load-script (html-document body)
               "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js")
  (clog-gui-initialize body)
  (add-class body "w3-blue")
  (let* ((actions (create-gui-menu-drop-down (create-gui-menu-bar body) :content "Minskytron"))
         (div-canvas (create-div body))
         (canvas (create-canvas div-canvas :width *width* :height *height*))
         (gl (create-webgl canvas :attributes '("preserveDrawingBuffer" "true")))
         (quad (make-quad gl))
         (minskytron (make-minskytron gl 64 *width* *height*))
         (sb (create-style-block body))
         (div-form (create-div body))
         (form (create-form div-form))
         (switches (loop for i from 0
                         for value across (subseq (minskytron-data minskytron) 0 6)
                         collect (create-byte-switches form
                                                       :num-switch i
                                                       :value value
                                                       :on-change (create-on-change minskytron)))))
    (create-gui-menu-item actions :content "Restart"
                                  :on-click (create-on-restart minskytron))
    (create-gui-menu-item actions :content "Reset"
                                  :on-click (create-on-reset minskytron switches))
    (setf (display form) :flex
          (align-items form) :center
          (justify-content form) :center)
    (format *debug-io* "~D x ~D~%" (drawing-buffer-width gl) (drawing-buffer-height gl))
    (add-style sb :element "canvas" '(("width" "512px") ("height" "512px")))
    (add-class canvas "w3-black")
    (set-border canvas :medium :solid "#0066aa")
    (set-margin canvas "6px" "6px" "6px" "6px")
    (setf (display div-canvas) :flex
          (align-items div-canvas) :center
          (justify-content div-canvas) :center)
    (enable-capability gl :BLEND)
    (blend-function gl :ONE :ONE_MINUS_SRC_ALPHA)
    (clear-color gl 0.0f0 0.0f0 0.0f0 1.0f0)
    (clear-webgl gl :COLOR_BUFFER_BIT)
    (clear-color gl 0.0f0 0.0f0 0.0f0 0.05f0)
    (bt:make-thread (lambda ()
                      (loop (draw quad (draw minskytron))
                            (sleep 1/30))))))

(defun start ()
  "Start Minskytron."
  (initialize 'on-new-window)
  (open-browser))
