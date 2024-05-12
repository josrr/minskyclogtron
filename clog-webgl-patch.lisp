(in-package :clog-webgl)

(export '(texture-image-2d
          tex-parameteri
          uniformf
          uniformi
          bind-canvas-frame-buffer
          bind-null-texture))

(defgeneric texture-image-2d (clog-webgl glenum-target level glenum-iformat
                              width height border glenum-format glenum-type image)
  (:documentation "Specifies all levels of two-dimensional texture storage"))

(defmethod texture-image-2d ((webgl clog-webgl) glenum-target level glenum-iformat
                             width height border glenum-format glenum-type image)
  (execute webgl (format nil "texImage2D(~A.~A, ~D, ~A.~A, ~D, ~D, ~D, ~A.~A, ~A.~A, ~A)"
                         (script-id webgl) glenum-target level
                         (script-id webgl) glenum-iformat
                         width height border
                         (script-id webgl) glenum-format
                         (script-id webgl) glenum-type
                         "null")))

(defgeneric tex-parameteri (clog-webgl glenum-target glenum-pname glenum-vname)
  (:documentation "Set parameters of textures"))

(defmethod tex-parameteri ((obj clog-webgl) glenum-target glenum-pname glenum-vname)
  (query obj (format nil "texParameteri(~A.~A,~A.~A,~A.~A)"
                     (script-id obj) glenum-target
                     (script-id obj) glenum-pname
                     (script-id obj) glenum-vname)))

(defgeneric uniformf (clog-webgl-program location x &optional y z w)
  (:documentation "Sets the value of uniform at LOCATION in clog-webgl-program"))

(defmethod uniformf ((webgl clog-webgl) location x &optional y z w)
  (execute webgl (cond
                 (w (format nil "uniform4fv(~A, new Float32Array([~F,~F,~F,~F]))" (script-id location)
                            (float x) (float y) (float z) (float w)))
                 (z (format nil "uniform3fv(~A, new Float32Array([~F,~F,~F]))" (script-id location)
                            (float x) (float y) (float z)))
                 (y (format nil "uniform2fv(~A, new Float32Array([~F,~F]))" (script-id location)
                            (float x) (float y)))
                 (x (format nil "uniform1fv(~A, new Float32Array([~F]))" (script-id location)
                            (float x))))))

(defgeneric uniformi (clog-webgl-program location x &optional y z w)
  (:documentation "Sets the value of uniform at LOCATION in clog-webgl-program"))

(defmethod uniformi ((webgl clog-webgl) location x &optional y z w)
  (execute webgl (cond
                   (w (format nil "uniform4iv(~A, new Int32Array([~D,~D,~D,~D]))" (script-id location) x y z w))
                   (z (format nil "uniform3iv(~A, new Int32Array([~D,~D,~D]))" (script-id location) x y z))
                   (y (format nil "uniform2iv(~A, new Int32Array([~D,~D]))" (script-id location) x y))
                   (x (format nil "uniform1iv(~A, new Int32Array([~D]))" (script-id location) x)))))

(defmethod bind-canvas-frame-buffer ((webgl clog-webgl) glenum-target)
  (execute webgl (format nil "bindFramebuffer(~A.~A, null)" (script-id webgl) glenum-target)))

(defmethod bind-null-texture ((webgl clog-webgl) glenum-target)
  (execute webgl (format nil "bindTexture(~A.~A,null)"
                         (script-id webgl) glenum-target)))

(defgeneric create-webgl (clog-canvas &key context attributes)
  (:documentation "Create a new CLOG-WebGL from a CLOG-Canvas. Context
can be webgl (version 1) or webgl2 (default). Attributes is a
plist: (\"attr0\" val0 ...)"))

(defmethod create-webgl ((obj clog-canvas) &key (context "webgl2") attributes)
  (let ((web-id (generate-id)))
    (js-execute obj (format nil "clog['~A']=clog['~A'].getContext('~A'~@[,{~{~A: ~A~^,~}}~])"
                            web-id (html-id obj) context attributes))
    (make-instance 'clog-webgl
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))
