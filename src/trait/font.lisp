(in-package #:zed.trait.font)

(z::define-internal-trait font (:order (:before geometry))
  ((%asset :reader asset
           :inline t
           :type list
           :initarg :asset
           :initform nil)
   (%text :reader text
          :inline t
          :type (or string function)
          :initarg :text
          :initform "")
   (%update-rate :reader update-rate
                 :inline t
                 :type u:f32
                 :initarg :update-rate
                 :initform 0.5)
   (%spec :accessor spec
          :inline t
          :initform nil)
   (%update-time :accessor update-time
                 :inline t
                 :type u:non-negative-fixnum
                 :initform 0)
   (%dimensions :accessor dimensions
                :inline t
                :type v2:vec
                :initform (v2:zero)))
  (:attach attach)
  (:update update))

(z:define-geometry-layout text ()
  (:data (:format :interleaved)
         (position :type float :count 2)
         (uv :type float :count 2)))

(z:define-geometry text ()
  (:layout text
   :vertex-count 6
   :primitive :triangles))

(defun load-font-asset (font)
  (let ((context (z:trait-context font))
        (asset (asset font)))
    (unless asset
      (error "A font trait must have an asset specified."))
    (z::with-resource-cache (context :font asset)
      (destructuring-bind (asset-system asset-path) asset
        (prog1 (z::with-asset (asset asset-path data :format :character-stream)
                 (3b-bmfont-json:read-bmfont-json data))
          (v:debug :zed "Cached font resource: ~a (~s)" asset-path asset-system))))))

(defun make-geometry (font)
  (let* ((context (z:trait-context font))
         (game-object (z:trait-owner font))
         (geometry (z:make-trait context 'tr.geo:geometry :name 'text)))
    (z:attach-trait game-object geometry)))

(defun resolve-text (font)
  (let ((text (text font)))
    (typecase text
      (string text)
      ((or function symbol)
       (let ((text (funcall text)))
         (unless (stringp text)
           (error "Font trait ~s has text that is not a string." font))
         text)))))

(defun generate-geometry (geometry)
  (lambda (data)
    (z::update-geometry (tr.geo::resource geometry) :data data)))

(defun center-text (font width height)
  (let ((game-object (z:trait-owner font)))
    (v2:with-components ((d (dimensions font))
                         (s (z:get-scale game-object)))
      (setf dx width
            dy height)
      (z::translate game-object (v3:vec (* (- dx) sx) (* dy sy) 0) :replace-p t :instant-p t))))

(defun update-p (font)
  (let* ((context (z:trait-context font))
         (clock (z::context-clock context))
         (elapsed-time (z::clock-elapsed-time clock)))
    (>= elapsed-time (+ (update-time font)
                        (the fixnum (round (z::clock-units-per-second clock)
                                           (/ (update-rate font))))))))

;;; Hooks

(u:fn-> attach (font) null)
(defun attach (font)
  (declare (optimize speed))
  (setf (spec font) (load-font-asset font))
  (make-geometry font)
  nil)

(u:fn-> update (font) null)
(defun update (font)
  (declare (optimize speed))
  (let* ((context (z:trait-context font))
         (game-object (z:trait-owner font))
         (clock (z::context-clock context))
         (elapsed-time (z::clock-elapsed-time clock))
         (geometry (z:find-trait game-object 'tr.geo:geometry)))
    (when (update-p font)
      (u:mvlet* ((text (resolve-text font))
                 (func (generate-geometry geometry))
                 (width height (z::map-font-glyphs (spec font) func text)))
        (center-text font width height))
      (setf (update-time font) elapsed-time)))
  nil)
