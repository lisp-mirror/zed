(in-package #:zed.shader-library)

(defconstant +gamma+ (/ 2.2))
(defconstant +gamma-inverse+ 2.2)

(defun color/rgb->grayscale ((color :vec3))
  (vec3 (+ (* (.r color) 0.2126)
           (* (.g color) 0.7152)
           (* (.b color) 0.0722))))

(defun color/rgb->grayscale ((color :vec4))
  (vec4 (rgb->grayscale (.rgb color)) (.a color)))

(defun color/hue->rgb ((hue :float))
  (let ((v (* hue 6)))
    (saturate
     (vec3 (1- (abs (- v 3)))
           (- 2 (abs (- v 2)))
           (- 2 (abs (- v 4)))))))

(defun color/rgb->hcv ((color :vec3))
  (let* ((k (vec4 0 (/ -1 3.0) (/ 2 3.0) -1))
         (p (if (< (.g color) (.b color))
                (vec4 (.bg color) (.wz k))
                (vec4 (.gb color) (.xy k))))
         (q (if (< (.r color) (.x p))
                (vec4 (.xyw p) (.r color))
                (vec4 (.r color) (.yzx p))))
         (d (- (.x q) (min (.w q) (.y q))))
         (h (abs (+ (/ (- (.w q) (.y q))
                       (+ (* 6 d) 1e-7))
                    (.z q)))))
    (vec3 h d (.x q))))

(defun color/rgb->hcv ((color :vec4))
  (vec4 (color/rgb->hcv (.rgb color)) (.a color)))

(defun color/rgb->hsv ((color :vec3))
  (let* ((hcv (color/rgb->hcv color))
         (s (/ (.y hcv) (+ (.z hcv) 1e-7))))
    (vec3 (.x hcv) s (.z hcv))))

(defun color/rgb->hsv ((color :vec4))
  (vec4 (color/rgb->hsv (.rgb color)) (.a color)))

(defun color/hsv->rgb ((color :vec3))
  (let ((rgb (color/hue->rgb (.x color))))
    (* (1+ (* (1- rgb) (.y color))) (.z color))))

(defun color/hsv->rgb ((color :vec4))
  (vec4 (color/hsv->rgb (.xyz color)) (.a color)))

(defun color/rgb->hcy ((color :vec3))
  (let* ((hcy-weights (vec3 0.299 0.587 0.114))
         (hcv (color/rgb->hcv color))
         (y (dot color hcy-weights))
         (z (dot (color/hue->rgb (.x hcv)) hcy-weights)))
    (if (< y z)
        (multf (.y hcv) (/ z (+ 1e-7 y)))
        (multf (.y hcv) (/ (- 1 z) (- (1+ 1e-7) y))))
    (vec3 (.xy hcv) y)))

(defun color/rgb->hcy ((color :vec4))
  (vec4 (color/rgb->hcy (.rgb color)) (.a color)))

(defun color/hcy->rgb ((color :vec3))
  (let* ((hcy-weights (vec3 0.299 0.587 0.114))
         (rgb (color/hue->rgb (.x color)))
         (z (dot rgb hcy-weights)))
    (cond
      ((< (.z color) z)
       (multf (.y color) (/ (.z color) z)))
      ((< z 1)
       (multf (.y color) (/ (- 1 (.z color)) (- 1 z)))))
    (+ (* (- rgb z) (.y color)) (.z color))))

(defun color/hcy->rgb ((color :vec4))
  (vec4 (color/hcy->rgb (.xyz color)) (.a color)))

(defun color/rgb->hsl ((color :vec3))
  (let* ((hcv (color/rgb->hcv color))
         (l (- (.z hcv) (* (.y hcv) 0.5)))
         (s (/ (.y hcv) (- 1 (+ (abs (1- (* l 2)))) 1e-7))))
    (vec3 (.x hcv) s l)))

(defun color/rgb->hsl ((color :vec4))
  (vec4 (color/rgb->hsl (.rgb color)) (.a color)))

(defun color/hsl->rgb ((color :vec3))
  (let ((rgb (color/hue->rgb (.x color)))
        (c (* (- 1 (abs (1- (* 2 (.z color))))) (.y color))))
    (+ (* (- rgb 0.5) c) (.z color))))

(defun color/hsl->rgb ((color :vec4))
  (vec4 (color/hsl->rgb (.xyz color)) (.a color)))

(defun color/rgb->srgb-approx ((color :vec3))
  (expt (.rgb color) (vec3 +gamma+)))

(defun color/rgb->srgb-approx ((color :vec4))
  (vec4 (color/rgb->srgb-approx (.rgb color)) (.a color)))

(defun color/rgb->srgb ((color :vec3))
  (mix (* 12.92 color)
       (- (* 1.055 (expt color (vec3 (/ 2.4)))) 0.055)
       (step (vec3 0.0031308) color)))

(defun color/rgb->srgb ((color :vec4))
  (vec4 (color/rgb->srgb (.rgb color)) (.a color)))

(defun color/srgb->rgb-approx ((color :vec3))
  (expt color (vec3 +gamma-inverse+)))

(defun color/srgb->rgb-approx ((color :vec4))
  (vec4 (color/srgb->rgb-approx (.rgb color)) (.a color)))

(defun color/srgb->rgb ((color :vec3))
  (mix (/ color 12.92)
       (expt (/ (+ color 0.055) 1.055) (vec3 2.4))
       (step (vec3 0.04045) color)))

(defun color/srgb->rgb ((color :vec4))
  (vec4 (color/srgb->rgb (.rgb color)) (.a color)))

(defun color/rgb->xyz ((color :vec3))
  (let ((transform (mat3 0.4124564 0.3575761 0.1804375
                         0.2126729 0.7151522 0.072175
                         0.0193339 0.119192 0.9503041)))
    (* transform color)))

(defun color/rgb->xyz ((color :vec4))
  (vec4 (color/rgb->xyz (.rgb color)) (.a color)))

(defun color/xyz->rgb ((color :vec3))
  (let ((transform (mat3 3.2404542 -1.5371385 -0.4985314
                         -0.969266 1.8760108 0.041556
                         0.0556434 -0.2040259 1.0572252)))
    (* transform color)))

(defun color/xyz->rgb ((color :vec4))
  (vec4 (color/xyz->rgb (.xyz color)) (.a color)))

(defun color/xyy->xyz ((color :vec3))
  (let* ((y (.z color))
         (x (/ (* y (.x color)) (.y color)))
         (z (/ (* y (- 1 (.x color) (.y color))) (.y color))))
    (vec3 x y z)))

(defun color/xyy->xyz ((color :vec4))
  (vec4 (color/xyy->xyz (.xyz color)) (.a color)))

(defun color/xyz->xyy ((color :vec3))
  (let* ((v (+ (.x color) (.y color) (.z color)))
         (x (/ (.x color) v))
         (y (/ (.y color) v)))
    (vec3 x y (.y color))))

(defun color/xyz->xyy ((color :vec4))
  (vec4 (color/xyz->xyy (.xyz color)) (.a color)))

(defun color/rgb->xyy ((color :vec3))
  (color/xyz->xyy (color/rgb->xyz color)))

(defun color/rgb->xyy ((color :vec4))
  (color/xyz->xyy (color/rgb->xyz color)))

(defun color/xyy->rgb ((color :vec3))
  (color/xyz->rgb (color/xyy->xyz color)))

(defun color/xyy->rgb ((color :vec4))
  (color/xyz->rgb (color/xyy->xyz color)))

(defun color/set-exposure ((color :vec3)
                           (exposure :int))
  (* color (expt 2 exposure)))

(defun color/set-exposure ((color :vec4)
                           (exposure :int))
  (vec4 (color/set-exposure (.rgb color) exposure) (.a color)))

(defun color/set-saturation ((color :vec3)
                             (saturation :float))
  (mix (color/rgb->grayscale color) color saturation))

(defun color/set-saturation ((color :vec4)
                             (saturation :float))
  (vec4 (color/set-saturation (.rgb color) saturation) (.a color)))

(defun color/set-contrast ((color :vec3)
                           (contrast :float))
  (+ (* (- (.rgb color) 0.5) contrast) 0.5))

(defun color/set-contrast ((color :vec4)
                           (contrast :float))
  (vec4 (color/set-contrast (.rgb color) contrast) (.a color)))

(defun color/set-brightness ((color :vec3)
                             (brightness :float))
  (+ color brightness))

(defun color/set-brightness ((color :vec4)
                             (brightness :float))
  (vec4 (color/set-brightness (.rgb color) brightness) (.a color)))

(defun color/set-gamma ((color :vec3)
                        (gamma :float))
  (vec3 (expt (abs (.r color)) (/ gamma))
        (expt (abs (.g color)) (/ gamma))
        (expt (abs (.b color)) (/ gamma))))

(defun color/set-gamma ((color :vec4)
                        (gamma :float))
  (vec4 (set-gamma (.rgb color) gamma) (.a color)))

(defun color/filter ((color :vec3)
                     (filter :vec3)
                     (exposure :int))
  (let ((exposure (color/set-exposure color exposure)))
    (* color filter exposure)))

(defun color/filter ((color :vec4)
                     (filter :vec3)
                     (exposure :int))
  (vec4 (color/filter (.rgb color) filter exposure) (.a color)))

(defun color/tone-map-linear ((color :vec3)
                              (exposure :int))
  (color/set-gamma (color/set-exposure color exposure) +gamma+))

(defun color/tone-map-linear ((color :vec4)
                              (exposure :int))
  (vec4 (color/tone-map-linear (.rgb color) exposure) (.a color)))

(defun color/tone-map-reinhard ((color :vec3)
                                (exposure :int))
  (let ((color (color/set-exposure color exposure)))
    (color/set-gamma (/ color (1+ color)) +gamma+)))

(defun color/tone-map-reinhard ((color :vec4)
                                (exposure :int))
  (vec4 (color/tone-map-reinhard (.rgb color) exposure) (.a color)))

(defun color/tone-map-haarm-peter-duiker ((color :vec3)
                                          (exposure :int)
                                          (film-lut :sampler-2d))
  (let* ((color (color/set-exposure color exposure))
         (log-color (saturate (/ (+ (* (/ (log10 (* 0.4 color)) 0.002) 0.45)
                                    444)
                                 1023.0)))
         (padding 0.001953125)
         (r (vec2 (mix padding (- 1 padding) (.r color)) 0.5))
         (g (vec2 (mix padding (- 1 padding) (.g color)) 0.5))
         (b (vec2 (mix padding (- 1 padding) (.b color)) 0.5)))
    (vec3 (.r (texture film-lut r))
          (.r (texture film-lut g))
          (.r (texture film-lut b)))))

(defun color/tone-map-haarm-peter-duiker ((color :vec4)
                                          (exposure :int)
                                          (film-lut :sampler-2d))
  (vec4 (color/tone-map-haarm-peter-duiker (.rgb color) exposure film-lut)
        (.a color)))

(defun color/tone-map-hejl-burgess-dawson ((color :vec3)
                                           (exposure :int))
  (let* ((color (color/set-exposure color exposure))
         (x (max (vec3 0) (- color 0.004)))
         (y (* 6.2 x)))
    (/ (* x (+ y 0.5))
       (+ (* x (+ y 1.7)) 0.06))))

(defun color/tone-map-hejl-burgess-dawson ((color :vec4)
                                           (exposure :int))
  (vec4 (color/tone-map-hejl-burgess-dawson (.rgb color) exposure) (.a color)))

(defun color/tone-map-uncharted2 ((color :vec3)
                                  (exposure :int))
  (flet ((tone-map ((x :vec3))
           (- (/ (+ (* x (+ (* 0.15 x) 0.05)) 0.004)
                 (+ (* x (+ (* 0.15 x) 0.5)) 0.06))
              0.006)))
    (expt (* (tone-map (* 2 (color/set-exposure color exposure)))
             (/ (tone-map (vec3 11.2))))
          (vec3 +gamma+))))

(defun color/tone-map-uncharted2 ((color :vec4)
                                  (exposure :int))
  (vec4 (color/tone-map-uncharted2 (.rgb color) exposure) (.a color)))

(defun color/tone-map-aces ((color :vec3)
                            (exposure :int))
  (let ((color (color/set-exposure color exposure))
        (a 2.51)
        (b 0.03)
        (c 2.43)
        (d 0.59)
        (e 0.14))
    (saturate (/ (* color (+ (* color a) b))
                 (+ (* color (+ (* color c) d)) e)))))

(defun color/tone-map-aces ((color :vec4)
                            (exposure :int))
  (vec4 (color/tone-map-aces (.rgb color) exposure) (.a color)))
