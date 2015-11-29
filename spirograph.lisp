;;;; spirograph.lisp
;;;;
;;;; Copyright (c) 2015 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:spirograph)
(named-readtables:in-readtable :qtools)

(define-widget main-window (QMainWindow)
  ())

(define-menu (main-window File)
  (:item ("Save" (ctrl s))
         (q+:close main-window))
  (:separator)
  (:item ("Quit" (ctrl alt q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About"
         (q+:qmessagebox-information
          main-window "About"
          "Sp program to help me learn QTols.")))


;; map-val is used to map logical coordinates to screen coordinates.
(defun map-val (x xmin xmax new-xmin new-xmax)
  "Map a value from the range xmin,xmax to the range new-xmin,new-xmax"
  (+ (* (/ (- x xmin) (- xmax xmin)) (- new-xmax new-xmin)) new-xmin))

(defun epitrochoid-x (a b h tv)
  "X component of the parametric equation for an epitrochoid curve."
  (- (* (+ a b) (cos tv)) (* h (cos (* tv (/ (+ a b) b))))))

(defun epitrochoid-y (a b h tv)
  "Y component of the parametric equation for an epitrochoid curve."
  (- (* (+ a b) (sin tv)) (* h (sin (* tv (/ (+ a b) b))))))

(defun hypotrochoid-x (a b h tv)
  "X component of the parametric equation for an hypotrochoid curve."
  (+ (* (- a b) (cos tv)) (* h (cos (* tv (/ (- a b) b))))))

(defun hypotrochoid-y (a b h tv)
  "Y component of the parametric equation for an hypotrochoid curve."
  (+ (* (- a b) (sin tv)) (* h (sin (* tv (/ (- a b) b))))))


;; Other interesting values:
;; a 11.50, b 4.0, h 12.0, steps 2400, dt 0.230
;; a 40.00, b 7.00, h 40.00, steps 3200, dt 0.181
;; a 59.00, b 4.27, h 35.22, steps 3200, dt 1.536
;; a 30.00, b 2.45, h 23.00, steps 2000, dt 1.300

(define-widget spirograph-drawer (QWidget)
  ((steps :initform 2000)
   (a-val :initform 30.0)
   (b-val :initform 2.0)
   (h-val :initform 22.0)
   (dt :initform 0.5)
   (x-function :initform #'epitrochoid-x)
   (y-function :initform #'epitrochoid-y))
  (:documentation "The spirograh-drawer widget draws an epitrochoid or hyptrochoid curve using the currently specified parameters."))


(define-override (spirograph-drawer paint-event paint) (ev)
  "Handle pain events."
  (declare (ignore ev))

  ;; Set max-radius to an estimate on the max radius of the curve, given the current values of a, b, and h.
  ;; Multiply by 1.1 to give a bit of empty space around the sides.
  (let ((max-radius (* 1.1 (+ a-val b-val h-val))))
    ;; Define some local functions for convenience
    (flet (
           ;; xmapper maps logical x coordinates in the range x-min to x-max to screen coordinates in the range 0 to width
           (xmapper (x) (map-val x (- max-radius) max-radius 0 (q+:width spirograph-drawer)))

           ;; ymapper does the same thing, but for y coordinates
           (ymapper (y) (map-val y (- max-radius) max-radius 0 (q+:height spirograph-drawer)))
           
           ;; spirograph-x and spirograph-y hide funcall and make the code easier to read below
           (spirograph-x (a b h tv) (funcall x-function a b h tv))
           (spirograph-y (a b h tv) (funcall y-function a b h tv)))
      
      (with-finalizing 
          ;; Create a painter object to draw on
          ((painter (q+:make-qpainter spirograph-drawer)))

        ;; Clear the background
        (q+:fill-rect painter (q+:rect spirograph-drawer) (q+:qt.white))

        ;; Draw the curve
        (loop
           for i below steps
           for cur-t = 0.0 then (+ cur-t dt)
           do
             (q+:draw-line painter
                           (truncate (xmapper (spirograph-x a-val b-val h-val cur-t)))
                           (truncate (ymapper (spirograph-y a-val b-val h-val cur-t)))
                           (truncate (xmapper (spirograph-x a-val b-val h-val (+ dt cur-t))))
                           (truncate (ymapper (spirograph-y a-val b-val h-val (+ dt cur-t))))))))))

(define-widget spirograph-controls (QWidget)
  ()
  (:documentation "The spirograph-controls widget contails input controls for all of the data fields
                   in a spirograph-viewer object."))


;; Create all of the controls

(define-subwidget (spirograph-controls sviewer) (make-instance 'spirograph-drawer)
  "The spirograph-drawer itself.")

(define-subwidget (spirograph-controls aval-spin) (q+:make-qdoublespinbox spirograph-controls)
  "The 'a' value spinbox."
  (q+:set-decimals aval-spin 2)
  (q+:set-single-step aval-spin 0.01)
  (q+:set-maximum aval-spin 1000.0)
  (q+:set-minimum aval-spin 0.01)
  (q+:set-value aval-spin (slot-value sviewer 'a-val)))

(define-subwidget (spirograph-controls bval-spin) (q+:make-qdoublespinbox spirograph-controls)
  "The 'b' value spinbox."
  (q+:set-decimals bval-spin 2)
  (q+:set-single-step bval-spin 0.01)
  (q+:set-maximum bval-spin 1000.0)
  (q+:set-minimum bval-spin 0.01)
  (q+:set-value bval-spin (slot-value sviewer 'b-val)))

(define-subwidget (spirograph-controls hval-spin) (q+:make-qdoublespinbox spirograph-controls)
  "The 'h' value spinbox."
  (q+:set-decimals hval-spin 2)
  (q+:set-single-step hval-spin 0.01)
  (q+:set-maximum hval-spin 1000.0)
  (q+:set-minimum hval-spin 0.01)
  (q+:set-value hval-spin (slot-value sviewer 'h-val)))

(define-subwidget (spirograph-controls steps-spin) (q+:make-qspinbox spirograph-controls)
  "The spinbox for the number of steps."
  (q+:set-maximum steps-spin 10000000)
  (q+:set-minimum steps-spin 4)
  (q+:set-value steps-spin (slot-value sviewer 'steps)))

(define-subwidget (spirograph-controls dt-spin) (q+:make-qdoublespinbox spirograph-controls)
  "The 'h' value spinbox."
  (q+:set-decimals dt-spin 3)
  (q+:set-single-step dt-spin 0.001)
  (q+:set-minimum dt-spin 0.001)
  (q+:set-maximum dt-spin (* 2 pi))
  (q+:set-value dt-spin (slot-value sviewer 'dt)))

(define-subwidget (spirograph-controls epitrochoid-button) (q+:make-qradiobutton "Epitrochoid")
  "Epitrochoid radio button."
  (q+:set-checked epitrochoid-button t))

(define-subwidget (spirograph-controls hypotrochoid-button) (q+:make-qradiobutton "Hypotrochoid")
  "Hypotrochoid radio button.")

(define-subwidget (spirograph-controls button-group) (q+:make-qbuttongroup spirograph-controls)
  "Button group to ensure radio buttons are exclusive."
  (q+:set-exclusive button-group t)
  (q+:add-button button-group epitrochoid-button)
  (q+:add-button button-group hypotrochoid-button))


(define-slot (spirograph-controls type-changed) ()
  "Handle radio button changes that hcange the curve type."
  (declare (connected epitrochoid-button (released)))
  (declare (connected hypotrochoid-button (released)))
  (cond 
    ;; Epitrochoid selected
    ((q+:is-checked epitrochoid-button)
     (setf (slot-value sviewer 'x-function) #'epitrochoid-x)
     (setf (slot-value sviewer 'y-function) #'epitrochoid-y))

    ;; Hypotrochoid selected
    ((q+:is-checked hypotrochoid-button)
     (setf (slot-value sviewer 'x-function) #'hypotrochoid-x)
     (setf (slot-value sviewer 'y-function) #'hypotrochoid-y))
    
    ;; One of the above should always be true, but just in case...
    ;; Print a warning and toggle the  epitrochoid-button
    (t
     (format t "Warning: No type selected, defaulting to epitrochoid.~%")
     (q+:set-checked epitrochoid-button t)
     (setf (slot-value sviewer 'x-function) #'epitrochoid-x)
     (setf (slot-value sviewer 'y-function) #'epitrochoid-y)))

  ;; Repaint to reflect the changes
  (q+:repaint sviewer))

;; It would be nice to handle all spin box changes in one slot, but I don't know 
;; how to ignore the value type.
(define-slot (spirograph-controls steps-changed) ((value int))
  "Handle changes to the steps-spin box."
  (declare (connected steps-spin (value-changed int)))
  (setf (slot-value sviewer 'steps) (q+:value steps-spin))
  (q+:repaint sviewer))

(define-slot (spirograph-controls values-changed) ((value double))
  "Handle changes to all of the spin boxes except steps."
  (declare (connected aval-spin (value-changed double)))
  (declare (connected bval-spin (value-changed double)))
  (declare (connected hval-spin (value-changed double)))
  (declare (connected dt-spin (value-changed double)))
  (setf (slot-value sviewer 'a-val) (q+:value aval-spin))
  (setf (slot-value sviewer 'b-val) (q+:value bval-spin))
  (setf (slot-value sviewer 'h-val) (q+:value hval-spin))
  (setf (slot-value sviewer 'dt) (q+:value dt-spin))
  (q+:repaint sviewer))

(define-subwidget (spirograph-controls control-layout) (q+:make-qvboxlayout spirograph-controls)
  "Layout all of the control widgets in a vertical box layout."

  ;; Create horizontal layouts to hold the labels and spinboxes
  (let ((a-inner (q+:make-qhboxlayout))
        (b-inner (q+:make-qhboxlayout))
        (h-inner (q+:make-qhboxlayout))
        (steps-inner (q+:make-qhboxlayout))
        (dt-inner (q+:make-qhboxlayout)))
    
    ;; Populate the horizontal layouts and add them to the top level vertical layout
    (q+:add-widget a-inner (q+:make-qlabel "A: " spirograph-controls))
    (q+:add-widget a-inner aval-spin)
    (q+:add-layout control-layout a-inner)

    (q+:add-widget b-inner (q+:make-qlabel "B: " spirograph-controls))
    (q+:add-widget b-inner bval-spin)
    (q+:add-layout control-layout b-inner)

    (q+:add-widget h-inner (q+:make-qlabel "H: " spirograph-controls))
    (q+:add-widget h-inner hval-spin)
    (q+:add-layout control-layout h-inner)

    (q+:add-widget steps-inner (q+:make-qlabel "Steps: " spirograph-controls))
    (q+:add-widget steps-inner steps-spin)
    (q+:add-layout control-layout steps-inner)

    (q+:add-widget dt-inner (q+:make-qlabel "dt: " spirograph-controls))
    (q+:add-widget dt-inner dt-spin)
    (q+:add-layout control-layout dt-inner)

    ;; Add the radio buttons directly to the vertical layout
    (q+:add-widget control-layout epitrochoid-button)
    (q+:add-widget control-layout hypotrochoid-button)

    ;; Finally add the spirograph viewer directly to the vertical layout
    (q+:add-widget control-layout sviewer)
    ))


(define-subwidget (main-window controls) (make-instance 'spirograph-controls )
  "The main-window's spirograph-controls widget."
  )

(define-initializer (main-window setup)
  "Set the window title and set the spirograph-controls to be the central widget."
  (setf (q+:window-title main-window) "Spirograph Curve Drawer")
  (setf (q+:central-widget main-window) controls))

(defun main ()
  "Create the main window."
  (with-main-window (window (make-instance 'main-window))))

