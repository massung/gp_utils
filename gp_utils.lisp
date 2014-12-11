;;;; Graphics Port Utility Functions for LispWorks
;;;;
;;;; Copyright (c) 2014 by Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License. You may obtain
;;;; a copy of the License at
;;;;
;;;; http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied. See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :gp-utils
  (:use :cl :lw :capi :gp)
  (:export

   ;; shape functions
   #:draw-round-rect

   ;; string functions
   #:draw-ellipsized-string
   #:draw-centered-string
   #:draw-right-justified-string

   ;; image functions
   #:draw-fitted-image
   ))

(in-package :gp-utils)

(defun test (port &rest bounds)
  (declare (ignore bounds))
  (draw-round-rect port 10 10 100 100 18 :filled t :foreground :red))

(defun draw-round-rect (port x1 y1 w h r &rest gp-args &key filled &allow-other-keys)
  "Draw a rectangle with the corners rounded."
  (let* ((d (+ r r))

         ;; line segments
         (left (list :line 0 (- h r)))
         (bottom (list :line (- w r) h))
         (right (list :line w r))
         (top (list :line r 0))

         ;; corner arcs
         (top-left (list :arc 0 0 d d fpi-by-2 fpi-by-2 t))
         (bottom-left (list :arc 0 (- h d) d d fpi fpi-by-2 t))
         (bottom-right (list :arc (- w d) (- h d) d d (+ fpi fpi-by-2) fpi-by-2 t))
         (top-right (list :arc (- w d) 0 d d 0 fpi-by-2 t))

         ;; path moves in counter-clockwise direction
         (path (if (not filled)
                   (list top-left left bottom-left bottom bottom-right right top-right top)
                 (list top-left left bottom-left bottom bottom-right right top-right top left bottom right top))))
    
    ;; draw the path counter-clockwise (since arcs move with a positive sweep)
    (apply #'draw-path port path x1 y1 :closed t gp-args)))

(defun draw-ellipsized-string (port text x1 y x2 &rest gp-args &key (start 0) font &allow-other-keys)
  "Draw a string, but truncate with ellipsis if too long or a newline."
  (loop with ew = (get-char-width port #\u+2026 font)
        
        ;; loop over each character and get their width
        for i from start below (length text)
        for c = (char text i)
        for cw = (get-char-width port c font)

        ;; accumulate the widths
        sum cw into w

        ;; stop when too long or end of line
        when (or (char= c #\newline) (> (+ x1 w ew) x2))
        return (progn
                 (apply #'draw-string port text x1 y :start start :end i gp-args)
                 (apply #'draw-character port #\u+2026 (- (+ x1 w) cw) y gp-args))

        ;; string is long enough to fit, just draw the whole thing
        finally (apply #'draw-string port text x1 y gp-args)))

(defun draw-centered-string (port text x y &rest gp-args &key (h-center-p t) v-center-p font &allow-other-keys)
  "Draw a string centered"
  (multiple-value-bind (left top right bottom)
      (get-string-extent port text font)
    (let ((pos-x (- x (if h-center-p (/ (- right left) 2) 0)))
          (pos-y (- y (if v-center-p (/ (- bottom top) 2) 0))))
      (apply #'draw-string port text pos-x pos-y gp-args))))

(defun draw-right-justified-string (port text x y &rest gp-args &key (start 0) font &allow-other-keys)
  "Draw a string that is right-justified. Return the left-most x coordinate."
  (loop for i from start below (length text)
        for c = (char text i)

        ;; accumulate the total width before rendering
        sum (get-char-width port c font) into w

        ;; render the string and return the left-most edge
        finally (return (prog1
                            (- x w)
                          (apply #'draw-string port text (- x w) y gp-args)))))

(defun draw-fitted-image (port image to-x to-y &key to-width to-height (best-fit-p t))
  "Render an image in a bounded area of a port and maintain aspect ratio."
  (when image
    (let* ((w (or to-width (port-width port)))
           (h (or to-height (port-height port)))
           
           ;; get the image size
           (iw (image-width image))
           (ih (image-height image))
           
           ;; calculate the aspect ratio to use
           (aspect (funcall (if best-fit-p #'min #'max)
                            
                            ;; bound the image to the area
                            (min (/ w iw) 1.0)
                            (max (/ h ih) 1.0)))
           
           ;; figure out the final width and height
           (to-w (* iw aspect))
           (to-h (* ih aspect))
           
           ;; render to the middle of the bounded area
           (x (- (+ (/ to-width 2) to-x) (/ to-w 2)))
           (y (- (+ (/ to-height 2) to-y) (/ to-h 2))))
      
      ;; blit the image
      (draw-image port image x y :from-width iw :from-height ih :to-width to-w :to-height to-h))))
