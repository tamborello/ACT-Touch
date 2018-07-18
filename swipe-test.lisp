;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Frank Tamborello
;;; Copyright   : (c) 2012 Cogscent, LLC
;;; Availability: GNU LGPL, see LGPL.txt
;;; Address     : Cogscent, LLC
;;; 		: PMB 7431
;;;		: 2711 Centerville Rd, Ste 120
;;;		: Wilmington DE, USA 19808-1676
;;;		: frank.tamborello@cogscent.com
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser General Public
;;; License: the GNU Lesser General Public License as published by the
;;; Free Software Foundation (either version 2.1 of the License, 
;;; or, at your option, any later version),
;;; and the Franz, Inc Lisp-specific preamble.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the Lisp Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;; and see Franz, Inc.'s preamble to the GNU Lesser General Public License,
;;; http://opensource.franz.com/preamble.html.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Acknowledgements
;;;		: This research is sponsored by Measurement Science and 
;;;		Engineering grant 60NANB12D134 from the 
;;;		National Institute of Standards and Technology (NIST).
;;;		Special acknowledgements are due to Dr. Ross Micheals and 
;;;		Dr. Kristen K. Greene of NIST's Information Technology 
;;;		Laboratory.
;;;		Thanks also to Dr. Michael D. Byrne, upon whose experiment 
;;;		library code I based the device code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : act-touch-demo-model.lisp
;;; Revision    : 1
;;; 
;;; Description : A quickie demonstration of ACT-Touch
;;;
;;; Usage	: Place in ACT-R folder "User Loads." This file will load
;;;		automatically after ACT-R loads.
;;; 
;;; Bugs        : None known
;;;
;;; To do       : Nothing
;;;
;;; ----- History -----
;;; 2012.09.29 fpt 1
;;;		: Inception: Forked from act-touch.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; ---------------------------------------------------------------------- ;;;;
;;;;   A Demonstration Model
;;;; ---------------------------------------------------------------------- ;;;;

(defgeneric device-handle-swipe (device loc &rest params)
  (:documentation "Handle the swipe gesture."))

(defmethod device-handle-swipe (device (loc vector) &rest params)
  (model-output "Model swiped device ~A at ~A with ~A ." device loc params))


(clear-all)

(define-model act-touch-demo

  (sgp :v t :trace-detail high)

  (add-dm
   (goal op start))

  (goal-focus goal)


  (p do-swipe-init
     =goal>
        op start
     ?manual>
        state free
     ==>
     =goal>
        op swipe1
     +manual>
     cmd swipe
     hand right
     finger index
     r 50
     theta 0
     num-fngrs 2)

  ;; 0.999 s between this swipe and init swipe
  (p do-swipe1
     =goal>
     op swipe1
     ?manual>
     state free
     ==>
     =goal>
        op swipe2
     +manual>
        cmd swipe
	hand right
	finger index
	r 500
	theta 0
	num-fngrs 2
	swipe-speed 1)

  ;; 1.298 s between this swipe and swipe1
  (p do-swipe2
     =goal>
        op swipe2
     ?manual>
        state free
     ==>
     =goal>
        op swipe3
     +manual>
        cmd swipe
        hand right
	finger index
	r 500
	theta 0
	num-fngrs 2
	swipe-speed 5)

  ;; 1.794 s between this swipe and swipe2
  (p do-swipe3
     =goal>
        op swipe3
     ?manual>
        state free
     ==>
     =goal>
        op stop
     +manual>
        cmd swipe
        hand right
	finger index
	r 5000
	theta 0
	num-fngrs 2)

  )

#|
Check: computer-exec-time & compute-finish-time, both called from perform-movement

;; This should take distance (r) into consideration, so long swipes should take longer than short swipes.
(defmethod compute-exec-time ((mtr-mod motor-module) (self ply))
  (+ (init-time mtr-mod)
     (randomize-time (fitts mtr-mod (fitts-coeff self) (r self)
                       (target-width self)))))


|#

;; compute a swipe execution time
(let ((mvmt (make-instance 'swipe :hand (right-hand (get-module :motor)) :finger 'index :r 5000 :theta 0)))
  (compute-exec-time (get-module :motor) mvmt)
  (terpri)
  (format t "Move-time: ~a~%" (move-time mvmt)))

;; for 50, 500, & 5000

Move-time: 0.4243659
 Toplevel Forms...

Move-time: 0.672542
 Toplevel Forms...

Move-time: 0.9215893




;; compute a swipe finish time
(compute-finish-time (get-module :motor)
		     )

(let ((r 50))
  (fitts
   (get-module :motor)
   (fitts-coeff (make-instance 'swipe :hand (right-hand (get-module :motor)) :finger 'index :r r :theta 0))
   r
   50))



;; Model execution trace
? (run 15)
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL REQUESTED NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED DO-SWIPE-INIT
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED DO-SWIPE-INIT
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  SWIPE HAND RIGHT FINGER INDEX R 50 THETA 0 NUM-FNGRS 2
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  PREPARATION-COMPLETE
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.400   MOTOR                  INITIATION-COMPLETE
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.824   MOTOR                  DEVICE-HANDLE-SWIPE NIL #(57 4) 2
Model swiped device NIL at #(57 4) with (2) .
     0.824   PROCEDURAL             CONFLICT-RESOLUTION
     1.299   MOTOR                  FINISH-MOVEMENT
     1.299   PROCEDURAL             CONFLICT-RESOLUTION
     1.299   PROCEDURAL             PRODUCTION-SELECTED DO-SWIPE1
     1.299   PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.299   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.349   PROCEDURAL             PRODUCTION-FIRED DO-SWIPE1
     1.349   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.349   PROCEDURAL             MODULE-REQUEST MANUAL
     1.349   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.349   MOTOR                  SWIPE HAND RIGHT FINGER INDEX R 50 THETA 0 NUM-FNGRS 2
     1.349   MOTOR                  PREPARATION-COMPLETE
     1.349   PROCEDURAL             CONFLICT-RESOLUTION
     1.399   MOTOR                  INITIATION-COMPLETE
     1.399   PROCEDURAL             CONFLICT-RESOLUTION
     1.823   MOTOR                  DEVICE-HANDLE-SWIPE NIL #(57 4) 2
Model swiped device NIL at #(57 4) with (2) .
     1.823   PROCEDURAL             CONFLICT-RESOLUTION
     2.298   MOTOR                  FINISH-MOVEMENT
     2.298   PROCEDURAL             CONFLICT-RESOLUTION
     2.298   PROCEDURAL             PRODUCTION-SELECTED DO-SWIPE2
     2.298   PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.298   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     2.348   PROCEDURAL             PRODUCTION-FIRED DO-SWIPE2
     2.348   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.348   PROCEDURAL             MODULE-REQUEST MANUAL
     2.348   PROCEDURAL             CLEAR-BUFFER MANUAL
     2.348   MOTOR                  SWIPE HAND RIGHT FINGER INDEX R 500 THETA 0 NUM-FNGRS 2
     2.348   PROCEDURAL             CONFLICT-RESOLUTION
     2.398   MOTOR                  PREPARATION-COMPLETE
     2.398   PROCEDURAL             CONFLICT-RESOLUTION
     2.448   MOTOR                  INITIATION-COMPLETE
     2.448   PROCEDURAL             CONFLICT-RESOLUTION
     3.121   MOTOR                  DEVICE-HANDLE-SWIPE NIL #(507 4) 2
Model swiped device NIL at #(507 4) with (2) .
     3.121   PROCEDURAL             CONFLICT-RESOLUTION
     3.843   MOTOR                  FINISH-MOVEMENT
     3.843   PROCEDURAL             CONFLICT-RESOLUTION
     3.843   PROCEDURAL             PRODUCTION-SELECTED DO-SWIPE3
     3.843   PROCEDURAL             BUFFER-READ-ACTION GOAL
     3.843   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     3.893   PROCEDURAL             PRODUCTION-FIRED DO-SWIPE3
     3.893   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     3.893   PROCEDURAL             MODULE-REQUEST MANUAL
     3.893   PROCEDURAL             CLEAR-BUFFER MANUAL
     3.893   MOTOR                  SWIPE HAND RIGHT FINGER INDEX R 5000 THETA 0 NUM-FNGRS 2
     3.893   PROCEDURAL             CONFLICT-RESOLUTION
     3.943   MOTOR                  PREPARATION-COMPLETE
     3.943   PROCEDURAL             CONFLICT-RESOLUTION
     3.993   MOTOR                  INITIATION-COMPLETE
     3.993   PROCEDURAL             CONFLICT-RESOLUTION
     4.915   MOTOR                  DEVICE-HANDLE-SWIPE NIL #(5007 4) 2
Model swiped device NIL at #(5007 4) with (2) .
     4.915   PROCEDURAL             CONFLICT-RESOLUTION
     5.886   MOTOR                  FINISH-MOVEMENT
     5.886   PROCEDURAL             CONFLICT-RESOLUTION
     5.886   ------                 Stopped because no events left to process
5.886
75
NIL




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2018.02.06
;;; Implement a new feature to allow the model to "consciously" control swiping
;;; speed: an integer from the set [1,5], where 1 is slowest and 5 is fastest,
;;; and 3 is default.
;;; I lack a principled way to map user intention of swiping speed to
;;; computing its execution time, so I'll just do what's expedient and otherwise
;;; seems at least not stupidly unreasonable: multiply fitts' r parameter in
;;; compute-exec-time by the product of 1/swipe-speed and (1+ (act-r-noise .2)).


(let ((mvmt
       (make-instance
	'swipe
	:hand (right-hand (get-module :motor))
	:finger 'index
	:r 5000
	:theta 0
	:swipe-speed 3)))
  (compute-exec-time (get-module :motor) mvmt)
  (terpri)
  (format t "Move-time: ~a~%" (move-time mvmt)))
