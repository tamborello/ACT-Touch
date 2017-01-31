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

(clear-all)

(define-model act-touch-demo

  (sgp :v t :trace-detail high)

  (add-dm
   (goal op start))

  (goal-focus goal)

(p move-hand
   =goal>
        op start
   =visual-location>
	color red
   ?manual>
	state free
==>
   +manual>
	cmd move-hand-touch
        loc =visual-location
   =goal>
	op tap)


(p do-tap
   =goal>
        op tap
   ?manual>
	preparation free
==>
  +manual>
	cmd tap
        hand right
        finger index
  =goal>
	op tapped)

(p do-tap-hold
   =goal>
	op tapped
   ?manual>
	preparation free
==>
   =goal>
	op tap-holding
   +manual>
	cmd tap-hold
        hand right
        finger index)

(p do-tap-release
!eval! (zerop (index-z (current-device)))
   =goal>
	op tap-holding
   ?manual>
	preparation free
==>
   =goal>
   	op tap-drag-release
   +manual>
	cmd tap-release
        hand right
        finger index)

;; Here's an example of a composed movement: the tap-hold, drag, & release
(p do-tapping-drag-release
   =goal>
	op tap-drag-release
   ?manual>
	preparation free
   ?imaginal>
	buffer empty
==>
   =goal>
   	op tapping-drag-release
   +manual>
	cmd tap-hold
        hand right
        finger index
   +imaginal>
	screen-x 550
        screen-y 400)

(p do-tapped-dragging-release
!eval! (zerop (index-z (current-device)))
   =goal>
	op tapping-drag-release
   ?manual>
	preparation free
   =imaginal>
	screen-x =x
        screen-y =y
==>
   =goal>
	op tapped-dragging-release
   +manual>
	cmd move-hand-touch
        loc =imaginal)

(p do-tapped-dragged-releasing
!eval! (zerop (index-z (current-device)))
   =goal>
   	op tapped-dragging-release
   ?manual>
	preparation free
==>
   =goal>
	op swipe
   +manual>
	cmd tap-release
        hand right
        finger index)

(p do-swipe
   =goal>
   	op swipe
   ?manual>
	preparation free
==>
   =goal>
	op pinch
   +manual>
	cmd swipe
        hand right
        finger index
        r 50
        theta 0
        num-fngrs 2)

(p do-pinch
   =goal>
	op pinch
   ?manual>
	preparation free
==>
   =goal>
	op rotate
   +manual>
	cmd pinch
        hand right
        finger index
        start-width 20
        end-width 50)

(p do-rotate
   =goal>
	op rotate
   ?manual>
	preparation free
==>
   =goal>
	op rotated
   +manual>
	cmd rotate
        hand right
        finger index
        rotation 1)
  )

