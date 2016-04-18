;;;; ===================================================
;;;; DYELgame.cl
;;;; ===================================================
;;;  do you have what it takes to get the GAINZ?
;;;
;;;
;;; by Jerrin Lawi-an and Jordan Dumlao

(defparameter +AUTHORS+ "Jerrin Lawi-an & Jordan Dumlao")


;;; battle system and stats and stuff
(defparameter *player-spec* nil)
(defparameter *specs* '(benching squatting deadlifting))


(defun fight ()
	(boss-battle))

(defparameter *in-battle* nil)
(defparameter *player-health* nil)
(defparameter *player-damage* nil)


(defparameter *boss-health* nil)
(defparameter *boss-damage* nil)


;; Calls functions which set player and boss stats,
;; then proceeds to the battle functions.
(defun boss-battle ()
	(init-boss)
	(init-player)
	(fresh-line)
	(princ "You engage Wheydolf Hitler!")
	(battle-loop)
	(when (player-dead)
		(princ "You don't even lift, bro. Try again."))
	(when (boss-dead)
		(princ "So easy brah. I can curl his max.")))


 ;; the battle cycle entered.
(defun battle-loop ()
	(unless (or (player-dead)(boss-dead))
		(show-player)
		(unless (boss-dead)
			(show-boss)
			(player-attack))
		(fresh-line)
		(boss-attack)
		(battle-loop)))
 
;; player actions in combat
(defun player-flex () 
	(fresh-line)
	(princ "You flex mightily! Your strength grows!")
	(setf *player-damage* (+ 1 *player-damage*)))

(defun player-attack ()
	(fresh-line)
	(princ "What do you do? [f]lex [a]ttack [i]nsult")
	(case (read)
		(f (flex))
		(a (let ((x (randval (truncate (/ *player-damage* 6)))))
			(princ "You run up and swing for ")
			(princ x)
			(fresh-line)
			(boss-hit x)))
		(otherwise (fresh-line)
			(princ "Unsure of what to do, you resort to drastic measures.")
			(fresh-line)
             (princ "You proudly shout at him: ")
             (princ "DO YOU EVEN LIFT BRO?")
                        (fresh-line)
			(boss-hit 1))))


;; boss actions
(defun boss-attack ()
  (let ((x  (randval *boss-damage*)))
                (fresh-line)
		(princ "Wheydolf fires up and strikes you for ")
		(princ x)
    (princ "damage!")
    (fresh-line)
   (decf *player-health* x)))

(defun boss-dead ()
  (<= *boss-health* 0))

(defun boss-hit (x)
  (decf *boss-health* x)
	(if (boss-dead)
		(progn (princ "You defeated Wheydolf Hitler!")
				(princ "You really do lift bro."))
		(progn (princ "You strike Wheydolf for ")
				(princ x)
				(princ " damage!"))))
	

		
 

 
;;; various helper functions


	
(defun init-boss ()
	(setf *boss-health* 50)
	(setf *boss-damage* 30))

(defun init-player ()
	(setf *player-health* 50)
	(setf *player-damage* 30)
	(setf *player-inspired* 0))
  
(defun player-dead ()
	(<= *player-health* 0))	
	
(defun show-player ()
	(fresh-line)
	(princ "You are the swolest around with ")
	(princ *player-health*) 
	(princ " health.)
  (princ *player-spec*))

(defun show-boss ()
  (fresh-line)
  (princ "The boss is str8 flexin on you. He has ")
  (princ *boss-health*)
  (princ " health."))

(defparameter *player-damaged* nil)
(defun player-hurt ()
  (if (<= *player-health* 15)
      (setf *player-damaged* 't)))
  
;; Generates a random value from 1 to n. From book.
(defun randval (n)
  (1+ (random (max 1 n))))

