;;;; ===================================================
;;;; DYELgame.cl
;;;; ===================================================
;;;  do you have what it takes to get the GAINZ?
;;;
;;;
;;; by Jerrin Lawi-an and Jordan Dumlao

(defparameter +AUTHORS+ "LITSQUAD")


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
		(format t "The bro-oss strikes you down. You die. Try again maybe."))
	(when (boss-dead)
		(format t "You defeat Wheydolf! You win!")))


 ;; the battle cycle entered.
(defun battle-loop ()
  (unless (or (player-dead)(boss-dead))
		(show-player)
		(unless (boss-dead)
			(show-boss)
			(player-attack))
		(fresh-line)
   (unless (boss-dead) 
     (boss-attack))
    (battle-loop)))
 
;; player actions in combat
(defun player-flex () 
	(fresh-line)
	(princ "You flex mightily! Your strength grows!")
	(setf *player-damage* (+ 6 *player-damage*)))

(defun player-attack ()
	(fresh-line)
  (princ "What do you do? [f]lex [a]ttack [i]nsult")
  (fresh-line)
	(case (read)
		(f (player-flex))
		(a (let ((x (randval (truncate (/ *player-damage* 3)))))
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
  (let  ((x (randval (truncate (/ *boss-damage* 3)))))
                (fresh-line)
		(princ "Wheydolf fires up and strikes you for ")
		(princ x)
    (princ " damage!")
    (fresh-line)
   (decf *player-health* x)))

(defun boss-dead ()
  (<= *boss-health* 0))

(defun boss-hit (x)
  (decf *boss-health* x)
	(if (boss-dead)
     (progn (princ "Wheydolf stumbles! ")
       (princ "He takes ")
       (princ x)
       (princ " damage and EXPLODES EVERYWHERE!!!!!"))
		(progn (princ "You strike Wheydolf for ")
				(princ x)
				(princ " damage!"))))
	

		
 

 
;;; various helper functions


	
(defun init-boss ()
	(setf *boss-health* 50)
	(setf *boss-damage* 20))

(defun init-player ()
	(setf *player-health* 50)
	(setf *player-damage* 20))
  
(defun player-dead ()
	(<= *player-health* 0))	
	
(defun show-player ()
	(fresh-line)
	(princ "You are the swolest around with ")
	(princ *player-health*) 
	(princ " health."))

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

