;;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;;; Name: Jordan Dumlao, Jerrin Lawi-an                    Date:04/18/2016
;;;; Course: ICS313        Assignment:#6



;;create global variable *nodes*
;;contains list and description of all locations
(defparameter *nodes* '((entrance (you are at the gym entrance. a hot babe is at working at the front desk. ))
                        (lockers(you are in the locker rooms. you take a glance at yourself in the mirror. looking swoll bro.))
                        (pools (you are at the swimming pools. there are some fine ladies tanning. but you cannot get any gains here.))
                        (cardio (you are in the cardio room. you see a fat sweaty man running on the treadmill. you cannot get gains doing cardio))
                        (weight(you are in the free weight rooms. there is where you get your bicep curls in for them swoll arms.))
                        (platform(you are in the platform room. squats and deadlifts are done here. you hear the loud noise of people dropping heavy weights on the ground))
			(office (this is the bosses office.))
			(private (this is the bosses private lifting room. you see the boss sitting on the chair doing some 10 lb bicep curls. weak! do he even lift? challenge this myrin to a lift off!))
))

;;find the correct item in the list of locations
(defun describe-location (location nodes)
  (cadr (assoc location nodes))) ;Uses assoc to find the proper location

;;contains the path from each location to another location
(defparameter *edges* '((entrance (cardio west door) (weight north door)(lockers east door))
                        (lockers (pools east door) (entrance west door))
                        (cardio (entrance east door))
                        (pools (lockers west door))
			(weight(platform east door)(benches west door)(office upstairs door)(entrance south door))
		         (platform (weight west door))
			  (benches(weight east door))
			(office (weight downstairs door) (private north door))
                        (private (office south door))

))

;;given the edge, describe the path
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.)) ;uses ` to swap between code and data mode

;;describes all paths for a given location
;;mapcar iterates through each object in the list and runs describe-path on it
;;append function is then applied to combine descriptions
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;;global variable containing all the objects in the world
(defparameter *objects* '(protein-shake towel joggers tank-top shoes gloves wrist-straps squat-belt key))

;;global variable containing the location of each object
(defparameter *object-locations* '((towel entrance)
                                   (protein-shake entrance)
                                   ;(protein-shake lockers)
                                   ;(protein-shake weight)
                                   ;(protein-shake platform)
                                   ;(protein-shake benches)
                                   (joggers lockers)
                                   (tank-top lockers)
                                   (shoes cardio)
                                   (gloves weight)
                                   (squat-belt platform)
                                   (wrist-straps benches)
                                 ))


;;returns filtered list of locations for which the
;;object exists in
;;defines at-loc-p using the labels command that
;;returns true if the obj exists in that location
;;remove-if-not removes all locations that the obj
;;does not exist in
(defun objects-at (loc objs obj-loc)
   (labels ((is-at (obj)
              (eq (cadr (assoc obj obj-loc)) loc)))
       (remove-if-not #'is-at objs)))

;;describes objects at a given location
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj) ;creates local function
             `(you see a ,obj in the room.))) ;returns text about the object	
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc))))) ;appends all existing objects at loc into a list

;;default location
(defparameter *location* 'entrance)

;;tells player current location, paths, and objects
(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

;;allows player to walk in a certain direction
(defun walk (direction)
  (labels ((correct-way (edge) ;creates local function that checks if direction is valid
             (eq (cadr edge) direction)))
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next 
          (progn (setf *location* (car next)) 
                 (look))
          '(you cannot go that way.)))))

(defparameter *strength* 0)
;;picks up desired object
(defun pickup (object)
  (cond ((and(eq object 'protein-shake) (member object (objects-at *location* *objects* *object-locations*)))
         (setf *strength* (+ *strength* 100)) (push (list object 'shakes) *object-locations*) `(you drank a protein shake your strength is now ,*strength*))
   ((member object (objects-at *location* *objects* *object-locations*)) ;must be a member of that location
         (push (list object 'body) *object-locations*) ;push onto list body the current object being picked up
         `(you are now carrying the ,object)) ;returns object that is being picked up
        (t '(you cannot get that.)))) ;if object is not there

;;displays inventory
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;;checks if have that object
(defun have (object) 
    (member object (cdr (inventory))))

;;capture the command that the player types
;;evals and prints then calls loop recursively
;;as long as quit is not called
(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))

;;reads a syntax expression
;;adds quotes around the read string
;;quotes any arguments the player has in a command
;;applies quote it to every argument in the player's command
(defun game-read ()
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;;allowed commands the user can choose from
(defparameter *allowed-commands* '(look walk pickup inventory  dunk assemble help h location object edge challenge fight))

;;checks if the first word is allowed in the commands
(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))
;;makes text look "nice" to user
;;makes first word a capital, all other words lower case etc.
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

;;Neatly prints the list that was tweaked by tweak-text
(defun game-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
    (fresh-line)
    (princ #\newline)) ;prints a new line for nicer text output

;;game action macro
;;takes params command, subject, object, place, and rest of body
;;checks to see if user has subject and object
;;is in current location
;;then runs rest of body macro according to command
(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (object)
            (if (and (eq *location* ',place)
                     (eq object ',obj))
                ,@body
            '(i cant ,command like that.)))
          (pushnew ',command *allowed-commands*)))

;;;special actions section

(defparameter *gym-bro* 't)
;;macro to challenge the boss
;;must have all items to challenge
(game-action challenge towel boss private
             (cond ((not *gym-bro*) '(you need to get all the items!))
                   (t (progn '(you challenge
d the boss)
                        (fight)))))


;;checks if user has all items
(defun gym-bro()
  (if(and (have 'towel) (have 'joggers) (have 'tank-top) (have 'gloves) (have 'squat-belt) (have 'wrist-straps) (have 'shoes)) 't
    nil))
            
(defun help()
  (princ "Available commands:  
walk - walk to a direction 
look - look at current location 
pickup - pick up items in that location 
inventory - look at items you have ")
(princ""))

(defun h()
  (princ "Available commands:  
walk - walk to a direction 
look - look at current location 
pickup - pick up items in that location 
inventory - look at items you have ")
(princ""))

;;adds new object to game
;;if obj exists, it will not be added
;;if location does not exist, it will not be added
;;else, the object will be added to objects, and object-locations 
(defmacro object (loc obj)
  `(progn (cond ((member ',obj *objects*) '(this item already exists.))
                ((not (member ',loc (flatten *nodes*))) '(this is not a valid location.))
                ('t (progn (pushnew ',obj *objects*)
                      (pushnew '(,obj ,loc) *object-locations*))))))

;;adds new location and description
;;if location already exists, it will not be added
;;else loc and desc will be added to *nodes*
(defmacro location (loc desc)
  `(progn (cond ((member ',loc (flatten *nodes*))'(this location already exists.))
                ('t (progn (pushnew '(,loc ,desc) *nodes*)(pushnew '(,loc) *edges*))))))

(defun opposite-direction(dir)
  (cond ((eq dir 'west) 'east)
        ((eq dir 'east) 'west)
        ((eq dir 'north) 'south)
        ((eq dir 'south) 'north)
        ((eq dir 'upstairs) 'downstairs)
        ((eq dir 'downstairs) 'upstairs)
        ('t 'void)))

;;check if loc1 is in list andcheck if loc2 is within loc1's list 
;;check if edge exists
;;check if a dir for loc1 exists
;;else the location2 with dir and entrance is pushed onto edges
(defmacro edge (loc1 loc2 dir entrance)
  `(progn (cond ((eq ',loc1 ',loc2) 
                 '(cannot connect it to itself))
                ((member ',loc2 (mapcar #'car (cdr(assoc ',loc1 *edges*)))) 
                 '(there is already an edge))
                ((member ',dir (mapcar #'car (mapcar #'cdr (cdr(assoc ',loc1 *edges*)))))
                 '(there is already a direction that way))
                ('t (progn (pushnew '(,loc2 ,dir ,entrance) (cdr(assoc ',loc1 *edges*))) (pushnew '(,loc1 ,(opposite-direction dir) ,entrance) (cdr(assoc ',loc2 *edges*))))))))

;;helper function to flatten lists
(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

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
   (progn (format t "You defeat Wheydolf! You win!"))))


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