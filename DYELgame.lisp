;;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;;; Name: Jordan Dumlao                     Date:03/19/2016
;;;; Course: ICS313        Assignment:#5
;;;; This code has been taken from Chapter 5 of  Land of Lisp by Conrad Barski
(defparameter +ID+ "Jordan Dumlao")
(defun ID ()
  +ID+)

;;create global variable *nodes*
;;contains list and description of all locations
(defparameter *nodes* '((entrance (you are at the gym entrance. a hot babe is at working at the front desk. ))
                        (lockers(you are in the locker rooms. you take a glance at yourself in the mirror. looking swoll bro.))
                        (pools (you are at the swimming pools. there are some fine ladies tanning. but you cannot get any gains here.))
                        (cardio (you are in the cardio room. you see a fat sweaty man running on the treadmill. you cannot get gains doing cardio))
                        (weight(you are in the free weight rooms. there is where you get your bicep curls in for them swoll arms.))
                        (platform(you are in the platform room. squats and deadlifts are done here. you hear the loud noise of people dropping heavy weights on the ground))
			(office (this is the bosses office))
			(private (this is the bosses private lifting room. you see him sitting on the chair doing some 10 lb bicep curls. weak! do he even lift? challenge this myrin to a lift off!))
))

;;find the correct item in the list of locations
(defun describe-location (location nodes)
  (cadr (assoc location nodes))) ;Uses assoc to find the proper location

;;contains the path from each location to another location
(defparameter *edges* '((entrance (cardio west door) (weight north door)(lockers east door))
                        (lockers (pools east door) (entrance west door))
                        (cardio (entrance east door))
                        (pools (lockers west door))
			(weight(platform east door)(benches west door)(office upstairs door))
		         (platform (weight west door))
			  (benches(weight east door))
			(office (weight downstairs door) (private north door))

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
                                   (protein-shake lockers)
                                   (protein-shake weight)
                                   (protein-shake platform)
                                   (protein-shake benches)
                                   (joggers lockers)
                                   (tank-top lockers)
                                   (shoes cardio)
                                   (gloves weight)
                                   (squat-belt platform)
                                   (wrist-straps benches)
                                   (key office)
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

;;picks up desired object
(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*)) ;must be a member of that location
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
(defparameter *allowed-commands* '(look walk pickup inventory weld dunk assemble help h location object edge))

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
(defmacro game-action (command subj place &body body)
  `(progn (defun ,command (subject object)
            (if (and (eq *location* ',place)
                     (eq subject ',subj)
                     (have ',subj))
                ,@body
            '(i cant ,command like that.)))
          (pushnew ',command *allowed-commands*)))

(defparameter *door-unlocked* nil)

;;;wizard special actions section
;;game-action macro to weld chain and bucket
;;check if have bucket and chain isnt welded
;;set chain-welded to true and tell user success
(game-action use key office
             (if (and (have 'key) (not *door-unlocked*))
                 (progn (setf *door-unlocked* 't)
                        '(you have unlocked the room in the office))
               '(you need the key)))

(defparameter *bucket-filled* nil)

;;game-action macro to dunk bucket into well
;;need chain to be welded
(game-action dunk  well garden
             (if *chain-welded* 
                 (progn (setf *bucket-filled* 't)
                        '(the bucket is now full of water))
               '(the water level is too low to reach.)))

;;macro to splash water onto wizard
;;must have bucket filled with water
;;game will win or lose depending on 
;;if have frog
(game-action splash bucket living-room
             (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
                   ((have 'frog) '(the wizard awakens and sees that you stole his frog. 
                                   he is so upset he banishes you to the 
                                   netherworlds- you lose! the end.))
                   (t '(the wizard awakens from his slumber and greets you warmly. 
                        he hands you the magic low-carb donut- you win! the end.))))


(defparameter *swole-bro* nil)
;;checks if user has all limbs
(defun has-limbs()
  (if(and (have 'towel) (have 'joggers) (have 'tank-top) (have 'showes) (have 'wrist-straps) (have 'gloves) (have 'squat-belt)) 't
    nil))
;;if user has all limbs and exodia is not assembled
;;assemble Exodia
(game-action assemble exodia attic
  (if (and (has-limbs)(not *exodia-assembled*)) 
                 (progn (setf *exodia-assembled* 't)
                        '(you have assembled exodia the forbidden one! you can now obliterate your opponents))
               '(exodia has already been assembled.)))
            
(defun help()
  (princ "Available commands:  
walk - walk to a direction 
look - look at current location 
pickup - pick up items in that location 
inventory - look at items you have 
weld - weld the chain and bucket together. you must have both items 
dunk - put water into the welded bucket. bucket must be welded 
splash - splash water onto the wizard. must have water int the welded bucket 
assemble - assemble something awesome. must have the correct pieces")
(princ""))

(defun h()
  (princ "Available commands:  
walk - walk to a direction 
look - look at current location 
pickup - pick up items in that location 
inventory - look at items you have 
weld - weld the chain and bucket together. you must have both items 
dunk - put water into the welded bucket. bucket must be welded 
splash - splash water onto the wizard. must have water int the welded bucket 
assemble - assemble something awesome. must have the correct pieces")
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
