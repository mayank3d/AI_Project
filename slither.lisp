
;; Program: Slither Link Game
;; Author: Mayank K Dadheech
;; UFID : 6980-5273
;; Artificial Intelligence Project
;; Final Submission

(eval-when (:compile-toplevel)
  (defparameter *code-optimization*
    '(optimize
      (speed 3)
      (safety 0)
      (space 0)
      (debug 0)
      (compilation-speed 0))
    "The code optimization parameters suggested to compiler .")
)

(proclaim '(optimize (speed 3) (space 0) (debug 0)))

(defun slither ()       ;;Main program routine
	(instruction)
	(loop while (y-or-n-p "Proceed with the Game?")
		do (if (y-or-n-p "Ready with the Game Board file name with path?")
            (progn
            (format t "Please enter the file name: ")
            (play-game (read-board (fetch-file)))
            )
			)
	)
)


(defun instruction()        ;;Printing Instructions
	(format t "
        Welcome to the Slyther Link.~&")
	(format t "
        Goal:      
        You need to make a perfect loop in the game board,
        while meeting the constraint that any grid has exactly
        same number of bars around it as the number mentioned in its cell.~&
         ")
	(format t "
        Instructions: 
        -Load the game board.
        -Decide your move.
        -Play your move in the form of a triplet as in N1 N2 L
        -N1 and N2 are numbers representing Row and Column
        -L can be t, b, l or r for top, bottom, left and right wall of cell 
        -While writing file name, don't use quotation marks
        -Writing quit at any time will quit the game
		-Writing solve at any time will send game to auto solver routine.
        ~&")
	(format t "
         All the Best~&")
)


(defun fetch-file ()            ;;Reads file from the user
	(loop for filename = (read-line *query-io*)
		until (probe-file filename)
		do (format t "Incorrect file name ")
		finally (return filename)
	)
)


(defun read-board (pathname)      ;;Reads the game board file, breaks it and parse it for printing and processing
	(parse-board (break-file pathname))
)


(defun break-file (filename)      ;;Breaks the file into list of line it reads
	(with-open-file (stream filename)
		(loop for line = (read-line stream nil nil)
		while line
		collecting line into lines
		finally (return lines)
    )
	)
)

(defun parse-board (strings)      ;;converting the string into a board
	(declare (type string strings))
	(let ((board (make-properly-sized-array (list (length (car strings)) (length strings)))))
		(loop :for row fixnum :from 0 :to (1- (array-dimension board 0))
			:for row-string = nil
			:when (oddp row) :do (setf row-string (elt strings (/ (1- row) 2)))
			:do (loop :for column fixnum :from 0 :to (1- (array-dimension board 1))
				:do (cond ((is-vertex? row column) 
                        (setf (aref board row column) #\+))
                        ((is-face? row column) 
                        (setf (aref board row column)
                            (if (parse-integer (string (elt row-string (/ (1- column) 2))) :junk-allowed t) 
                                (parse-integer (string (elt row-string (/ (1- column) 2))) :junk-allowed t)
                                #\Space))) 
                        ((is-line? row column) 
                    (setf (aref board row column) #\Space))))
	)
    board)
)


(defun play-game (board)            ;;original game playing routine
    (declare (type array board))
	(loop
        (terpri)
		(print-board board) 
		(loop for move = (read-move)
			do (when (string-equal "quit" (string-trim " " move))
               (format t "Quitting current game! ")
               (return-from play-game))
			do (when (string-equal "solve" (string-trim " " move))
				(format t "Board sent to solver... ")
                (solve-board board)
				(return-from play-game))
			do (setf move (parse-move move board))
				until move
				finally (progn
                (place-move move board)))
		(if (check-board board)
			(progn
            (format t "Congratulations, You Won!~&")
            (print-board board) 
            (return))
        )
    )
)


(defun read-move ()             ;;to query the user about the next move
	(format *query-io* "Enter your move in the form of a triplet: ")
	(read-line *query-io*)
)


(defun parse-move (string board)    ;;to parse user move and apply to board
	(declare (type array board) (type string string))
	(setq t1 (subseq string 0 1))
	(setq t2 (subseq string 2 3))
	(setq t3 (subseq string 4 5))
	(setq tlist (list t1 t2 t3))
	(let ((possible-move tlist))
    (declare (type list possible-move))
    (if (= 3 (length possible-move))
        (if (AND (if (numberp (parse-integer (car possible-move) :junk-allowed t))
                     (allowable-board-x (parse-integer (car possible-move)) board)
                     nil)
                 (if (numberp (parse-integer (cadr possible-move) :junk-allowed t))
                     (allowable-board-y (parse-integer (cadr possible-move)) board)
                     nil)
                 (if (= 1 (length (caddr possible-move)))
                     (valid-move (character (caddr possible-move)))
            nil))
            
			(progn
              (list (parse-integer (car possible-move))
                    (parse-integer (cadr possible-move))
                    (character (caddr possible-move))))
            nil)
    nil))
)


(defun valid-move (char)        ;; to make sure user only enters from given options
	(declare (type character char))
	(case char
    (#\t t)
    (#\b t)
    (#\l t)
    (#\r t))
)


(defun place-move (triplet board)       ;;applying user's move on the game board
	(declare (type array board))
	(let ((herex (1+ (* 2 (1- (car triplet)))))
		(herey (1+ (* 2 (1- (cadr triplet)))))
        (move (char-downcase (caddr triplet))))
    (cond ((char= #\t move)
			(if (char= (aref board (1- herex) herey) #\Space)
               (setf (aref board (1- herex) herey) #\-)
               (setf (aref board (1- herex) herey) #\Space)))
			((char= #\b move)
			(if (char= (aref board (1+ herex) herey) #\Space)
               (setf (aref board (1+ herex) herey) #\-)
               (setf (aref board (1+ herex) herey) #\Space)))
			((char= #\l move)
			(if (char= (aref board herex (1- herey)) #\Space)
               (setf (aref board herex (1- herey)) #\|)
               (setf (aref board herex (1- herey)) #\Space)))
			((char= #\r move)
			(if (char= (aref board herex (1+ herey)) #\Space)
               (setf (aref board herex (1+ herey)) #\|)
               (setf (aref board herex (1+ herey)) #\Space)))
    )
	)
)


(defun is-vertex? (x y)
	(declare (type fixnum x y))
	(and (evenp x) (evenp y))
)
(defun is-face? (x y)
	(declare (type fixnum x y))
	(and (oddp x) (oddp y))
)
(defun is-line? (x y)
	(declare (type fixnum x y))
	(oddp (+ x y))
)


(defun make-properly-sized-array (dimensions)
	(let ((x (proper-size (cadr dimensions)))
		(y (proper-size (car dimensions))))
    (make-array (list x y) :initial-element #\Space))
)

(defun proper-size (dimension)
  (1+ (* 2 dimension))
)


(defun print-board (board)
	(declare (type array board))
	(format t "~&") 
	(loop :for row :from 0 :to (array-dimension board 0)
		:do (loop :for column :from 0 :to (array-dimension board 1)
            :do (cond
                ((= row 0) 
                (cond ((= 0 column)
                (format t "     "))
				((oddp column) 
                (format t " "))
                ((evenp column)
                (format t "~3D  " (/ column 2)))))
                ((= column 0) 
                (cond ((oddp row) (format t "     "))
                ((evenp row) (format t "~3D  " (/ row 2)))))
                ((AND (> row 0) (> column 0)) 
                (format t "~3A" (aref board (1- row) (1- column))))))
			:do (format t "~2&")
	)
)

(defun allowable-board-x (x board)
	(declare (type fixnum x) (type array board))
	(if (AND (> x 0)
		(<= x (/ (1- (array-dimension board 0)) 2)))
		t
	nil)
)

(defun allowable-board-y (y board)
	(declare (type fixnum y) (type array board))
	(if (AND (> y 0)
		(<= y (/ (1- (array-dimension board 1)) 2)))
		t
	nil)
)

(defun check-board (view)
	(let ((x-limit (array-dimension view 0))
        (y-limit (array-dimension view 1))
        (x 1)
        (y 1)
        (return-val T))
    (declare (type fixnum x y x-limit y-limit))
		(loop
			(loop
			(if (not (check-space view x y) )
				(setf return-val nil)
			)
				(incf x 2)
				(when (> x (1- x-limit))
				(progn
				(setf x 1)
				(return)))
			)
			
			(incf y 2)
			(when (> y (1- y-limit))
			(progn
			(setf y 1)
			(return)))
		)
    
		(setf x 0)
		(setf y 0)
		(loop
			(loop
			(if (not (check-node view x y x-limit y-limit))
            (setf return-val nil)
			)
			(incf x 2)
			(when (> x (1- x-limit))
            (progn
            (setf x 0)
            (return))))
       
			(incf y 2)
			(when (> y (1- y-limit))
			(progn
			(setf y 0)
			(return)))
		)
		return-val)
)


(defun check-space (view x y)
	(let ((goal (aref view x y)))
		(if (numberp goal)
        (progn
        (if (= (num-of-edges view x y) goal) T nil))
        T)
	)
)



(defconstant +goal+ (the fixnum 0) 		"No edges lead away")
(defconstant +goal-one+ (the fixnum 1)  "Exactly one edge leads away")
(defconstant +goal-two+ (the fixnum 2)  "Exactly two edges lead away")


(defun check-node (board x y x-limit y-limit)
	(declare (type fixnum x y) (type array board))
	"Return true if there is an out-degree of 0 or 2. This means that the node is valid."
	(let (
        (current 0))
		(declare (type fixnum x-limit y-limit current))
		(if (not (< (1- x) 0))
			(if (is-line board (the fixnum (1- x)) y )
            (incf current))
		)
		(if (not (< (1- y) 0))
			(if (is-line board x (the fixnum (1- y) ) )
            (incf current))
		)
		(if (not (> (1+ x) (1- x-limit)))
			(if (is-line board (the fixnum (1+ x)) y)
            (incf current))
		)
		(if (not (> (1+ y) (1- y-limit)))
			(if (is-line board x (the fixnum (1+ y)))
            (incf current))
		)
		(if (or (= current +goal+) (= current +goal-two+) ) T nil)
	)
)


(defun is-line (view x y)
	(declare (type array view) (type fixnum x y))
	"Pass in the position in the array
	If it is a line, return a 1"
	(let ((char (aref view x y)))
    (declare (type character char))
    (case char
      (#\| t)
      (#\- t)
      (#\Space nil))
	)
)

(defun print-moves (moves)
	(format t "Your moves in order were:")
	(loop for move in moves
    :do (format t "~&   ~A~&" move))
)
	 
;;###############################################################################################################
;;################################################################################################################

;;Mayank K Dadheech
;;Post-midterm work
;;Final solver code below
;;

(defun solve-board (board)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (array board))
	
	(format t "Your board is being solved with AI, please wait... ")

	(let((start-time (get-universal-time))
		(end-time nil)
		(moves nil))
		(apply-algo (apply-heuristics board) moves)
		(setf end-time (get-universal-time))
		(format t "The solution found is : ~&")
		(print-board board)
		;(print-moves (nreverse moves))
		(format t "Time taken by CPU to solve : ~f ~&" (- end-time start-time))
	)
)

  
	;;apply first-level heuristic to detect cells with no possibility of having an edge
(defun apply-heuristics (board)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (type array board))
	
	(loop for x fixnum :from 1 :to (1- (array-dimension board 0)) by 2
		:do (loop :for y fixnum from 1 to (1- (array-dimension board 1)) by 2
			do (let ((cell-value (aref board x y)))
				(if (numberp cell-value)
				(case cell-value
				(0 (heuristic-zero  board x y))
				(1 (heuristic-one   board x y))
				(2 (heuristic-two   board x y))
				(3 (heuristic-three board x y))
				)
				)
			)
		)
	)
		(apply-more-rules (apply-more-rules (apply-more-rules board)))
;board
)

(defun apply-more-rules (board)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (type array board))
	
	(loop for x fixnum :from 1 :to (1- (array-dimension board 0)) by 2
		:do (loop :for y fixnum from 1 to (1- (array-dimension board 1)) by 2
			do (let ((cell-value (aref board x y)))
				(if (numberp cell-value)
				(case cell-value
				(0 (zero-rules  board x y))
				(1 (one-rules   board x y))
				(2 (two-rules   board x y))
				(3 (three-rules board x y))
				)
				)
			)
		)
	)
	
	(edge-rules board)
;board
)

(defun one-rules (board x y)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (array board) (fixnum x y))
	
	;;if we have three x then mark remaining edge as a link 
	(let ((temp nil))
	(loop for value in (list 	(is-cross board x (1+ y) )
								(is-cross board x (1- y) )
								(is-cross board (1+ x) y )
								(is-cross board (1- x) y ))
	
     with temp fixnum = 0
     when value do (setf temp (the fixnum (1+ temp)))
     finally (progn
				(if (= temp 3)
					(progn
					(if (is-space board x (+ y 1))
						(setf (aref board x (+ y 1)) #\|)
					)
					(if (is-space board x (- y 1))
						(setf (aref board x (- y 1)) #\|)
					)
					(if (is-space board (- x 1) y)
						(setf (aref board (- x 1) y) #\-)
					)
					(if (is-space board (+ x 1) y)
						(setf (aref board (+ x 1) y) #\-)
					))
				)
			 )
	
	)
	)
	
	;;if we have one edge then mark remaining three as x
	(let ((temp nil))
	(loop for value in (list 	(is-line board x (the fixnum (1+ y)) )
								(is-line board x (the fixnum (1- y)) )
								(is-line board (the fixnum (1+ x)) y )
								(is-line board (the fixnum (1- x)) y ))
	
     with temp fixnum = 0
     when value do (setf temp (the fixnum (1+ temp)))
     finally (progn
				(if (= temp 1)
					(progn
					(if (is-space board x (+ y 1))
						(setf (aref board x (+ y 1)) #\x)
					)
					(if (is-space board x (- y 1))
						(setf (aref board x (- y 1)) #\x)
					)
					(if (is-space board (- x 1) y)
						(setf (aref board (- x 1) y) #\x)
					)
					(if (is-space board (+ x 1) y)
						(setf (aref board (+ x 1) y) #\x)
					))
				)
			 )
	
	)
	)
	
	
		;;if any corner has two outgoing edges as crossed then mark links in rest two outgoing edges
;	(progn 
;	;;in up left corner
;	(if (and (if-points-valid board (- x 1) (- y 2)) (if-points-valid board (- x 2) (- y 1)))
;		(if (and (is-cross board (- x 1) (- y 2)) (is-cross board (- x 2) (- y 1)))
;			(setf (aref board (- x 1) y) #\x (aref board x (- y 1)) #\x)
;		)
;	)
;	
;	;;in top right corner
;	(if (and (if-points-valid board(- x 1) (+ y 2)) (if-points-valid board (- x 2) (+ y 1)))
;		(if (and (is-cross board (- x 1) (+ y 2)) (is-cross board (- x 2) (+ y 1)))
;			(setf (aref board (- x 1) y) #\x (aref board x (+ y 1)) #\x)
;		)
;	)
;	
;	;;bottom right corner
;	(if (and (if-points-valid board (+ x 1) (+ y 2)) (if-points-valid board (+ x 2) (+ y 1)))
;		(if (and (is-cross board (+ x 1) (+ y 2)) (is-cross board (+ x 2) (+ y 1)))
;			(setf (aref board (+ x 1) y) #\x (aref board x (+ y 1)) #\x)
;		)
;	)

;	;;bottom left corner
;	(if (and (if-points-valid board (+ x 1) (- y 2)) (if-points-valid board (+ x 2) (- y 1)))
;		(if (and (is-cross board (+ x 1) (- y 2)) (is-cross board (+ x 2) (- y 1)))
;			(setf (aref board (+ x 1) y) #\x (aref board x (- y 1)) #\x)
;		)
;	)
	
;	)
	
	
	
)

(defun is-cross (board x y)
	(if (equal (aref board x y) #\x)
	t)
)

(defun is-space (board x y)
	(if (equal (aref board x y) #\Space)
	t)
)

(defun zero-rules (board x y)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (array board))
	
)

(defun two-rules (board x y)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (array board))
	
	;;if two edges are crossed "x" then mark rest two as link
	(let ((temp nil))
	(loop for value in (list 	(is-cross board x (the fixnum (1+ y)) )
								(is-cross board x (the fixnum (1- y)) )
								(is-cross board (the fixnum (1+ x)) y )
								(is-cross board (the fixnum (1- x)) y ))
	
     with temp fixnum = 0
     when value do (setf temp (the fixnum (1+ temp)))
     finally (progn
				(if (= temp 2)
					(progn
					(if (is-space board x (+ y 1))
						(setf (aref board x (+ y 1)) #\|)
					)
					(if (is-space board x (- y 1))
						(setf (aref board x (- y 1)) #\|)
					)
					(if (is-space board (- x 1) y)
						(setf (aref board (- x 1) y) #\-)
					)
					(if (is-space board (+ x 1) y)
						(setf (aref board (+ x 1) y) #\-)
					))
				)
			 )
	
	)
	)
	
	;;if two edges have link then mark rest two as "x"
	(let ((temp nil))
	(loop for value in (list 	(is-line board x (1+ y) )
								(is-line board x (1- y) )
								(is-line board (1+ x) y )
								(is-line board (1- x) y ))
	
     with temp fixnum = 0
     when value do (setf temp (the fixnum (1+ temp)))
     finally (progn
				(if (= temp 2)
					(progn
					(if (is-space board x (+ y 1))
						(setf (aref board x (+ y 1)) #\x)
					)
					(if (is-space board x (- y 1))
						(setf (aref board x (- y 1)) #\x)
					)
					(if (is-space board (- x 1) y)
						(setf (aref board (- x 1) y) #\x)
					)
					(if (is-space board (+ x 1) y)
						(setf (aref board (+ x 1) y) #\x)
					))
				)
			 )
	
	)
	)
			
)




(defun three-rules (board x y)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (array board) (type fixnum x y))
	
	;;if one edge is crossed "x" then mark rest three as link
	(let ((temp nil))
	(loop for value in (list 	(is-cross board x (1+ y))
								(is-cross board x (1- y))
								(is-cross board (1+ x) y)
								(is-cross board (1- x)y))
	;(format t "test call for debugging in three rule")
     with temp fixnum = 0
     when value do (setf temp (the fixnum (1+ temp)))
     finally (progn
				(if (= temp 1)
					(progn
					(if (is-space board x (+ y 1))
						(setf (aref board x (+ y 1)) #\|)
					)
					(if (is-space board x (- y 1))
						(setf (aref board x (- y 1)) #\|)
					)
					(if (is-space board (- x 1) y)
						(setf (aref board (- x 1) y) #\-)
					)
					(if (is-space board (+ x 1) y)
						(setf (aref board (+ x 1) y) #\-)
					))
				)
			 )
	
	)
	)
	
	;;if three edges have link then mark rest one as "x"
	(let ((temp nil))
	(loop for value in (list 	(is-line board x (1+ y) )
								(is-line board x (1- y) )
								(is-line board (1+ x) y )
								(is-line board (1- x) y ))
	
     with temp fixnum = 0
     when value do (setf temp (the fixnum (1+ temp)))
     finally (progn
				(if (= temp 3)
					(progn
					(if (is-space board x (+ y 1))
						(setf (aref board x (+ y 1)) #\x)
					)
					(if (is-space board x (- y 1))
						(setf (aref board x (- y 1)) #\x)
					)
					(if (is-space board (- x 1) y)
						(setf (aref board (- x 1) y) #\x)
					)
					(if (is-space board (+ x 1) y)
						(setf (aref board (+ x 1) y) #\x)
					))
				)
			 )
	
	)
	)
	
	;;if any corner has two outgoing edges as crossed then mark links in rest two outgoing edges
;	(progn 
	;;in up left corner
;	(if (and (if-points-valid board (- x 1) (- y 2)) (if-points-valid board (- x 2) (- y 1)))
;		(if (and (is-cross board (- x 1) (- y 2)) (is-cross board (- x 2) (- y 1)))
;			(setf (aref board (- x 1) y) #\- (aref board x (- y 1)) #\|)
;		)
;	)
;	
;	;;in top right corner
;	(if (and (if-points-valid board (- x 1) (+ y 2)) (if-points-valid board (- x 2) (+ y 1)))
;		(if (and (is-cross board (- x 1) (+ y 2)) (is-cross board (- x 2) (+ y 1)))
;			(setf (aref board (- x 1) y) #\- (aref board x (+ y 1)) #\|)
;		)
;	)
;	
;	;;bottom right corner
;	(if (and (if-points-valid board (+ x 1) (+ y 2)) (if-points-valid board (+ x 2) (+ y 1)))
;		(if (and (is-cross board (+ x 1) (+ y 2)) (is-cross board (+ x 2) (+ y 1)))
;			(setf (aref board (+ x 1) y) #\- (aref board x (+ y 1)) #\|)
;		)
;	)
;
;	;;bottom left corner
;	(if (and (if-points-valid board (+ x 1) (- y 2)) (if-points-valid board (+ x 2) (- y 1)))
;		(if (and (is-cross board (+ x 1) (- y 2)) (is-cross board (+ x 2) (- y 1)))
;			(setf (aref board (+ x 1) y) #\- (aref board x (- y 1)) #\|)
;		)
;	)
	
;	)
	
)

(defun edge-rules (board)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (type array board))

	;;traverse each node i.e. + in game board ;; for each check if they have 2 lines going out ;; if yes, then mark other two edges as "x"
	
	(loop for x fixnum :from 0 :to (1- (array-dimension board 0)) by 2
		:do (loop :for y fixnum from 0 to (1- (array-dimension board 1)) by 2
			do (let ((temp nil))
					(loop for value in (list 	(if (if-points-valid board x (1+ y))
												(is-line board x (1+ y) ))
												(if (if-points-valid board x (1- y))
												(is-line board x (1- y) ))
												(if (if-points-valid board (1+ x) y)
												(is-line board (1+ x) y ))
												(if (if-points-valid board (1- x) y)
												(is-line board (1- x) y )))
	
						with temp fixnum = 0
						when value do (setf temp (the fixnum (1+ temp)))
						finally (progn
						(if (= temp 2)
							(progn
								(if (and (if-points-valid board x (+ y 1)) (is-space board x (+ y 1)))
								(setf (aref board x (+ y 1)) #\x)
								)
								(if (and (if-points-valid board x (- y 1)) (is-space board x (- y 1)))
								(setf (aref board x (- y 1)) #\x)
								)
								(if (and (if-points-valid board (- x 1) y)(is-space board (- x 1) y))
								(setf (aref board (- x 1) y) #\x)
								)
								(if (and (if-points-valid board (+ x 1) y) (is-space board (+ x 1) y))
								(setf (aref board (+ x 1) y) #\x)
								)
							)
						)
						)
	
					)
				
				;) ;;within this block
			)
		)
	)
	
	
			;;if a + has three x then add fourth x-limit
			
		(loop for x fixnum :from 0 :to (1- (array-dimension board 0)) by 2
		:do (loop :for y fixnum from 0 to (1- (array-dimension board 1)) by 2
			do (let ((temp nil))
					(loop for value in (list 	(if (if-points-valid board x (1+ y))
												(is-cross board x (1+ y) ))
												(if (if-points-valid board x (1- y))
												(is-cross board x (1- y) ))
												(if (if-points-valid board (1+ x) y)
												(is-cross board (1+ x) y ))
												(if (if-points-valid board (1- x) y)
												(is-cross board (1- x) y )))
	
						with temp fixnum = 0
						when value do (setf temp (the fixnum (1+ temp)))
						finally (progn
						(if (= temp 3)
							(progn
								(if (and (if-points-valid board x (+ y 1)) (is-space board x (+ y 1)))
								(setf (aref board x (+ y 1)) #\x)
								)
								(if (and (if-points-valid board x (- y 1)) (is-space board x (- y 1)))
								(setf (aref board x (- y 1)) #\x)
								)
								(if (and (if-points-valid board (- x 1) y)(is-space board (- x 1) y))
								(setf (aref board (- x 1) y) #\x)
								)
								(if (and (if-points-valid board (+ x 1) y) (is-space board (+ x 1) y))
								(setf (aref board (+ x 1) y) #\x)
								)
							)
						)
						)
	
					)
				
				;) ;;within this block
			)
		)
	)
	
	
	;;if a + has two x and one edge then add second edge
	
	(loop for x fixnum :from 0 :to (1- (array-dimension board 0)) by 2
		:do (loop :for y fixnum from 0 to (1- (array-dimension board 1)) by 2
			do (let ((temp nil) (temp1 nil))
					(loop for value in (list 	(if (if-points-valid board x (1+ y))
												(is-cross board x (1+ y) ))
												(if (if-points-valid board x (1- y))
												(is-cross board x (1- y) ))
												(if (if-points-valid board (1+ x) y)
												(is-cross board (1+ x) y ))
												(if (if-points-valid board (1- x) y)
												(is-cross board (1- x) y )))
	
						with temp fixnum = 0
						when value do (setf temp (the fixnum (1+ temp)))
						finally (progn
						(if (= temp 2)
							(loop for value in (list 	(if (if-points-valid board x (1+ y))
												(is-line board x (1+ y) ))
												(if (if-points-valid board x (1- y))
												(is-line board x (1- y) ))
												(if (if-points-valid board  (1+ x) y)
												(is-line board (1+ x) y ))
												(if (if-points-valid board (1- x) y)
												(is-line board (1- x) y )))
	
						with temp1 fixnum = 0
						when value do (setf temp1 (the fixnum (1+ temp1)))
						finally (progn
						(if (= temp 1)
							(progn
								(if (and (if-points-valid board x (+ y 1)) (is-space board x (+ y 1)))
								(setf (aref board x (+ y 1)) #\-)
								)
								(if (and (if-points-valid board x (- y 1)) (is-space board x (- y 1)))
								(setf (aref board x (- y 1)) #\-)
								)
								(if (and (if-points-valid board (- x 1) y)(is-space board (- x 1) y))
								(setf (aref board (- x 1) y) #\|)
								)
								(if (and (if-points-valid board (+ x 1) y) (is-space board (+ x 1) y))
								(setf (aref board (+ x 1) y) #\|)
								)
							)
						)
						)
	
					)
						)
						)
	
					)
				
				;) ;;within this block
			)
		)
	)
	
	
board
)

	;;detect the cell with zero value and mark its edges Yellow
(defun heuristic-zero (board x y)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (type fixnum x y) (type array board))
	;(format t "in heuristic zero ~&")
	(progn
	(setf (aref board x (+ y 1)) #\x)
	(setf (aref board x (- y 1)) #\x)
	(setf (aref board (+ x 1) y) #\x)
	(setf (aref board (- x 1) y) #\x)
	)
	
	(if (corner-cell board x y)
		(cond 
			((and (= x 1) (= y 1)) 
					(setf (aref board (- x 1)) #\x (aref board (+ x 2) (- y 1)) #\x))
			((and (= x 1) (= y (- (array-dimension board 1) 2))) 
					(setf (aref board (- x 1) (- y 2)) #\x (aref board (+ x 2) (+ y 1)) #\x))
			((and (= x (- (array-dimension board 0) 2)) (= y 1)) 
					(setf (aref board (+ x 1) (+ y 2)) #\x (aref board (- x 2) (- y 1)) #\x))
			((and (= x (- (array-dimension board 0) 2)) (= y (- (array-dimension board 1) 2)))
					(setf (aref board (- x 2) (+ y 1)) #\x (aref board (+ x 1) (- y 2)) #\x))
		)
	)		
)

(defun heuristic-one (board x y)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (type fixnum x y) (type array board))
	;(format t "in heuristic one ~&")
	(progn
	(if (corner-cell board x y)
		(cond 
		((and (= x 1) (= y 1)) 
			(setf (aref board (- x 1) y) #\x (aref board x (- y 1)) #\x))
		((and (= x 1) (= y (- (array-dimension board 1) 2))) 
			(setf (aref board (- x 1) y) #\x (aref board x (+ y 1)) #\x))
		((and (= x (- (array-dimension board 0) 2)) (= y 1)) 
			(setf (aref board (+ x 1) y) #\x (aref board x (- y 1)) #\x))
		((and (= x (- (array-dimension board 0) 2)) (= y (- (array-dimension board 1) 2)))
			(setf (aref board (+ x 1) y) #\x (aref board x (+ y 1)) #\x))
		)
	)
	)
)

(defun heuristic-two (board x y)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (fixnum x y) (array board))
	;(format t "in heuristic two ~&")
	(progn
		(if (corner-cell board x y)
			(cond 
			((and (= x 1) (= y 1))
			(setf (aref board (- x 1) (+ y 2)) #\- (aref board (+ x 2) (- y 1)) #\|))
			((and (= x 1) (= y (- (array-dimension board 1) 2)))
			(setf (aref board (- x 1) (- y 2)) #\- (aref board (+ x 2) (+ y 1)) #\|))
			((and (= x (- (array-dimension board 0) 2) (= y 1)))
			(setf (aref board (+ x 1) (+ y 2)) #\- (aref board (- x 2) (- y 1)) #\|))
			((and (= x (- (array-dimension board 0) 2) (= y (- (array-dimension board 1) 2))))
			(setf (aref board (- x 2) (+ y 1)) #\| (aref board (+ x 1) (- y 2)) #\-))
			)
		)
				
	)
)


	
(defun heuristic-three (board x y)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (type fixnum x y) (type array board))
	;(format t "in heuristic three ~&")
		;;########################################################
		;;###	Rule for 3 in corner                           ###
		;;########################################################

	(progn
	(if (corner-cell board x y)			;;first set rule for 3 in corner cells
		(cond
		((and (= x 1) (= y 1)) 
		(setf (aref board (- x 1) y) #\- (aref board x (- y 1)) #\|))
		((and (= x 1) (= y (- (array-dimension board 1) 2))) 
		(setf (aref board (- x 1) y) #\- (aref board x (+ y 1)) #\|))
		((and (= x (- (array-dimension board 0) 2)) (= y 1)) 
		(setf (aref board (+ x 1) y) #\- (aref board x (- y 1)) #\|))
		((and (= x (- (array-dimension board 0) 2)) (= y (- (array-dimension board 1) 2))) 
		(setf (aref board (+ x 1) y) #\- (aref board x (+ y 1)) #\|))
		)
	)
		
		;;########################################################
		;;###	Rule for 3 in corner with 3 nearby- begins     ###
		;;########################################################

		
		;;top left corner with 3 in down cell
		(if (and (= x 1) (= y 1))
		(let ((x (+ x 2)) (y y))
			(if (if-points-valid board x y)
				(let (temp (aref board x y))
				(if (and (numberp temp) (= temp 3))
				(setf (aref board x (+ y 1)) #\| (aref board (+ x 1) y) #\- 
				(aref board (+ x 2) (- y 1)) #\| (aref board (- x 1) y) #\- (aref board (- x 3) (+ y 1)) #\-)
				)
				)
			)
		)
		)
		
		;;top left corner with 3 in right cell
		(if (and (= x 1) (= y 1))
		(let ((x x) (y (+ y 2)))
			(if (if-points-valid board x y)
				(let (temp (aref board x y))
				(if (and (numberp temp) (= temp 3))
				(setf (aref board x (+ y 1)) #\| (aref board (+ x 1) y) #\-
				(aref board x (- y 1)) #\| (aref board (- x 1) (+ y 2)) #\- (aref board (+ x 2) (- y 3)) #\|)
				)
				)
			)
		)
		)
		
		;;top right corner with 3 in down cell
		(if (and (= x 1) (= y (- (array-dimension board 1) 2)))
		(let ((x (+ x 2)) (y y))
			(if (if-points-valid board x y)
				(let (temp (aref board x y))
				(if (and (numberp temp) (= temp 3))
				(setf (aref board x (- y 1)) #\| (aref board (+ x 1) y) #\-
				(aref board (+ x 2) (+ y 1)) #\| (aref board (- x 1) y) #\- (aref board (- x 3) (- y 2)) #\-)
				)
				)
			)
		)
		)
		
		;;top right corner with 3 in left cell
		(if (and (= x 1) (= y (- (array-dimension board 1) 2)))
		(let ((x x) (y (- y 2)))
			(if (if-points-valid board x y)
				(let (temp (aref board x y))
				(if (and (numberp temp) (= temp 3))
				(setf (aref board x (- y 1)) #\| (aref board (+ x 1) y) #\-
				(aref board x (+ y 1)) #\| (aref board (- x 1) (- y 2)) #\- (aref board (+ x 2) (+ y 3)) #\|)
				)
				)
			)
		)
		)
		;;bottom left corner with 3 in up cell
		(if (and (= x (- (array-dimension board 0) 2)) (= y 1))
		(let ((x (- x 2)) (y y))
			(if (if-points-valid board x y)
				(let (temp (aref board x y))
				(if (and (numberp temp) (= temp 3))
				(setf (aref board x (+ y 1)) #\| (aref board (- x 1) y) #\-
				(aref board (+ x 1) y) #\- (aref board (- x 2) (- y 1)) #\| (aref board (+ x 3) (+ y 2)) #\-)
				)
				)
			)
		)
		)
		;;bottom left corner with 3 in right cell
		(if (and (= x (- (array-dimension board 0) 2)) (= y 1))
		(let ((x x) (y (+ y 2)))
			(if (if-points-valid board x y)
				(let (temp (aref board x y))
				(if (and (numberp temp) (= temp 3))
				(setf (aref board x (+ y 1)) #\| (aref board (- x 1) y) #\-
				(aref board x (- y 1)) #\| (aref board (+ x 1) (+ y 2)) #\- (aref board (- x 2) (- y 3)) #\|)
				)
				)
			)
		)
		)
		;;bottom right corner with 3 in top cell
		(if (and (= x (- (array-dimension board 0) 2)) (= y (- (array-dimension board 1) 2)))
		(let ((x (- x 2)) (y y))
			(if (if-points-valid board x y)
				(let (temp (aref board x y))
				(if (and (numberp temp) (= temp 3))
				(setf (aref board x (- y 1)) #\| (aref board (- x 1) y) #\-
				(aref board (+ x 1) y) #\- (aref board (- x 2) (+ y 1)) #\| (aref board (+ x 3) (- y 2)) #\-)
				)
				)
			)
		)
		)
		;;bottom right corner with 3 in left cell
		(if (and (= x (- (array-dimension board 0) 2)) (= y (- (array-dimension board 1) 2)))
		(let ((x x) (y (- y 2)))
			(if (if-points-valid board x y)
				(let (temp (aref board x y))
				(if (and (numberp temp) (= temp 3))
				(setf (aref board x (- y 1)) #\| (aref board (- x 1) y) #\-
				(aref board x (+ y 1)) #\| (aref board (+ x 1) (- y 2)) #\- (aref board (- x 2) (+ y 3)) #\|)
				)
				)
			)
		)
		)
		;;########################################################
		;;###	Rule for 3 in corner with 3 nearby- ends       ###
		;;########################################################
		

		;;########################################################
		;;###	Rule for adjacent 3 anywhere - begins          ###
		;;########################################################
		
			;;anywhere 3 with 3 on right
			(if (if-points-valid board x (+ y 2))
				(let ((temp (aref board x (+ y 2))))
				(if (and (numberp temp) (= temp 3))
				(setf (aref board x (- y 1)) #\| (aref board x (+ y 1)) #\| (aref board x (+ y 3)) #\|)
				)
				(if (and (numberp temp) (= temp 3) (if-points-valid board (- x 2) (+ y 1)))
				(setf (aref board (- x 2) (+ y 1)) #\x)
				)
				(if (and (numberp temp) (= temp 3) (if-points-valid board (+ x 2) (+ y 1)))
				(setf (aref board (+ x 2) (+ y 1)) #\x)
				)
				)
			)
		
			;;anywhere 3 with 3 at bottom cell
			(if (if-points-valid board (+ x 2) y)
				(let ((temp (aref board (+ x 2) y)))
				(if (and (numberp temp) (= temp 3))
				(setf (aref board (- x 1) y) #\- (aref board (+ x 1) y) #\- (aref board (+ x 3) y) #\-)
				)
				(if (and (numberp temp) (= temp 3) (if-points-valid board (+ x 1) (- y 2)))
				(setf (aref board (+ x 1) (- y 2)) #\x)
				)
				(if (and (numberp temp) (= temp 3) (if-points-valid board (+ x 1) (+ y 2)))
				(setf (aref board (+ x 1) (+ y 2)) #\x)
				)
				)
			)
		
		
		;;########################################################
		;;###	Rule for adjacent 3 anywhere - ends            ###
		;;########################################################

		;;########################################################
		;;###	Rule for diagonal 3 anywhere - begins          ###
		;;########################################################

			;;anywhere 3 with 3 at left down diagonal cell
			(if (if-points-valid board (+ x 2) (- y 2))
				(let ((temp (aref board (+ x 2) (- y 2))))
				(if (and (numberp temp) (= temp 3))
				(setf (aref board x (+ y 1)) #\| (aref board (- x 1) y) #\-
				(aref board (+ x 3) (- y 2)) #\- (aref board (+ x 2) (- y 3)) #\-)
				)
				(if (and (numberp temp) (= temp 3) (if-points-valid board (- x 1) (+ y 2)))
				(setf (aref board (- x 1) (+ y 2)) #\x)
				)
				(if (and (numberp temp) (= temp 3) (if-points-valid board (- x 2) (+ y 1)))
				(setf (aref board (- x 2) (+ y 1)) #\x)
				)
				(if (and (numberp temp) (= temp 3) (if-points-valid board (+ x 3) (- y 4)))
				(setf (aref board (+ x 3) (- y 4)) #\x)
				)
				(if (and (numberp temp) (= temp 3) (if-points-valid board (+ x 4) (- y 3)))
				(setf (aref board (+ x 4) (- y 3)) #\x)
				)
				)
			)
		
			;;anywhere 3 with 3 at right down diagonal cell
			(if (if-points-valid board (+ x 2) (+ y 2))
				(let ((temp (aref board (+ x 2) (+ y 2))))
				(if (and (numberp temp) (= temp 3))
				(setf (aref board (- x 1) y) #\- (aref board x (- y 1)) #\|
				(aref board (+ x 3) (+ y 2)) #\- (aref board (+ x 2) (+ y 3)) #\|)
				)
				(if (and (numberp temp) (= temp 3) (if-points-valid board (- x 2) (- y 1)))
				(setf (aref board (- x 2) (- y 1)) #\x)
				)
				(if (and (numberp temp) (= temp 3) (if-points-valid board (- x 1) (- y 2)))
				(setf (aref board (- x 1) (- y 2)) #\x)
				)
				(if (and (numberp temp) (= temp 3) (if-points-valid board (+ x 3) (+ y 4)))
				(setf (aref board (+ x 3) (+ y 4)) #\x)
				)
				(if (and (numberp temp) (= temp 3) (if-points-valid board (+ x 4) (+ y 3)))
				(setf (aref board (+ x 4) (+ y 3)) #\x)
				)
				)
			)
		
		;;########################################################
		;;###	Rule for diagonal 3 anywhere - ends            ###
		;;########################################################
		
		
		
		;;###########################################################
		;;###Rule for 3 anywhere with 0 in adjacent cells- begins ###
		;;###########################################################
		
		;;anywhere 3 with 0 in top cell
			(if (if-points-valid board (- x 2) y)
				(let ((temp (aref board (- x 2) y)))
				(if (and (numberp temp) (= temp 0))
				(setf (aref board x (+ y 1)) #\| (aref board (+ x 1) y) #\- (aref board x (- y 1)) #\| ;;right, bottom, left
				(aref board (- x 1) (- y 2)) #\- (aref board (- x 1) (+ y 2)) #\|) 		;;only way to branch out from cell with 3
				)
				)
			)
		
		
		;;anywhere 3 with 0 in right cell
			(if (if-points-valid board x (+ y 2))
				(let ((temp (aref board x (+ y 2))))
				(if (and (numberp temp) (= temp 0))
				(setf (aref board (+ x 1) y) #\- (aref board x (- y 1)) #\| (aref board (- x 1) y) #\- ;bottom, left, top
				(aref board (- x 2) (+ y 1)) #\| (aref board (+ x 2) (+ y 1)) #\|) 		;;only way to branch out from cell with 3
				)
				)
			)
		
		
		;;anywhere 3 with 0 in bottom cell
			(if (if-points-valid board (+ x 2) y)
				(let ((temp (aref board (+ x 2) y)))
				(if (and (numberp temp) (= temp 0))
				(setf (aref board x (- y 1)) #\| (aref board (- x 1) y) #\- (aref board x (+ y 1)) #\- ;left, top, right
				(aref board (+ x 1) (- y 2)) #\- (aref board (+ x 1) (+ y 2)) #\-) 		;;only way to branch out from cell with 3
				)
				)
			)
		
		
		;;anywhere 3 with 0 in left cell
			(if (if-points-valid board x (- y 2))
				(let ((temp (aref board x (- y 2))))
				(if (and (numberp temp) (= temp 0))
				(setf (aref board (- x 1) y) #\- (aref board x (+ y 1)) #\| (aref board (+ x 1) y) #\-	;top, right, bottom 	
				(aref board (- x 2) (- y 1)) #\| (aref board (+ x 2) (- y 1)) #\|) 		;;only way to branch out from cell with 3
				)
				)
			)
			
		;;########################################################
		;;###	Rule for 3 with 0 in adjacent cells- ends      ###
		;;########################################################
		
		;;########################################################
		;;###	Rule for 3 with 0 in diagonal cells- begins    ###
		;;########################################################
		
		;;anywhere 3 with 0 in top left diagonal cell
			(if (if-points-valid board (- x 2) (- y 2))
				(let ((temp (aref board (- x 2) (- y 2))))
				(if (and (numberp temp) (= temp 0))
				(setf (aref board (- x 1) y) #\- (aref board x (- y 1)) #\|)	;top, left 	
				)
				)
			)
			
		;;anywhere 3 with 0 in top right diagonal cell
			(if (if-points-valid board (- x 2) (+ y 2))
				(let ((temp (aref board (- x 2) (+ y 2))))
				(if (and (numberp temp) (= temp 0))
				(setf (aref board (- x 1) y) #\- (aref board x (+ y 1)) #\|)	;top, right 	
				)
				)
			)
			
		;;anywhere 3 with 0 in bottom left diagonal cell
			(if (if-points-valid board (+ x 2) (- y 2))
				(let ((temp (aref board (+ x 2) (- y 2))))
				(if (and (numberp temp) (= temp 0))
				(setf (aref board (+ x 1) y) #\- (aref board x (- y 1)) #\|)	;bottom, left 	
				)
				)
			)
		
		;;anywhere 3 with 0 in bottom right diagonal cell
			(if (if-points-valid board (+ x 2) (+ y 2))
				(let ((temp (aref board (+ x 2) (+ y 2))))
				(if (and (numberp temp) (= temp 0))
				(setf (aref board (+ x 1) y) #\- (aref board x (+ y 1)) #\|)	;bottom, right 	
				)
				)
			)
			
		;;########################################################
		;;###	Rule for 3 with 0 in diagonal cells- ends      ###
		;;########################################################
		
	)

)


    
(defun corner-cell (board x y)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (type fixnum x y) (type array board))
	(or 
		(and (= x 1) (= y 1))
		(and (= x 1) (= y (- (array-dimension board 1) 2)))
		(and (= x (- (array-dimension board 0) 2)) (= y 1))
		(and (= x (- (array-dimension board 0) 2)) (= y (- (array-dimension board 1) 2)))
	)
)

(defun if-points-valid (board x y)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (type fixnum x y) (type array board))
	(if (AND 
		(>= x 0)
        (<= x (1- (array-dimension board 0)))
		(>= y 0)
		(<= y (1- (array-dimension board 1)))
		)
		t nil
	)
)


(defun apply-algo (board moves)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (type array board))
	(let ((x-origin nil) (y-origin nil))
		(loop for x fixnum from 1 to (- (array-dimension board 0) 2) by 2
		:do (loop for y fixnum from 1 to (- (array-dimension board 1) 2) by 2
            :do (if (typep (aref board x y) 'integer) ;;mkd try to use more efficient way
                (if (= 3 (aref board x y))
                (progn
                (multiple-value-setq (x-origin y-origin) (values x y))
                (return))
                ()) 
                ())
                )
			(if (typep x-origin 'integer) (return) () ) ;;mkd modify
        )
		 
		(if (algo board (- x-origin 1) (- y-origin 1)  moves 0)
			(return-from apply-algo t)
			(algo board (+ x-origin 1) (+ y-origin 1)  moves 0) ;;try again from diagonal point
		)
	)
	
)


(defun algo (board x y moves &optional (depth 0) (visited (make-array (array-dimensions board) :initial-element nil)))
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (fixnum x y) (array board))	
	
	(if (> depth (1- (* (array-dimension board 0) (array-dimension board 1))))
		(progn   ;; depth exceeded, too deep, returning
        (return-from algo nil))
		
		(progn				
        (if (check-board board)		;;mkd- check if solved
        (progn (print-moves moves)
        (return-from algo board)))

		(progn
        (let ((num-of-edges (num-of-edges board x y)))
            (if (< num-of-edges 3) 		;mkd point x,y has less than 3 edges falling on it. if more, it becomes invalid board
			(progn
				(flet((	emptyp (board x y)				
						(declare (type array board)	(type fixnum x y))
						"Pass in the position in the array If it is a line, return a 1"
						(let ((char (aref board x y)))
						(declare (type character char))
						(case char
						((#\| #\-) nil)
						((#\Space #\x) t)
						)
						)
						)
					)
			
				(if (= 2 num-of-edges) 
					(progn 
						(let ((x (1+ x)) (y y))		;;downwards
							(if (and (if-points-valid board x y) (not (emptyp board x y)))      
								(if (and (not (aref visited x y)) (can-proceed board x y))	
									(progn
										(setf (aref visited x y) t) ; mark as visited
										(push (cons x y) moves) ; push move.
										(if (algo board (1+ x) y moves (1+ depth) visited)
											(return-from algo t)
											(progn ; else remove the move and backtrack
											(pop moves)
											(setf (aref visited x y) nil))
										)
									)
								)
							)
						)

								
                          
						(let ((x x) (y (1+ y)))		;; rightwards
							(if (and (if-points-valid board x y) (not (emptyp board x y)))	
								(if (and (not (aref visited x y)) (can-proceed board x y))
									(progn
										(setf (aref visited x y) t) ; mark it visited
										(push (cons x y) moves) ; push move.
										(if (algo board x (1+ y) moves (1+ depth) visited)
											(return-from algo t)
											(progn ; else remove the move and backtrack
											(pop moves)
											(setf (aref visited x y) nil))
										)
									)
								)
							)
						)
                          
						  
						  
						(let ((x (1- x)) (y y))		;; upwards
							(if (and (if-points-valid board x y)
							(not (emptyp board x y)))
								(if (and (not (aref visited x y)) (can-proceed board x y))
									(progn
										(setf (aref visited x y) t) ; mark it visited
										(push (cons x y) moves) ; push move.
										(if (algo board (1- x) y moves (1+ depth) visited)
											(return-from algo t)
											(progn ; else remove the move and backtrack
											(pop moves)
											(setf (aref visited x y) nil))
										)
									)
								)
							)
						)
                          
						  
						  
						  
						(let ((x x) (y (1- y)))	;; leftward
							(if (and (if-points-valid board x y) (not (emptyp board x y)))
								(if (and (not (aref visited x y)) (can-proceed board x y))
									(progn
										(setf (aref visited x y) t) ;mark it visited
										(push (cons x y) moves) ; push move.
										(if (algo board x (1- y) moves (1+ depth) visited)
											(return-from algo t)
											(progn ; else remove the move and backtrack
											(pop moves)
											(setf (aref visited x y) nil))
										)
									)
								)
							)
						)
						
						
						
					)
						
			
					(progn ;else, dfs over there
						(let ((x (1+ x)) (y y)) ;; downward
							(if (if-points-valid board x y)
								(if (emptyp board x y)
								(let ((temp (aref board x y)))
								(if (not (equal temp #\x))
									(progn 
										(setf (aref board x y) #\| (aref visited x y) t)
										(if (can-proceed board x y)
											(progn
											(push (cons x y) moves)
											(if (algo board (1+ x) y moves (1+ depth) visited)
												(return-from algo t)
												(progn
												(pop moves)
												(setf (aref board x y) #\Space (aref visited x y) nil))
											)
											)
											(setf (aref board x y) #\Space (aref visited x y) nil)
										)
									)

									
									(if (and (not (aref visited x y)) (can-proceed board x y))
										(progn
										(setf (aref visited x y) t) ; mark it visited
										(push (cons x y) moves) ; push move.
										(if (algo board (1+ x) y moves (1+ depth) visited)
										(return-from algo t)
										(progn ; else remove the move and backtrack
										(pop moves)
										(setf (aref visited x y) nil))))
									)
								)
							)	
						)))
						
						
                         
                    (let ((x x) (y (1+ y))) 	;; right (+y)
                        (if  (if-points-valid board x y)
							(if (emptyp board x y)
							(let ((temp (aref board x y)))
								(if (not (equal temp #\x))
                                (progn
                                (setf (aref board x y) #\- (aref visited x y) t)
									(if (can-proceed board x y)
										(progn
                                        (push (cons x y) moves)
                                        (if (algo board x (1+ y) moves (1+ depth) visited)
                                            (return-from algo t)
                                            (progn
												(pop moves)
												(setf (aref board x y) #\Space (aref visited x y) nil)
											)
										)
										)
											(setf (aref board x y) #\Space (aref visited x y) nil)
									)
								)
                                     
								(if (and (not (aref visited x y)) (can-proceed board x y)) ;; Follow line
									(progn
										(setf (aref visited x y) t) ; mark it visited
										(push (cons x y) moves) ; push move.
										(if (algo board x (1+ y)  moves (1+ depth) visited)
										(return-from algo t)
										(progn ; else remove move and backtrack
										(pop moves)
										(setf (aref visited x y) nil)))
									)
								)
							)							
						)						
					)))

                         
						 
                    (let ((x (1- x)) (y y))	 ;; upward 
                        (if (if-points-valid board x y)
                            (if (emptyp board x y)
							(let ((temp (aref board x y)))
								(if (not (equal temp #\x))
                                (progn
                                    (setf (aref board x y) #\| (aref visited x y) t)
										(if (can-proceed board x y)
                                        (progn
                                        (push (cons x y) moves)
                                        (if (algo board (1- x) y moves (1+ depth) visited)
                                        (return-from algo t)
                                        (progn
                                        (pop moves)
                                        (setf (aref board x y) #\Space (aref visited x y) nil)
										)
										)
										)
                                        (setf (aref board x y) #\Space (aref visited x y) nil)
										)
								)
                                    
                                (if (and (not (aref visited x y)) (can-proceed board x y))	 ;; upward
                                    (progn
										(setf (aref visited x y) t) ; mark visited
										(push (cons x y) moves) ; push the move.
										(if (algo board (1- x) y moves (1+ depth) visited)
                                            (return-from algo t)
                                            (progn ; else remove move and backtrack
                                            (pop moves)
                                            (setf (aref visited x y) nil))
										)
									)
								)
							)
						)												
					)))

						
                         
                    (let ((x x) (y (1- y)))		;; leftward
                        (if (if-points-valid board x y)
                            (if (emptyp board x y)
							(let ((temp (aref board x y)))
								(if (not (equal temp #\x))
                                (progn
                                    (setf (aref board x y) #\- (aref visited x y) t)
									(if (can-proceed board x y)
										(progn
										(push (cons x y) moves)
										(if (algo board x (1- y) moves (1+ depth) visited)
											(return-from algo t)
											(progn
											(pop moves)
											(setf (aref board x y) #\Space (aref visited x y) nil)))
										)
											(setf (aref board x y) #\Space (aref visited x y) nil)
									)
								)
                                    
                                (if (and (not (aref visited x y)) (can-proceed board x y))	
                                    (progn
                                    (setf (aref visited x y) t) ; mark it visited
                                    (push (cons x y) moves) ; push the move
                                    (if (algo board x (1+ y) moves (1+ depth) visited)
                                        (return-from algo t)
                                        (progn ; else remove move and backtrack
                                        (pop moves)
                                        (setf (aref visited x y) nil))
									)
									)
								)
							)
						)
					)))
                         
				nil)
			
			
				)		   
							   
			)
	
			)

))))))



(defun can-proceed (board x y)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (type fixnum x y) (type array board))
	(let ((char (aref board x y)))
    (declare (type character char))
    (case char
       (#\| ;; Vertical, so check left and right.
       (progn
        (AND 
			(let ((y (1- y))) ;left
				(if (if-points-valid board x y)
                (if (numberp (aref board x y))
                (>= (aref board x y) (num-of-edges board x y))
                t)
                t)
			)
			(let ((y (1+ y))) ;right
				(if (if-points-valid board x y)
                (if (numberp (aref board x y))
                (>= (aref board x y) (num-of-edges board x y))
                t)
                t)
			)
		)))
      
		(#\- ;; Horizontal, co check above and below/
		(progn
        (AND
			(let ((x (1- x))) ;above
            (if (if-points-valid board x y)
            (if (numberp (aref board x y))
            (>= (aref board x y) (num-of-edges board x y))
            t)
            t))
			(let ((x (1+ x))) ;below
            (if (if-points-valid board x y)
            (if (numberp (aref board x y))
            (>= (aref board x y) (num-of-edges board x y))
            t)
            t))
		)))
	))
)
				
				
				
				
(defun num-of-edges (board x y)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (type fixnum x y) (type array board))

   (loop for value in (list (if (if-points-valid board x (the fixnum (1+ y)))
                               (is-line board x (the fixnum (1+ y)) ))
                           (if (if-points-valid board x (the fixnum (1- y)))
                               (is-line board x (the fixnum (1- y)) ))
                           (if (if-points-valid board  (the fixnum (1+ x)) y)
                               (is-line board (the fixnum (1+ x)) y ))
                           (if (if-points-valid board (the fixnum (1- x)) y)
                               (is-line board (the fixnum (1- x)) y )))
     with x fixnum = 0
     when value do (setf x (the fixnum (1+ x)))
     finally (progn
             (return (the fixnum x))
			 )
	)
)
			   
			   
