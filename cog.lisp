;;; (load (compile-file "cog.lisp"))
(setf *random-state* (make-random-state t))

;;; =====================================================================
;;; Create a decision tree by asking questions.

;;; The dtree is a set of triples comprising a distinguishing
;;; question, and then a left/yes branch and a right/no branch. 

(defvar *dtree* nil)

(defun dtree-learn ()
  (dtree-reset))

(defun dtree-reset ()
  (setf *dtree* nil)
  (dtree-inner)
  )

(defun dtree-inner ()
  (format t "Think of an animal.") (terpri)
  (sleep 1)
  (dtree-inner-r))

(defun dtree-inner-r (&optional (dtree *dtree*) (upnode nil))
  (cond ((null dtree) ;; Special case for very first time in.
	 (let ((animal (read-from-string (ask "What is it (just the animal, not with 'a' or 'an')?"))))
	   (setf *dtree* animal)
	   (dtree-inner)))
	((atom dtree)
	 (if (yes-or-no-p "Is it a ~a?" dtree)
	     (dtree-inner)
	     (let* ((animal (read-from-string (ask "What is it (just the animal, not with 'a' or 'an')?")))
		    (question (ask (format nil "What is a question that is yes for a(n) ~a and no for a(n) ~a" animal dtree))))
	       (if upnode
		   (nsubst `(,question ,animal ,dtree) dtree upnode)
		   (setf *dtree* `(,question ,animal ,dtree)))
	       (dtree-inner))))
	(t (if (yes-or-no-p (first dtree))
	       (dtree-inner-r (second dtree) dtree)
	       (dtree-inner-r (third dtree) dtree)
	       ))
	))

(defun ask (question)
  (format t question) (terpri)
  (read-line)) 

;;; Displaying the DTREE

(defun dtree2dot (&optional (dtree *dtree*))
  (with-open-file (o "tree.dot" :direction :output :if-exists :supersede)
    (format o "digraph BTree {
  node [shape=record];

  // Graph direction (top to bottom)
  rankdir=TB;

  // Define the B-tree nodes with keys and child pointers
")
    (dtreeout o dtree)
    (format o "~%}~%" )))

(defun dtreeout (o tree)
  (if (atom (second tree))
      (format o "~s -> ~s [label=\"yes\"];~%" (first tree) (second tree))
      (progn 
	(format o "~s -> ~s [label=\"yes\"];~%" (first tree) (car (second tree)))
	(dtreeout o (second tree))))
  (if (atom (third tree))
      (format o "~s -> ~s [label=\"no\"];~%" (first tree) (third tree))
      (progn 
	(format o "~s -> ~s [label=\"no\"];~%" (first tree) (car (third tree)))
	(dtreeout o (third tree)))))

#|
(dtree-learn)
(dtree2dot)
(uiop::launch-program "dot -Tpdf tree.dot -o tree.pdf")
|#

;;; =====================================================================

;;; A semantic network is given by a set of relationship between N
;;; objects. All you need to do is define the strength of association
;;; between the symbols. The association is assumed to be symmetric,
;;; and 0.0 when unspecified.

(defun relations2dot (relations file)
  (with-open-file (o file :direction :output :if-exists :supersede)
    (format o "digraph G {
~{  ~A -> ~A [label=\"~A\"];

~}
}" 
          (loop for (start strength end) in relations
                collect start
                collect end
                collect strength))))

(defvar *term.term->strength* (make-hash-table :test #'equal))

;;; This is used to fake up a random semantic net. We do use the "main
;;; line" of word-to-next-word as a bit of a biaser. 

(defun create-random-network (terms &key (edges-per-term 5) (direct-next-term-connection-strength 1.0)) 
  (clrhash *term.term->strength*)
  (loop for (term direct-next-term) on terms
	with nterms = (length terms)
	do
	(setf (gethash (cons term direct-next-term) *term.term->strength*) direct-next-term-connection-strength)
	(loop for i below edges-per-term
		 do (setf (gethash (cons term (nth (random nterms) terms))
				   *term.term->strength*)
			  (/ (random 100) 100.0))))
  (loop for (from . to) being the hash-keys of *term.term->strength*
	using (hash-value strength)
	collect `(,from ,strength ,to)))

(defun snet-syms (snet)
  (loop with syms = nil
	as (a nil b) in snet
	do
	(pushnew a syms)
	(pushnew b syms)
	finally (return syms)))

(defstruct smat syms mat)

(defun snet->smat (snet &key (selfref? t) (symmetric? t) (max-bkg 0.1))
  (let* ((syms (snet-syms snet))
	 (smat (make-random-smat syms max-bkg)))
    (loop for (a to b) in snet
	  as pa = (position a syms)
	  as pb = (position b syms)
	  do
	  ;; Non-obviously, you want this to go down the array by
	  ;; default. This obviously won't matter if it's symmetric,
	  ;; but if not, then the multiplication will do the wrong
	  ;; thing if you fill this in across instead of down.
	  (setf (aref smat pb pa) to) 
	  (when selfref? (setf (aref smat pa pa) 1.0 (aref smat pb pb) 1.0))
	  (when symmetric? (setf (aref smat pa pb) to))) 
    (make-smat :syms syms :mat smat)))

(defun make-random-smat (syms max-bkg)
  (let* ((nsyms (length syms))
	 (backint (truncate (* 100 max-bkg)))
	 (array (make-array (list nsyms nsyms))))
    (loop for i below nsyms
	  do (loop for j below nsyms
		   do (setf (aref array i j) (/ (1+ (random backint)) 100.0))))
    array))

(defun symvals->symvec (symvals smat)
  (let ((syms (smat-syms smat)))
    (make-array (length syms)
		:initial-contents
		(loop for sym in syms
		      collect (or (second (assoc sym symvals)) 0.0)))))
		    
(defun symvpprint (smat symvec &key (trace-n nil) (vsort? t))
  (format t "---~%")
  (let ((r (symvcollect smat symvec)))
    (loop for (sym . v) in (if vsort? (sort r #'> :key #'cdr) r)
	  as i below (if (numberp trace-n) trace-n (length r))
	  do (format t "~a = ~$~%" sym v))))

(defun symvcollect (smat symvec)
  (loop for sym in (smat-syms smat)
	as v across symvec
	collect (cons sym v)))

(defun /! (a b) (if (zerop b) 0.0d0 (/ a b)))

(defun normvec (v0)
  (let* ((v (copy-seq v0))
	 (sum (reduce #'+ v)))
    (loop for p below (length v)
	  do (setf (aref v p) (/! (aref v p) sum)))
    v))
	 

;;; This will return a normalized vector.

(defun spreadloop (smat isymvec cycles &key (trace-n nil))
  (loop with symvec = isymvec
	as i below cycles
	do
	(setf symvec (normvec (lla:mm (smat-mat smat) symvec)))
	(when trace-n (symvpprint smat symvec :trace-n trace-n :vsort? t))
	finally (return symvec)))

;;; DTREE version of smat

;;; To compute distance matrix we need to be able to compute the
;;; length of the patth the root and each leaf.

(defvar *paths* nil)

(defun cache-paths-to-leaves ()
  (setf *paths* nil)
  (path-to-leaves-r *dtree* nil)
  (mapcar #'print *paths*)
  :done
  )

(defun path-to-leaves-r (tree path)
  (cond ((atom tree) (push (cons tree (reverse path)) *paths*))
       	(t (list (path-to-leaves-r (second tree) (cons (cons :yes (first tree)) path))
		 (path-to-leaves-r (third tree) (cons (cons :no (first tree)) path))))))


;;; Now we can make the distance matrix between the leaves

(defun all-leaves ()
  (mapcar #'car *paths*))

(defun path-to (leaf)
  (cdr (assoc leaf *paths* :test #'string-equal)))

(defun length-to (leaf)
  (length (path-to leaf)))

(defun depth-to-common-node (p1 p2)
  (let* ((cn (cdar (intersection p1 p2 :test #'(lambda (a b) (equal (cdr a) (cdr b))))))) ;; Ignore the yes/no
    (position cn p1 :test #'(lambda (a b) (equal a (cdr b)))) 
    ))

(defun distance-between (l1 l2)
  (if (equal l1 l2) 0
      (let* ((p1 (path-to l1))
	     (p2 (path-to l2)))
	(- (+ (length p1) (length p2))
	   (* 2 (depth-to-common-node p1 p2))))))
  
(setf *dtree*
      '("Is it a kind of monkey?"
	("Did Dianne Fosse study them?" APE
	 ("Are they matriarcal?" BONOBO
	  ("Does they have stripped tails?" LEMUR
					    ("Does they program computers?" HUMAN BABOON))))
	("Does it have a long neck?"
	 ("Is it a bird?" ("Are they the heaviest flying bird?" SWAN HERON) GIRAFFE)
	 ("Does it live in water?"
	  ("Does it have a shell?" TURTLE
				   ("Does it live in the ocean?" ("Do they bark?" SEAL WHALE) HIPPO))
	  ("Is it cold blooded?" ("Does it live in soil?" WORM LIZARD)
				 ("Is it the king of the beasts?" LION CAT))))))

#|
(defun create-smat-from-dtree ()
  (cache-paths-to-leaves)
  (let* ((syms (all-leaves))
	(nsyms (length syms))
	(smat (make-array (list nsyms nsyms))))
    (loop for sym1 in syms
	  as p1 from 0 by 1
	  do (loop for sym2 in syms
		   as p2 from 0 by 1
		   as d = (distance-between sym1 sym2)
		   do (setf (aref smat p1 p2) (- d))))
    (make-smat :syms syms :mat smat)))
|#

(defun create-snet-from-dtree ()
  (cache-paths-to-leaves)
  (let* ((syms (all-leaves)))
    (loop for sym1 in syms
	  append (loop for sym2 in syms
		   as d = (distance-between sym1 sym2)
		   collect `(,sym1 ,(- d) ,sym2)))))

#|

;;; Semantic Net on the dtree:

(defparameter *terms* (all-leaves))
(setq *snet* (create-snet-from-dtree))
(defparameter *smat* (snet->smat *snet*))
(relations2dot *snet* "anet.dot")
(uiop::launch-program "dot -Tpdf anet.dot -o anet.pdf")
(print *smat*)
(defun dtree-sstest (initsymvals &optional (cycles 10))
  (let* ((initsymvec (symvals->symvec initsymvals *smat*)))
    (symvpprint *smat* initsymvec :vsort? t)
    (spreadloop *smat* initsymvec cycles :trace-n t)))
(defparameter *initsymvals* '((cat 1.0) (human 1.0)))
(dtree-sstest *initsymvals*)

;;; Random semantic net

(defparameter *terms*
  '(car bus school ambulance stop fire slow slow fast brake accelerator
    brake glass water drink teen eat swim drown hospital))
(defparameter *snet* (create-random-network *terms*))
(defparameter *smat* (snet->smat *snet*))
(relations2dot *snet* "rnet.dot")
(uiop::launch-program "dot -Tpdf rnet.dot -o rnet.pdf")
(defparameter *initsymvals* '((drown 1.0)))
(defun random-sstest (initsymvals &optional (cycles 10))
  (print *smat*)
  (let* ((initsymvec (symvals->symvec initsymvals *smat*)))
    (symvpprint *smat* initsymvec :vsort? t)
    (spreadloop *smat* initsymvec cycles :trace-n t)))
(random-sstest *initsymvals*)

|#

;;; =====================================================================
;;; Semantic Graph Version of a Language Model

(defparameter *the-raven*
  '(Once upon a midnight dreary while I pondered  weak and weary
Over many a quaint and curious volume of forgotten lore
    While I nodded  nearly napping  suddenly there came a tapping 
As of some one gently rapping  rapping at my chamber door 
  Tis some visitor   I muttered   tapping at my chamber door 
            Only this and nothing more

    Ah  distinctly I remember it was in the bleak December 
And each separate dying ember wrought its ghost upon the floor 
    Eagerly I wished the morrow  vainly I had sought to borrow
    From my books surcease of sorrow sorrow for the lost Lenore 
For the rare and radiant maiden whom the angels name Lenore 
            Nameless here for evermore 

    And the silken  sad  uncertain rustling of each purple curtain
Thrilled me filled me with fantastic terrors never felt before 
    So that now  to still the beating of my heart  I stood repeating
      Tis some visitor entreating entrance at my chamber door 
Some late visitor entreating entrance at my chamber door  
            This it is and nothing more  

    Presently my soul grew stronger  hesitating then no longer 
 Sir   said I   or Madam  truly your forgiveness I implore 
    But the fact is I was napping  and so gently you came rapping 
    And so faintly you came tapping  tapping at my chamber door 
That I scarce was sure I heard you  here I opened wide the door  
            Darkness there and nothing more 

    Deep into that darkness peering  long I stood there wondering  fearing 
Doubting  dreaming dreams no mortal ever dared to dream before 
    But the silence was unbroken  and the stillness gave no token 
    And the only word there spoken was the whispered word   Lenore 
This I whispered  and an echo murmured back the word   Lenore   
            Merely this and nothing more 

    Back into the chamber turning  all my soul within me burning 
Soon again I heard a tapping somewhat louder than before 
     Surely   said I   surely that is something at my window lattice 
      Let me see  then  what thereat is  and this mystery explore 
Let my heart be still a moment and this mystery explore  
             Tis the wind and nothing more  

    Open here I flung the shutter  when  with many a flirt and flutter 
In there stepped a stately Raven of the saintly days of yore 
    Not the least obeisance made he  not a minute stopped or stayed he 
    But  with mien of lord or lady  perched above my chamber door 
Perched upon a bust of Pallas just above my chamber door 
            Perched  and sat  and nothing more 

Then this ebony bird beguiling my sad fancy into smiling 
By the grave and stern decorum of the countenance it wore 
 Though thy crest be shorn and shaven  thou   I said   art sure no craven 
Ghastly grim and ancient Raven wandering from the Nightly shore 
Tell me what thy lordly name is on the Nights Plutonian shore  
            Quoth the Raven  Nevermore  

    Much I marvelled this ungainly fowl to hear discourse so plainly 
Though its answer little meaning little relevancy bore 
    For we cannot help agreeing that no living human being
    Ever yet was blessed with seeing bird above his chamber door 
Bird or beast upon the sculptured bust above his chamber door 
            With such name as  Nevermore  

    But the Raven  sitting lonely on the placid bust  spoke only
That one word  as if his soul in that one word he did outpour 
    Nothing farther then he uttered not a feather then he fluttered 
    Till I scarcely more than muttered  Other friends have flown before 
On the morrow he will leave me  as my Hopes have flown before  
            Then the bird said  Nevermore  

    Startled at the stillness broken by reply so aptly spoken 
 Doubtless   said I   what it utters is its only stock and store
    Caught from some unhappy master whom unmerciful Disaster
    Followed fast and followed faster till his songs one burden bore 
Till the dirges of his Hope that melancholy burden bore
            Of  Never nevermore   

    But the Raven still beguiling all my fancy into smiling 
Straight I wheeled a cushioned seat in front of bird  and bust and door 
    Then  upon the velvet sinking  I betook myself to linking
    Fancy unto fancy  thinking what this ominous bird of yore 
What this grim  ungainly  ghastly  gaunt  and ominous bird of yore
            Meant in croaking  Nevermore  

    This I sat engaged in guessing  but no syllable expressing
To the fowl whose fiery eyes now burned into my bosoms core 
    This and more I sat divining  with my head at ease reclining
    On the cushions velvet lining that the lamplight gloated oer 
But whose velvet violet lining with the lamplight gloating oer 
            She shall press  ah  nevermore 

    Then  methought  the air grew denser  perfumed from an unseen censer
Swung by Seraphim whose footfalls tinkled on the tufted floor 
     Wretch   I cried   thy God hath lent thee by these angels he hath sent thee
    Respite respite and nepenthe from thy memories of Lenore 
Quaff  oh quaff this kind nepenthe and forget this lost Lenore  
            Quoth the Raven  Nevermore  

     Prophet   said I   thing of evil  prophet still  if bird or devil  
Whether Tempter sent  or whether tempest tossed thee here ashore 
    Desolate yet all undaunted  on this desert land enchanted 
    On this home by Horror haunted tell me truly  I implore 
Is there is there balm in Gilead tell me tell me  I implore  
            Quoth the Raven  Nevermore  

     Prophet   said I   thing of evil  prophet still  if bird or devil 
By that Heaven that bends above us by that God we both adore 
    Tell this soul with sorrow laden if  within the distant Aidenn 
    It shall clasp a sainted maiden whom the angels name Lenore 
Clasp a rare and radiant maiden whom the angels name Lenore  
            Quoth the Raven  Nevermore  

     Be that word our sign of parting  bird or fiend   I shrieked  upstarting 
 Get thee back into the tempest and the Nights Plutonian shore 
    Leave no black plume as a token of that lie thy soul hath spoken 
    Leave my loneliness unbroken  quit the bust above my door 
Take thy beak from out my heart  and take thy form from off my door  
            Quoth the Raven  Nevermore  

    And the Raven  never flitting  still is sitting  still is sitting
On the pallid bust of Pallas just above my chamber door 
    And his eyes have all the seeming of a demons that is dreaming 
    And the lamplight oer him streaming throws his shadow on the floor 
And my soul from out that shadow that lies floating on the floor
    Shall be lifted nevermore ))

(defparameter *rlen* (length *the-raven*))

(defvar *w1.w2->count* (make-hash-table :test #'equal))

(defun text-to-smat (text &key (factor 0.25) (max-bkg 0.1))
  (clrhash *w1.w2->count*)
  (let ((p 1.0))
    (loop for (a b) on text
	  until (null b)
	  do 
	  (incf (gethash (cons a b) *w1.w2->count* 0) 1.0))
    (setf p (* p factor))
    (loop for (a nil b) on text
	  until (null b)
	  do 
	  (incf (gethash (cons a b) *w1.w2->count* 0) 0.25))
    (setf p (* p factor))
    (loop for (a nil nil b) on text
	  until (null b)
	  do 
	  (incf (gethash (cons a b) *w1.w2->count* 0) 0.05))
    (snet->smat
     (loop for a.b being the hash-keys of *w1.w2->count*
	   using (hash-value count)
	   collect (list (car a.b) count (cdr a.b)))
     :selfref? nil
     :symmetric? nil
     :max-bkg max-bkg)
    ))
     
(defun next-word (context smat &key (cycles 10) (trace-n 10) (noise-depth-limit 1))
  (let* ((factor (/ 1.0 (length context)))
	 (symvec (symvals->symvec
		  ;; Strength of latter words increases, which seems
		  ;; backwards but is correct.
		  (loop for word in context
			as p from factor by factor ;; Should end up at 1.0
			collect (list word p))
		  smat))
	 )
    ;;; FFF Make this work in accord with probabilities
    (nth-per-prob noise-depth-limit
		  ;; These come back normalized and we sort them here
		  (sort (symvcollect smat (spreadloop smat symvec cycles :trace-n trace-n))
			#'> :key #'cdr))))

;;; This pics from the top n in accord with their probabilities,
;;; except that it can only go n deep.

(defun nth-per-prob (noise-depth-limit ns-symvec)
  ;; ns- to remind us these are already normed and sorted
  (loop for elt in ns-symvec
	as i below noise-depth-limit
	if (> (cdr elt) (/ (random 100) 100.0))
	do (return elt)
	finally (return elt)))

(defparameter *rmat* (text-to-smat *the-raven*))

(defun rmat-setup (&key (factor 0.25) (max-bkg 0.1))
  (setf *rmat* (text-to-smat *the-raven* :factor factor :max-bkg max-bkg)))

(defun caw (context &key (context-limit 5) (cycles 1) (trace-n nil) (noise-depth-limit 1) (n-responses 25)
		      (supress-direct-repetition? t))
 (append context 
	 (loop as r below n-responses
	       with previous-word = nil
	       as next-word = (car (next-word context *rmat* :cycles cycles :trace-n trace-n
						     ;; FFF Should work by probability, not just Nth.
						     :noise-depth-limit noise-depth-limit))
	       when (or (null supress-direct-repetition?) (not (equal next-word previous-word)))
	       collect
	       (progn 
		 (setf previous-word next-word)
		 (setf context (reverse (loop for i below context-limit as word in (cons next-word (reverse context))
						     collect word)))
	         next-word))))

(defun gssr (smat sym) ;; get smat symbol row
  (sort 
   (loop for word in (smat-syms smat)
	 as val across (array-slice (smat-mat smat) (position sym (smat-syms smat)))
	 collect (cons word val))
   #'> :key #'cdr))
  
(defun array-slice (arr row)
    (make-array (array-dimension arr 1) 
      :displaced-to arr 
       :displaced-index-offset (* row (array-dimension arr 1))))

#|

;; Looks like (0.5 0.05 1 1 2 0.12) and (0.4 0.05 1 1 2 0.12) are winners

(caw '(once upon a midnit dreary)
     :cycles 1
     :context-limit 2
     :trace-n nil
     :noise-depth-limit 1
     :n-responses 25
     :supress-direct-repetition? t
     )

|#


;;; Now we need to optimize the parameters. The test is easy, because
;;; we have the original.

(defparameter *params*
  '(
    ;; Setup factors
    (:factor 0.1 0.5 0.1)
    (:max-bkg 0.01 0.1 0.01) ;; We're gonna skip searching this
    ;; Run factors
    (:cycles 1 4 1)
    (:noise-depth-limit 1 3 1)
    (:context-limit 1 4 1)
    ))

(defun pstart (p) (second (assoc p *params*)))
(defun pend (p) (third (assoc p *params*)))
(defun pstep (p) (fourth (assoc p *params*)))

(defun setup-raven (&key (factor 0.25) (max-bkg 0.1))
  (setf *rmat* (text-to-smat *the-raven* :factor factor :max-bkg max-bkg))
  )

(defun raven-search (prompt-length)
  (with-open-file
      (o "raven-search.drb" :direction :output :if-exists :supersede)
    ;; Setup is outer loop as it recreates the rmat
    (loop for factor from (pstart :factor) to (pend :factor) by (pstep :factor)
	  ;; do (loop for max-bkg from (pstart :max-bkg) to (pend :max-bkg) by (pstep :max-bkg)
	  with max-bkg = 0.05 ;; Temp instead of searching
	  do (setup-raven :factor factor :max-bkg max-bkg)
	  (loop for cycles from (pstart :cycles) to (pend :cycles) by (pstep :cycles)
		do (loop for noise-depth-limit from (pstart :noise-depth-limit) to (pend :noise-depth-limit) by (pstep :noise-depth-limit)
			 do (loop for context-limit from (pstart :context-limit) to (pend :context-limit) by (pstep :context-limit)
				  do (print (print (list factor max-bkg cycles noise-depth-limit context-limit
						  (score-raven :cycles cycles
							       :noise-depth-limit noise-depth-limit
							       :prompt-length prompt-length
							       :context-limit context-limit
							       )))
					    o)
				  ))))))

(defun score-raven (&key (prompt-length 10) (n-tests-to-average 10) cycles noise-depth-limit context-limit)
  (loop with sum = 0
	;; First half of the prompt 
	with maxlen = (- *rlen* (* 2 (1+ prompt-length))) 
	as testn below n-tests-to-average
	as selection = (loop for word in (nthcdr (random maxlen) *the-raven*)
			     as i below (* 2 prompt-length)
			     collect word)
	as prompt = (loop for i below prompt-length as w in selection collect w)
	as correct-response = (nthcdr prompt-length selection)
	as response = (caw prompt :cycles cycles :noise-depth-limit noise-depth-limit :n-responses prompt-length
			   :context-limit context-limit :supress-direct-repetition? t)
	do (incf sum (/ (loop for cw in correct-response
			      as rw in (nthcdr prompt-length response)
			      if (eq cw rw)
			      sum 1)
			prompt-length))
	finally (return (float (/ sum n-tests-to-average)))
	))
  
#|

(raven-search 5)
;; Looks like (0.5 0.05 1 1 2 0.12) and (0.4 0.05 1 1 2 0.12) are winners
(setf *params*
  '(
    ;; Setup factors
    (:factor 0.4 0.5 0.1)
    (:max-bkg 0.01 0.1 0.01) ;; We're gonna skip searching this
    ;; Run factors
    (:cycles 1 1 1)
    (:noise-depth-limit 1 1 1)
    (:context-limit 2 2 1)
    ))
(trace score-raven caw)
(raven-search 5)

|#

;;; =====================================================================
;;; Markov model RAVEN from Old Stemhacks code

(defparameter *structured-raven*
  '(
    ((Once upon a midnight dreary while I pondered weak and weary)
     (Over many a quaint and curious volume of forgotten lore)
     (While I nodded nearly napping suddenly there came a tapping)
     (As of some one gently rapping rapping at my chamber door)
     (Tis some visitor I muttered tapping at my chamber door)
     (Only this and nothing more))
    ((Ah distinctly I remember it was in the bleak December)
     (And each separate dying ember wrought its ghost upon the floor)
     (Eagerly I wished the morrow vainly I had sought to borrow)
     (From my books surcease of sorrow sorrow for the lost Lenore)
     (For the rare and radiant maiden whom the angels name Lenore)
     (Nameless here for evermore))
    ((And the silken sad uncertain rustling of each purple curtain)
     (Thrilled me filled me with fantastic terrors never felt before)
     (So that now to still the beating of my heart I stood repeating)
     (Tis some visitor entreating entrance at my chamber door)
     (Some late visitor entreating entrance at my chamber door)
     (This it is and nothing more))
    ((Presently my soul grew stronger hesitating then no longer)
     (Sir said I or Madam truly your forgiveness I implore)
     (But the fact is I was napping and so gently you came rapping)
     (And so faintly you came tapping tapping at my chamber door)
     (That I scarce was sure I heard you here I opened wide the door)
     (Darkness there and nothing more))
    ((Deep into that darkness peering long I stood there wondering fearing)
     (Doubting dreaming dreams no mortal ever dared to dream before)
     (But the silence was unbroken and the stillness gave no token)
     (And the only word there spoken was the whispered word Lenore)
     (This I whispered and an echo murmured back the word Lenore)
     (Merely this and nothing more))
    ((Back into the chamber turning all my soul within me burning)
     (Soon again I heard a tapping somewhat louder than before)
     (Surely said I surely that is something at my window lattice)
     (Let me see then what thereat is and this mystery explore)
     (Let my heart be still a moment and this mystery explore)
     (Tis the wind and nothing more))
    ((Open here I flung the shutter when with many a flirt and flutter)
     (In there stepped a stately Raven of the saintly days of yore)
     (Not the least obeisance made he not a minute stopped or stayed he)
     (But with mien of lord or lady perched above my chamber door)
     (Perched upon a bust of Pallas just above my chamber door)
     (Perched and sat and nothing more))
    ((Then this ebony bird beguiling my sad fancy into smiling)
     (By the grave and stern decorum of the countenance it wore)
     (Though thy crest be shorn and shaven thou I said art sure no craven)
     (Ghastly grim and ancient Raven wandering from the Nightly shore)
     (Tell me what thy lordly name is on the Nights Plutonian shore)
     (Quoth the Raven Nevermore))
    ((Much I marvelled this ungainly fowl to hear discourse so plainly)
     (Though its answer little meaning little relevancy bore)
     (For we cannot help agreeing that no living human being)
     (Ever yet was blessed with seeing bird above his chamber door)
     (Bird or beast upon the sculptured bust above his chamber door)
     (With such name as Nevermore))
    ((But the Raven sitting lonely on the placid bust spoke only)
     (That one word as if his soul in that one word he did outpour)
     (Nothing farther then he uttered not a feather then he fluttered)
     (Till I scarcely more than muttered Other friends have flown before)
     (On the morrow he will leave me as my Hopes have flown before)
     (Then the bird said Nevermore))
    ((Startled at the stillness broken by reply so aptly spoken)
     (Doubtless said I what it utters is its only stock and store)
     (Caught from some unhappy master whom unmerciful Disaster)
     (Followed fast and followed faster till his songs one burden bore)
     (Till the dirges of his Hope that melancholy burden bore)
     (Of Never nevermore))
    ((But the Raven still beguiling all my fancy into smiling)
     (Straight I wheeled a cushioned seat in front of bird and bust and door)
     (Then upon the velvet sinking I betook myself to linking)
     (Fancy unto fancy thinking what this ominous bird of yore)
     (What this grim ungainly ghastly gaunt and ominous bird of yore)
     (Meant in croaking Nevermore))
    ((This I sat engaged in guessing but no syllable expressing)
     (To the fowl whose fiery eyes now burned into my bosoms core)
     (This and more I sat divining with my head at ease reclining)
     (On the cushions velvet lining that the lamplight gloated oer)
     (But whose velvet violet lining with the lamplight gloating oer)
     (She shall press ah nevermore))
    ((Then methought the air grew denser perfumed from an unseen censer)
     (Swung by Seraphim whose footfalls tinkled on the tufted floor)
     (Wretch I cried thy God hath lent thee by these angels he hath sent thee)
     (Respite respite and nepenthe from thy memories of Lenore)
     (Quaff oh quaff this kind nepenthe and forget this lost Lenore)
     (Quoth the Raven Nevermore))
    ((Prophet said I thing of evil prophet still if bird or devil)
     (Whether Tempter sent or whether tempest tossed thee here ashore)
     (Desolate yet all undaunted on this desert land enchanted)
     (On this home by Horror haunted tell me truly I implore)
     (Is there is there balm in Gilead tell me tell me I implore)
     (Quoth the Raven Nevermore))
    ((Prophet said I thing of evil prophet still if bird or devil)
     (By that Heaven that bends above us by that God we both adore)
     (Tell this soul with sorrow laden if within the distant Aidenn)
     (It shall clasp a sainted maiden whom the angels name Lenore)
     (Clasp a rare and radiant maiden whom the angels name Lenore)
     (Quoth the Raven Nevermore))
    ((Be that word our sign of parting bird or fiend I shrieked upstarting)
     (Get thee back into the tempest and the Nights Plutonian shore)
     (Leave no black plume as a token of that lie thy soul hath spoken)
     (Leave my loneliness unbroken quit the bust above my door)
     (Take thy beak from out my heart and take thy form from off my door)
     (Quoth the Raven Nevermore))
    ((And the Raven never flitting still is sitting still is sitting)
     (On the pallid bust of Pallas just above my chamber door)
     (And his eyes have all the seeming of a demons that is dreaming)
     (And the lamplight oer him streaming throws his shadow on the floor)
     (And my soul from out that shadow that lies floating on the floor)
     (Shall be lifted nevermore))
    ))

;;; Simple raven learning (uses structured poem)

(defvar *thisword->nextwords* (make-hash-table :test #'equal))

(defun learn-raven ()
  (clrhash *thisword->nextwords*)
  (loop for paragraph in *structured-raven*
	do 
	(loop for sentence in paragraph
	      do
	      (loop for (cur nxt) on `(:start ,@sentence :end)
		    ;; Probabilities are modeled by number of copies -- so NOT PUSHNEW!
		    do (push nxt (gethash cur *thisword->nextwords*))))))

(defun compose-line ()
  (loop with cur = :start
        as options = (gethash cur *thisword->nextwords*)
        as nxt = (nth (random (length options)) options)
        until (eq nxt :end)
        do (setq cur nxt)
        collect cur))

(defun compose-poem (&key (free-length? t))
  (loop for paragraph in *structured-raven*
	do (format t "~%~%")
	(loop for sentence in paragraph
              as this-length = (length sentence)
              if (= this-length 0)
              do (print nil)
              else do (print (loop for random-line = (compose-line)
				   until (or free-length? (= this-length (length random-line)))
				   finally (return random-line))))))

(defun stwt () ;; show-*thisword->nextwords*-table
  (mapcar #'print
	  (sort 
	   (loop for this-word being the hash-keys of *thisword->nextwords*
		 using (hash-value next-words)
		 collect (list (length next-words) this-word next-words))
	   #'> :key #'car)))

(defun wt2dot () ;; word table to dot
  (with-open-file (o "raven.dot" :direction :output :if-exists :supersede)
    (format o "digraph BTree {~%")
    (loop for this-word being the hash-keys of *thisword->nextwords*
	  using (hash-value next-words)
	  do (loop for next-word in next-words
		   with seenwords = nil
		   unless (member next-word seenwords)
		   do (format o "~a -> ~a [penwidth=~a];~%"
			      this-word next-word (count next-word next-words))
		   (pushnew next-word seenwords)))
    (format o "~%}~%" )))

#|

(format t "~%~%===================================================~%~%")
(learn-raven)
(format t "~%~%===================================================~%~%")
(stwt)
(format t "~%~%===================================================~%~%")
(compose-poem :free-length? t)
(format t "~%~%===================================================~%~%")
(compose-poem :free-length? nil)
(format t "~%~%===================================================~%~%")
(wt2dot)
(uiop::launch-program "dot -Tpdf raven.dot -o raven.pdf")

|#

