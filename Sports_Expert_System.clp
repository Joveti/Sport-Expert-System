;;;======================================================
;;;   Sports Expert System
;;;======================================================
;;****************
;;* DEFTEMPLATES *
;;****************
(deftemplate run-ans
   (slot answer))

(deftemplate contact-ans
   (slot answer))

(deftemplate alone-ans
   (slot answer))

(deftemplate throwing-ans
   (slot answer))

(deftemplate playing-ans
   (slot answer))

(deftemplate water-ans
   (slot answer))

(deftemplate music-ans
   (slot answer))

(deftemplate sports
   (slot suggestion))

;;****************
;;* DEFFUNCTIONS *
;;****************
(deffunction str-lower-case (?str)
   (lowcase ?str))

(deffunction ask-question (?question $?values)
   (printout t ?question " ")
   (bind ?answer (read))
   (if (lexemep ?answer)
       then (bind ?answer (str-lower-case ?answer)))
   (while (not (member$ ?answer (create$ yes no both y n b))) do
      (printout t ?question " ")
      (bind ?answer (read))
      (if (lexemep ?answer)
          then (bind ?answer (str-lower-case ?answer))))
   (printout t ?answer crlf)  ;; Print the answer without the trailing "t"
   ?answer)

(deffunction answers (?question)
   (bind ?response (ask-question ?question (create$ yes no both y n b)))
   (if (or (member$ ?response (create$ yes y))
           (member$ ?response (create$ both b)))
       then return yes
       else return no))

;;;***************
;;;* QUERY RULES *
;;;***************
(defrule running
   (not (run-ans (answer ?)))
   (not (sports (suggestion ?)))
   =>
   (assert (run-ans (answer (answers "Do you like running? (yes/no)")))))

(defrule contact
   (run-ans (answer yes))
   (not (sports (suggestion ?)))
   =>
   (assert (contact-ans (answer (answers "Do you prefer contact sports? (yes/no)")))))

(defrule alone
   (run-ans (answer no))
   (not (sports (suggestion ?)))
   =>
   (assert (alone-ans (answer (answers "Do you like to play sports alone? (yes/no)")))))

(defrule throwing
   (contact-ans (answer yes))
   (not (sports (suggestion ?)))
   =>
   (assert (throwing-ans (answer (answers "Do you like throwing or kicking? (yes - throwing/no - kicking/both - both)")))))

(defrule playing
   (contact-ans (answer no))
   (not (sports (suggestion ?)))
   =>
   (assert (playing-ans (answer (answers "Do you like playing on a team or as an individual? (yes - team/no - individual)")))))

(defrule water
   (alone-ans (answer yes))
   (not (sports (suggestion ?)))
   =>
   (assert (water-ans (answer (answers "Do you like water? (yes/no)")))))

(defrule music
   (alone-ans (answer no))
   (not (sports (suggestion ?)))
   =>
   (assert (music-ans (answer (answers "Do you like exercising to music? (yes/no)")))))

;;;****************
;;;* REPAIR RULES *
;;;****************
(defrule basketball
   (throwing-ans (answer yes))
   (not (sports (suggestion ?)))
   =>
   (assert (sports (suggestion "You may like Basketball."))))

(defrule football
   (throwing-ans (answer no))
   (not (sports (suggestion ?)))
   =>
   (assert (sports (suggestion "You may like Football."))))

(defrule rugby
   (throwing-ans (answer both))
   (not (sports (suggestion ?)))
   =>
   (assert (sports (suggestion "You may like Rugby."))))

(defrule cricket
   (playing-ans (answer yes))
   (not (sports (suggestion ?)))
   =>
   (assert (sports (suggestion "You may like Cricket."))))

(defrule tennis
   (playing-ans (answer no))
   (not (sports (suggestion ?)))
   =>
   (assert (sports (suggestion "You may like Tennis."))))

(defrule ballroom
   (music-ans (answer yes))
   (not (sports (suggestion ?)))
   =>
   (assert (sports (suggestion "You may like Ballroom dancing."))))

(defrule yoga
   (music-ans (answer no))
   (not (sports (suggestion ?)))
   =>
   (assert (sports (suggestion "You may like Yoga."))))

(defrule swimming
   (water-ans (answer yes))
   (not (sports (suggestion ?)))
   =>
   (assert (sports (suggestion "You may like Swimming."))))

(defrule surfing
   (water-ans (answer no))
   (not (sports (suggestion ?)))
   =>
   (assert (sports (suggestion "You may like Surfing."))))

;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************
(defrule sports-system
   (declare (salience 10))
   =>
   (printout t crlf crlf)
   (printout t "Sports Expert System" crlf crlf))

(defrule print-sports
   (declare (salience 10))
   (sports (suggestion ?item))
   =>
   (printout t crlf crlf)
   (printout t "Suggested Sports:" crlf crlf)
   (printout t ?item crlf crlf))
