;;;; trello-review.lisp

(in-package #:trello-review)

;;; "trello-review" goes here. Hacks and glory await!

(defvar *token* "a278ef3ab2e8fb0310b8317445ac74133d6790cb612bc6b11f67ed20b52da295")

(defvar *api-key* "2e8178984273f4bf22ac309c3f36f9b6")

(defvar *auth-params* (format nil "key=~a&token=~a" *api-key* *token*))

(defvar *api-url* "https://api.trello.com/1")

(defvar *boards* '("Docking Server" "Backend - Identity & Access Context"))

(defvar *lists* '("Backlog" "Pending" "Ongoing" "Testing" "Done"))

(defun get-user-boards ()
  "Request a list of boards the user is a member of. Returns a list of hashmaps."
  (yason:parse
    (flexi-streams:octets-to-string
      (drakma:http-request (format nil
                                   "~a/members/me/boards?fields=name&lists=open&~a"
                                   *api-url* *auth-params*)))))

(defun filter-boards (boards by)
  "Filter out the boards specified by the user."
  (reduce #'(lambda (result board)
              (if (find (gethash "name" board) by :test #'equalp)
                (append result (list board))
                result))
          boards
          :initial-value ()))

(defun board-list-to-hashtable (boards)
  (let ((ht (make-hash-table :test #'equalp)))
    (dolist (b boards)
      (let ((id (gethash "id" b))
            (lists (gethash "lists" b)))
        (setf (gethash "lists" b :test #'equalp) (lists-to-hashtable lists))
        (setf (gethash id ht) b)))
    ht))

(defun lists-to-hashtable (board-lists)
  "Transform the board-lists list into a hashmap where the key is the id of
   a list of the board."
  ; See here -> http://cl-cookbook.sourceforge.net/hashes.html on how to
  ; iterate through hash-tables. The with-hash-table-iterator macro looks
  ; promising
  (let ((ht (make-hash-table :test #'equalp)))
    (dolist (l board-lists)
      (let ((id (gethash "id" l)))
        (setf (gethash id ht) l)))))


(let (toc-entries
      boards
      (show-comments T))
  (dotimes (i 5)
    (push (list :board (format nil "board-~a" i)) toc-entries)
    (push (list :name (format nil "board-~a" i)
                :statuses (list (list :status "Ongoing")
                                (list :cards (loop for y from 1 to 3
                                                   collecting (list :name (format nil "card-~a" y)
                                                                    :desc (format nil "This is card-~a" y)
                                                                    :status (format nil "Card status")
                                                                    :comments (list (list :text "Hello World"
                                                                                          :date "2016-04-06T16:43:50,123Z"
                                                                                          :author "otti")
                                                                                    (list :text "This is quite neat"
                                                                                          :date "2016-04-06T16:44:12,123Z"
                                                                                          :author "jliu"))
                                                                    :show-comments show-comments))))) boards))
  (with-open-file (report "./report.html" :direction :output :if-exists :supersede)
    (html-template:fill-and-print-template #p"./report.tmpl"
                                           (list :toc-entries (reverse toc-entries)
                                                 :boards (reverse boards))
                                           :stream report)))
