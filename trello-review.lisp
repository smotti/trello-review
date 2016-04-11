;;;; trello-review.lisp

(in-package #:trello-review)

;;; "trello-review" goes here. Hacks and glory await!

(defvar *token* "a278ef3ab2e8fb0310b8317445ac74133d6790cb612bc6b11f67ed20b52da295")

(defvar *api-key* "2e8178984273f4bf22ac309c3f36f9b6")

(defvar *auth-params* (format nil "key=~a&token=~a" *api-key* *token*))

(defvar *api-url* "https://api.trello.com/1")

(defvar *boards* '("Docking Server" "Backend - Identity & Access Context"))

(defvar *lists* '("Backlog" "Pending" "Ongoing" "Testing" "Done"))

(defun get-boards ()
  "Request a list of boards the user is a member of. Returns a list of hashmaps."
  (flexi-streams:octets-to-string
    (drakma:http-request (format nil
                                 "~a/members/me/boards?fields=name&lists=open&~a"
                                 *api-url* *auth-params*))))

(defun object-key-fn (key)
  "Key fn for yason:parse to turn keys into keywords."
  (values (intern (string-upcase key) "KEYWORD")))

(defun parse-boards (boards)
  (yason:parse boards :object-key-fn #'object-key-fn :object-as :plist))

;TODO: How to non-destructevily filter lists of a board and return a list of
;the boards with their lists filtered.
(defun filter-lists (boards by)
  "Filter out the lists specified by the user."
  (labels ((iter (old new)
             (if (not (null old))
               (let ((board (concatenate
                              'list
                              (first old)
                              (remove-if-not #'(lambda (entry)
                                                 (find (getf entry :name) by :test #'equalp))
                                             (getf board :lists)))))
                 (iter (rest old) (append new board)))
               new)))
    (iter boards ())))

(filter-lists *parsed* *lists*)

(defun filter-boards (boards by)
  "Filter out the boards specified by the user."
  (reduce #'(lambda (result board)
              (if (find (getf board :name) by :test #'equalp)
                (append result (list board))
                result))
          boards
          :initial-value ()))

(defun toc-entries (boards)
  "Build a list of ToC entries"
  (labels ((build (c r n)
             (if (not (null c))
               (build
                 (first r)
                 (rest r)
                 (append n (list (list :board (getf c :name)))))
               n)))
    (build (first boards) (rest boards) '())))

(toc-entries (filter-boards *parsed* *boards*))

(let* ((boards (filter-boards *parsed* *boards*))
       (toc (toc-entries boards)))
  (html-template:fill-and-print-template #p"./report.tmpl" (list :toc toc
                                                                 :boards boards)))

(let (toc-entries
      boards
      (show-comments T))
  (dotimes (i 5)
    (push (list :board (format nil "board-~a" i)) toc-entries)
    (push (list :name (format nil "board-~a" i)
                :lists (list (list :list "Ongoing")
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
