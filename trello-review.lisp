(in-package #:trello-review)

(defvar *boards* '("Docking Server" "Backend - Identity & Access Context"))

(defvar *lists* '("Backlog" "Pending" "Ongoing" "Testing" "Done"))

;;; "trello-review" goes here. Hacks and glory await!

(defparameter *token* "")

(defparameter *api-key* "2e8178984273f4bf22ac309c3f36f9b6")

(defparameter *auth-params* (format nil "key=~a&token=~a" *api-key* *token*))

(defparameter *api-url* "https://api.trello.com/1")

(defparameter *show-comments* nil)

(defun read-config (path)
  (with-open-file (file path :direction :input)
    (read file)))

(defun init-config-params (config)
  (let ((token (getf config :token))
        (api-key (getf config :api-key))
        (api-url (getf config :api-url))
        (show-comments (getf config :show-comments)))
    (setq *token* token)
    (setq *api-key* api-key)
    (setq *auth-params* (format nil "key=~a&token=~a" api-key token))
    (setq *api-url* api-url)
    (setq *show-comments* show-comments)))

(defun get-boards ()
  "Request a list of boards the user is a member of."
  (flexi-streams:octets-to-string
    (drakma:http-request (format nil
                                 "~a/members/me/boards?fields=name&lists=open&~a"
                                 *api-url* *auth-params*))))

(defun get-list-cards (list-id)
  "Request a list of cards the user created or is a member of."
  (let ((url "~a/lists/~a/cards?fields=name,desc,due,idMembers&~a"))
   (flexi-streams:octets-to-string
    (drakma:http-request (format nil url *api-url* list-id *auth-params*)))))

(defun get-card-comments (card-id)
  (let ((url "~a/cards/~a/actions?filter=commentCard&fields=data,date&memberCreator_fields=fullName&~a"))
    (flexi-streams:octets-to-string
      (drakma:http-request (format nil url *api-url* card-id *auth-params*)))))

(defun get-user-id ()
  (flexi-streams:octets-to-string
    (drakma:http-request (format nil
                                 "~a/members/me?fields=username&~a"
                                 *api-url* *auth-params*))))

(defun object-key-fn (key)
  "Key fn for yason:parse to turn keys into keywords."
  (values (intern (string-upcase key) "KEYWORD")))

(defun parse-request-body (body)
  (yason:parse body :object-key-fn #'object-key-fn :object-as :plist))

(defun filter-lists (boards by)
  "Filter out the lists specified by the user."
  (labels ((remove-list-entry (entry) (find (getf entry :name)
                                            by
                                            :test #'equalp))
           (iter (boards result)
             (let ((board (first boards)))
               (if (not (null board))
                 ; then
                 (let ((lists (getf board :lists))
                       (new-board (list :id (getf board :id)
                                        :name (getf board :name))))
                   (iter (rest boards)
                         (append result
                                 (list
                                   (append new-board
                                           (list :lists (remove-if-not
                                                          #'remove-list-entry
                                                          lists)))))))
                 ; else
                 result))))
    (iter boards '())))

(defun filter-boards (boards by)
  "Filter out the boards specified by the user."
  (reduce #'(lambda (result board)
              (if (find (getf board :name) by :test #'equalp)
                (append result (list board))
                result))
          boards
          :initial-value ()))

(defun add-list-cards (board-list user-id)
  (labels ((member-p (card) (find user-id (getf card :idmembers) :test #'equalp)))
    (let ((cards (remove-if-not #'member-p
                                (parse-request-body
                                  (get-list-cards (getf board-list :id))))))
      (concatenate 'list board-list (list :cards cards)))))

(defun add-cards (boards user-id)
  ; For each board add list with cards to board
  (do ((board (first boards) (first remaining-boards))
       (remaining-boards (rest boards) (rest remaining-boards))
       (new-boards nil))
      ((null board) new-boards)
    (let ((id (getf board :id))
          (name (getf board :name))
          (lists (getf board :lists)))
      (labels ((cards (entry)
                 (add-list-cards entry user-id)))
        (push (list :id id :name name :lists (mapcar #'cards lists))
              new-boards)))))

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

; TODO: Need to htmlify board names for href of toc entries, so that they
; actually work.
; TODO: Add card comments to cards if requested.
(defun create-report (template output)
  (let* ((boards (add-cards
                   (filter-lists (filter-boards *parsed* *boards*) *lists*)
                   (getf (parse-request-body (get-user-id)) :id)))
         (toc (toc-entries boards)))
    (with-open-file (report output :direction :output :if-exists :supersede)
      (html-template:fill-and-print-template template
                                             (list :toc toc
                                                   :boards boards)
                                             :stream report))))

(defun main (argv)
  (print argv))

;(let (toc-entries
;      boards
;      (show-comments T))
;  (dotimes (i 5)
;    (push (list :board (format nil "board-~a" i)) toc-entries)
;    (push (list :name (format nil "board-~a" i)
;                :lists (list (list :list "Ongoing")
;                                (list :cards (loop for y from 1 to 3
;                                                   collecting (list :name (format nil "card-~a" y)
;                                                                    :desc (format nil "This is card-~a" y)
;                                                                    :status (format nil "Card status")
;                                                                    :comments (list (list :text "Hello World"
;                                                                                          :date "2016-04-06T16:43:50,123Z"
;                                                                                          :author "otti")
;                                                                                    (list :text "This is quite neat"
;                                                                                          :date "2016-04-06T16:44:12,123Z"
;                                                                                          :author "jliu"))
;                                                                    :show-comments show-comments))))) boards))
;  (with-open-file (report "./report.html" :direction :output :if-exists :supersede)
;    (html-template:fill-and-print-template #p"./report.tmpl"
;                                           (list :toc-entries (reverse toc-entries)
;                                                 :boards (reverse boards))
;                                           :stream report)))
