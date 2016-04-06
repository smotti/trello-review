;;;; trello-review.lisp

(in-package #:trello-review)

;;; "trello-review" goes here. Hacks and glory await!

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
