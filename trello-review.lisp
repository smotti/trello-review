;;;; trello-review.lisp

(in-package #:trello-review)

;;; "trello-review" goes here. Hacks and glory await!

(let (toc-entries
      boards)
  (dotimes (i 5)
    (push (list :board (format nil "board-~a" i)) toc-entries)
    (push (list :name (format nil "board-~a" i)) boards))
  (html-template:fill-and-print-template #p"./report.tmpl" (list :toc-entries toc-entries
                                                                 :boards boards)))
