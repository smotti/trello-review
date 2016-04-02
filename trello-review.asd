;;;; trello-review.asd

(asdf:defsystem #:trello-review
  :description "A small tool to create a weekly review based on cards from trello boards."
  :author "BIT"
  :license "MIT"
  :depends-on (#:drakma
               #:yason
               #:html-template)
  :serial t
  :components ((:file "package")
               (:file "trello-review")))

