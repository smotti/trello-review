(asdf:defsystem #:trello-review
  :description "A small tool to create a weekly report based on cards from trello boards."
  :author "BIT"
  :license "MIT"
  :depends-on (#:drakma
               #:yason
               #:html-template
               #:flexi-streams)
  :serial t
  :components ((:file "package")
               (:file "trello-review")))
