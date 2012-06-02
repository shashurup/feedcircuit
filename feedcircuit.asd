
(asdf:defsystem :feedcircuit
  :description "feedcircuit: news grabber"
  :version "0.9"
  :author "George Kibardin <george-kibadin@yandex.ru>"
  :license "BSD"
  :depends-on (:drakma :puri :cxml :local-time :closure-html :cl-fad :zip :babel)
  :components ((:file "fc")))
