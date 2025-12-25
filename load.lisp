(define-lw-system lw-image ()
  (:system "lw-crypto")
  (:system "lw-markup")
  (:file "jpg" :depends-on "package")
  (:file "package")
  (:file "png" :depends-on "package")
  (:file "svg" :depends-on "package"))


