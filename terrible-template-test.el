(require 'ert)
(require 'terrible-template)

(ert-deftest terrible-template-make-interpolation-token ()
  "Test regex generation"
  (should
   (equal
    (terrible-template-make-interpolation-token "panda")
    "{{TERRIBLE_TEMPLATE_VARIABLE=< panda >=TERRIBLE_TEMPLATE_VARIABLE}}"))
  (should
   (equal
    (terrible-template-make-interpolation-token "bamboo")
    "{{TERRIBLE_TEMPLATE_VARIABLE=< bamboo >=TERRIBLE_TEMPLATE_VARIABLE}}"))
  (should
   (equal
    (terrible-template-make-interpolation-token "curry-noodle")
    "{{TERRIBLE_TEMPLATE_VARIABLE=< curry-noodle >=TERRIBLE_TEMPLATE_VARIABLE}}")))

(ert-deftest terrible-template-object-to-string-conversion ()
  "Test object-to-string conversion"
  (should
   (equal
    (terrible-template-convert-to-safe-string 'panda)
    "panda"))
  (should
   (equal
    (terrible-template-convert-to-safe-string :panda)
    "panda"))
  (should
   (equal
    (terrible-template-convert-to-safe-string "panda")
    "panda"))
  (should
   (equal
    (terrible-template-convert-to-safe-string 123)
    "123")))

(ert-deftest terrible-template-substitution ()
  (let ((test-template-string "{{TERRIBLE_TEMPLATE_VARIABLE=< panda >=TERRIBLE_TEMPLATE_VARIABLE}}"))
    (should
     (equal
      (terrible-template-substitute test-template-string '("panda" "bamboo"))
      "bamboo"))
    (should
     (equal
      (terrible-template-substitute test-template-string '("panda" "amazing string"))
      "amazing string"))))

(ert-deftest terrible-template-compile-template-test ()
  (let ((name '("panda"))
        (tags '("panda" "bamboo")))
    (should
     (equal
      (plist-get (terrible-template-compile-template (list name tags)) :template-string)
      "panda"))
    (should
     (equal
      (plist-get (terrible-template-compile-template (list name tags)) :template-variables)
      tags))))

(ert-deftest terrible-template-valid-template-variable-pair-p-test ()
  (let ((valid-pair '(var "panda"))
        (invalid-pair '(panda "crack"))
        (another-invalid-pair '(panda)))
    (should
     (terrible-template-valid-template-variable-pair-p valid-pair))
    (should
     (not (terrible-template-valid-template-variable-pair-p invalid-pair)))
    (should
     (not (terrible-template-valid-template-variable-pair-p another-invalid-pair)))
    (should
     (not (terrible-template-valid-template-variable-pair-p "panda")))
    (should
     (not (terrible-template-valid-template-variable-pair-p '(var 1 2 3))))))
