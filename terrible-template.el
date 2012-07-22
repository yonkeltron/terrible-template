(defvar *terrible-template-global-template-list* (make-hash-table :test 'equal))

(defconst terrible-template-config
  '(:variable-start-string "{{TERRIBLE_TEMPLATE_VARIABLE=< "
    :variable-end-string " >=TERRIBLE_TEMPLATE_VARIABLE}}"))

(defun terrible-template-make-interpolation-token (variable-name)
  "Create and format interpolation token for variable-name"
  (concat (plist-get  terrible-template-config :variable-start-string)
          (terrible-template-convert-to-safe-string variable-name)
          (plist-get terrible-template-config :variable-end-string)))

(defun terrible-template-valid-template-variable-pair-p (pair)
  "Determine whether or not a sexp constitutes a valid template variable pair"
  (terrible-template-log (concat "Checking validity of variable pair " (prin1-to-string pair t)))
  (and (listp pair)
       (equal
        (car pair)
        'var)
       (equal
        (length pair)
        2)))

(defun terrible-template-log (msg)
  "Logs MSG to the *terrible-template-output* buffer"
  (get-buffer-create "*terrible-template-output*")
  (with-current-buffer "*terrible-template-output*"
    (setq buffer-read-only nil)
    (insert (concat (prin1-to-string msg) "\n"))
    (setq buffer-read-only t)))

(defun terrible-template-error(message)
  (terrible-template-log (concat "Error: " message))
  (error message))

(defun terrible-template-convert-to-safe-string (obj)
  "Turn whatever key (symbol keyword, number or string) into a regex-safe string"
  (cond ((stringp obj) obj)
        ((keywordp obj) (substring (symbol-name obj) 1))
        ((symbolp obj) (symbol-name obj))
        ((numberp obj) (number-to-string obj))
        (t (terrible-template-error
            (concat "Unable to convert to string safely: "
                    (prin1-to-string obj))))))

(defun terrible-template-compile-template (structure)
  "Transform macro-parsed structure into a safe-to-serialize  "
  (list :template-string (apply 'concat (reverse (car structure)))
        :template-variables (cadr structure)))

(defun terrible-template-store-template (template-name template-structure)
  "Store template object in global template store"
  (puthash template-name
           (terrible-template-compile-template template-structure)
           *terrible-template-global-template-list*))

(defun terrible-template-substitute (template-string key-value-pair)
  "Actually perform substitution on template-string with key-value-pair"
  (replace-regexp-in-string (terrible-template-make-interpolation-token (car key-value-pair))
                            (cadr key-value-pair)
                            template-string))

(defun terrible-template-apply-template (template-string &rest key-value-pairs)
  (terrible-template-log (concat "Key value pairs: " (prin1-to-string key-value-pairs t)))
  (reduce 'terrible-template-substitute
          (car key-value-pairs)
          :initial-value (copy-sequence template-string)))

(defun terrible-template-insert (template-name)
  (interactive "sTemplate Name: ")
  (terrible-template-log (concat "Inserting template: " (prin1-to-string template-name t)))
  (let ((template-object (gethash template-name *terrible-template-global-template-list*)))
    (insert (terrible-template-apply-template
             (plist-get template-object :template-string)
             (terrible-template-prompt-for-variables
              (plist-get template-object :template-variables))))))

(defun terrible-template-prompt-for-variables (template-variables)
  "Prompt user for variables values"
  (terrible-template-log (concat "Prompting for variables " (prin1-to-string template-variables t)))
  (mapcar (lambda (variable)
            (list variable (read-string (concat variable ": "))))
          template-variables))

(defmacro defterrible (template-name &rest template-body)
  (let ((contents nil)
        (tags nil))
    (mapcar (lambda (obj)
              (cond ((stringp obj) (push obj contents))
                    ((terrible-template-valid-template-variable-pair-p obj)
                     (let ((var-name (terrible-template-convert-to-safe-string (cadr obj))))
                       (push (terrible-template-make-interpolation-token var-name) contents)
                       (push var-name tags)))
                    (t (terrible-template-error (concat "Template Error" (prin1-to-string obj t))))))
            template-body)
    `(terrible-template-store-template ,template-name (list ',contents ',tags))))

(provide 'terrible-template)
