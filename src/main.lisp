(defpackage ust
  (:use :cl)
  (:export :cli))

(in-package ust)

(defun shell (&rest cmds)
  (let ((command (str:join ";" cmds)))
    (third
     (multiple-value-list
      (uiop:run-program
       (format nil "~A~A"
               #+os-windows "powershell $OLDPWD=pwd;"
               #-os-windows ""
               command)
       :output :interactive
       :ignore-error-status t
       :error-output :interactive)))))

(defun detect-repository ()
  (loop for path-str in (append
                         (list #p"~/.dotfiles/scripts"
                               #p"~/.config/ust/scripts"
                               #p"~/.scripts")
                         (uiop:getenv-pathnames "UST_SCRIPT"))
        for path = (probe-file path-str)
        when path
        collect path))

(defparameter *repository* (detect-repository))

(defun get-scripts (&optional (repos *repository*))
  (apply #'append
           (loop for repo in repos
                 collect (loop for file in (uiop:directory-files repo)
                               collect (cons (pathname-name file) file)))))

(defparameter *scripts* (get-scripts))

(defun run-script (script &rest args)
  (let* ((type (intern (string-upcase (pathname-type script))))
         (cmd (case type
                ('lisp "ros")
                ('py "python")
                ('js "node")
                ('ts "node")
                ('sh "sh"))))
    (shell (format nil "~A ~A~{ ~A~}" cmd script args))))

(defun get-script-path (name)
  (let* ((tmp (str:split "." name))
         (name (car tmp))
         (type (cadr tmp)))
    (cdar (member (cons name type) *scripts*
                  :test (lambda (a b)
                          (and (equal (car a) (car b))
                               (or (not (cdr a))
                                   (equal (cdr a) (pathname-type (cdr b))))))))))

(defun dispatch (&rest arguments)
  (let ((cmd (car arguments))
        (args (cdr arguments)))
    (if cmd
      (let ((script (get-script-path cmd)))
        (if script
            (apply #'run-script (cons script args))
            (format t "Script ~A not founded~%" cmd)))
      (format t "ss"))))

(defun list-script ()
  (format t "Scripts:~%~{  ~A~%~}~%" *scripts*))

(defun cat-script (script)
  (let ((script (get-script-path script)))
    (format t "Script: ~A:~% ~A~%" script (str:from-file script))))

(clish:defcli cli
  (:default #'dispatch)
  (list #'list-script)
  (cat #'cat-script))


