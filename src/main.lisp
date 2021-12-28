(defpackage ust
  (:use :cl)
  (:export :cli))

(in-package ust)

(defparameter *commands*
  (list
   (cons 'lisp "ros")
   (cons 'py "python")
   (cons 'js "node")
   (cons 'ts "node")
   #-os-windows (cons 'sh "sh")
   #+os-windows (cons 'cmd "cmd")
   #+os-windows (cons 'ps1 "powershell")))

(defun shell (&rest cmds)
  (let ((command (str:join ";" cmds)))
    (third
     (multiple-value-list
      (uiop:run-program
       (format nil "~A~A"
               #+os-windows "powershell $OLDPWD=pwd;"
               #-os-windows ""
               command)
       :input :interactive
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
                               when (member (intern (string-upcase (pathname-type file))) *commands* :key #'car)
                               collect (cons (pathname-name file) file)))))

(defparameter *scripts* (get-scripts))

(defun get-script-path (name)
  (let* ((tmp (str:split "." name))
         (name (car tmp))
         (type (cadr tmp)))
    (cdar (member (cons name type) *scripts*
                  :test (lambda (a b)
                          (and (equal (car a) (car b))
                               (or (not (cdr a))
                                   (equal (cdr a) (pathname-type (cdr b))))))))))

(defun run-script (script &rest args)
  (let* ((type (intern (string-upcase (pathname-type script))))
         (cmd (cdar (member type *commands* :key #'car))))
    (format t "Eval script ~A~%" script)
    (if cmd
        (shell (format nil "~A ~A~{ ~A~}" cmd script args))
      (format t "No available command for script ~A~%" script))))

(defun default (&rest arguments)
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

(defun dispatch (action &rest scripts)
  (dolist (script scripts)
    (default script action)))

(clish:defcli cli
  (:default #'default)
  (list #'list-script)
  (cat #'cat-script)
  (install (lambda (&rest scripts) (apply #'dispatch (cons "install" scripts))))
  (remove (lambda (&rest scripts) (apply #'dispatch (cons "remove" scripts))))
  (update (lambda (&rest scripts) (apply #'dispatch (cons "update" scripts)))))


