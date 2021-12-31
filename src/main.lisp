(defpackage ust
  (:use :cl)
  (:export :cli))

(in-package ust)

(defun load-file (path &optional (default '()))
  (if (probe-file path)
      (with-open-file (in path)
        (with-standard-io-syntax
          (format t "Load config from ~A~%" path)
          (read in)))
      default))

(defun load-config ()
  (load-file
    #p"~/.config/ust/config"
    (list (list :commands
                (cons :lisp "ros")
                (cons :py "python")
                (cons :js "node")
                (cons :ts "node")
                #-os-windows (cons :sh "sh")
                #+os-windows (cons :cmd "cmd")
                #+os-windows (cons :ps1 "powershell"))
          (cons :repos
            (list #p"~/.dotfiles/scripts/"
                  #p"~/.config/ust/scripts/"
                  #p"~/.scripts/")))))

(defun load-cache () (load-file #p"~/.config/ust/cache" '((:scripts))))

(defun save-file (content path)
  (uiop:ensure-all-directories-exist (list path))
  (with-open-file (out path :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (print content out))))

(defun save-config () (save-file *config* #p"~/.config/ust/config"))
(defun save-cache () (save-file *cache* #p"~/.config/ust/cache"))

(defparameter *config* (load-config))
(defparameter *cache* (load-cache))

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

(defun update-cache (key value)
  (setf (cdr (assoc key *cache*)) value))

(defun scan-scripts ()
  (let* ((repos (cdr (assoc :repos *config*)))
         (commands (cdr (assoc :commands *config*)))
         (scripts
          (apply #'append
                  (loop for path in repos
                        for repo = (probe-file (uiop:ensure-directory-pathname path))
                        when repo
                        do (format t "Scaning scripts in ~A...~%" path)
                        when repo
                        collect (loop for file in (uiop:directory-files repo)
                                      when (assoc (intern (string-upcase (pathname-type file)) 'keyword) commands)
                                      collect (cons (pathname-name file) file))))))
    (update-cache :scripts (or scripts '()))))

(defun get-script-path (name)
  (let* ((tmp (str:split "." name))
         (name (car tmp))
         (type (cadr tmp)))
    (cdar (member (cons name type) (cdr (assoc :scripts *cache*))
                  :test (lambda (a b)
                          (and (equal (car a) (car b))
                               (or (not (cdr a))
                                   (equal (cdr a) (pathname-type (cdr b))))))))))

(defun run-script (script &rest args)
  (let* ((type (intern (string-upcase (pathname-type script)) 'keyword))
         (cmd (cdar (member type (cdr (assoc :commands *config*)) :key #'car))))
    (format t "Eval script ~A~%" script)
    (if cmd
        (shell (format nil "~A ~A~{ ~A~}" cmd script args))
      (format t "No available command for script ~A~%" script))))

(defun default (&rest arguments)
  (let ((cmd (car arguments))
        (args (cdr arguments)))
    (when cmd
      (let ((script (get-script-path cmd)))
        (if script
            (apply #'run-script (cons script args))
            (format t "Script ~A not founded~%" cmd))))))

(defun list-script ()
  (scan-scripts)
  (format t "Scripts:~%~{  ~A~%~}~%" (cdr (assoc :scripts *cache*))))

(defun cat-script (script)
  (let ((script (get-script-path script)))
    (format t "Script: ~A:~% ~A~%" script (str:from-file script))))

(defun dispatch (action &rest scripts)
  (dolist (script scripts)
    (default script action)))

(defun check (&rest args)
  (when (and (not (eq (intern "LIST" 'command) (car args)))
             (not (cdr (assoc :scripts *cache*))))
    (format t "Empty scripts, scaning...~%")
    (scan-scripts)))

(clish:defcli cli
  (:default #'default)
  (:pre #'check)
  (:post (lambda (&rest args) (save-cache)))
  (cat #'cat-script)
  (list #'list-script)
  (remove (lambda (&rest scripts) (apply #'dispatch (cons "remove" scripts))))
  (update (lambda (&rest scripts) (apply #'dispatch (cons "update" scripts))))
  (install (lambda (&rest scripts) (apply #'dispatch (cons "install" scripts)))))


