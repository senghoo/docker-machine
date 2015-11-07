(require 'dash)
(defcustom docker-machine-binary "docker-machine"
  "Docker machine binary file"
  :group 'docker-machine)

(defconst docker-machine-buffer-name "*docker-machine*"
  "Name of docker-machine mode buffer.")

(defconst docker-machine-list-format
  [("Active" 3 t :right-align t)
   ("Name" 5)
   ("Driver" 10)
   ("Status" 10)
   ("URL" 15)
   ("Swarm" 5)])

(defconst docker-machine-list-sort-key
  '("Name" . nil)
  "Docker machine sort key")

(defvar docker-machine-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'kill-this-buffer)
    (define-key map (kbd "e") 'docker-machine-env)
    map)
  "Map for `docker-machine-mode'")

(defun docker-machine-command (action &rest args)
  "Execute docker ACTION passing arguments ARGS."
  (let ((command (format "%s %s %s" docker-machine-binary action (s-join " " (-non-nil args)))))
    (message command)
    (shell-command-to-string command)))

(defun docker-machine-ls-parse (line)
  (mapcar* 'cons '(name driver state url swarm) (s-split "[\s\*]+" line)))

(defun docker-machine--ls ()
  (mapcar (lambda (line)
            (let ((info (docker-machine-ls-parse line)))
              (cons (cons 'ACTIVE (if (string-equal (getenv "DOCKER_MACHINE_NAME")
                                      (cdar info))
                        "*" ""))
                  info)))
          (cdr (s-split "\n"
                        (docker-machine-command "ls") t))))

(defun docker-machine--env (name)
  (delq nil
        (-map (lambda (line)
          (when (string-match "export \\(.+\\)=\"\\(.+\\)\"" line)
            (list (match-string 1 line) (match-string 2 line))))
              (s-split "\n" (docker-machine-command "env" name) t))))

(defun docker-machine-env ()
  "Set env at point"
  (interactive)
  (docker-machine-with-refresh
   (let ((name (tabulated-list-get-id)))
     (message "Active env %s" name)
     (-map (lambda (env)
             (message "Set env %s=%s" (first env) (second env))
             (setenv (first env) (second env)))
           (docker-machine--env name)))))

(defun docker-machine-list-entries ()
  (-map
   (lambda (machine)
     (list (cdadr machine) (apply 'vector (-map 'cdr machine))))
   (docker-machine--ls)))

(defun docker-machine-buffer ()
    "Return docker-machine buffer if it exists."
    (get-buffer docker-machine-buffer-name))

(defun docker-machine-referesh ()
  "Refresh docker-machine buffer"
  (interactive)
  (docker-machine--refresh (docker-machine-buffer)))

(define-derived-mode docker-machine-mode tabulated-list-mode "docker-machine"
  "Special mode for docker-machine"
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "Docker-machine")
  (setq major-mode 'docker-machine-mode)
  (use-local-map docker-machine-mode-map)
  (hl-line-mode 1)
  (setq tabulated-list-format docker-machine-list-format)
  (setq tabulated-list-entries 'docker-machine-list-entries)
  (setq tabulated-list-sort-key docker-machine-list-sort-key)
  (tabulated-list-init-header)
  (tabulated-list-print))

(defmacro docker-machine-with-refresh (&rest body)
  "Execute BODY and then refresh."
  `(progn ,@body (docker-machine--refresh (current-buffer))))

(defun docker-machine--refresh (buffer)
  "refresh buffer content"
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (tabulated-list-print :remember-pos)
      (hl-line-highlight))))

(defun docker-machine ()
  "Manage docker machine within Emacs."
  (interactive)
  (let ((buffer-p (docker-machine-buffer))
        (buffer (get-buffer-create docker-machine-buffer-name)))
    (pop-to-buffer buffer)
    (unless buffer-p
      (docker-machine-mode))))

(provide 'docker-machine)
