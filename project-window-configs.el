;;
;; project-window-configs.el
;; -------------------------

;; Author: Michael Dickens <emacs@mdickens.me>
;; Created: 2023-10-03
;; Version: 0.0.1
;; Package-Requires: ((projectile))

;;; Commentary:
;;
;; Save and load window configurations for Projectile projects.
;;
;; A window configuration may be associated with a Projectile project root (as
;; given by `(projectile-project-root)'), or with both a Projectile project root
;; and a Git branch (as given by `(car (vc-git-branches))'). You may save the
;; current window configuration in three different ways:
;;
;; 1. `save-project-window-config': Associate the current window config with
;;    the current project.
;; 2. `save-project-branch-window-config': Associate the current window config
;;    with the current project-branch pair.
;; 3. `save-project-and-project-branch-window-config': Do both #1 and #2.
;;
;; Then you may use `load-project-window-config' to load the window config of
;; the current project. `load-project-window-config' first checks if there's a
;; saved config for the current project-branch pair and, if so, loads it. If
;; not, it loads the saved config for the current project (if there is one).
;;
;; If you want to load a project's window config upon calling
;; `projectile-switch-project', you can do that by setting
;; `projectile-switch-project-action' to `window-config-switch-project-action'.
;; If you attempt to switch to a project that does not have a saved window
;; config, it will call `window-config-switch-project-action-fallback', which is
;; a configurable variable with a default value of `projectile-find-file'.

;;; Code:


(defcustom project-window-configs-table (make-hash-table :test #'equal)
  "Saved window configurations for Projectile projects. It is
recommended that you do not manually modify this variable. This
is an alist where the key is either the project root or the cons
pair (project root . git branch) and the value is a cons pair (window
config . filenames for visible buffers)."
  :group 'project-window-configs
  :type 'sexp)


(defcustom window-config-switch-project-action-fallback 'projectile-find-file
  "If `projectile-switch-project-action' is set to
`window-config-switch-project-action', this action is invoked as
a fallback action if a window configuration could not be found.
This variable can be set to any value that
`projectile-switch-project-action' could be set to."
  :group 'project-window-configs
  :type 'function)


(defun -get-window-state ()
  (window-state-get (frame-root-window) t))

(defun get-window-files ()
  "Get a list of all files associated with windows in the current frame."
  (mapcar
   (lambda (window)
     (buffer-file-name (window-buffer window)))
   (window-list)))

(defun open-listed-files (filenames)
  "Open every file in a list."
  (dolist (filename filenames)
    (when (not (null filename))
      ;; TODO: filenames are nil sometimes, idk why
      (find-file filename))))


(defun clear-project-configs-matching-condition (condition)
  "Clear all saved window configs where (condition key) returns non-nil."
  (setq keys-to-remove nil)

  (maphash
   (lambda (k v)
     (when (funcall condition k)
       (setq keys-to-remove (cons k keys-to-remove))))
   project-window-configs-table)

  (mapcar
   (lambda (k) (remhash k project-window-configs-table))
   keys-to-remove))


(defun clear-saved-branch-configs-for-project (project-root)
  "Clear all saved window configs associated with a branch for the
given project, but do not clear the saved config for the project."
  (interactive "fProject root:")
  (setq user-supplied-project-root project-root)
  (setq project-root (projectile-project-root user-supplied-project-root))
  (when (null project-root)
    (error (format "Could not find project" user-supplied-project-root)))

  (clear-project-configs-matching-condition
   ;; Add any key that is an alist and where the first cell contains
   ;; 'project-root.
   (lambda (k) (and (listp k)
                    (string= project-root (car k))))))


(defun clear-all-saved-configs-for-project (project-root)
  "Clear all saved window configs for the given project, including
the branch configs and the default config."
  (interactive "fProject root:")
  (setq user-supplied-project-root project-root)
  (setq project-root (projectile-project-root user-supplied-project-root))
  (when (null project-root)
    (error (format "Could not find project" user-supplied-project-root)))

  (clear-project-configs-matching-condition
   (lambda (k)
     (or (and (stringp k)
              (string= project-root k))
         (and (listp k)
              (string= project-root (car k)))))))


(defun clear-configs-for-deleted-projects ()
  "Clear all saved window configs for Projectile projects that no longer exist."
  (interactive)
  (clear-project-configs-matching-condition
   (lambda (k)
     (or (and (stringp k)
              (not (projectile-project-p k)))
         (and (listp k)
              (not (projectile-project-p (car k))))))))

(defun show-projects-with-window-configs ()
  "Print a list of projects and project-branch pairs that have saved
window configs and return the list."
  (interactive)
  (let ((projects-list nil))
    (maphash
     (lambda (k v) (setq projects-list (cons k projects-list)))
     project-window-configs-table)
    (prin1 projects-list)
    projects-list))


(defun save-project-branch-window-config ()
  "Save the window configuration for the current project and VC branch."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (branch (car (vc-git-branches)))  ; branch = nil if project doesn't have VC
         (key (cons project-root branch)))
    (puthash key (cons (-get-window-state) (get-window-files)) project-window-configs-table)
    ;; not sure this is required
    (customize-save-variable 'project-window-configs-table project-window-configs-table)
    (message (format "Saved window configuration for project %s, branch %s" project-root branch))))


(defun save-project-window-config ()
  "Save the window configuration for the current project (ignoring VC branch)."
  (interactive)
  (setq key (projectile-project-root))
  (puthash key (cons (-get-window-state) (get-window-files)) project-window-configs-table)
  ;; not sure this is required
  (customize-save-variable 'project-window-configs-table project-window-configs-table)
  (message (format "Saved window configuration for project %s" key)))


(defun save-project-and-project-branch-window-config ()
  "Save the window configuration for both the current project and the current project-VC branch pair."
  (interactive)
  (save-project-window-config)
  (save-project-branch-window-config))


(defvar project-config-to-load nil
  "Temporary storage for project configs to be used by `load-saved-project-config-as-hook'.")


(defun load-saved-project-config-as-hook ()
  "This function is used for shenanigans to make
`load-project-window-config' work correctly with Projectile.

We can't run 'window-state-put from inside
'projectile-switch-project-action because
'projectile-switch-project messes with the window state in a way
that interacts poorly with 'window-state-put. Instead,
'load-project-window-config adds a this function as a hook to run
after 'projectile-switch-project. This function calls
'window-state-put and then removes itself from
'projectile-after-switch-project-hook.
"
  (unwind-protect
      (progn
        (open-listed-files (cdr project-config-to-load))
        (window-state-put (car project-config-to-load)))
    (remove-hook 'projectile-after-switch-project-hook
                 'load-saved-project-config-as-hook)))

;; TODO: This can fail if it tries to load a file that doesn't exist on the
;; current branch. It should catch errors and fail gracefully by calling
;; window-config-switch-project-action-fallback.
(defun load-project-window-config ()
  "Load the current window config from the saved config for the
current project and VC branch, or for the current project if
there is no saved config for the current project-branch pair.
Return t on success, nil if there is no saved window config."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (branch (car (vc-git-branches)))  ; branch = nil if project doesn't have VC
         (key (cons project-root branch))
         (primary-config (gethash key project-window-configs-table))
         (fallback-config (gethash project-root project-window-configs-table))
         (config (or primary-config fallback-config)))
    (if (null config)
        (progn
          (message (format "load-project-window-config: Could not find a window config for project %s, branch %s" project-root branch))
          nil)
      (add-hook 'projectile-after-switch-project-hook 'load-saved-project-config-as-hook)
      (setq project-config-to-load config)
      (message (format "Loading %s window configuration for project %s, branch %s"
                       (if (null primary-config) "fallback" "branch-specific")
                       project-root branch))
      t)))


(defun window-config-switch-project-action ()
  "If the project has a saved window config, load it using
'load-project-window-config. Otherwise, call
'window-config-switch-project-action-fallback."
  (interactive)
  (if (not (load-project-window-config))
      (funcall window-config-switch-project-action-fallback)))

(provide 'project-window-configs)


;;; project-window-configs.el ends here
