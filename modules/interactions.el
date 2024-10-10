;;; package -- Interactions

;;; Commentary:
;; The package contains all the necessary tools to interact with
;; Emacs, the main are:
;; - helm
;; - evil

;;; Code:

;; (defcustom lz/evil-side
;;   1
;;   "Weather or not to activate evil shortcuts."
;;   :type 'integer
;;   :group 'lz-group)

(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-b") #'helm-buffers-list)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
  
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-j") 'helm-next-line)

  (setq helm-multi-edit-save t)
  (setq helm-split-with-multiple-windows t)
  (setq helm-split-direction 'split-window-vertically)
  (setq helm-speed-or-color t)
  (setq helm-move-to-line-cycle t)
  (setq helm-use-line-number-face t)
  (setq helm-use-fuzzy-match t)
  (setq helm-ff-default-directory (getenv "HOME"))
  ;; To fix error at compile:
  ;; Error (bytecomp): Forgot to expand macro with-helm-buffer in
  ;; (with-helm-buffer helm-echo-input-in-header-line)
  (if (version< "26.0.50" emacs-version)
      (eval-when-compile (require 'helm-lib)))

  (defun helm-hide-minibuffer-maybe ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))

  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  ;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
  ;; (global-unset-key (kbd "C-x c"))

  ;; rebihnd tab to do persistent action
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; make TAB works in terminal
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  ;; list actions using C-z
  (define-key helm-map (kbd "C-z")  'helm-select-action)

  (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
  (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
  (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-google-suggest-use-curl-p t
        helm-scroll-amount 4 ; scroll 4 lines other window using
			     ; M-<next>/M-<prior>
        ;; helm-quick-update t ; do not display invisible candidates
	;; search for library in `require' and `declare-function' sexp.
        helm-ff-search-library-in-sexp t

        ;; you can customize helm-do-grep to execute ack-grep
        ;; helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
        ;; helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f"
        helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window

        helm-echo-input-in-header-line t
	helm-completion-style 'emacs

        ;; helm-candidate-number-limit 500 ; limit the number of displayed canidates
        helm-ff-file-name-history-use-recentf t
        helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
        helm-buffer-skip-remote-checking t

        helm-mode-fuzzy-match t

        helm-buffers-fuzzy-matching t ; fuzzy matching buffer names when non-nil
                                        ; useful in helm-mini that lists buffers
        helm-org-headings-fontify t
        ;; helm-find-files-sort-directories t
        ;; ido-use-virtual-buffers t
        helm-semantic-fuzzy-match t
        helm-M-x-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-lisp-fuzzy-completion t
        ;; helm-apropos-fuzzy-match t
        helm-buffer-skip-remote-checking t
        helm-locate-fuzzy-match t
        helm-display-header-line nil)

  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-c r") 'helm-recentf)
  (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
  (global-set-key (kbd "C-c h o") 'helm-occur)
  (global-set-key (kbd "C-c h o") 'helm-occur)

  (use-package helm-wikipedia
    :ensure t
    :config
    (global-set-key (kbd "C-c h w") 'helm-wikipedia-suggest))
  (global-set-key (kbd "C-c h g") 'helm-google-suggest)

  (global-set-key (kbd "C-c h x") 'helm-register)
  ;; (global-set-key (kbd "C-x r j") 'jump-to-register)

  (define-key 'help-command (kbd "C-f") 'helm-apropos)
  (define-key 'help-command (kbd "r") 'helm-info-emacs)
  (define-key 'help-command (kbd "C-l") 'helm-locate-library)

  ;; use helm to list eshell history
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

;;; Save current position to mark ring
  (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

  ;; show minibuffer history with Helm
  (define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
  (define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

  (define-key global-map [remap find-tag] 'helm-etags-select)

  (define-key global-map [remap list-buffers] 'helm-buffers-list))

(use-package helm-swoop
  :ensure t
  :config
  ;; (global-set-key (kbd "C-s") 'helm-swoop)
  (global-set-key (kbd "M-S") 'helm-swoop-back-to-last-point)
  (global-set-key (kbd "C-c C-s") 'helm-multi-swoop)
  (global-set-key (kbd "C-x M-s") 'helm-multi-swoop-all)

  (define-key helm-swoop-map (kbd "C-s") 'helm-multi-swoop-all-from-helm-swoop)
  (define-key helm-swoop-map (kbd "C-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

  (define-key helm-swoop-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-j") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-j") 'helm-next-line)

  (setq helm-multi-swoop-edit-save t)
  (setq helm-swoop-split-with-multiple-windows t)
  (setq helm-swoop-split-direction 'split-window-vertically)
  (setq helm-swoop-speed-or-color t)
  (setq helm-swoop-move-to-line-cycle t)
  (setq helm-swoop-use-line-number-face t)
  (setq helm-swoop-use-fuzzy-match t))

(use-package evil
  :ensure t
  :init (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (setf evil-default-state 'emacs)
  (evil-set-initial-state 'prog-mode 'normal)
  (evil-set-initial-state 'elisp-mode 'normal)
  (evil-set-initial-state 'LaTeX-mode 'normal))

;; (use-package marginalia
;;   :ensure t
;;   :config
;;   (marginalia-mode 1))

(defalias 'yes-or-no-p 'y-or-n-p)

(if (daemonp)
    (global-set-key (kbd "C-x C-c") 'delete-frame)
  (global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs))

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(global-set-key (kbd "<f5>") 'revert-buffer)

(defun lz/open-configs ()
  "Funzione per aprire il file di configurazione."
  (interactive)
  (find-file
   (expand-file-name
    (concat user-emacs-directory "init.el"))))

(defun lz/switch-buffers-2-win ()
  "When in split window view switch the buffers."
  (interactive)
  (let* ((other-window (other-window-for-scrolling))
	 (this-window (selected-window))
	 (other-buffer (window-buffer other-window))
	 (this-buffer (window-buffer this-window)))
    (set-window-buffer other-window this-buffer)
    (set-window-buffer this-window other-buffer)))
(global-set-key (kbd "C-x w i") 'lz/switch-buffers-2-win)

(provide 'interactions)
;;; interactions.el ends here
