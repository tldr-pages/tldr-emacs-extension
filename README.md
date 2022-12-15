# tldr-emacs-extension

## Requirenments

- `tldr-lint` installed
- `emacs 27.1` higher

## Features

- [x] Code linting
- [ ] Code snippets
- [ ] Code actions

![image](https://user-images.githubusercontent.com/42812113/207983063-03efd5da-eed9-4c52-8913-8ae2e0a95a9f.png)

## Installation


### Copy-paste `.el` contents to `~/.emacs` while enabling `-*- lexical-binding: t`

Example `~/.emacs` config:

```emacs
;;; flymake-tldr-lint.el --- A TlDr Flymake backend powered by tldr-lint  -*- lexical-binding: t; -*-
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'flymake)

(defgroup flymake-tldr-lint nil
  "tldr-lint backend for Flymake."
  :prefix "flymake-tldr-lint-"
  :group 'tools)

(defcustom flymake-tldr-lint-program "tldr-lint"
  "The name of the `tldr-lint' executable."
  :type 'string)

(defvar-local flymake-tldr-lint--proc nil)

(defun flymake-tldr-lint--backend (report-fn &rest _args)
  "tldr-lint backend for Flymake.
Check for problems, then call REPORT-FN with results."
  (unless (executable-find flymake-tldr-lint-program)
    (error "Could not find tldr-lint executable"))

  (when (process-live-p flymake-tldr-lint--proc)
    (kill-process flymake-tldr-lint--proc)
    (setq flymake-tldr-lint--proc nil))

  (let* ((source (current-buffer))
	     (filename (buffer-file-name source)))
    (save-restriction
      (widen)
      (setq
       flymake-tldr-lint--proc
       (make-process
        :name "tldr-lint-flymake" :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer " *tldr-lint-flymake*")
        :command (remove nil (list flymake-tldr-lint-program
                       filename))
        :sentinel
        (lambda (proc _event)
          (when (eq 'exit (process-status proc))
            (unwind-protect
                (if (with-current-buffer source (eq proc flymake-tldr-lint--proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      (cl-loop
                       while (search-forward-regexp
                              "^.+?:\\([0-9]+\\): \\(.*\\)$"
                              nil t)
		               for severity = "error"
                       for msg = (match-string 2)
                       for (beg . end) = (flymake-diag-region
                                          source
                                          (string-to-number (match-string 1))
					                      (+ 1 (string-to-number (match-string 1))))
                       for type = (cond ((string= severity "note") :note)
					                    ((string= severity "warning") :warning)
					                    (t :error))
                       collect (flymake-make-diagnostic source
                                                        beg
                                                        end
                                                        type
                                                        msg)
                       into diags
                       finally (funcall report-fn diags)))
                  (flymake-log :warning "Canceling obsolete check %s"
                               proc))
              (kill-buffer (process-buffer proc))))))))))

;;;###autoload
(defun flymake-tldr-lint-load ()
  "Add the tldr-lint backend into Flymake's diagnostic functions list."
  (add-hook 'flymake-diagnostic-functions 'flymake-tldr-lint--backend nil t))

(provide 'flymake-tldr-lint)

(add-hook 'markdown-mode-hook 'flymake-tldr-lint-load)

(custom-set-variables
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
'(package-selected-packages '(markdown-mode)))
(custom-set-faces
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
)
```

## Starting linting

- Use `M-x flymake-mode RET` (press `Alt` with `x` and then type `flymake-mode` and press `Enter`)

## Settings

- `flymake-tldr-lint-program` (**default**: `tldr-lint`) - path to tldr-lint executable (useful in case it's not in `$PATH`)

