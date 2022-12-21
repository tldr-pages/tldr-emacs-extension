;;; flymake-tldr-lint.el --- A TlDr Flymake backend powered by tldr-lint  -*- lexical-binding: t; -*-

;; Copyright (c) 2022 Emily Grace Seville <EmilySeville7cfg@gmail.com>
;; Version: 0.3
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Based in part on:
;;   https://www.gnu.org/software/emacs/manual/html_node/flymake/An-annotated-example-backend.html
;;
;; Usage:
;;   (add-hook 'markdown-mode-hook 'flymake-tldr-lint-load)

;; URL: https://github.com/tldr-pages/tldr-emacs-extension

;;; Code:

(require 'flymake)

(defgroup flymake-tldr-lint nil
  "TlDr linter backend for Flymake."
  :prefix "flymake-tldr-lint-"
  :group 'tools)

(defcustom flymake-tldr-lint-program "tldr-lint"
  "The name of the TlDr linter executable."
  :type 'string)

(defcustom flymake-tldr-lint-ignored ""
  "List of ignored errors."
  :type 'string)

(defvar-local flymake-tldr-lint--proc nil)

(defun flymake-tldr-lint--replace-regexp-entire-buffer (pattern replacement)
  "Perform regular-expression replacement throughout buffer.
PATTERN: pattern to search
REPLACEMENT: replacement for pattern"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (replace-match replacement))))

(defun flymake-tldr-lint-remove-broken-ellipsis()
  "Remove broken ellipsis placeholder.
Remove {{...}} placeholders in the current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (flymake-tldr-lint--replace-regexp-entire-buffer
      "[ ]*{{\\.\\{3\\}}}[ ]*"
      "")
    (message "Save file to update list of TlDr errors")))

(defun flymake-tldr-lint-remove-broken-numbers()
  "Remove broken numbers in placeholders.
Replace {{placeholder_number}}
placeholders with {{placeholder}} in the current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (flymake-tldr-lint--replace-regexp-entire-buffer
      "{{\\([^{}_]+\\)_+\\(?:[0-9]+\\)}}"
      "{{\\1}}")
    (message "Save file to update list of TlDr errors")))

(defun flymake-tldr-lint-remove-broken-files()
  "Remove broken file placeholders.
Remove {{file}}, {{filename}}, and {{file_name}}
placeholders in the current buffer.
Trailing numbers are respected too."
  (interactive)
  (with-current-buffer (current-buffer)
    (flymake-tldr-lint--replace-regexp-entire-buffer
      "[ ]*\\([\"']?\\){{file_?\\(?:name\\)?\\(?:[0-9]*\\)}}\\1[ ]*"
      "")
    (message "Save file to update list of TlDr errors")))

(defun flymake-tldr-lint-remove-broken-directories()
  "Remove broken directory placeholders.
Remove {{dir}}, {{dirname}}, {{dir_name}},
{{directory}}, {{directoryname}}, and {{directory_name}}
placeholders in the current buffer.
Trailing numbers are respected too."
  (interactive)
  (with-current-buffer (current-buffer)
    (flymake-tldr-lint--replace-regexp-entire-buffer
      "[ ]*\\([\"']?\\){{dir\\(?:ectory\\)?_?\\(?:name\\)?\\(?:[0-9]*\\)}}\\1[ ]*"
      "")
    (message "Save file to update list of TlDr errors")))

(defun flymake-tldr-lint-remove-broken-all()
  "Apply all `tldr-remove-*` actions."
  (interactive)
  (flymake-tldr-lint-remove-broken-ellipsis)
  (flymake-tldr-lint-remove-broken-numbers)
  (flymake-tldr-lint-remove-broken-files)
  (flymake-tldr-lint-remove-broken-directories))

(defun flymake-tldr-lint-correct-broken-ellipsis()
  "Correct broken ellipsis placeholder.
Replace {{placeholdernumber1}} {{placeholdernumber2}} ...
placeholders with {{placeholdernumber1 placeholdernumber2 ...}}
in the current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (flymake-tldr-lint--replace-regexp-entire-buffer
      "{{\\([^{}]+\\)[0-9]+}}\\([ ]+\\(?:{{\\1[0-9]+}}\\)\\)+"
      "{{\\11 \\12 ...}}")
    (message "Save file to update list of TlDr errors")))

(defun flymake-tldr-lint-correct-broken-numbers()
  "Correct broken numbers in placeholders.
Replace {{placeholder_number}}
placeholders with {{placeholdernumber}} in the current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (flymake-tldr-lint--replace-regexp-entire-buffer
      "{{\\([^{}_]+\\)_+\\([0-9]+\\)}}" "{{\\1\\2}}")
    (message "Save file to update list of TlDr errors")))

(defun flymake-tldr-lint-correct-broken-files()
  "Remove broken file placeholders.
Replace {{file}}, {{filename}}, and {{file_name}}
placeholders with {{path/to/file}} in the current buffer.
Trailing numbers are respected too."
  (interactive)
  (with-current-buffer (current-buffer)
    (flymake-tldr-lint--replace-regexp-entire-buffer
      "\\([\"']?\\){{file_?\\(?:name\\)?\\([0-9]*\\)}}\\1"
      "{{path/to/file\\2}}")
    (message "Save file to update list of TlDr errors")))

(defun flymake-tldr-lint-correct-broken-directories()
  "Correct broken directory placeholders.
Replace {{dir}}, {{dirname}}, {{dir_name}},
{{directory}}, {{directoryname}}, and {{directory_name}}
placeholders with {{path/to/directory}} in the current buffer.
Trailing numbers are respected too."
  (interactive)
  (with-current-buffer (current-buffer)
    (flymake-tldr-lint--replace-regexp-entire-buffer
      "\\([\"']?\\){{dir\\(?:ectory\\)?_?\\(?:name\\)?\\([0-9]*\\)}}\\1"
      "{{path/to/directory\\2}}")
    (message "Save file to update list of TlDr errors")))

(defun flymake-tldr-lint-correct-broken-ranges()
  "Correct broken range placeholders.
Replace {{from-to}} placeholders with {{from..to}} in the current buffer.
If `from` or `to` is missing then
it's replaced with negative or positive infinity respectively."
  (interactive)
  (with-current-buffer (current-buffer)
    (flymake-tldr-lint--replace-regexp-entire-buffer
      "{{\\([0-9]+\\)-+\\([0-9]+\\)}}"
      "{{\\1..\\2}}")
    (flymake-tldr-lint--replace-regexp-entire-buffer
      "{{-+\\([0-9]+\\)}}"
      "{{-infinity..\\1}}")
    (flymake-tldr-lint--replace-regexp-entire-buffer
      "{{\\([0-9]+\\)-+}}"
      "{{\\1..infinity}}")
    (flymake-tldr-lint--replace-regexp-entire-buffer
      "{{-+}}"
      "{{any}}")
    (message "Save file to update list of TlDr errors")))

(defun flymake-tldr-lint-correct-broken-long-option-argument()
  "Correct term duplication for option-argument pair.
Replace --option option syntax with --option any in the current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (flymake-tldr-lint--replace-regexp-entire-buffer
      "--\\([A-Za-z0-9]\\{2,\\}\\)[ ]+\\([\"']?\\)\\1\\2"
      "--\\1 \\2any\\2")
    (message "Save file to update list of TlDr errors")))

(defun flymake-tldr-lint-correct-broken-mnemonics ()
  "Correct broken mnemonics in description."
  (interactive)
  (let ((description-pattern "^- .*:$")
      (command-pattern "^`[^`]+`$")
      (descriptions '())
      (new-descriptions '())
      (commands '())
      (option-lists '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward description-pattern nil t)
        (push (match-string 0) descriptions)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward command-pattern nil t)
        (push (match-string 0) commands)))
    (if (not (equal (length descriptions) (length commands)))
      (error (format "Code example count (%d) is not equal to description count (%d)." (length commands) (length descriptions))))
    
    (dolist (command commands) ;; extract short and long options
      (with-temp-buffer
        (let ((option-pattern "--?\\([[:alnum:]]+\\)")
            (option-list '()))
          (insert command)
          (goto-char (point-min))
          (while (re-search-forward option-pattern nil t)
            (push (match-string 1) option-list))
          (push option-list option-lists))))
    (dolist (description descriptions) ;; remove all existing mnemonics
      (push (replace-regexp-in-string "\\[\\([[:alpha:]]+\\)\\]" "\\1" description) new-descriptions))
    
    (message (format "option-lists: %s | descriptions: %s" option-lists descriptions))

    (dotimes (i (length new-descriptions)) ;; building descriptions with new mnemonics
      (dolist (option (elt option-lists i))
        (setq result (elt new-descriptions i))

        (message (format "BEFORE option: %s | result: %s" option result))

        (setq old-result result)
        (setq result (replace-regexp-in-string (concat "\\(" option "\\)[[:alpha:]]+.*\\'") (concat "[" option "]") result nil nil 1))
        (if (string= result old-result)
          (setq result (replace-regexp-in-string (concat "[[:alpha:]]+\\(" option "\\).*\\'") (concat "[" option "]") result nil nil 1)))
        
        (message (format "AFTER option: %s | result: %s" option result))
        (setf (nth i new-descriptions) result)))
    
    (message (format "new-descriptions: %s" new-descriptions))
    
    (dotimes (i (length new-descriptions))
      (with-current-buffer (current-buffer)
        (message (format "REPLACE %s WITH %s" (elt descriptions i) (elt new-descriptions i)))
        (flymake-tldr-lint--replace-regexp-entire-buffer
          (elt descriptions (- (length descriptions) i 1))
          (elt new-descriptions i))))

    (message "Save file to update list of TlDr errors")))

(defun flymake-tldr-lint-correct-broken-all()
  "Apply all `tldr-correct-*` actions."
  (interactive)
  (flymake-tldr-lint-correct-broken-numbers)
  (flymake-tldr-lint-correct-broken-files)
  (flymake-tldr-lint-correct-broken-directories)
  (flymake-tldr-lint-correct-broken-ranges)
  (flymake-tldr-lint-correct-broken-ellipsis)
  (flymake-tldr-lint-correct-broken-long-option-argument))

(defun flymake-tldr-lint-convert-long-option-space-separated()
  "Convert long option-argument pair to space separated.
Replace --option=value syntax with --option value any in the current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (flymake-tldr-lint--replace-regexp-entire-buffer
      "--\\([A-Za-z0-9]\\{2,\\}\\)=\\([^\"' ]+\\)"
      "--\\1 \\2")
    (flymake-tldr-lint--replace-regexp-entire-buffer
      "--\\([A-Za-z0-9]\\{2,\\}\\)=\"\\([^\"]+\\)\""
      "--\\1 \"\\2\"")
    (flymake-tldr-lint--replace-regexp-entire-buffer
      "--\\([A-Za-z0-9]\\{2,\\}\\)='\\([^\"]+\\)'"
      "--\\1 '\\2'")
    (message "Save file to update list of TlDr errors")))

(defun flymake-tldr-lint-convert-long-option-equal-sign-separated()
  "Convert long option-argument pair to sign separated.
Replace --option value syntax with --option=value any in the current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (flymake-tldr-lint--replace-regexp-entire-buffer
      "--\\([A-Za-z0-9]\\{2,\\}\\)[ ]+\\([^\"' ]+\\)"
      "--\\1=\\2")
    (flymake-tldr-lint--replace-regexp-entire-buffer
      "--\\([A-Za-z0-9]\\{2,\\}\\)[ ]+\"\\([^\"]+\\)\""
      "--\\1=\"\\2\"")
    (flymake-tldr-lint--replace-regexp-entire-buffer
      "--\\([A-Za-z0-9]\\{2,\\}\\)[ ]+'\\([^\"]+\\)'"
      "--\\1='\\2'")
    (message "Save file to update list of TlDr errors")))

(defun flymake-tldr-lint--backend (report-fn &rest _args)
  "TlDr lint backend for Flymake.
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
                              "^.+?:\\([0-9]+\\): \\(TLDR[0-9]+\\) \\(.*\\)$"
                              nil t)
                       for id = (match-string 2)
                       for msg = (match-string 3)
                       for (beg . end) = (flymake-diag-region
                                          source
                                          (string-to-number (match-string 1)) 1)
                       for type = :error
                       if (null (string-match (regexp-quote id) flymake-tldr-lint-ignored))
                       collect (flymake-make-diagnostic source
                                                        beg
                                                        end
                                                        type
                                                        (format "[%s] %s" id msg))
                       into diags
                       
                       finally (funcall report-fn diags)))
                  (flymake-log :warning "Canceling obsolete check %s"
                               proc))
              (kill-buffer (process-buffer proc))))))))))

;;;###autoload
(defun flymake-tldr-lint-load ()
  "Add the tldr-lint backend into Flymake's diagnostic functions list."
  (add-hook #'flymake-diagnostic-functions #'flymake-tldr-lint--backend nil t))

(provide 'flymake-tldr-lint)

;;; flymake-tldr-lint.el ends here
