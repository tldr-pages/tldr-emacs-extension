;;; flymake-tldr-lint.el --- A TlDr Flymake backend powered by tldr-lint  -*- lexical-binding: t; -*-

;; Copyright (c) 2022 Emily Grace Seville <EmilySeville7cfg@gmail.com>

;; Copyright (c) 2022 Emily Grace Seville <EmilySeville7cfg@gmail.com>
;; Package-Version: 0.1
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

;;; Code:

(require 'flymake)

(defgroup flymake-tldr-lint nil
  "tldr-lint backend for Flymake."
  :prefix "flymake-tldr-lint-"
  :group 'tools)

(defcustom flymake-tldr-lint-program "tldr-lint"
  "The name of the `tldr-lint' executable."
  :type 'string)

(defcustom flymake-tldr-lint-ignored ""
  "List of ignored errors."
  :type 'string)

(defvar-local flymake-tldr-lint--proc nil)

(defun replace-regexp-entire-buffer (pattern replacement)
  "Perform regular-expression replacement throughout buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (replace-match replacement))))

(defun tldr-remove-broken-ellipsis()
  "Remove {{...}} placeholders in the current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (replace-regexp-entire-buffer "[ ]*{{\\.\\{3\\}}}[ ]*" "")
    (message "Save file to update list of TlDr errors")
  )
)

(defun tldr-remove-broken-numbers()
  "Replace {{placeholder_number}} placeholders with {{placeholder}} in the current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (replace-regexp-entire-buffer "{{\\([^{}_]+\\)_+\\(?:[0-9]+\\)}}" "{{\\1}}")
    (message "Save file to update list of TlDr errors")
  )
)

(defun tldr-remove-broken-files()
  "Remove {{file}}, {{filename}}, and {{file_name}} placeholders in the current buffer.
Trailing numbers are respected too."
  (interactive)
  (with-current-buffer (current-buffer)
    (replace-regexp-entire-buffer "[ ]*{{file_?\\(?:name\\)?\\(?:[0-9]+\\)}}[ ]*" "")
    (message "Save file to update list of TlDr errors")
  )
)

(defun tldr-remove-broken-directories()
  "Remove {{dir}}, {{dirname}}, {{dir_name}}, {{directory}}, {{directoryname}}, and {{directory_name}} placeholders in the current buffer.
Trailing numbers are respected too."
  (interactive)
  (with-current-buffer (current-buffer)
    (replace-regexp-entire-buffer "[ ]*{{dir\\(?:ectory\\)?_?\\(?:name\\)?\\(?:[0-9]+\\)}}[ ]*" "")
    (message "Save file to update list of TlDr errors")
  )
)

(defun tldr-correct-broken-ellipsis()
  "Replace {{placeholdernumber1}} {{placeholdernumber2}} ... placeholders with {{placeholdernumber1 placeholdernumber2 ...}} in the current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (replace-regexp-entire-buffer "{{\\([^{}]+\\)[0-9]+}}\\([ ]+\\(?:{{\\1[0-9]+}}\\)\\)+" "{{\\11 \\12 ...}}")
    (message "Save file to update list of TlDr errors")
  )
)

(defun tldr-correct-broken-numbers()
  "Replace {{placeholder_number}} placeholders with {{placeholdernumber}} in the current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (replace-regexp-entire-buffer "{{\\([^{}_]+\\)_+\\([0-9]+\\)}}" "{{\\1\\2}}")
    (message "Save file to update list of TlDr errors")
  )
)

(defun tldr-correct-broken-files()
  "Replace {{file}}, {{filename}}, and {{file_name}} placeholders with {{path/to/file}} in the current buffer.
Trailing numbers are respected too."
  (interactive)
  (with-current-buffer (current-buffer)
    (replace-regexp-entire-buffer "{{file_?\\(?:name\\)?\\([0-9]+\\)}}" "{{path/to/file\\1}}")
    (message "Save file to update list of TlDr errors")
  )
)

(defun tldr-correct-broken-directories()
  "Replace {{dir}}, {{dirname}}, {{dir_name}}, {{directory}}, {{directoryname}}, and {{directory_name}} placeholders with {{path/to/directory}} in the current buffer.
Trailing numbers are respected too."
  (interactive)
  (with-current-buffer (current-buffer)
    (replace-regexp-entire-buffer "{{dir\\(?:ectory\\)?_?\\(?:name\\)?\\([0-9]+\\)}}" "{{path/to/directory\\1}}")
    (message "Save file to update list of TlDr errors")
  )
)

(defun tldr-correct-broken-ranges()
  "Replace {{from-to}} placeholders with {{from..to}} in the current buffer.
If `from` or `to` is missing then it's replaced with negative or positive infinity respectively."
  (interactive)
  (with-current-buffer (current-buffer)
    (replace-regexp-entire-buffer "{{\\([0-9]+\\)-+\\([0-9]+\\)}}" "{{\\1..\\2}}")
    (replace-regexp-entire-buffer "{{-+\\([0-9]+\\)}}" "{{-infinity..\\1}}")
    (replace-regexp-entire-buffer "{{\\([0-9]+\\)-+}}" "{{\\1..infinity}}")
    (replace-regexp-entire-buffer "{{-+}}" "{{-infinity..infinity}}")
    (message "Save file to update list of TlDr errors")
  )
)

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
  (add-hook 'flymake-diagnostic-functions 'flymake-tldr-lint--backend nil t))

(provide 'flymake-tldr-lint)

;;; flymake-tldr-lint.el ends here
