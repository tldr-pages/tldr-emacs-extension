;;; flymake-tldr-lint.el --- A TlDr Flymake backend powered by tldr-lint  -*- lexical-binding: t; -*-

;; Copyright (c) 2022 Emily Grace Seville <EmilySeville7cfg@gmail.com>

;; Copyright (c) 2022 Emily Grace Seville <EmilySeville7cfg@gmail.com>
;; Package-Version: 0.1
;; Package-Requires: ((emacs "26"))

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
