;;; helm-shell-history.el --- find shell history from helm -*- lexical-binding: t; -*-

;; Copyright (C) 2012, 2015 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/helm-shell-history
;; Version: 0.0.1
;; Package-Requires: ((helm "20130208.1156"))
;; Keywords: helm

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; To find shell-history, you can use `M-x helm-shell-history' command

;;; Preference setting:
;; (add-to-list 'load-path "path/to/this/package")
;; (require 'helm-shell-history)
;; (add-hook 'term-mode-hook
;;           (lambda ()
;;             (define-key term-raw-map (kbd "C-r") 'helm-shell-history)))
;;; Code:
(require 'helm)
(require 'term nil t)
(require 'cl-lib)

(defvar helm-shell-history-file
  (shell-command-to-string "echo -n $HISTFILE")
  "Specify your the history filepath of bash or zsh etc.
By default it is specified variable of $HISTFILE")

(defvar helm-shell-history-command
  (lambda (pattern)
    (let* ((patterns (split-string pattern))
           (grep (when (string< "" pattern)
                   (helm-shell-history-make-grep-command patterns))))
      (mapconcat 'identity (delq nil
                                 `(,(concat "\\tac " helm-shell-history-file)
                                   ,grep
                                   "\\sed 's/^: [0-9]*:[0-9];//'"))
                 " | "))))

(defun helm-shell-history-make-grep-command (patterns)
  "Return grep command form PATTERNS."
  (cl-loop with cmd = "\\grep -E -e "
           for search-word in patterns
           collect (concat cmd " \"" search-word "\" ") into result
           finally return (mapconcat 'identity result " | ")))

(defvar helm-c-shell-history
  '((name . "helm-shell-history")
    (candidates-process . (lambda ()
                            (start-process
                             "helm-shell-history-process" nil "/bin/sh" "-c"
                             (funcall helm-shell-history-command
                                      helm-pattern))))
    (nohighlight)
    (candidates-in-buffer)
    (action . (lambda (line)
                (funcall helm-shell-history-action-function line)))
    (delayed)))

(defvar helm-shell-history-action-function
  (lambda (line)
    (cl-case major-mode
      (term-mode (term-send-raw-string line))
      (t         (insert line)))))

;;;###autoload
(defun helm-shell-history ()
  "Display command line history from history file.
You can specify at `helm-shell-history-file'."
  (interactive)
  (helm :sources helm-c-shell-history
        :prompt "shell command: "
        :buffer "*helm shell history*"))

(provide 'helm-shell-history)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; helm-shell-history.el ends here
