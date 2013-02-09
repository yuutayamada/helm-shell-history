;;; -*- coding: utf-8 mode: emacs-lisp -*-
;;; helm-shell-history.el --- find shell history from helm

;; Copyright (C) 2012 by Yuta Yamada

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
;; WIP

;;; Usage:
;; WIP

;;; Preference setting:
;; WIP

(eval-when-compile (require 'cl))
(require 'helm)
(require 'term nil t)

(defvar helm-shell-history-file
  (shell-command-to-string "echo -n $HISTFILE")
  "Specify your the history filepath of bash or zsh etc.
 By default it is specified variable of $HISTFILE")

(defvar helm-shell-history-command
  (lambda (pattern)
    (let*
        ((patterns (split-string pattern))
         (create-grep-command
          (lambda (minibuffer-patterns)
            (loop for search-word in minibuffer-patterns
                  collect (concat "\\grep -E -e \"" search-word "\" | "))))
         (grep-commands
          (mapconcat 'identity (funcall create-grep-command patterns) "")))
      (concat "\\tac " helm-shell-history-file " | "
              grep-commands
              "\\sed 's/^: [0-9]*:[0-9];//'"))))

(defvar helm-c-shell-history
  '((name . "helm-shell-history")
    (candidates-process . (lambda ()
                            (funcall helm-shell-history-process)))
    (nohighlight)
    (candidates-in-buffer)
    (action . (lambda (line)
                (funcall helm-shell-history-action-function line)))
    (delayed)))

(defvar helm-shell-history-action-function
  (lambda (line)
    (case major-mode
      (term-mode (term-send-raw-string line))
      (t         (insert line)))))

;;;###autoload
(defun helm-shell-history ()
  "Display command line history from your history file that you ware specified
at `helm-shell-history-file'"
  (interactive)
  (let ((helm-shell-history-process
         (lambda ()
           (start-process
            "helm-shell-history-process" nil "/bin/sh" "-c"
            (funcall helm-shell-history-command
                     helm-pattern)))))
    (helm :sources helm-c-shell-history
          :input-idle-delay 0.3
          :prompt "shell command: "
          :buffer "*helm shell history*")))

(provide 'helm-shell-history)

;;; helm-shell-history.el ends here
