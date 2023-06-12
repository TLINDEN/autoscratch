;;; autoscratch.el --- Automatically switch scratch buffer mode -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2023, T.v.Dein <tlinden@cpan.org>

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Version: 0.0.4
;; Package-Version: 20230505.800
;; Author: T.v.Dein <tlinden@cpan.org>
;; Keywords: convenience, buffer, scrach
;; URL: https://github.com/tlinden/autoscratch
;; License: GNU General Public License >= 2
;; Package-Requires: ((emacs "24.1"))
;; SPDX-License-Identifier: GPL-3.0

;;; Commentary:
;;;; Introduction:
;; This simple  major mode can be  used as the initial  major mode for
;; the scratch buffer.  It automatically switches to another major mode
;; based on regexes triggered by text input.
;;
;; In   the   default  configuration   it   responds   to  the   first
;; non-whitespace  character  entered  into  the  scratch  buffer  and
;; switches   major   mode   based   on  this.    See   the   variable
;; `autoscratch-triggers-alist' for the defaults.  For example, if you
;; enter a paren, it will switch to `emacs-lisp-mode'.

;;;; Configuration:
;;
;; The minimum configuration looks like this:
;;
;;     (require 'autoscratch)
;;     (setq initial-major-mode 'autoscratch-mode)
;;     (setq initial-scratch-message "")
;;     (setq inhibit-startup-screen t)
;;
;; You     may,     however,     configure    the     trigger     list
;; `autoscratch-triggers-alist' according  to your  preferences.  This
;; list consists  of cons cells, where  the `car' is a  regexp and the
;; `cdr' an Emacs Lisp form (e.g. a  lambda or defun).  If you want to
;; use regexps which  match more than one character, then  you need to
;; set `autoscratch-trigger-on-first-char' to  `nil' and possibly tune
;; `autoscratch-trigger-after' accordingly.

;; If   no  regexp   matches  and/or   `autoscratch-trigger-after'  is
;; exceeded,  then  the  `autoscratch-default-trigger'  form  will  be
;; executed, which  by default is  `fundamental-mode', but you  can of
;; course change this.

;; Autoscratch can  also be  configured to  rename the  current buffer
;; after  it  switched mode  based  on  a  trigger  and create  a  new
;; `autoscratch-buffer'  in the  background.  In  order to  enable this
;; feature, set `autoscratch-fork-after-trigger' to t.

;; To   further    tune   the   trigger   bahavior    you   can   tune
;; `autoscratch-trigger-hook' which will be called after executing the
;; form of the matching trigger.

;;; Code:
;;;; Customizables

(defgroup autoscratch nil
  "Autoscratch Mode."
  :prefix "autoscratch-"
  :group 'emacs)

(defcustom autoscratch-triggers-alist
  '(("[(;]"         . (emacs-lisp-mode))
    ("#"            . (autoscratch-select
                       '(("perl"   . (cperl-mode))
                         ("ruby"   . (ruby-mode))
                         ("python" . (python-mode))
                         ("conf"   . (conf-unix-mode))
                         ("shell"  . (shell-script-mode)))))
    ("[-a-zA-Z0-9]" . (text-mode))
    ("/"            . (c-mode))
    ("*"            . (progn (insert " ") (org-mode)))
    ("."            . (fundamental-mode)))
  "Default trigger alist for autoscratch mode.

This list triggers after the first character entered.
You can customize this variable directly or use `add-to-list'
to add more triggers.

In case there are more possibilities for a character, you can use
`autoscratch-select' to make autoscratch ask interactively for a
major mode to switch to, which see.  A good example for such a
character is # which is the comment-char in many modes."
  :group 'autoscratch
  :type 'list)

(defcustom autoscratch-trigger-on-first-char t
  "Trigger after the first character has been entered, no matter what."
  :group 'autoscratch
  :type 'boolean)

(defcustom autoscratch-default-trigger '(fundamental-mode)
  "Default form to execute when nothing else matches."
  :group 'autoscratch
  :type 'code)

(defcustom autoscratch-fork-after-trigger t
  "Create a new autoscratch buffer after the trigger fired."
  :group 'autoscratch
  :type :boolean)

(defcustom autoscratch-trigger-after 5
  "Max chars to be entered to force trigger the default form."
  :group 'autoscratch
  :type :variable)

(defcustom autoscratch-reset-default-directory nil
  "Reset default directory when a new scratch buffer is created.

When autoscratch creates a new scratch buffer, it can reset the
default directory to `~/' (change to home directory).  By default
it doesn't do this."
  :group 'autoscratch
  :type :boolean)

;;;; Public Vars

(defvar autoscratch-trigger-hook ()
  "Hooks called after executing a matching trigger form.")

(defvar autoscratch-rename-hook ()
  "Hooks called after renaming the current buffer.")

;;;; Public Functions

(defun autoscratch-buffer-rename ()
  "Rename current autoscratch buffer.

New name is '*autoscratch-<new-major-mode><N>*

Executes `autoscratch-rename-hook' afterwards."
  (interactive)
  (rename-buffer
   (generate-new-buffer-name
    (format "*%s-scratch*"
            (replace-regexp-in-string
             "-mode" ""
             (format "%s" major-mode))))
   (run-hooks 'autoscratch-rename-hook)))

(defun autoscratch-buffer ()
  "Create and switch to a new autoscratch buffer."
  (interactive)
  (let ((buf (get-buffer-create "*scratch*")))
    (switch-to-buffer buf)
    (autoscratch-mode)))

(defun autoscratch-select(modelist)
  "Interactively ask for major mode to switch to.

Argument MODELIST must be an alist where the car of each
pair must be a name and the cdr some arbitrary Emacs Lisp
form.

Example for MODELIST:

'((\"perl\"   . (cperl-mode))
  (\"ruby\"   . (ruby-mode))
  (\"python\" . (python-mode))
  (\"shell\"  . (shell-script-mode))

This function shall only be called from `autoscratch-triggers-alist'."
  (interactive)
  (let ((keys ()))
    (dolist (e modelist)
      (push (car e) keys))
    (eval (cdr (assoc (completing-read "switch mode to: " keys) modelist)))))


;;;; Internal Functions

(defun autoscratch--function-p (form)
  "Check if FORM is a function."
  (if (symbolp form)
      (fboundp form)
    (functionp form)))

(defun autoscratch--fork-and-rename-current ()
  "Rename buffer and create new autoscratch.
If `autoscratch-fork-after-trigger' is t, create a
new autoscratch buffer and rename the current one
to $mode-scratch."
  (interactive)
  (let ((cur (current-buffer)))
    (when (eq t autoscratch-fork-after-trigger)
      (autoscratch-buffer-rename)
      (autoscratch-buffer)
      (switch-to-buffer cur))))

(defun autoscratch--eval-trigger (form)
  "If FORM is a function execute interactively, otherwise evaluate it.
Executes `autoscratch-trigger-hook' after evaluation.

Supported values for FORM include:
 'emacs-lisp-mode
 '(lambda() (emacs-lisp-mode)
 '(emacs-lisp-mmode)"
  (if (autoscratch--function-p form)
      (funcall form)
    (eval form))
  (run-hooks 'autoscratch-post-trigger-hook)
  (message "autoscratch switched to %s" major-mode))

(defun autoscratch--look-for-triggers (forward)
  "Check if one of the configured trigger chars has been entered.

If FORWARD matchs one of the trigger chars, evaluated the the
associated Lisp form."
  (let ((matchform nil))
    (when (or (catch 'done
              (dolist (trigger autoscratch-triggers-alist)
                (when (if forward
                          (looking-at (car trigger))
                        (looking-back (car trigger) 1))
                  (setq matchform (cdr trigger))
                  (throw 'done t))))
            (eq t autoscratch-trigger-on-first-char))
        (autoscratch--eval-trigger (or matchform autoscratch-default-trigger))
        (autoscratch--fork-and-rename-current)
      ;; else: multichar allowed, continue until max
      (when (> (point) autoscratch-trigger-after)
        (autoscratch--eval-trigger autoscratch-default-trigger)))))

(defun autoscratch--self-insert-command (n)
  "Look for autoscratch trigger, execute if found and call `self-insert-command'.

N is the char the user just entered into the (new) scratch buffer."
  (interactive "p")
  (self-insert-command n)
  (autoscratch--look-for-triggers nil))

(defun autoscratch--yank (&optional arg)
  "Look for autoscratch trigger, execute if found and call `yank'.

ARG is the content of the clipboard being yanked."
  (interactive "*P")
  (let ((start (point))
        (end))
    (yank arg)
    (setq end (point))
    (goto-char start)
    (autoscratch--look-for-triggers t)
    (goto-char end)))

;;;; Keymap

;;;###autoload
(defvar autoscratch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'autoscratch--self-insert-command)
    (define-key map [remap yank] 'autoscratch--yank)
    map)
  "Keymap used by autoscratch mode.")

;;;; Major Mode

;;;###autoload
(define-derived-mode autoscratch-mode fundamental-mode "autoscratch"
  "Autoscratch major mode automatically switches major mode.

This simple major mode can be  used as the initial major mode for
the scratch  buffer.  It  automatically switches to  another major
mode based on regexes triggered by text input.

In   the  default   configuration  it   responds  to   the  first
non-whitespace  character entered  into  the  scratch buffer  and
switches   major  mode   based   on  this.    See  the   variable
`autoscratch-triggers-alist' for  the defaults.  For  example, if
you enter a paren, it will switch to `emacs-lisp-mode'.

Configuration:

The minimum configuration looks like this:

    (require 'autoscratch-mode)
    (setq initial-major-mode 'autoscratch-mode)

You     may,    however,     configure    the     trigger    list
`autoscratch-triggers-alist' according to your preferences.  This
list consists of cons cells, where  the `car' is a regexp and the
`cdr' an Emacs  Lisp form (e.g. a lambda or  defun).  If you want
to use regexps which match more than one character, then you need
to set  `autoscratch-trigger-on-first-char' to true  and possibly
tune `autoscratch-trigger-after' accordingly.

If  no  regexp   matches  and/or  `autoscratch-trigger-after'  is
exceeded,  then the  `autoscratch-default-trigger'  form will  be
executed, which by default is  `fundamental-mode', but you can of
course change this.

Autoscratch can also  be configured to rename  the current buffer
after  it switched  mode  based on  a trigger  and  create a  new
`autoscratch-buffer' in  the background.  In order  to enable this
feature, set `autoscratch-fork-after-trigger' to t.

\\{autoscratch-mode-map}"

  ;; fixes github#1
  (if autoscratch-reset-default-directory
      (setq default-directory "~/")))

(provide 'autoscratch)

;;; autoscratch.el ends here
