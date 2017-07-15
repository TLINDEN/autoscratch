;;; autoscratch-mode.el --- automatically switch scratch buffer mode

;; Copyright (C) 2017, T.v.Dein <tlinden@cpan.org>

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

;; Version: 0.01
;; Author: T.v.Dein <tlinden@cpan.org>
;; Keywords: files
;; URL: https://github.com/tlinden/autoscratch
;; License: GNU General Public License >= 2

;;; Commentary:
;;;; Introduction:
;; This simple  major mode can be  used as the initial  major mode for
;; the scratch buffer. It automatically switches to another major mode
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

;; You     may,     however,     configure    the     trigger     list
;; `autoscratch-triggers-alist' according  to your  preferences.  This
;; list consists  of cons cells, where  the `car' is a  regexp and the
;; `cdr' an emacs lisp form (e.g. a  lambda or defun).  If you want to
;; use regexps which  match more than one character, then  you need to
;; set `autoscratch-trigger-on-first-char' to  `nil' and possibly tune
;; `autoscratch-trigger-after' accordingly.

;; If   no  regexp   matches  and/or   `autoscratch-trigger-after'  is
;; exceeded,  then  the  `autoscratch-default-trigger'  form  will  be
;; executed, which  by default is  `fundamental-mode', but you  can of
;; course change this.

;; Autoscratch can  also be  configured to  rename the  current buffer
;; after  it  switched mode  based  on  a  trigger  and create  a  new
;; `autoscratch-buffer'  in the  background. In  order to  enable this
;; feature, set `autoscratch-fork-after-trigger' to t.


;;; Code:
;;;; Customizables

(defgroup autoscratch nil
  "Autoscratch Mode."
  :prefix "autoscratch-"
  :group 'emacs)

(defcustom autoscratch-triggers-alist
  '(("("            . (emacs-lisp-mode))
    (";"            . (emacs-lisp-mode))
    ("#"            . (conf-unix-mode))
    ("[-a-zA-Z0-9]" . (text-mode))
    ("/"            . (c-mode))
    ("*"            . (progn (insert " ") (org-mode)))
    ("."            . (fundamental-mode)))
  "Default trigger alist for autoscratch mode.

This list triggers after the first character entered."
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
  "Create a new autoscratch buffer after the trigger fired.")

(defcustom autoscratch-trigger-after 5
  "Max chars to be entered to force trigger the default form.")

;;;; Public Functions

(defun autoscratch-buffer-rename ()
  "Rename current autoscratch buffer.

New name is '*autoscratch-<new-major-mode><N>*"
  (interactive)
  (rename-buffer
   (generate-new-buffer-name
    (format "*%s-scratch*"
            (replace-regexp-in-string "-mode" "" (format "%s" major-mode))))))

(defun autoscratch-buffer ()
  "Create a new autoscratch buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*autoscratch*")
    (autoscratch-mode)))

(defun autoscratch-fork-and-rename-current ()
  "Rename buffer and create new autoscratch.

If `autoscratch-fork-after-trigger' is t, rename buffer
and create a new autoscratch buffer."
  (interactive)
  (when (eq t autoscratch-fork-after-trigger)
    (autoscratch-buffer-rename)
    (autoscratch-buffer)))

;;;; Internal Functions

(defun autoscratch--eval-trigger-and-rename (form)
  (eval form)
  (message (format "autoscratch switched to %s" major-mode))
  (autoscratch-fork-and-rename-current))

(defun autoscratch--look-for-triggers (forward)
  (let ((matchform nil)
        (renamed nil)
        (newname nil)
        (C 0))
    (if (or (catch 'done
              (dolist (trigger autoscratch-triggers-alist)
                (when (if forward
                          (looking-at (car trigger))
                        (looking-back (car trigger)))
                  (setq matchform (cdr trigger))
                  (throw 'done t))))
            (eq t autoscratch-trigger-on-first-char))
        (autoscratch--eval-trigger-and-rename (or matchform autoscratch-default-trigger))
      ;; else: multichar allowed, continue until max
      (when (> (point) autoscratch-trigger-after)
        (eval autoscratch-default-trigger)))))

(defun autoscratch--self-insert-command (N)
  "Look for autoscratch trigger, execute if found and call `self-insert-command'."
  (interactive "p")
  (self-insert-command N)
  (autoscratch--look-for-triggers nil))

(defun autoscratch--yank (&optional arg)
  "Look for autoscratch trigger, execute if found and call `yank'."
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
  "keymap used by autoscratch mode.")

;;;; Major Mode

;;;###autoload
(define-derived-mode autoscratch-mode fundamental-mode "autoscratch"
  "Autoscratch major mode automatically switches major mode.

This simple major mode can be  used as the initial major mode for
the scratch  buffer. It  automatically switches to  another major
mode based on regexes triggered by text input.

In   the  default   configuration  it   responds  to   the  first
non-whitespace  character entered  into  the  scratch buffer  and
switches   major  mode   based   on  this.    See  the   variable
`autoscratch-triggers-alist' for  the defaults.  For  example, if
you enter a paren, it will switch to `emacs-lisp-mode'.

Configuration:

The minimum configuration looks like this:

    (require 'autoscratch)
    (setq initial-major-mode 'autoscratch-mode)

You     may,    however,     configure    the     trigger    list
`autoscratch-triggers-alist' according to your preferences.  This
list consists of cons cells, where  the `car' is a regexp and the
`cdr' an emacs  lisp form (e.g. a lambda or  defun).  If you want
to use regexps which match more than one character, then you need
to set  `autoscratch-trigger-on-first-char' to true  and possibly
tune `autoscratch-trigger-after' accordingly.

If  no  regexp   matches  and/or  `autoscratch-trigger-after'  is
exceeded,  then the  `autoscratch-default-trigger'  form will  be
executed, which by default is  `fundamental-mode', but you can of
course change this.

Autoscratch can also  be configured to rename  the current buffer
after  it switched  mode  based on  a trigger  and  create a  new
`autoscratch-buffer' in  the background. In order  to enable this
feature, set `autoscratch-fork-after-trigger' to t.

\\{autoscratch-mode-map}")

(provide 'autoscratch)

;;; autoscratch.el ends here
