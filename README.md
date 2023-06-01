[![Build Status](https://travis-ci.org/TLINDEN/autoscratch.svg?branch=master)](https://travis-ci.org/TLINDEN/autoscratch)

# autoscratch-mode - automatically switch scratch buffer mode

*Author:* T.v.Dein <tlinden AT cpan DOT org><br>
*URL:* [https://github.com/tlinden/autoscratch](https://github.com/tlinden/autoscratch)<br>

## Introduction

This simple  major mode can be  used as the initial  major mode for
the scratch buffer. It automatically switches to another major mode
based on regexes triggered by text input.

In   the   default  configuration   it   responds   to  the   first
non-whitespace  character  entered  into  the  scratch  buffer  and
switches   major   mode   based   on  this.    See   the   variable
`autoscratch-triggers-alist` for the defaults.  For example, if you
enter a paren, it will switch to `emacs-lisp-mode`.

## History / Why

I use emacs  for more than 20 years  now. I love it, I  am addicted, I
can't even do  anything productive without it. However,  there was one
problem left: the  *scratch* buffer. This is a  non-file buffer, which
always  exists always  in  emacs. By  default  it has  emacs-lisp-mode
enabled and you can  use it to hack short elisp  sexps for testing. In
fact I use it for exactly this purpose.

But sometimes I hate it as well! I get a phone call and need to take a
note quickly, *scratch*  is already open, so I use  this. But the mode
doesn't  really fit.  So  I switch  to text-mode  and  then enter  the
notes. I did it this way almost since day one of my emacs usage.

A couple of  months ago I came  up with a "solution", I  just create a
*text*  buffer  on  startup  with  text-mode  already  enabled  in  my
.emacs. But I  still need to switch  to it everytime I  need it. Still
too annoying!

So now,  here's my final solution,  which tries to fix  this mess once
and for all: **autoscratch**.

This major  mode is really  simple. Basically  it consits of  an alist
with instructions on how to  automatically switch the *scratch** buffer
mode. Enter an  "(" and it switches to emacs-lisp-mode,  enter "*" and
it switches  to org-mode, enter  some letter  a-z, and it  switches to
text-mode. You get the idea.

It also  solves another problem I  (and many other users  according to
google)  have:  once  I  set   the  *scratch*  buffer  mode  to,  say,
*text-mode* with some  text in there, I don't have  an elisp *scratch*
buffer left anymore. I'd need another  one, but how to create a second
*scratch*  buffer?  Obviously  you'll   need  to  rename  the  current
text-mode buffer first  and then create a new empty  buffer. The emacs
wiki contains lots of suggestions for this kind of solution.

No more  of this! Autoscratch can  just "fork" the buffer,  if enabled
(set autoscratch-fork-after-trigger to t which is the default). Here's
how it works: type a  "(" into the empty autoscratch-enabled *scratch*
buffer.  Autoscratch  renames  this  buffer  to  *emacs-lisp-scratch*,
enables  emacs-lisp-mode and  creates a  new *scratch*  buffer in  the
background. So, if  you need some *scratch* space, it'll  be there for
you waiting bravely for input.

## Installation

Put `autoscratch-mode.el` into your load-path or use `package-install`
to install it.

If you're using `use-package`, then add this:
```elisp
(use-package autoscratch
  :ensure t
  :config
  (setq initial-major-mode 'autoscratch-mode))
```

## Configuration

The minimum configuration looks like this:

        (require 'autoscratch-mode)
        (setq initial-major-mode 'autoscratch-mode)
        (setq initial-scratch-message "")
        (setq inhibit-startup-screen t)

You     may,     however,     configure    the     trigger     list
`autoscratch-triggers-alist` according  to your  preferences.  This
list consists  of cons cells, where  the `car` is a  regexp and the
`cdr` an emacs lisp form (e.g. a  lambda or defun).  If you want to
use regexps which  match more than one character, then  you need to
set `autoscratch-trigger-on-first-char`  to `nil` and  possibly tune
`autoscratch-trigger-after` accordingly.

If   no  regexp   matches  and/or   `autoscratch-trigger-after`  is
exceeded,  then  the  `autoscratch-default-trigger`  form  will  be
executed, which  by default is  `fundamental-mode`, but you  can of
course change this.

This is the default trigger list:

```lisp
(("[(;]"         . (emacs-lisp-mode))
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
```

Please note  the trigger of  the `#` character: an  interactive prompt
will ask you which mode to select.

Autoscratch can  also be  configured to  rename the  current buffer
after  it  switched mode  based  on  a  trigger  and create  a  new
`autoscratch-buffer`  in the  background. In  order to  enable this
feature, set `autoscratch-fork-after-trigger` to t.

This is my own config:
```lisp
(use-package autoscratch-mode
  :ensure t
  :config
  (setq initial-major-mode 'autoscratch-mode)
  (add-hook 'autoscratch-mode-hook '(lambda ()
     (setq autoscratch-triggers-alist
           '(("[(;]"         . (progn
                                 (call-interactively 'emacs-lisp-mode)
                                 (call-interactively 'enable-paredit-mode)
                                 (call-interactively 'electric-pair-mode)))
             ("#"            . (progn
                                 (call-interactively 'config-general-mode)
                                 (electric-indent-local-mode t)))
             ("[-a-zA-Z0-9]" . (text-mode))
             ("/"            . (c-mode))
             ("*"            . (progn (insert " ") (org-mode)))
             ("."            . (fundamental-mode)))
           autoscratch-trigger-on-first-char t
           autoscratch-reset-default-directory t)
     (electric-indent-local-mode nil)))
  (defalias 'scratch 'autoscratch-buffer))
```

I'd also recommend to use [persistent-scratch](https://github.com/Fanael/persistent-scratch)
in combination with autoscratch, here's my config for this:

```listp
(defun tvd-autoscratch-p ()
  "Return non-nil if the current buffer is a scratch buffer"
  (string-match "scratch*" (buffer-name)))

(use-package persistent-scratch
             :config
             (setq persistent-scratch-save-file (expand-file-name "scratches.el" user-emacs-directory))
             (persistent-scratch-setup-default)

             (setq persistent-scratch-scratch-buffer-p-function 'tvd-autoscratch-p))
```

