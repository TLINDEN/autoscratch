;;; trigger-tests.el --- Tests for autoscratch-mode

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

;;; Code:

(require 'autoscratch-mode)
(require 'ert)

(ert-deftest switch-text ()
  (autoscratch-buffer)
  (insert "t")
  (autoscratch--look-for-triggers nil)
  (should (string= major-mode "text-mode")))

(provide 'trigger-tests)

;;; trigger-tests.el ends here
