;; Copyright (C) 2007-2008 Shawn Betts
;;
;;  This file is part of thesiswm.
;;
;; thesiswm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; thesiswm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;; Generate the texinfo manual from docstrings in the source. Note,
;; this only works in sbcl, clisp and lispworks
;;
;; Code:

(in-package #:thesiswm)

(require :sb-introspect)

;; handy for figuring out which symbol is borking the documentation
(defun dprint (sym)
  (declare (ignorable sym))
  ;;(format t "~&Doing ~a..." sym))
)

(defun generate-function-doc (s line)
  (ppcre:register-groups-bind (name) ("^@@@ (.*)" line)
                              (dprint name)
                              (let ((fn (if (find #\( name :test 'char=)
                                            ;; handle (setf <symbol>) functions
                                            (with-standard-io-syntax
                                              (let ((*package* (find-package :thesiswm)))
                                                (fdefinition (read-from-string name))))
                                            (symbol-function (find-symbol (string-upcase name) :thesiswm))))
                                    (*print-pretty* nil))
                                (format s "@defun {~a} ~{~a~^ ~}~%~a~&@end defun~%~%"
                                        name
                                        (sb-introspect:function-lambda-list fn)
                                        (documentation fn 'function))
                                t)))

(defun generate-macro-doc (s line)
  (ppcre:register-groups-bind (name) ("^%%% (.*)" line)
                              (dprint name)
                              (let* ((symbol (find-symbol (string-upcase name) :thesiswm))
                                     (*print-pretty* nil))
                                (format s "@defmac {~a} ~{~a~^ ~}~%~a~&@end defmac~%~%"
                                        name
                                        (sb-introspect:function-lambda-list (macro-function symbol))
                                        (documentation symbol 'function))
                                t)))

(defun generate-variable-doc (s line)
  (ppcre:register-groups-bind (name) ("^### (.*)" line)
                              (dprint name)
                              (let ((sym (find-symbol (string-upcase name) :thesiswm)))
                                (format s "@defvar ~a~%~a~&@end defvar~%~%"
                                        name (documentation sym 'variable))
                                t)))

(defun generate-hook-doc (s line)
  (ppcre:register-groups-bind (name) ("^\\$\\$\\$ (.*)" line)
                              (dprint name)
                              (let ((sym (find-symbol (string-upcase name) :thesiswm)))
                                (format s "@defvr {Hook} ~a~%~a~&@end defvr~%~%"
                                        name (documentation sym 'variable))
                                t)))

(defun generate-command-doc (s line)
  (ppcre:register-groups-bind (name) ("^!!! (.*)" line)
    (dprint name)
    (if-let (symbol (find-symbol (string-upcase name) :thesiswm))
      (let ((cmd (symbol-function symbol))
            (*print-pretty* nil))
        (format s "@deffn {Command} ~a ~{~a~^ ~}~%~a~&@end deffn~%~%"
                name
                (sb-introspect:function-lambda-list cmd)
                (documentation cmd 'function))
        t)
      (warn "Symbol ~A not found in package STUMPWM" name))))

(defun generate-manual (&key (in #p"thesiswm.texi.in") (out #p"thesiswm.texi"))
  (let ((*print-case* :downcase))
    (with-open-file (os out :direction :output :if-exists :supersede)
      (with-open-file (is in :direction :input)
	(loop for line = (read-line is nil is)
	   until (eq line is) do
	     (or (generate-function-doc os line)
		 (generate-macro-doc os line)
		 (generate-hook-doc os line)
		 (generate-variable-doc os line)
		 (generate-command-doc os line)
		 (write-line line os)))))))
