;; package.lisp -- 
;; Copyright (C) 2003-2008 Shawn Betts
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

(defpackage :thesiswm
  (:use :cl
        #:alexandria)
  (:shadow #:yes-or-no-p #:y-or-n-p)
  (:export
   #:call-in-main-thread))

(defpackage :thesiswm-user
  (:use :cl :thesiswm))
