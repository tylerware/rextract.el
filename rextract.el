;;; rextract  --- parse data from strings -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;; (liscense coming soon TM)
;;
;; Author: Tyler Ware <http://github.com/tylerware>
;; Maintainer: Tyler Ware <tyler.ware.777@gmail.com>
;; Created: May 22, 2020
;; Modified: May 22, 2020
;; Version: 0.0.1
;; Keywords: regexp, extract, rx, data
;; Homepage: https://github.com/tylerware/rextract.el
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Warning: This is a work-in-progress (WIP) and the interface is likely to
;; undergo some changes, which may be drastic. This package is meant to provide
;; a library for extracting structured data from strings using regular
;; expressions with an easy to use interface.
;;
;;
;;; Code:

(defvar rextract-field-terminator-default (rx whitespace)
  "TODO")

(defvar rextract-destructive-extract nil
  "TODO")

(rx-define repeat-rx (n &rest RXs)
 (eval
  `(: ,@(cl-loop repeat n append
                 `(RXs)))))

(defmacro rextract-groups (str regexp &optional labels)
  "Extracts groups from the STR that are defined in REGEXP.

TODO: update --> only destruct if var is set
Warning: This macro destructively modifies the STR in the calling context.

LABELS is a plist of labels with the number of groups that a label goes to."
  `(when (string-match ,regexp ,str)
     (let* ((group-match-count (/ (1- (length (match-data))) 2))
            (groups (cl-loop for i from 1 to group-match-count
                             collect (match-string i ,str)))
            (labels ,labels))
       ,(when (symbolp str)
         `(unless rextract-destructive-extract
           (setq ,str (substring ,str (match-end 0)))))
       (if labels
           (cl-loop for i
                    from 0 below (length labels) by 2
                    with j = 0
                    append
                    (let* ((label (nth i labels))
                           (group-count (nth (1+ i) labels))
                           (j+ (+ j group-count))
                           (groups-subset (subseq groups j j+))
                           (return-value (if (eq 1 group-count)
                                             (car groups-subset)
                                           groups-subset)))
                      (setq j j+)
                      (list label return-value))
                    )
         groups))))

(defmacro rextract-group (str regexp)
  "TODO
Note: this uses 1-indexing as matched regex groups start at 1"
  `(rextract-group-n ,str ,regexp 1))

(defmacro rextract-group-n (str regexp n)
  "TODO
Note: this uses 1-indexing as matched regex groups start at 1"
  `(nth (1- ,n) (rextract-groups ,str ,regexp)))

(defmacro rextract-fields (str count-or-labels &optional field-terminator)
  "TODO"
  (let* ((count-or-labels (eval count-or-labels))
         (labels (when (sequencep count-or-labels)
                   count-or-labels))
         (n (if (sequencep count-or-labels)
                (cl-loop for x in count-or-labels
                         sum
                         (if (numberp x) x 0))
               count-or-labels)))

  `(let* ((field-terminator (or ,(macroexpand field-terminator) rextract-field-terminator-default)))
     (rextract-groups ,str (rx-to-string
                            `(: bos
                              (repeat-rx
                               ,,n
                               (group (+? any))
                               (or (regexp ,field-terminator)
                                   line-end))))
                      ',labels))))

(defmacro rextract-field-n (str n &optional field-terminator)
  "TODO"
  `(nth ,n (rextract-fields ,str ,(1+ n) ,field-terminator)))

(defmacro rextract-field (str &optional field-terminator)
  "TODO"
  `(rextract-field-n ,str 0))



(provide 'rextract)
;;; rextract.el ends here