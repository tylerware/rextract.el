;;; rextract  --- parse data from strings -*- lexical-binding: t; -*-

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

;;; Variables:
(defvar rextract-field-terminator-default (rx (or whitespace line-end))
  "The default field terminator for extracting fields, which is a regular expression.

Note that this is not a field seperator, but a terminator. It's a nuance as
most of the time it will mean the same thing, but on edge cases it matters.

For example, if you have:
    \"1 2 3 4 5\"
and you're using a space a the field terminator, then 5 will not be returned
as field, because it is not followed by a space. However, if we instead use
the the field terminator with whitespace or line-end then we get all 5 numbers
as fields that we can extract.")

(defvar rextract-destructive-extract nil
  "If non-nil, this will cause the `rextract-field*' and `rextract-group*'
functions to destructively mutate the STR passed into them. This can be
useful when you are parsing an inputted string and don't care about it once
you've transformed it into it's new form.")

;;; RX forms:
(rx-define repeat-rx (n &rest RXs)
 (eval
  `(: ,@(cl-loop repeat n append
                 `(RXs)))))

;;; Library:
(defmacro rextract-groups (str regexp &optional labels)
  "Extracts groups from the STR that are defined in REGEXP.

If `rextract-destructive-extract' is set then the STR is desctructively modified.
Specifically, the full matched REGEXP is removed from the STR.

LABELS, when provided, must be a plist of labels for the groups you want to extract
and the number of fields you want assigned to that label. For example:

    (:label-1 1 :label-2 4)

In this example, `:label-1' will get the first matched group and `:label-2' will be
assigned the second through fifth matched groups.
"
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


(defmacro rextract-group-n (str regexp n)
  "Extract the N-th REGEXP defined group matched in STR.

See `rextract-groups' for more details.

If `rextract-destructive-extract' is set then the STR is desctructively modified.
Specifically, the FULL matched REGEXP is removed from the STR.
"
  `(let ((n ,n))
     (unless (> n 0)
       (error "Groups count from 1 up, provided value less than 1"))
     (nth (1- n) (rextract-groups ,str ,regexp))))

(defmacro rextract-group (str regexp)
  "Extract the first REGEXP defined group matched in STR.

This will extract exactly the first matched group defined in REGEXP.

See `rextract-groups' for more details.

If `rextract-destructive-extract' is set then the STR is desctructively modified.
Specifically, the FULL matched REGEXP is removed from the STR.
"
  `(rextract-group-n ,str ,regexp 1))


(defmacro rextract-fields (str count-or-labels &optional field-terminator)
  "Extracts fields from STR given a field terminator.

This will only extract as many fields as it is told to extract. COUNT-OR-LABELS
is how you specify that. Provide a positive integer, n, to extract n fields. You
can however provide a list of labels, see `rextract-fields' for more details.

The FIELD-TERMINATOR will override the `rextract-field-terminator-default'. This
should be a regular expression. See `rextract-field-terminator-default' for more
on this.

If `rextract-destructive-extract' is set then the STR is desctructively modified.
Specifically, fields matched are removed from the STR.
"
  `(let* ((count-or-labels ,count-or-labels)
          (labels (when (sequencep count-or-labels)
                    count-or-labels))
          (n (if (sequencep count-or-labels)
                 (cl-loop for x in count-or-labels
                          sum
                          (if (numberp x) x 0))
               count-or-labels))
          (field-terminator (or ,(macroexpand field-terminator)
                                rextract-field-terminator-default)))
     (rextract-groups ,str (rx-to-string
                            `(: bos
                              (repeat-rx
                               ,n
                               (group (+? any))
                               (regexp ,field-terminator))))
                      labels)))


(defmacro rextract-field-n (str n &optional field-terminator)
  "Extract the N-th field from matched in STR.

See `rextract-fields' for details on the FIELD-TERMINATOR and general behavior.

If `rextract-destructive-extract' is set then the STR is desctructively modified.
Specifically, all the fields up to N matched are removed from the STR.
"
  `(nth ,n (rextract-fields ,str ,(1+ n) ,field-terminator)))


(defmacro rextract-field (str &optional field-terminator)
  "Extract the first field from matched in STR.

See `rextract-fields' for details on the FIELD-TERMINATOR and general behavior.

If `rextract-destructive-extract' is set then the STR is desctructively modified.
Specifically, the first matched field is removed from the STR.
"
  `(rextract-field-n ,str 0, field-terminator))


(provide 'rextract)
;;; rextract.el ends here
