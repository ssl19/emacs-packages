;;; corfu-quick-access.el --- Quick access to corfu-mode candidates -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022-2023, Samuel Lee
;; Copyright (C) 2009-2023  Free Software Foundation, Inc.

;; Author: Luis Henriquez-Perez <luis@luishp.xyz>, Daniel Mendler <mail@daniel-mendler.de>,
;;         Dmitry Gutov <dgutov@yandex.ru>, Nikolaj Schumacher
;;         Samuel Lee <samuelleewhu@gmail.com>
;; Maintainer: Samuel Lee <samuelleewhu@gmail.com>
;; Homepage: https://codeberg.org/spike_spiegel/corfu-quick-access.el
;; Created: 2022
;; Version: 0.1
;; Package-Version: 20230307.20801
;; Package-Requires: ((emacs "28.1") (corfu "0.35"))


;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a corfu extension, which show quick-access prefix with corfu
;; candidates if `corfu-quick-access-mode' is enabled. It allows you to select
;; candidates with `corfu-quick-access-keys' and `corfu-quick-access-modifier'
;; keybindings.

;; This package took the idea from company-mode and used some adapted code from
;;  company-mode(https://github.com/company-mode/company-mode) and
;;  corfu-indexed.el(https://github.com/minad/corfu/blob/main/extensions/corfu-indexed.el)
;;  Special thanks for all the contributors of these two fantastic repos.

;;; Code:

(require 'corfu)

(defgroup corfu-quick-access nil
  "Quick access corfu candidates."
  :group 'corfu)

(defcustom corfu-quick-access-annotation-width 1
  "Annotation width of corfu-quick-access."
  :type 'integer
  :group 'corfu-quick-access)

(defcustom corfu-quick-access-keys '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0")
  "Character strings used as a part of quick-access key sequences.

If `corfu-quick-access-mode' is enabled, show quick-access hints
beside the candidates."
  :type '(choice
          (const :tag "Digits" ("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
          (const :tag "QWERTY home row" ("a" "s" "d" "f" "g" "h" "j" "k" "l" ";"))
          (repeat :tag "User defined" string))
  :group 'corfu-quick-access)

(defcustom corfu-quick-access-modifier 'meta
  "Modifier key used for quick-access keys sequences."
  :type '(choice (const :tag "Meta key" meta)
                 (const :tag "Super key" super)
                 (const :tag "Hyper key" hyper)
                 (const :tag "Control key" control))
  :group 'corfu-quick-access)

(defface corfu-quick-access-face
  '((default :height 0.75)
    (((class color) (min-colors 88) (background dark))
     :foreground "#f4f4f4" :background "#323232")
    (((class color) (min-colors 88) (background light))
     :foreground "#404148" :background "#d7d7d7")
    (t :background "black"))
  "Face used for the corfu quick access prefix."
  :group 'corfu-quick-access)


(cl-defmethod corfu--affixate :around (cands &context (corfu-quick-access-mode (eql t)))
  (setq cands (cdr (cl-call-next-method cands)))
  (let* ((space #(" " 0 1 (face (:height 0.5 :inherit corfu-quick-access-face))))
         (width corfu-quick-access-annotation-width)
         (fmt (concat space
                      (propertize (format "%%%ds" width)
                                  'face 'corfu-quick-access-face)
                      space))
         (align
          (propertize (make-string width ?\s)
                      'display
                      `(space :align-to (+ left ,(1+ width))))))
    (cl-loop for cand in cands for key-annotation in corfu-quick-access-keys do
             (setf (cadr cand)
                   (concat
                    (propertize " " 'display (format fmt key-annotation))
                    align
                    (cadr cand))))
    (cons t cands)))

(defun corfu-quick-access--modifier ()
  "Return string representation of the `corfu-quick-access-modifier"
  (if-let ((modifier (assoc-default corfu-quick-access-modifier
                                    '((meta . "M")
                                      (super . "s")
                                      (hyper . "H")
                                      (control . "C")))))
      modifier
    (warn "corfu quick access modifier value unknown: %S"
          corfu-quick-access-modifier)
    "M"))

(defun corfu-quick-access--bind-key (keymap)
  (let ((modifier (corfu-quick-access--modifier)))
    (dolist (key corfu-quick-access-keys)
      (let ((key-seq (corfu-quick-access--kbd modifier key)))
        (if (lookup-key keymap key-seq)
            (warn "Key sequence %s already bound" (key-description key-seq))
          (define-key keymap key-seq #'corfu-quick-access-complete))))))

(defun  corfu-quick-access--unbind-key (keymap)
  (let ((modifier (corfu-quick-access--modifier)))
    (dolist (key corfu-quick-access-keys)
      (let ((key-seq (corfu-quick-access--kbd modifier key)))
        (define-key keymap key-seq nil)))))

(defun corfu-quick-access--kbd (modifier key)
  (kbd (format "%s-%s" modifier key)))

(defun corfu-quick-access--complete-nth (index)
  (let ((corfu--index (+ corfu--scroll index)))
    (if (or (< corfu--index 0)
            (>= corfu--index corfu--total)
            (>= corfu--index (+ corfu--scroll corfu-count)))
        (message "Out of range")
      (corfu-insert))))


(defun corfu-quick-access-complete (index)
  (interactive
   (list (let* ((event-type (event-basic-type last-command-event))
                (event-string (if (characterp event-type)
                                  (string event-type)
                                (error "Unexpected input"))))
           (cl-position event-string corfu-quick-access-keys :test 'equal))))
  (when index
    (corfu-quick-access--complete-nth index)))

;;;###autoload
(define-minor-mode corfu-quick-access-mode
  "Quick access for corfu candidates."
  :global t
  (cond
   (corfu-quick-access-mode
    (corfu-quick-access--bind-key corfu-map))
   (t
    (corfu-quick-access--unbind-key corfu-map))))

(provide 'corfu-quick-access)
;;; corfu-quick-access.el ends here
;; End:
