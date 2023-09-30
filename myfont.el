;;; my-fonts.el --- Easy switching between predefined fonts.--- -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Drxaxc
;;
;; Author: Drxaxc
;; Version: 0.0.2
;; Keywords: convenience help lisp local tools unix
;; Homepage: https://github.com/mgcsysinfcat/myfont.el
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;


;;; Commentary:

;; This package allows you to quickly switch between predefined fonts
;; in Emacs. The list of fonts is generated from the system's available
;; fonts. The default size for these fonts is 14, but this can be changed.
;; Only for Unix-based system

;;; Code:

(defgroup my-fonts nil
  "Easy switching between predefined fonts."
  :group 'convenience
  :prefix "my-fonts-")

(defcustom my-fonts-default-size 14
  "The default size for fonts."
  :type 'integer
  :group 'my-fonts)

(defvar my-fonts nil
  "List of predefined fonts and their attributes.")

(defvar my-fonts-mono nil
  "List of predefined mono fonts.")
(defun list-font-families ()
  "List font families from fc-list."
  (interactive)
  (let ((raw-output (shell-command-to-string "fc-list : family"))
        (families '()))
    (dolist (line (split-string raw-output "\n"))
      (unless (string-empty-p line) ; skip empty lines
        (let ((family (car (split-string line ","))))
          (push family families))))
    families))

(defun my-fonts-generate ()
  "Generate the my-fonts and my-fonts-mono variables."
  (interactive)
  (setq my-fonts
        (mapcar (lambda (font-family)
                  (cons (decode-coding-string font-family 'utf-8)
                        (list ':size my-fonts-default-size)))
                (list-font-families)))
  (setq my-fonts-mono
        (mapcar (lambda (font-family)
                  (cons (decode-coding-string font-family 'utf-8)
                        (list ':size my-fonts-default-size)))
                (seq-filter (lambda (font-family) (string-match-p "mono" font-family)) (list-font-families)))))

(defun my-fonts-update (font-family &rest attributes)
  "Update the attributes for FONT-FAMILY based on provided ATTRIBUTES."
  (let ((font-in-my-fonts (assoc font-family my-fonts))
        (font-in-my-fonts-mono (assoc font-family my-fonts-mono))
        (updated nil)) ; Added variable to track if any font was updated
    (when font-in-my-fonts
      (setcdr font-in-my-fonts (append attributes (unless (plist-member attributes ':size ) (list ':size my-fonts-default-size))))
      (setq updated t))
    (when font-in-my-fonts-mono
      (setcdr font-in-my-fonts-mono (append attributes (unless (plist-member attributes ':size ) (list ':size my-fonts-default-size))))
      (setq updated t))
    (unless updated
      (error "Font not found: %s" font-family))))



(defun my-fonts-set ()
  "Asks user whether to filter mono fonts, then sets font."
  (interactive)
  (let ((use-doom (featurep 'doom))) ; Settings specifically for doom
    (let* ((is-mono (y-or-n-p "Filter Mono Fonts? "))
           (filtered-fonts (if is-mono my-fonts-mono my-fonts))
           (font-names (mapcar 'car filtered-fonts))
           (selected-font-name (completing-read "Select font: " font-names))
           (font-properties (cdr (assoc selected-font-name filtered-fonts))))
      (if use-doom
          (progn
            (setq doom-font (apply 'font-spec :family selected-font-name font-properties))
            (doom/reload-font))
          (set-frame-font (apply 'font-spec :family selected-font-name font-properties))))))


(defun my-fonts-set-mono ()
  "Directly sets mono font."
  (interactive)
  (let ((use-doom (featurep 'doom)) ; Settings specifically for doom
        (mono-fonts my-fonts-mono))
    (let* ((font-names (mapcar 'car mono-fonts))
           (selected-font-name (completing-read "Select font: " font-names))
           (font-properties (cdr (assoc selected-font-name mono-fonts))))
      (if use-doom
          (progn
            (setq doom-font (apply 'font-spec :family selected-font-name font-properties))
            (doom/reload-font))
          (set-frame-font (apply 'font-spec :family selected-font-name font-properties))))))



;; Generate the font list immediately when the package is loaded
(my-fonts-generate)

;;; my-fonts.el ends here

(provide 'my-fonts)
