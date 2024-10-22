;;; theme-switcher-consult.el --- Consult integration for theme-switcher  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Duncan Britt

;; Author: Duncan Britt <dbru997@gmail.com>
;; Homepage: https://github.com/Duncan-Britt/theme-switcher-consult
;; Keywords: convenience, themes
;; Version: 0.0.1
;; Package-Requires: ((emacs "29") (consult "0.19") (theme-switcher "0.0.1"))

;; This file is not part of GNU Emacs.

;; The software is provided "as is", without warranty of any kind, express or implied,
;; including but not limited to the warranties of merchantability, fitness for a particular
;; purpose and noninfringement. in no event shall the authors or copyright holders be liable
;; for any claim, damages or other liability, whether in an action of contract, tort or
;; otherwise, arising from, out of or in connection with the software or the use or other
;; dealings in the software.

;;; Commentary:

;; This package provides integration between theme-switcher and consult,
;; adding theme preview capabilities while maintaining theme-switcher's
;; Light/Dark theme organization.

;;; Code:

(require 'theme-switcher)
(require 'consult)

(defun theme-switcher-consult--filter-themes (category)
  "Filter available themes based on CATEGORY (Light or Dark)."
  (let ((themes-list (if (eq category 'Light)
                        *theme-switcher-themes-light*
                      *theme-switcher-themes-dark*)))
    (seq-filter (lambda (theme)
                  (memq theme (mapcar #'intern themes-list)))
                (cons 'default (custom-available-themes)))))

(defun theme-switcher-consult--apply-theme (theme)
  "Apply THEME with proper handling of default theme and refreshing."
  (let ((theme-sym (if (stringp theme) (intern theme) theme)))
    (when (eq theme-sym 'default)
      (setq theme-sym nil))
    (unless (eq theme-sym (car custom-enabled-themes))
      (mapc #'disable-theme custom-enabled-themes)
      (when theme-sym
        (if (custom-theme-p theme-sym)
            (enable-theme theme-sym)
          (load-theme theme-sym :no-confirm))))
    (ts-refresh-inline-images)))

(defun theme-switcher-consult-choose-theme ()
  "Choose and preview themes using consult, organized by Light/Dark categories."
  (interactive)
  (let* ((options '("Light" "Dark"))
         (category-selection (intern (completing-read "Choose category: " options)))
         (saved-theme (car custom-enabled-themes))
         (avail-themes (theme-switcher-consult--filter-themes category-selection)))
    
    (setq *theme-switcher-themes-category* category-selection)
    
    (let ((theme-chosen
           (consult--read
            (mapcar #'symbol-name avail-themes)
            :prompt "Choose theme: "
            :require-match t
            :category 'theme
            :history 'consult--theme-history
            :lookup (lambda (selected &rest _)
                     (setq selected (and selected (intern-soft selected)))
                     (or (and selected (car (memq selected avail-themes)))
                         saved-theme))
            :state (lambda (action theme)
                    (pcase action
                      ('return (theme-switcher-consult--apply-theme theme))
                      ((and 'preview (guard theme))
                       (theme-switcher-consult--apply-theme theme))))
            :default (symbol-name (or saved-theme 'default)))))
      
      (when theme-chosen
        (message "Loaded %s" theme-chosen)))))

(provide 'theme-switcher-consult)
;;; theme-switcher-consult.el ends here
