;;; alect-themes.el --- 2 color themes for Emacs

;; Copyright (C) 2013 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; URL: http://github.com/alezost/alect-themes
;; Created: 2013-07-10
;; Version: 0.1.4
;; Last-Updated: 2013-10-22
;; Package-Requires: ((emacs "24.0"))
;; Keywords: color theme

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

;; This package provides light and dark color-themes for GNU Emacs 24 or
;; later.  These themes were intended to be used with a GUI.  Use those
;; in terminal at your own risk :)

;; Screenshots are available at <http://github.com/alezost/alect-themes>

;; It is possible to modify color palette for the themes by customizing
;; `alect-colors' variable (you may use `alect-generate-colors' function
;; for that -- see the code).

;; If you use one (or both) of these themes and want it to support a
;; major mode the faces of which are not supported yet, you may send me
;; a letter to add it.

;;; Code:

(require 'cl-macs)  ; for cl-flet

(defun alect-put-colors (color-name theme-names color-vals var)
  "Put theme colors into the variable VAR.

THEME-NAMES is a list of symbols. Theme names should already
exist in the variable.

COLOR-VALS is a list of colors for the specified theme (theme
names and color values should be in matching order)."
  (when theme-names
    (let ((theme (assoc (car theme-names) var))
          (color-val (car color-vals)))
      (setcdr theme (cons (cons color-name color-val) (cdr theme))))
    (alect-put-colors color-name (cdr theme-names) (cdr color-vals) var)))

;;;###autoload
(defun alect-generate-colors (theme-names colors)
  "Return alist of themes suitable for the variable `alect-colors'.

THEME-NAMES is a list of symbols.

COLORS is a list of lists (COLOR-NAME COLOR-VAL...) where
COLOR-VAL is a color for specified theme (theme names and color
values should be in matching order)."
  (let (cols)
    (dolist (theme theme-names)
      (add-to-list 'cols (list theme)))
    (dolist (elem colors)
      (alect-put-colors (car elem) theme-names (cdr elem) cols))
    cols))

(defgroup alect nil
  "Options for alect color themes."
  :group 'faces)

(defcustom alect-colors
  (alect-generate-colors
   '(light dark)
   '((cursor    "#1074cd" "#ffec8b")
     (white     "#fafafa" "#e9e9e9")
     (gray      "#808080" "#aaaaaa")
     (black     "#070707" "#000000")
     (fg-2      "#6c6c6c" "#8c826d")
     (fg-1      "#505050" "#d0bf8f")
     (fg        "#3f3f3f" "#f0dfaf")
     (fg+1      "#262626" "#d5d2be")
     (fg+2      "#101010" "#f6f0e1")
     (bg-2      "#f6f0e1" "#222222")
     (bg-1      "#ded6c5" "#3f3f3f")
     (bg        "#d9ceb2" "#4f4f4f")
     (bg+1      "#d7c89b" "#5f5f5f")
     (bg+2      "#ccc19b" "#6f6f6f")
     (red-2     "#fa5151" "#fa6a6e")
     (red-1     "#e43838" "#fa5151")
     (red       "#f71010" "#e43838")
     (red+1     "#d81212" "#f21515")
     (red+2     "#b22222" "#bc1417")
     (yellow-2  "#f8ffc5" "#f8ffa0")
     (yellow-1  "#ab9c3a" "#e8e815")
     (yellow    "#ef8300" "#fe8b04")
     (yellow+1  "#a0682d" "#e5c900")
     (yellow+2  "#6a621b" "#abab3a")
     (green-2   "#3cb368" "#8ce096")
     (green-1   "#1c9e28" "#32cd32")
     (green     "#028902" "#7fb07f")
     (green+1   "#008b45" "#3cb370")
     (green+2   "#077707" "#099709")
     (cyan-2    "#1fb3b3" "#8cf1f1")
     (cyan-1    "#409e9f" "#2fdbde")
     (cyan      "#528d8d" "#1fb3b3")
     (cyan+1    "#0d7b72" "#528d8d")
     (cyan+2    "#286060" "#0d7b72")
     (blue-2    "#0092ff" "#b2c3f8")
     (blue-1    "#2c53ca" "#94bff3")
     (blue      "#0000ff" "#62b6ea")
     (blue+1    "#0505cc" "#00aff5")
     (blue+2    "#00008b" "#2884c6")
     (magenta-2 "#dc63dc" "#ebabde")
     (magenta-1 "#ba55d3" "#dc8cc3")
     (magenta   "#a020f0" "#e81ee7")
     (magenta+1 "#9400d3" "#ba55d3")
     (magenta+2 "#8b008b" "#a44bbb")))
  "Alist of alect color themes.

Each association is a cons cell of a theme name and alist of
color names and values."
  :group 'alect)

(defun alect-get-color (theme-name color-name)
  "Return the value of color for the specified theme."
  (cdr (assoc color-name
              (cdr (assoc theme-name alect-colors)))))

(defun alect-get-faces (theme)
  "Return a list for `custom-theme-set-faces' function."
  (cl-flet ((gc (col) (alect-get-color theme col)))
    `(;; basic colors
      (default             ((t (:foreground ,(gc 'fg+1)
                                :background ,(gc 'bg-1)))))
      (cursor              ((t (:background ,(gc 'cursor)))))
      (button              ((t (:inherit link))))
      (link                ((t (:foreground ,(gc 'blue-1)
                                :underline t :weight normal))))
      (link-visited        ((t (:foreground ,(gc 'blue+2)
                                :underline t :weight normal))))
      (escape-glyph        ((t (:foreground ,(gc 'yellow) :weight bold))))
      (fringe              ((t (:foreground ,(gc 'gray)
                                :background ,(gc 'bg-2)))))
      (header-line         ((t (:foreground ,(gc 'fg+2)
                                :height 1.13
                                :box (:line-width 1
                                      :color ,(gc 'fg+2)
                                      :style nil)))))
      (highlight           ((t (:foreground ,(gc 'black)
                                :background ,(gc 'white)))))
      (shadow              ((t (:foreground ,(gc 'gray)))))
      (success             ((t (:foreground ,(gc 'green) :weight bold))))
      (warning             ((t (:foreground ,(gc 'yellow-1) :weight normal))))
      (region              ((t (:background ,(gc 'bg+2)))))
      (menu                ((t (:foreground ,(gc 'fg+1)
                                :background ,(gc 'bg-1)))))
      (minibuffer-prompt   ((t (:foreground ,(gc 'magenta-1) :weight bold))))
      (secondary-selection ((t (:background ,(gc 'bg+1)))))
      (trailing-whitespace ((t (:background ,(gc 'red)))))
      (vertical-border     ((t (:foreground ,(gc 'fg+1)))))

      ;; auxiliary faces to be inherited by org, gnus and other faces with levels
      (color-level-1 ((t (:foreground ,(gc 'blue+1)))))
      (color-level-2 ((t (:foreground ,(gc 'green)))))
      (color-level-3 ((t (:foreground ,(gc 'red+1)))))
      (color-level-4 ((t (:foreground ,(gc 'yellow+2)))))
      (color-level-5 ((t (:foreground ,(gc 'cyan+1)))))
      (color-level-6 ((t (:foreground ,(gc 'blue-1)))))
      (color-level-7 ((t (:foreground ,(gc 'magenta-1)))))
      (color-level-8 ((t (:foreground ,(gc 'yellow)))))
      (color-level-9 ((t (:foreground ,(gc 'green+2)))))
      (color-level-10 ((t (:foreground ,(gc 'red-2)))))
      (color-level-11 ((t (:foreground ,(gc 'cyan-2)))))
      (color-level-12 ((t (:foreground ,(gc 'magenta+2)))))

      ;; ace-jump
      (ace-jump-face-background ((t (:foreground ,(gc 'bg+2)
                                     :background ,(gc 'bg-1)
                                     :inverse-video nil))))
      (ace-jump-face-foreground ((t (:foreground ,(gc 'green+2)
                                     :background ,(gc 'bg-1)
                                     :inverse-video nil))))

      ;; ack
      (ack-separator  ((t (:foreground ,(gc 'fg+1)))))
      (ack-file       ((t (:foreground ,(gc 'blue)))))
      (ack-line       ((t (:foreground ,(gc 'yellow)))))
      (ack-match      ((t (:foreground ,(gc 'fg-2)
                           :background ,(gc 'bg-2) :weight bold))))

      ;; android mode
      (android-mode-debug-face    ((t (:foreground ,(gc 'green+1)))))
      (android-mode-error-face    ((t (:foreground ,(gc 'fg-2) :weight bold))))
      (android-mode-info-face     ((t (:foreground ,(gc 'fg+1)))))
      (android-mode-verbose-face  ((t (:foreground ,(gc 'green)))))
      (android-mode-warning-face  ((t (:foreground ,(gc 'yellow)))))

      ;; auctex
      (font-latex-bold     ((t (:inherit bold))))
      (font-latex-warning  ((t (:inherit font-lock-warning-face))))
      (font-latex-sedate   ((t (:foreground ,(gc 'yellow) :weight bold ))))
      (font-latex-title-4  ((t (:inherit variable-pitch :weight bold))))

      ;; auto-complete
      (ac-candidate-face                 ((t (:foreground ,(gc 'black)
                                              :background ,(gc 'bg+2)))))
      (ac-selection-face                 ((t (:foreground ,(gc 'fg+1)
                                              :background ,(gc 'cyan+1) ))))

      ;; bm
      (bm-face                   ((t (:background ,(gc 'yellow-1)
                                      :foreground ,(gc 'bg-1)))))
      (bm-fringe-face            ((t (:background ,(gc 'yellow-1)
                                      :foreground ,(gc 'bg-1)))))
      (bm-fringe-persistent-face ((t (:background ,(gc 'green-1)
                                      :foreground ,(gc 'bg-1)))))
      (bm-persistent-face        ((t (:background ,(gc 'green-1)
                                      :foreground ,(gc 'bg-1)))))

      ;; clojure-test-mode
      (clojure-test-failure-face ((t (:foreground ,(gc 'fg-2)
                                      :weight bold :underline t))))
      (clojure-test-error-face   ((t (:foreground ,(gc 'red)
                                      :weight bold :underline t))))
      (clojure-test-success-face ((t (:foreground ,(gc 'green+1)
                                      :weight bold :underline t))))

      ;; comint
      (comint-highlight-prompt ((t (:inherit minibuffer-prompt))))
      (comint-highlight-input  ((t (:weight bold))))

      ;; compilation
      (compilation-column-face           ((t (:foreground ,(gc 'yellow)))))
      (compilation-enter-directory-face  ((t (:foreground ,(gc 'green)))))
      (compilation-error-face            ((t (:foreground ,(gc 'red-1) :weight bold :underline t))))
      (compilation-face                  ((t (:foreground ,(gc 'fg+1)))))
      (compilation-info-face             ((t (:foreground ,(gc 'blue)))))
      (compilation-info                  ((t (:foreground ,(gc 'green-2) :underline t))))
      (compilation-leave-directory-face  ((t (:foreground ,(gc 'green)))))
      (compilation-line-face             ((t (:foreground ,(gc 'yellow)))))
      (compilation-line-number           ((t (:foreground ,(gc 'yellow)))))
      (compilation-message-face          ((t (:foreground ,(gc 'blue)))))
      (compilation-warning-face          ((t (:foreground ,(gc 'fg-2) :weight bold :underline t))))
      (compilation-mode-line-exit        ((t (:foreground ,(gc 'green+2) :weight bold))))
      (compilation-mode-line-fail        ((t (:foreground ,(gc 'red) :weight bold))))
      (compilation-mode-line-run         ((t (:foreground ,(gc 'yellow) :weight bold))))

      ;; ctable
      (ctbl:face-cell-select  ((t (:background ,(gc 'blue)
                                   :foreground ,(gc 'bg-1)))))
      (ctbl:face-continue-bar ((t (:background ,(gc 'bg-2)
                                   :foreground ,(gc 'bg-1)))))
      (ctbl:face-row-select   ((t (:background ,(gc 'cyan)
                                   :foreground ,(gc 'bg-1)))))

      ;; customization
      (custom-button                  ((t (:inherit custom-button-unraised
                                           :box (:line-width 2
                                                 :style released-button)))))
      (custom-button-pressed          ((t (:inherit custom-button-unraised
                                           :box (:line-width 2
                                                 :style pressed-button)))))
      (custom-button-mouse            ((t (:inherit custom-button-unraised 
                                           :background ,(gc 'white)
                                           :box (:line-width 2
                                                 :style released-button)))))
      (custom-button-unraised         ((t (:foreground ,(gc 'black)
                                           :background ,(gc 'bg+2)))))
      (custom-button-pressed-unraised ((t (:inherit custom-button-unraised :underline t))))
      (custom-documentation           ((t (:inherit font-lock-doc-face))))
      (custom-comment                 ((t (:foreground ,(gc 'gray)))))
      (custom-tag                     ((t (:foreground ,(gc 'blue+2)))))
      (custom-state                   ((t (:foreground ,(gc 'green+1)))))
      (custom-link                    ((t (:inherit link))))
      (custom-group-subtitle          ((t (:weight bold))))
      (custom-group-tag               ((t (:inherit outline-1))))
      (custom-group-tag-1             ((t (:inherit outline-2))))
      (custom-face-tag                ((t (:foreground ,(gc 'magenta+1) :weight bold))))
      (custom-variable-tag            ((t (:inherit font-lock-variable-name-face :weight bold))))
      (custom-variable-button         ((t (:weight bold :underline t))))
      (custom-visibility              ((t (:inherit link :height 0.8))))

      ;; diff
      (diff-context           ((t (:foreground ,(gc 'fg-1)))))
      (diff-added             ((t (:foreground ,(gc 'green-1)))))
      (diff-changed           ((t (:foreground ,(gc 'yellow-1)))))
      (diff-removed           ((t (:foreground ,(gc 'red-1)))))
      (diff-indicator-added   ((t (:inherit diff-added))))
      (diff-indicator-changed ((t (:inherit diff-changed))))
      (diff-indicator-removed ((t (:inherit diff-removed))))
      (diff-refine-added      ((t (:inherit diff-added :slant italic))))
      (diff-refine-change     ((t (:inherit diff-changed :slant italic))))
      (diff-refine-removed    ((t (:inherit diff-removed :slant italic))))
      (diff-header            ((t (:foreground ,(gc 'blue-2) :weight bold))))
      (diff-hunk-header       ((t (:inherit diff-header :foreground ,(gc 'green+2)))))
      (diff-file-header       ((t (:inherit diff-header :foreground ,(gc 'cyan-1)))))
      (diff-function          ((t (:inherit diff-header :foreground ,(gc 'blue)))))
      (diff-index             ((t (:inherit diff-header :foreground ,(gc 'red-1)))))
      (diff-nonexistent       ((t (:inherit diff-header :foreground ,(gc 'gray)))))

      ;; dired
      (dired-directory  ((t (:inherit font-lock-function-name-face))))
      (dired-flagged    ((t (:foreground ,(gc 'red)))))
      (dired-header     ((t (:inherit header-line))))
      (dired-ignored    ((t (:foreground ,(gc 'gray)))))
      (dired-mark       ((t (:foreground ,(gc 'blue+1)))))
      (dired-marked     ((t (:inherit warning))))
      (dired-perm-write ((t (:foreground ,(gc 'green-1)))))
      (dired-symlink    ((t (:inherit font-lock-constant-face))))
      (dired-warning    ((t (:inherit font-lock-warning-face
                             :background ,(gc 'bg-2)))))

      ;; egg
      (egg-text-base        ((t (:foreground ,(gc 'fg+1)))))
      (egg-help-header-1    ((t (:foreground ,(gc 'cyan+1)))))
      (egg-help-header-2    ((t (:foreground ,(gc 'cyan-1)))))
      (egg-branch           ((t (:foreground ,(gc 'yellow)))))
      (egg-branch-mono      ((t (:foreground ,(gc 'yellow+1)))))
      (egg-term             ((t (:foreground ,(gc 'blue)))))
      (egg-diff-add         ((t (:foreground ,(gc 'green-2)))))
      (egg-diff-del         ((t (:foreground ,(gc 'red+1)))))
      (egg-diff-file-header ((t (:foreground ,(gc 'yellow-2)))))
      (egg-section-title    ((t (:foreground ,(gc 'red-1)))))
      (egg-stash-mono       ((t (:foreground ,(gc 'green)))))

      ;; emms
      (emms-playlist-track-face    ((t (:inherit gnus-summary-normal-unread))))
      (emms-playlist-selected-face ((t (:inherit gnus-summary-selected))))
      (emms-stream-name-face       ((t (:foreground ,(gc 'blue+1)))))
      (emms-stream-url-face        ((t (:inherit default))))

      ;; erc
      (erc-header-line          ((t (:inherit header-line))))
      (erc-bold-face            ((t (:weight bold))))
      (erc-underline-face       ((t (:underline t))))
      (erc-current-nick-face    ((t (:foreground ,(gc 'blue) :weight bold))))
      (erc-dangerous-host-face  ((t (:inherit font-lock-warning-face))))
      (erc-default-face         ((t (:foreground ,(gc 'fg+1)))))
      (erc-direct-msg-face      ((t (:inherit erc-default-face :foreground ,(gc 'red-2)))))
      (erc-action-face          ((t (:inherit erc-bold-face))))
      (erc-error-face           ((t (:inherit font-lock-warning-face))))
      (erc-fool-face            ((t (:foreground ,(gc 'blue-2)))))
      (erc-highlight-face       ((t (:inherit hover-highlight))))
      (erc-input-face           ((t (:foreground ,(gc 'magenta)))))
      (erc-keyword-face         ((t (:foreground ,(gc 'green+1) :weight normal))))
      (erc-nick-default-face    ((t (:foreground ,(gc 'blue+1) :weight normal))))
      (erc-my-nick-face         ((t (:foreground ,(gc 'red) :weight normal))))
      (erc-nick-msg-face        ((t (:foreground ,(gc 'fg+2)))))
      (erc-notice-face          ((t (:foreground ,(gc 'green)))))
      (erc-palect-face          ((t (:foreground ,(gc 'fg-2) :weight normal))))
      (erc-prompt-face          ((t (:inherit comint-highlight-prompt))))
      (erc-timestamp-face       ((t (:foreground ,(gc 'yellow+1)))))

      ;; epa
      (epa-mark              ((t (:foreground ,(gc 'blue+1)))))
      (epa-string            ((t (:foreground ,(gc 'cyan+2)))))
      (epa-validity-disabled ((t (:foreground ,(gc 'fg-2)))))
      (epa-validity-high     ((t (:foreground ,(gc 'green-1)))))
      (epa-validity-medium   ((t (:foreground ,(gc 'yellow-1)))))
      (epa-validity-low      ((t (:foreground ,(gc 'red-1)))))

      ;; ert
      (ert-test-result-expected    ((t (:foreground ,(gc 'green-2)
                                        :background ,(gc 'bg-1)))))
      (ert-test-result-unexpected  ((t (:foreground ,(gc 'red)
                                        :background ,(gc 'bg-1)))))

      ;; eshell
      (eshell-prompt         ((t (:foreground ,(gc 'fg+2) :weight bold))))
      (eshell-ls-archive     ((t (:foreground ,(gc 'green)))))
      (eshell-ls-backup      ((t (:inherit dired-ignored))))
      (eshell-ls-clutter     ((t (:inherit font-lock-comment-face))))
      (eshell-ls-directory   ((t (:inherit dired-directory))))
      (eshell-ls-executable  ((t (:foreground ,(gc 'yellow)))))
      (eshell-ls-unreadable  ((t (:foreground ,(gc 'red-2)))))
      (eshell-ls-readonly    ((t (:foreground ,(gc 'fg-2)))))
      (eshell-ls-missing     ((t (:inherit dired-warning))))
      (eshell-ls-product     ((t (:inherit font-lock-doc-face))))
      (eshell-ls-special     ((t (:foreground ,(gc 'fg+1) :weight bold))))
      (eshell-ls-symlink     ((t (:inherit dired-symlink))))

      ;; flycheck
      (flycheck-error-face    ((t (:foreground ,(gc 'red-1) :weight bold :underline t))))
      (flycheck-warning-face  ((t (:foreground ,(gc 'fg-2) :weight bold :underline t))))

      ;; flymake
      (flymake-errline        ((t (:foreground ,(gc 'red-1) :weight bold :underline t))))
      (flymake-warnline       ((t (:foreground ,(gc 'fg-2) :weight bold :underline t))))

      ;; flyspell
      (flyspell-duplicate     ((t (:foreground ,(gc 'fg-2) :weight bold :underline t))))
      (flyspell-incorrect     ((t (:foreground ,(gc 'red-1) :weight bold :underline t))))

      ;; font lock
      (font-lock-builtin-face           ((t (:foreground ,(gc 'magenta-1)))))
      (font-lock-comment-face           ((t (:foreground ,(gc 'green+1)))))
      (font-lock-comment-delimiter-face ((t (:foreground ,(gc 'green+1)))))
      (font-lock-constant-face          ((t (:foreground ,(gc 'cyan-1)))))
      (font-lock-doc-face               ((t (:foreground ,(gc 'fg-1) :slant italic))))
      (font-lock-doc-string-face        ((t (:foreground ,(gc 'cyan+2)))))
      (font-lock-function-name-face     ((t (:foreground ,(gc 'blue-1)))))
      (font-lock-keyword-face           ((t (:foreground ,(gc 'blue+1) :weight bold))))
      (font-lock-negation-char-face     ((t (:foreground ,(gc 'fg+1)))))
      (font-lock-preprocessor-face      ((t (:foreground ,(gc 'green-1)))))
      (font-lock-string-face            ((t (:foreground ,(gc 'red-2)))))
      (font-lock-type-face              ((t (:foreground ,(gc 'magenta+2)))))
      (font-lock-variable-name-face     ((t (:foreground ,(gc 'yellow+2)))))
      (font-lock-warning-face           ((t (:foreground ,(gc 'red+1) :weight bold))))

      ;; git-gutter
      (git-gutter:added       ((t (:foreground ,(gc 'green) :weight bold :inverse-video t))))
      (git-gutter:deleted     ((t (:foreground ,(gc 'red) :weight bold :inverse-video t))))
      (git-gutter:modified    ((t (:foreground ,(gc 'magenta) :weight bold :inverse-video t))))
      (git-gutter:unchanged   ((t (:foreground ,(gc 'fg+1) :weight bold :inverse-video t))))
      (git-gutter-fr:added    ((t (:foreground ,(gc 'green)  :weight bold))))
      (git-gutter-fr:deleted  ((t (:foreground ,(gc 'red) :weight bold))))
      (git-gutter-fr:modified ((t (:foreground ,(gc 'magenta) :weight bold))))

      ;; gnus
      (gnus-group-news-1-empty      ((t (:inherit color-level-1))))
      (gnus-group-news-2-empty      ((t (:inherit color-level-2))))
      (gnus-group-news-3-empty      ((t (:inherit color-level-3))))
      (gnus-group-news-4-empty      ((t (:inherit color-level-4))))
      (gnus-group-news-5-empty      ((t (:inherit color-level-5))))
      (gnus-group-news-6-empty      ((t (:inherit color-level-6))))
      (gnus-group-news-low-empty    ((t (:inherit color-level-7))))
      (gnus-group-news-1            ((t (:inherit gnus-group-news-1-empty :weight bold))))
      (gnus-group-news-2            ((t (:inherit gnus-group-news-2-empty :weight bold))))
      (gnus-group-news-3            ((t (:inherit gnus-group-news-3-empty :weight bold))))
      (gnus-group-news-4            ((t (:inherit gnus-group-news-4-empty :weight bold))))
      (gnus-group-news-5            ((t (:inherit gnus-group-news-5-empty :weight bold))))
      (gnus-group-news-6            ((t (:inherit gnus-group-news-6-empty :weight bold))))
      (gnus-group-news-low          ((t (:inherit gnus-group-news-low-empty :weight bold))))
      (gnus-group-mail-1-empty      ((t (:inherit gnus-group-news-1-empty :slant italic))))
      (gnus-group-mail-2-empty      ((t (:inherit gnus-group-news-2-empty :slant italic))))
      (gnus-group-mail-3-empty      ((t (:inherit gnus-group-news-3-empty :slant italic))))
      (gnus-group-mail-low-empty    ((t (:inherit gnus-group-news-low-empty :slant italic))))
      (gnus-group-mail-1            ((t (:inherit gnus-group-news-1 :slant italic))))
      (gnus-group-mail-2            ((t (:inherit gnus-group-news-2 :slant italic))))
      (gnus-group-mail-3            ((t (:inherit gnus-group-news-3 :slant italic))))
      (gnus-group-mail-low          ((t (:inherit gnus-group-news-low :slant italic))))

      (gnus-header-content          ((t (:inherit message-header-other))))
      (gnus-header-from             ((t (:inherit message-header-from))))
      (gnus-header-name             ((t (:inherit message-header-name))))
      (gnus-header-newsgroups       ((t (:inherit message-header-newsgroups))))
      (gnus-header-subject          ((t (:inherit message-header-subject))))
      (gnus-summary-cancelled       ((t (:background ,(gc 'fg-1)
                                         :foreground ,(gc 'bg-2)))))
      (gnus-summary-low-ancient     ((t (:foreground ,(gc 'blue-2)))))
      (gnus-summary-low-read        ((t (:foreground ,(gc 'green-2)))))
      (gnus-summary-low-ticked      ((t (:foreground ,(gc 'red-2)))))
      (gnus-summary-low-unread      ((t (:foreground ,(gc 'fg-1)))))
      (gnus-summary-normal-ancient  ((t (:foreground ,(gc 'blue)))))
      (gnus-summary-normal-read     ((t (:foreground ,(gc 'green)))))
      (gnus-summary-normal-ticked   ((t (:foreground ,(gc 'red)))))
      (gnus-summary-normal-unread   ((t (:foreground ,(gc 'fg+1)))))
      (gnus-summary-high-ancient    ((t (:inherit gnus-summary-normal-ancient :weight bold))))
      (gnus-summary-high-read       ((t (:inherit gnus-summary-normal-read    :weight bold))))
      (gnus-summary-high-ticked     ((t (:inherit gnus-summary-normal-ticked  :weight bold))))
      (gnus-summary-high-unread     ((t (:inherit gnus-summary-normal-unread  :weight bold))))
      (gnus-summary-selected        ((t (:background ,(gc 'green)
                                         :foreground ,(gc 'white)))))
      (gnus-cite-1                  ((t (:inherit color-level-1))))
      (gnus-cite-2                  ((t (:inherit color-level-2))))
      (gnus-cite-3                  ((t (:inherit color-level-3))))
      (gnus-cite-4                  ((t (:inherit color-level-4))))
      (gnus-cite-5                  ((t (:inherit color-level-5))))
      (gnus-cite-6                  ((t (:inherit color-level-6))))
      (gnus-cite-7                  ((t (:inherit color-level-7))))
      (gnus-cite-8                  ((t (:inherit color-level-8))))
      (gnus-cite-9                  ((t (:inherit color-level-9))))
      (gnus-cite-10                 ((t (:inherit color-level-10))))
      (gnus-cite-11                 ((t (:inherit color-level-11))))
      (gnus-signature               ((t (:foreground ,(gc 'cyan+1)))))
      (gnus-x                       ((t (:background ,(gc 'fg+1)
                                         :foreground ,(gc 'bg-1)))))

      ;; grep
      (grep-context-face  ((t (:foreground ,(gc 'fg+1)))))
      (grep-error-face    ((t (:foreground ,(gc 'red-1) :weight bold :underline t))))
      (grep-hit-face      ((t (:foreground ,(gc 'blue)))))
      (grep-match-face    ((t (:foreground ,(gc 'fg-2) :weight bold))))
      (match              ((t (:background ,(gc 'green+1)
                               :foreground ,(gc 'yellow-2) :weight normal))))

      ;; guide-key
      (guide-key/highlight-command-face ((t (:foreground ,(gc 'blue)))))
      (guide-key/key-face ((t (:foreground ,(gc 'green)))))
      (guide-key/prefix-command-face ((t (:foreground ,(gc 'green+1)))))

      ;; helm
      (helm-header           ((t (:foreground ,(gc 'green)
                                  :background ,(gc 'bg-1)
                                  :underline nil
                                  :box nil))))
      (helm-source-header    ((t (:foreground ,(gc 'yellow)
                                  :background ,(gc 'bg-2)
                                  :underline nil
                                  :weight bold
                                  :box (:line-width -1 :style released-button)))))
      (helm-selection        ((t (:background ,(gc 'bg) :underline nil))))
      (helm-selection-line   ((t (:background ,(gc 'bg)))))
      (helm-visible-mark     ((t (:foreground ,(gc 'bg-1) :background ,(gc 'yellow-2)))))
      (helm-candidate-number ((t (:foreground ,(gc 'green-2) :background ,(gc 'bg-2)))))
      (helm-ff-directory     ((t (:foreground ,(gc 'magenta)))))

      ;; hl-line-mode
      (hl-line       ((t (:background ,(gc 'bg)))))

      ;; ido-mode
      (ido-first-match ((t (:weight bold))))
      (ido-only-match  ((t (:inherit ido-first-match :foreground ,(gc 'fg+2)))))
      (ido-subdir      ((t (:inherit dired-directory))))
      (ido-virtual     ((t (:foreground ,(gc 'red-2)))))

      ;; isearch
      (isearch-fail         ((t (:foreground ,(gc 'fg+1)
                                 :background ,(gc 'red-2)))))
      (isearch              ((t (:foreground ,(gc 'bg+1)
                                 :background ,(gc 'fg+2)))))
      (lazy-highlight       ((t (:foreground ,(gc 'bg-2)
                                 :background ,(gc 'fg-2)))))

      ;; js2-mode
      (js2-warning-face           ((t (:foreground,(gc 'fg-2)))))
      (js2-error-face             ((t (:foreground ,(gc 'red) :weight bold))))
      (js2-jsdoc-tag-face         ((t (:foreground ,(gc 'green-1)))))
      (js2-jsdoc-type-face        ((t (:foreground ,(gc 'green+2)))))
      (js2-jsdoc-value-face       ((t (:foreground ,(gc 'cyan-1)))))
      (js2-function-param-face    ((t (:foreground ,(gc 'cyan-1)))))
      (js2-external-variable-face ((t (:foreground ,(gc 'fg-2)))))

      ;; jabber-mode
      (jabber-roster-user-away       ((t (:foreground ,(gc 'green+2)))))
      (jabber-roster-user-online     ((t (:foreground ,(gc 'blue-1)))))
      (jabber-roster-user-dnd        ((t (:foreground ,(gc 'red+1)))))
      (jabber-rare-time-face         ((t (:foreground ,(gc 'green+1)))))
      (jabber-chat-prompt-local      ((t (:foreground ,(gc 'blue-1)))))
      (jabber-chat-prompt-foreign    ((t (:foreground ,(gc 'red+1)))))
      (jabber-activity-face          ((t (:foreground ,(gc 'red+1)))))
      (jabber-activity-personal-face ((t (:foreground ,(gc 'blue+1)))))
      (jabber-title-small            ((t (:height 1.1 :weight bold))))
      (jabber-title-medium           ((t (:height 1.2 :weight bold))))
      (jabber-title-large            ((t (:height 1.3 :weight bold))))

      ;; linum-mode
      (linum ((t (:foreground ,(gc 'fg-2)))))

      ;; magit
      (magit-section-title ((t (:foreground ,(gc 'yellow) :weight bold))))
      (magit-branch ((t (:foreground ,(gc 'fg-2) :weight bold))))
      (magit-item-highlight ((t (:background ,(gc 'bg)))))

      ;; markdown mode
      (markdown-header-face           ((t (:inherit outline-1))))
      (markdown-header-face-1         ((t (:inherit outline-1))))
      (markdown-header-face-2         ((t (:inherit outline-2))))
      (markdown-header-face-3         ((t (:inherit outline-3))))
      (markdown-header-face-4         ((t (:inherit outline-4))))
      (markdown-header-face-5         ((t (:inherit outline-5))))
      (markdown-header-face-6         ((t (:inherit outline-6))))
      (markdown-header-rule-face      ((t (:inherit markdown-header-face :foreground ,(gc 'fg)))))
      (markdown-header-delimiter-face ((t (:inherit markdown-header-face :foreground ,(gc 'fg+2)))))
      (markdown-footnote-face         ((t (:foreground ,(gc 'blue+2)))))
      (markdown-inline-code-face      ((t (:foreground ,(gc 'cyan+1)))))
      (markdown-comment-face          ((t (:inherit font-lock-comment-face))))
      (markdown-blockquote-face       ((t (:inherit font-lock-doc-face))))
      (markdown-bold-face             ((t (:inherit bold))))
      (markdown-italic-face           ((t (:inherit italic))))
      (markdown-missing-link-face     ((t (:inherit font-lock-warning-face))))
      (markdown-link-face             ((t (:inherit link))))
      (markdown-link-title-face       ((t (:inherit font-lock-string-face))))
      (markdown-url-face              ((t (:foreground ,(gc 'green+2)))))
      (markdown-language-keyword-face ((t (:foreground ,(gc 'magenta+2)))))
      (markdown-line-break-face       ((t (:inherit underline))))
      (markdown-list-face             ((t (:foreground ,(gc 'yellow)))))
      (markdown-metadata-key-face     ((t (:foreground ,(gc 'blue-2)))))
      (markdown-metadata-value-face   ((t (:foreground ,(gc 'red-1)))))
      (markdown-pre-face              ((t (:foreground ,(gc 'yellow+2)))))
      (markdown-reference-face        ((t (:inherit link))))

      ;; message-mode
      (message-cited-text        ((t (:inherit font-lock-comment-face))))
      (message-separator         ((t (:inherit font-lock-comment-face))))
      (message-header-name       ((t (:foreground ,(gc 'yellow+1) :weight bold))))
      (message-header-other      ((t (:foreground ,(gc 'green+1)))))
      (message-header-to         ((t (:foreground ,(gc 'blue)))))
      (message-header-from       ((t (:foreground ,(gc 'red-1)))))
      (message-header-cc         ((t (:foreground ,(gc 'blue-1)))))
      (message-header-newsgroups ((t (:foreground ,(gc 'yellow+2)))))
      (message-header-subject    ((t (:foreground ,(gc 'red+1)))))
      (message-header-xheader    ((t (:foreground ,(gc 'green-1)))))
      (message-mml               ((t (:foreground ,(gc 'cyan-2) :weight bold))))

      ;; mew
      (mew-face-header-subject    ((t (:foreground ,(gc 'fg-2)))))
      (mew-face-header-from       ((t (:foreground ,(gc 'yellow)))))
      (mew-face-header-date       ((t (:foreground ,(gc 'green)))))
      (mew-face-header-to         ((t (:foreground ,(gc 'red)))))
      (mew-face-header-key        ((t (:foreground ,(gc 'green)))))
      (mew-face-header-private    ((t (:foreground ,(gc 'green)))))
      (mew-face-header-important  ((t (:foreground ,(gc 'blue)))))
      (mew-face-header-marginal   ((t (:foreground ,(gc 'fg+1) :weight bold))))
      (mew-face-header-warning    ((t (:foreground ,(gc 'red)))))
      (mew-face-header-xmew       ((t (:foreground ,(gc 'green)))))
      (mew-face-header-xmew-bad   ((t (:foreground ,(gc 'red)))))
      (mew-face-body-url          ((t (:foreground ,(gc 'fg-2)))))
      (mew-face-body-comment      ((t (:foreground ,(gc 'fg+1) :slant italic))))
      (mew-face-body-cite1        ((t (:foreground ,(gc 'green)))))
      (mew-face-body-cite2        ((t (:foreground ,(gc 'blue)))))
      (mew-face-body-cite3        ((t (:foreground ,(gc 'fg-2)))))
      (mew-face-body-cite4        ((t (:foreground ,(gc 'yellow)))))
      (mew-face-body-cite5        ((t (:foreground ,(gc 'red)))))
      (mew-face-mark-review       ((t (:foreground ,(gc 'blue)))))
      (mew-face-mark-escape       ((t (:foreground ,(gc 'green)))))
      (mew-face-mark-delete       ((t (:foreground ,(gc 'red)))))
      (mew-face-mark-unlink       ((t (:foreground ,(gc 'yellow)))))
      (mew-face-mark-refile       ((t (:foreground ,(gc 'green)))))
      (mew-face-mark-unread       ((t (:foreground ,(gc 'yellow+2)))))
      (mew-face-eof-message       ((t (:foreground ,(gc 'green)))))
      (mew-face-eof-part          ((t (:foreground ,(gc 'yellow)))))

      ;; mic-paren
      (paren-face-match    ((t (:foreground ,(gc 'cyan)
                                :background ,(gc 'bg-1)
                                :weight bold))))
      (paren-face-mismatch ((t (:foreground ,(gc 'bg-1)
                                :background ,(gc 'magenta)
                                :weight bold))))
      (paren-face-no-match ((t (:foreground ,(gc 'bg-1)
                                :background ,(gc 'red)
                                :weight bold))))

      ;; mingus
      (mingus-directory-face ((t (:foreground ,(gc 'blue)))))
      (mingus-pausing-face ((t (:foreground ,(gc 'magenta)))))
      (mingus-playing-face ((t (:foreground ,(gc 'cyan)))))
      (mingus-playlist-face ((t (:foreground ,(gc 'cyan) ))))
      (mingus-song-file-face ((t (:foreground ,(gc 'yellow)))))
      (mingus-stopped-face ((t (:foreground ,(gc 'red)))))

      ;; i don't know what it is, but this face can often be met in mails
      (mm-uu-extract ((t (:background ,(gc 'bg)
                          :foreground ,(gc 'fg+1)))))

      ;; mode-line
      (mode-line-buffer-id  ((t (:foreground ,(gc 'blue-1) :weight bold))))
      (mode-line            ((t (:foreground ,(gc 'fg+1)
                                 :background ,(gc 'bg-2)
                                 :box (:line-width 2
                                       :style released-button)))
                             (t :inverse-video t)))
      (mode-line-inactive   ((t (:foreground ,(gc 'fg-2)
                                 :background ,(gc 'bg-1)
                                 :box (:line-width 2
                                       :color ,(gc 'bg-2)
                                       :style nil)))))

      ;; mu4e
      (mu4e-cited-1-face ((t (:foreground ,(gc 'blue)    :slant italic))))
      (mu4e-cited-2-face ((t (:foreground ,(gc 'green+2) :slant italic))))
      (mu4e-cited-3-face ((t (:foreground ,(gc 'cyan-2)  :slant italic))))
      (mu4e-cited-4-face ((t (:foreground ,(gc 'green)   :slant italic))))
      (mu4e-cited-5-face ((t (:foreground ,(gc 'cyan+1)  :slant italic))))
      (mu4e-cited-6-face ((t (:foreground ,(gc 'green-1) :slant italic))))
      (mu4e-cited-7-face ((t (:foreground ,(gc 'blue)    :slant italic))))
      (mu4e-replied-face ((t (:foreground ,(gc 'bg+2)))))
      (mu4e-trashed-face ((t (:foreground ,(gc 'bg+2) :strike-through t))))

      ;; mumamo
      (mumamo-background-chunk-major    ((t (:background nil))))
      (mumamo-background-chunk-submode1 ((t (:background ,(gc 'bg-2)))))
      (mumamo-background-chunk-submode2 ((t (:background ,(gc 'bg+1)))))
      (mumamo-background-chunk-submode3 ((t (:background ,(gc 'bg+2)))))
      (mumamo-background-chunk-submode4 ((t (:background ,(gc 'bg)))))

      ;; nav
      (nav-face-heading     ((t (:foreground ,(gc 'yellow)))))
      (nav-face-button-num  ((t (:foreground ,(gc 'cyan)))))
      (nav-face-dir         ((t (:foreground ,(gc 'green)))))
      (nav-face-hdir        ((t (:foreground ,(gc 'red)))))
      (nav-face-file        ((t (:foreground ,(gc 'fg+1)))))
      (nav-face-hfile       ((t (:foreground ,(gc 'red-2)))))

      ;; nethack-el
      (nethack-dark-gray-face          ((t (:foreground ,(gc 'fg-1)))))
      (nethack-message-highlight-face  ((t (:background ,(gc 'bg)
                                            :foreground ,(gc 'red+2)))))
      (nethack-yellow-face             ((t (:foreground ,(gc 'yellow)))))

      ;; newsticker
      (newsticker-date-face                ((t (:foreground ,(gc 'fg+1)))))
      (newsticker-default-face             ((t (:foreground ,(gc 'fg+1)))))
      (newsticker-enclosure-face           ((t (:foreground ,(gc 'cyan-1)))))
      (newsticker-extra-face               ((t (:foreground ,(gc 'bg+1) :height 0.8))))
      (newsticker-feed-face                ((t (:foreground ,(gc 'fg+1)))))
      (newsticker-immortal-item-face       ((t (:foreground ,(gc 'green)))))
      (newsticker-new-item-face            ((t (:foreground ,(gc 'blue)))))
      (newsticker-obsolete-item-face       ((t (:foreground ,(gc 'red)))))
      (newsticker-old-item-face            ((t (:foreground ,(gc 'bg+2)))))
      (newsticker-statistics-face          ((t (:foreground ,(gc 'fg+1)))))
      (newsticker-treeview-face            ((t (:foreground ,(gc 'fg+1)))))
      (newsticker-treeview-immortal-face   ((t (:foreground ,(gc 'green)))))
      (newsticker-treeview-listwindow-face ((t (:foreground ,(gc 'fg+1)))))
      (newsticker-treeview-new-face        ((t (:foreground ,(gc 'blue) :weight bold))))
      (newsticker-treeview-obsolete-face   ((t (:foreground ,(gc 'red)))))
      (newsticker-treeview-old-face        ((t (:foreground ,(gc 'bg+2)))))
      (newsticker-treeview-selection-face  ((t (:foreground ,(gc 'yellow)))))

      ;; org-mode
      (org-agenda-date-today     ((t (:foreground ,(gc 'white)
                                      :slant italic :weight bold))) t)
      (org-agenda-structure      ((t (:inherit font-lock-comment-face))))
      (org-archived              ((t (:foreground ,(gc 'fg+1) :weight bold))))
      (org-checkbox              ((t (:background ,(gc 'bg+1)
                                      :foreground ,(gc 'white)
                                      :box (:line-width 1 :style released-button)))))
      (org-date                  ((t (:foreground ,(gc 'cyan-2) :underline nil))))
      (org-date-selected         ((t (:foreground ,(gc 'white) :background ,(gc 'red+1)))))
      (org-deadline-announce     ((t (:foreground ,(gc 'red-1)))))
      (org-done                  ((t (:foreground ,(gc 'cyan-1) :weight bold))))
      (org-formula               ((t (:foreground ,(gc 'yellow-2)))))
      (org-headline-done         ((t (:foreground ,(gc 'cyan-1)))))
      (org-hide                  ((t (:foreground ,(gc 'bg-2)))))
      (org-level-1               ((t (:inherit outline-1))))
      (org-level-2               ((t (:inherit outline-2))))
      (org-level-3               ((t (:inherit outline-3))))
      (org-level-4               ((t (:inherit outline-4))))
      (org-level-5               ((t (:inherit outline-5))))
      (org-level-6               ((t (:inherit outline-6))))
      (org-level-7               ((t (:inherit outline-7))))
      (org-level-8               ((t (:inherit outline-8))))
      (org-link                  ((t (:inherit link))))
      (org-scheduled             ((t (:foreground ,(gc 'green-2)))))
      (org-scheduled-previously  ((t (:foreground ,(gc 'red-2)))))
      (org-scheduled-today       ((t (:foreground ,(gc 'blue+1)))))
      (org-special-keyword       ((t (:inherit font-lock-doc-face))))
      (org-table                 ((t (:foreground ,(gc 'fg-1)))))
      (org-tag                   ((t (:slant italic))))
      (org-time-grid             ((t (:foreground ,(gc 'fg-2)))))
      (org-todo                  ((t (:foreground ,(gc 'red) :weight bold))))
      (org-upcoming-deadline     ((t (:inherit font-lock-keyword-face))))
      (org-warning               ((t (:foreground ,(gc 'red) :weight bold :underline nil))))
      (org-column                ((t (:background ,(gc 'bg-2)))))
      (org-column-title          ((t (:background ,(gc 'bg-2) :underline t :weight bold))))

      ;; outline
      (outline-1 ((t (:inherit color-level-1 :weight bold :height 1.12))))
      (outline-2 ((t (:inherit color-level-2 :weight bold :height 1.12))))
      (outline-3 ((t (:inherit color-level-3 :weight bold :height 1.12))))
      (outline-4 ((t (:inherit color-level-4 :weight bold :height 1.12))))
      (outline-5 ((t (:inherit color-level-5 :weight bold :height 1.12))))
      (outline-6 ((t (:inherit color-level-6 :weight bold :height 1.12))))
      (outline-7 ((t (:inherit color-level-7 :weight bold :height 1.12))))
      (outline-8 ((t (:inherit color-level-8 :weight bold :height 1.12))))

      ;; popup
      (popup-tip-face                    ((t (:foreground ,(gc 'black)
                                              :background ,(gc 'yellow-2)))))
      (popup-scroll-bar-foreground-face  ((t (:background ,(gc 'cyan+2)))))
      (popup-scroll-bar-background-face  ((t (:background ,(gc 'bg-2)))))
      (popup-isearch-match               ((t (:foreground ,(gc 'fg+1)
                                              :background ,(gc 'bg-1) ))))

      ;; rainbow-delimiters
      (rainbow-delimiters-depth-1-face   ((t (:foreground ,(gc 'fg+1)))))
      (rainbow-delimiters-depth-2-face   ((t (:foreground ,(gc 'green+2)))))
      (rainbow-delimiters-depth-3-face   ((t (:foreground ,(gc 'yellow-2)))))
      (rainbow-delimiters-depth-4-face   ((t (:foreground ,(gc 'cyan)))))
      (rainbow-delimiters-depth-5-face   ((t (:foreground ,(gc 'green-1)))))
      (rainbow-delimiters-depth-6-face   ((t (:foreground ,(gc 'blue+1)))))
      (rainbow-delimiters-depth-7-face   ((t (:foreground ,(gc 'yellow-1)))))
      (rainbow-delimiters-depth-8-face   ((t (:foreground ,(gc 'green+1)))))
      (rainbow-delimiters-depth-9-face   ((t (:foreground ,(gc 'cyan-2)))))
      (rainbow-delimiters-depth-10-face  ((t (:foreground ,(gc 'fg-2)))))
      (rainbow-delimiters-depth-11-face  ((t (:foreground ,(gc 'green)))))
      (rainbow-delimiters-depth-12-face  ((t (:foreground ,(gc 'cyan+2)))))

      ;; rcirc
      (rcirc-my-nick                   ((t (:foreground ,(gc 'blue)))))
      (rcirc-other-nick                ((t (:foreground ,(gc 'fg-2)))))
      (rcirc-bright-nick               ((t (:foreground ,(gc 'blue+1)))))
      (rcirc-dim-nick                  ((t (:foreground ,(gc 'cyan-2)))))
      (rcirc-server                    ((t (:foreground ,(gc 'green)))))
      (rcirc-server-prefix             ((t (:foreground ,(gc 'green+1)))))
      (rcirc-timestamp                 ((t (:foreground ,(gc 'green+2)))))
      (rcirc-nick-in-message           ((t (:foreground ,(gc 'yellow)))))
      (rcirc-nick-in-message-full-line ((t (:weight bold))))
      (rcirc-prompt                    ((t (:foreground ,(gc 'yellow) :weight bold))))
      (rcirc-track-nick                ((t (:inverse-video t))))
      (rcirc-track-keyword             ((t (:weight bold))))
      (rcirc-url                       ((t (:weight bold))))
      (rcirc-keyword                   ((t (:foreground ,(gc 'yellow) :weight bold))))

      ;; rpm-mode
      (rpm-spec-dir-face           ((t (:foreground ,(gc 'green)))))
      (rpm-spec-doc-face           ((t (:foreground ,(gc 'green)))))
      (rpm-spec-ghost-face         ((t (:foreground ,(gc 'red)))))
      (rpm-spec-macro-face         ((t (:foreground ,(gc 'yellow)))))
      (rpm-spec-obsolete-tag-face  ((t (:foreground ,(gc 'red)))))
      (rpm-spec-package-face       ((t (:foreground ,(gc 'red)))))
      (rpm-spec-section-face       ((t (:foreground ,(gc 'yellow)))))
      (rpm-spec-tag-face           ((t (:foreground ,(gc 'blue)))))
      (rpm-spec-var-face           ((t (:foreground ,(gc 'red)))))

      ;; rst-mode
      (rst-level-1-face ((t (:inherit color-level-1))))
      (rst-level-2-face ((t (:inherit color-level-2))))
      (rst-level-3-face ((t (:inherit color-level-3))))
      (rst-level-4-face ((t (:inherit color-level-4))))
      (rst-level-5-face ((t (:inherit color-level-5))))
      (rst-level-6-face ((t (:inherit color-level-6))))

      ;; show-paren
      (show-paren-mismatch  ((t (:foreground ,(gc 'white)
                                 :background ,(gc 'red)))))
      (show-paren-match     ((t (:foreground ,(gc 'white)
                                 :background ,(gc 'green+1)))))

      ;; SLIME
      (slime-error-face                 ((t (:inherit font-lock-warning-face))))
      (slime-repl-input-face            ((t (:inherit comint-highlight-input))))
      (slime-repl-output-face           ((t (:foreground ,(gc 'green-1)))))
      (slime-repl-inputed-output-face   ((t (:foreground ,(gc 'red)))))
      (slime-repl-output-mouseover-face ((t (:inherit highlight))))
      (slime-repl-prompt-face           ((t (:inherit comint-highlight-prompt))))
      (slime-repl-result-face           ((t (:foreground ,(gc 'blue+2)))))

      ;; sml-mode-line
      (sml-modeline-end-face ((t :inherit default :width condensed)))

      ;; sunrise-commander
      (sr-active-path-face       ((t (:inherit dired-header))))
      (sr-passive-path-face      ((t (:foreground ,(gc 'fg-2)
                                      :height 1.13
                                      :box (:line-width 1
                                            :color ,(gc 'fg-2)
                                            :style nil)))))
      (sr-directory-face         ((t (:inherit dired-directory))))
      (sr-marked-file-face       ((t (:inherit dired-marked))))
      (sr-marked-dir-face        ((t (:inherit sr-alt-marked-file-face :weight bold))))
      (sr-alt-marked-file-face   ((t (:inherit sr-marked-file-face :slant italic))))
      (sr-alt-marked-dir-face    ((t (:inherit sr-marked-dir-face :slant italic))))
      (sr-symlink-face           ((t (:inherit dired-symlink))))
      (sr-symlink-directory-face ((t (:inherit sr-symlink-face :weight bold))))
      (sr-broken-link-face       ((t (:inherit dired-warning))))
      (sr-highlight-path-face    ((t (:inherit highlight))))
      (sr-editing-path-face      ((t (:foreground ,(gc 'bg-1)
                                      :background ,(gc 'blue-1)))))
      (sr-clex-hotchar-face      ((t (:foreground ,(gc 'red)))))
      (sr-encrypted-face         ((t (:foreground ,(gc 'yellow)))))
      (sr-compressed-face        ((t (:foreground ,(gc 'magenta-1)))))
      (sr-packaged-face          ((t (:foreground ,(gc 'magenta+1)))))
      (sr-log-face               ((t (:foreground ,(gc 'green-1)))))
      (sr-xml-face               ((t (:foreground ,(gc 'green+2)))))
      (sr-html-face              ((t (:foreground ,(gc 'cyan+2)))))

      ;; syslog-mode
      (syslog-error ((t (:inherit font-lock-warning-face))))
      (syslog-warn  ((t (:inherit warning))))
      (syslog-info  ((t (:foreground ,(gc 'blue-2)))))
      (syslog-debug ((t (:foreground ,(gc 'magenta-1)))))
      (syslog-hour  ((t (:foreground ,(gc 'blue+1)))))
      (syslog-su    ((t (:foreground ,(gc 'cyan)))))
      (syslog-ip    ((t (:foreground ,(gc 'yellow+1) :underline t))))

      ;; tabbar
      (tabbar-button     ((t (:foreground ,(gc 'fg+1)
                              :background ,(gc 'bg-1)))))
      (tabbar-selected   ((t (:foreground ,(gc 'fg+1)
                              :background ,(gc 'bg-1)
                              :box (:line-width -1 :style pressed-button)))))
      (tabbar-unselected ((t (:foreground ,(gc 'fg+1)
                              :background ,(gc 'bg)
                              :box (:line-width -1 :style released-button)))))

      ;; term
      (term-color-black       ((t (:foreground ,(gc 'bg-1)
                                   :background ,(gc 'bg-2)))))
      (term-color-red         ((t (:foreground ,(gc 'yellow+2)
                                   :background ,(gc 'red-2)))))
      (term-color-green       ((t (:foreground ,(gc 'green)
                                   :background ,(gc 'green+2)))))
      (term-color-yellow      ((t (:foreground ,(gc 'fg-2)
                                   :background ,(gc 'yellow)))))
      (term-color-blue        ((t (:foreground ,(gc 'blue-1)
                                   :background ,(gc 'cyan+1)))))
      (term-color-magenta     ((t (:foreground ,(gc 'magenta)
                                   :background ,(gc 'red)))))
      (term-color-cyan        ((t (:foreground ,(gc 'cyan)
                                   :background ,(gc 'blue)))))
      (term-color-white       ((t (:foreground ,(gc 'fg+1)
                                   :background ,(gc 'bg+2)))))
      (term-default-fg-color  ((t (:inherit term-color-white))))
      (term-default-bg-color  ((t (:inherit term-color-black))))

      ;; volatile-highlights
      (vhl/default-face ((t (:background ,(gc 'bg-2)))))

      ;; emacs-w3m
      (w3m-anchor                       ((t (:inherit link))))
      (w3m-arrived-anchor               ((t (:inherit link-visited))))
      (w3m-form                         ((t (:foreground ,(gc 'red-1) :underline t))))
      (w3m-form-button                  ((t (:inherit custom-button))))
      (w3m-form-button-pressed          ((t (:inherit custom-button-pressed))))
      (w3m-form-button-mouse            ((t (:inherit custom-button-mouse))))
      (w3m-tab-background               ((t (:inherit default))))
      (w3m-tab-selected                 ((t (:inherit custom-button
                                             :foreground ,(gc 'fg+2)))))
      (w3m-tab-selected-retrieving      ((t (:inherit custom-button
                                             :foreground ,(gc 'red)))))
      (w3m-tab-selected-background      ((t (:background ,(gc 'bg)))))
      (w3m-tab-unselected               ((t (:inherit custom-button
                                             :foreground ,(gc 'fg-1)))))
      (w3m-tab-unselected-retrieving    ((t (:inherit custom-button
                                             :foreground ,(gc 'red+2)))))
      (w3m-tab-unselected-unseen        ((t (:inherit custom-button
                                             :backround ,(gc 'gray)))))
      (w3m-tab-mouse                    ((t (:inherit custom-button-mouse))))
      (w3m-header-line-location-title   ((t (:inherit header-line))))
      (w3m-header-line-location-content ((t (:foreground ,(gc 'yellow+2)
                                             :inherit header-line))))
      (w3m-history-current-url          ((t (:inherit match))))
      (w3m-lnum                         ((t (:foreground ,(gc 'green+2)
                                             :background ,(gc 'bg-1)))))
      (w3m-lnum-match                   ((t (:background ,(gc 'bg-2)
                                             :foreground ,(gc 'fg-2)
                                             :weight bold))))
      (w3m-lnum-minibuffer-prompt       ((t (:foreground ,(gc 'blue-1)))))
      (w3m-image-anchor                 ((t (:background ,(gc 'bg+1)))))

      ;; wanderlust
      (wl-highlight-folder-few-face                     ((t (:foreground ,(gc 'yellow+2)))))
      (wl-highlight-folder-many-face                    ((t (:foreground ,(gc 'red-1)))))
      (wl-highlight-folder-path-face                    ((t (:foreground ,(gc 'fg-2)))))
      (wl-highlight-folder-unread-face                  ((t (:foreground ,(gc 'blue)))))
      (wl-highlight-folder-zero-face                    ((t (:foreground ,(gc 'fg+1)))))
      (wl-highlight-folder-unknown-face                 ((t (:foreground ,(gc 'blue)))))
      (wl-highlight-message-citation-header             ((t (:foreground ,(gc 'red-1)))))
      (wl-highlight-message-cited-text-1                ((t (:foreground ,(gc 'red)))))
      (wl-highlight-message-cited-text-2                ((t (:foreground ,(gc 'green+2)))))
      (wl-highlight-message-cited-text-3                ((t (:foreground ,(gc 'blue)))))
      (wl-highlight-message-cited-text-4                ((t (:foreground ,(gc 'blue+1)))))
      (wl-highlight-message-header-contents-face        ((t (:foreground ,(gc 'green)))))
      (wl-highlight-message-headers-face                ((t (:foreground ,(gc 'red+1)))))
      (wl-highlight-message-important-header-contents   ((t (:foreground ,(gc 'green+2)))))
      (wl-highlight-message-header-contents             ((t (:foreground ,(gc 'green+1)))))
      (wl-highlight-message-important-header-contents2  ((t (:foreground ,(gc 'green+2)))))
      (wl-highlight-message-signature                   ((t (:foreground ,(gc 'green)))))
      (wl-highlight-message-unimportant-header-contents ((t (:foreground ,(gc 'fg+1)))))
      (wl-highlight-summary-answered-face               ((t (:foreground ,(gc 'blue)))))
      (wl-highlight-summary-disposed-face               ((t (:foreground ,(gc 'fg+1)
                                                             :slant italic))))
      (wl-highlight-summary-new-face                    ((t (:foreground ,(gc 'blue)))))
      (wl-highlight-summary-normal-face                 ((t (:foreground ,(gc 'fg+1)))))
      (wl-highlight-summary-thread-top-face             ((t (:foreground ,(gc 'yellow)))))
      (wl-highlight-thread-indent-face                  ((t (:foreground ,(gc 'magenta)))))
      (wl-highlight-summary-refiled-face                ((t (:foreground ,(gc 'fg+1)))))
      (wl-highlight-summary-displaying-face             ((t (:underline t :weight bold))))

      ;; which-func-mode
      (which-func ((t (:foreground ,(gc 'green-2)))))

      ;; whitespace-mode
      (whitespace-space             ((t (:background ,(gc 'bg)
                                         :foreground ,(gc 'blue-2)))))
      (whitespace-hspace            ((t (:background ,(gc 'bg)
                                         :foreground ,(gc 'gray)))))
      (whitespace-tab               ((t (:background ,(gc 'fg-2)
                                         :foreground ,(gc 'blue-2)))))
      (whitespace-newline           ((t (:foreground ,(gc 'blue-2)))))
      (whitespace-trailing          ((t (:background ,(gc 'red)))))
      (whitespace-line              ((t (:background ,(gc 'gray)
                                         :foreground ,(gc 'white)))))
      (whitespace-space-before-tab  ((t (:background ,(gc 'fg-2)
                                         :foreground ,(gc 'fg-2)))))
      (whitespace-indentation       ((t (:background ,(gc 'yellow-2)
                                         :foreground ,(gc 'red-2)))))
      (whitespace-empty             ((t (:background ,(gc 'yellow)))))
      (whitespace-space-after-tab   ((t (:background ,(gc 'yellow-2)
                                         :foreground ,(gc 'red-2)))))

      ;; widget
      (widget-field             ((t (:background ,(gc 'bg)
                                     :box (:line-width -1
                                           :color ,(gc 'fg-2)
                                           :style nil)))))
      (widget-button            ((t (:foreground ,(gc 'blue-1) :weight bold))))
      (widget-button-pressed    ((t (:foreground ,(gc 'blue+2) :inherit widget-button))))
      (widget-documentation     ((t (:foreground ,(gc 'green-1)))))
      (widget-inactive          ((t (:inherit shadow))))
      (widget-single-line-field ((t (:foreground ,(gc 'fg)
                                     :inherit widget-field))))

      ;; yascroll
      (yascroll:thumb-text-area ((t (:background ,(gc 'bg-2)))))
      (yascroll:thumb-fringe    ((t (:background ,(gc 'bg-2)
                                     :foreground ,(gc 'fg-2))))))))

(defun alect-get-vars (theme)
  "Return a list for `custom-theme-set-variables' function."
  (cl-flet ((gc (col) (alect-get-color theme col)))
    `((ansi-color-names-vector
       [,(gc 'bg-1)
        ,(gc 'red)
        ,(gc 'green)
        ,(gc 'yellow)
        ,(gc 'blue)
        ,(gc 'magenta)
        ,(gc 'cyan)
        ,(gc 'fg+1)])

      ;; emms icon at mode line (is taken from emms source)
      (emms-mode-line-icon-image-cache
       '(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c " (gc 'green+1)  "\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };"))
       t)

      ;; gnus icon at mode line (is taken from gnus source)
      (gnus-mode-line-image-cache
       '(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c " (gc 'blue-1) "\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };"))
       t)

      ;; fill-column-indicator
      (fci-rule-color ,(gc 'bg-2))

      ;; vc-annotate
      (vc-annotate-color-map
       '(( 20 . ,(gc 'red-1))
         ( 40 . ,(gc 'red))
         ( 60 . ,(gc 'yellow-2))
         ( 80 . ,(gc 'yellow-1))
         (100 . ,(gc 'yellow))
         (120 . ,(gc 'yellow+1))
         (140 . ,(gc 'green-1))
         (160 . ,(gc 'green-2))
         (180 . ,(gc 'green))
         (200 . ,(gc 'green+1))
         (220 . ,(gc 'green+2))
         (240 . ,(gc 'cyan-1))
         (260 . ,(gc 'cyan))
         (280 . ,(gc 'cyan-2))
         (300 . ,(gc 'blue-1))
         (320 . ,(gc 'blue))
         (340 . ,(gc 'blue+1))
         (360 . ,(gc 'magenta))))
      (vc-annotate-very-old-color ,(gc 'magenta))
      (vc-annotate-background ,(gc 'bg-2))
      )))

(defmacro alect-create-theme (theme)
  (let ((theme-name  (intern (concat "alect-" (symbol-name theme))))
        (theme-vars  (alect-get-vars theme))
        (theme-faces (alect-get-faces theme)))
    ;; FIXME is there a way to avoid this?: variables are not set with
    ;; `custom-theme-set-variables' if they have not been defined yet
    (defvar emms-mode-line-icon-image-cache nil)
    (defvar gnus-mode-line-image-cache nil)

    `(progn
       (deftheme ,theme-name ,(format "Just another %s color theme." theme))
       (apply 'custom-theme-set-variables ',theme-name ',theme-vars)
       (apply 'custom-theme-set-faces     ',theme-name ',theme-faces)
       (provide-theme ',theme-name))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'alect-themes)

;; Local Variables:
;; fill-column: 72
;; eval: (and (fboundp 'rainbow-mode) (rainbow-mode))
;; End:

;;; alect-themes.el ends here
