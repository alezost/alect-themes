;;; alect-themes.el --- Configurable light, dark and black themes for Emacs 24 or later   -*- lexical-binding: t -*-

;; Copyright (C) 2013–2018 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 10 Jul 2013
;; Version: 0.8
;; Package-Requires: ((emacs "24.0"))
;; URL: https://github.com/alezost/alect-themes
;; Keywords: color theme

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides 6 highly customizable color themes (light, dark
;; and black) for GNU Emacs 24 or later.  These themes are intended to
;; be used with GUI, so only graphical terminals are supported by
;; default.  However you can "enable" the themes for other classes of
;; terminals with `alect-display-class' variable.

;; You can install the package from MELPA.  If you prefer the manual
;; installation, put these lines into your init-file:
;;
;;   (add-to-list 'load-path "/path/to/alect-themes")
;;   (add-to-list 'custom-theme-load-path "/path/to/alect-themes")

;; If you also want to enable a theme on Emacs start, use this:
;;
;;   (load-theme 'alect-light t)

;; Ways for configuring the themes:
;;
;; 1. Modifying color palette for the themes - by customizing
;;    `alect-colors' variable.  You may use `alect-generate-colors' to
;;    redefine the whole variable (see the code) or `alect-set-color' to
;;    modify specified colors.
;;
;; 2. Ignoring faces and variables.  You can force alect-themes not to
;;    modify some (or all) faces and variables with
;;    `alect-ignored-faces' and `alect-ignored-variables' variables.
;;
;; 3. Overriding face specification.  You can change the look of
;;    particular faces by setting `alect-overriding-faces' variable.
;;
;; 4. Above that, the inverted color themes (`alect-light-alt' and
;;    `alect-dark-alt') can be configured with
;;    `alect-inverted-color-regexp' variable.

;; For full description and some screenshots, see
;; <https://github.com/alezost/alect-themes>.
;; All screenshots can be found at <http://imgur.com/a/eBx96>.

;;; Code:

(require 'cl-lib)

(defun alect-put-colors (color-name theme-names color-vals var)
  "Put theme colors into the variable VAR.

THEME-NAMES is a list of symbols.  Theme names should already
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
  (let ((cols (mapcar #'list theme-names)))
    (dolist (elem colors)
      (alect-put-colors (car elem) theme-names (cdr elem) cols))
    cols))

(defgroup alect nil
  "Options for alect color themes."
  :group 'faces)

(defgroup alect-faces nil
  "Auxiliary faces used by alect color themes."
  :group 'alect
  :group 'faces)

(defface alect-prompt
  '((t nil))
  "Auxiliary face for inheriting by some other faces.
Used for various prompts like `minibuffer-prompt' or `eshell-prompt'."
  :group 'alect-faces)

(defface alect-time
  '((t nil))
  "Auxiliary face for inheriting by some other faces.
Used for date/time faces like `org-date' or `erc-timestamp-face'."
  :group 'alect-faces)

(defface alect-file
  '((t nil))
  "Auxiliary face for inheriting by some other faces.
Used for 'file name' faces like `change-log-file' or
`compilation-info'."
  :group 'alect-faces)

(defface alect-author
  '((t nil))
  "Auxiliary face for inheriting by some other faces.
Used for author faces like `magit-log-author' or `change-log-name'."
  :group 'alect-faces)

(defface alect-key
  '((t nil))
  "Auxiliary face for inheriting by some other faces.
Used for key faces like `apropos-keybinding' or `magit-popup-key'."
  :group 'alect-faces)

(defface alect-selected-item
  '((t nil))
  "Auxiliary face for inheriting by some other faces.
Used for selected items like `org-date-selected' or
`gnus-summary-selected'."
  :group 'alect-faces)

(defface alect-title
  '((t nil))
  "Auxiliary face for inheriting by some other faces.
Used for titles without levels like `dired-header' or
`magit-section-title'."
  :group 'alect-faces)

(defface alect-field-title
  '((t nil))
  "Auxiliary face for inheriting by some other faces.
Used for field titles like `package-help-section-name' or
`message-header-name'."
  :group 'alect-faces)

(defface alect-button
  '((t nil))
  "Auxiliary face for inheriting by some other faces.
Used for buttons like `custom-button' or `w3m-form-button'."
  :group 'alect-faces)

(defface alect-button-pressed
  '((t nil))
  "Auxiliary face for inheriting by some other faces.
Used for buttons like `custom-button-pressed' or
`w3m-form-button-pressed'."
  :group 'alect-faces)

(defface alect-button-mouse
  '((t nil))
  "Auxiliary face for inheriting by some other faces.
Used for buttons like `custom-button-mouse' or
`w3m-form-button-mouse'."
  :group 'alect-faces)

(defface alect-tab-default
  '((t nil))
  "Auxiliary face for inheriting by some other faces.
Used for faces like `tabbar-default' or `w3m-tab-background'."
  :group 'alect-faces)

(defface alect-tab-unselected
  '((t nil))
  "Auxiliary face for inheriting by some other faces.
Used for tabs like `tabbar-unselected' or `w3m-tab-unselected'."
  :group 'alect-faces)

(defface alect-tab-selected
  '((t nil))
  "Auxiliary face for inheriting by some other faces.
Used for tabs like `tabbar-selected' or `w3m-tab-selected'."
  :group 'alect-faces)

(defface alect-tab-mouse
  '((t nil))
  "Auxiliary face for inheriting by some other faces.
Used for tabs like `tabbar-highlight' or `w3m-tab-mouse'."
  :group 'alect-faces)

(defmacro alect-define-color-level-face (n)
  "Define face for color level N.
Name of the defined face is `alect-color-level-N'."
  `(defface ,(intern (format "alect-color-level-%d" n))
     '((t nil))
     "Auxiliary face for inheriting by some other faces."
     :group 'alect-faces))

(defmacro alect-define-title-face (n)
  "Define title face for level N.
Name of the defined face is `alect-title-N'."
  `(defface ,(intern (format "alect-title-%d" n))
     '((t nil))
     ,(format "Auxiliary face for inheriting by some other faces.
Used for titles with levels like `org-level-%s' or
`markdown-header-face-%s'." n n)
     :group 'alect-faces))

(defmacro alect-define-faces (definer n)
  `(progn
     ,@(mapcar (lambda (i) (list definer i))
               (number-sequence 1 n))))

(alect-define-faces alect-define-color-level-face 12)
(alect-define-faces alect-define-title-face 8)

(defcustom alect-header-height 1.13
  "Height of `header-line' face."
  :type 'number
  :group 'alect)

(defcustom alect-single-title-height 1.13
  "Height of `alect-title' face."
  :type 'number
  :group 'alect)

(defcustom alect-multiple-titles-height 1.13
  "Height of `alect-title-N' faces."
  :type 'number
  :group 'alect)

(defcustom alect-overriding-faces nil
  "List of faces that override original themed faces.

The faces should be in a form accepted by `custom-theme-set-faces'.
Instead of color values (like \"SkyBlue\" or \"#abcdef\") you may
use the names of colors from `alect-colors' (like `magenta' or
`blue+1').  During loading a theme these symbols will be
substituted with values according to the current theme (light or
dark).

Use this variable if you want alect-themes to use non-default
specifications of faces.

Example:
  (setq alect-overriding-faces
        '((mode-line-buffer-id ((t :foreground bg-2 :weight bold)))
          (mode-line           ((t :foreground bg-1 :background fg+1
                                   :box (:line-width 2 :color bg-2))))))
Evaluate it and reload an alect-theme to see the difference."
  :type 'sexp
  :group 'alect)

(defcustom alect-colors
  (alect-generate-colors
   '(light dark black)
   '((cursor       "#1074cd" "#d0d060" "#b1c721")
     (gray-2       "#fafafa" "#e9e9e9" "#dedede")
     (gray-1       "#adadad" "#c0c0c0" "#bababa")
     (gray         "#909090" "#9f9f9f" "#9b9b9b")
     (gray+1       "#444444" "#505050" "#555555")
     (gray+2       "#070707" "#000000" "#000000")
     (fg-2         "#6c6c6c" "#8c826d" "#8b806c")
     (fg-1         "#505050" "#d0bf8f" "#ab9861")
     (fg           "#3f3f3f" "#f0dfaf" "#c4ad63")
     (fg+1         "#262626" "#d5d2be" "#b2af95")
     (fg+2         "#101010" "#f6f0e1" "#d6cbae")
     (bg-2         "#f6f0e1" "#222222" "#404040")
     (bg-1         "#ded6c5" "#3f3f3f" "#000000")
     (bg           "#d9ceb2" "#4f4f4f" "#202020")
     (bg+1         "#d4caa7" "#5f5f5f" "#303030")
     (bg+2         "#ccc19b" "#6f6f6f" "#454545")
     (red-2        "#fa5151" "#fa6a6e" "#e96060")
     (red-1        "#e43838" "#fa5151" "#ea4141")
     (red          "#f71010" "#ea3838" "#db4334")
     (red+1        "#d81212" "#db4334" "#c83029")
     (red+2        "#b22222" "#c83029" "#ae2823")
     (red-bg-1     "#ff6868" "#c64242" "#a52621")
     (red-bg       "#fb9494" "#a83838" "#86201c")
     (red-bg+1     "#eec5c5" "#6a3636" "#531311")
     (yellow-2     "#ab9c3a" "#f8ffa0" "#e9e953")
     (yellow-1     "#9ca30b" "#e8e815" "#c9d617")
     (yellow       "#ef8300" "#fe8b04" "#dc7700")
     (yellow+1     "#958323" "#e5c900" "#bcaa00")
     (yellow+2     "#6a621b" "#abab3a" "#959508")
     (yellow-bg-1  "#cbcb20" "#76742d" "#73712a")
     (yellow-bg    "#dddd44" "#5e5c28" "#565624")
     (yellow-bg+1  "#e0e0a0" "#3c3c20" "#35351c")
     (green-2      "#3cb368" "#8ce096" "#47cd57")
     (green-1      "#1c9e28" "#32cd32" "#29b029")
     (green        "#028902" "#7fb07f" "#60a060")
     (green+1      "#008b45" "#3cb370" "#319448")
     (green+2      "#077707" "#099709" "#078607")
     (green-bg-1   "#58c87c" "#31945c" "#297d4d")
     (green-bg     "#9cdb6c" "#247744" "#1f673b")
     (green-bg+1   "#c9e6b3" "#2c5434" "#203f26")
     (cyan-2       "#0eaeae" "#8cf1f1" "#26d5d5")
     (cyan-1       "#259ea2" "#2fdbde" "#1ec1c4")
     (cyan         "#358d8d" "#1fb3b3" "#1ba1a1")
     (cyan+1       "#0d7b72" "#528d8d" "#4c8383")
     (cyan+2       "#286060" "#0c8782" "#0a7874")
     (cyan-bg-1    "#4ecad7" "#1a758a" "#155f70")
     (cyan-bg      "#80d7db" "#195f73" "#0f414d")
     (cyan-bg+1    "#c3d4d7" "#235050" "#132c2c")
     (blue-2       "#0092ff" "#b0c0ff" "#8cb7ff")
     (blue-1       "#2c53ca" "#94bff3" "#58b1f3")
     (blue         "#1111ff" "#62b6ea" "#00a2f5")
     (blue+1       "#2020cc" "#30a5f5" "#1e7bda")
     (blue+2       "#00008b" "#3390dc" "#2062d0")
     (blue-bg-1    "#7cc0f7" "#1a63b3" "#144f8f")
     (blue-bg      "#b0d0f3" "#134b87" "#0c325a")
     (blue-bg+1    "#bcd9f5" "#2b3f6b" "#0d1a38")
     (magenta-2    "#dc63dc" "#ebabde" "#dc8cc3")
     (magenta-1    "#ba55d3" "#dc8cc3" "#e353b9")
     (magenta      "#a020f0" "#e353b9" "#da26ce")
     (magenta+1    "#9400d3" "#e81eda" "#c251df")
     (magenta+2    "#8b008b" "#be59d8" "#a92ec9")
     (magenta-bg-1 "#e98bb7" "#864d7d" "#72416a")
     (magenta-bg   "#e5b3c4" "#6e4266" "#54324e")
     (magenta-bg+1 "#ecd0d0" "#55334f" "#351f31")))
  "List of lists containing color palettes for alect-themes.

List ((theme (color . val) ...) ...).

Each list is a cons cell of a theme name (symbol) and alist of
color names (symbols) and values (strings)."
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol
                                   :value-type color))
  ;; another suitable variant
  ;; :type '(repeat (cons symbol
  ;;                      (alist :key-type symbol :value-type color)))
  :group 'alect)

(defun alect-set-color (theme-name color-name color-val)
  "Set color COLOR-NAME of a theme THEME-NAME to the value of COLOR-VAL.
COLOR-NAME and THEME-NAME are symbols, COLOR-VAL is a string.
See `alect-colors' for details."
  (let ((color-alist (cdr (assoc theme-name alect-colors))))
    (or color-alist
        (error "Theme '%s' does not exist" theme-name))
    (let ((color-cons (assoc color-name color-alist)))
      (or color-cons
          (error "Color '%s' does not exist" color-name))
      (setcdr color-cons color-val))))

(defcustom alect-inverted-color-regexp
  "^\\(red\\|yellow\\|green\\|cyan\\|blue\\|magenta\\)\\([-+]\\)\\([012]\\)$"
  "Regexp matching a name of the color for inverted theme.

The first parenthesized group should match a base color
name (e.g. \"fg\" or \"blue\").
The second group should match a sign (\"-\" or \"+\").
The third group should match a color number (0, 1 or 2).

For available color names, see `alect-colors'.
For description of inverting colors, see `alect-get-color'."
  :type 'regexp
  :group 'alect)

(defun alect-get-color (theme-name color-name &optional invert)
  "Return the value of color COLOR-NAME for a theme THEME-NAME.

If INVERT is non-nil, return the value of the \"opposite\" color.
E.g. use the value of \"magenta+1\" if COLOR-NAME is
\"magenta-1\" or use \"red-2\" instead of \"red+2\" and so on.
Invert only the color matching `alect-inverted-color-regexp'.

For the values of THEME-NAME and COLOR-NAME, see `alect-colors'."
  (and invert
       (let ((color (symbol-name color-name)))
         (and (string-match alect-inverted-color-regexp color)
              (let ((base (match-string 1 color))
                    (sign (match-string 2 color))
                    (num  (match-string 3 color)))
                (and base sign num
                     (setq color-name
                           (intern (concat base
                                           (if (equal sign "-")
                                               "+"
                                             "-")
                                           num))))))))
  (cdr (assoc color-name
              (cdr (assoc theme-name alect-colors)))))

(defcustom alect-display-class
  '((type graphic))
  "Class of terminals (DISPLAY) for which alect-themes are applied.
For other terminals, faces stay unthemed.
See Info node `(elisp) Defining Faces' for the possibilities for
DISPLAY."
  :type '(choice
          (const :tag "Graphical terminals" ((type graphic)))
          (const :tag "Terminals with at least 256 colors"
                 ((class color) (min-colors 256)))
          (const :tag "All terminals")
          (sexp :tag "Other"))
  :group 'alect)

(defun alect-get-customization (theme &optional invert)
  "Return cons of settings for theme THEME.
Car of the cons is a list for `custom-theme-set-faces' function.
Cdr of the cons is a list for `custom-theme-set-variables' function.
THEME is a name of the color theme (symbol from `alect-colors').
For INVERT, see `alect-get-color'."
  (cl-flet ((gc (col) (alect-get-color theme col invert)))
    (let ((c alect-display-class))
      (cons
       ;; FACES
       `( ;; basic colors
         (default             ((,c :foreground ,(gc 'fg+1)
                                   :background ,(gc 'bg-1))))
         (cursor              ((,c :background ,(gc 'cursor))))
         (button              ((,c :inherit link
                                   :underline (:color ,(gc 'fg+1)))))
         (link                ((,c :foreground ,(gc 'blue-1)
                                   :underline t)))
         (link-visited        ((,c :foreground ,(gc 'blue+2)
                                   :underline t)))
         (match               ((,c :foreground ,(gc 'fg+1)
                                   :background ,(gc 'blue-bg))))
         (escape-glyph        ((,c :foreground ,(gc 'yellow)
                                   :weight bold)))
         (fringe              ((,c :foreground ,(gc 'gray)
                                   :background ,(gc 'bg-2))))
         (header-line         ((,c :foreground ,(gc 'fg+2)
                                   :height ,alect-header-height
                                   :box (:line-width 1
                                         :color ,(gc 'fg+2)
                                         :style nil))))
         (highlight           ((,c :foreground ,(gc 'gray+2)
                                   :background ,(gc 'gray-2))))
         (shadow              ((,c :foreground ,(gc 'gray))))
         (success             ((,c :foreground ,(gc 'green)
                                   :weight bold)))
         (error               ((,c :foreground ,(gc 'red)
                                   :weight bold)))
         (warning             ((,c :foreground ,(gc 'yellow-1))))
         (region              ((,c :background ,(gc 'bg+2))))
         (menu                ((,c :foreground ,(gc 'fg+2)
                                   :background ,(gc 'bg-2)
                                   :height ,alect-header-height)))
         (tool-bar            ((,c :inherit mode-line)))
         (scroll-bar          ((,c :background ,(gc 'bg+2))))
         (tooltip             ((,c :inherit variable-pitch
                                   :foreground ,(gc 'cyan+2)
                                   :background ,(gc 'bg-2))))
         (minibuffer-prompt   ((,c :inherit alect-prompt)))
         (secondary-selection ((,c :background ,(gc 'bg+1))))
         (trailing-whitespace ((,c :background ,(gc 'red-bg-1))))
         (vertical-border     ((,c :foreground ,(gc 'fg+1))))
         (window-divider      ((,c :foreground ,(gc 'green+2))))
         (window-divider-first-pixel ((,c :foreground ,(gc 'green+1))))
         (window-divider-last-pixel  ((,c :inherit window-divider-first-pixel)))

         ;; auxiliary faces for inheriting
         (alect-field-title    ((,c :foreground ,(gc 'yellow+1))))
         (alect-prompt         ((,c :foreground ,(gc 'magenta-1)
                                    :weight bold)))
         (alect-time           ((,c :foreground ,(gc 'cyan-2))))
         (alect-file           ((,c :foreground ,(gc 'green+1))))
         (alect-author         ((,c :foreground ,(gc 'magenta-1))))
         (alect-key            ((,c :foreground ,(gc 'red-2)
                                    :weight bold)))
         (alect-selected-item  ((,c :background ,(gc 'bg)
                                    :box (:line-width -1
                                          :color ,(gc 'fg+1)
                                          :style nil))))
         (alect-color-level-1  ((,c :foreground ,(gc 'blue+1))))
         (alect-color-level-2  ((,c :foreground ,(gc 'green))))
         (alect-color-level-3  ((,c :foreground ,(gc 'red+1))))
         (alect-color-level-4  ((,c :foreground ,(gc 'yellow+2))))
         (alect-color-level-5  ((,c :foreground ,(gc 'cyan+1))))
         (alect-color-level-6  ((,c :foreground ,(gc 'blue-1))))
         (alect-color-level-7  ((,c :foreground ,(gc 'magenta-1))))
         (alect-color-level-8  ((,c :foreground ,(gc 'yellow))))
         (alect-color-level-9  ((,c :foreground ,(gc 'green-1))))
         (alect-color-level-10 ((,c :foreground ,(gc 'red-2))))
         (alect-color-level-11 ((,c :foreground ,(gc 'cyan-2))))
         (alect-color-level-12 ((,c :foreground ,(gc 'magenta+2))))

         (alect-title          ((,c :foreground ,(gc 'green+2) :weight bold
                                    :height ,alect-single-title-height)))
         (alect-title-1        ((,c :inherit alect-color-level-1 :weight bold
                                    :height ,alect-multiple-titles-height)))
         (alect-title-2        ((,c :inherit alect-color-level-2 :weight bold
                                    :height ,alect-multiple-titles-height)))
         (alect-title-3        ((,c :inherit alect-color-level-3 :weight bold
                                    :height ,alect-multiple-titles-height)))
         (alect-title-4        ((,c :inherit alect-color-level-4 :weight bold
                                    :height ,alect-multiple-titles-height)))
         (alect-title-5        ((,c :inherit alect-color-level-5 :weight bold
                                    :height ,alect-multiple-titles-height)))
         (alect-title-6        ((,c :inherit alect-color-level-6 :weight bold
                                    :height ,alect-multiple-titles-height)))
         (alect-title-7        ((,c :inherit alect-color-level-7 :weight bold
                                    :height ,alect-multiple-titles-height)))
         (alect-title-8        ((,c :inherit alect-color-level-8 :weight bold
                                    :height ,alect-multiple-titles-height)))

         (alect-button         ((,c :foreground ,(gc 'fg+2)
                                    :background ,(gc 'bg+2)
                                    :box (:line-width 2
                                          :style released-button))))
         (alect-button-pressed ((,c :inherit alect-button
                                    :box (:line-width 2
                                          :style pressed-button))))
         (alect-button-mouse   ((,c :inherit highlight
                                    :box (:line-width 2
                                          :style released-button))))
         (alect-tab-default    ((,c :height 0.9 :box nil)))
         (alect-tab-unselected ((,c :inherit alect-tab-default
                                    :foreground ,(gc 'fg-2)
                                    :background ,(gc 'bg)
                                    :box (:line-width 1
                                          :style released-button))))
         (alect-tab-selected   ((,c :inherit alect-tab-unselected
                                    :foreground ,(gc 'fg+2)
                                    :background ,(gc 'bg+1))))
         (alect-tab-mouse      ((,c :inherit alect-tab-default
                                    :inherit mode-line-highlight)))

         ;; apropos
         (apropos-keybinding      ((,c :inherit alect-key)))
         (apropos-symbol          ((,c :foreground ,(gc 'green+1)
                                       :weight bold)))

         ;; auctex
         (font-latex-bold     ((,c :inherit bold)))
         (font-latex-warning  ((,c :inherit font-lock-warning-face)))
         (font-latex-sedate   ((,c :foreground ,(gc 'yellow)
                                   :weight bold )))
         (font-latex-title-4  ((,c :inherit variable-pitch
                                   :weight bold)))

         ;; aurel
         (aurel-info-id           ((,c :foreground ,(gc 'fg+2))))
         (aurel-info-name         ((,c :inherit alect-title)))
         (aurel-info-maintainer   ((,c :foreground ,(gc 'magenta-1)
                                       :weight bold)))
         (aurel-info-date         ((,c :inherit alect-time)))
         (aurel-info-license      ((,c :foreground ,(gc 'yellow))))
         (aurel-info-version      ((,c :foreground ,(gc 'yellow+2))))
         (aurel-info-category     ((,c :foreground ,(gc 'green-1))))
         (aurel-info-size         ((,c :foreground ,(gc 'red-1))))
         (aurel-info-provides     ((,c :foreground ,(gc 'green+2))))
         (aurel-info-depends      ((,c :foreground ,(gc 'fg))))
         (aurel-info-depends-opt  ((,c :foreground ,(gc 'fg-2))))
         (aurel-info-required     ((,c :foreground ,(gc 'fg))))
         (aurel-info-optional-for ((,c :foreground ,(gc 'fg-2))))
         (aurel-info-voted        ((,c :foreground ,(gc 'green)
                                       :weight bold)))

         ;; auto-complete
         (ac-candidate-face       ((,c :foreground ,(gc 'gray+2)
                                       :background ,(gc 'bg+2))))
         (ac-selection-face       ((,c :foreground ,(gc 'bg-1)
                                       :background ,(gc 'fg+1))))
         (ac-completion-face      ((,c :foreground ,(gc 'fg-2))))
         (ac-candidate-mouse-face ((,c :inherit highlight)))

         ;; bui
         (bui-action-button       ((,c :inherit alect-button)))
         (bui-action-button-mouse ((,c :inherit alect-button-mouse)))
         (bui-file-name           ((,c :inherit alect-file
                                       :underline t)))
         (bui-time                ((,c :inherit alect-time)))
         (bui-url                 ((,c :inherit link)))
         (bui-info-heading        ((,c :inherit alect-title)))
         (bui-info-param-title    ((,c :inherit alect-field-title)))
         (bui-hint-key            ((,c :inherit alect-key)))

         ;; calendar, diary
         (calendar-today    ((,c :box (:line-width -1
                                       :color ,(gc 'red)
                                       :style nil))))
         (diary             ((,c :foreground ,(gc 'green+2) :weight bold)))
         (diary-anniversary ((,c :foreground ,(gc 'red))))
         (diary-time        ((,c :inherit alect-time)))
         (holiday           ((,c :background ,(gc 'bg+2))))

         ;; change-log
         (change-log-date           ((,c :inherit alect-time)))
         (change-log-name           ((,c :inherit alect-author)))
         (change-log-email          ((,c :foreground ,(gc 'red-2))))
         (change-log-file           ((,c :inherit alect-file)))
         (change-log-list           ((,c :inherit font-lock-function-name-face)))
         (change-log-conditionals   ((,c :inherit font-lock-variable-name-face)))
         (change-log-function       ((,c :foreground ,(gc 'yellow))))
         (change-log-acknowledgment ((,c :inherit font-lock-keyword-face)))

         ;; comint
         (comint-highlight-prompt ((,c :inherit alect-prompt)))
         (comint-highlight-input  ((,c :weight bold)))

         ;; company
         (company-tooltip                   ((,c :foreground ,(gc 'fg-1)
                                                 :background ,(gc 'bg+1))))
         (company-tooltip-selection         ((,c :foreground ,(gc 'fg-1)
                                                 :background ,(gc 'bg-2))))
         (company-tooltip-mouse             ((,c :inherit highlight)))
         (company-tooltip-annotation        ((,c :inherit company-tooltip
                                                 :foreground ,(gc 'blue))))
         (company-tooltip-common            ((,c :inherit company-tooltip
                                                 :foreground ,(gc 'fg+1))))
         (company-tooltip-common-selection  ((,c :inherit company-tooltip-selection
                                                 :foreground ,(gc 'fg+1))))
         (company-preview                   ((,c :foreground ,(gc 'fg-1))))
         (company-preview-common            ((,c :inherit company-preview
                                                 :underline t)))
         (company-scrollbar-fg              ((,c :background ,(gc 'gray+1))))
         (company-scrollbar-bg              ((,c :background ,(gc 'gray))))

         ;; compilation
         (compilation-info                  ((,c :inherit alect-file)))
         (compilation-warning               ((,c :inherit warning)))
         (compilation-error                 ((,c :inherit error)))
         (compilation-line-number           ((,c :foreground ,(gc 'yellow))))
         (compilation-column-number         ((,c :foreground ,(gc 'yellow+2))))
         (compilation-mode-line-run         ((,c :foreground ,(gc 'blue))))
         (compilation-mode-line-exit        ((,c :inherit success)))
         (compilation-mode-line-fail        ((,c :inherit compilation-error)))

         ;; completions
         (completions-common-part ((,c :foreground ,(gc 'fg-2))))

         ;; cperl
         (cperl-hash-face           ((,c :foreground ,(gc 'cyan+1))))
         (cperl-array-face          ((,c :foreground ,(gc 'yellow-1))))
         (cperl-nonoverridable-face ((,c :foreground ,(gc 'green-1))))

         ;; ctable
         (ctbl:face-cell-select  ((,c :background ,(gc 'blue)
                                      :foreground ,(gc 'bg-1))))
         (ctbl:face-continue-bar ((,c :background ,(gc 'bg-2)
                                      :foreground ,(gc 'bg-1))))
         (ctbl:face-row-select   ((,c :background ,(gc 'cyan)
                                      :foreground ,(gc 'bg-1))))

         ;; customization
         (custom-button                  ((,c :inherit alect-button)))
         (custom-button-pressed          ((,c :inherit alect-button-pressed)))
         (custom-button-mouse            ((,c :inherit alect-button-mouse)))
         (custom-button-unraised         ((,c :inherit alect-button :box nil)))
         (custom-button-pressed-unraised ((,c :inherit custom-button-unraised
                                              :underline t)))
         (custom-documentation           ((,c :inherit font-lock-doc-face)))
         (custom-comment                 ((,c :foreground ,(gc 'gray))))
         (custom-tag                     ((,c :foreground ,(gc 'blue+2))))
         (custom-state                   ((,c :foreground ,(gc 'green+1))))
         (custom-link                    ((,c :inherit link)))
         (custom-group-tag               ((,c :inherit alect-title-1)))
         (custom-group-tag-1             ((,c :inherit alect-title-2)))
         (custom-group-subtitle          ((,c :inherit alect-title-3)))
         (custom-face-tag                ((,c :foreground ,(gc 'magenta+1)
                                              :weight bold)))
         (custom-variable-tag            ((,c :inherit font-lock-variable-name-face
                                              :weight bold)))
         (custom-variable-button         ((,c :weight bold :underline t)))
         (custom-visibility              ((,c :inherit link :height 0.8)))

         ;; debbugs
         (debbugs-gnu-done     ((,c :foreground ,(gc 'fg-1))))
         (debbugs-gnu-handled  ((,c :inherit gnus-summary-normal-read)))
         (debbugs-gnu-new      ((,c :inherit gnus-summary-normal-unread)))
         (debbugs-gnu-archived ((,c :foreground ,(gc 'fg-2))))
         (debbugs-gnu-stale    ((,c :foreground ,(gc 'yellow))))
         (debbugs-gnu-pending  ((,c :foreground ,(gc 'blue))))
         (debbugs-gnu-tagged   ((,c :foreground ,(gc 'red))))

         ;; dictem
         (dictem-reference-definition-face ((,c :inherit link :underline nil)))
         (dictem-database-description-face ((,c :inherit alect-title)))
         (dictem-reference-dbname-face     ((,c :foreground ,(gc 'red+1))))
         (dictem-reference-m1-face         ((,c :foreground ,(gc 'cyan))))
         (dictem-reference-m2-face         ((,c :foreground ,(gc 'green))))

         ;; diff
         (diff-context           ((,c :foreground ,(gc 'fg-1))))
         (diff-added             ((,c :foreground ,(gc 'green-1))))
         (diff-changed           ((,c :foreground ,(gc 'yellow-1))))
         (diff-removed           ((,c :foreground ,(gc 'red-1))))
         (diff-indicator-added   ((,c :inherit diff-added)))
         (diff-indicator-changed ((,c :inherit diff-changed)))
         (diff-indicator-removed ((,c :inherit diff-removed)))
         (diff-refine-added      ((,c :inherit diff-added
                                      :underline t)))
         (diff-refine-changed    ((,c :inherit diff-changed
                                      :underline t)))
         (diff-refine-removed    ((,c :inherit diff-removed
                                      :underline t)))
         (diff-header            ((,c :foreground ,(gc 'blue-2) :weight bold)))
         (diff-hunk-header       ((,c :inherit diff-header
                                      :foreground ,(gc 'green+2))))
         (diff-file-header       ((,c :inherit diff-header
                                      :foreground ,(gc 'fg+2))))
         (diff-function          ((,c :inherit diff-header
                                      :foreground ,(gc 'blue))))
         (diff-index             ((,c :inherit diff-header
                                      :foreground ,(gc 'red-1))))
         (diff-nonexistent       ((,c :inherit diff-header
                                      :foreground ,(gc 'gray))))

         ;; diff-hl
         (diff-hl-insert ((,c :foreground ,(gc 'fg+1)
                              :background ,(gc 'green-bg))))
         (diff-hl-delete ((,c :inherit diff-hl-insert
                              :background ,(gc 'red-bg))))
         (diff-hl-change ((,c :inherit diff-hl-insert
                              :background ,(gc 'blue-bg))))

         ;; dired
         (dired-directory  ((,c :inherit font-lock-function-name-face)))
         (dired-flagged    ((,c :foreground ,(gc 'red))))
         (dired-header     ((,c :inherit alect-title)))
         (dired-ignored    ((,c :foreground ,(gc 'gray))))
         (dired-mark       ((,c :foreground ,(gc 'blue+1))))
         (dired-marked     ((,c :inherit warning)))
         (dired-perm-write ((,c :foreground ,(gc 'green-1))))
         (dired-symlink    ((,c :inherit font-lock-constant-face)))
         (dired-warning    ((,c :inherit font-lock-warning-face
                                :background ,(gc 'bg-2))))

         ;; ediff
         (ediff-current-diff-A        ((,c :background ,(gc 'red-bg+1))))
         (ediff-fine-diff-A           ((,c :background ,(gc 'red-bg))))
         (ediff-even-diff-A           ((,c :background ,(gc 'bg))))
         (ediff-odd-diff-A            ((,c :background ,(gc 'bg+1))))
         (ediff-current-diff-B        ((,c :background ,(gc 'green-bg+1))))
         (ediff-fine-diff-B           ((,c :background ,(gc 'green-bg))))
         (ediff-even-diff-B           ((,c :inherit ediff-even-diff-A)))
         (ediff-odd-diff-B            ((,c :inherit ediff-odd-diff-A)))
         (ediff-current-diff-C        ((,c :background ,(gc 'yellow-bg+1))))
         (ediff-fine-diff-C           ((,c :background ,(gc 'yellow-bg))))
         (ediff-even-diff-C           ((,c :inherit ediff-even-diff-A)))
         (ediff-odd-diff-C            ((,c :inherit ediff-odd-diff-A)))
         (ediff-current-diff-Ancestor ((,c :background ,(gc 'magenta-bg+1))))
         (ediff-fine-diff-Ancestor    ((,c :background ,(gc 'magenta-bg))))
         (ediff-even-diff-Ancestor    ((,c :inherit ediff-even-diff-A)))
         (ediff-odd-diff-Ancestor     ((,c :inherit ediff-odd-diff-A)))

         ;; eldoc
         (eldoc-highlight-function-argument ((,c :foreground ,(gc 'green)
                                                 :weight bold)))

         ;; emms
         (emms-playlist-track-face    ((,c :inherit gnus-summary-normal-unread)))
         (emms-playlist-selected-face ((,c :inherit alect-selected-item)))
         (emms-stream-name-face       ((,c :foreground ,(gc 'blue+1))))
         (emms-stream-url-face        ((,c :inherit default)))

         ;; erc
         (erc-header-line          ((,c :inherit header-line)))
         (erc-bold-face            ((,c :weight bold)))
         (erc-underline-face       ((,c :underline t)))
         (erc-current-nick-face    ((,c :foreground ,(gc 'blue) :weight bold)))
         (erc-dangerous-host-face  ((,c :inherit font-lock-warning-face)))
         (erc-default-face         ((,c :foreground ,(gc 'fg+1))))
         (erc-direct-msg-face      ((,c :inherit erc-default-face
                                        :foreground ,(gc 'red-2))))
         (erc-action-face          ((,c :inherit erc-bold-face)))
         (erc-error-face           ((,c :inherit error)))
         (erc-fool-face            ((,c :foreground ,(gc 'blue-2))))
         (erc-highlight-face       ((,c :inherit hover-highlight)))
         (erc-input-face           ((,c :background ,(gc 'cyan-bg+1))))
         (erc-keyword-face         ((,c :foreground ,(gc 'green+1))))
         (erc-nick-default-face    ((,c :foreground ,(gc 'blue+1))))
         (erc-my-nick-face         ((,c :foreground ,(gc 'red))))
         (erc-nick-msg-face        ((,c :foreground ,(gc 'cyan+2))))
         (erc-notice-face          ((,c :foreground ,(gc 'green))))
         (erc-pal-face             ((,c :foreground ,(gc 'magenta+2))))
         (erc-prompt-face          ((,c :inherit alect-prompt)))
         (erc-timestamp-face       ((,c :inherit alect-time)))

         ;; epa
         (epa-mark              ((,c :foreground ,(gc 'blue+1))))
         (epa-string            ((,c :foreground ,(gc 'cyan+2))))
         (epa-validity-disabled ((,c :foreground ,(gc 'fg-2))))
         (epa-validity-high     ((,c :foreground ,(gc 'green-1))))
         (epa-validity-medium   ((,c :foreground ,(gc 'yellow-1))))
         (epa-validity-low      ((,c :foreground ,(gc 'red-1))))

         ;; ert
         (ert-test-result-expected    ((,c :foreground ,(gc 'green)
                                           :background ,(gc 'bg-2)
                                           :box (:line-width 1
                                                 :style nil))))
         (ert-test-result-unexpected  ((,c :inherit ert-test-result-expected
                                           :foreground ,(gc 'red))))

         ;; eshell
         (eshell-prompt         ((,c :inherit alect-prompt)))
         (eshell-ls-archive     ((,c :foreground ,(gc 'green))))
         (eshell-ls-backup      ((,c :inherit dired-ignored)))
         (eshell-ls-clutter     ((,c :inherit font-lock-comment-face)))
         (eshell-ls-directory   ((,c :inherit dired-directory)))
         (eshell-ls-executable  ((,c :foreground ,(gc 'yellow))))
         (eshell-ls-unreadable  ((,c :foreground ,(gc 'red-2))))
         (eshell-ls-readonly    ((,c :foreground ,(gc 'fg-2))))
         (eshell-ls-missing     ((,c :inherit dired-warning)))
         (eshell-ls-product     ((,c :inherit font-lock-doc-face)))
         (eshell-ls-special     ((,c :foreground ,(gc 'fg+1) :weight bold)))
         (eshell-ls-symlink     ((,c :inherit dired-symlink)))

         ;; ffap
         (ffap ((,c :foreground ,(gc 'fg+1)
                    :background ,(gc 'blue-bg))))

         ;; flycheck
         (flycheck-error-face    ((,c :foreground ,(gc 'red-1)
                                      :weight bold
                                      :underline t)))
         (flycheck-warning-face  ((,c :foreground ,(gc 'fg-2)
                                      :weight bold
                                      :underline t)))

         ;; flymake
         (flymake-errline        ((,c :foreground ,(gc 'red-1)
                                      :weight bold
                                      :underline t)))
         (flymake-warnline       ((,c :foreground ,(gc 'fg-2)
                                      :weight bold
                                      :underline t)))

         ;; flyspell
         (flyspell-duplicate     ((,c :foreground ,(gc 'gray)
                                      :background ,(gc 'bg))
                                      :weight bold))
         (flyspell-incorrect     ((,c :inherit flyspell-duplicate
                                      :foreground ,(gc 'red+1))))

         ;; font lock
         (font-lock-builtin-face           ((,c :foreground ,(gc 'magenta-1))))
         (font-lock-comment-face           ((,c :foreground ,(gc 'green+1))))
         (font-lock-comment-delimiter-face ((,c :inherit font-lock-comment-face)))
         (font-lock-constant-face          ((,c :foreground ,(gc 'cyan-1))))
         (font-lock-doc-face               ((,c :foreground ,(gc 'fg-1)
                                                :slant italic)))
         (font-lock-function-name-face     ((,c :foreground ,(gc 'blue-1))))
         (font-lock-keyword-face           ((,c :foreground ,(gc 'blue+1)
                                                :weight bold)))
         (font-lock-negation-char-face     ((,c :foreground ,(gc 'blue))))
         (font-lock-preprocessor-face      ((,c :foreground ,(gc 'green-1))))
         (font-lock-string-face            ((,c :foreground ,(gc 'red-2))))
         (font-lock-type-face              ((,c :foreground ,(gc 'magenta+1))))
         (font-lock-variable-name-face     ((,c :foreground ,(gc 'yellow+2))))
         (font-lock-warning-face           ((,c :foreground ,(gc 'red)
                                                :weight bold)))
         (font-lock-regexp-grouping-backslash ((,c :foreground ,(gc 'red+2))))
         (font-lock-regexp-grouping-construct ((,c :foreground ,(gc 'yellow-1))))

         ;; gdb
         (breakpoint-enabled  ((,c :foreground ,(gc 'red))))
         (breakpoint-disabled ((,c :foreground ,(gc 'gray))))

         ;; geiser
         (geiser-font-lock-doc-title          ((,c :inherit bold)))
         (geiser-font-lock-doc-link           ((,c :inherit link)))
         (geiser-font-lock-doc-button         ((,c :inherit button)))
         (geiser-font-lock-xref-header        ((,c :inherit bold)))
         (geiser-font-lock-xref-link          ((,c :inherit link)))
         (geiser-font-lock-error-link         ((,c :inherit (error link))))
         (geiser-font-lock-autodoc-identifier ((,c :inherit font-lock-function-name-face)))
         (geiser-font-lock-autodoc-current-arg ((,c :inherit font-lock-variable-name-face)))

         ;; gnus
         (gnus-group-news-1-empty      ((,c :inherit alect-color-level-1)))
         (gnus-group-news-2-empty      ((,c :inherit alect-color-level-2)))
         (gnus-group-news-3-empty      ((,c :inherit alect-color-level-3)))
         (gnus-group-news-4-empty      ((,c :inherit alect-color-level-4)))
         (gnus-group-news-5-empty      ((,c :inherit alect-color-level-5)))
         (gnus-group-news-6-empty      ((,c :inherit alect-color-level-6)))
         (gnus-group-news-low-empty    ((,c :inherit alect-color-level-7)))
         (gnus-group-news-1            ((,c :inherit gnus-group-news-1-empty
                                            :weight bold)))
         (gnus-group-news-2            ((,c :inherit gnus-group-news-2-empty
                                            :weight bold)))
         (gnus-group-news-3            ((,c :inherit gnus-group-news-3-empty
                                            :weight bold)))
         (gnus-group-news-4            ((,c :inherit gnus-group-news-4-empty
                                            :weight bold)))
         (gnus-group-news-5            ((,c :inherit gnus-group-news-5-empty
                                            :weight bold)))
         (gnus-group-news-6            ((,c :inherit gnus-group-news-6-empty
                                            :weight bold)))
         (gnus-group-news-low          ((,c :inherit gnus-group-news-low-empty
                                            :weight bold)))
         (gnus-group-mail-1-empty      ((,c :inherit gnus-group-news-1-empty
                                            :slant italic)))
         (gnus-group-mail-2-empty      ((,c :inherit gnus-group-news-2-empty
                                            :slant italic)))
         (gnus-group-mail-3-empty      ((,c :inherit gnus-group-news-3-empty
                                            :slant italic)))
         (gnus-group-mail-low-empty    ((,c :inherit gnus-group-news-low-empty
                                            :slant italic)))
         (gnus-group-mail-1            ((,c :inherit gnus-group-news-1
                                            :slant italic)))
         (gnus-group-mail-2            ((,c :inherit gnus-group-news-2
                                            :slant italic)))
         (gnus-group-mail-3            ((,c :inherit gnus-group-news-3
                                            :slant italic)))
         (gnus-group-mail-low          ((,c :inherit gnus-group-news-low
                                            :slant italic)))

         (gnus-header-content          ((,c :inherit message-header-other)))
         (gnus-header-from             ((,c :foreground ,(gc 'red-1))))
         (gnus-header-name             ((,c :inherit message-header-name)))
         (gnus-header-newsgroups       ((,c :inherit message-header-newsgroups)))
         (gnus-header-subject          ((,c :inherit message-header-subject)))
         (gnus-summary-cancelled       ((,c :background ,(gc 'fg-1)
                                            :foreground ,(gc 'bg-2))))
         (gnus-summary-low-ancient     ((,c :foreground ,(gc 'blue-2))))
         (gnus-summary-low-read        ((,c :foreground ,(gc 'green-2))))
         (gnus-summary-low-ticked      ((,c :foreground ,(gc 'red-2))))
         (gnus-summary-low-unread      ((,c :foreground ,(gc 'fg-1))))
         (gnus-summary-normal-ancient  ((,c :foreground ,(gc 'blue))))
         (gnus-summary-normal-read     ((,c :foreground ,(gc 'green))))
         (gnus-summary-normal-ticked   ((,c :foreground ,(gc 'red))))
         (gnus-summary-normal-unread   ((,c :foreground ,(gc 'fg+1))))
         (gnus-summary-high-ancient    ((,c :inherit gnus-summary-normal-ancient
                                            :weight bold)))
         (gnus-summary-high-read       ((,c :inherit gnus-summary-normal-read
                                            :weight bold)))
         (gnus-summary-high-ticked     ((,c :inherit gnus-summary-normal-ticked
                                            :weight bold)))
         (gnus-summary-high-unread     ((,c :inherit gnus-summary-normal-unread
                                            :weight bold)))
         (gnus-summary-selected        ((,c :inherit alect-selected-item)))
         (gnus-cite-1                  ((,c :inherit alect-color-level-1)))
         (gnus-cite-2                  ((,c :inherit alect-color-level-2)))
         (gnus-cite-3                  ((,c :inherit alect-color-level-3)))
         (gnus-cite-4                  ((,c :inherit alect-color-level-4)))
         (gnus-cite-5                  ((,c :inherit alect-color-level-5)))
         (gnus-cite-6                  ((,c :inherit alect-color-level-6)))
         (gnus-cite-7                  ((,c :inherit alect-color-level-7)))
         (gnus-cite-8                  ((,c :inherit alect-color-level-8)))
         (gnus-cite-9                  ((,c :inherit alect-color-level-9)))
         (gnus-cite-10                 ((,c :inherit alect-color-level-10)))
         (gnus-cite-11                 ((,c :inherit alect-color-level-11)))
         (gnus-signature               ((,c :foreground ,(gc 'cyan+1))))
         (gnus-x                       ((,c :background ,(gc 'fg+1)
                                            :foreground ,(gc 'bg-1))))
         (gnus-server-agent            ((,c :foreground ,(gc 'magenta+2))))
         (gnus-server-closed           ((,c :foreground ,(gc 'blue))))
         (gnus-server-denied           ((,c :inherit font-lock-warning-face)))
         (gnus-server-offline          ((,c :foreground ,(gc 'yellow-1))))
         (gnus-server-opened           ((,c :foreground ,(gc 'green))))

         ;; google-translate
         (google-translate-text-face             ((,c :foreground ,(gc 'blue+2))))
         (google-translate-translation-face      ((,c :foreground ,(gc 'green+1))))
         (google-translate-phonetic-face         ((,c :foreground ,(gc 'gray))))
         (google-translate-suggestion-label-face ((,c :foreground ,(gc 'red))))
         (google-translate-suggestion-face       ((,c :inherit button)))
         (google-translate-listen-button-face    ((,c :inherit alect-button)))

         ;; guix
         (guix-true                             ((,c :foreground ,(gc 'green-1)
                                                     :weight bold)))
         (guix-operation-option-key             ((,c :inherit alect-key)))
         (guix-package-info-name                ((,c :inherit alect-title)))
         (guix-package-info-version             ((,c :foreground ,(gc 'blue-2))))
         (guix-package-info-name-button         ((,c :foreground ,(gc 'fg)
                                                     :weight bold)))
         (guix-package-info-synopsis            ((,c :inherit font-lock-doc-face)))
         (guix-package-info-license             ((,c :foreground ,(gc 'red-1))))
         (guix-package-info-inputs              ((,c :inherit guix-package-info-name-button)))
         (guix-package-info-native-inputs       ((,c :inherit guix-package-info-name-button
                                                     :foreground ,(gc 'fg-1))))
         (guix-package-info-propagated-inputs   ((,c :inherit guix-package-info-name-button)))
         (guix-package-info-future              ((,c :foreground ,(gc 'blue-1)
                                                     :inherit guix-package-info-installed-outputs)))
         (guix-package-info-unknown             ((,c :foreground ,(gc 'fg+2)
                                                     :background ,(gc 'red-bg)
                                                     :inherit guix-package-info-installed-outputs)))
         (guix-devel-gexp-symbol                ((,c :foreground ,(gc 'yellow+2))))

         ;; TODO Remove ("covered" by 'guix-true' face since Emacs-Guix 0.3.3).
         (guix-package-info-installed-outputs   ((,c :foreground ,(gc 'green-1)
                                                     :weight bold)))

         ;; helm
         (helm-header-line-left-margin ((,c :inherit alect-prompt)))
         (helm-header             ((,c :inherit header-line)))
         (helm-source-header      ((,c :inherit alect-title)))
         (helm-separator          ((,c :foreground ,(gc 'blue+2))))
         (helm-M-x-key            ((,c :foreground ,(gc 'yellow))))
         (helm-selection          ((,c :background ,(gc 'bg))))
         (helm-selection-line     ((,c :background ,(gc 'bg))))
         (helm-visible-mark       ((,c :foreground ,(gc 'yellow-1))))
         (helm-candidate-number   ((,c :background ,(gc 'bg-2)
                                       :foreground ,(gc 'green-2))))
         (helm-ff-file            ((,c)))
         (helm-ff-executable      ((,c :foreground ,(gc 'green-1))))
         (helm-ff-directory       ((,c :inherit dired-directory)))
         (helm-ff-symlink         ((,c :inherit dired-symlink)))
         (helm-ff-invalid-symlink ((,c :inherit dired-warning)))
         (helm-ff-prefix          ((,c :background ,(gc 'bg)
                                       :foreground ,(gc 'red+1))))
         (helm-buffer-directory   ((,c :inherit helm-ff-directory)))
         (helm-buffer-saved-out   ((,c :background ,(gc 'bg-2)
                                       :foreground ,(gc 'red))))
         (helm-buffer-not-saved   ((,c :foreground ,(gc 'red-2))))
         (helm-buffer-process     ((,c :foreground ,(gc 'green+1))))
         (helm-buffer-size        ((,c :foreground ,(gc 'cyan))))
         (helm-grep-file          ((,c :inherit compilation-info)))

         ;; help
         (help-argument-name ((,c :inherit font-lock-variable-name-face)))

         ;; hl-line-mode
         (hl-line ((,c :background ,(gc 'bg))))

         ;; hl-todo
         (hl-todo ((,c :foreground ,(gc 'red) :weight bold)))

         ;; hydra
         (hydra-face-amaranth ((,c :foreground ,(gc 'yellow)    :weight bold)))
         (hydra-face-red      ((,c :foreground ,(gc 'red-1)     :weight bold)))
         (hydra-face-pink     ((,c :foreground ,(gc 'magenta-2) :weight bold)))
         (hydra-face-blue     ((,c :foreground ,(gc 'blue-1)    :weight bold)))
         (hydra-face-teal     ((,c :foreground ,(gc 'cyan+1)    :weight bold)))

         ;; indent-guide
         (indent-guide-face ((,c :foreground ,(gc 'yellow+1))))

         ;; info
         (info-title-1     ((,c :inherit alect-color-level-1
                                :height 1.5 :weight bold)))
         (info-title-2     ((,c :inherit alect-color-level-2
                                :height 1.4 :weight bold)))
         (info-title-3     ((,c :inherit alect-color-level-3
                                :height 1.3 :weight bold)))
         (info-title-4     ((,c :inherit alect-color-level-4
                                :height 1.2 :weight bold)))
         (info-menu-header ((,c :inherit alect-color-level-5
                                :height 1.1 :weight bold)))
         (info-node        ((,c :foreground ,(gc 'red+1))))
         (info-menu-star   ((,c :foreground ,(gc 'red))))
         (Info-quoted      ((,c :foreground ,(gc 'fg+2) :weight bold)))

         ;; ido-mode
         (ido-first-match ((,c :weight bold)))
         (ido-only-match  ((,c :inherit ido-first-match
                               :foreground ,(gc 'blue))))
         (ido-subdir      ((,c :inherit dired-directory)))
         (ido-virtual     ((,c :foreground ,(gc 'red-2))))
         ;; I have no idea where this face is used.
         (ido-indicator   ((,c :background ,(gc 'red-bg)
                               :foreground ,(gc 'fg+1))))

         ;; isearch
         (isearch-fail         ((,c :background ,(gc 'red-bg))))
         (isearch              ((,c :foreground ,(gc 'fg+1)
                                    :background ,(gc 'blue-bg-1))))
         (lazy-highlight       ((,c :foreground ,(gc 'fg+1)
                                    :background ,(gc 'bg+2))))

         ;; ivy
         (ivy-cursor                  ((,c :background ,(gc 'cursor)
                                           :foreground ,(gc 'bg-2)
                                           :box (:line-width 1
                                                 :color ,(gc 'bg-2)
                                                 :style nil))))
         (ivy-current-match           ((,c :foreground ,(gc 'fg+2)
                                           :weight bold)))
         (ivy-confirm-face            ((,c :inherit minibuffer-prompt
                                           :foreground ,(gc 'green))))
         (ivy-match-required-face     ((,c :inherit minibuffer-prompt
                                           :foreground ,(gc 'red))))
         (ivy-virtual                 ((,c :foreground ,(gc 'red-2))))
         (ivy-remote                  ((,c :foreground ,(gc 'blue+1))))
         ;; Ideally `ivy-minibuffer-match-face-2' should have some
         ;; background, but by default, ivy mixes colors (in
         ;; `ivy--add-face') instead of applying only one face.  This
         ;; may look really ugly, so just disable all minibuffer match
         ;; faces.
         (ivy-minibuffer-match-face-1 ((,c nil)))
         (ivy-minibuffer-match-face-2 ((,c nil)))
         (ivy-minibuffer-match-face-3 ((,c :inherit ivy-minibuffer-match-face-2)))
         (ivy-minibuffer-match-face-4 ((,c :inherit ivy-minibuffer-match-face-2)))

         ;; linum-mode
         (linum ((,c :foreground ,(gc 'fg-2))))

         ;; magit and related git modes
         (magit-dimmed                      ((,c :inherit shadow)))
         (magit-section-heading             ((,c :inherit alect-title)))
         (magit-section-highlight           ((,c :background ,(gc 'bg))))
         (magit-section-heading-selection   ((,c :foreground ,(gc 'blue-2))))
         (magit-head                        ((,c :background ,(gc 'bg-2)
                                                 :foreground ,(gc 'fg+2)
                                                 :box (:line-width 2
                                                       :color ,(gc 'red)))))
         (magit-refname                     ((,c :box (:line-width 1
                                                       :color ,(gc 'fg+1)))))
         (magit-refname-wip                 ((,c :inherit magit-refname
                                                 :foreground ,(gc 'red-1))))
         (magit-refname-stash               ((,c :inherit magit-refname
                                                 :foreground ,(gc 'magenta-1))))
         (magit-branch-local                ((,c :inherit magit-refname
                                                 :foreground ,(gc 'blue))))
         (magit-branch-current              ((,c :inherit magit-branch-local
                                                 :box (:line-width 2
                                                       :color ,(gc 'red)))))
         (magit-branch-remote               ((,c :inherit magit-refname
                                                 :foreground ,(gc 'green+1))))
         (magit-tag                         ((,c :foreground ,(gc 'yellow-1)
                                                 :weight bold)))
         (magit-hash                        ((,c :foreground ,(gc 'yellow+2))))
         (magit-log-date                    ((,c :inherit alect-time)))
         (magit-log-author                  ((,c :inherit alect-author)))
         (magit-reflog-checkout             ((,c :inherit magit-branch-local)))
         (magit-reflog-remote               ((,c :inherit magit-branch-remote)))
         (magit-reflog-commit               ((,c :foreground ,(gc 'green-1))))
         (magit-reflog-amend                ((,c :foreground ,(gc 'magenta))))
         (magit-reflog-merge                ((,c :foreground ,(gc 'blue-2)
                                                 :weight bold)))
         (magit-reflog-reset                ((,c :foreground ,(gc 'red)
                                                 :weight bold)))
         (magit-reflog-rebase               ((,c :foreground ,(gc 'yellow))))
         (magit-reflog-cherry-pick          ((,c :foreground ,(gc 'cyan))))
         (magit-reflog-other                ((,c :inherit magit-branch-local
                                                 :foreground ,(gc 'fg+1))))
         (magit-bisect-bad                  ((,c :foreground ,(gc 'red+1)
                                                 :box (:line-width -1))))
         (magit-bisect-skip                 ((,c :inherit magit-bisect-bad
                                                 :foreground ,(gc 'yellow+1))))
         (magit-bisect-good                 ((,c :inherit magit-bisect-bad
                                                 :foreground ,(gc 'green+1))))
         (magit-cherry-unmatched            ((,c :foreground ,(gc 'red+2))))
         (magit-cherry-equivalent           ((,c :foreground ,(gc 'cyan+2))))
         (magit-process-ok                  ((,c :inherit magit-section-title
                                                 :foreground ,(gc 'green))))
         (magit-process-ng                  ((,c :inherit magit-section-title
                                                 :foreground ,(gc 'red))))
         (magit-blame-heading               ((,c :background ,(gc 'bg-2))))
         (magit-blame-summary               ((,c :inherit magit-blame-heading)))
         (magit-blame-hash                  ((,c :inherit (magit-hash
                                                           magit-blame-heading))))
         (magit-blame-name                  ((,c :inherit (magit-log-author
                                                           magit-blame-heading))))
         (magit-blame-date                  ((,c :inherit (magit-log-date
                                                           magit-blame-heading))))
         (magit-diff-file-heading           ((,c :inherit diff-file-header)))
         (magit-diff-file-heading-selection ((,c :inherit (magit-section-heading-selection
                                                           magit-diff-file-heading-highlight))))
         (magit-diff-lines-heading          ((,c :foreground ,(gc 'fg+1)
                                                 :background ,(gc 'blue-bg-1))))
         (magit-diff-lines-boundary         ((,c :background ,(gc 'blue+1))))
         (magit-diff-conflict-heading       ((,c :inherit magit-diff-hunk-heading
                                                 :foreground ,(gc 'red+2))))
         (magit-diff-hunk-heading           ((,c :inherit diff-hunk-header)))
         (magit-diff-hunk-heading-highlight ((,c :inherit magit-diff-hunk-heading
                                                 :background ,(gc 'bg-2))))
         (magit-diff-hunk-heading-selection ((,c :inherit (magit-section-heading-selection
                                                           magit-diff-hunk-heading-highlight))))
         (magit-diff-context                ((,c :foreground ,(gc 'fg-2))))
         (magit-diff-context-highlight      ((,c :inherit magit-section-highlight)))
         (magit-diff-added                  ((,c :inherit diff-added)))
         (magit-diff-added-highlight        ((,c :inherit (magit-diff-added
                                                           magit-diff-context-highlight))))
         (magit-diff-removed                ((,c :inherit diff-removed)))
         (magit-diff-removed-highlight      ((,c :inherit (magit-diff-removed
                                                           magit-diff-context-highlight))))
         (magit-diffstat-added              ((,c :inherit magit-diff-added)))
         (magit-diffstat-removed            ((,c :inherit magit-diff-removed)))
         (magit-popup-heading               ((,c :inherit alect-title)))
         (magit-popup-key                   ((,c :inherit alect-key)))
         (magit-popup-argument              ((,c :foreground ,(gc 'blue-2)
                                                 :weight bold)))
         (magit-popup-disabled-argument     ((,c :foreground ,(gc 'gray))))
         (magit-sequence-pick               ((,c :underline (:color ,(gc 'fg-2)))))
         (magit-sequence-part               ((,c :inherit magit-sequence-pick
                                                 :foreground ,(gc 'yellow+1))))
         (magit-sequence-head               ((,c :inherit magit-sequence-pick
                                                 :foreground ,(gc 'blue-1))))
         (magit-sequence-drop               ((,c :inherit magit-sequence-pick
                                                 :foreground ,(gc 'red+1))))
         (magit-sequence-onto               ((,c :inherit magit-sequence-pick
                                                 :foreground ,(gc 'magenta+1))))
         (magit-sequence-stop               ((,c :inherit magit-sequence-pick
                                                 :foreground ,(gc 'cyan+1))))
         (magit-sequence-done               ((,c :inherit magit-sequence-pick
                                                 :foreground ,(gc 'blue+2))))
         (magit-signature-good              ((,c :foreground ,(gc 'green-1))))
         (magit-signature-bad               ((,c :foreground ,(gc 'red-1))))
         (magit-signature-untrusted         ((,c :foreground ,(gc 'cyan-1))))
         (magit-signature-expired           ((,c :foreground ,(gc 'yellow))))
         (magit-signature-revoked           ((,c :foreground ,(gc 'magenta))))
         (magit-signature-error             ((,c :inherit error)))
         (git-rebase-hash                   ((,c :inherit magit-hash)))
         (git-rebase-comment-heading        ((,c :inherit alect-title)))
         (git-commit-summary                ((,c :weight bold)))
         (git-commit-comment-branch         ((,c :foreground ,(gc 'blue))))
         (git-commit-comment-file           ((,c :inherit default)))
         (git-commit-comment-heading        ((,c :inherit alect-title)))
         (git-commit-comment-action         ((,c :foreground ,(gc 'yellow+1))))

         ;; makefile
         (makefile-shell       ((,c :weight bold)))
         (makefile-space       ((,c :inherit trailing-whitespace)))
         (makefile-makepp-perl ((,c :background ,(gc 'bg+1))))

         ;; man
         (Man-overstrike ((,c :foreground ,(gc 'cyan+1) :weight bold)))

         ;; markdown mode
         (markdown-header-face           ((,c :inherit alect-title-1)))
         (markdown-header-face-1         ((,c :inherit alect-title-1)))
         (markdown-header-face-2         ((,c :inherit alect-title-2)))
         (markdown-header-face-3         ((,c :inherit alect-title-3)))
         (markdown-header-face-4         ((,c :inherit alect-title-4)))
         (markdown-header-face-5         ((,c :inherit alect-title-5)))
         (markdown-header-face-6         ((,c :inherit alect-title-6)))
         (markdown-header-rule-face      ((,c :inherit markdown-header-face
                                              :foreground ,(gc 'fg))))
         (markdown-header-delimiter-face ((,c :inherit markdown-header-face
                                              :foreground ,(gc 'fg+2))))
         (markdown-footnote-face         ((,c :foreground ,(gc 'blue+2))))
         (markdown-inline-code-face      ((,c :foreground ,(gc 'cyan+1))))
         (markdown-markup-face           ((,c :foreground ,(gc 'fg-2))))
         (markdown-comment-face          ((,c :inherit font-lock-comment-face)))
         (markdown-blockquote-face       ((,c :inherit font-lock-doc-face)))
         (markdown-bold-face             ((,c :inherit bold)))
         (markdown-italic-face           ((,c :inherit italic)))
         (markdown-missing-link-face     ((,c :inherit font-lock-warning-face)))
         (markdown-link-face             ((,c :inherit link)))
         (markdown-link-title-face       ((,c :inherit font-lock-string-face)))
         (markdown-url-face              ((,c :foreground ,(gc 'green+2))))
         (markdown-language-keyword-face ((,c :foreground ,(gc 'magenta+2))))
         (markdown-line-break-face       ((,c :inherit underline)))
         (markdown-list-face             ((,c :foreground ,(gc 'fg+2)
                                              :weight bold)))
         (markdown-metadata-key-face     ((,c :foreground ,(gc 'blue-2))))
         (markdown-metadata-value-face   ((,c :foreground ,(gc 'red-1))))
         (markdown-pre-face              ((,c :foreground ,(gc 'yellow+2))))
         (markdown-reference-face        ((,c :inherit link)))

         ;; merlin
         (merlin-locked-face              ((,c :background ,(gc 'bg-2))))
         (merlin-type-face                ((,c :background ,(gc 'bg+1))))
         (merlin-compilation-warning-face ((,c :inherit warning)))
         (merlin-compilation-error-face   ((,c :inherit error)))

         ;; message-mode
         (message-cited-text        ((,c :inherit font-lock-comment-face)))
         (message-separator         ((,c :inherit font-lock-comment-face)))
         (message-header-name       ((,c :inherit alect-field-title)))
         (message-header-other      ((,c :foreground ,(gc 'fg))))
         (message-header-to         ((,c :foreground ,(gc 'blue))))
         (message-header-cc         ((,c :foreground ,(gc 'blue-1))))
         (message-header-newsgroups ((,c :foreground ,(gc 'yellow+2))))
         (message-header-subject    ((,c :inherit alect-title)))
         (message-header-xheader    ((,c :foreground ,(gc 'green-1))))
         (message-mml               ((,c :foreground ,(gc 'cyan-2)
                                         :weight bold)))

         ;; mm (used in mails)
         (mm-uu-extract ((,c :background ,(gc 'bg))))

         ;; mode-line
         (mode-line-buffer-id  ((,c :foreground ,(gc 'blue-1)
                                    :weight bold)))
         (mode-line            ((,c :foreground ,(gc 'fg+1)
                                    :background ,(gc 'bg-2)
                                    :box (:line-width 2
                                          :style released-button))))
         (mode-line-inactive   ((,c :foreground ,(gc 'fg-2)
                                    :background ,(gc 'bg-1)
                                    :box (:line-width 2
                                          :color ,(gc 'bg-2)
                                          :style nil))))
         (mode-line-highlight  ((,c :inherit highlight
                                    :box (:line-width 1
                                          :style released-button))))

         ;; mu4e
         (mu4e-title-face                ((,c :inherit alect-title)))
         (mu4e-cited-1-face              ((,c :inherit alect-color-level-1)))
         (mu4e-cited-2-face              ((,c :inherit alect-color-level-2)))
         (mu4e-cited-3-face              ((,c :inherit alect-color-level-3)))
         (mu4e-cited-4-face              ((,c :inherit alect-color-level-4)))
         (mu4e-cited-5-face              ((,c :inherit alect-color-level-5)))
         (mu4e-cited-6-face              ((,c :inherit alect-color-level-6)))
         (mu4e-cited-7-face              ((,c :inherit alect-color-level-7)))
         (mu4e-compose-separator-face    ((,c :inherit message-separator)))
         (mu4e-contact-face              ((,c :inherit message-header-to)))
         (mu4e-draft-face                ((,c :foreground ,(gc 'red-2))))
         (mu4e-flagged-face              ((,c :foreground ,(gc 'yellow+2))))
         (mu4e-footer-face               ((,c :foreground ,(gc 'cyan+1))))
         (mu4e-forwarded-face            ((,c :foreground ,(gc 'magenta+2))))
         (mu4e-header-highlight-face     ((,c :inherit alect-selected-item)))
         (mu4e-header-key-face           ((,c :inherit message-header-name)))
         (mu4e-header-value-face         ((,c :inherit message-header-other)))
         (mu4e-special-header-value-face ((,c :foreground ,(gc 'green))))
         (mu4e-header-marks-face         ((,c :foreground ,(gc 'red))))
         (mu4e-highlight-face            ((,c :inherit alect-key)))
         (mu4e-modeline-face             ((,c :foreground ,(gc 'green+1))))
         (mu4e-moved-face                ((,c :foreground ,(gc 'blue+2))))
         (mu4e-region-code               ((,c :background ,(gc 'bg))))
         (mu4e-replied-face              ((,c :foreground ,(gc 'green-1))))
         (mu4e-system-face               ((,c :foreground ,(gc 'cyan))))
         (mu4e-trashed-face              ((,c :foreground ,(gc 'fg-2))))
         (mu4e-unread-face               ((,c :foreground ,(gc 'blue-1))))
         (mu4e-attach-number-face        ((,c :foreground ,(gc 'fg)
                                              :background ,(gc 'bg-2))))
         (mu4e-url-number-face           ((,c :inherit mu4e-attach-number-face
                                              :foreground ,(gc 'yellow+1))))

         ;; nethack-el
         (nethack-dark-gray-face          ((,c :foreground ,(gc 'fg-1))))
         (nethack-message-highlight-face  ((,c :background ,(gc 'bg)
                                               :foreground ,(gc 'red+2))))
         (nethack-yellow-face             ((,c :foreground ,(gc 'yellow))))

         ;; ocaml
         (ocaml-help-face ((,c :inherit match)))
         (ocaml-link-face ((,c :inherit link)))

         ;; org-mode
         (org-agenda-date           ((,c :inherit alect-time)))
         (org-agenda-date-today     ((,c :inherit alect-selected-item
                                         :foreground ,(gc 'cyan-2))))
         (org-agenda-structure      ((,c :inherit alect-title)))
         (org-archived              ((,c :foreground ,(gc 'fg+1) :weight bold)))
         (org-block                 ((,c :background ,(gc 'bg))))
         (org-block-background      ((,c :background ,(gc 'bg))))
         (org-block-begin-line      ((,c :inherit org-meta-line
                                         :background ,(gc 'bg+1))))
         (org-block-end-line        ((,c :inherit org-block-begin-line)))
         (org-checkbox              ((,c :background ,(gc 'bg-2)
                                         :foreground ,(gc 'fg)
                                         :box (:line-width 1
                                               :style pressed-button))))
         (org-date                  ((,c :inherit alect-time)))
         (org-date-selected         ((,c :inherit alect-selected-item)))
         (org-deadline-announce     ((,c :foreground ,(gc 'red-1))))
         (org-done                  ((,c :foreground ,(gc 'cyan-1)
                                         :weight bold)))
         (org-document-title        ((,c :inherit alect-title)))
         (org-document-info         ((,c :foreground ,(gc 'red-1))))
         (org-document-info-keyword ((,c :foreground ,(gc 'yellow+1))))
         (org-footnote              ((,c :foreground ,(gc 'cyan)
                                         :underline t)))
         (org-formula               ((,c :foreground ,(gc 'yellow-2))))
         (org-headline-done         ((,c :foreground ,(gc 'cyan-1))))
         (org-hide                  ((,c :foreground ,(gc 'bg-2))))
         (org-level-1               ((,c :inherit alect-title-1)))
         (org-level-2               ((,c :inherit alect-title-2)))
         (org-level-3               ((,c :inherit alect-title-3)))
         (org-level-4               ((,c :inherit alect-title-4)))
         (org-level-5               ((,c :inherit alect-title-5)))
         (org-level-6               ((,c :inherit alect-title-6)))
         (org-level-7               ((,c :inherit alect-title-7)))
         (org-level-8               ((,c :inherit alect-title-8)))
         (org-link                  ((,c :inherit link)))
         (org-mode-line-clock       ((,c :foreground ,(gc 'green+1))))
         (org-mode-line-clock-overrun ((,c :foreground ,(gc 'red)
                                           :weight bold)))
         (org-scheduled             ((,c :foreground ,(gc 'green-2))))
         (org-scheduled-previously  ((,c :foreground ,(gc 'red-2))))
         (org-scheduled-today       ((,c :foreground ,(gc 'blue+1))))
         (org-special-keyword       ((,c :inherit font-lock-doc-face)))
         (org-table                 ((,c :foreground ,(gc 'fg-1))))
         (org-tag                   ((,c :slant italic)))
         (org-time-grid             ((,c :foreground ,(gc 'fg-2))))
         (org-todo                  ((,c :foreground ,(gc 'red) :weight bold)))
         (org-upcoming-deadline     ((,c :inherit font-lock-keyword-face)))
         (org-verbatim              ((,c :foreground ,(gc 'fg-2))))
         (org-code                  ((,c :foreground ,(gc 'cyan+1))))
         (org-warning               ((,c :foreground ,(gc 'red)
                                         :weight bold :underline nil)))
         (org-column                ((,c :background ,(gc 'bg-2))))
         (org-column-title          ((,c :background ,(gc 'bg-2)
                                         :underline t :weight bold)))

         ;; outline
         (outline-1 ((,c :inherit alect-title-1)))
         (outline-2 ((,c :inherit alect-title-2)))
         (outline-3 ((,c :inherit alect-title-3)))
         (outline-4 ((,c :inherit alect-title-4)))
         (outline-5 ((,c :inherit alect-title-5)))
         (outline-6 ((,c :inherit alect-title-6)))
         (outline-7 ((,c :inherit alect-title-7)))
         (outline-8 ((,c :inherit alect-title-8)))

         ;; package
         (package-help-section-name ((,c :inherit alect-field-title)))
         (package-name              ((,c :foreground ,(gc 'blue-1))))
         (package-status-installed  ((,c :foreground ,(gc 'green+1))))
         (package-status-dependency ((,c :foreground ,(gc 'green-1))))
         (package-status-built-in   ((,c :foreground ,(gc 'blue+2))))
         (package-status-external   ((,c :foreground ,(gc 'blue+1))))
         (package-status-incompat   ((,c :foreground ,(gc 'magenta-1))))
         (package-status-avail-obso ((,c :foreground ,(gc 'magenta+1))))
         (package-status-disabled   ((,c :foreground ,(gc 'red))))
         (package-status-unsigned   ((,c :inherit warning)))

         ;; popup
         (popup-tip-face                    ((,c :foreground ,(gc 'gray+2)
                                                 :background ,(gc 'yellow-2))))
         (popup-scroll-bar-foreground-face  ((,c :background ,(gc 'fg-2))))
         (popup-scroll-bar-background-face  ((,c :background ,(gc 'bg-2))))
         (popup-isearch-match               ((,c :foreground ,(gc 'fg+1)
                                                 :background ,(gc 'bg-1) )))

         ;; powerline
         (powerline-active1   ((,c :inherit mode-line
                                   :background ,(gc 'cyan-bg))))
         (powerline-active2   ((,c :inherit mode-line)))
         (powerline-inactive1 ((,c :inherit mode-line-inactive
                                   :background ,(gc 'cyan-bg))))
         (powerline-inactive2 ((,c :inherit mode-line-inactive)))

         ;; rainbow-delimiters
         (rainbow-delimiters-depth-1-face   ((,c :foreground ,(gc 'fg+1))))
         (rainbow-delimiters-depth-2-face   ((,c :foreground ,(gc 'green+2))))
         (rainbow-delimiters-depth-3-face   ((,c :foreground ,(gc 'yellow-2))))
         (rainbow-delimiters-depth-4-face   ((,c :foreground ,(gc 'cyan))))
         (rainbow-delimiters-depth-5-face   ((,c :foreground ,(gc 'green-1))))
         (rainbow-delimiters-depth-6-face   ((,c :foreground ,(gc 'blue+1))))
         (rainbow-delimiters-depth-7-face   ((,c :foreground ,(gc 'yellow-1))))
         (rainbow-delimiters-depth-8-face   ((,c :foreground ,(gc 'green+1))))
         (rainbow-delimiters-depth-9-face   ((,c :foreground ,(gc 'cyan-2))))
         (rainbow-delimiters-depth-10-face  ((,c :foreground ,(gc 'fg-2))))
         (rainbow-delimiters-depth-11-face  ((,c :foreground ,(gc 'green))))
         (rainbow-delimiters-depth-12-face  ((,c :foreground ,(gc 'cyan+2))))

         ;; realgud
         (realgud-overlay-arrow1        ((,c :foreground ,(gc 'green-1))))
         (realgud-overlay-arrow2        ((,c :foreground ,(gc 'yellow+1))))
         (realgud-overlay-arrow3        ((,c :foreground ,(gc 'yellow))))
         (realgud-bp-enabled-face       ((,c :foreground ,(gc 'red))))
         (realgud-bp-disabled-face      ((,c :foreground ,(gc 'gray))))
         (realgud-bp-line-enabled-face  ((,c :box (:color ,(gc 'red) :style nil))))
         (realgud-bp-line-disabled-face ((,c :box (:color ,(gc 'gray) :style nil))))
         (realgud-file-name             ((,c :inherit alect-file)))
         (realgud-line-number           ((,c :foreground ,(gc 'yellow))))
         (realgud-backtrace-number      ((,c :foreground ,(gc 'fg+2)
                                             :weight bold)))

         ;; rcirc
         (rcirc-my-nick                   ((,c :foreground ,(gc 'blue))))
         (rcirc-other-nick                ((,c :foreground ,(gc 'fg-2))))
         (rcirc-bright-nick               ((,c :foreground ,(gc 'blue+1))))
         (rcirc-dim-nick                  ((,c :foreground ,(gc 'cyan-2))))
         (rcirc-server                    ((,c :foreground ,(gc 'green))))
         (rcirc-server-prefix             ((,c :foreground ,(gc 'green+1))))
         (rcirc-timestamp                 ((,c :inherit alect-time)))
         (rcirc-nick-in-message           ((,c :foreground ,(gc 'yellow))))
         (rcirc-nick-in-message-full-line ((,c :weight bold)))
         (rcirc-prompt                    ((,c :inherit alect-prompt)))
         (rcirc-track-nick                ((,c :inverse-video t)))
         (rcirc-track-keyword             ((,c :weight bold)))
         (rcirc-url                       ((,c :weight bold)))
         (rcirc-keyword                   ((,c :foreground ,(gc 'yellow)
                                               :weight bold)))

         ;; rst-mode
         (rst-level-1   ((,c :inherit alect-title-1)))
         (rst-level-2   ((,c :inherit alect-title-2)))
         (rst-level-3   ((,c :inherit alect-title-3)))
         (rst-level-4   ((,c :inherit alect-title-4)))
         (rst-level-5   ((,c :inherit alect-title-5)))
         (rst-level-6   ((,c :inherit alect-title-6)))
         (rst-reference ((,c :inherit link)))
         (rst-adornment ((,c :foreground ,(gc 'fg+2))))
         (rst-literal   ((,c :foreground ,(gc 'cyan+1))))

         ;; rubik
         (rubik-yellow ((,c :background ,(gc 'yellow-bg-1))))
         (rubik-white  ((,c :background ,(gc 'gray-2))))
         (rubik-red    ((,c :background ,(gc 'red-bg-1))))
         (rubik-orange ((,c :background ,(gc 'yellow))))
         (rubik-green  ((,c :background ,(gc 'green-bg-1))))
         (rubik-blue   ((,c :background ,(gc 'blue-bg-1))))

         ;; ruler-mode
         (ruler-mode-default        ((,c :inherit alect-tab-unselected
                                         :height 1.0)))
         (ruler-mode-pad            ((,c :inherit ruler-mode-default)))
         (ruler-mode-column-number  ((,c :inherit ruler-mode-default
                                         :foreground ,(gc 'fg+1))))
         (ruler-mode-current-column ((,c :inherit ruler-mode-default
                                         :background ,(gc 'cursor)
                                         :foreground ,(gc 'cursor))))
         (ruler-mode-comment-column ((,c :inherit (font-lock-comment-face
                                                   ruler-mode-default))))
         (ruler-mode-fill-column    ((,c :inherit ruler-mode-default
                                         :foreground ,(gc 'red)
                                         :weight bold)))
         (ruler-mode-goal-column    ((,c :inherit ruler-mode-default
                                         :foreground ,(gc 'red-1))))
         (ruler-mode-tab-stop       ((,c :inherit ruler-mode-default
                                         :foreground ,(gc 'blue-2))))
         (ruler-mode-fringes        ((,c :inherit ruler-mode-default
                                         :foreground ,(gc 'green))))
         (ruler-mode-margins        ((,c :inherit ruler-mode-default
                                         :foreground ,(gc 'magenta))))

         ;; sauron
         (sauron-timestamp-face  ((,c :inherit alect-time)))
         (sauron-message-face    ((,c :inherit font-lock-doc-face)))
         (sauron-origin-face     ((,c :foreground ,(gc 'blue+1))))
         (sauron-priority-face   ((,c :foreground ,(gc 'yellow+2))))
         ;; `sauron-header-face' is useless as the header is already
         ;; fontified by `header-line' face.
         (sauron-header-face     ((,c nil)))
         (sauron-highlight1-face ((,c :foreground ,(gc 'green))))
         (sauron-highlight2-face ((,c :foreground ,(gc 'red-1))))
         (sauron-highlight3-face ((,c :foreground ,(gc 'magenta))))

         ;; shell-script
         (sh-heredoc     ((,c :inherit font-lock-doc-face)))
         (sh-quoted-exec ((,c :foreground ,(gc 'cyan))))
         (sh-escaped-newline ((,c :foreground ,(gc 'fg-1)
                                  :weight bold)))

         ;; show-paren
         (show-paren-mismatch  ((,c :foreground ,(gc 'gray-2)
                                    :background ,(gc 'red))))
         (show-paren-match     ((,c :foreground ,(gc 'gray-2)
                                    :background ,(gc 'green+1))))

         ;; SLIME
         (slime-error-face                 ((,c :inherit error)))
         (slime-repl-input-face            ((,c :inherit comint-highlight-input)))
         (slime-repl-output-face           ((,c :foreground ,(gc 'green-1))))
         (slime-repl-inputed-output-face   ((,c :foreground ,(gc 'red))))
         (slime-repl-output-mouseover-face ((,c :inherit highlight)))
         (slime-repl-prompt-face           ((,c :inherit alect-prompt)))
         (slime-repl-result-face           ((,c :foreground ,(gc 'blue+2))))
         (sldb-section-face                ((,c :inherit alect-title)))
         (sldb-frame-label-face            ((,c :foreground ,(gc 'fg-2))))
         (sldb-restart-number-face         ((,c :inherit sldb-frame-label-face
                                                :weight bold)))
         (sldb-detailed-frame-line-face    ((,c :weight bold)))
         (sldb-restartable-frame-line-face ((,c :foreground ,(gc 'green+1))))
         (sldb-non-restartable-frame-line-face ((,c :foreground ,(gc 'red+1))))

         ;; speedbar
         (speedbar-file-face      ((,c)))
         (speedbar-directory-face ((,c :inherit dired-directory)))
         (speedbar-tag-face       ((,c :foreground ,(gc 'yellow+1))))
         (speedbar-selected-face  ((,c :foreground ,(gc 'red))))
         (speedbar-separator-face ((,c :inherit alect-title)))
         (speedbar-highlight-face ((,c :inherit highlight)))
         (speedbar-button-face    ((,c :foreground ,(gc 'green)
                                       :box (:line-width 1
                                             :style released-button))))

         ;; sunrise-commander
         (sr-active-path-face       ((,c :inherit dired-header)))
         (sr-passive-path-face      ((,c :inherit dired-header
                                         :foreground ,(gc 'fg-2))))
         (sr-directory-face         ((,c :inherit dired-directory)))
         (sr-marked-file-face       ((,c :inherit dired-marked)))
         (sr-marked-dir-face        ((,c :inherit sr-alt-marked-file-face
                                         :weight bold)))
         (sr-alt-marked-file-face   ((,c :inherit sr-marked-file-face
                                         :slant italic)))
         (sr-alt-marked-dir-face    ((,c :inherit sr-marked-dir-face
                                         :slant italic)))
         (sr-symlink-face           ((,c :inherit dired-symlink)))
         (sr-symlink-directory-face ((,c :inherit sr-symlink-face
                                         :weight bold)))
         (sr-broken-link-face       ((,c :inherit dired-warning)))
         (sr-highlight-path-face    ((,c :inherit highlight)))
         (sr-editing-path-face      ((,c :foreground ,(gc 'bg-1)
                                         :background ,(gc 'blue-1))))
         (sr-clex-hotchar-face      ((,c :foreground ,(gc 'red))))
         (sr-encrypted-face         ((,c :foreground ,(gc 'yellow))))
         (sr-compressed-face        ((,c :foreground ,(gc 'magenta-1))))
         (sr-packaged-face          ((,c :foreground ,(gc 'magenta+1))))
         (sr-log-face               ((,c :foreground ,(gc 'green-1))))
         (sr-xml-face               ((,c :foreground ,(gc 'green+2))))
         (sr-html-face              ((,c :foreground ,(gc 'cyan+2))))

         ;; syslog-mode
         (syslog-error ((,c :inherit error)))
         (syslog-warn  ((,c :inherit warning)))
         (syslog-info  ((,c :foreground ,(gc 'blue-2))))
         (syslog-debug ((,c :foreground ,(gc 'magenta-1))))
         (syslog-hour  ((,c :foreground ,(gc 'blue+1))))
         (syslog-su    ((,c :foreground ,(gc 'cyan))))
         (syslog-ip    ((,c :foreground ,(gc 'yellow+1) :underline t)))

         ;; tabbar
         (tabbar-default          ((,c :inherit alect-tab-default)))
         (tabbar-separator        ((,c :inherit tabbar-default)))
         (tabbar-highlight        ((,c :inherit alect-tab-mouse)))
         (tabbar-button           ((,c :inherit alect-button)))
         (tabbar-button-highlight ((,c :inherit tabbar-highlight)))
         (tabbar-unselected       ((,c :inherit alect-tab-unselected)))
         (tabbar-selected         ((,c :inherit alect-tab-selected)))
         (tabbar-modified         ((,c :inherit tabbar-unselected
                                       :foreground ,(gc 'red+2))))

         ;; table
         (table-cell ((,c :background ,(gc 'bg))))

         ;; term
         (term-color-black       ((,c :foreground ,(gc 'gray+2)
                                      :background ,(gc 'gray+2))))
         (term-color-white       ((,c :foreground ,(gc 'gray-2)
                                      :background ,(gc 'gray-2))))
         (term-color-red         ((,c :foreground ,(gc 'red+1)
                                      :background ,(gc 'red))))
         (term-color-green       ((,c :foreground ,(gc 'green+1)
                                      :background ,(gc 'green))))
         (term-color-yellow      ((,c :foreground ,(gc 'yellow-1)
                                      :background ,(gc 'yellow+1))))
         (term-color-blue        ((,c :foreground ,(gc 'blue)
                                      :background ,(gc 'blue-1))))
         (term-color-magenta     ((,c :foreground ,(gc 'magenta+1)
                                      :background ,(gc 'magenta))))
         (term-color-cyan        ((,c :foreground ,(gc 'cyan)
                                      :background ,(gc 'cyan+1))))

         ;; texinfo
         (texinfo-heading ((,c :inherit alect-title)))

         ;; tuareg
         (tuareg-font-lock-governing-face          ((,c :foreground ,(gc 'fg+2)
                                                        :weight bold)))
         (tuareg-font-lock-multistage-face         ((,c :foreground ,(gc 'blue-2)
                                                        :background ,(gc 'bg))))
         (tuareg-font-lock-line-number-face        ((,c :foreground ,(gc 'fg-2))))
         (tuareg-font-lock-operator-face           ((,c :foreground ,(gc 'green-1))))
         (tuareg-font-lock-module-face             ((,c :foreground ,(gc 'cyan))))
         (tuareg-font-lock-constructor-face        ((,c :foreground ,(gc 'yellow))))
         (tuareg-font-lock-error-face              ((,c :inherit error
                                                        :underline t)))
         (tuareg-font-lock-interactive-error-face  ((,c :inherit font-lock-warning-face)))
         (tuareg-font-lock-interactive-output-face ((,c :foreground ,(gc 'fg))))

         ;; utop
         (utop-prompt ((,c :inherit alect-prompt)))
         (utop-error  ((,c :inherit error)))
         (utop-stderr ((,c :foreground ,(gc 'red+1))))

         ;; vc
         (vc-up-to-date-state    ((,c :foreground ,(gc 'green-1))))
         (vc-edited-state        ((,c :foreground ,(gc 'yellow+1))))
         (vc-missing-state       ((,c :foreground ,(gc 'red))))
         (vc-conflict-state      ((,c :foreground ,(gc 'red+2)
                                      :weight bold)))
         (vc-locked-state        ((,c :foreground ,(gc 'cyan-1))))
         (vc-locally-added-state ((,c :foreground ,(gc 'blue))))
         (vc-needs-update-state  ((,c :foreground ,(gc 'magenta))))
         (vc-removed-state       ((,c :foreground ,(gc 'red-1))))

         ;; emacs-w3m
         (w3m-anchor                       ((,c :inherit link)))
         (w3m-arrived-anchor               ((,c :inherit link-visited)))
         (w3m-form                         ((,c :foreground ,(gc 'red-1)
                                                :underline t)))
         (w3m-form-button                  ((,c :inherit alect-button)))
         (w3m-form-button-pressed          ((,c :inherit alect-button-pressed)))
         (w3m-form-button-mouse            ((,c :inherit alect-button-mouse)))
         (w3m-tab-background               ((,c :inherit alect-tab-default)))
         (w3m-tab-selected                 ((,c :inherit alect-tab-selected)))
         (w3m-tab-selected-retrieving      ((,c :inherit w3m-tab-selected
                                                :foreground ,(gc 'red+1))))
         (w3m-tab-selected-background      ((,c :background ,(gc 'bg))))
         (w3m-tab-unselected               ((,c :inherit alect-tab-unselected)))
         (w3m-tab-unselected-retrieving    ((,c :inherit w3m-tab-unselected
                                                :foreground ,(gc 'red+1))))
         (w3m-tab-unselected-unseen        ((,c :inherit w3m-tab-unselected
                                                :foreground ,(gc 'fg))))
         (w3m-tab-mouse                    ((,c :inherit alect-tab-mouse)))
         (w3m-header-line-location-title   ((,c :inherit header-line)))
         (w3m-header-line-location-content ((,c :foreground ,(gc 'blue-1)
                                                :inherit header-line)))
         (w3m-history-current-url          ((,c :inherit alect-selected-item)))
         (w3m-image-anchor                 ((,c :background ,(gc 'bg+1))))

         ;; which-func-mode
         (which-func ((,c :inherit font-lock-function-name-face)))

         ;; whitespace-mode
         (whitespace-space            ((,c :foreground ,(gc 'bg+1))))
         (whitespace-hspace           ((,c :background ,(gc 'bg)
                                           :foreground ,(gc 'yellow))))
         (whitespace-tab              ((,c :background ,(gc 'bg+1)
                                           :foreground ,(gc 'blue-2))))
         (whitespace-newline          ((,c :foreground ,(gc 'blue-2))))
         (whitespace-trailing         ((,c :inherit trailing-whitespace)))
         (whitespace-line             ((,c :background ,(gc 'bg-2))))
         (whitespace-empty            ((,c :background ,(gc 'bg+1))))
         (whitespace-indentation      ((,c :background ,(gc 'bg+1)
                                           :foreground ,(gc 'red))))
         (whitespace-space-before-tab ((,c :background ,(gc 'bg+1)
                                           :foreground ,(gc 'green))))
         (whitespace-space-after-tab  ((,c :background ,(gc 'bg+1)
                                           :foreground ,(gc 'blue))))

         ;; widget
         (widget-field             ((,c :background ,(gc 'bg)
                                        :box (:line-width -1
                                              :color ,(gc 'fg-2)
                                              :style nil))))
         (widget-button            ((,c :inherit alect-button)))
         (widget-button-pressed    ((,c :inherit alect-button-pressed)))
         (widget-documentation     ((,c :inherit font-lock-doc-face)))
         (widget-inactive          ((,c :inherit shadow)))
         (widget-single-line-field ((,c :foreground ,(gc 'fg)
                                        :inherit widget-field)))

         ;; woman
         (woman-bold     ((,c :inherit Man-overstrike)))
         (woman-italic   ((,c :inherit Man-underline))))

       ;; VARIABLES
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
\". c " (gc 'cyan)  "\",
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
\". c " (gc 'cyan) "\",
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

         ;; gnus startup logo
         (gnus-logo-colors '(,(gc 'cyan+1) ,(gc 'gray-1)) t)

         ;; diary events in calendar
         (diary-entry-marker 'font-lock-variable-name-face t)

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
         )))))

(defun alect-substitute-color (theme-name plist prop)
  "Substitute color name for property PROP in property list PLIST.

Return plist with substituted color value.  Replace a color only
if PROP contains such color name (symbol from `alect-colors').

This function is destructive: PLIST may not stay the same.

See `alect-substitute-colors-in-plist' for details."
  (let ((color-name (plist-get plist prop))
        color-val)
    (and (symbolp color-name)
         (setq color-val (alect-get-color theme-name color-name))
         (setq plist (plist-put plist prop color-val)))
    plist))

(defun alect-substitute-colors-in-plist (theme-name plist)
  "Substitute color names in property list PLIST with color values.

PLIST can also be a list containing property list.

Color values (strings) are defined by color names (symbols) for a
specified theme THEME-NAME from `alect-colors' variable.  Replace
colors for the `:foreground' and `:background' properties.  If
there is also `:box' property in PLIST, replace its `:color'
property as well.

Return plist with substituted colors.  This function is
destructive: PLIST may not stay the same."
  (if (and (consp (car plist))
           (null (cdr plist)))
      (alect-substitute-colors-in-plist theme-name (car plist))
    (setq plist (alect-substitute-color theme-name plist :foreground))
    (setq plist (alect-substitute-color theme-name plist :background))
    (let ((box-plist (plist-get plist :box)))
      (and box-plist
           (setq box-plist (alect-substitute-color theme-name box-plist :color))
           (setq plist (plist-put plist :box box-plist))))
    plist))

(defun alect-substitute-colors-in-faces (theme-name faces)
  "Substitute color names in a list FACES with color values.

FACES is a list of face specifications accepted by
`custom-theme-set-faces'.

Return a list of faces with substituted colors.  This function is
destructive: FACES may not stay the same.

See `alect-substitute-colors-in-plist' for details."
  (mapcar (lambda (face)
            (list (car face)
                  (mapcar (lambda (spec)
                            (cons (car spec)
                                  (alect-substitute-colors-in-plist
                                   theme-name (cdr spec))))
                          (cadr face))))
          faces))

(defun alect-override-faces (original overriding)
  "Override faces from ORIGINAL list with faces from OVERRIDING list.

Both ORIGINAL and OVERRIDING are lists of face specifications
accepted by `custom-theme-set-faces'.

Replace face specifications from ORIGINAL list with the ones from
OVERRIDING list, add new faces from OVERRIDING list, and return the
resulting list.

This function is destructive: ORIGINAL list may not stay the same."
  (mapc (lambda (face)
          (let ((orig-face (assoc (car face) original)))
            (and orig-face
                 (setq original (delete orig-face original)))
            (add-to-list 'original face)))
        overriding)
  original)

(defcustom alect-ignored-faces nil
  "List of faces that will not be themed.

If nil, an alect theme will change all faces it can.

See Info node `(elisp) Custom Themes' for information about how a
theme customizes faces and variables."
  :type '(choice
          (const :tag "Theme (change) all intended faces" nil)
          (repeat :tag "Choose ignored faces" face))
  :group 'alect)

(defcustom alect-ignored-variables nil
  "List of variables that will not be themed.

If nil, an alect theme will change some variables.
If t, none of the variables will be modified, only faces are
themed.

See Info node `(elisp) Custom Themes' for information about how a
theme customizes faces and variables."
  :type `(choice
          (const :tag "Theme (change) all intended variables" nil)
          (const :tag "Ignore all (do not change any variable)" t)
          (repeat :tag "Choose ignored variables"
                  (radio
                   ,@(mapcar (lambda (var-def)
                               (list 'variable-item (car var-def)))
                             (cdr (alect-get-customization nil))))))
  :group 'alect)

(defun alect-delete-objects (original ignored)
  "Delete IGNORED objects from ORIGINAL list.

Delete all objects from ORIGINAL list whose car is an object from
IGNORED list and return result.

If IGNORED is nil, return ORIGINAL.  If IGNORED is t, return nil.

This function is destructive to ORIGINAL."
  (cond
   ((null ignored) original)
   ((eq t ignored) nil)
   (t (cl-delete-if (lambda (elt) (memq (car elt) ignored))
                    original))))

(defmacro alect-create-theme (theme &optional invert)
  "Define and provide a color theme THEME.
For INVERT, see `alect-get-color'."
  (let* ((theme-name  (intern (concat "alect-"
                                      (symbol-name theme)
                                      (and invert "-alt"))))
         (theme-vals  (alect-get-customization theme invert))
         (theme-faces (alect-override-faces
                       (alect-delete-objects
                        (car theme-vals) alect-ignored-faces)
                       (alect-substitute-colors-in-faces
                        theme (copy-tree alect-overriding-faces))))
         (theme-vars  (alect-delete-objects
                       (cdr theme-vals) alect-ignored-variables)))

    `(progn
       (deftheme ,theme-name ,(format "The %s color theme."
                                      (concat (and invert "alternative ")
                                              (symbol-name theme))))
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

;;; alect-themes.el ends here
