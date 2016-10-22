;;; alect-tests.el --- Tests for alect-themes package

;; Copyright © 2013, 2016 Alex Kost <alezost@gmail.com>

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

;; These tests can be run like this:
;;
;;   cd /path/to/alect-themes
;;   emacs -Q -nw -L . --batch --eval '(progn (load-file "tests/alect-tests.el") (ert-run-tests-batch-and-exit))'

;;; Code:

(require 'ert)
(require 'alect-themes)

(ert-deftest alect-test-substitute-colors ()
  "Test functions for substituting colors."
  (should (equal (alect-substitute-color
                  'dark '(:foreground "pink" :background green-1) :foreground)
                 '(:foreground "pink" :background green-1)))
  (should (equal (alect-substitute-color
                  'dark '(:foreground "pink" :background green-1) :background)
                 '(:foreground "pink" :background "#32cd32")))
  (should (equal (alect-substitute-colors-in-plist
                  'light '(((:foreground "pink" :background green-1 :underline t
                                         :box (:line-width 1 :color fg :style nil)))))
                 '(:foreground "pink" :background "#1c9e28" :underline t
                               :box (:line-width 1 :color "#3f3f3f" :style nil))))
  (should (equal (alect-substitute-colors-in-faces
                  'light
                  '((fringe ((t (:background "pink"))))
                    (font-lock-string-face ((t :foreground green-1)))
                    (button ((((class color) (min-colors 88) (:background blue)) (:foreground magenta))
                             (((class color) (background dark)) :foreground "LightSkyBlue")
                             (((class color) (min-colors 16)) (:bold t :background fg+2))
                             (t (:slant italic :box (:line-width 1 :color red-1 :background cyan)))))))
                 '((fringe ((t :background "pink")))
                   (font-lock-string-face ((t :foreground "#1c9e28")))
                   (button ((((class color) (min-colors 88) (:background blue)) :foreground "#a020f0")
                            (((class color) (background dark)) :foreground "LightSkyBlue")
                            (((class color) (min-colors 16)) :bold t :background "#101010")
                            (t :slant italic :box (:line-width 1 :color "#e43838" :background cyan)))))))
  (should (equal (alect-substitute-colors-in-faces
                  'dark
                  '((hl-line ((((class color) (min-colors 256)) :background bg)
                              (t nil)))))
                 '((hl-line ((((class color) (min-colors 256)) :background "#4f4f4f")
                             (t nil)))))))

(provide 'alect-tests)

;;; alect-tests.el ends here
