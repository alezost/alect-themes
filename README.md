## About

Alect is a package that provides (rather low contrast but colourful
enough) **configurable** light and dark color themes for GNU Emacs 24 or
later.  I use it only with GUI, so colors in terminal may look not very
nice.

## History

At first i had only a light theme – it was just a set of customized
faces.  Then i realized that at night it's better for eyes to use a
dark theme (it was derived from zenburn-theme initially, but then the
colors were modified a lot).  The idea of creating two themes with
different colors and the same code base came from solarized-theme.
The code of solarized and zenburn themes was used hardly.  Many thanks
to their authors.

## Installation

### Manual

Add this to your init file (`~/.emacs.d/init.el` or `~/.emacs`):

```lisp
(add-to-list 'load-path              "/path/to/alect-themes")
(add-to-list 'custom-theme-load-path "/path/to/alect-themes")
```

### MELPA

The package can be installed from [MELPA](http://melpa.milkbox.net).
(with `M-x package-install` or `M-x list-packages`).

If you want to enable (see [Usage section](#usage)) **any** theme
installed with a package system on Emacs start, you should know the
following: Emacs loads packages **after** processing the init file, so
loading a theme will fail because the path to a theme is not known
yet.  That's why you need to initialize the package system before
loading the theme:

```lisp
(setq package-enable-at-startup nil)
(package-initialize)
...
(load-theme ...)
```

For further details, see `(info "(emacs) Package Installation")`.

## Usage

To activate a theme interactively use `customize-themes` or `load-theme`:

`M-x load-theme RET alect-light`

To load a theme on Emacs start, add this to your init file:

```lisp
(load-theme 'alect-light t)
```

*Note:* For quick switching between themes (with unloading the previous
one), I use `utl-load-theme` function from
[emacs-utils](https://github.com/alezost/emacs-utils/blob/master/utl-color.el)

## Configuration

You can find the names and values of all colors used by alect-themes in
`alect-colors` variable.  Also you can open
[colors](https://github.com/alezost/alect-themes/blob/master/colors)
file **in Emacs** to get an idea about the used color palette.

There are 2 main ways for configuring the themes:
- modifying palette (`alect-colors` variable);
- overriding face specifications.

### Modifying palette

If you don't like how some colors look, you can change `alect-colors`
variable by customizing it or by using `alect-generate-colors` function
(see how the variable is defined in the code).

However those methods redefine the whole variable, so if the palette
will be changed in future (it happens sometimes) or a new theme will
be added (it's planned), you may not notice that.  So you can use
another approach if you want to modify only some colors.

Let's say, you don't like `cyan-2` color for the light theme as it's too
light and `bg-1` color for the dark theme as you prefer black color for
the background.  You can change those colors by putting this into your
`.emacs` (before loading an alect-theme if you use it on Emacs start):

```lisp
(eval-after-load 'alect-themes
  '(progn
     (alect-set-color 'light 'cyan-2 "#00a8a8")
     (alect-set-color 'dark 'bg-1 "black")))
```

The function `alect-set-color` is just a convenient way for modifying
`alect-colors` variable, so if you are playing with it, don't forget to
reload an alect-theme for the changes to take effect.

### Overriding faces

If you don't like how particular faces look, you can change those by
modifying `alect-overriding-faces` variable.  The real power here is
that you can use themed color names from `alect-colors` along with the
usual strings with hex values or defined color names (available with
``M-x list-colors-display``).

Let's say, you want green strings, gray comments, more distinguishable
mode-line, and of course you don't like those pink (`magenta-1`) prompts
everywhere (minibuffer, comint, ...).  Just set that variable like this:

```lisp
(setq
 alect-overriding-faces
 '((alect-prompt           ((t :foreground blue :weight bold)))
   (font-lock-string-face  ((t :foreground green-1)))
   (font-lock-doc-face     ((t :inherit font-lock-string-face)))
   (font-lock-comment-face ((t :foreground gray)))
   (mode-line-buffer-id    ((t :foreground "yellow" :weight bold)))
   (mode-line              ((((background light))
                             :foreground fg+1 :background "#ffaaaa"
                             :box (:line-width 2 :color bg-2 :style nil))
                            (((background dark))
                             :foreground fg+1 :background "firebrick3"
                             :box (:line-width 2 :color bg-2 :style nil))))))
```

See [these screenshots](#elisp-ido) to compare the original and modified
themes.

### Alternative themes

Along with 2 original light and dark themes, the package provides 2
inverted (alternative) themes (`alect-light-alt` and `alect-dark-alt`).
They use the same color palettes, so they look very similar to the
original ones.  The difference (by default) is that dark and bright
colors are reversed.

There is an additional way of configuring alternative themes: with
`alect-inverted-color-regexp` variable (for details, see docstrings of
this variable and `alect-get-color` function).  For example, you may set
this variable to invert background colors:

```lisp
(setq alect-inverted-color-regexp "^\\(bg\\)\\([-+]\\)\\([012]\\)$")
```

See [these screenshots](#dired-elisp) for the result.

### Emacs 24.3.1 and earlier

While using **any** theme (not only from this package), you may meet
faces that do not look how they should (intended by the theme).  For
example, if you enable `alect-light` theme, you can see ugly gray
buttons and other faces in the `Custom-mode` (the left picture)
instead of the themed colored buttons (the right picture):

<a href="http://i.imgur.com/sLKZI90.png">
<img src="http://i.imgur.com/Ah1YXNH.png" title="alect-light - Custom-mode (wrong colors)"/></a>
<a href="http://i.imgur.com/yn6Njtq.png">
<img src="http://i.imgur.com/66G9VvX.png" title="alect-light - Custom-mode (proper colors)"/></a>

This happens because Emacs applies default face settings even for a
themed face.  This behaviour is changed in new versions of Emacs (24.4
and above).  Happily it can be easily fixed for earlier versions by
redefining `face-spec-recalc` function (can be found on
[Emacs git mirror](http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/faces.el)):

```lisp
(defun face-spec-recalc (face frame)
  "Reset the face attributes of FACE on FRAME according to its specs.
This applies the defface/custom spec first, then the custom theme specs,
then the override spec."
  (while (get face 'face-alias)
    (setq face (get face 'face-alias)))
  (face-spec-reset-face face frame)
  ;; If FACE is customized or themed, set the custom spec from
  ;; `theme-face' records, which completely replace the defface spec
  ;; rather than inheriting from it.
  (let ((theme-faces (get face 'theme-face)))
    (if theme-faces
	(dolist (spec (reverse theme-faces))
	  (face-spec-set-2 face frame (cadr spec)))
      (face-spec-set-2 face frame (face-default-spec face))))
  (face-spec-set-2 face frame (get face 'face-override-spec)))
```

If you put it into your `.emacs`, you will always get pure themes
without unintended face settings.

### Emacs bug

Emacs has a bug
([#16266](http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16266)) that
doesn't allow to set undefined variables.  For example, if a theme
customizes a color for emms icon and you load the theme before emms,
then emms icon will use a default color instead of the themed one.  This
bug takes effect only on `defvar`-ed variables, `defcustom`-ed variables
are safe.

The range of such variables (effected by the bug) suitable for
colorizing is very limited.  For alect themes, the following variables
are themed: `emms-mode-line-icon-image-cache`,
`gnus-mode-line-image-cache` and `gnus-logo-colors`.

You can use this workaround to avoid the bug:
```lisp
(defadvice custom-theme-set-variables
  (around fix-inhibit-bug activate)
  "Allow setting of undefined variables in themes."
  (let (custom--inhibit-theme-enable)
    ad-do-it))
```

## Screenshots

You can see the following and other screenshots in
[this imgur album](http://imgur.com/a/eBx96).

### C, shell, linum, ido

**Themes:** *alect-light*, *alect-dark*

**Font:** *Terminus-12*

<a href="http://i.imgur.com/XS9TM80.png">
<img src="http://i.imgur.com/XS9TM80.png" title="alect-light - c, shell" width=320 height=240/></a>
<a href="http://i.imgur.com/Fj0s7oT.png">
<img src="http://i.imgur.com/Fj0s7oT.png" title="alect-dark - c, shell" width=320 height=240/></a>

### Org, markdown

**Themes:** *alect-light*, *alect-dark*

**Font:** *DejaVu Sans Mono-12*

<a href="http://i.imgur.com/B1Pl5kX.png">
<img src="http://i.imgur.com/B1Pl5kX.png" title="alect-light - org, markdown" width=320 height=240/></a>
<a href="http://i.imgur.com/Tck5Aj2.png">
<img src="http://i.imgur.com/Tck5Aj2.png" title="alect-dark - org, markdown" width=320 height=240/></a>

### Magit

**Themes:** *alect-light*, *alect-dark*

**Font:** *Anonymous Pro-13*

<a href="http://i.imgur.com/rjGvhV6.png">
<img src="http://i.imgur.com/rjGvhV6.png" title="alect-light - magit" width=320 height=240/></a>
<a href="http://i.imgur.com/5WJ4xu9.png">
<img src="http://i.imgur.com/5WJ4xu9.png" title="alect-dark - magit" width=320 height=240/></a>

### Elisp, ido

**Themes:** *alect-dark* (default), *alect-dark* (modified) – the
original dark theme and a dark theme with some changed faces (see
[overriding faces](#overriding-faces))

**Font:** *Liberation Mono-12*

<a href="http://i.imgur.com/gZAZgvT.png">
<img src="http://i.imgur.com/gZAZgvT.png" title="alect-dark (default) - elisp" width=320 height=240/></a>
<a href="http://i.imgur.com/mAlbxLd.png">
<img src="http://i.imgur.com/mAlbxLd.png" title="alect-dark (modified) - elisp" width=320 height=240/></a>

### Dired, elisp

**Themes:** *alect-light-alt* (modified), *alect-dark-alt* (modified) –
alternative themes, configured to invert background (see
[configuring alternative themes](#alternative-themes))

**Font:** *Anonymous Pro-13*

<a href="http://i.imgur.com/ljO1Dlf.png">
<img src="http://i.imgur.com/ljO1Dlf.png" title="alect-light-alt (modified) - dired, elisp" width=320 height=240/></a>
<a href="http://i.imgur.com/HNMr4qj.png">
<img src="http://i.imgur.com/HNMr4qj.png" title="alect-dark-alt (modified) - dired, elisp" width=320 height=240/></a>

## Feedback

If you want this package to support more faces, you may send me a
letter about your favourite unsupported modes.

