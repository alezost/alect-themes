## About

Alect is a package that provides (rather low contrast but colourful
enough) light and dark color themes for Emacs 24 or later.  I use it
only with GUI, so colors in terminal may look not very nice.

You can open
[colors](https://github.com/alezost/alect-themes/blob/master/colors)
file (in Emacs) to get an idea about the color palettes.

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

## Configuration

All color values for the themes are stored in `alect-colors`
variable.  You can change this variable by customizing it or you may
use `alect-generate-colors` function (see how the variable is defined
in the code).

However those methods redefine the whole variable, so if the palette
will be changed in future (it happens sometimes) or a new theme will
be added (it's planned), you may not notice that.  So you can use
another approach if you want to modify only some colors.

Let's say you don't like `cursor` color for the light theme and
`red-2` color (it is used for `font-lock-string-face`, and you
strongly believe that «strings should not be red!!») for both themes.
You can change those colors by putting this into your `.emacs`:

```lisp
(eval-after-load 'alect-themes
  '(progn
     (alect-set-color 'light 'cursor "black")
     (alect-set-color 'light 'red-2 "#126512")
     (alect-set-color 'dark 'red-2 "#32cd32")))
```

### Alternative themes

Along with 2 original light and dark themes, the package provides 2
inverted (alternative) themes.  They use the same color palettes, so
they look very similar to the original ones.  The difference (by
default) is that dark and bright colors are reversed.

However these 2 themes can be configured with
`alect-inverted-color-regexp` variable (for details, see docstrings of
this variable and `alect-get-color` function).  For example, if you
set this variable to invert background colors, alternative themes will
look... unusual – see [alternative screenshots](#dired-elisp).

### Emacs 24.3.1 and earlier

While using any theme (not only from this package) you may meet faces
that do not look how they should (intended by the theme).  For example,
if you enable `alect-light` theme, you can see ugly gray buttons (the
left picture) in the `Custom-mode` instead of the themed colored
buttons (the right picture):

<a href="http://imgur.com/M6eyfWq" title="alect-light - Custom-mode">
<img src="http://i.imgur.com/M6eyfWq.png" title="alect-light - Custom-mode (wrong colors)" width=320 height=240/></a>
<a href="http://imgur.com/WOz6Po2" title="alect-light - Custom-mode">
<img src="http://i.imgur.com/WOz6Po2.png" title="alect-light - Custom-mode (proper colors)" width=320 height=240/></a>

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

## Screenshots

### C, shell, linum, ido

**Font:** *Anonymous Pro-13*

<a href="http://imgur.com/Mjupclm">
<img src="http://i.imgur.com/Mjupclm.png" title="alect-light - c, shell" width=320 height=240/></a>
<a href="http://imgur.com/AiG3TsM">
<img src="http://i.imgur.com/AiG3TsM.png" title="alect-dark - c, shell" width=320 height=240/></a>

### Org, markdown

**Font:** *DejaVu Sans Mono-12*

<a href="http://imgur.com/B1Pl5kX">
<img src="http://i.imgur.com/B1Pl5kX.png" title="alect-light - org, markdown" width=320 height=240/></a>
<a href="http://imgur.com/Tck5Aj2">
<img src="http://i.imgur.com/Tck5Aj2.png" title="alect-dark - org, markdown" width=320 height=240/></a>

### Dired, elisp

Alternative themes, configured to invert background (see
[alternative configuration](#alternative-themes)) like this:

```lisp
(setq alect-inverted-color-regexp "^\\(bg\\)\\([-+]\\)\\([012]\\)$")
```

**Font:** *Anonymous Pro-13*

<a href="http://imgur.com/ljO1Dlf">
<img src="http://i.imgur.com/ljO1Dlf.png" title="alect-light-alt - dired, elisp" width=320 height=240/></a>
<a href="http://imgur.com/HNMr4qj">
<img src="http://i.imgur.com/HNMr4qj.png" title="alect-dark-alt - dired, elisp" width=320 height=240/></a>

## Feedback

If you want this package to support more faces, you may send me a
letter about your favourite unsupported modes.

