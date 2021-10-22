[![License GPL 3](https://img.shields.io/badge/license-GPL_3-orange.svg)](https://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/alect-themes-badge.svg)](https://melpa.org/#/alect-themes)
[![MELPA Stable](https://stable.melpa.org/packages/alect-themes-badge.svg)](https://stable.melpa.org/#/alect-themes)

## About

Alect is a package that provides (rather low contrast but colourful
enough) **configurable** light, dark and black color themes for GNU
Emacs 24 or later.  The themes are intended to be used with GUI (see
[Class of terminals](#class-of-terminals)).

## Table of Contents

* [Supported modes](#supported-modes)
* [Installation](#installation)
  * [Manual](#manual)
  * [MELPA](#melpa)
* [Usage](#usage)
* [Configuration](#configuration)
  * [Class of terminals](#class-of-terminals)
  * [Modifying palette](#modifying-palette)
  * [Ignoring faces and variables](#ignoring-faces-and-variables)
  * [Overriding faces](#overriding-faces)
  * [Alternative themes](#alternative-themes)
  * [Other variables](#other-variables)
  * [Emacs 24.3.1 and earlier](#emacs-2431-and-earlier)
  * [Emacs bug in themed variables](#emacs-bug-in-themed-variables)
* [Screenshots](#screenshots)
* [History](#history)
* [Contributors](#contributors)

## Supported modes

Along with the general basic faces, the following modes and packages are
supported (themed):

- Built-in packages (part of Emacs):
  + isearch
  + ido
  + help, apropos, info, man
  + mode-line (including vc faces)
  + calendar, diary
  + changelog
  + comint, eshell, term
  + compilation
  + widget, customization
  + diff, ediff
  + dired
  + erc
  + epa
  + ert
  + [Org](https://orgmode.org/)
  + message
  + gnus (also see a note about Gnus [here](#other-variables))
  + flyspell
  + makefile
  + ruler
  + whitespace
  + linum
  + package
  + show-paren
  + speedbar
- [aurel](https://github.com/alezost/aurel)
- [auto-complete](https://github.com/auto-complete/auto-complete)
- Caml packages: [tuareg](https://github.com/ocaml/tuareg),
  [merlin](https://github.com/the-lambda-church/merlin),
  [utop](https://github.com/diml/utop)
- [company](https://company-mode.github.io/)
- [cperl](https://github.com/jrockway/cperl-mode)
- [debbugs](https://elpa.gnu.org/packages/debbugs.html)
- [diff-hl](https://github.com/dgutov/diff-hl)
- [dictem](https://github.com/cheusov/dictem)
- [EMMS](https://www.gnu.org/software/emms/)
- [Geiser](https://github.com/jaor/geiser)
- [google-translate](https://github.com/atykhonov/google-translate)
- [Emacs-Guix](https://github.com/alezost/guix.el)
- [Helm](https://emacs-helm.github.io/helm/) (partial support)
- [Hydra](https://github.com/abo-abo/hydra)
- [indent-guide](https://github.com/zk-phi/indent-guide)
- [ivy](https://github.com/abo-abo/swiper)
- [Magit](https://magit.vc/)
- [markdown](https://jblevins.org/projects/markdown-mode/)
- [mu4e](https://github.com/djcb/mu)
- [org-transclusion](https://github.com/nobiot/org-transclusion)
- [powerline](https://github.com/milkypostman/powerline)
- [realgud](https://github.com/realgud/realgud)
- [rubik](https://github.com/Kurvivor19/rubik-mode)
- [sauron](https://github.com/djcb/sauron)
- [slime](https://common-lisp.net/project/slime/)
- [sunrise-commander](https://github.com/escherdragon/sunrise-commander)
- [syslog](https://github.com/vapniks/syslog-mode)
- [tabbar](https://github.com/dholm/tabbar)
- [transient](https://github.com/magit/transient)
- [w3m](http://emacs-w3m.namazu.org/)
- [which-key](https://github.com/justbur/emacs-which-key)
- [winum](https://github.com/deb0ch/emacs-winum)

The other packages are not supported yet even if there are some
customized faces (these faces left from [other themes](#history)).  So
if you see some ugly faces or if you would like some mode to be
supported, you may mail me or
[open an issue](https://github.com/alezost/alect-themes/issues/new).

## Installation

### Manual

Add this to your init file (`~/.emacs.d/init.el` or `~/.emacs`):

```elisp
(add-to-list 'load-path              "/path/to/alect-themes")
(add-to-list 'custom-theme-load-path "/path/to/alect-themes")
```

### MELPA

The package can be installed from [MELPA](https://melpa.org) (with `M-x
package-install` or `M-x list-packages`).

If you want to enable (see [Usage section](#usage)) **any** theme
installed with a package system on Emacs start, you should know the
following: Emacs loads packages **after** processing the init file, so
loading a theme will fail because the path to a theme is not known
yet.  That's why you need to initialize the package system before
loading the theme:

```elisp
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

```elisp
(load-theme 'alect-light t)
```

*Note:* For quick switching between themes (with unloading the previous
one), I use `al/load-theme` command from my
[emacs-config](https://github.com/alezost/emacs-config/blob/master/utils/al-color.el).

## Configuration

You can find the names and values of all colors used by alect-themes in
`alect-colors` variable.  Also you can open
[colors](https://github.com/alezost/alect-themes/blob/master/colors)
file **in Emacs** to get an idea about the used color palette.

There are several ways for configuring the themes:
- modifying palette (`alect-colors` variable);
- ignoring faces and variables;
- overriding face specifications;
- configuring other alect variables.

### Class of terminals

By default only graphical terminals are supported, i.e. if you enable a
theme, you will see themed faces in GUI and default faces in text-only
terminals.  So if you use `emacs --daemon`, you will not be disturbed by
the ugly colors when you are in a non-graphical terminal.

You can add support for other terminals by setting `alect-display-class`
variable.  For example, if you want to enable alect-themes in 256-colors
terminals, use the following:

```elisp
(setq alect-display-class '((class color) (min-colors 256)))
```

See `(info "(elisp) Defining Faces")` for how a class of terminals
should be specified.

### Modifying palette

If you don't like how some colors look, you can change `alect-colors`
variable by customizing it or by using `alect-generate-colors` function
(see how the variable is defined in the code).

However those methods redefine the whole variable, so if the palette
will be changed in future (it happens sometimes) or a new theme will be
added, you may not notice that.  So you can use another approach if you
want to modify only some colors.

Let's say, you don't like `cyan-2` color for the light theme as it's too
light and `bg-1` color for the dark theme as you prefer black color for
the background.  You can change those colors by putting this into your
`.emacs` (before loading an alect-theme if you use it on Emacs start):

```elisp
(with-eval-after-load 'alect-themes
  (alect-set-color 'light 'cyan-2 "#00a8a8")
  (alect-set-color 'dark 'bg-1 "black"))
```

The function `alect-set-color` is just a convenient way for modifying
`alect-colors` variable, so if you are playing with it, don't forget to
reload an alect-theme for the changes to take effect.

### Ignoring faces and variables

By default along with a lot of faces, an `alect-theme` customizes
several variables that contain color information,
e.g. `ansi-color-names-vector` (see also
[Emacs bug in themed variables](#emacs-bug-in-themed-variables)).  You
can disable theming of faces and variables with `alect-ignored-faces`
and `alect-ignored-variables` variables.

For example, if you prefer the default appearance of the titles in
`Info-mode` and of the minibuffer prompt, and if you want to disable
modifying the variables at all, use the following:

```elisp
(setq alect-ignored-variables t
      alect-ignored-faces
      '(minibuffer-prompt
        info-title-1 info-title-2
        info-title-3 info-title-4))
```

### Overriding faces

If you don't like how particular faces look, you can change those by
modifying `alect-overriding-faces` variable.  The real power here is
that you can use themed color names from `alect-colors` along with the
usual strings with hex values or defined color names (available with
``M-x list-colors-display``).

Let's say, you want green strings, gray comments, more distinguishable
mode-line, and of course you don't like those pink (`magenta-1`) prompts
everywhere (minibuffer, comint, ...).  Just set that variable like this:

```elisp
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

Along with 3 original light, dark and black themes, the package provides
inverted (alternative) themes (`alect-light-alt` and `alect-dark-alt`).
They use the same color palettes, so they look very similar to the
original ones.  The difference (by default) is that dark and bright
colors are reversed.

There is an additional way of configuring alternative themes: with
`alect-inverted-color-regexp` variable (for details, see docstrings of
this variable and `alect-get-color` function).  For example, you may set
this variable to invert background colors:

```elisp
(setq alect-inverted-color-regexp "^\\(bg\\)\\([-+]\\)\\([012]\\)$")
```

See [these screenshots](#dired-elisp) for the result.

### Other variables

Many headers and titles are themed and you may not like their height.
The following 3 variables may be useful:
- `alect-header-height`
- `alect-single-title-height`
- `alect-multiple-titles-height`

For example, if you want to have a normal height for `org` and
`markdown` titles, use this (put it in `.emacs` before loading an
alect-theme):

```elisp
(setq alect-multiple-titles-height 1.0)
```

*Note:* `1.0` and `1` are different values.  You can play with `Height`
in Custom buffer (`M-x customize-face RET <any-face>`) to see how
integers and floats are treated.

There are other settings that may affect a visual appearance.  For
example GNUS uses widgets in article buffers.  And `widget-button` face
looks like a real button in `alect-themes`.  I find such buttons not
very attractive in articles but I also don't want to modify
`widget-button` face, so I use the following to get rid of button faces
only in GNUS articles:

```elisp
(add-hook 'gnus-article-mode-hook
          (lambda () (setq-local widget-button-face nil)))
```

### Emacs 24.3.1 and earlier

While using **any** theme (not only from this package), you may meet
faces that do not look how they should (intended by the theme).  For
example, if you enable `alect-light` theme, you can see ugly gray
buttons and other faces in the `Custom-mode` (the left picture)
instead of the themed colored buttons (the right picture):

<a href="https://i.imgur.com/sLKZI90.png">
<img src="https://i.imgur.com/Ah1YXNH.png" title="alect-light - Custom-mode (wrong colors)"/></a>
<a href="https://i.imgur.com/yn6Njtq.png">
<img src="https://i.imgur.com/66G9VvX.png" title="alect-light - Custom-mode (proper colors)"/></a>

This happens because Emacs applies default face settings even for a
themed face.  This behaviour is changed in the new versions of Emacs:
since 24.4 you will always get pure themes without unintended face
settings.  If you use a previous version, you can try the following
workaround to achieve the new behaviour:

```elisp
(when (version< emacs-version "24.3.50")
  (defun face-spec-recalc-new (face frame)
    "Improved version of `face-spec-recalc'."
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

  (defadvice face-spec-recalc (around new-recalc (face frame) activate)
    "Use `face-spec-recalc-new' instead."
    (face-spec-recalc-new face frame)))
```

That version of `face-spec-recalc` (wrapped with advice) is one of the
development variants from 24.3.50.1.  It works good except of one
particular case: if you try to use that workaround and enable a theme in
an unsupported (by the theme) terminal (e.g. alect-theme in a text
terminal), your monitor may suddenly explode.  Currently
[faces.el](https://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/faces.el)
is changed a lot (comparing to 24.3) and I don't know a better
workaround.

*Summary:* `alect-themes` use inheriting a lot and because of the nature
of faces in Emacs 24.3, **many** of them look ugly.  With Emacs 24.4 or
with the above workaround (use it only with GUI), the themes look how
they should.

### Emacs bug in themed variables

Emacs has a bug
([#16266](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=16266)) that
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

```elisp
(defadvice custom-theme-set-variables
    (around fix-inhibit-bug activate)
  "Allow setting of undefined variables in themes."
  (let (custom--inhibit-theme-enable)
    ad-do-it))
```

## Screenshots

You can see the following and other screenshots in
[this imgur album](https://imgur.com/a/eBx96).

### C, shell, linum, ido

**Themes:** *alect-light*, *alect-dark*

**Font:** *Terminus-12*

<a href="https://i.imgur.com/XS9TM80.png">
<img src="https://i.imgur.com/XS9TM80.png" title="alect-light - c, shell" width=320 height=240/></a>
<a href="https://i.imgur.com/Fj0s7oT.png">
<img src="https://i.imgur.com/Fj0s7oT.png" title="alect-dark - c, shell" width=320 height=240/></a>

### Org, markdown

**Themes:** *alect-light*, *alect-black*

**Font:** *DejaVu Sans Mono-12*

<a href="https://i.imgur.com/QoKlSxn.png">
<img src="https://i.imgur.com/QoKlSxn.png" title="alect-light - org, markdown" width=320 height=240/></a>
<a href="https://i.imgur.com/Ufx12Nm.png">
<img src="https://i.imgur.com/Ufx12Nm.png" title="alect-black - org, markdown" width=320 height=240/></a>

### Magit

**Themes:** *alect-light*, *alect-dark*

**Font:** *Anonymous Pro-13*

<a href="https://i.imgur.com/rjGvhV6.png">
<img src="https://i.imgur.com/rjGvhV6.png" title="alect-light - magit" width=320 height=240/></a>
<a href="https://i.imgur.com/5WJ4xu9.png">
<img src="https://i.imgur.com/5WJ4xu9.png" title="alect-dark - magit" width=320 height=240/></a>

### Elisp, ido

**Themes:** *alect-dark* (default), *alect-dark* (modified) – the
original dark theme and a dark theme with some changed faces (see
[overriding faces](#overriding-faces))

**Font:** *Liberation Mono-12*

<a href="https://i.imgur.com/gZAZgvT.png">
<img src="https://i.imgur.com/gZAZgvT.png" title="alect-dark (default) - elisp" width=320 height=240/></a>
<a href="https://i.imgur.com/mAlbxLd.png">
<img src="https://i.imgur.com/mAlbxLd.png" title="alect-dark (modified) - elisp" width=320 height=240/></a>

### Dired, elisp

**Themes:** *alect-light-alt* (modified), *alect-dark-alt* (modified) –
alternative themes, configured to invert background (see
[configuring alternative themes](#alternative-themes))

**Font:** *Anonymous Pro-13*

<a href="https://i.imgur.com/ljO1Dlf.png">
<img src="https://i.imgur.com/ljO1Dlf.png" title="alect-light-alt (modified) - dired, elisp" width=320 height=240/></a>
<a href="https://i.imgur.com/HNMr4qj.png">
<img src="https://i.imgur.com/HNMr4qj.png" title="alect-dark-alt (modified) - dired, elisp" width=320 height=240/></a>

## History

At first I had only a light theme – it was just a set of customized
faces.  Then I realized that at night it's better for eyes to use a
dark theme (it was derived from zenburn-theme initially, but then the
colors were modified a lot).  The idea of creating two themes with
different colors and the same code base came from solarized-theme.
The code of solarized and zenburn themes was used hardly.  Many thanks
to their authors.

## Contributors

The following people helped to improve the themes:

- Davor Rotim
- Rocky Bernstein
- Thomas S. Dye
