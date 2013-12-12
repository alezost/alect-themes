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

## Screenshots

### Custom, emacs-lisp, minibuffer ("DejaVu Sans Mono-12" font)
<a href="http://imgur.com/D0UASEQ"><img src="http://i.imgur.com/D0UASEQ.png" width=320 height=240/></a>
<a href="http://imgur.com/GXnGuO0"><img src="http://i.imgur.com/GXnGuO0.png" width=320 height=240/></a>

### C, shell ("Anonymous Pro-13" font)
<a href="http://imgur.com/GkjhzKK"><img src="http://i.imgur.com/GkjhzKK.png" width=320 height=240/></a>
<a href="http://imgur.com/Stl0mba"><img src="http://i.imgur.com/Stl0mba.png" width=320 height=240/></a>

## Feedback

If you want this package to support more faces, you may send me a
letter about your favourite unsupported modes.

