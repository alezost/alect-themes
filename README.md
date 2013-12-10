## About

Alect is a package that provides (rather low contrast but colourful
enough) light and dark color themes for Emacs 24 or later.  I use it
only with GUI, so colors in terminal may look not very nice.

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

## Screenshots

![alect-light](http://i.imgur.com/D0UASEQ.png "alect-light-theme screenshot")
![alect-dark](http://i.imgur.com/GXnGuO0.png "alect-dark-theme screenshot")

## Feedback

If you use this package and want it to support a major mode the faces
of which are not supported yet, you may send me a letter about it.

