## About

Alect is a package provides (rather low contrast but colourful enough)
light and dark color themes for Emacs 24 or later.  I use it only with
GUI; terminals are not supported.

At first i had only a light theme -- it was just a set of customized
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

You can install `alect-themes` with [MELPA](http://melpa.milkbox.net).
using `list-packages` or `package-install`:

`M-x package-install alect-themes`

As emacs package system loads the packages after processing the init
file, loading a theme from `.emacs` (see below) will fail because the
path to a theme is not known yet, so you have to add the following
code to your init file before `(load-theme ...)`:

```lisp
(setq package-enable-at-startup nil)
(package-initialize)
```

For further details see emacs info: `(info "(emacs) Package Installation")`.

## Usage

To activate a theme interactively use `customize-themes` or `load-theme`:

`M-x load-theme RET alect-light`

To load a theme on Emacs startup add this to your init file:

```lisp
(load-theme 'alect-light t)
```

## Screenshots

![alect-light](http://i.imgur.com/D0UASEQ.png "alect-light-theme screenshot")
![alect-dark](http://i.imgur.com/GXnGuO0.png "alect-dark-theme screenshot")

