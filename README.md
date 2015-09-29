# Vimish Fold

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/vimish-fold-badge.svg)](http://melpa.org/#/vimish-fold)
[![Build Status](https://travis-ci.org/mrkkrp/vimish-fold.svg?branch=master)](https://travis-ci.org/mrkkrp/vimish-fold)

This is package to do text folding like in Vim. It has the following
features:

* batteries included: activate minor mode, bind a couple of commands and
  everything will just work;

* it works on regions you select;

* it's persistent: when you close file your folds don't disappear;

* in addition to being persistent, it scales well, you can work on hundreds
  of files with lots of folds without adverse effects;

* it's obvious which parts of text are folded;

* it doesn't break indentation or something;

* it can refold unfolded folds (oh, my);

* for fans of `avy` package: you can use `avy` to fold text with minimal
  number of key strokes!

## Installation

If you would like to install the package manually, download or clone it and
put on Emacs' `load-path`, then you can require it in your init file like
this:

```emacs-lisp
(require 'vimish-fold)
```

It's available via MELPA, so you can just <kbd>M-x package-install RET
vimish-fold RET</kbd>.

## Usage

Create binding for some of/all these functions:

* `vimish-fold` — basic folding of selected region;

* `vimish-fold-unfold` — you don't really need to bind this, just use
  <kbd>C-g</kbd> when point is placed on header representing folded text;

* `vimish-fold-unfold-all` — nevertheless this may be useful;

* `vimish-fold-refold` — you can either bind this command or use
  <kbd>C-g</kbd> to refold regions;

* `vimish-fold-delete` — use it to permanently delete fold;

* `vimish-fold-toggle` — toggle fold;

* `vimish-fold-avy` — use `avy` to fold your text!
* 

The width of the fold headers could be be even width as your current window. Set  `vimish-fold-header-width` to `nil`, and 
 `vimish-fold` will change the width of the fold headers to your current window. 

You can turn `vimish-fold-mode` selectively for modes where you want to have
persistent folding, or simply activate it everywhere:


```emacs-lisp
(vimish-fold-global-mode 1)
```

It's as simple as that.

## Customization

There are a number of customization options that are available via <kbd>M-x
customize-group vimish-fold</kbd>. Everything is carefully documented, as
always.

## License

This work is based on Magnar Sveen's `fold-this` package to some extent, so
I think I should include him as an author, thanks Magnar!

Copyright © 2015 Mark Karpov<br>
Copyright © 2012–2013 Magnar Sveen

Distributed under GNU GPL, version 3.
