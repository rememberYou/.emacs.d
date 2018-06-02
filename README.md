![GNU Emacs Logo](assets/emacs.png "GNU Emacs logo") GNU Emacs Configuration
===============================

This repository contains all my GNU Emacs configuration.

I wanted to make this configuration simple, powerful and ergonomic by choosing
the most trivial keyboard possible.

List of files:

* [`config.org`](https://github.com/rememberYou/.emacs.d/blob/master/config.org/):
  main configuration file.

* [`init.el`](https://github.com/rememberYou/.emacs.d/blob/master/init.el/):
  load the generated configuration file.

--------------------

### Getting Started ###

If you are new to GNU Emacs, I advise you to take code blocks from the
[`config.org`](https://github.com/rememberYou/.emacs.d/blob/master/config.org/)
file for your own configuration file, and evaluate them with `eval-last-sepx`
(`C-x C-e`). Be careful to take pairs of parentheses.

For the most curious, you can test my complete configuration with:

	git clone --recursive https://github.com/rememberYou/.emacs.d

**NOTE:** the first time GNU Emacs starts, it will install additional packages
that will be better managed by the package manager.

--------------------

### TODO ###

Here is a list of tasks I intend to do in my spare time to improve this configuration:

*  Show images on `erc` in the center of the buffer with `magic-buffer`.
*  Show images on `erc` only if they are not large with `imagemagick`.

--------------------

### Contributions ###

Various functions may be optimized or spelling errors may occur. If you want to
make your own correction on this configuration, you are free to do so.

--------------------

### License ###

The code is not licensed, take what you like and hope that this configuration
can be so useful to you that it is for me.

GNU Emacs is above all a concept of sharing in order to facilitate our daily life.
