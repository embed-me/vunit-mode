# VUnit Mode [![check][check-badge]][check-link]

This is a package for GNU Emacs that can be used to interface with
[VUnit](https://vunit.github.io/) which is an open-source testing
framework for VHDL/SystemVerilog.

It hooks to the VHDL mode and wraps the function calls to
the run script used by VUnit, therefore allows verification of unit
tests while writing code without ever leaving Emacs.
At the moment, it is in a very early stage of development where it
simply makes use of the compile package and hydra, but in the near
future and with some support from the community, the idea is to
implement something similar to Magit for VUnit.

![](media/animation.gif)

## Prerequisites

The basic requirements are that the HDL simulator 
(eg. [Modelsim](https://eda.sw.siemens.com/en-US/ic/modelsim/))
is in the path and that VUnit is installed in the python environment.

## Installation

### MELPA

Currently, the mode is not available from [MELPA](https://melpa.org/).

### Manual

To manually install the package, clone this repository and add the
following in your `~/.emacs` or `~/.emacs.d/init.el` init file.

```elisp
(add-to-list 'load-path "/path-to-repository/")
(require 'vunit-mode)
```

## Configuration

As for now, it is possible to configure the following variables:

Path to the VUnit directory.
> vunit-path

The Python executable used by VUnit.
> vunit-python-executable

Name of the VUnit output directory.
> vunit-run-outdir

Name of the python script to run.
> vunit-run-script

Number of threads to use in parallel.
> vunit-num-threads

## Contributing

Since this is my first application ever written in Emacs Lisp
support and input from advanced developers is highly appreciated.

To report bugs and suggest new features please use the
[issue tracker](https://github.com/embed-me/vunit-mode/issues).
In order to merge some code, please open a
[pull request](https://github.com/embed-me/vunit-mode/pulls).

## Acknowledgments

The idea for a VUnit Emacs mode seemed to be around for a while and was
already discussed online a couple of years ago, however,
for me, Markus Pfaff from FH-Hagenberg was the one who inspired me to
take the implementation into my own hands and despite my lack of knowledge
in elisp publish the code written so far.


[check-link]: https://github.com/embed-me/vunit-mode/actions
[check-badge]: https://github.com/embed-me/vunit-mode/actions/workflows/github-actions-ci.yml/badge.svg