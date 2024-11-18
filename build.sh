#!/usr/bin/env sh
CI=${CI:-true} emacs -Q --batch -l ./build-site.el --funcall mtr/build-site
