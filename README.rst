=============
UnitTest mode
=============

This is a minor mode enabling running python unit tests within
emacs. It is intended to be used alongside python-mode in emacs.

To install, put ``unittest.el`` on you emacs load path and add the following to the init file::

    (require 'unittest)
    (add-hook 'python-mode-hook 'unittest-mode)
