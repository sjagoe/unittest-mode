=============
UnitTest mode
=============

This is a minor mode enabling running python unit tests within
emacs. It is intended to be used alongside python-mode in emacs.

To install, put ``unittest.el`` on you emacs load path and add the following to the init file::

    (require 'unittest)
    (add-hook 'python-mode-hook 'unittest-mode)


Using in Windows
================

To use this in Windows, you need to ``customize`` the the
``unittest-shell-exec`` variable to have the (exact) value of ``cmd /S
/C``.  This will make scripts and tests execute under a cmd.exe
subshell, and set up ``unittest.el`` to correctly quote paths with
space, and cmd.exe to correctly parse the quoting.
