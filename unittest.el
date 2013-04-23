;;; unittest.el --- Minor mode for running Python unit tests

;; Copyright 2013 Simon Jagoe

;; Author: Simon Jagoe <simon@simonjagoe.com>
;; URL: http://github.com/sjagoe/unittest-mode
;; Version: 0.2.0

(require 'cl)
(require 'compile)
(require 'python)


(defgroup unittest nil
  "Run Python unit tests in a compilation-mode buffer"
  :prefix "unittest-"
  :group 'tools)


(defcustom unittest-shell-exec
  "bash -c"
  "Command to execute under a shell, or nil for no shell"
  :group 'unittest
  :type 'string)


;; use Python's unbuffered stdout option so that output is displayed
;; immediately in the compilation-mode buffer
(defcustom unittest-python-command
  "python -u"
  "Command to execute Python"
  :group 'unittest
  :type 'string)


(defcustom unittest-last-executed-module
  nil
  "Dotted module name of the last executed module"
  :group 'unittest
  :type 'string)


(defcustom unittest-run-tests-command
  "python -u -m unittest"
  "Command used to run tests"
  :group 'unittest
  :type 'string)


(defcustom unittest-discover-tests-command
  "python -u -m unittest discover"
  "Command used to discover run tests"
  :group 'unittest
  :type 'string)


(defcustom unittest-discover-module
  nil
  "Dotted module name of where to look for tests"
  :group 'unittest
  :type 'string)


(defcustom unittest-run-tests-directory
  nil
  "Default working directory in which to run
`unittest-run-tests-command' and
`unittest-discover-tests-command'."
  :group 'unittest
  :type 'string)


(defcustom unittest-remember-tests-directory
  t
  "Remember the directory from which to run tests, or prompt
every time?"
  :group 'unittest
  :type 'boolean)


(defvar unittest-indent-str "    ")


(defun how-many-str (regexp str)
  (loop with start = 0
        for count from 0
        while (string-match regexp str start)
        do (setq start (match-end 0))
        finally return count))


(defun unittest-match-defun-name ()
  (save-excursion
    (beginning-of-defun)
    (let ((def-line (thing-at-point 'line)))
      (if (string-match
           (rx (and (submatch (0+ "    ")) (submatch (or "def" "class")) " "
                    (submatch (in "a-zA-Z_") (0+ (in "a-zA-Z_0-9"))) "("))
           def-line)
          def-line))))


(defun unittest-get-defun-name (def-line)
  (match-string-no-properties 3 def-line))


(defun unittest-defun-is-class (def-line)
  (string= (match-string-no-properties 2 def-line) "class"))


(defun unittest-defun-is-function (def-line)
  (string= (match-string-no-properties 2 def-line) "def"))


(defun unittest-get-enclosing-scope-name ()
  "FIXME: (Really) Dirty way of finding name of enclosing scope"
  (save-excursion
    (let ((this-scope-defun (unittest-match-defun-name)))
      (if this-scope-defun
          (let ((indent-level (how-many-str
                               unittest-indent-str
                               (match-string-no-properties 1 this-scope-defun)))
                (outer-scope-defun this-scope-defun))
            (if (> indent-level 0)
                (loop for count from 0
                      while (and outer-scope-defun
                                 (<= indent-level (how-many-str unittest-indent-str
                                                                   outer-scope-defun)))
                      do (loop for inner-count from 0
                               while (>= count inner-count)
                               do (beginning-of-defun)
                               finally return (setq outer-scope-defun (unittest-match-defun-name)))
                      finally return (if outer-scope-defun
                                         (unittest-get-defun-name outer-scope-defun)))))))))


(defun unittest-get-class-function-name ()
  (let ((def-line (unittest-match-defun-name)))
    (if def-line
        (let ((function-name (unittest-get-defun-name def-line)))
          (if (unittest-defun-is-function def-line)
              (let ((enclosing-scope (unittest-get-enclosing-scope-name)))
                (if enclosing-scope
                    (if (string-match (rx bos "test_") function-name)
                        (concat enclosing-scope "." function-name)
                      enclosing-scope)
                  function-name))
            function-name)))))


(define-compilation-mode unittest-output-mode "unittest"
  "A compilation buffer for Python unittest")


(define-compilation-mode python-exec-mode "python-exec"
  "A compilation buffer for Python scripts"
  (setq buffer-read-only nil)
  (with-no-warnings (inferior-python-mode)))


(defun verbose-cmd (cmd verbose)
  "Returns the command used to execute unit tests"
  (let ((verbose-flag
         (if verbose
             " -v"
           "")))
    (concat cmd verbose-flag)))


(defun unittest-shell-exec-quote-filename (filename)
  (if (string= unittest-shell-exec "cmd /S /C")
      (concat "\"" filename "\"")
    filename))


(defun unittest-shell-exec-command (command)
  (if unittest-shell-exec
      (concat unittest-shell-exec " \"" command "\"")
    command))


(defun run-in-compile (command &optional mode)
  (let ((mode (if mode mode 'unittest-output-mode)))
    (compilation-start
     (unittest-shell-exec-command command)
     mode)))


(defun run-in-shell (command)
  (run-in-compile command 'python-exec-mode))


(defun unittest-get-test-file-name ()
  "Returns the full path the the test file for the current module"
  (let ((dirname (file-name-directory (buffer-file-name))))
    (let ((filename (file-name-nondirectory (buffer-file-name))))
      (let ((python-arg
             (if (string-match (rx bos "test_") filename)
                 (buffer-file-name)
               (concat dirname "tests/test_" filename))))
        python-arg))))


(defun unittest-run-test-case (tests-dir)
  "Executes a test case file"
  (interactive (list (read-directory-name "Run tests in: " (if (string= unittest-run-tests-directory nil)
                                                               (unittest-setup-py-directory)
                                                             unittest-run-tests-directory))))
  (let ((verbose current-prefix-arg)
        (default-directory tests-dir)
        (python-arg (unittest-current-module t (unittest-get-test-file-name))))
    (run-in-compile
     (concat (unittest-unittest-run-cmd verbose) " " python-arg ))))


;; (defun unittest-run-test-case (verbose)
;;   "Executes a test case file"
;;   (interactive "P")
;;   (let ((python-arg (unittest-shell-exec-quote-filename (unittest-get-test-file-name))))
;;     (run-in-compile
;;      (verbose-cmd (concat unittest-python-command " " python-arg ) verbose))))


(defun unittest-run-single-test (verbose)
  "Executes the test under point. If point is not in a single
test, the test case will be executed. If point is not in a test,
all tests for the module are run."
  (interactive "P")
  (let ((python-arg (unittest-shell-exec-quote-filename (unittest-get-test-file-name))))
    (let ((test-cmd (verbose-cmd (concat unittest-python-command " " python-arg) verbose)))
      (run-in-compile
       (concat test-cmd " " (unittest-get-class-function-name))))))


(defun unittest-setup-py-directory ()
  "Returns the directory containing a setup.py file"
  (let ((directory (file-name-directory (buffer-file-name))))
    (loop while (not (file-exists-p (concat directory "setup.py")))
          do (setq directory (file-name-directory
                              (directory-file-name
                               (file-name-directory directory))))
          finally return directory)))


(defun unittest-current-module (include-module &optional start)
  "Returns the dotted module name of the current package"
  (interactive "P")
  (let ((start (if (string= start nil)
                   (buffer-file-name)
                 start)))
    (let ((directory (file-name-directory start))
          (module-name (if include-module
                           (file-name-base start)
                         nil)))
      (let ((return-value (loop while (file-exists-p (concat directory "__init__.py"))
                                do (let ((directory-name (directory-file-name (file-name-directory directory))))
                                     (let ((module-part (file-name-nondirectory directory-name)))
                                       (setq module-name (if (string= module-name nil)
                                                             module-part
                                                           (concat module-part "." module-name))))
                                     (setq directory (file-name-directory directory-name)))
                                finally return module-name)))
        (if (string= nil module-name)
            (error (concat start " is not in a package!"))
          module-name)))))


(defun unittest-unittest-discover-cmd (verbose)
  "Returns the command used to execute unit tests"
  (verbose-cmd unittest-discover-tests-command verbose))


(defun unittest-unittest-run-cmd (verbose)
  "Returns the command used to execute unit tests"
  (verbose-cmd unittest-run-tests-command verbose))


(defun unittest-run-tests-top-level (tests-dir)
  "Executes \"python -m unittest discover\" in the selected directory"
  (interactive (list (read-directory-name "Run tests in: " (if (string= unittest-run-tests-directory nil)
                                                               (unittest-setup-py-directory)
                                                             unittest-run-tests-directory))))
  (progn
    (if unittest-remember-tests-directory
        (customize-save-variable 'unittest-run-tests-directory tests-dir))
    (let ((verbose current-prefix-arg)
          (default-directory tests-dir)
          (package (car (split-string (unittest-current-module nil) "\\."))))
      (run-in-compile (concat (unittest-unittest-discover-cmd verbose) " " package)))))


;; (defun unittest-run-tests-in-directory (tests-dir)
;;   "Executes \"python -m unittest discover\" in the selected directory"
;;   (interactive (list (read-directory-name "Run tests in: " (unittest-setup-py-directory))))
;;   (let ((verbose current-prefix-arg)
;;         (default-directory tests-dir))
;;     (run-in-compile (unittest-unittest-discover-cmd verbose))))


(defun unittest-run-tests-in-current-package (tests-dir)
  "Executes \"python -m unittest discover\" for the current buffer's package"
  (interactive (list (read-directory-name "Run tests in: " (if (string= unittest-run-tests-directory nil)
                                                               (unittest-setup-py-directory)
                                                             unittest-run-tests-directory))))
  (let ((verbose current-prefix-arg)
        (default-directory tests-dir)
        (package (unittest-current-module nil)))
    (run-in-compile (concat (unittest-unittest-discover-cmd verbose) " " package))))


;; (defun unittest-run-tests-in-current-directory (verbose)
;;   "Executes \"python -m unittest discover\" in the current buffer's directory"
;;   (interactive "P")
;;   (let ((default-directory (file-name-directory (directory-file-name (buffer-file-name)))))
;;     (run-in-compile (unittest-unittest-discover-cmd verbose))))


(defun unittest-execute-current-file ()
  "Executes the current buffer"
  (interactive)
  (let ((file-to-run (unittest-shell-exec-quote-filename (buffer-file-name))))
    (run-in-shell (concat unittest-python-command " " file-to-run))))


(defun unittest-execute-module-file (module-file)
  "Takes a path to a python module, relative to the packages
directory, turns it into a dotted package name and executes with
'python -m'.

e.g. foo/bar will be executed as 'python -m foo.bar'"
  (let ((package-module (replace-regexp-in-string "/" "." module-file)))
    (progn
      (customize-save-variable 'unittest-last-executed-module package-module)
      (let ((default-directory "/"))
        (run-in-shell (concat unittest-python-command " -m " package-module))))))


(defun unittest-execute-last-module ()
  (interactive)
  (let ((default-directory "/"))
    (if (not (string= unittest-last-executed-module nil))
        (run-in-shell (concat unittest-python-command " -m " unittest-last-executed-module))
      (error "No last module set"))))


(defun unittest-execute-current-module ()
  "Executes the current buffer as python -m package.module.

e.g. /home/simon/packages/some-package/foo/bar.py will be
executed as 'python -m foo.bar', assuming the sop-level is in
some-package (as determined by a setup.py file)"
  (interactive)
  (let ((top-level (unittest-setup-py-directory)))
    (let ((module-file (file-name-sans-extension
                        (file-relative-name (buffer-file-name)
                                            top-level))))
      (if (string= (file-name-nondirectory module-file) "__main__")
          (let ((package-directory (file-name-directory module-file)))
            (let ((module-file (directory-file-name package-directory)))
              (if (not (string= module-file ""))
                  (unittest-execute-module-file module-file)
                (error (concat "No python package found at " package-directory)))))
        (unittest-execute-module-file module-file)))))


(defvar unittest-mode-map
  (let ((map (make-sparse-keymap))
        (pmap (make-sparse-keymap)))
    (define-key pmap "r" 'unittest-execute-current-file)
    (define-key pmap "m" 'unittest-execute-current-module)
    (define-key pmap "l" 'unittest-execute-last-module)
    (define-key pmap "f" 'unittest-run-test-case)
    (define-key pmap "s" 'unittest-run-single-test)
    (define-key pmap "t" 'unittest-run-tests-top-level)
    (define-key pmap "d" 'unittest-run-tests-in-current-package)
    (define-key map (kbd "C-x t") pmap)
    map)
  "Keymap of `unittest-mode'.")


;;;###autoload
(define-minor-mode unittest-mode
  "Minor mode allowing execution of Python unit tests

\\{unittest-mode-map}"
  :init-value nil
  :keymap unittest-mode-map
  :lighter nil
  :group 'unittest
  :require 'unittest)


(provide 'unittest)

;;; unittest.el ends here
