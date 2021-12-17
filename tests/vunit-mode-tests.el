;;; vunit-mode.el --- VUnit Runner Interface -*- lexical-binding: t -*-

;; Copyright (C) 2021  Lukas Lichtl <support@embed-me.com>

;; Author: Lukas Lichtl <support@embed-me.com>
;; URL: https://github.com/embed-me
;; Version: 1.0
;; Package-Requires: ((hydra "0.14.0")(emacs "24.3"))
;; Keywords: VUnit, Python, tools

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Some basic tests for vunit-mode.

;;; Code:

(require 'vunit-mode)
(require 'ert)
(require 'ert-x)

(ert-deftest vunit-flags-check ()
  (should (equal vunit-flags '()))
  (vunit-toggle-flag "--gui")
  (should (equal vunit-flags '("--gui")))
  (vunit-toggle-flag "--verbose")
  (should (equal vunit-flags '("--verbose" "--gui")))
  (vunit-toggle-flag "--verbose")
  (should (equal vunit-flags '("--gui")))
  (vunit-toggle-flag "--gui")
  (should (equal vunit-flags '())))

(ert-deftest vunit-sim-cursor-check ()
  (should (vunit--regex-testcase "run (\"foo\")"))
  (should (vunit--regex-testcase "run(\"foo\")"))
  (should (equal (vunit--regex-testcase "runF (\"foo\")") nil))
  (should (equal (vunit--regex-testcase "whatever (\"foo\")") nil))
  (should (equal (vunit--regex-testcase "whatever some more") nil)))

(provide 'vunit-mode-tests)

;;; vunit-mode-tests.el ends here
