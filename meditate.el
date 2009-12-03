;;; meditate.el --- talk to ZenMOO and optionally make emacs meditate for you

;; Copyright (C) 1993, 1994, 1996 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: meditation, oink
;; Created: 1992-10-??

;; $Id: meditate.el,v 1.7 1996/06/13 21:19:50 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:
;;; Code:

;; yow.el in emacs 18 doesn't have a provide.
(or (fboundp 'yow) (load "yow"))

;;;###autoload
(defvar meditate-buffer-name "*meditation*"
  "*Basic buffer name for meditations.")

;;;###autoload
(defvar meditate-hostname "zenmoo.zennet.com"
  "*Host where ZenMOO is located.")

;;;###autoload
(defvar meditate-port 7777
  "*Port number for ZenMOO.")

;;;###autoload
(defvar meditate-autoreply t
  "*If non-nil, attempt to autoreply when appropriate.")


;; Turn this into an alist in which the cdr of each sublist contains cells like
;; ((ignore (string list ...) (response . thunk)))
;; This way you can handle cases like "As part of a test, you must include
;; the word "shastras" in your next answer."  And you can selectively test
;; for ignore regexps only in cases where you know a particular regexp is
;; too general.

;; A possibly better alternative to using a regexp list would be to time
;; the output.  It turns out that ZenMOO spits out mantras at a consistent
;; time interval, and questions where is expects a response appear at
;; inconsistent intervals.  The only obstacle to making this work in emacs
;; is that the process filter may be delayed getting called become some
;; non-interruptable operating was occuring.
;;;###autoload
(defvar meditate-regexp-list
  '(".* is most unlike *\\. *\\. *\\. *$"
    ".* is to .* as .* is to *\\. *\\. *\\. *$"
    ".* said you .*reply, telling them.*"
    ".*respond or snooze\\.$"
    "[^!]\\?$"
    "\\benter\\b.*[.:]$"
    "ask i you[ .]*\\.$"
    "do tell "
    "enlighten me .*"
    "enter .* for me\\.$"
    "enter .*, if you will *\\. *\\. *\\.$"
    "explain .*, when .*\\.$"
    "fill in the blanks for \".*\""
    "if .*, show me .*\\.$"
    "please .*:$"
    "please \\(enter\\|type\\|key\\) .*\\.[ .]*$"
    "pontificate on"
    "tell me .*[.:]$"
    "type .* for me\\.$"
    "type .*[.:]$")
  "*Case-insensitive regexp of lines from ZenMOO which require a response.")
;(makunbound 'meditate-regexp-list)


;; ".*\\bMOO\\b.*\\?$"
;; Apparently the above regexp is too general; sometimes real questions
;; appear with the string `MOO' in them because zenmoo recycles people's
;; answers.  If someone answered a question and used the string MOO in it,
;; it may appear as a valid question to someone else later.

;;;###autoload
(defvar meditate-ignore-regexp-list
  '("what is the sound of one moo running\\?"
    ;; This occurs up in the opening message.
    "please read the following URL:$")
  "*List of regexps matching \"rhetorical questions\".
Some questions ZenMOO asks aren't questions at all.  Since the regexps in
meditate-regexp-alist may be overly general to catch most instances, this
regexp list serves as a filter to remove those pathological cases.")

(defvar meditate-mode-map nil
  "Sparse keymap for meditate-mode")

;;;###autoload
(defun meditate-mode ()
  "Basically just like text mode, except for the following keybindings:

\\[mediate-mode-map]"
  (interactive)
  (let ((text-mode-hook nil))
    (text-mode))
  (setq mode-name "meditate")
  (setq major-mode 'meditate-mode)
  (setq mode-line-process '(":%s"))
  (make-local-variable 'scroll-step)
  (setq scroll-step 1)
  (make-local-variable 'case-fold-search)
  (setq case-fold-search t)
  (or meditate-mode-map
      (progn
        (setq meditate-mode-map (make-sparse-keymap))
        (define-key meditate-mode-map "\n" 'meditate-send-line)
        (define-key meditate-mode-map "\C-m" 'meditate-send-line)))
  (use-local-map meditate-mode-map)
  (run-hooks 'meditate-mode-hook))

;;;###autoload
(defun meditate ()
  "Oink."
  (interactive)
  (let ((meditate-buffer (generate-new-buffer meditate-buffer-name)))
    (switch-to-buffer meditate-buffer)
    (set (make-local-variable 'meditate-process)
         (open-network-stream "meditate"
                              meditate-buffer
                              meditate-hostname
                              meditate-port))
    (set-marker (process-mark meditate-process) (point-max))
    (set-process-filter meditate-process 'meditate-filter)
    (set-process-sentinel meditate-process 'meditate-sentinel)
    (meditate-mode)))

(defun meditate-filter (proc string)
  (let ((orig-buffer (current-buffer))
        proc-mark
        region-begin
        window
        current-point-mark)
    (unwind-protect
        (progn
          (set-buffer (process-buffer proc))
          (setq proc-mark (process-mark proc))
          (setq region-begin (marker-position proc-mark))
          (setq current-point-mark (set-marker (make-marker) (point)))

          ;; If process mark is at window start, insert-before-markers will
          ;; insert text off-window since it's also inserting before the start
          ;; window mark.  Make sure we can see the most recent text.
          (setq window (and (= proc-mark (window-start))
                            (get-buffer-window (current-buffer))))
          (goto-char proc-mark)
          (insert-before-markers string)
          (goto-char region-begin)
          (while (search-forward "\C-m" proc-mark 'goto-end)
            (delete-char -1))
          (goto-char current-point-mark)
          (and meditate-autoreply
               (meditate-maybe-autoreply proc
                                         (buffer-substring region-begin
                                                           proc-mark)))
          (and window
               (>= (window-start window) region-begin)
               (set-window-start window region-begin 'noforce)))
      (set-buffer orig-buffer))))

(defun meditate-maybe-autoreply (proc string)
  (let ((case-fold-search t)
        (orig-match-data (match-data))
        (regexp-list meditate-regexp-list)
        regexp
        (current-point-mark nil)
        (found nil)
        (ifound nil))
    (unwind-protect
        (while (and regexp-list (not found))
          (setq regexp (car regexp-list)
                regexp-list (cdr regexp-list))
          (and (string-match regexp string)
               (progn
                 (setq found t)

                 (setq regexp-list meditate-ignore-regexp-list)
                 (while (and regexp-list (not ifound))
                   (setq regexp (car regexp-list)
                         regexp-list (cdr regexp-list))
                   (and (string-match regexp string)
                        (setq ifound t)))

                 ;; No need to compare `found'; we know it's t.
                 (and (not ifound)
                      (let ((proc-mark (process-mark proc))
                            (yow-line (format "%s" (yow)))
                            region-begin)
                        (setq current-point-mark
                              (set-marker (make-marker) (point)))
                        (goto-char proc-mark)
                        (setq region-begin (point))
                        (insert-before-markers yow-line)
                        (subst-char-in-region region-begin proc-mark
                                              ?\C-j ?\  'noundo)
                        (goto-char proc-mark)
                        (insert-before-markers "\n")
                        (setq yow-line
                              (buffer-substring region-begin proc-mark))
                        (process-send-string proc yow-line))))))
      (store-match-data orig-match-data)
      (and current-point-mark
           (goto-char current-point-mark)))))

(defun meditate-sentinel (proc event)
  (save-excursion
    (set-buffer (process-buffer proc))
    (insert (format "Process %s: %s" proc event)))
  (and (eq 'closed (process-status proc))
       (delete-process proc)))

;;;###autoload
(defun meditate-send-line ()
  "Send current line to ZenMOO."
  (interactive)
  (insert "\n")
  (let* ((proc (get-buffer-process (current-buffer)))
         (string (buffer-substring (marker-position (process-mark proc))
                                   (point))))
    (set-marker (process-mark proc) (point))
    (process-send-string proc string)))

(provide 'meditate)

;;; meditate.el ends here
