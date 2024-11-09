;;; -*- lexical-binding: t -*-

;; Author:  yangxue.cs@foxmail.com
;; Created: 2024-11-09 01:12:13
;; License: GPLv3

;;; Commentary:

;; 

;;; Code:
(require 'gptel)
(require 'thingatpt)

(defvar gptel-ext--history nil)

(defvar gptel-ext--word-count 18)

(defconst gptel-ext--commit-sys-role
  "You are a senior programmer assistant living in Emacs. The user provides the result of running `git diff --cached`. You suggest a conventional commit message. Don't add anything else to the response. The following describes conventional commits.

# Conventional Commits 1.0.0

## Summary

The Conventional Commits specification is a lightweight convention on top of commit messages. It provides an easy set of rules for creating an explicit commit history, which makes it easier to write automated tools on top of. This convention dovetails with [SemVer](http://semver.org), by describing the features, fixes, and breaking changes made in commit messages.

The commit message should be structured as follows:

---

```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```
---

<br />
The commit contains the following structural elements, to communicate intent to the consumers of your library:

1. **fix:** a commit of the _type_ `fix` patches a bug in your codebase (this correlates with [`PATCH`](http://semver.org/#summary) in Semantic Versioning).
2. **feat:** a commit of the _type_ `feat` introduces a new feature to the codebase (this correlates with [`MINOR`](http://semver.org/#summary) in Semantic Versioning).
3. **BREAKING CHANGE:** a commit that has a footer `BREAKING CHANGE:`, or appends a `!` after the type/scope, introduces a breaking API change (correlating with [`MAJOR`](http://semver.org/#summary) in Semantic Versioning). A BREAKING CHANGE can be part of commits of any _type_.
4. _types_ other than `fix:` and `feat:` are allowed, for example [@commitlint/config-conventional](https://github.com/conventional-changelog/commitlint/tree/master/%40commitlint/config-conventional) (based on the [Angular convention](https://github.com/angular/angular/blob/22b96b9/CONTRIBUTING.md#-commit-message-guidelines)) recommends `build:`, `chore:`, `ci:`, `docs:`, `style:`, `refactor:`, `perf:`, `test:`, and others.
5. _footers_ other than `BREAKING CHANGE: <description>` may be provided and follow a convention similar to [git trailer format](https://git-scm.com/docs/git-interpret-trailers).

Additional types are not mandated by the Conventional Commits specification, and have no implicit effect in Semantic Versioning (unless they include a BREAKING CHANGE).
<br /><br />
A scope may be provided to a commit's type, to provide additional contextual information and is contained within parenthesis, e.g., `feat(parser): add ability to parse arrays`.")

(defun gptel-ext--callback-1 (response info)
  "Show RESPONSE in the echo area."
  (if response
      (message "ChatGPT: %s" response)
    (message "gptel failed with message: %s" (plist-get info :status))))

(defun gptel-ext--callback-2 (response info)
  (if response
      (with-current-buffer (get-buffer-create "*gptel-ext*")
        (let ((inhibit-read-only t))
            (erase-buffer)
            (insert response))
        (special-mode)
        (goto-char (point-min))
        (display-buffer (current-buffer)))
    (message "gptel-ext failed with message: %s" (plist-get info :status))))

;;;###autoload
(defun gptel-quick (prompt)
    (interactive (list (read-string "Prompts: " nil gptel-ext--history)))
    (when (string= prompt "") (user-error "A prompt is required."))
    (gptel-request prompt
      :system "You are a large language model living in Emacs and a helpful assistant.Respond succinctly and concisely."
      :callback #'gptel-ext--callback-2))

;;;###autoload
(defun gptel-commit ()
  "Generate commit message with gptel and insert it into the buffer."
  (interactive)
  (let* ((lines (magit-git-lines "diff" "--cached"))
         (changes (string-join lines "\n")))
    (gptel-request changes
      :system gptel-ext--commit-sys-role)))

;;;###autoload
(defun gptel-explain (query-text &optional count)
  "Explain or summarize region or thing at point with an LLM.

QUERY-TEXT is the text being explained.  COUNT is the approximate word count of the response."
  (interactive
   (list (cond
          ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
          (t (thing-at-point 'sexp)))
         current-prefix-arg))
  (let* ((count (or count gptel-ext--word-count)))
    (gptel-request query-text
      :system (format "Explain in %d words or fewer." count)
      :context (list query-text count
                     (posn-at-point (and (use-region-p) (region-beginning))))
      :callback #'gptel-ext--callback-1)))

;;;###autoload
(defun gptel-translate (query-text)
  "Translate region or thing at point."
  (interactive
   (list (cond
          ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
          (t (thing-at-point 'word)))))
  (let ((trans-prompt (format "Please translate the following into each other's language (only the translated result will be returned)：\n%s" query-text)))
    (gptel-request trans-prompt
      :system "You are a translator, well versed in translating between Chinese and English."
      :callback #'gptel-ext--callback-1)))


(provide 'gptel-ext)
;;; gptel-ext.el ends here
