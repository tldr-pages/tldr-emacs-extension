# tldr-emacs-extension

## Requirenments

- `tldr-lint` installed
- `emacs 27.1` higher
- `yasnippet` (optional, if snippets must be enabled)

## Features

- [x] Code linting
- [x] Code snippets
- [x] Code actions

![linting](./assets/screenshot.png)

![settings](./assets/settings-screenshot.png)

## Installation

### Copy-paste plugin script contents to `~/.emacs` while enabling lexical-binding

Example `~/.emacs` config:

```emacs
;;; flymake-tldr-lint.el --- A TlDr Flymake backend powered by tldr-lint  -*- lexical-binding: t; -*-
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; The extension file content with all comments removed can be placed here.

(add-hook 'markdown-mode-hook 'flymake-tldr-lint-load)

(custom-set-variables
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
'(package-selected-packages '(markdown-mode)))
(custom-set-faces
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
)
```

## Starting linting

- Use `M-x flymake-mode RET` (view the next chapter below to understand how to interpret `M-x` and `RET`)

## Starting fixing

> ⚠️ `tldr-` prefix was replaced with `flymake-tldr-lint-` in actions.

To speed up fixing TlDr pages several examples provided for each code fix down below:

- Action: `flymake-tldr-lint-remove-broken-ellipsis`
  Input: `command {{...}}`
  Result: `command`
- Action: `flymake-tldr-lint-remove-broken-numbers`
  Input: `command {{path/to/file_1}}`
  Result: `command {{path/to/file}}`
- Action: `flymake-tldr-lint-remove-broken-files`
  Input: `command {{file}}`
  Result: `command`
- Action: `flymake-tldr-lint-remove-broken-directories`
  Input: `command {{dir}}`
  Result: `command`
- Action: `flymake-tldr-lint-correct-broken-ellipsis`
  Input: `command {{path/to/file1}} {{path/to/file2}} {{path/to/file3}}`
  Result: `command {{path/to/file1 path/to/file2 ...}}`
- Action: `flymake-tldr-lint-correct-broken-numbers`
  Input: `command {{path/to/file_1}}`
  Result: `command {{path/to/file1}}`
- Action: `flymake-tldr-lint-correct-broken-files`
  Input: `command {{file}}`
  Result: `command {{path/to/file}}`
- Action: `flymake-tldr-lint-correct-broken-directories`
  Input: `command {{dir}}`
  Result: `command {{path/to/directory}}`
- Action: `flymake-tldr-lint-correct-broken-ranges`
  Input: `command {{1-10}}`
  Result: `command {{1..10}}`
- Action: `flymake-tldr-lint-correct-broken-long-option-argument`
  Input: `command --option {{option}}`
  Result: `command --option {{any}}`
- Action: `flymake-tldr-lint-convert-long-option-space-separated`
  Input: `command --option={{any}}`
  Result: `command --option {{any}}`
- Action: `flymake-tldr-lint-convert-long-option-equal-sign-separated`
  Input: `command --option {{any}}`
  Result: `command --option={{any}}`
- Action: `flymake-tldr-lint-remove-broken-all`
- Action: `flymake-tldr-lint-correct-broken-all`

⚠️ Note that all actions are regex-based substitutions. If you need more smart behaviour use TlDr extensions for another editor.

### Actions in action

For instace we have the following `tar` page:

```md
# tar

> Archiving utility.
> Often combined with a compression method, such as gzip or bzip2.
> More information: <https://www.gnu.org/software/tar>.

- [c]reate an archive and write it to a [f]ile:

`tar cf {{target.tar}} {{file1}} {{file2}} {{file3}}`

- [c]reate a g[z]ipped archive and write it to a [f]ile:

`tar czf {{target.tar.gz}} {{file1}} {{file2}} {{file3}}`

- [c]reate a g[z]ipped archive from a directory using relative paths:

`tar czf {{target.tar.gz}} --directory={{path/to/directory}} .`

- E[x]tract a (compressed) archive [f]ile into the current directory [v]erbosely:

`tar xvf {{source.tar[.gz|.bz2|.xz]}}`

- E[x]tract a (compressed) archive [f]ile into the target directory:

`tar xf {{source.tar[.gz|.bz2|.xz]}} --directory={{path/to/directory}}`

- [c]reate a compressed archive and write it to a [f]ile, using [a]rchive suffix to determine the compression program:

`tar caf {{target.tar.xz}} {{file1}} {{file2}} {{file3}}`

- Lis[t] the contents of a tar [f]ile [v]erbosely:

`tar tvf {{source.tar}}`

- E[x]tract files matching a pattern from an archive [f]ile:

`tar xf {{source.tar}} --wildcards "{{*.html}}"`
```

To fix all fixable issues at once we can use `M-x flymake-tldr-lint-correct-broken-all` command. The result is as follows:

```md
# tar

> Archiving utility.
> Often combined with a compression method, such as gzip or bzip2.
> More information: <https://www.gnu.org/software/tar>.

- [c]reate an archive and write it to a [f]ile:

`tar cf {{target.tar}} {{path/to/file1 path/to/file2 ...}}`

- [c]reate a g[z]ipped archive and write it to a [f]ile:

`tar czf {{target.tar.gz}} {{path/to/file1 path/to/file2 ...}}`

- [c]reate a g[z]ipped archive from a directory using relative paths:

`tar czf {{target.tar.gz}} --directory={{path/to/directory}} .`

- E[x]tract a (compressed) archive [f]ile into the current directory [v]erbosely:

`tar xvf {{source.tar[.gz|.bz2|.xz]}}`

- E[x]tract a (compressed) archive [f]ile into the target directory:

`tar xf {{source.tar[.gz|.bz2|.xz]}} --directory={{path/to/directory}}`

- [c]reate a compressed archive and write it to a [f]ile, using [a]rchive suffix to determine the compression program:

`tar caf {{target.tar.xz}} {{path/to/file1 path/to/file2 ...}}`

- Lis[t] the contents of a tar [f]ile [v]erbosely:

`tar tvf {{source.tar}}`

- E[x]tract files matching a pattern from an archive [f]ile:

`tar xf {{source.tar}} --wildcards "{{*.html}}"`
```

`{{target.tar}}` placeholder and similar ones were not fixed as extension didn't recognized them as path placeholders.

## Settings

To change settings `M-x customize-option RET {{flymake-tldr-lint-program|flymake-tldr-lint-ignored}} RET` can be used where:

- `M-x` is `Alt` with `x`
- `RET` is `Enter`.

Settings:

- `flymake-tldr-lint-program` (**default**: `"tldr-lint"`) - executable name
- `flymake-tldr-lint-ignored` (**default**: `""`) - list of ignored errors (as a delimiter any character can be used `TLDR004 TLDR006` or `TLDR004,TLDR006`)

## FAQ

- Can I remove broken placeholders automatically?
  This can be done via all `flymake-tldr-lint-remove-broken-*` actions.
- Can I fix broken placeholders automatically?
  This can be done via all `flymake-tldr-lint-correct-broken-*` actions.
