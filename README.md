# tldr-emacs-extension

## Requirenments

- `tldr-lint` installed
- `emacs 27.1` higher

## Features

- [x] Code linting
- [ ] Code snippets
- [ ] Code actions

![image](https://user-images.githubusercontent.com/42812113/207983063-03efd5da-eed9-4c52-8913-8ae2e0a95a9f.png)

## Installation

- Copy-paste `.el` contents to `~/.emacs` while enabling `-*- lexical-binding: t`

## Settings

- `flymake-tldr-lint-program` (**default**: `tldr-lint`) - path to tldr-lint executable (useful in case it's not in `$PATH`)
- `flymake-tldr-lint-use-file` (**default**: `nil`) - whether to check file contents instead of buffer ones
