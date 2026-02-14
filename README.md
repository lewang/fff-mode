# fff-mode

Fat Finger Forgiveness for Emacs -- auto-handle buffers with trivially small accidental modifications.

Two annoyances when you accidentally hit a key in a buffer backed by a file:

1. **auto-revert-mode** skips the buffer because it's "modified," even though the modification was an accidental
   keypress. The file on disk has changed but Emacs won't pick it up until you manually revert.
2. **save-some-buffers** (C-x s, or triggered by compile/grep) nags you to save the fat-fingered buffer. You have to
   dismiss the prompt every time.

`fff-mode` detects trivially small modifications (configurable thresholds) and tells auto-revert to proceed and
save-some-buffers to skip the prompt.

## Installation

With [elpaca](https://github.com/progfolio/elpaca) + use-package:

```elisp
(use-package fff-mode
  :ensure (:host github :repo "lewang/fff-mode")
  :init (fff-mode 1))
```

## Customization

| Variable | Default | Description |
|---|---|---|
| `fff-max-characters` | 4 | Max total characters inserted+deleted to qualify as fat-fingered |
| `fff-max-changes` | 1 | Max distinct change groups (undo boundaries) to qualify |
| `fff-features` | `(auto-revert save-some-buffers)` | Which features to enable |

## How it works

**auto-revert path**: `fff-mode` installs a custom `buffer-stale-function`. When the default function says "not stale"
(because the buffer is modified), the custom function checks whether the file changed on disk AND the buffer
modifications are below the thresholds. If so, it reports the buffer as stale and auto-revert proceeds normally.

**save-some-buffers path**: `fff-mode` sets `save-some-buffers-default-predicate` to a function that returns nil for
fat-fingered buffers, causing `save-some-buffers` to skip the save prompt for them.

## License

GPL-3.0-or-later
