# fff-mode

Fat Finger Forgiveness for Emacs.

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

**magit path**: Magit saves modified repository buffers before running git commands (`magit-save-repository-buffers`),
but it passes its own predicate to `save-some-buffers`, bypassing `save-some-buffers-default-predicate` entirely.
`fff-mode` replaces `magit-save-repository-buffers-predicate` with a named function that applies the same fat-finger
filter used by the save-some-buffers path, then delegates to the original predicate for repo-scoping. The two concerns
are orthogonal: the fat-finger check is buffer-local; the original predicate handles repository membership via
`topdir`. The integration is deferred via `eval-after-load` when magit isn't yet loaded, so load order doesn't matter.

**Note**: `save-buffers-kill-emacs` (`C-x C-c`) intentionally bypasses `save-some-buffers-default-predicate` — Emacs
wants to be extra cautious when quitting. You will still be prompted about fat-fingered buffers on exit, which is a
reasonable safety net.

## Prior art

- **[unmodified-buffer](https://github.com/arthurcgusmao/unmodified-buffer)** — shells out to `diff -q` to detect
  edits that cancel out. Complementary: it clears the modified flag when content matches disk, but doesn't hook into
  `buffer-stale-function` or `save-some-buffers-default-predicate`.
- **revert-buffer-all** — brute-force revert all buffers; no threshold logic.

What's novel here: undo-list introspection with configurable thresholds (no disk I/O), `buffer-stale-function`
override for auto-revert, and a single fat-finger predicate shared by both `save-some-buffers-default-predicate` and
`magit-save-repository-buffers-predicate`. No advice is used — everything works through dedicated extension points.

## License

GPL-3.0-or-later
