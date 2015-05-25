(setq toplevel-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat toplevel-dir "/vendor/git-emacs"))
(setq git-state-modeline-decoration 'git-state-decoration-large-dot)
(require 'git-emacs)
