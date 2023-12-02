(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-refresh-contents)

(use-package evil
  :config
  (evil-mode 1)
  :ensure t)
(use-package tuareg
  :ensure t)
(use-package modus-themes
  :ensure t)
(use-package magit
  :ensure t)
(use-package treemacs
  :ensure t)
(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.blade.php\\'" . web-mode)))

(setq indent-tabs-mode nil)
(setq tab-width 4)

;; For ocamlrpc
(add-to-list 'load-path "/home/andrija/.opam/5.1.0/share/emacs/site-lisp")
     (require 'ocp-indent)

(keymap-global-set "C-c t c" #'treemacs-select-directory)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-operandi-tinted))
 '(custom-safe-themes
   '("3d94d6d1a1c23113a60c8496c9aed094dbc2695f219e8127bb168d17b1e6dab3" "15604b083d03519b0c2ed7b32da6d7b2dc2f6630bef62608def60cdcf9216184" "88cb0f9c0c11dbb4c26a628d35eb9239d1cf580cfd28e332e654e7f58b4e721b" "69f7e8101867cfac410e88140f8c51b4433b93680901bb0b52014144366a08c8" "611ef0918b8b413badb8055089b5499c1d4ac20f1861efba8f3bfcb36ad0a448" "58264887d7ab17702ef85bbd96e11bd7f613622ff9c63990be860b958c978f09" "4b026ac68a1aa4d1a91879b64f54c2490b4ecad8b64de5b1865bca0addd053d9" "21e3d55141186651571241c2ba3c665979d1e886f53b2e52411e9e96659132d4" default))
 '(package-selected-packages
   '(php-mode treemacs-evil treemacs magit modus-themes tuareg evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
