;; Package Functionality
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
(use-package typescript-mode
  :ensure t)
(use-package kaolin-themes
  :ensure t)

;; Custom options
(defun default-code-layout (directory)
  (interactive "DDirectory: ") 
  (term "/bin/fish")
  (process-send-string "*terminal*" (concat "cd " directory "\n"))
  (split-window-horizontally)
  (setq default-directory directory)
  (treemacs))
  
(setq indent-tabs-mode nil)
(setq tab-width 4)
(keymap-global-set "C-c t c" #'treemacs-select-directory)
(keymap-global-set "C-c l" #'default-code-layout)

;; For ocamlrpc
(add-to-list 'load-path "/home/andrija/.opam/5.1.0/share/emacs/site-lisp")
     (require 'ocp-indent)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(kaolin-eclipse))
 '(custom-safe-themes
   '("9cd57dd6d61cdf4f6aef3102c4cc2cfc04f5884d4f40b2c90a866c9b6267f2b3" "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37" "788121c96b7a9b99a6f35e53b7c154991f4880bb0046a80330bb904c1a85e275" "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3" "b5fab52f16546a15f171e6bd450ff11f2a9e20e5ac7ec10fa38a14bb0c67b9ab" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "3d94d6d1a1c23113a60c8496c9aed094dbc2695f219e8127bb168d17b1e6dab3" "15604b083d03519b0c2ed7b32da6d7b2dc2f6630bef62608def60cdcf9216184" "88cb0f9c0c11dbb4c26a628d35eb9239d1cf580cfd28e332e654e7f58b4e721b" "69f7e8101867cfac410e88140f8c51b4433b93680901bb0b52014144366a08c8" "611ef0918b8b413badb8055089b5499c1d4ac20f1861efba8f3bfcb36ad0a448" "58264887d7ab17702ef85bbd96e11bd7f613622ff9c63990be860b958c978f09" "4b026ac68a1aa4d1a91879b64f54c2490b4ecad8b64de5b1865bca0addd053d9" "21e3d55141186651571241c2ba3c665979d1e886f53b2e52411e9e96659132d4" default))
 '(package-selected-packages
   '(kaolin-themes typescript-mode php-mode treemacs-evil treemacs magit modus-themes tuareg evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
