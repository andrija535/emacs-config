;; Package Functionality
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)


(add-to-list 'load-path (concat user-emacs-directory "vue-ts-mode"))
(require 'vue-ts-mode)
(setq-default vue-ts-mode-indent-offset 4)

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
(use-package sgml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" html-mode)))
(use-package multi-web-mode
  :ensure t
  :config
  (setq mweb-default-major-mode 'html-mode)
  (setq mweb-tags 
	'((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
	  (js-mode  "<script[^>]*>" "</script>")
	  (css-mode "<style[^>]*>" "</style>")))
  (setq mweb-filename-extensions '("php" "html")))
(use-package php-mode
  :ensure t)
(use-package typescript-mode
  :ensure t)
(use-package kaolin-themes
  :ensure t)
(use-package json-ts-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode)))
(use-package typescript-ts-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode)))
(use-package company
  :ensure t
  :config
  (global-company-mode))

;; Custom options
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(keymap-global-set "C-c t c" #'treemacs-select-directory)
(keymap-global-set "C-c l" #'default-code-layout)

;; For ocamlrpc
(add-to-list 'load-path "/home/andrija/.opam/5.1.0/share/emacs/site-lisp")
     (require 'ocp-indent)

;; TreeSitter
(setq treesit-language-source-alist
      '((json "https://github.com/tree-sitter/tree-sitter-json")
       (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
       (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
       (vue "https://github.com/ikatyang/tree-sitter-vue")
       (css "https://github.com/tree-sitter/tree-sitter-css")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(kaolin-light))
 '(custom-safe-themes
   '("c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428" "0170347031e5dfa93813765bc4ef9269a5e357c0be01febfa3ae5e5fcb351f09" "5a00018936fa1df1cd9d54bee02c8a64eafac941453ab48394e2ec2c498b834a" "249e100de137f516d56bcf2e98c1e3f9e1e8a6dce50726c974fa6838fbfcec6b" "06ed754b259cb54c30c658502f843937ff19f8b53597ac28577ec33bb084fa52" "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336" "b95f61aa5f8a54d494a219fcde9049e23e3396459a224631e1719effcb981dbd" "2ce76d65a813fae8cfee5c207f46f2a256bac69dacbb096051a7a8651aa252b0" "a131602c676b904a5509fff82649a639061bf948a5205327e0f5d1559e04f5ed" "9cd57dd6d61cdf4f6aef3102c4cc2cfc04f5884d4f40b2c90a866c9b6267f2b3" "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37" "788121c96b7a9b99a6f35e53b7c154991f4880bb0046a80330bb904c1a85e275" "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3" "b5fab52f16546a15f171e6bd450ff11f2a9e20e5ac7ec10fa38a14bb0c67b9ab" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "3d94d6d1a1c23113a60c8496c9aed094dbc2695f219e8127bb168d17b1e6dab3" "15604b083d03519b0c2ed7b32da6d7b2dc2f6630bef62608def60cdcf9216184" "88cb0f9c0c11dbb4c26a628d35eb9239d1cf580cfd28e332e654e7f58b4e721b" "69f7e8101867cfac410e88140f8c51b4433b93680901bb0b52014144366a08c8" "611ef0918b8b413badb8055089b5499c1d4ac20f1861efba8f3bfcb36ad0a448" "58264887d7ab17702ef85bbd96e11bd7f613622ff9c63990be860b958c978f09" "4b026ac68a1aa4d1a91879b64f54c2490b4ecad8b64de5b1865bca0addd053d9" "21e3d55141186651571241c2ba3c665979d1e886f53b2e52411e9e96659132d4" default))
 '(package-selected-packages
   '(company neotree kaolin-themes typescript-mode php-mode treemacs-evil treemacs magit modus-themes tuareg evil))
 '(sgml-basic-offset 4)
 '(typescript-ts-mode-indent-offset 4)
 '(vue-ts-mode-indent-offset 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
