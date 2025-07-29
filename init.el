(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Package Functionality
(require 'package)
(setq use-package-compute-statistics t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

;; Eshell
(add-hook 'eshell-mode-hook (lambda ()
                              (setq-local corfu-auto nil)
                              (corfu-mode)))

;; Packages
(use-package evil
  :ensure t
  :config (evil-mode 1))
(use-package tuareg
  :ensure t
  :commands tuareg-mode)
(use-package reason-mode
  :ensure t
  :mode "\\.re\\'")
(use-package opam-switch-mode
  :after tuareg
  :ensure t)
(use-package magit
  :ensure t
  :commands magit)
(use-package simple-modeline
  :ensure t
  :hook after-init)
(use-package doom-themes
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

;; Treesitter
(use-package json-ts-mode
  :ensure t
  :commands json-ts-mode)
(use-package typescript-ts-mode
  :ensure t
  :commands typescript-ts-mode)
(use-package typst-ts-mode
  :ensure t
  :commands typst-ts-mode
  :config (add-hook 'typst-ts-mode-hook #'visual-line-mode))

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . js-ts-mode)
         ("\\.mjs\\'" . js-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . js-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.py\\'" . python-ts-mode))
  :preface
  (defun setup-treesitter ()
    (interactive)
    (dolist (grammar
             '((json "https://github.com/tree-sitter/tree-sitter-json")
              (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
              (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
              (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
              (vue "https://github.com/ikatyang/tree-sitter-vue")
              (css "https://github.com/tree-sitter/tree-sitter-css")
              (c "https://github.com/tree-sitter/tree-sitter-c")
              (typst "https://github.com/uben0/tree-sitter-typst")
              (python "https://github.com/tree-sitter/tree-sitter-python")))
      (add-to-list 'treesit-language-source-alist grammar)
      (let ((grammar (car grammar)))
        (unless (treesit-language-available-p grammar)
          (treesit-install-language-grammar grammar)))))
  :config
  (setup-treesitter))

(setq tab-always-indent 'complete)

(use-package corfu
  :ensure t
  :config (global-corfu-mode))
(use-package exec-path-from-shell
  :disabled
  :config
  (exec-path-from-shell-initialize))
(use-package pyvenv
  :ensure t)

;; Set appropriate colour scheme on startup

(defcustom default-dark-theme 'doom-outrun-electric "Default dark theme to use for example with auto-dark mode but also on startup")
(defcustom default-light-theme 'doom-bluloco-light "Default light theme to use for example with auto-dark mode but also on startup")

(defun set-startup-colour-scheme ()
  (let
      ((is-dark (eq 1 (caar (dbus-ignore-errors
                              (dbus-call-method
                               :session
                               "org.freedesktop.portal.Desktop"
                               "/org/freedesktop/portal/desktop"
                               "org.freedesktop.portal.Settings" "Read"
                               "org.freedesktop.appearance" "color-scheme")))))
       (enabled-themes custom-enabled-themes))
    (load-theme (if is-dark default-dark-theme default-light-theme) t)
    (dolist (theme enabled-themes) (disable-theme theme))))

(setq custom-safe-themes t)
(add-hook 'after-init-hook #'set-startup-colour-scheme)

(use-package auto-dark
  :ensure t
  :config (progn
            (set-variable 'auto-dark-themes (list (list default-dark-theme) (list default-light-theme)))
            (auto-dark-mode)))

(defun org-count-words-selection ()
  (interactive)
  (let ((b (region-beginning))
        (e (region-end)))
    (message (format "%d" (length (split-string (buffer-substring b e)))))))

(defun insert-babel-code-block (language session)
  (interactive "MLanguage: \nMSession: ")
  (save-excursion
    (if (not (string= session ""))
        (insert (format "#+BEGIN_SRC %s :session %s\n\n#+END_SRC" language session))
      (insert (format "#+BEGIN_SRC %s\n\n#+END_SRC" language)))))

(use-package org
  :config (progn
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((python t)))
            (add-hook 'org-mode-hook #'visual-line-mode))
  :bind (:map org-mode-map
              ("C-c b i" . insert-babel-code-block)
              ("C-c w c" . org-count-words-selection)))
  
(defun init-org-roam-dir ()
  (let ((org-roam-default-directory (file-truename "~/org-roam")))
    (progn
      ;; Create org-roam directory if it doesn't already exist
      (if
          (not (file-exists-p org-roam-default-directory))
          (make-directory org-roam-default-directory))
      ;; Set it as default
      (setq org-roam-directory org-roam-default-directory)
      ;; Turn on autosync mode, if we don't do the above it crashes on startup
      (org-roam-db-autosync-mode))))

(use-package org-roam
  :ensure t
  :init #'init-org-roam-dir
  :bind (:map org-mode-map
              ("C-c n f" . org-roam-node-find)
              ("C-c n i" . org-roam-node-insert)))

(use-package web-mode
  :ensure t
  :mode (("\\.php\\'" . web-mode)))

(use-package eglot
  :ensure t
  :hook ((typescript-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (dart-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode js-ts-mode) "typescript-language-server" "--stdio")
               '(dart-mode "dart" "language-server")))

(use-package ess
  :ensure t
  :commands ess-mode)

(use-package auctex
  :ensure t
  :commands LaTeX-mode
  :config (progn
            (setq-default TeX-master nil)
            (add-hook 'LaTeX-mode-hook (lambda ()
                                         (turn-on-reftex)
                                         (visual-line-mode)))))

(use-package rust-mode
  :ensure t
  :commands rust-mode)

(use-package dart-mode
  :ensure t
  :commands dart-mode
  :mode ("\\.dart\\'" . dart-mode))

(defun open-terminal-in-current-dired-dir ()
  (interactive)
  (async-shell-command (format "kitty %s" dired-directory)))

(add-hook 'dired-mode-hook (lambda () (keymap-local-set "C-c t" #'open-terminal-in-current-dired-dir)))

;; Custom options
(setq-default indent-tabs-mode nil)
(keymap-global-set "C-c l" #'default-code-layout)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; For ocamlrpc
;; Condition case because this breaks every time I updated emacs and it was
;; getting annoying
(condition-case nil
    (progn
      (add-to-list 'load-path "/home/andrija/.opam/5.3.0+flambda/share/emacs/site-lisp")
      (require 'ocp-indent))
  (error nil))

;; Merlin
(let ((opam-share (ignore-errors (car (process-lines "opam" "var"
                                                     "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp"
                                              opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'reason-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)
    ;; To easily change opam switches within a given Emacs session, you can
    ;; install the minor mode https://github.com/ProofGeneral/opam-switch-mode
    ;; and use one of its "OPSW" menus.
    ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-default-style
   '((c-mode . "bsd") (java-mode . "java") (awk-mode . "awk")
     (csharp-mode . "csharp") (other . "gnu")))
 '(c-ts-mode-indent-offset 4)
 '(custom-enabled-themes '(kaolin-temple))
 '(custom-safe-themes
   '("9e36779f5244f7d715d206158a3dade839d4ccb17f6a2f0108bf8d476160a221"
     "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9"
     "e14884c30d875c64f6a9cdd68fe87ef94385550cab4890182197b95d53a7cf40"
     "691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b"
     "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738"
     "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350"
     "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36"
     "7c28419e963b04bf7ad14f3d8f6655c078de75e4944843ef9522dbecfcd8717d"
     "c5878086e65614424a84ad5c758b07e9edcf4c513e08a1c5b1533f313d1b17f1"
     "6a5584ee8de384f2d8b1a1c30ed5b8af1d00adcbdcd70ba1967898c265878acf"
     "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851"
     "113a135eb7a2ace6d9801469324f9f7624f8c696b72e3709feb7368b06ddaccc"
     "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
     "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554"
     "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e"
     "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700"
     "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f"
     "2771ec93656faf267521dce9ffe1a6ad88cd0bea87aa0e8c4fc80bf355c58c1d"
     "9d5124bef86c2348d7d4774ca384ae7b6027ff7f6eb3c401378e298ce605f83a"
     "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a"
     "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290"
     "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9"
     "e978b5106d203ba61eda3242317feff219f257f6300bd9b952726faf4c5dee7b"
     "a75aff58f0d5bbf230e5d1a02169ac2fbf45c930f816f3a21563304d5140d245"
     "d41229b2ff1e9929d0ea3b4fde9ed4c1e0775993df9d998a3cdf37f2358d386b"
     "712dda0818312c175a60d94ba676b404fc815f8c7e6c080c9b4061596c60a1db"
     "faf642d1511fb0cb9b8634b2070a097656bdb5d88522657370eeeb11baea4a1c"
     "0cc70543214e5133e0eb479a01e57128a4f3e62195ca9073dffe90c8a57519e1"
     "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428"
     "0170347031e5dfa93813765bc4ef9269a5e357c0be01febfa3ae5e5fcb351f09"
     "5a00018936fa1df1cd9d54bee02c8a64eafac941453ab48394e2ec2c498b834a"
     "249e100de137f516d56bcf2e98c1e3f9e1e8a6dce50726c974fa6838fbfcec6b"
     "06ed754b259cb54c30c658502f843937ff19f8b53597ac28577ec33bb084fa52"
     "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336"
     "b95f61aa5f8a54d494a219fcde9049e23e3396459a224631e1719effcb981dbd"
     "2ce76d65a813fae8cfee5c207f46f2a256bac69dacbb096051a7a8651aa252b0"
     "a131602c676b904a5509fff82649a639061bf948a5205327e0f5d1559e04f5ed"
     "9cd57dd6d61cdf4f6aef3102c4cc2cfc04f5884d4f40b2c90a866c9b6267f2b3"
     "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37"
     "788121c96b7a9b99a6f35e53b7c154991f4880bb0046a80330bb904c1a85e275"
     "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3"
     "b5fab52f16546a15f171e6bd450ff11f2a9e20e5ac7ec10fa38a14bb0c67b9ab"
     "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041"
     "3d94d6d1a1c23113a60c8496c9aed094dbc2695f219e8127bb168d17b1e6dab3"
     "15604b083d03519b0c2ed7b32da6d7b2dc2f6630bef62608def60cdcf9216184"
     "88cb0f9c0c11dbb4c26a628d35eb9239d1cf580cfd28e332e654e7f58b4e721b"
     "69f7e8101867cfac410e88140f8c51b4433b93680901bb0b52014144366a08c8"
     "611ef0918b8b413badb8055089b5499c1d4ac20f1861efba8f3bfcb36ad0a448"
     "58264887d7ab17702ef85bbd96e11bd7f613622ff9c63990be860b958c978f09"
     "4b026ac68a1aa4d1a91879b64f54c2490b4ecad8b64de5b1865bca0addd053d9"
     "21e3d55141186651571241c2ba3c665979d1e886f53b2e52411e9e96659132d4"
     default))
 '(dired-listing-switches "-alh")
 '(eglot-confirm-server-edits nil)
 '(eglot-confirm-server-initiated-edits nil)
 '(erlang-indent-level 2)
 '(ispell-dictionary "en_GB")
 '(js-indent-level 2)
 '(mailcap-user-mime-data '(("xdg-open %s" "application/pdf")))
 '(omnisharp-server-executable-path
   "/home/andrija/Downloads/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe")
 '(org-agenda-files
   '("~/Documents/school/ipcv/home_assignment/main.org"
     "/home/andrija/agenda.org"))
 '(org-confirm-babel-evaluate nil)
 '(package-selected-packages
   '(auctex auto-dark dart-mode docker dockerfile-mode doom-themes ess
            evil exec-path-from-shell haskell-mode magit neotree
            opam-switch-mode org-roam php-mode pyvenv reason-mode
            rust-mode simple-modeline tuareg typst-ts-mode web-mode))
 '(python-indent-offset 2)
 '(safe-local-variable-values
   '((org-roam-directory
      . "/home/andrija/Documents/school/dissertation/notes/")))
 '(sgml-basic-offset 4)
 '(typescript-ts-mode-indent-offset 2)
 '(typst-ts-mode-indent-offset 2)
 '(web-mode-code-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight regular :height 120 :width normal)))))
(put 'dired-find-alternate-file 'disabled nil)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
