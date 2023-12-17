;; init-golang.el --- Initialize Golang configurations. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :hook (go-mode . lsp!)
  :hook (go-mode . tree-sitter!)
  :hook (go-mode . rainbow-delimiters-mode)
  :hook (go-mode . my-enable-show-trailing-whitespace-h)
  :config
  (when (executable-find "goimports")
    (setq gofmt-command "goimports")) ;; go install golang.org/x/tools/cmd/goimports@latest

  (setq gofmt-show-errors nil)
  (map! :map go-mode-map
        :localleader
        "a" #'go-tag-add
        "d" #'go-tag-remove
        "i" #'go-goto-imports      ; Go to imports
        (:prefix ("h" . "help")
                 "." #'godoc-at-point     ; Lookup in godoc
                 "d" #'go-guru-describe   ; Describe this
                 "v" #'go-guru-freevars   ; List free variables
                 "i" #'go-guru-implements ; Implements relations for package types
                 "p" #'go-guru-peers      ; List peers for channel
                 "P" #'go-guru-pointsto   ; What does this point to
                 "r" #'go-guru-referrers  ; List references to object
                 "e" #'go-guru-whicherrs  ; Which errors
                 "w" #'go-guru-what       ; What query
                 "c" #'go-guru-callers    ; Show callers of this function
                 "C" #'go-guru-callees)   ; Show callees of this function
        (:prefix ("ri" . "imports")
                 "a" #'go-import-add
                 "r" #'go-remove-unused-imports)
        (:prefix ("b" . "build")
                 :desc "go run ." "r" (cmd! (compile "go run ."))
                 :desc "go build" "b" (cmd! (compile "go build"))
                 :desc "go clean" "c" (cmd! (compile "go clean"))))

  (after! exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GOBIN" "GO111MODULE" "GOPROXY")))

  (use-package go-tag
    :init (setq go-tag-args (list "-transform" "camelcase")))

  ;; Install: See https://github.com/golangci/golangci-lint#install
  (use-package flycheck-golangci-lint
    :if (executable-find "golangci-lint")
    :after flycheck
    :defines flycheck-disabled-checkers
    :init
    (setq flycheck-golangci-lint-tests t
          flycheck-golangci-lint-disable-linters '("staticcheck" "misspell" "errcheck"))
    :hook (go-mode . (lambda ()
                       ;; Remove default go flycheck-checkers except:go-build and go-test
                       (setq flycheck-disabled-checkers '(go-gofmt
                                                          go-golint
                                                          go-vet
                                                          go-errcheck
                                                          go-unconvert
                                                          go-staticcheck))
                       (flycheck-golangci-lint-setup)
                       ;; Make sure to only run golangci after go-build
                       ;; to ensure we show at least basic errors in the buffer
                       ;; when golangci fails. Make also sure to run go-test if possible.
                       ;; See #13580 for details
                       (flycheck-add-next-checker 'go-build '(warning . golangci-lint) t)
                       (flycheck-add-next-checker 'go-test '(warning . golangci-lint) t)
                       ;; Set basic checkers explicitly as flycheck will
                       ;; select the better golangci-lint automatically.
                       ;; However if it fails we require these as fallbacks.
                       (cond ((flycheck-may-use-checker 'go-test) (flycheck-select-checker 'go-test))
                             ((flycheck-may-use-checker 'go-build) (flycheck-select-checker 'go-build))))))

  (use-package gotest
    :init
    (setq go-test-verbose t)))

(provide 'init-golang)
;;; init-golang ends here
