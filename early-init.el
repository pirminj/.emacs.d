;;; early-init.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)
(setq inhibit-startup-echo-area-message (user-login-name))
(setq default-frame-alist '((fullscreen . maximized)
                            (background-color . "#000000")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)
			    (menu-bar-mode . 0)
			    (scroll-bar-mode . 0)
			    (tool-bar-mode . 0)))

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
