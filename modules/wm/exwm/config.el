;;; wm/exwm/config.el -*- lexical-binding: t; -*-

(use-package! exwm
  :config
  (setq exwm-workspace-number 6))

(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "DP-4"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output DP-4 --mode 3440x1440 pos 0")))
(exwm-randr-enable)

(require 'exwm-systemtray)
(exwm-systemtray-enable)

(setq exwm-input-global-keys
      `(([?\s-r] . (lambda (command)
                      (interactive (list (read-shell-command "$ ")))
                      (start-process-shell-command command nil command)))
        ([?\s-w] . exwm-workspace-switch)
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))))

(exwm-enable)
