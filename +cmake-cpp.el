;;; +cmake-cpp.el -*- lexical-binding: t; -*-
(require 'json)
(setq lsp-clients-clangd-executable my-clangd-path)

;;; Preset Support
(defvar my/cmake-presets nil
  "Loaded CMake presets from CMakePresets.json.")

(defvar my/cmake-active-preset nil
  "Currently active CMake preset.")

(defun my/cmake-load-presets ()
  "Load CMake presets from CMakePresets.json in project root."
  (let ((preset-file (expand-file-name "CMakePresets.json" (projectile-project-root))))
    (when (file-exists-p preset-file)
      (with-temp-buffer
        (insert-file-contents preset-file)
        (setq my/cmake-presets (json-parse-buffer))))))

(defun my/cmake-select-preset ()
  "Select CMake preset from loaded presets."
  (interactive)
  (unless my/cmake-presets
    (my/cmake-load-presets))
  (when my/cmake-presets
    (let ((presets (mapcar (lambda (p) (gethash "name" p))
                         (gethash "configurePresets" my/cmake-presets))))
      (setq my/cmake-active-preset
            (completing-read "Select CMake preset: " presets))))

;;; Original Configuration
(defvar my/cmake-build-type "Debug"
  "Current CMake build type (Debug/Release/RelWithDebInfo/MinSizeRel).")

(defvar my/cmake-kit nil
  "Selected CMake kit (toolchain file path).")

(defvar my/cmake-build-dir "build"
  "Default build directory name (without build type suffix).")

(defvar my/cmake-configure-args ""
  "Additional CMake configure arguments.")

;;; Path Utilities
(defun my/cmake--get-build-dir ()
  "Get actual build directory path with platform-specific considerations."
  (let ((base-dir (expand-file-name my/cmake-build-dir (projectile-project-root))))
    (cond
     ((eq system-type 'windows-nt)
      (concat base-dir "/" my/cmake-build-type)) ; Windows: build/Debug
     (t base-dir)))) ; Unix-like: build

(defun my/cmake--get-executable-suffix ()
  "Get platform-specific executable suffix."
  (cond ((eq system-type 'windows-nt) ".exe")
        (t "")))

;;; Automatic Configuration
(defvar my/cmake-auto-configure nil
  "Whether to automatically configure when CMakeLists.txt changes.")

(defvar my/cmake--watcher nil
  "File watcher for automatic configuration.")

(defun my/cmake--auto-configure-callback (event)
  "Callback for CMakeLists.txt changes."
  (when (and my/cmake-auto-configure
             (string-match "CMakeLists\\.txt" (cadr event)))
    (my/cmake-configure)))

(defun my/cmake-toggle-auto-configure ()
  "Toggle automatic CMake configuration."
  (interactive)
  (setq my/cmake-auto-configure (not my/cmake-auto-configure))
  (message "CMake auto-configure %s"
           (if my/cmake-auto-configure "enabled" "disabled"))
  (when my/cmake-auto-configure
    (unless my/cmake--watcher
      (setq my/cmake--watcher
            (file-notify-add-watch (projectile-project-root)
                                  '(change) #'my/cmake--auto-configure-callback)))
    (when (file-exists-p "CMakeLists.txt")
      (my/cmake-configure)))

;;; Kit Management
(defun my/cmake-scan-kits ()
  "Scan for CMake toolchain files in platform-specific locations."
  (interactive)
  (let* ((search-paths
          (append
           (list (expand-file-name "cmake/" (projectile-project-root))
           (cond
            ((eq system-type 'windows-nt)
             '("C:/Program Files/CMake/share/cmake/"
               "C:/Program Files (x86)/CMake/share/cmake/"))
            ((eq system-type 'darwin)
             '("/usr/local/share/cmake/"
               "/opt/homebrew/share/cmake/"))
            (t ; Linux/Unix
             '("/usr/share/cmake/"
               "/usr/local/share/cmake/")))))
         (kits (cl-loop for path in search-paths
                        when (file-exists-p path)
                        append (directory-files path t "\\.cmake$"))))
    (if kits
        (setq my/cmake-kit (completing-read "Select CMake kit: " kits))
      (message "No CMake kits found in: %s" search-paths)))))

;;; Build Configuration
(defun my/cmake-select-build-type ()
  "Select build type with platform-aware defaults."
  (interactive)
  (setq my/cmake-build-type
        (completing-read
         (format "Build type (current: %s): " my/cmake-build-type)
         '("Debug" "Release" "RelWithDebInfo" "MinSizeRel")
         nil t nil nil my/cmake-build-type)))

;;; Core Commands
(defun my/cmake-configure ()
  "Cross-platform CMake configuration using presets if available."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (if my/cmake-active-preset
        (compile (format "cmake --preset %s" my/cmake-active-preset))
      (compile
       (format "cmake -S . -B %s -DCMAKE_BUILD_TYPE=%s %s %s"
               (shell-quote-argument (my/cmake--get-build-dir))
               my/cmake-build-type
               (if my/cmake-kit
                   (concat "-DCMAKE_TOOLCHAIN_FILE=" (shell-quote-argument my/cmake-kit))
                 "")
               my/cmake-configure-args)))))
    ;; Handle compile_commands.json
    (run-at-time
     1 nil
     (lambda ()
       (let ((src (expand-file-name "compile_commands.json" (my/cmake--get-build-dir))))
         (when (file-exists-p src)
           (cond
            ((eq system-type 'windows-nt)
             (shell-command (format "copy /Y %s ." (shell-quote-argument src))))
            (t
             (make-symbolic-link src (projectile-project-root) t)))))))))

(defun my/cmake-build (&optional target)
  "Build with platform-specific command using presets if available."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (if my/cmake-active-preset
        (compile (format "cmake --build --preset %s%s"
                         my/cmake-active-preset
                         (if target (concat " --target " target) "")))
      (let ((build-dir (my/cmake--get-build-dir)))
        (compile
         (format "cmake --build %s --config %s%s"
                 (shell-quote-argument build-dir)
                 my/cmake-build-type
                 (if target (concat " --target " target) "")))))))

;;; Target Management
(defun my/cmake-get-targets ()
  "Get available targets in a cross-platform way."
  (let ((default-directory (projectile-project-root)))
    (split-string
     (shell-command-to-string
      (format "cmake --build %s --target help 2>&1 | grep -v '^\\.\\.\\.' | grep -v '^ '"
              (shell-quote-argument (my/cmake--get-build-dir)))))))

(defun my/cmake-select-target ()
  "Select target with completion."
  (interactive)
  (let ((targets (my/cmake-get-targets)))
    (when targets
      (my/cmake-build (completing-read "Select target: " targets)))))

;;; Execution
(defun my/cmake-find-executables ()
  "Find executables with platform-specific paths."
  (let* ((build-dir (my/cmake--get-build-dir))
    (directory-files
     build-dir t
     (if (eq system-type 'windows-nt) "^[^.]+\.exe$" "^[^.]+$")))))

(defun my/cmake-run ()
  "Run executable with platform-specific handling."
  (interactive)
  (let* ((executables (my/cmake-find-executables))
         (exe (if (= (length executables) 1)
                  (car executables)
                (completing-read "Select executable: " executables))))
    (compile (shell-quote-argument exe))))

(defun my/cmake-debug ()
  "Debug with platform-specific debugger."
  (interactive)
  (let* ((executables (my/cmake-find-executables))
    (when executables
      (let ((exe (if (= (length executables) 1)
                     (car executables)
                   (completing-read "Select executable: " executables))))
        (cond
         ((eq system-type 'windows-nt)
          (compile (format "devenv /debugexe %s" (shell-quote-argument exe))))
         ((eq system-type 'darwin)
          (compile (format "lldb %s" (shell-quote-argument exe))))
         (t ; Linux/Unix
          (compile (format "gdb -i=mi %s" (shell-quote-argument exe))))))))))
;;; Test Integration
(defun my/cmake-run-tests ()
  "Run tests using CTest."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (if my/cmake-active-preset
        (compile (format "ctest --preset %s" my/cmake-active-preset))
      (let ((build-dir (my/cmake--get-build-dir)))
        (compile (format "ctest --test-dir %s"
                         (shell-quote-argument build-dir)))))))

(defun my/cmake-show-config ()
  "Show current CMake configuration."
  (interactive)
  (message "CMake Config: BuildType=%s, Kit=%s, Preset=%s, Auto=%s"
           my/cmake-build-type
           (or my/cmake-kit "None")
           (or my/cmake-active-preset "None")
           (if my/cmake-auto-configure "On" "Off")))

;;; Configuration UI
(defhydra my/cmake-hydra (:color pink :hint nil)
  "
CMake Configuration:
_c_: Configure  _b_: Build  _t_: Select Target  _r_: Run
_d_: Debug  _k_: Kits  _T_: Build Type  _p_: Presets
_R_: Tests  _a_: Auto-config (%s`my/cmake-auto-configure)
_q_: Quit
"
  ("c" my/cmake-configure)
  ("b" my/cmake-build)
  ("t" my/cmake-select-target)
  ("r" my/cmake-run)
  ("d" my/cmake-debug)
  ("k" my/cmake-scan-kits)
  ("T" my/cmake-select-build-type)
  ("p" my/cmake-select-preset)
  ("R" my/cmake-run-tests)
  ("a" my/cmake-toggle-auto-configure)
  ("q" nil "quit"))


;;; Keybindings
(map! :after (cc-mode cmake-mode)
      :map (c-mode-map c++-mode-map cmake-mode-map)
      :localleader
      :prefix ("c" . "CMake")
      "c" #'my/cmake-configure
      "b" #'my/cmake-build
      "t" #'my/cmake-select-target
      "r" #'my/cmake-run
      "d" #'my/cmake-debug
      "k" #'my/cmake-scan-kits
      "T" #'my/cmake-select-build-type
      "p" #'my/cmake-select-preset
      "R" #'my/cmake-run-tests
      "C" #'my/cmake-show-config
      "H" #'my/cmake-hydra/body
      "s" #'my/cmake-status-mode)
;;; Status Indicators
(defvar my/cmake-mode-line-string ""
  "Mode line string showing CMake status.")

(defun my/cmake-update-mode-line ()
  "Update the mode line string with current CMake status."
  (setq my/cmake-mode-line-string
        (format " CMake[%s%s%s]"
                my/cmake-build-type
                (if my/cmake-active-preset (format ":%s" my/cmake-active-preset) "")
                (if my/cmake-auto-configure "(A)" ""))))

(define-minor-mode my/cmake-status-mode
  "Toggle display of CMake status in mode line."
  :global t
  :lighter my/cmake-mode-line-string
  (if my/cmake-status-mode
      (progn
        (add-hook 'after-save-hook #'my/cmake-update-mode-line)
        (my/cmake-update-mode-line))
    (remove-hook 'after-save-hook #'my/cmake-update-mode-line)))

;; Optional: Install language parsers
;; M-x tree-sitter-install-language-grammar
