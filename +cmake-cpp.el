;;; +cmake-cpp.el -*- lexical-binding: t; -*-
(defvar my/cmake-presets '("clang" "llvm-mingw" "llvm-msvc" "mingw" "riscv-hpm")
  "Available CMake presets.")

(defvar my/cmake-build '("debug" "release" "debug-llvm-msvc" "release-llvm-msvc" "debug-llvm-mingw" "release-llvm-mingw" "debug-mingw" "release-mingw" "riscv-hpm"))


(defun my/cmake-run-preset (preset)
  (interactive
   (list (completing-read "Select CMake preset: " my/cmake-presets)))
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile (format "cmake --preset=%s" preset))))

(defun my/cmake-build-preset (preset)
  (interactive
   (list (completing-read "Select Build preset: " my/cmake-build)))
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile (format "cmake --build --preset=%s" preset))))

(defun my/cmake--scan-executables ()
  "Scan build directory for executables."
  (let* ((root (or (projectile-project-root) default-directory))
         (build-dir (expand-file-name "build" root))
         (executables (when (file-exists-p build-dir)
                       (directory-files-recursively build-dir "" t))))
    (seq-filter
     (lambda (f)
       (and (not (file-directory-p f))
            (if (eq system-type 'windows-nt)
                (string-match-p "\\.exe$" f)
              (file-executable-p f))))
     executables)))

(defun my/cmake-smart-run ()
  (interactive)
  (let* ((exes (my/cmake--scan-executables)))
    (cond
     ((null exes)
      (message "❌ No executables found in build directory. Did you compile successfully?"))
     ((= 1 (length exes))
      (compile (shell-quote-argument (car exes))))
     (t
      (let ((choice (completing-read "Select executable: " exes)))
        (compile (shell-quote-argument choice)))))))(defun my/cmake-debug ()
  (interactive)
  (let* ((root (or (projectile-project-root) default-directory))
         (exe (completing-read "Select executable to debug: "
                               (my/cmake--scan-executables))))
    (compile (format "gdb %s" (shell-quote-argument exe)))))

(map! :after cc-mode
      :map (c-mode-map c++-mode-map cmake-mode-map)
      :localleader
      :desc "Cmake Configure" "c" #'my/cmake-run-preset
      :desc "Cmake Build"     "b" #'my/cmake-build-preset
      :desc "Cmake Run" "a" #'my/cmake-smart-run
      :desc "Cmake Debug" "d" #'my/cmake-debug)
