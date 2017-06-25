;;; google.el --- Configuration for working with google services inside emacs

;;; Commentary:
;; 


(defvar gdocs-folder-id "0Bw-6SdA-TP7nc0hVWEloWTNwUlE"
     "location for storing org to gdocs exported files, use 'gdrive list  -t <foldername>' to find the id")

(defun gdrive-pull ()
  "Import a file in gdocs-folder-id into current buffer"
  (interactive) 
  (let ((id (completing-read "file to pull from: "
			     (split-string (shell-command-to-string  "gdrive list") "\n"))))
    (shell-command (format "gdrive -c export -f --mime text/plain %s" (car (split-string id " "))))))

(defun gdrive-update ()
  "Import a file in gdocs-folder-id into current buffer"
  (interactive) 
  (let* ((id (split-string (completing-read "file to update: "
					    (split-string (shell-command-to-string  "gdrive list") "\n")) " "))
	 (file (nth 0 id))
	 (name (nth 3 id)))
    (shell-command (format "gdrive update --mime text/plain --name %s %s %s" name file buffer-file-name))))

(defun gdrive-push ()
  "Import a file in gdocs-folder-id into current buffer"
  (interactive) 
  (let ((id (completing-read "directory to push to: "
			     (split-string (shell-command-to-string  "gdrive list") "\n"))))
    (shell-command (format "gdrive import --mime text/plain -p %s %s" (car (split-string id " ")) buffer-file-name))))

(defun gdrive-pull-all ()
  "Sync the current directory with a folder from google drive"
  (interactive) 
  (let ((id (completing-read "directory to pull from: "
			     (split-string (shell-command-to-string  "gdrive list") "\n"))))
    (shell-command (format "gdrive -c ~/google/ sync download %s ." (car (split-string id " "))))))

(defun gdrive-push-all ()
  "Sync the current directory with a folder from google drive"
  (interactive) 
  (let ((id (completing-read "directory to push to: "
			     (split-string (shell-command-to-string  "gdrive list") "\n"))))
    (shell-command (format "gdrive sync upload ~/google/ %s" (car (split-string id " "))))))

(provide 'google)


;;; google.el ends here
