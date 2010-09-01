;;; episode-renamer.el --- Rename episodes with TheTVDB info
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sun Aug 22 2010
;; Keywords: episodes, tvshows, rename, tvdbid, dired
;;
;;; Usage =============================================================
;;
;; Open a dired buffer with tv show episodes and optionally subtitles.
;; M-x rename-episodes
;;
;; A tvshow.nfo file will be searched in one of the parent directories
;; and the tvdbid and language field will be extracted. The season
;; number will be extracted from the path name.
;;
;; Example: i:/TV Shows/The IT Crowd/Season 2/ is opened in a dired
;; buffer. The file i:/TV Shows/The IT Crowd/tvshow.nfo should exist
;; (tvshow.nfo files are used for XBMC).
;;
;; With this information the script episodetitles will be called, this
;; script fires up episodetitles.jar with the necessary arguments. The
;; jar contains a Clojure program that fetches the episode titles from
;; tvdbid and prints them to stdout.
;;
;; This output is picked up by Emacs and used to rename the video and
;; subtitle files.
;;
;; When the number of video files, subtitles and episode titles
;; doesn't match, the program stops.
;;
;; If you don't like the titles for the episodes, change them
;; afterwards, preferably with wdired-change-to-wdired-mode
;;

(defun get-tvdbid-and-season-and-lang ()
  "Returns of list containing the tvdb, season and language for
the season currently displayed in the dired buffer"
  (let* ((season (progn  (string-match "Season \\([0-9]+\\)"
                                       (dired-current-directory))
                         (match-string 1 (dired-current-directory))))
         (tv-show-folder (locate-dominating-file (dired-current-directory)
                                                 "tvshow.nfo"))
         (nfo (concat tv-show-folder "tvshow.nfo")))
    (if (file-readable-p nfo)
        (let* ((tvdbid-line (shell-command-to-string
                             (concat "egrep '<tvdbid>([0-9]+)</tvdbid>' '"
                                     nfo "'")))
               (tvdbid (progn (string-match "[0-9]+" tvdbid-line)
                              (match-string 0 tvdbid-line)))
               (lang-line (shell-command-to-string
                           (concat "egrep '<language>([a-z]+)</language>' '"
                                   nfo "'")))
               (lang (progn (string-match ">[a-z]+" lang-line)
                            (substring (match-string 0 lang-line) 1))))
          (list tvdbid season lang)))))

(defun filter (pred list)
  "Returns a list of the items in coll for which (pred item)
returns true. pred must be free of side-effects."
  (cond ((null list) list)
        ((apply pred (list (car list)))
         (cons (car list) (filter pred (cdr list))))
        (t (filter pred (cdr list)))))

(defun some (pred coll)
  "Returns the first logical true value of (pred x) for any x in
coll, else nil."
  (when (not (null coll))
    (or (apply pred (list (car coll))) (some pred (cdr coll)))))

(defun rename-episodes ()
  (interactive)
  (let* ((vid-exts '("avi" "mkv" "mp4" "wmv" "flv" "divx" "mpg"))
         (sub-exts '("srt" "sub"))
         (cd (dired-current-directory))
         (files (directory-files cd))
         (vids (filter
                (lambda (f) (some (lambda (ext) (string-match ext f)) vid-exts))
                files))
         (subs (filter
                (lambda (f) (some (lambda (ext) (string-match ext f)) sub-exts))
                files)))
    (if (or (null subs)
            (= (length vids) (length subs)))
        (destructuring-bind (tvdbid season lang)
            (get-tvdbid-and-season-and-lang)
          (message "Downloading episode titles from TheTVDB...")
          (let ((episode-titles
                 (butlast
                  (split-string
                   (shell-command-to-string
                    (concat "/usr/local/bin/episodetitles "
                            tvdbid " " season " " lang))
                   "\n"))))
            (if (= (length vids) (length episode-titles))
                (progn
                  (dolist (x (mapcar* 'list vids subs episode-titles))
                    (rename-file
                     (concat cd (first x))
                     (concat cd (third x) "." (file-name-extension (first x))))
                    (rename-file
                     (concat cd (second x))
                     (concat cd (third x) "." (file-name-extension (second x)))))
                  (message "Successfully renamed %d files" (length vids))
                  (revert-buffer))
              (message "Number of files (%d) does not match number of episodes (%d)"
                       (length vids) (length episode-titles)))))
      (message "Number of files (%d) does not match number of subtitles (%d)"
               (length vids) (length subs)))))

;; Unnecessary function, but I couldn't just delete it ;-)
(defun get-tvshow-tvdbid ()
  "Prompts for TV Show and extracts its tvdbid from tvshow.nfo in
the TV Show folder"
  (interactive)
  (flet ((list-tv-shows
          (dir) (delete ".."
                        (delete "folder.jpg"
                                (delete "." (directory-files dir))))))
    (let* ((tv-shows (nconc (list-tv-shows "d:/TV Shows/")
                            (list-tv-shows "i:/TV Shows/")))
           (tv-show (ido-completing-read "TV Show? " tv-shows))
           (dnfo (concat "d:/TV Shows/" tv-show "/tvshow.nfo"))
           (info (concat "i:/TV Shows/" tv-show "/tvshow.nfo"))
           (nfo (cond ((file-readable-p dnfo) dnfo)
                      ((file-readable-p info) info))))
      (if nfo
          (let* ((tvdbid-line (shell-command-to-string
                               (concat "egrep '<tvdbid>([0-9]+)</tvdbid>' '"
                                       nfo "'"))))
            (string-match "[0-9]+" tvdbid-line)
            (message (match-string 0 tvdbid-line)))
        (message "tvshow.nfo not found for %s" tv-show)))))

(provide 'episode-renamer)