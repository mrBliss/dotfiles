;;; episode-renamer.el --- Rename episodes with TheTVDB info
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sun Aug 22 2010
;; Keywords: episodes, tvshows, rename, tvdbid, dired
;;
;;; Usage =============================================================
;;
;; * Open a dired buffer containing tv show episodes and optionally
;;   subtitles.
;; * M-x rename-episodes
;;
;; A tvshow.nfo file will be searched for in one of the parent
;; directories and the tvdbid and language field will be extracted.
;; The season number will be extracted from the path name.
;;
;; Example: i:/TV Shows/The IT Crowd/Season 2/ is opened in a dired
;; buffer. The file i:/TV Shows/The IT Crowd/tvshow.nfo should exist
;; (tvshow.nfo files are used for XBMC).
;;
;; When there is no subfolder from which the season can be deducted, 1
;; will be used.
;;
;; With this information the script `episodetitles' will be called, this
;; script fires up episodetitles.jar with the necessary arguments. The
;; jar contains a Clojure program that fetches the episode titles from
;; TheTVDB and prints them to stdout.
;;
;; This output is picked up by Emacs and used to rename the video and
;; subtitle files.
;;
;; When the number of video files, subtitles and episode titles
;; don't match, the program stops.
;;
;; If you don't like the titles for the episodes, change them
;; afterwards, preferably with wdired-change-to-wdired-mode
;;

(defun get-tvdbid-and-season-and-lang ()
  "Returns a list containing the tvdb, season and language for
the season currently displayed in the dired buffer."
  (let* ((season (or (progn (string-match "Season \\([0-9]+\\)"
                                          (dired-current-directory))
                            (match-string 1 (dired-current-directory)))
                     "1"))
         (tv-show-folder (locate-dominating-file (dired-current-directory)
                                                 "tvshow.nfo"))
         (nfo (concat tv-show-folder "tvshow.nfo")))
    (if (file-readable-p nfo)
        (let* ((tvdbid-line (shell-command-to-string
                             (concat "egrep '<tvdbid>([0-9]+)</tvdbid>' \""
                                     nfo "\"")))
               (tvdbid (progn (string-match "[0-9]+" tvdbid-line)
                              (match-string 0 tvdbid-line)))
               (lang-line (shell-command-to-string
                           (concat "egrep '<language>([a-z]+)</language>' \""
                                   nfo "\"")))
               (lang (progn (string-match ">[a-z]+" lang-line)
                            (substring (match-string 0 lang-line) 1))))
          (list tvdbid season lang)))))

(defun filter-files (exts files)
  "Filters out the files having an extension that is listed in exts."
  (remove-if-not (lambda (f) (member (file-name-extension f) exts)) files))

(defun rename-episodes ()
  (interactive)
  (let* ((cd (dired-current-directory))
         (files (directory-files cd))
         (vids (filter-files '("avi" "mkv" "mp4" "wmv" "flv" "divx") files))
         (subs (filter-files '("srt") files)))
    (if (or (null subs) (= (length vids) (length subs)))
        (destructuring-bind (tvdbid season lang)
            (get-tvdbid-and-season-and-lang)
          (message "Downloading episode titles from TheTVDB...")
          (let* ((output (shell-command-to-string
                          (format "/usr/local/bin/episodetitles %s %s %s"
                                  tvdbid season lang)))
                 (episode-titles (butlast (split-string output "\n"))))
            (if (= (length vids) (length episode-titles))
                (progn
                  (dolist (x (mapcar* 'list vids
                                      (or subs (make-list (length vids) nil))
                                      episode-titles))
                    (rename-file
                     (concat cd (first x))
                     (concat cd (third x) "." (file-name-extension (first x))))
                    (when (second x)
                      (rename-file
                       (concat cd (second x))
                       (concat cd (third x) "."
                               (file-name-extension (second x))))))
                  (message "Successfully renamed %d files" (length vids))
                  (revert-buffer))
              (message "Number of files (%d) and episodes (%d) don't match"
                       (length vids) (length episode-titles)))))
      (message "Number of files (%d) does not match number of subtitles (%d)"
               (length vids) (length subs)))))


(provide 'episode-renamer)
