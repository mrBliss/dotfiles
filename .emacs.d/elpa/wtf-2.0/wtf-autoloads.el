;;; wtf-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (wtf-is wtf-remove wtf-add) "wtf" "wtf.el" (19408
;;;;;;  29424))
;;; Generated autoloads from wtf.el

(autoload 'wtf-add "wtf" "\
Add ACRONYM and its DEFINITION to the list of custom associations.

If you add a custom acronym definition, and feel it to be worth
sharing, you are encouraged to contact <mwolson@gnu.org> via
email, providing the acronym and its definition.  This increases
the chance that it will appear in future versions of wtf.el.

\(fn ACRONYM DEFINITION)" t nil)

(autoload 'wtf-remove "wtf" "\
Remove ACRONYM from the list of custom associations.
If ACRONYM is not in the custom associations, but instead in
`wtf-alist', it will be marked as ignored by adding it to
`wtf-removed-acronyms'.

\(fn ACRONYM)" t nil)

(autoload 'wtf-is "wtf" "\
Provide the definition for ACRONYM.
When called interactively, display the message \"ACRONYM is DEF\".
Otherwise, return DEF.

DEF refers to the definition associated with ACRONYM in `wtf-alist'.

\(fn ACRONYM)" t nil)

;;;***

;;;### (autoloads nil nil ("wtf-pkg.el") (19408 29424 493480))

;;;***

(provide 'wtf-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wtf-autoloads.el ends here
