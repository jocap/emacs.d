;;; autothemer-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (autothemer-generate-templates autothemer-deftheme)
;;;;;;  "autothemer" "autothemer.el" (22771 19573 825077 600000))
;;; Generated autoloads from autothemer.el

(autoload 'autothemer-deftheme "autothemer" "\
Define a theme NAME with description DESCRIPTION.
A color PALETTE can be used to define let*-like
bindings within both the REDUCED-SPECS and the BODY.

\(fn NAME DESCRIPTION PALETTE REDUCED-SPECS &rest BODY)" nil t)

(autoload 'autothemer-generate-templates "autothemer" "\
Autogenerate customizations for all unthemed faces.
Iterate through all currently defined faces, select those that
have been left uncustomized by the most recent call to
`autothemer-deftheme' and generate customizations that best
approximate the faces' current definitions using the color
palette used in the most recent invocation of
`autothemer-deftheme'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("autothemer-pkg.el") (22771 19573 973069
;;;;;;  300000))

;;;***

(provide 'autothemer-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; autothemer-autoloads.el ends here
