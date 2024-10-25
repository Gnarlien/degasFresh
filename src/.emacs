;; Use fortran-mode for web files.
(setq auto-mode-alist
      (cons '("\\.hweb$" . fortran-mode) auto-mode-alist))

(setq auto-mode-alist
      (cons '("\\.web$" . fortran-mode) auto-mode-alist))

;;; pcl-cvs-startup.el,v 1.2 1992/04/07 20:49:17 berliner Exp
(autoload 'cvs-update "pcl-cvs"
	  "Run a 'cvs update' in the current working directory. Feed the
output to a *cvs* buffer and run cvs-mode on it.
If optional prefix argument LOCAL is non-nil, 'cvs update -l' is run."
	  t)
;; The jka-compr package causes automatic decompression
(load "jka-compr")

;; The following get jka-compr to invoke ncdump on netcdf files.
(setq jka-compr-compression-info-list
      (cons '["\\.nc~?\\'"
	      "running ncgen"   "ncgen-filter"         nil
	      "running ncdump"  "ncdump-filter"        nil
	      nil nil "\037\213"]
	    jka-compr-compression-info-list))

(and (jka-compr-installed-p)
     (jka-compr-uninstall))

(jka-compr-install)
