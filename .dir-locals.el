((nil . ((dante-methods . (v2-build-same-builddir))
       (dante-methods-alist
        . ((v2-build-same-builddir "cabal.project.local"
                                   ("cabal"
                                    "v2-repl"
                                    (or dante-target (dante-package-name) nil)
                                    "--builddir=dist-newstyle")))))))
