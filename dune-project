(lang dune 2.9)
(name mattlude)
(generate_opam_files true)

(package
 (name mattlude)
 (synopsis "Experimental additions to KW's Prelude")
 (description "Experimental additions to KW's Prelude")
 (maintainers "Matt Teichman <teichman@uchicago.edu>")
 (authors "Matt Teichman <teichman@uchicago.edu>")
 (homepage "https://github.com/bufordrat/mattlude")
 (bug_reports "https://github.com/bufordrat/mattlude")
 (depends prelude)
 (source (github bufordrat/mattlude)))

