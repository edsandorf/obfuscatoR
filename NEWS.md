# obfuscatoR v0.2.1
* Added website and online documentation using `pkgdown`
* Added inst/CITATION and .onAttach() now shows how to call utils::citation(obfuscatoR)
* Updated the vignette to reflect CRAN availability and CITATION.
* Corrected mathematical notation. R and L are now in italics.

# obfuscatoR v0.2.0

* Written wrappers for calc_entropy() and calc_payouts().
* All designs and entropy calculations are returned as lists for consistency.
* Added extract_attr() as a wrapper to attr() for extracting attributes of list elements.
* Users no longer need to call check_design_options(). These are checked automatically.
* Fixed a bug where the seed option in design_opt was not working (thanks to Teodora Szep).
