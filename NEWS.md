## obfuscatoR v. 0.2.0

* Written wrappers for calc_entropy() and calc_payoutouts().
* All designs and entropy calculations are returned as lists for consistency.
* Added extract_attr() as a wrapper to attr() for extracting attributes of list elements.
* Users no longer need to call check_design_options(). These are checked automatically.
* Fixed a bug where the seed option in design_opt was not working (thanks to Teodora Szep).
