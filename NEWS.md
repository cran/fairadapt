# fairadapt 1.0.0
Non-breaking changes:
- vignette updates follows the release of the article in Journal of Statistical Software

# fairadapt 0.2.7
Non-breaking changes:
- fix vignette compile issues on Debian

# fairadapt 0.2.6
Non-breaking changes:
- update of the printing methods

# fairadapt 0.2.5
Non-breaking changes:
- remove reliance on deprecated ggplot2 functionality

# fairadapt 0.2.2
Non-breaking changes:
-Addition of `fairadaptBoot()` functionality, which allows for bootstrap repetitions of the adaptation
procedure performed by `fairadapt()` (used for uncertainty quantification)
- adding `print()` and `summary()` methods for all the exported S3 classes in the package
- improving the documentation for the quantile computation functions

# fairadapt 0.2.1
Non-breaking changes:
- `fairTwins()` now has a default `train.id` of `seq_len(nrow(x$train))` (previous default was `1L`)
- `print.fairadapt()` has been changed to print the "Formula", not the "Call" (previously incorrect)

# fairadapt 0.2.0
Breaking changes:
- `protect.A` argument renamed to `prot.attr`
- argument ordering changed; `prot.attr` moved to position number two, `adj.mat`
argument moved to position three

Non-breaking changes:
- `top.ord` argument introduced, for running adaptation with topological ordering,
without the need for `adj.mat`
- `quant.method` argument now a function, allowing for custom user quantile regression
methods (via S3 dispatch)
- `visualize.graph` argument available for plotting the causal diagram
