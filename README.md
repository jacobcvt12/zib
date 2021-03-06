This is a writeup of an analysis that calculates reliability of EHR data.
Programs are included to simulate data (using a mixture of betas) as well as
to perform three types of analysis

1. Beta Binomial model
2. Beta Mixture Binomial model
3. Dirichlet Process Binomial model

These programs should be run from the root of the package (i.e. `R -f
R/dp-binomial.R`) to produce output graphs in `doc/`, models in `output`, and
data in `data`. To produce the data, models, and graphs, run

```
R -f R/data.R
R -f R/beta-binomial.R
R -f R/betamix-binomial.R
R -f R/dp-binomial.R
R -f R/results.R
```

To compile the latex document, run (from `doc/`) 

```
pdflatex writeup
bibtex writeup
pdflatex writeup
pdflatex writeup
```
