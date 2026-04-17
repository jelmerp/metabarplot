# metabarplot

`metabarplot` provides helper functions to summarize taxonomic abundance data and create stacked abundance barplots for metabarcoding workflows.

## Installation

```r
remotes::install_github("jelmerp/metabarplot")
```

## Quick example

```r
library(metabarplot)

abund_df <- tibble::tibble(
  Sample = c("S1", "S1", "S2", "S2"),
  Phylum = c("Firmicutes", "Proteobacteria", "Firmicutes", "Proteobacteria"),
  Abundance = c(0.4, 0.6, 0.7, 0.3)
)

pbar(abund_df = abund_df, taxrank = "Phylum")
```
