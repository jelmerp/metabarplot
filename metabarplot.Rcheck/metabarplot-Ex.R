pkgname <- "metabarplot"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "metabarplot-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('metabarplot')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("pbar")
### * pbar

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pbar
### Title: Create Taxonomic Abundance Barplots
### Aliases: pbar

### ** Examples

abund_df <- tibble::tibble(
  Sample = c("S1", "S1", "S2", "S2"),
  Phylum = c("Firmicutes", "Proteobacteria", "Firmicutes", "Proteobacteria"),
  Abundance = c(0.4, 0.6, 0.7, 0.3)
)
p <- pbar(abund_df = abund_df, taxrank = "Phylum")
p



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pbar", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ps_from_taxtable")
### * ps_from_taxtable

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ps_from_taxtable
### Title: Create a Dummy Phyloseq Object from a Taxonomy Table
### Aliases: ps_from_taxtable

### ** Examples

## Not run: 
##D x <- tibble::tibble(
##D   ASV = c("ASV1", "ASV2"),
##D   Kingdom = "Bacteria",
##D   Phylum = c("Firmicutes", "Proteobacteria")
##D )
##D ps_from_taxtable(x, tax_levels = c("Kingdom", "Phylum"))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ps_from_taxtable", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tax_stats")
### * tax_stats

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tax_stats
### Title: Build an Abundance Table from Taxonomy Data
### Aliases: tax_stats

### ** Examples

x <- tibble::tibble(
  method = "demo",
  Genus = c("A", "B", NA),
  reads = c(80, 15, 5)
)
tax_stats(x, tax_rank = "Genus", abund_column = "reads")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tax_stats", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
