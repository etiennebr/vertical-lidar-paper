
# Species, crown closure, and age as determinants of the vertical distribution of airborne LiDAR returns

This repos contains the code and data for the manuscript on vertical 
distribution of lidar returns


## Summary 

Tree species have different shapes and forest stand dynamics. Light detection
and ranging (LiDAR) can measure the three dimensional position of
reflective material by sending laser pulses through the canopy. We examined the 
influence of three forest stand attributes:
species, crown closure, and age on the vertical distribution of aerial
LiDAR returns of forested stands. We studied over five thousand regular,
even-aged stands in Quebec (Canada) with five dominant species: balsam
fir (*Abies balsamea* (L.) Mill.), paper birch (*Betula papyrifera*
Marsh), black spruce (*Picea mariana* (Mill.) BSP), white spruce (*Picea glauca* 
Moench) and aspen (*Populus tremuloides* Michx.). We modeled vertical distribution of LiDAR 
returns against the three attributes using a functional glm and a novel nonparametric graphical test of significance.
Results indicate that aspen stands had the most uniform vertical distribution of LiDAR returns; balsam fir and
white birch distributions were similar and centered around 50% of the
stand height; black spruce and white spruce distributions were
skewed below 30% of stand height ($p$<0.001). An increase in crown closure
concentrated the distributions around 50% height. Increasing age
gradually shifted the distributions higher in the stand until 50–70
years, where it plateaued and slowly declined at 90–120 years. The full model maximal R²
was 0.47 around 10% of stand height. Results suggest that the stands
vertical distributions of LiDAR returns depend on the three variables studied.
This understanding can be used for stand-level species classification and to study 
the evolution of the forest structure over changing conditions.

## Installation

You can install the dependencies needed to build the paper with R. 

``` r
install.packages(c("devtools", usethis))
create_from_github("etiennebr/vertical-lidar-paper", fork = TRUE) 
devtools::install_dev_deps()
```

## Example

You can run the analysis presented in the paper with:

``` r
usethis::edit_file("vignettes/contrasts.Rmd")
```

# Preprint

A pdf version of the preprint of the manuscript is available in 
[`vignette/paper.pdf`](vignette/paper.pdf).
