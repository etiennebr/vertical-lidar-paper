
# Tree species, crown cover, and age as determinants of the vertical distribution of airborne LiDAR returns

This repo contains the data and the code used in the manuscript ([DOI:10.1007/s00468-021-02155-2](https://doi.org/10.1007/s00468-021-02155-2), [arXiv](https://arxiv.org/abs/2104.05057))


## Abstract 

Light detection and ranging (LiDAR) provides information on the vertical structure of forest stands enabling detailed and extensive ecosystem study. 
The vertical structure is often summarized by scalar features and data-reduction techniques that limit the interpretation of results. 
Instead, we quantified the influence of three variables, species, crown cover, and age, on the vertical distribution of airborne LiDAR returns from forest stands.
We studied 5,428 regular, even-aged stands in Quebec (Canada) with five dominant species: balsam fir (*Abies balsamea* (L.) Mill.), paper birch (*Betula papyrifera* Marsh), black spruce (*Picea mariana* (Mill.) BSP), white spruce (*Picea glauca* Moench) and aspen (*Populus tremuloides* Michx.). 
We modeled the vertical distribution against the three variables using a functional general linear model and a novel nonparametric graphical test of significance.
Results indicate that LiDAR returns from aspen stands had the most uniform vertical distribution. 
Balsam fir and white birch distributions were similar and centered at around 50% of the stand height, and black spruce and white spruce distributions were skewed to below 30% of stand height ($p$<0.001). 
Increased crown cover concentrated the distributions around 50% of stand height.
Increasing age gradually shifted the distributions higher in the stand for stands younger than 70-years, before plateauing and slowly declining at 90–120 years.
Results suggest that the vertical distributions of LiDAR returns depend on the three variables studied.

## Installation

You can install the dependencies needed to build the paper with R. 

``` r
install.packages(c("devtools", usethis))
create_from_github("etiennebr/vertical-lidar-paper", fork = TRUE) 
renv::restore()
```

## Example

You can run the analysis presented in the paper with:

``` r
usethis::edit_file("vignettes/contrasts.Rmd")
```

# Preprint

A pdf version of the preprint of the manuscript is available at https://arxiv.org/abs/2104.05057 or in [`vignettes/paper.pdf`](vignettes/paper.pdf).
