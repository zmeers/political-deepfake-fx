# Political Deepfakes are as Credible as Other Fake Media and (Sometimes) Real Media

This is the replication folder for POL571 at Princeton University. 

My replication of Barari, et al.'s work can be found in the replication and analysis subdirs. 

## Replication Code - Available in `original_paper` subdir
### Authors: Soubhik Barari, Christopher Lucas, and Kevin Munger

This is the replication code repository for the entitled research article (conditionally accepted at *The Journal of Politics*) which can be found [here](https://osf.io/cdfh3/).

Before replicating the analyses, make sure to create a folder `/figures` and `/tables` in the root directory of the repository.

- `deepfake.RData`: R object containing cleaned and anonymized results of three waves of the two survey experiments (**exposure** and **detection**) in `dat`, the raw but anonymized survey results in `dfsurvdat`, and clip-level results of the detection experiment (`*fakes`).

- `cps2018_crosstabs*`: cross-tabs of demographic categories from the U.S. Census's 2018 Current Population Survey used for weighting.

- `01-weight_data.R`: creates population weights for observations in `dat` object in `deepfake.RData` via a simple raking algorithm on 2018 CPS data.

- `02-prereg_analyses.R`: replicates analyses specified in pre-analysis plan; generates outputs in `/tables` and `/figures` found in Appendix of article.

- `02.1-prereg_sensitivity.R`: as requested by a reviewer, conduct sensitivity tests for some pre-registered analyses as a robustness check.

- `02.2-prereg_power.R`: as requested by a reviewer, determine whether the observed sample is actually powered (post hoc) to detect the equivalence bounds specified in the paper.

- `02.3-prereg_bounds.R`: as requested by a reviewer, determine sensitivity of pre-registered analyses to differential attrition across treatment conditions by (1) re-weighting treatment conditions to parity and (2) conducting Manski extreme bounds analyses.

- `03-paper_toplines.R`: summarise pre-registered analysis results in a series of topline tables/figures; this replicates figures found in main text of article.

- `04-supplementary_analyses.R`: replicates supplementary/exploratory analyses found in Appendix of article.

