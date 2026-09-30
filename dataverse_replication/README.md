# Political Deepfakes are as Credible as Other Fake Media and (Sometimes) Real Media
## Replication Code
### Authors: Soubhik Barari, Christopher Lucas, and Kevin Munger

The entire analysis can be replicated in docker by running the following commands:

1. `docker build -t user/deepfakes docker_image`: Runs all analyses with appropriate versions of all dependencies (runtime is about an hour). If this command throws an error relating to a missing dependency, the installation step timed out (rerun this command with a faster/more stable internet connection).
2. `docker run --name my_container user/deepfakes`: Starts a container from the image created in step 1.
3. `docker cp my_container:/home/archive/ /tmp`: Copies the output of the analysis from the container to the local host directory `/tmp`.

You can also run the analysis without docker, though may encounter software version conflicts. Before doing so, create the additional, empty directories that are made in `docker_image/dockerfile`. The different scripts and necessary data are located in `docker_image`, and are as follows: 

- `/intermediate`: directory containing R object `deepfake_00.RData`, which includes cleaned and anonymized results from the survey experiments (**exposure** and **detection**) in `dat`, the raw but anonymized survey results in `dfsurvdat`, and clip-level results of the detection experiment (`*fakes`). This was generated from `00-deepfake_make_data.R`, which is not run in this archive because the inputs contain PII. `01-weight_data.R` writes an updated version of this data including survey weights. 

- `/supplemental_data`: contains data from the CPS used to create survey weights.

- `01-weight_data.R`: creates population weights for observations in `dat` object in `deepfake.RData` via a simple raking algorithm on 2018 CPS data.

- `02-prereg_analyses.R`: replicates analyses specified in pre-analysis plan; generates outputs in `/tables` and `/figures` found in Appendix of article.

- `02.1-prereg_sensitivity.R`: as requested by a reviewer, conduct sensitivity tests for some pre-registered analyses as a robustness check. This script also requires PII and is not run in this replication archive but is included for completeness.

- `02.2-prereg_power.R`: as requested by a reviewer, determine whether the observed sample is actually powered (post hoc) to detect the equivalence bounds specified in the paper.

- `02.3-prereg_bounds.R`: as requested by a reviewer, determine sensitivity of pre-registered analyses to differential attrition across treatment conditions by (1) re-weighting treatment conditions to parity and (2) conducting Manski extreme bounds analyses.

- `03-paper_toplines.R`: summarise pre-registered analysis results in a series of topline tables/figures; this replicates figures found in main text of article.

- `04-supplementary_analyses.R`: replicates supplementary/exploratory analyses found in Appendix of article.

