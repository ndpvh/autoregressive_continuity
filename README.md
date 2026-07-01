# Continuity Revisited

This repository contains relevant data and code for the preprint "Continuity revisited: The empirical relationship between sampling rate and temporal dependence" submitted by Niels Vanhasbroeck, Henry Lo, Simran K. Johal, Lisa Peeters, Laura F. Bringmann, Marieke A. Helmich, Manuel Rein, Peer-Ole Jacobsen, Leonie Cloos, and Anabel Büchner. In this project, we investigate whether temporal dependence between psychological variables - as measured through the experience sampling method - depends on the sampling rate employed within the study. Following the idea that we are measuring a continuous-time process, we should find that the longer the time between observations, the less these observations will be associated with each other. 

For details on the data and analysis procedure we used to answer our question, we refer the interested reader to the preprint of our article ([LINK]). In this README, we'll instead detail the structure of the project.

The central premise of the project and its preliminary results came out of a hackaton organized by the Measurement is the New Black consortium (https://mitnb.org).



## Project structure

The project consists of several folders and scripts that together define the analyses performed on the data. The folders contain the following information:

- `data`: Contains the raw data used to answer our research questions. We named the data after the original authors. For more information on these data, we refer the interested reader to the relevant publications around these data (Büchner et al., preprint; Cloos et al., 2024; Dejonckheere et al., 2022). Note that not all data is available for free use (see License);
- `data_preprocessed`: Contains the preprocessed data after running script `step 0 - preprocessing.R`, ensuring that all of them contain the relevant information and have a similar data structure. These preprocessed data are used in our analyses;
- `results`: Contains the results from the various scripts. All results make mention of the dataset (`BUCHNER`, `CLOOS`, or `DEJONCKHEERE`) and the type of results they contain, specifically:
    - `estimates`: Contains the estimates of the autoregressive effects per person per item, as computed in `step 1 - estimation.R`;
    - `comparison`: Contains the results of the comparison of these autoregressive effects across conditions and with a continuous-time prediction of what this autoregressive effect should look like, as computed in `step 2 - comparison.R`;
    - `meta-...`: Contains the results of the meta-analyses performed on the comparison results, aggregating across items, as computed in `step 4 - meta analysis.R`. We distinguish between three types of comparisons, namely a comparison between conditions (`condition`), a comparison with the continuous-time estimate for the long intervals (`continuous_s2l`, reported in the preprint), and the same comparison but for the short intervals (`continuous_l2s`).
- `utility`: Contains several scripts with helper functions used in the various analysis scripts, typically called through `source()`;
- `manuscript`: Contains several things that were relevant for the manuscript when it was under preparation. Most interesting to the reader may be the script `manuscript.R` which summarizes all relevant results that were included in 
the manuscript.

The scripts that are included at the top level have the following function:

- `step 0 - preprocessing.R`: Preprocesses the raw data into data that can be used for the estimation;
- `step 1 - estimation.R`: Uses the preprocessed data to estimate lag-1 autoregressive effects for each of the items for each person within each condition (high or low sampling rate);
- `step 2 - comparison.R`: Uses the estimates and compares them (a) across condition and (b) to a continuous-time prediction derived from the Ornstein-Uhlenbeck model, the continuous-time variant of the autoregressive model often employed in the psychological literature;
- `step 3 - time intervals.R`: Compares the time between measurements across conditions, serving as a manipulation check;
- `step 4 - meta analysis.R`: Aggregates the item-specific results into a comprehensive estimate of the effect through a meta-analysis.



## Dependencies

To be able to run the code, you should install the packages we use to perform our analyses. Specifically, we use the `here` package for file referencing (Müller, 2020), `dplyr`, `tidyr`, `purrr` for data handling and computation (Wickham et al., 2023, 2024; Wickham & Henry, 2025), `metafor` for performing the meta-analysis (Viechtbauer, 2010), and `ggplot2`, `ggpubr`, and `patchwork` for visualization (Kassambara, 2023; Pederson, 2025; Wickham, 2016).



## License

The work in this repository is shared under a dual license. The GNU AGPL-3.0 license applies to the analysis code, the preprocessed data, and the raw `DEJONCKHEERE` and `CLOOS` data. The CC BY-NC 4.0 license applies to both the raw and the preprocessed `BUCHNER` data, disallowing the commercial use of these data without reference to or contacting the original authors (https://osf.io/29e63/).

`SPDX-License-Identifier: AGPL-3.0 OR CC BY-NC 4.0`



## Citation

To cite the contents of this repository, you can cite the preprint of this project:

[CITATION]



## References

Büchner, A., Götz, F., Beck, E. D., Li, L. S. Y., Yeung, A. K. L., Sun, J., Hardin, B., & Horstmann, K. T. (preprint). Capturing the temporal dynamics of personality in daily life: The Personality and Contextualized Experiences (PACE) study. *Preprint available at https://osf.io/preprints/psyarxiv/9g2sq_v1*

Cloos, L., Mestdagh, M., Vanpaemel, W., Ceulemans, E., & Kuppens, P. (2024). Measuring continuous affect in daily life with intensity profile drawings. _Assessment, 32_(5), 689-704. doi: 10.1177/10731911241266286

Dejonckheere, E., Demeyer, F., Guesens, B., Piot, M., Tuerlinckx, F., Verdonck, S., & Mestdagh, M. (2022). Assessing the reliability of single-item momentary affective measurements in experience sampling. _Psychological Assessment, 34,_ 1138-1154. doi: 10.1037/pas0001178

Kassambara A (2023). _ggpubr: 'ggplot2' Based Publication Ready Plots_. R package version 0.6.0, <https://CRAN.R-project.org/package=ggpubr>.

Müller K (2020). _here: A Simpler Way to Find Your Files_. R package version 1.0.1, <https://CRAN.R-project.org/package=here>.

Pedersen T (2025). _patchwork: The Composer of Plots_. R package version 1.3.2, <https://CRAN.R-project.org/package=patchwork>.

Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. _Journal of Statistical Software, 36_(3), 1-48. doi: 10.18637/jss.v036.i03

Wickham, H. (2016). ggplot2: Elegant graphics for data analysis. Springer-Verlag.

Wickham H, François R, Henry L, Müller K, Vaughan D (2023). _dplyr: A Grammar of Data Manipulation_. R package version 1.1.4, <https://CRAN.R-project.org/package=dplyr>.

Wickham H, Henry L (2025). _purrr: Functional Programming Tools_. R package version 1.0.4, <https://CRAN.R-project.org/package=purrr>.

Wickham H, Vaughan D, Girlich M (2024). _tidyr: Tidy Messy Data_. R package version 1.3.1, <https://CRAN.R-project.org/package=tidyr>.