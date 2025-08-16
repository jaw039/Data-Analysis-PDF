# Me — Data Analyst (DSC190 Projects)

Assumptions
- You are the author/owner of this repository and want a concise "Me" profile built from the included course projects and code.

Quick summary
- DSC190 coursework and analysis projects showcasing applied statistical analysis, simulation, visualization, and reproducible R workflows.
- Primary language and tooling: R (base R and common packages: knitr, dplyr, caret, randomForest, corrplot). Familiarity shown with scripting, plotting, and report generation.

Core skills
- Data cleaning and preprocessing (handling missing values, recoding, filtering, outlier removal)
- Exploratory data analysis and visualization (histograms, heatmaps, barplots, Q–Q plots, annotated plots)
- Statistical testing & inference (chi-square tests, KS tests, permutation tests, bootstrap)
- Modeling & prediction (linear/log-linear regression, random forest classification, k-means clustering)
- Simulation and resampling methods (Monte Carlo simulation, bootstrapping, permutation tests)
- Reproducible reporting (R scripts and knitted tables via knitr)

Representative methods & libraries seen in the repo
- R functions and packages: base R plotting, knitr, dplyr, caret, randomForest, corrplot
- Techniques: k-means clustering, chi-square contingency analysis, correlation analysis, Poisson/chi-square for count data, model cross-validation, prediction intervals, reverse prediction computations, robust simulation experiments

Selected projects (files & one-line descriptions)
- "Analysing the Programming Language Preferences of Data Scientists in 2020" (`FinalProject.R`, PDF): survey processing (Kaggle survey), binary encoding of multi-part answers, regional and education-level language/library analysis, chi-square tests, clustering, random-forest classification experiments.
- "Detecting Replication Patterns in HCMV through Palindrome Analysis" (`casestudy3Code.R`, PDF): simulation-based null distributions for palindrome locations, spacing analysis, Q–Q & KS tests, count/cluster analysis across genome intervals.
- "Maternal Smoking and Birth Weight" (`CaseStudy1.R`, PDF): full data cleaning pipeline, descriptive stats and visualization, group comparisons (smokers vs non-smokers), permutation tests and kurtosis analysis.
- "Statistical Analysis and Calibration of a Gamma Transmission Gauge" (`casestudy4R.R`, PDF): scatter/regression analysis with transformations, prediction intervals, reverse (inverse) prediction, robustness checks via simulation, cross-validation.
- "Video Gaming Patterns and Academic Performance" (`casestudy2Rcode.R`, PDF): bootstrapping, confidence intervals, frequency & demographic analyses, hypothesis testing (Wilcoxon), grade-distribution tests.

Data & inputs referenced
- Survey: `kaggle_survey_2020_responses.csv` (used in language preferences project)
- Text/delimited files: `hcmv.txt`, `babies.txt`, `gauge.txt`, `videodata.txt`, `videoMultiple.txt` (used across case studies)

Project highlights / strengths
- Strong practical command of classical statistical techniques and resampling methods.
- Comfortable designing simulation experiments to assess null models and prediction uncertainty.
- Reproducible scripts with clear plotting and table output suitable for coursework reports.

Edge cases & caveats (from reading the code)
- Many scripts strip or assume header rows and specific sentinel values (e.g., 99 or 999) — double-check raw data conventions before reuse.
- Some scripts expect files (e.g., `kaggle_survey_2020_responses.csv`) that are not tracked here; confirm data availability and licenses before publishing.

Suggested next steps (small, low-risk improvements)
- Add a short contact or author section (email or GitHub handle) and a photo if you want a portfolio-style bio.
- Convert the strongest project (e.g., the Kaggle survey analysis) into a nicely knitted R Markdown report and include the rendered HTML/PDF in `docs/` or the repo root.
- Add a one-page CV or `CV.pdf` and link it from this `ME.md`.
- Add brief `README.md` badges or a short summary linking each project folder to the corresponding script and PDF.

How to edit
- This file was generated automatically from the R scripts and PDFs in this repo. Update any section to add personal details (name, contact, role, or specific accomplishments).

If any of the assumptions are incorrect or you want a different tone (professional CV, personal bio, or portfolio page), tell me which style to use and I will update `ME.md` accordingly.
