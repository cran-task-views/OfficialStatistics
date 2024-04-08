---
name: OfficialStatistics
topic: Official Statistics & Survey Statistics
maintainer: Matthias Templ, Alexander Kowarik, Tobias Schoch
email: matthias.templ@gmail.com
version: 2023-09-20
source: https://github.com/cran-task-views/OfficialStatistics/
---

This CRAN Task View contains a list of packages with methods typically used
in official statistics and survey statistics. Many packages provide
functions for more than one of the topics listed below. Therefore, this
list is not a strict categorization and packages may be listed more than once.

The task view is split into several parts

- First part: ["Producing Official Statistics"](#prod). This first part
  is targeted at people working at national statistical institutes,
  national banks, international organizations, etc. who are involved in
  the production of official statistics and using methods from survey statistics. It is loosely aligned to
  the ["Generic Statistical Business Process Model"](https://statswiki.unece.org/display/GSBPM).
- Second part: ["Access to Official Statistics"](#access). This second
  part's target audience is everyone interested to use official statistics
  results directly from within R.
- Third part: ["Related Methods"](#specific) shows packages that are important in official and survey statistics, but do not directly fit into the production of official statistics. It complements with a subsection on ["Miscellaneous"]("Miscellaneous") - a collection of packages that are loosely linked to official statistics or that provide limited complements to official statistics and survey methods.


# <a id="prod"></a>First Part: Production of Official Statistics

## 1 Preparations/ Management/ Planning (questionnaire design, etc.)

- `r pkg("questionr")` package contains a set of
    functions to make the processing and analysis of surveys easier. It
    provides interactive shiny apps and addins for data recoding,
    contingency tables, dataset metadata handling, and several
    convenience functions.
- `r pkg("surveydata")` makes it easy to keep
    track of metadata from surveys, and to easily extract columns with
    specific questions.
- `r pkg("blaise")` implements functions for reading and writing files
    in the Blaise Format (Statistics Netherlands).

## 2 Sampling

- `r pkg("sampling", priority = "core")` includes many different
    algorithms (Brewer, Midzuno, pps, systematic, Sampford, balanced
    (cluster or stratified) sampling via the cube method, etc.) for
    drawing survey samples and calibrating the design weights.
- `r pkg("pps")` contains functions to select
    samples using pps sampling. Also stratified simple random sampling
    is possible as well as to compute joint inclusion probabilities for
    Sampford's method of pps sampling.
- `r pkg("BalancedSampling")` provides functions to select balanced
    and spatially balanced probability samples in multi-dimensional
    spaces with any prescribed inclusion probabilities. It also includes
    the local pivot method, the cube and local cube method and a few
    more methods.
- `r pkg("PracTools")` contains functions for
    sample size calculation for survey samples using stratified or
    clustered one-, two-, and three-stage sample designs as well as
    functions to compute variance components for multistage designs and
    sample sizes in two-phase designs.
- `r pkg("surveyplanning")` includes tools for
    sample survey planning, including sample size calculation,
    estimation of expected precision for the estimates of totals, and
    calculation of optimal sample size allocation.
- `r pkg("stratification")` allows univariate
    stratification of survey populations with a generalisation of the
    Lavallee-Hidiroglou method.
- `r pkg("SamplingStrata", priority = "core")` offers an approach for
    choosing the best stratification of a sampling frame in a
    multivariate and multidomain setting, where the sampling sizes in
    each strata are determined in order to satisfy accuracy constraints
    on target estimates. To evaluate the distribution of target
    variables in different strata, information of the sampling frame, or
    data from previous rounds of the same survey, may be used.
- `r pkg("R2BEAT")` provides functions for multivariate, domain-specific optimal
    sample size allocation for one- and two-stage stratified sampling
    designs (i.e., generalization of the allocation methods of Neyman
    and Tschuprow to the case of several variables).

## 3 Data Collection (incl. record linkage)

### 3.1 Data Integration (Statistical Matching and Record Linkage)

- `r pkg("StatMatch")` provides functions to
    perform statistical matching between two data sources sharing a
    number of common variables. It creates a synthetic data set after
    matching of two data sources via a likelihood approach or via
    hot-deck.
- `r pkg("MatchIt")` allows nearest neighbor
    matching, exact matching, optimal matching and full matching amongst
    other matching methods. If two data sets have to be matched, the
    data must come as one data frame including a factor variable which
    includes information about the membership of each observation.
- `r pkg("MatchThem")` provides tools of matching
    and weighting multiply imputed datasets to control for effects of
    confounders. Multiple imputed data files from mice and amelia can be
    used directly.
- `r pkg("stringdist")` can calculate various
    string distances based on edits (damerau-levenshtein, hamming,
    levenshtein, optimal sting alignment), qgrams (q-gram, cosine,
    jaccard distance) or heuristic metrics (jaro, jaro-winkler).
- `r pkg("XBRL")` allows the extraction of
    business financial information from XBRL Documents.
- `r pkg("RecordLinkage")` implements the Fellegi-Sunter method for record
    linkage.
- `r pkg("fastLink")` implements a Fellegi-Sunter probabilistic record
    linkage model that allows for missing data and the inclusion of
    auxiliary information. Documentation can be found
    on http://imai.princeton.edu/research/linkage.html
- `r pkg("fuzzyjoin")` provides function for joining tables based on exact
    or similar matches. It allows for matching records based on inaccurate keys.
- `r pkg("PPRL")` implements privacy preserving record linkage, especially useful 
    when personal ID's cannot be used to link two data sets. This approach then
    protects the identity of persons.
- `r pkg("reclin2")` provides functions to assist in performing 
    probabilistic record linkage and deduplication.

### 3.2 Web Scraping

Web scraping is used nowadays used more frequently in the production
of official statistics. For example in price statistics, the collection
of product prices, formerly collected by hand over the web or by in person
visits to stores are replaced by scraping specific homepages. Tools
for this process step are not listed here, but a detailed overview can
be found on the CRAN task view on `r view("WebTechnologies")`.


## 4 Data Processing

### 4.1 Weighting and Calibration

- `r pkg("survey", priority = "core")` allows for
    post-stratification, generalized raking/calibration, GREG estimation
    and trimming of weights.
- `r pkg("svrep")` provides calibration tools with its function `calibrate_to_estimate` (method of Fuller 1998, raking, post-stratification) that extends package `r pkg("survey")`.
- `r pkg("sampling")`  provides the function `calib()` to calibrate for
    nonresponse (with response homogeneity
    groups) for stratified samples.
- `r pkg("laeken")` provides the function `calibWeights()`  for calibration,
    which is possibly faster (depending on the
    example) than `calib()` from `r pkg("sampling")`.
- `r pkg("icarus")` focuses on calibration and
    re-weighting in survey sampling and was designed to provide a
    familiar setting in R for users of the SAS macro `Calmar` developed by INSEE.
- `r pkg("CalibrateSSB")` include a function
    to calculate weights and estimates for panel data with non-response.
- `r pkg("Frames2")` allows point and interval
    estimation in dual frame surveys. When two probability samples (one
    from each frame) are drawn. Information collected is suitably
    combined to get estimators of the parameter of interest.
- `r pkg("surveysd", priority = "core")` provides calibration by iterative proportinal fitting,
    a calibrated bootstrap optimized for complex surveys and error
    estimation based on it.
- `r pkg("inca")` performs calibration weighting with integer weights.
- `r pkg("jointCalib")` performs a joint calibration of totals and quantiles.

### 4.2 Editing (including outlier detection)

- `r pkg("validate", priority = "core")` includes rule management and
    data validation and package `r pkg("validatetools", priority = "core")` is
    checking and simplifying sets of validation rules.
- `r pkg("errorlocate", priority = "core")` includes error
    localisation based on the principle of Fellegi and Holt. It supports
    categorical and/or numeric data and linear equalities, inequalities
    and conditional rules. The package includes a configurable backend
    for MIP-based error localization.
- `r pkg("editrules")` convert readable linear
    (in)equalities into matrix form.
- `r pkg("deducorrect")` depends on package
    `r pkg("editrules")` and applies deductive correction of
    simple rounding, typing and sign errors based on balanced edits.
    Values are changed so that the given balanced edits are fulfilled.
    To determine which values are changed the Levenstein-metric is
    applied.
- `r pkg("deductive")` allows for data correction and imputation using deductive methods.
- `r pkg("rspa")` implements functions to
    minimally adjust numerical records so they obey (in)equation
    restrictions.
- `r pkg("univOutl")` includes various methods
    for detecting univariate outliers, e.g. the Hidiroglou-Berthelot
    method.
- `r pkg("extremevalues")` is designed to detect
    univariate outliers based on modeling the bulk distribution.


### 4.3 Imputation

A general overview of imputation methods can be found in the CRAN Task View on Missing Data, `r view("MissingData")`. However, most of these presented methods do not take into account the specificities of survey's from complex designs, i.e., methods that are not specifically designed for official statistics and surveys. 
For example, the criteria for applying a method often depend on the scale of the data, which in official statistics are usually a mixture of continuous, semi-continuous, binary, categorical, and count variables. In addition, measurement error can greatly affect non-robust imputation methods.

Commonly used packages within statistical agencies are `r pkg("VIM", priority = "core")` and `r pkg("simputation")` having fast k-nearest neighbor (knn) algorithms for general distances and (robust) EM-based multiple imputation algorithms implemented.


### 4.4 Seasonal Adjustment

Seasonal adjustment is an important step in producing official statistics
and a very limited set of methodologies are used here frequently,
e.g. X13-ARIMA-SEATS developed by the US Census Bureau. In the CRAN
Task View `r view("TimeSeries")` section seasonal adjustment,
R packages for this can be found.

## 5 Analysis of Survey Data

### 5.1 Estimation and Variance Estimation

- `r pkg("survey", priority = "core")` works with
    survey samples. It allows to specify a complex survey design
    (stratified sampling design, cluster sampling, multi-stage sampling
    and pps sampling with or without replacement). Once the given survey
    design is specified within the function `svydesign()`, point and
    variance estimates can be computed. The resulting object can be used
    to estimate (Horvitz-Thompson-) totals, means, ratios and quantiles
    for domains or the whole survey sample, and to apply regression
    models. Variance estimation for means, totals and ratios can be done
    either by Taylor linearization or resampling (BRR, jackkife,
    bootstrap or user-defined).
- `r pkg("robsurvey")` provides functions for the computation of robust
    (outlier-resistant) estimators of finite population characteristics
    (means, totals, ratios, regression, etc.) using weight reduction,
    trimming, winsorization and M-estimation. The package
    complements `r pkg("survey")`.
- `r pkg("surveysd", priority = "core")` offers calibration, bootstrap and error
    estimation for complex surveys (incl. designs with rotational designs).

- `r pkg("gustave")` provides a toolkit for analytical variance
    estimation in survey sampling.
- `r pkg("vardpoor")` allows to calculate
    linearisation of several nonlinear population statistics, variance
    estimation of sample surveys by the ultimate cluster method,
    variance estimation for longitudinal and cross-sectional measures,
    and measures of change for any stage cluster sampling designs.
- `r pkg("rpms")` fits a linear model to
    survey data in each node obtained by recursively partitioning the
    data. The algorithm accounts for one-stage of stratification and
    clustering as well as unequal probability of selection.
- `r pkg("collapse")` implements advanced and
    computationally fast methods for grouped and weighted statistics and
    multi-type data aggregation (e.g. mean, variance, statistical mode
    etc.), fast (grouped, weighted) transformations of time series and
    panel data (e.g. scaling, centering, differences, growth rates), and
    fast (grouped, weighted, panel-decomposed) summary statistics for
    complex multilevel / panel data.
- `r pkg("srvyr")` is inspired by the synthetic style of the
    `dplyr` package (i.e., piping, verbs like `group_by` and
    `summarize`). It offers summary statistics for design objects of the
    `r pkg("survey")` package.
- `r pkg("weights")` provides a variety of functions for
    producing simple weighted statistics, such as weighted
    Pearson's correlations, partial correlations, Chi-Squared
    statistics, histograms and t-tests.
- `r pkg("svrep")` provides tools for creating, updating and
    analyzing survey replicate weights as an extension of `r pkg("survey")`.
    Non-response adjustments to both full-sample and replicate
    weights can be applied. Bootstrap replicate weights can be 
    created for a variety of sampling designs, including stratified multistage samples
    and samples selected using systematic or unequal probability sampling.
- `r pkg("NonProbEst")` includes different inference procedures to correct for selection
    bias that might be introduced with non-random selection mechanisms.
- `r pkg("nonprobsvy")` includes statistical inference methods with non-probability samples when auxiliary information is available from external sources such as probability samples or population totals or means.
    

### 5.2 Visualization

- `r pkg("VIM", priority = "core")` is designed to visualize missing values using suitable
    plot methods. It can be used to analyse the structure of missing values
    in microdata using univariate, bivariate, multiple and multivariate
    plots where the information of missing values from specified variables
    are highlighted in selected variables. It also comes with a
    graphical user interface.
- `r pkg("longCatEDA")` extends the matrixplot from package `r pkg("VIM")`
    to check for monotone missingness in longitudinal data.
- `r pkg("treemap")` provide treemaps. A treemap is a space-filling
    visualization of aggregates of data with hierarchical structures.
    Colors can be used to relate to highlight differences between
    comparable aggregates.
- `r pkg("tmap")` offers a layer-based way to make thematic maps,
    like choropleths and bubble maps.
- `r pkg("rworldmap")` outline how to map country referenced data and
    support users in visualizing their own data. Examples are given,
    e.g., maps for the world bank and UN. It provides also new ways
    to visualize maps.

## 6 Statistical Disclosure Control

Data from statistical agencies and other institutions are in its raw
form mostly confidential and data providers have to be ensure
confidentiality by both modifying the original data so that no
statistical units can be re-identified and by guaranteeing a minimum
amount of information loss.

### Unit-level data (microdata)

- `r pkg("sdcMicro", priority = "core")` can be used to anonymize data, i.e. to create
    anonymized files for public and scientific use. It implements a
    wide range of methods for anonymizing categorical and continuous
    (key) variables. The package also contains a graphical user
    interface, which is available by calling the function `sdcGUI`.
- `r pkg("simPop", priority = "core")` using linear and robust regression methods,
    random forests (and many more methods) to simulate synthetic data
    from given complex data. It is also suitable to produce synthetic
    data when the data have hierarchical and cluster information
    (such as persons in households) as well as when the data had been
    collected with a complex sampling design. It makes use of
    parallel computing internally.
- `r pkg("synthpop")` using regression tree methods to simulate
    synthetic data from given data. It is suitable to produce synthetic
    data when the data have no hierarchical and cluster information
    (such as households) as well as when the data does not collected
    with a complex sampling design.

### Aggregated information (tabular data)

- `r pkg("sdcTable", priority = "core")` can be used to provide
    confidential (hierarchical) tabular data. It includes the HITAS and
    the HYPERCUBE technique and uses linear programming packages (Rglpk
    and lpSolveAPI) for solving (a large amount of) linear programs.
- `r pkg("sdcSpatial")` can be used to smooth
    or/and suppress raster cells in a map. This is useful when plotting
    raster-based counts on a map.
- `r pkg("sdcHierarchies")` provides methods to
    generate, modify, import and convert nested hierarchies that are
    often used when defining inputs for statistical disclosure control
    methods.
- `r pkg("SmallCountRounding")` can be used to
    protect frequency tables by rounding necessary inner cells so that
    cross-classifications to be published are safe.
- `r pkg("GaussSuppression")` can be used to
    protect tables by suppression using the Gaussian elimination
    secondary suppression algorithm.   

### Remote access

- `r pkg("DSI")` is an interface to DataShield. DataShield is an
    infrastructure and series of R packages that enables the remote
    and non-disclosive analysis of sensitive research data.

# <a id="access"></a>Second Part: Access to Official Statistics

## Access to data from international organizations and multiple organizations

- `r pkg("OECD")` searches and extracts data from
    the OECD.
- `r pkg("Rilostat")` contains tools to download
    data from the [international labour organisation
    database](http://www.ilo.org/ilostat) together with search and
    manipulation utilities. It can also import ilostat data that are
    available on their data base in SDMX format.
- `r pkg("eurostat")` provides search for and
    access to data from Eurostat, the statistical agency for the
    European Union.
- `r pkg("ipumsr")` provides an easy way to import
    census, survey and geographic data provided by IPUMS.
- `r pkg("FAOSTAT")` can be used to download data from the
    FAOSTAT database of the Food and Agricultural Organization (FAO) of
    the United Nations.
- `r pkg("pxweb")` provides generic interface for
    the PX-Web/PC-Axis API used by many National Statistical Agencies.
- `r pkg("PxWebApiData")` provides easy API access
    to e.g. Statistics Norway, Statistics Sweden and Statistics Finland.
- `r pkg("rdhs")` interacts with The Demographic
    and Health Surveys (DHS) Program datasets.
- `r pkg("prevR")` implements functions (see
    `import.dhs()`) to import data from the
    Demographic Health Survey.
- `r pkg("rsdmx")` provides easy access to data from statistical organisations
    that support SDMX web services. The package contains a list of SDMX
    access points of various national and international statistical institutes.
- `r pkg("readsdmx")` implements functions to read SDMX into data frames
    from local SDMX-ML file or web-service. By OECD.
- `r pkg("regions")` offers tools to process regional statistics focusing on European data.    
- `r pkg("statcodelists")` makes the internationally standardized SDMX code lists available for the R user.
- `r pkg("rdbnomics")` provides access to the DB.nomics database on
    macroeconomic data from 38 official providers such as INSEE, Eurostat,
    Wolrd bank, etc.
- `r pkg("iotables")` makes input-output tables tidy, and allows for economic and environmental impact analysis with formatting the data received from the Eurostat data warehouse into appropriate, validated, matrix forms. 
- `r pkg("npi")` provides access to the API for the U.S. National Provider Identifier Registry, which is the authoritative data source for National Provider Identifier records in the healthcare domain. 
- `r pkg("WDI")` provides access to the API for the World Development Indicators gathered by the World Bank.
- `r pkg("refugees")` contains data from the [Refugee Population Statistics Database](https://www.unhcr.org/refugee-statistics/) published by the UN Refugee Agency (UNHCR).

## Access to data from national organizations

- `r pkg("tidyqwi")` provides an api for accessing
    the United States Census Bureau's Quarterly Workforce Indicator.
- `r pkg("tidyBdE")` provides access to official
    statistics provided by the Spanish Banking Authority Banco de Espana.
- `r pkg("cancensus")` provides access to Statistics
    Canada's Census data with the option to retrieve all data as
    spatial data.
- `r pkg("sorvi")` provides access to Finnish open government data.
- `r pkg("insee")` searches and extracts data from
    the Insee's BDM database.
- `r pkg("acs")` downloads, manipulates, and
    presents the American Community Survey and decennial data from the
    US Census.
- `r pkg("censusapi")` implements a wrapper for the U.S. Census Bureau APIs
    that returns data frames of Census data and meta data.
- `r pkg("idbr")` implements functions to make requests to
    the US Census Bureau's International Data Base API.
- `r pkg("tidycensus")` provides an integrated R
    interface to the decennial US Census and American Community Survey
    APIs and the US Census Bureau's geographic boundary files
- `r pkg("inegiR")`  provides access to data published by INEGI, Mexico's
    official statistics agency.
- `r pkg("cbsodataR")` provides access to
    Statistics Netherlands' (CBS) open data API.
- `r pkg("EdSurvey")` includes analysis of NCES
    Education Survey and Assessment Data.
- `r pkg("nomisr")` gives access to Nomis UK
    Labour Market Data including Census and Labour Force Survey.
- `r pkg("readabs")` implements functions to download and tidy time
    series data from the Australian Bureau of Statistics.
- `r pkg("BIFIEsurvey")` includes tools for survey
    statistics in educational assessment including data with replication
    weights (e.g. from bootstrap).
- `r pkg("CANSIM2R")` provides functions to extract CANSIM (Statistics Canada)
    tables and transform them into readily usable data.
- `r pkg("statcanR")` provides an R connection to Statistics Canada's Web
    Data Service. Open economic data (formerly CANSIM tables) are
    accessible as a data frame in the R environment.
- `r pkg("cdlTools")` provides functions to download USDA National
    Agricultural Statistics Service (NASS) cropscape data for a specified state.
- `r pkg("csodata")` provides functions to download data from Central
    Statistics Office (CSO) of Ireland.

# <a id="specific"></a>Third Part: Related Methods

## Small Area Estimation

- `r pkg("sae", priority = "core")` provides functions for small area
    estimation (basic area- and unit-level model, Fay-Herriot model with
    spatial/ temporal correlations), for example, direct estimators,
    the empirical best predictor and composite estimators.
- `r pkg("rsae")` provides functions to estimate
    the parameters of the basic unit-level small area estimation (SAE)
    model (aka nested error regression model) by means of maximum
    likelihood (ML) or robust M-estimation. On the basis of the estimated
    parameters, robust predictions of the area-specific means are
    computed (incl. MSE estimates; parametric bootstrap).
- `r pkg("emdi")` provides functions that support
    estimating, assessing and mapping regional disaggregated indicators.
    So far, estimation methods comprise direct estimation, the
    model-based unit-level approach Empirical Best Prediction, the
    area-level model and various extensions of it, as well as their
    precision estimates. The assessment of the used model is supported
    by a summary and diagnostic plots. For a suitable presentation of
    estimates, map plots can be easily created and exported.
- `r pkg("hbsae")` provides functions to
    compute small area estimates based on a basic area or unit-level
    model. The model is fit using restricted maximum likelihood, or in a
    hierarchical Bayesian way. Auxiliary information can be either counts
    resulting from categorical variables or means from continuous
    population information.
- `r pkg("SAEval")` provides diagnostics and graphic tools for the
    evaluation of small area estimators
- `r pkg("mind")` provides multivariate prediction and inference
    (mean square error) for domains using mixed linear models as proposed
    in Datta, Day, and Basawa (1999, J. Stat. Plan. Inference)
- `r pkg("JoSAE")` provides point and variance
    estimation for the generalized regression (GREG) and a unit level
    empirical best linear unbiased prediction EBLUP estimators can be
    made at domain level. It basically provides wrapper functions to the
    `r pkg("nlme")` package that is used to fit the basic
    random effects models.
- `r pkg("SUMMER")` SUMMER: provides small area estimation unit and area models and methods for spatial and spatio-temporal smoothing of demographic and health indicators using survey data

## Microsimulation

- `r pkg("simPop", priority = "core")` allows to produce synthetic population data, sometimes
    needed as a starting population for microsimulations.
- `r pkg("sms")` provides facilities to simulate
    micro-data from given area-based macro-data. Simulated annealing is
    used to best satisfy the available description of an area. For
    computational issues, the calculations can be run in parallel mode.
- `r pkg("saeSim")` implements tools for the simulation of
    data in the context of small area estimation.
- `r pkg("SimSurvey")` simulates age-structured
    spatio-temporal populations given built-in or user-defined sampling
    protocols.


## Indices, Indicators, Tables and Visualization of Indicators

- `r pkg("laeken")` provides functions to estimate
    popular risk-of-poverty and inequality indicators
    (at-risk-of-poverty rate, quintile share ratio, relative median
    risk-of-poverty gap, Gini coefficient). In addition, standard and
    robust methods for tail modeling of Pareto distributions are
    provided for semi-parametric estimation of indicators from
    continuous univariate distributions such as income variables.
- `r pkg("convey")` estimates variances on
    indicators of income concentration and poverty using familiar
    linearized and replication-based designs created by the
    `r pkg("survey")` package such as the Gini coefficient,
    Atkinson index, at-risk-of-poverty threshold, and more than a dozen
    others.
- `r pkg("ineq")` computes various inequality
    measures (Gini, Theil, entropy, among others), concentration
    measures (Herfindahl, Rosenbluth), and poverty measures (Watts, Sen,
    SST, and Foster). It also computes and draws empirical and
    theoretical Lorenz curves as well as Pen's parade. It is not
    designed to deal with sampling weights directly (these could only be
    emulated via `rep(x, weights)`).
- `r pkg("wINEQ")` fills the gap of `r pkg("ineq")` and allows for sampling weights directly.
   It contains various inequality measures such as Gini, Theil, Leti index, Palma ratio, 20:20 ratio, Allison and Foster index, Jenkins index, Cowell and Flechaire index, Abul Naga and Yalcin index, Apouey index, Blair and Lacy index. 
- `r pkg("DHS.rates")` estimates key indicators
    (especially fertility rates) and their variances for the Demographic
    and Health Survey (DHS) data.
- `r pkg("micEconIndex")` implements functions to compute prices indices
    (of type Paasche, Fisher and Laspeyres); see `priceIndex()`. For
    estimating quantities (of goods, for example) see function
    `quantityIndex()`.


# <a id="misc"></a>Miscellaneous

- `r pkg("samplingbook")` includes sampling
    procedures from the book 'Stichproben. Methoden und praktische
    Umsetzung mit R' by Goeran Kauermann and Helmut Kuechenhoff (2010).
- `r pkg("SDaA")` is designed to reproduce results
    from Lohr, S. (1999) 'Sampling: Design and Analysis, Duxbury' and
    includes the data sets from this book.
- `r pkg("samplingVarEst")` implements Jackknife methods for variance
    estimation of unequal probability with one or two stage designs.
- `r pkg("memisc")` includes tools for the
    management of survey data, graphics and simulation.
- `r pkg("anesrake")` provides a comprehensive
    system for selecting variables and weighting data to match the
    specifications of the American National Election Studies.
- `r pkg("spsurvey")` includes facilities for
    spatial survey design and analysis for equal and unequal probability
    (stratified) sampling.
- `r pkg("FFD")` provides function to calculate
    optimal sample sizes of a population of animals living in herds for
    surveys to substantiate freedom from disease. The criteria of
    estimating the sample sizes take the herd-level clustering of
    diseases as well as imperfect diagnostic tests into account and
    select the samples based on a two-stage design. Inclusion
    probabilities are not considered in the estimation. The package
    provides a graphical user interface as well.
- `r pkg("mipfp")` provides multidimensional iterative
    proportional fitting to calibrate n-dimensional arrays given target
    marginal tables.
- `r pkg("MBHdesign")` provides spatially balanced
    designs from a set of (contiguous) potential sampling locations in a
    study region.
- `r pkg("quantification")` provides different
    functions for quantifying qualitative survey data. It supports the
    Carlson-Parkin method, the regression approach, the balance approach
    and the conditional expectations method.
- `r pkg("surveybootstrap")` includes tools for using
    different kinds of bootstrap for estimating sampling variation using
    complex survey data.
- `r pkg("RRreg")` implements univariate and
    multivariate analysis (correlation, linear, and logistic regression)
    for several variants of the randomized response technique, a survey
    method for eliminating response biases due to social desirability.
- `r pkg("RRTCS")` includes randomized response
    techniques for complex surveys.
- `r pkg("panelaggregation")` aggregates business
    tendency survey data (and other qualitative surveys) to time series
    at various aggregation levels.
- `r pkg("rtrim")` implements functions to study trends and indices for
    monitoring data. It provides tools for estimating animal/plant
    populations based on site counts, including occurrence of missing data.
- `r pkg("rjstat")`. Read and write data sets in the JSON-stat format.
- `r pkg("diffpriv")` implements the perturbation of statistics with
    differential privacy.
- `r pkg("easySdcTable")` provides a graphical interface to a small selection of functionality of package `r pkg("sdcTable")`.
- `r pkg("MicSim")` includes methods for
    microsimulations. Given a initial population, mortality rates,
    divorce rates, marriage rates, education changes, etc. and their
    transition matrix can be defined and included for the simulation of
    future states of the population. The package does not contain
    compiled code but functionality to run the microsimulation in
    parallel is provided.
- `r pkg("singleRcapture")` provides methods to estimate the population size of hard-to-reach populations using single-source capture-recapture methods. 
