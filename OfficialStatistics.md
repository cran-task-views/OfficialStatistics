---
name: OfficialStatistics
topic: Official Statistics & Survey Statistics
maintainer: Matthias Templ
email: matthias.templ@gmail.com
version: 2021-12-29
source: https://github.com/cran-task-views/OfficialStatistics/
---


This CRAN task view contains a list of packages that include methods
typically used in official statistics and survey methodology. Many
packages provide functionality for more than one of the topics listed
below. Therefore this list is not a strict categorization and packages
can be listed more than once. Certain data import/export facilities
regarding to often used statistical software tools like SPSS, SAS or
Stata are mentioned in the end of the task view.

### Complex Survey Design: Sampling and Sample Size Calculation

-   Package `r pkg("sampling")` includes many different
    algorithms (Brewer, Midzuno, pps, systematic, Sampford, balanced
    (cluster or stratified) sampling via the cube method, etc.) for
    drawing survey samples and calibrating the design weights.
-   R package `r pkg("surveyplanning")` includes tools for
    sample survey planning, including sample size calculation,
    estimation of expected precision for the estimates of totals, and
    calculation of optimal sample size allocation.
-   The `r pkg("pps")` package contains functions to select
    samples using pps sampling. Also stratified simple random sampling
    is possible as well as to compute joint inclusion probabilities for
    Sampford's method of pps sampling.
-   Package `r pkg("stratification")` allows univariate
    stratification of survey populations with a generalisation of the
    Lavallee-Hidiroglou method.
-   Package `r pkg("SamplingStrata")` offers an approach for
    choosing the best stratification of a sampling frame in a
    multivariate and multidomain setting, where the sampling sizes in
    each strata are determined in order to satisfy accuracy constraints
    on target estimates. To evaluate the distribution of target
    variables in different strata, information of the sampling frame, or
    data from previous rounds of the same survey, may be used.
-   The package `r pkg("BalancedSampling")` selects balanced
    and spatially balanced probability samples in multi-dimensional
    spaces with any prescribed inclusion probabilities. It also includes
    the local pivot method, the cube and local cube method and a few
    more methods.
-   Package `r pkg("PracTools")` contains functions for
    sample size calculation for survey samples using stratified or
    clustered one-, two-, and three-stage sample designs as well as
    functions to compute variance components for multistage designs and
    sample sizes in two-phase designs.

**Complex Survey Design: Point and Variance Estimation and Model
Fitting**

-   Package `r pkg("survey", priority = "core")` works with
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
-   The methods from the `r pkg("survey")` package are
    called from package `r pkg("srvyr")` using the dplyr
    syntax, i.e., piping, verbs like `group_by` and `summarize`, and
    other dplyr-inspired syntactic style when calculating summary
    statistics on survey data.
-   Package `r pkg("convey")` extends package
    `r pkg("survey")` \-- see the topic about indicators
    below.
-   Package `r pkg("laeken")` provides functions to estimate
    certain Laeken indicators (at-risk-of-poverty rate, quintile share
    ratio, relative median risk-of-poverty gap, Gini coefficient)
    including their variance for domains and strata using a calibrated
    bootstrap.
-   The `r pkg("lavaan.survey")` package provides a wrapper
    function for packages `r pkg("survey")` and
    `r pkg("lavaan")`. It can be used for fitting structural
    equation models (SEM) on samples from complex designs. Using the
    design object functionality from package
    `r pkg("survey")`, lavaan objects are re-fit (corrected)
    with the `lavaan.survey()` function of package
    `r pkg("lavaan.survey")`. This allows for the
    incorporation of clustering, stratification, sampling weights, and
    finite population corrections into a SEM analysis. `lavaan.survey()`
    also accommodates replicate weights and multiply imputed datasets.
-   Package `r pkg("vardpoor")` allows to calculate
    linearisation of several nonlinear population statistics, variance
    estimation of sample surveys by the ultimate cluster method,
    variance estimation for longitudinal and cross-sectional measures
    and measures of change for any stage cluster sampling designs.
-   The package `r pkg("rpms")` fits a linear model to
    survey data in each node obtained by recursively partitioning the
    data. The algorithm accounts for one-stage of stratification and
    clustering as well as unequal probability of selection.
-   Package `r pkg("collapse")` implements advanced and
    computationally fast methods for grouped and weighted statistics and
    multi-type data aggregation (e.g. mean, variance, statistical mode
    etc.), fast (grouped, weighted) transformations of time series and
    panel data (e.g. scaling, centering, differences, growth rates), and
    fast (grouped, weighted, panel-decomposed) summary statistics for
    complex multilevel / panel data.
-   Package `r pkg("weights")` provides a variety of
    functions for producing simple weighted statistics, such as weighted
    Pearson's correlations, partial correlations, Chi-Squared
    statistics, histograms and t-tests.

### Complex Survey Design: Calibration

-   Package `r pkg("survey")` allows for
    post-stratification, generalized raking/calibration, GREG estimation
    and trimming of weights.
-   The `calib()` function in package `r pkg("sampling")`
    allows to calibrate for nonresponse (with response homogeneity
    groups) for stratified samples.
-   The `calibWeights()` function in package
    `r pkg("laeken")` is a possible faster (depending on the
    example) implementation of parts of `calib()` from package
    `r pkg("sampling")`.
-   Package `r pkg("icarus")` focuses on calibration and
    reweighting in survey sampling and was designed to provide a
    familiar setting in R for user of the SAS macro
    `             Calmar           `.
-   Package `r pkg("reweight")` allows for calibration of
    survey weights for categorical survey data so that the marginal
    distributions of certain variables fit more closely to those from a
    given population, but does not allow complex sampling designs.
-   The package `r pkg("CalibrateSSB")` include a function
    to calculate weights and estimates for panel data with non-response.
-   Package `r pkg("Frames2")` allows point and interval
    estimation in dual frame surveys. When two probability samples (one
    from each frame) are drawn. Information collected is suitably
    combined to get estimators of the parameter of interest.

### Editing and Visual Inspection of Microdata

Editing tools:

-   Package `r pkg("validate")` includes rule management and
    data validation and package `r pkg("validatetools")` is
    checking and simplifying sets of validation rules.
-   Package `r pkg("errorlocate")` includes error
    localisation based on the principle of Fellegi and Holt. It supports
    categorical and/or numeric data and linear equalities, inequalities
    and conditional rules. The package includes a configurable backend
    for MIP-based error localization.
-   Package `r pkg("editrules")` convert readable linear
    (in)equalities into matrix form.
-   Package `r pkg("deducorrect")` depends on package
    `r pkg("editrules")` and applies deductive correction of
    simple rounding, typing and sign errors based on balanced edits.
    Values are changed so that the given balanced edits are fulfilled.
    To determine which values are changed the Levenstein-metric is
    applied.
-   The package `r pkg("rspa")` implements functions to
    minimally adjust numerical records so they obey (in)equation
    restrictions.

Visual tools:

-   Package `r pkg("VIM")` is designed to visualize missing
    values using suitable plot methods. It can be used to analyse the
    structure of missing values in microdata using univariate,
    bivariate, multiple and multivariate plots where the information of
    missing values from specified variables are highlighted in selected
    variables. It also comes with a graphical user interface.
-   Package `r pkg("longCatEDA")` extends the matrixplot
    from package `r pkg("VIM")` to check for monotone
    missingness in longitudinal data.
-   Package `r pkg("treemap")` provide treemaps. A treemap
    is a space-filling visualization of aggregates of data with
    hierarchical structures. Colors can be used to relate to highlight
    differences between comparable aggregates.

### Imputation

A distinction between iterative model-based methods, k-nearest neighbor
methods and miscellaneous methods is made. However, often the criteria
for using a method depend on the scale of the data, which in official
statistics are typically a mixture of continuous, semi-continuous,
binary, categorical and count variables. In addition, measurement errors
may corrupt non-robust imputation methods. Note that only few imputation
methods can deal with mixed types of variables and only few methods
account for robustness issues.

EM-Based Imputation Methods:

-   Package `r pkg("mi")` provides iterative EM-based
    multiple Bayesian regression imputation of missing values and model
    checking of the regression models used. The regression models for
    each variable can also be user-defined. The data set may consist of
    continuous, semi-continuous, binary, categorical and/or count
    variables.
-   Package `r pkg("mice")` provides iterative EM-based
    multiple regression imputation. The data set may consist of
    continuous, binary, categorical and/or count variables.
-   Package `r pkg("mitools")` provides tools to perform
    analyses and combine results from multiply-imputed datasets.
-   Package `r pkg("Amelia")` provides multiple imputation
    where first bootstrap samples with the same dimensions as the
    original data are drawn, and then used for EM-based imputation. It
    is also possible to impute longitudinal data. The package in
    addition comes with a graphical user interface.
-   Package `r pkg("VIM")` provides EM-based multiple
    imputation (function `irmi()`) using robust estimations, which
    allows to adequately deal with data including outliers. It can
    handle data consisting of continuous, semi-continuous, binary,
    categorical and/or count variables.
-   Single imputation methods are included or called from other packages
    by the package `r pkg("simputation")`. It supports
    regression (standard, M-estimation, ridge/lasso/elasticnet),
    hot-deck methods (powered by VIM), randomForest, EM-based, and
    iterative randomForest imputation.
-   Package `r pkg("mix")` provides iterative EM-based
    multiple regression imputation. The data set may consist of
    continuous, binary or categorical variables, but methods for
    semi-continuous variables are missing.
-   Package `r pkg("pan")` provides multiple imputation for
    multivariate panel or clustered data.
-   Package `r pkg("norm")` provides EM-based multiple
    imputation for multivariate normal data.
-   Package `r pkg("cat")` provides EM-based multiple
    imputation for multivariate categorical data.
-   Package `r pkg("MImix")` provides tools to combine
    results for multiply-imputed data using mixture approximations.
-   Package `r pkg("missForest")` uses the functionality of
    the randomForest to impute missing values in an iterative
    single-imputation fashion. It can deal with almost any kind of
    variables except semi-continuous ones. Even the underlying bootstrap
    approach of random forests ensures that from multiple runs one can
    get multiple imputations but the additional uncertainty of
    imputation is only considered when choosing the random forest method
    of package `r pkg("mice")`.

Nearest Neighbor Imputation Methods

-   Package `r pkg("VIM")` provides an implementation of the
    popular sequential and random (within a domain) hot-deck algorithm.
-   `r pkg("VIM")` also provides a fast k-nearest neighbor
    (knn) algorithm which can be used for large data sets. It uses a
    modification of the Gower Distance for numerical, categorical,
    ordered, continuous and semi-continuous variables.
-   Package `r pkg("yaImpute")` performs popular nearest
    neighbor routines for imputation of continuous variables where
    different metrics and methods can be used for determining the
    distance between observations.
-   Package `r bioc("impute")` on Bioconductor impute
    provides knn imputation of continuous variables.

Copula-Based Imputation Methods:

-   The S4 class package `r pkg("CoImp")` imputes
    multivariate missing data by using conditional copula functions. The
    imputation procedure is semiparametric: the margins are
    non-parametrically estimated through local likelihood of low-degree
    polynomials while a range of different parametric models for the
    copula can be selected by the user. The missing values are imputed
    by drawing observations from the conditional density functions by
    means of the Hit or Miss Monte Carlo method. It works either for a
    matrix of continuous scaled variables or a matrix of discrete
    distributions.

Miscellaneous Imputation Methods:

-   Package `r pkg("missMDA")` allows to impute incomplete
    continuous variables by principal component analysis (PCA) or
    categorical variables by multiple correspondence analysis (MCA).
-   Package `r pkg("mice")` (function `mice.impute.pmm()`)
    and Package `r pkg("Hmisc")` (function `aregImpute()`)
    allow predictive mean matching imputation.
-   Package `r pkg("VIM")` allows to visualize the structure
    of missing values using suitable plot methods. It also comes with a
    graphical user interface.

### Statistical Disclosure Control

Data from statistical agencies and other institutions are in its raw
form mostly confidential and data providers have to be ensure
confidentiality by both modifying the original data so that no
statistical units can be re-identified and by guaranteeing a minimum
amount of information loss.

-   Package `r pkg("sdcMicro")` can be used to anonymize
    data, i.e. for the generation of public- and scientific-use files.
    It implements a wide range of methods for the anonymization of
    categorical and continuous (key) variables. The package also comes
    with a graphical user interface.
-   Package `r pkg("sdcTable")` can be used to provide
    confidential (hierarchical) tabular data. It includes the HITAS and
    the HYPERCUBE technique and uses linear programming packages (Rglpk
    and lpSolveAPI) for solving (a large amount of) linear programs.
-   Package `r pkg("sdcSpatial")` can be used to smooth
    or/and suppress raster cells in a map. This is useful when plotting
    raster-based counts on a map.
-   An interface to the package `r pkg("sdcTable")` is
    provided by package `r pkg("easySdcTable")`.
-   Package `r pkg("sdcHierarchies")` provides methods to
    generate, modify, import and convert nested hierarchies that are
    often used when defining inputs for statistical disclosure control
    methods.
-   Package `r pkg("SmallCountRounding")` can be used to
    protect frequency tables by rounding necessary inner cells so that
    cross-classifications to be published are safe.

### Seasonal Adjustment and Forecasting

For a more general view on time series methodology we refer to the
`r view("TimeSeries")` task view. Only very specialized time
series packages related to complex surveys are discussed here.

-   Decomposition of time series can be done with the function
    `decompose()`, or more advanced by using the function `stl()`, both
    from the basic stats package. Decomposition is also possible with
    the `StructTS()` function, which can also be found in the stats
    package.
-   Many powerful tools can be accessed via packages
    `r pkg("x12")` and `r pkg("seasonal")`.
    `r pkg("x12")` provides a wrapper function for the X12
    binaries, which have to be installed first. It uses with a S4-class
    interface for batch processing of multiple time series. Less
    functionality but with the support of SEATS Spec is supported by
    package `r pkg("seasonal")`.
-   A wrapper to JDemetra plus is provided by
    `r pkg("RJDemetra")`. It offers a full access to all
    options and outputs from JDemetra plus including the two seasonal
    adjustment methods from TRAMO/SEATS and X12/X13 Arima SEATS.
-   Given the large pool of individual forecasts in survey-type
    forecasting, forecast combination techniques from package
    `r pkg("GeomComb")` can be useful. It can also handle
    missing values in the time series.

### Statistical Matching and Record Linkage

-   Package `r pkg("StatMatch")` provides functions to
    perform statistical matching between two data sources sharing a
    number of common variables. It creates a synthetic data set after
    matching of two data sources via a likelihood approach or via
    hot-deck.
-   Package `r pkg("MatchIt")` allows nearest neighbor
    matching, exact matching, optimal matching and full matching amongst
    other matching methods. If two data sets have to be matched, the
    data must come as one data frame including a factor variable which
    includes information about the membership of each observation.
-   Package `r pkg("MatchThem")` provides tools of matching
    and weighting multiply imputed datasets to control for effects of
    confounders. Multiple imputed data files from mice and amelia can be
    used directly.
-   Package `r pkg("stringdist")` can calculate various
    string distances based on edits (damerau-levenshtein, hamming,
    levenshtein, optimal sting alignment), qgrams (q-gram, cosine,
    jaccard distance) or heuristic metrics (jaro, jaro-winkler).
-   Package `r pkg("reclin")` is a record linkage toolkit to
    assist in performing probabilistic record linkage and deduplication
-   Package `r pkg("XBRL")` allows the extraction of
    business financial information from XBRL Documents.

### Small Area Estimation

-   Package `r pkg("sae")` include functions for small area
    estimation, for example, direct estimators, the empirical best
    predictor and composite estimators.
-   Package `r pkg("rsae")` provides functions to estimate
    the parameters of the basic unit-level small area estimation (SAE)
    model (aka nested error regression model) by means of maximum
    likelihood (ML) or robust ML. On the basis of the estimated
    parameters, robust predictions of the area-specific means are
    computed (incl. MSE estimates; parametric bootstrap). The current
    version (rsae 0.4-x) does not allow for categorical independent
    variables.
-   Package `r pkg("emdi")` contains functions that support
    estimating, assessing and mapping regional disaggregated indicators.
    So far, estimation methods comprise direct estimation, the
    model-based unit-level approach Empirical Best Prediction, the
    area-level model and various extensions of it, as well as their
    precision estimates. The assessment of the used model is supported
    by a summary and diagnostic plots. For a suitable presentation of
    estimates, map plots can be easily created and exported
-   Package `r pkg("nlme")` provides facilities to fit
    Gaussian linear and nonlinear mixed-effects models and
    `r pkg("lme4")` provides facilities to fit linear and
    generalized linear mixed-effects model, both used in small area
    estimation.
-   The `r pkg("hbsae")` package provides functions to
    compute small area estimates based on a basic area or unit-level
    model. The model is fit using restricted maximum likelihood, or in a
    hierarchical Bayesian way. Auxilary information can be either counts
    resulting from categorical variables or means from continuous
    population information.
-   With package `r pkg("JoSAE")` point and variance
    estimation for the generalized regression (GREG) and a unit level
    empirical best linear unbiased prediction EBLUP estimators can be
    made at domain level. It basically provides wrapper functions to the
    `r pkg("nlme")` package that is used to fit the basic
    random effects models.
-   The package `r pkg("BayesSAE")` also allows for Bayesian
    methods range from the basic Fay-Herriot model to its improvement
    such as You-Chapman models, unmatched models, spatial models and so
    on.

### Indices, Indicators, Tables and Visualisation of Indicators

-   Package `r pkg("laeken")` provides functions to estimate
    popular risk-of-poverty and inequality indicators
    (at-risk-of-poverty rate, quintile share ratio, relative median
    risk-of-poverty gap, Gini coefficient). In addition, standard and
    robust methods for tail modeling of Pareto distributions are
    provided for semi-parametric estimation of indicators from
    continuous univariate distributions such as income variables.
-   Package `r pkg("convey")` estimates variances on
    indicators of income concentration and poverty using familiar
    linearized and replication-based designs created by the
    `r pkg("survey")` package such as the Gini coefficient,
    Atkinson index, at-risk-of-poverty threshold, and more than a dozen
    others.
-   Package `r pkg("ineq")` computes various inequality
    measures (Gini, Theil, entropy, among others), concentration
    measures (Herfindahl, Rosenbluth), and poverty measures (Watts, Sen,
    SST, and Foster). It also computes and draws empirical and
    theoretical Lorenz curves as well as Pen's parade. It is not
    designed to deal with sampling weights directly (these could only be
    emulated via `rep(x, weights)`).
-   Package `r pkg("IC2")` include three inequality indices:
    extended Gini, Atkinson and Generalized Entropy. It can deal with
    sampling weights and subgroup decomposition is supported.
-   Package `r pkg("DHS.rates")` estimates key indicators
    (especially fertility rates) and their variances for the Demographic
    and Health Survey (DHS) data.
-   Functions `priceIndex()` from package
    `r pkg("micEconIndex")` allows to estimate the Paasche,
    the Fisher and the Laspeyres price indices. For estimating
    quantities (of goods, for example), function `quantityIndex()` might
    be your friend.
-   Package `r pkg("tmap")` offers a layer-based way to make
    thematic maps, like choropleths and bubble maps.
-   Package `r pkg("rworldmap")` outline how to map country
    referenced data and support users in visualising their own data.
    Examples are given, e.g., maps for the world bank and UN. It
    provides also new ways to visualise maps.
-   Package `r pkg("rrcov3way")` provides robust methods for
    multiway data analysis, applicable also for compositional data.

### Microsimulation and Synthetic Data

-   The `r pkg("MicSim")` package includes methods for
    microsimulations. Given a initial population, mortality rates,
    divorce rates, marriage rates, education changes, etc. and their
    transition matrix can be defined and included for the simulation of
    future states of the population. The package does not contain
    compiled code but functionality to run the microsimulation in
    parallel is provided.
-   Package `r pkg("sms")` provides facilities to simulate
    micro-data from given area-based macro-data. Simulated annealing is
    used to best satisfy the available description of an area. For
    computational issues, the calculations can be run in parallel mode.
-   Package `r pkg("simPop")` using linear and robust
    regression methods, random forests (and many more methods) to
    simulate synthetic data from given complex data. It is also suitable
    to produce synthetic data when the data have hierarchical and
    cluster information (such as persons in households) as well as when
    the data had been collected with a complex sampling design. It makes
    use of parallel computing internally.
-   Package `r pkg("synthpop")` using regression tree
    methods to simulate synthetic data from given data. It is suitable
    to produce synthetic data when the data have no hierarchical and
    cluster information (such as households) as well as when the data
    does not collected with a complex sampling design.
-   Package `r pkg("saeSim")` Tools for the simulation of
    data in the context of small area estimation.
-   Package `r pkg("SimSurvey")` simulates age-structured
    spatio-temporal populations given built-in or user-defined sampling
    protocols.

### Additional Packages and Functionalities

Various additional packages are available that provides certain
functionality useful in official statistics and survey methodology.

-   The `r pkg("questionr")` package contains a set of
    functions to make the processing and analysis of surveys easier. It
    provides interactive shiny apps and addins for data recoding,
    contingency tables, dataset metadata handling, and several
    convenience functions.

Data Import and Export:

-   Package `r pkg("SAScii")` imports ASCII files directly
    into R using only a SAS input script, which is parsed and converted
    into arguments for a read.fwf call. This is useful whenever SAS
    scripts for importing data are already available.
-   The `r pkg("foreign")` package includes tools for
    reading data from SAS Xport (function `read.xport()`), Stata
    (function `read.dta()`), SPSS (function `read.spss()`) and various
    other formats. It provides facilities to write file to various
    formats, see function `write.foreign()`.
-   Also the package `r pkg("haven")` imports and exports
    SAS, Stata and SPSS (function `read.spss()`) files. The package is
    more efficient for loading heavy data sets and it handles the
    labelling of variables and values in an advanced manner.
-   Also the package `r pkg("Hmisc")` provides tools to read
    data sets from SPSS (function `spss.get()`) or Stata (function
    `stata.get()`).
-   The `r pkg("pxR")` package provides a set of functions
    for reading and writing PC-Axis files, used by different statistical
    organizations around the globe for dissemination of their
    (multidimensional) tables.
-   With package `r pkg("prevR")` and it's function
    `import.dhs()` it is possible to directly imports data from the
    Demographic Health Survey.
-   Function `describe()` from package `r pkg("questionr")`
    describes the variables of a dataset that might include labels
    imported with the foreign or memisc packages.
-   Package `r pkg("OECD")` searches and extracts data from
    the OECD.
-   Package `r pkg("tidyqwi")` provides an api for accessing
    the United States Census Bureau's Quartely Workforce Indicator.
-   Package `r pkg("tidyBdE")` provides access to official
    statistics provided by the Spanish Banking Authority Banco de Espana
-   `r pkg("cancensus")` provides access to Statistics
    Canada's Census data with the option to retrieve all data as
    spatial data.
-   Package `r pkg("Rilostat")` contains tools to download
    data from the [international labour organisation
    database](http://www.ilo.org/ilostat) together with search and
    manipulation utilities. It can also import ilostat data that are
    available on their data base in SDMX format.
-   Package `r pkg("eurostat")` provides search for and
    access to data from Eurostat, the statistical agency for the
    European Union.
-   Access to Finnish open government data is provided by package
    `r pkg("sorvi")`
-   Package `r pkg("insee")` searches and extracts data from
    the Insee's BDM database.
-   Package `r pkg("acs")` downloads, manipulates, and
    presents the American Community Survey and decennial data from the
    US Census.
-   A wrapper for the U.S. Census Bureau APIs that returns data frames
    of Census data and metadata is implemented in package
    `r pkg("censusapi")`.
-   Package `r pkg("censusGeography")` converts spefific
    United States Census geographic code for city, state (FIP and ICP),
    region, and birthplace.
-   With package `r pkg("idbr")` you can to make requests to
    the US Census Bureau's International Data Base API.
-   Package `r pkg("ipumsr")` provides an easy way to import
    census, survey and geographic data provided by IPUMS.
-   Package `r pkg("tidycensus")` provides an integrated R
    interface to the decennial US Census and American Community Survey
    APIs and the US Census Bureau's geographic boundary files
-   Access to data published by INEGI, Mexico's official statistics
    agency, is supported by package `r pkg("inegiR")`
-   Package `r pkg("cbsodataR")` provides access to
    Statistics Netherlands' (CBS) open data API.
-   Package `r pkg("EdSurvey")` includes analysis of NCES
    Education Survey and Assessment Data
-   Package `r pkg("nomisr")` gives access to Nomis UK
    Labour Market Data including Census and Labour Force Survey
-   Package `r pkg("readabs")` to download and tidy time
    series data from the Australian Bureau of Statistics
    https://cran.r-project.org/package=readabs
-   Package `r pkg("FAOSTAT")` to download data from the
    FAOSTAT database of the Food and Agricultural Organization (FAO) of
    the United Nations
-   Package `r pkg("pxweb")` provides generic interface for
    the PX-Web/PC-Axis API used by many National Statistical Agencies.
-   Package `r pkg("PxWebApiData")` provides easy API access
    to e.g. Statistics Norway, Statistics Sweden and Statistics Finland.
-   Package `r pkg("rdhs")` interacts with The Demographic
    and Health Surveys (DHS) Program datasets.

Misc:

-   Package `r pkg("samplingbook")` includes sampling
    procedures from the book 'Stichproben. Methoden und praktische
    Umsetzung mit R' by Goeran Kauermann and Helmut Kuechenhoff (2010).
-   Package `r pkg("SDaA")` is designed to reproduce results
    from Lohr, S. (1999) 'Sampling: Design and Analysis, Duxbury' and
    includes the data sets from this book.
-   The main contributions of `r pkg("samplingVarEst")` are
    Jackknife alternatives for variance estimation of unequal
    probability with one or two stage designs.
-   Package `r pkg("memisc")` includes tools for the
    management of survey data, graphics and simulation.
-   Package `r pkg("anesrake")` provides a comprehensive
    system for selecting variables and weighting data to match the
    specifications of the American National Election Studies.
-   Package `r pkg("spsurvey")` includes facilities for
    spatial survey design and analysis for equal and unequal probability
    (stratified) sampling.
-   The `r pkg("FFD")` package is designed to calculate
    optimal sample sizes of a population of animals living in herds for
    surveys to substantiate freedom from disease. The criteria of
    estimating the sample sizes take the herd-level clustering of
    diseases as well as imperfect diagnostic tests into account and
    select the samples based on a two-stage design. Inclusion
    probabilities are not considered in the estimation. The package
    provides a graphical user interface as well.
-   `r pkg("mipfp")` provides multidimensional iterative
    proportional fitting to calibrate n-dimensional arrays given target
    marginal tables.
-   Package `r pkg("MBHdesign")` provides spatially balanced
    designs from a set of (contiguous) potential sampling locations in a
    study region.
-   Package `r pkg("quantification")` provides different
    functions for quantifying qualitative survey data. It supports the
    Carlson-Parkin method, the regression approach, the balance approach
    and the conditional expectations method.
-   `r pkg("BIFIEsurvey")` includes tools for survey
    statistics in educational assessment including data with replication
    weights (e.g. from bootstrap).
-   `r pkg("surveybootstrap")` includes tools for using
    different kinds of bootstrap for estimating sampling variation using
    complex survey data.
-   Package `r pkg("surveyoutliers")` winsorize values of a
    variable of interest.
-   The package `r pkg("univOutl")` includes various methods
    for detecting univariate outliers, e.g. the Hidiroglou-Berthelot
    method.
-   Package `r pkg("extremevalues")` is designed to detect
    univariate outliers based on modeling the bulk distribution.
-   Package `r pkg("RRreg")` implements univariate and
    multivariate analysis (correlation, linear, and logistic regression)
    for several variants of the randomized response technique, a survey
    method for eliminating response biases due to social desirability.
-   Package `r pkg("RRTCS")` includes randomized response
    techniques for complex surveys.
-   Package `r pkg("panelaggregation")` aggregates business
    tendency survey data (and other qualitative surveys) to time series
    at various aggregation levels.
-   Package `r pkg("surveydata")` makes it easy to keep
    track of metadata from surveys, and to easily extract columns with
    specific questions.
-   `r pkg("RcmdrPlugin.sampling")` includes tools for
    sampling in official statistical surveys. It includes tools for
    calculating sample sizes and selecting samples using various
    sampling designs.
-   Package `r pkg("mapStats")` does automated calculation
    and visualization of survey data statistics on a color-coded map.



### Links
-   [useR!2008 Tutorial: Small Area Estimation](http://www.R-project.org/conferences/useR-2008/tutorials/gomez.html)
