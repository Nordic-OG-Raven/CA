## Partial Least Squares approach to structural equation modeling ## 

# Background ----
    # SEM are a combination of path and causation analysis 
    # --> 2 parts that represent researchers' hypotheses about causal relationships between variables:
            # Measurement part, linking observed variables to latent variables using several variables to measure a concept
            # Structural part, linking latent variables to another via a system of equation

# PB-SEM or PLS-SEM?
    # CB: Testing theory
    # PLS: Prediction and explanation of target constructs

# PLS Purpose:
    # not theory test or confiration due to lack of global goodness-of-fit measure
    # Instead: Causal inference; Test predictive power of a model
        # As such, the model strikes a balance between 
              # ML that are fully predictive, 
              # and CB-sem, which focuses on confirmation and model fit.
                    # Virtually the same as CB-SEM when.
                          # Models have 4 or more indicators
                          # Indicator loadings meet the common standards (>= 0.7)
  
# Path Model Interpretation:
  # Circles = Constructs (latent variables) 
      # Exogenous (explain other constructs) --> Endogenous (explained by other constructs)
            # path model development is always from left (independent) to right
      # The "inner model", aka "structural model" link these.
  # Rectangles = Indicators 
      # The "outer model" aka "measurement model" link these to the constructs
  # Arrows = Relationships
      # In PLS-SEM they are always single headed (predictive)
            # With enough theoretical support, they can be interpreted as causal.
            # Theories are multiple hypotheses that are logically linked and can be tested empirically.
      # Formatively measured = Arrow from indicator to construct 
            # Indicating the assumption that the constructs "cause" the covariation in indicator values
      # Reflectively measured = Arrow from construct to indicator
            # Assumed to be error free
      # Note: For single-item constructs the direction of relationship is irrelevant, as construct and item are equivalent.
  # Error terms = The unexplained variance 
      # Only associated with reflective measures.

# Two-step theory testing process using PLS-SEM:
    # Two types of theories are needed to develop path models:
        # Measurement theory = Specification of what indicators measure certain theoretical concepts, and how.
        # Structural theory = Specification of how the constructs are related to another
    # 1) Test and confirm reliability and validity of the measurement theory
              # Formatively or Reflectively
    # 2) Test and confirm the structural theory

# Data Characteristics
    # Influential observations, outliers, and collineairity do influence the OLS in PLS-SEM.
    # No causal loops (although extensions exist)
    # Simple size; Minimum requirements
    # No distributional assumptions needed
    # Less than 5% NAs
    # Scales  
        # Works with metric and quasi-metric (ordinal) scales
        # Standard PLS-SEM accomodates binary-coded variables
              # However, additional considerations are needed for when they are used as moderators 
              # and in the analysis of data from discrete choice experiments 

## Preparation ----
    # Set-up ----
        rm(list=ls())
        library(seminr)
        # Loading Data ----
        corp_rep_data <- read.csv("/Users/jonas/Documents/BSS+/BI S2/Customer Analytics/Customer Analytics Template/Data/Corporate Reputation Data.csv", header=TRUE,sep=";")
        head(corp_rep_data)
        # PLS-SEM data requirements ----
            # Influential observations, outliers, and collineairity do influence the OLS in PLS-SEM.
            # No causal loops (although extensions exist)
            # Simple size; Minimum requirements
            # No distributional assumptions needed
            # Less than 5% NAs
            # Scales  
                # Works with metric and quasi-metric (ordinal) scales
                # Standard PLS-SEM accomodates binary-coded variables
                      # However, additional considerations are needed for when they are used as moderators 
                      # and in the analysis of data from discrete choice experiments 
        
    # Measurement Model ----
        # = Specification of what indicators measure certain theoretical concepts, and how.
        # Step 1) is to test and confirm the reliability and validity of the measurement theory. 
            # First we need to of course set it up though.
        
        # Two types: Formative or reflective
            # mode_A = reflective (highly correlated; explain overlapping aspects)
            # mode_B = formative (not highly correlated; explain different aspects)
    
    # Reflective and formative
    corp_rep_mm_ext <- constructs(
      composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
      composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
      composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
      composite("ATTR", multi_items("attr_", 1:3), weights = mode_B),
      composite("COMP", multi_items("comp_", 1:3), weights = mode_A),
      composite("LIKE", multi_items("like_", 1:3), weights = mode_A),
      composite("CUSA", single_item("cusa")),
      composite("CUSL", multi_items("cusl_", 1:3), weights = mode_A))
    
    # Exercise Example illustrating alternative specification
    # simple_mm <- constructs(
    #   composite("IMAG", multi_items("imag", 1:5), weights = mode_B),
    #   composite("EXPE", c("expe1", "expe2", "expe3", "expe4", "expe5"), weights = mode_A), #Alternative specification
    #   composite("QUAL", multi_items("qual", 1:5), weights = mode_A),
    #   composite("VAL", multi_items("val", 1:4), weights = mode_A),
    #   composite("SAT", multi_items("sat", 1:4), weights = mode_A),
    #   composite("LOY", multi_items("loy", 1:4), weights = mode_A))
  
    # Structural model ----
        # = Specification of how the constructs are related to another
        # This will be step 2): Test and confirm the structural model
        # Again, we first need to set it up of course

    # Formative and Reflective
        corp_rep_sm_ext <- relationships(
          paths(from = c("QUAL", "PERF", "CSOR", "ATTR"), to = c("COMP", "LIKE")),
          paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
          paths(from = c("CUSA"), to = c("CUSL")))
        
    # Exercise Example: 
        # simple_sm <- relationships(
        #   paths(from = c("IMAG"), to = c("EXPE", "SAT", "LOY")),
        #   paths(from = c("EXPE"), to = c("QUAL","VAL","SAT")),
        #   paths(from = c("QUAL"), to = c("VAL", "SAT")),
        #   paths(from = c("VAL"), to = c("SAT")),
        #   paths(from = c("SAT"), to = c("LOY")))

    # Estimating the model ----
        corp_rep_pls_model_ext <- estimate_pls(data = corp_rep_data,
                                               measurement_model = corp_rep_mm_ext,
                                               structural_model = corp_rep_sm_ext,
                                               missing = mean_replacement,
                                               missing_value = "-99")
        
        # There's also this version with "path weighting" (from very early on....)
        corp_rep_pls_model <- estimate_pls(data = corp_rep_data,
                                           measurement_model = corp_rep_mm,
                                           structural_model = corp_rep_sm,
                                           inner_weights = path_weighting,
                                           missing = mean_replacement,
                                           missing_value = "-99")

## Summarizing the results ----
    # Summarize the model results
    summary_simple_corp_rep <- summary(corp_rep_simple_model)
        # even though we have a single indicator (CUSA) it still gets a number, although it is a 1.
        
    # Iterations to converge
    summary_simple_corp_rep$iterations
    
    # Inspect the model's loadings
    summary_simple_corp_rep$loadings
    
    # Inspect the model's path coefficients and the R^2 values
    summary_simple_corp_rep$paths
      # here we see the inner relationships (structural)
    
    # Bootstrap the model
    boot_simple_corp_rep <- bootstrap_model(seminr_model = corp_rep_simple_model,
                                            nboot = 1000, cores = NULL, seed = 123)
        # This shows the associated uncertainty for the arrows from the indicators to the constructs
        # Naturally the single variable construct looks a bit odd (no variability, 0 uncertainty)
    
    # Store the summary of the bootstrapped model
    sum_boot_simple_corp_rep <- summary(boot_simple_corp_rep)
    
    # Inspect the bootstrapped indicator loadings
    sum_boot_simple_corp_rep$bootstrapped_loadings
      # Beware: the arrows here go from indicator to the construct, and for reflectively measured constructs the direction should be the other way around
    
    # Inspect the bootstrapped structural paths
    sum_boot_simple_corp_rep$bootstrapped_paths
    
## Evaluation of reflective measurement model ----
    
      # indicator reliability ----
      # = how much of each indicator’s variance is explained by its construct
      # How: Square loading to get the bivariate correlation between indicator and construct
          summary_corp_rep$loadings
          summary_corp_rep$loadings^2
          # Threshold: Min 0.708 recommended, i.e. explaining at least 50% of indicator variance
                # Consider removing if over 0.4 only if 
                        # 1) deletion leads to an increase in internal consistency reliability or convergent validity, 
                        # 2) AND if content validity is not threatened severely.
  
      # Construct validity (aka internal consistency reliability) ----
      # = the extent to which indicators measuring the same construct are associated with each other
      # How: ideally with rho_A, i.e. the mid-point between the overly conservative Cronbach's alpha and the overly liberal rho_c
          summary_corp_rep$reliability
          plot(summary_corp_rep$reliability)
          # rho_C thresholds:
                # rho_C values between 0.6 and 0.7 are "acceptable in exploratory research"
                # rho_C values between 0.7 and 0.9 range from "satisfactory to good"
                # values_C above 0.9 and definitively above 0.95 are problematic, implying indicators are redundant
          # Cronbach's alpha:
                # Same threshold as rho_C, but with limitation that it assumes all inidicators loadings are the same 

      # Convergrent validity ----
      # = The extent to which the construct converges in order to explain the variance of its indicators
      # How: AVE (communality of constructs)
          # Mathematically AVE is the grand mean value of squared loadings of indicators associated with the construct (i.e. sum of squared loadings / number of indicators)
          summary_corp_rep$validity$fl_criteria
          # AVE is on the diagnonal... don't get how to measure this
          # Threshold: Minimum 0.5, indicating the construct explains 50% or more of the indicators' variance making up the construct.
  
      # Discriminant validity ----
      # = The extent to which a construct is empirically distinct from other constructs in the structural model
      # How: HTMT ratio of correlations 
          summary_corp_rep$validity$htmt
          # HTMT Threshold:
              # Maximum 0.9 for  structural models with constructs that are conceptually very similar, such as cognitive satisfaction, affective satisfaction, and loyalty.
              # When constructs are conceptually more distinct, a lower, more conservative, threshold value is suggested, such as 0.85
              # Bootstrap confidence intervals with 1000 samples can be set-up to see if HTMT is significantly different from 1, or a lower threshold value, e.g. 0.85 - again dependent on study context. 
                boot_corp_rep <- bootstrap_model(seminr_model = corp_rep_pls_model,
                                                 nboot = 1000,cores = NULL,seed = 123)
                
                sum_boot_corp_rep <- summary(boot_corp_rep, alpha = 0.10)
                sum_boot_corp_rep$bootstrapped_HTMT
              # again: should be below 0.85 and definitively below 0.9
  
## Evaluation of the formative measurement model ----
  # The construct with arrows pointing towards them are assessed in the same two reliability and two validity areas, 
  # as well as collinearity and statistical significance and relevance of indicator weights, before potentially removing some features
  # However, establishing validity of formative constructs directly is more difficult, because of the direction of causality from indicators to the construct. 
  # To overcome this, we can correlate the formative construct with a global measure (a single item representing the construct's overall concept), 
  # If the formative construct and the global item are highly correlated, it supports the argument that the formative construct is validly measuring the intended concept.
      #1: ATTR ----
            
          # Create measurement model
            ATTR_redundancy_mm <- constructs(
              composite("ATTR_F", multi_items("attr_", 1:3), weights = mode_B),
              composite("ATTR_G", single_item("attr_global")))
                  # The ATTR_G is the globally measured ...
                  # Since it's a single item we do not need to think about formative or reflective measurement
          
          # Create structural model
            ATTR_redundancy_sm <- relationships(
              paths(from = c("ATTR_F"), to = c("ATTR_G")))
          
          # Estimate the model
            ATTR_redundancy_pls_model <- estimate_pls(data = corp_rep_data,
                                                    measurement_model = ATTR_redundancy_mm,
                                                    structural_model = ATTR_redundancy_sm,
                                                    missing = mean_replacement,
                                                    missing_value = "-99") # this tells R what is the symbol for a missing observation; 
          
          # Summarize the model
            sum_ATTR_red_model <- summary(ATTR_redundancy_pls_model)
      
      #2: CSOR ----
        
          # Create measurement model
          CSOR_redundancy_mm <- constructs(
            composite("CSOR_F", multi_items("csor_", 1:5), weights = mode_B),
            composite("CSOR_G", single_item("csor_global")))
          
          # Create structural model
          CSOR_redundancy_sm <- relationships(
            paths(from = c("CSOR_F"), to = c("CSOR_G")))
          
          # Estimate the model
          CSOR_redundancy_pls_model <- estimate_pls(data = corp_rep_data,
                                                    measurement_model = CSOR_redundancy_mm,
                                                    structural_model = CSOR_redundancy_sm,
                                                    missing = mean_replacement,
                                                    missing_value = "-99")
          
          # Summarize the model
          sum_CSOR_red_model <- summary(CSOR_redundancy_pls_model)
      
      #3: PERF ----
      
          # Create measurement model
          PERF_redundancy_mm <- constructs(
            composite("PERF_F", multi_items("perf_", 1:5), weights = mode_B),
            composite("PERF_G", single_item("perf_global")))
                  # --> threshold met; Collinearity is fine
          
          # Create structural model
          PERF_redundancy_sm <- relationships(
            paths(from = c("PERF_F"), to = c("PERF_G")))
                  # Below conservative threshold of 3 --> no multicollinearity issue
          
          # Estimate the model
          PERF_redundancy_pls_model <- estimate_pls(data = corp_rep_data,
                                                    measurement_model = PERF_redundancy_mm,
                                                    structural_model = PERF_redundancy_sm,
                                                    missing = mean_replacement,
                                                    missing_value = "-99")
          # Summarize the model
          sum_PERF_red_model <- summary(PERF_redundancy_pls_model)
          
      #4: QUAL ----
          # Create measurement model
          QUAL_redundancy_mm <- constructs(
            composite("QUAL_F", multi_items("qual_", 1:8), weights = mode_B),
            composite("QUAL_G", single_item("qual_global")))
          
          # Create structural model
          QUAL_redundancy_sm <- relationships(
            paths(from = c("QUAL_F"), to = c("QUAL_G")))
          
          # Estimate the model
          QUAL_redundancy_pls_model <- estimate_pls(data = corp_rep_data,
                                                    measurement_model = QUAL_redundancy_mm,
                                                    structural_model = QUAL_redundancy_sm,
                                                    missing = mean_replacement,
                                                    missing_value = "-99")
      
          # Summarize the model
          sum_QUAL_red_model <- summary(QUAL_redundancy_pls_model)
      
      # Convergent validity ----
      # = The degree to which the formatively specified construct correlates with an alternative reflectively measured variable(s) of the same concept.
      # For this we need to included alternate measures of the formatively measured construct in the questionnaire
      # How: Check the path coefficients 
          sum_ATTR_red_model$paths
          sum_CSOR_red_model$paths
          sum_PERF_red_model$paths
          sum_QUAL_red_model$paths
          # Threshold: At least 0.708, i.e. the construct explains at least 50% of the alternative measure's variance
      
      # Collinearity analysis ----
      # = Check whether two or more indicators in a formative measurement model are highly correlated
      # This is important, because collinearity can increase the standard error of the weights, thus triggering type 2 errors (i.e. false negatives)
      # It can even trigger changes in indicator weights with more pronounced levels of collinearity
      # How: VIF
          summary_corp_rep_ext$validity$vif_items
          # VIF threshold:
              # 5 or above indicate collinearity problems --> eliminate or merge indicators, or establish a higher-order construct
              # Below 3 can also indicate issues
              # Hence, when the analysis produces unexpected sign changes in the indicator weights, the initial step is to compare the sign of the relationship using bivariate correlation. 
              # If the relationship sign differs from the correlation sign, researchers should revise the model setup, also by eliminating or merging indicators or establishing a higher-order construct.
      
      # Statistical significance and relevance of indicator weights ----
      # Indicator weights result from regressing each formatively measured construct on its associated indicators
          # Thus, represent each indicator's relative importance in the construct formation
      # How: Bootstrapping to compare t-values with critical values
          # i.e. calculating standard error; std. dev of estimated coef. across 1000 samples, in order to avoid relying on any distributional assumptions.
            sum_boot_corp_rep_ext <- summary(boot_corp_rep_ext, alpha = 0.05)
                # alpha sets the specified level for significance, i.e. 0.05
            sum_boot_corp_rep_ext$bootstrapped_weights
                # Thresholds:
                    # 5% significance level --> 1.96 (two-tailed)
                    # or percentile method: confidence intervals without zero
            
                # What if an indicator weight is not significant; remove? 
                    # it is not necessarily interpreted as evidence of poor measurement model quality
                    # It is recommended to consider the absolute contribution of a formative indicator to the construct, given by the formative indicator's loading
                    # At a minimum a formative indicator's loading should be statistically significant
                          # Threshold:
                              # 0.5 and higher suggest the indicator makes a sufficient absolute contribution to forming the construct, even if it lacks a significant relative contribution.
            
                # Remove? Caution: To delete or not to delete a formative indicator 
                    # Formative indicator weights are a function of the number of indicators used to measure a construct; the more indicators used, the lower the average weight.
                    # Content validity; the indicators need to fully capture the domain of a construct.
                    # Thus, in contrast to reflective measurement models, formative indicators are not inter- changeable, and removing even one indicator can therefore reduce the measure- ment model’s content validity
                # Relevance
                    # for this, standardize indicator weights between -1 and 1
            
            
                # Example: Are the weights significant?
                    # Easiest way: examine confidence boundary  
                    # --> qual2, qual3, qual4, and seasaw 2 and 4 are problematic due to insignificant weights
                        # Thus we should look at their loadings (even though this is a formatively measured construct)
      
          # Inspecting the bootstrapping results for indicator loadings
            sum_boot_corp_rep_ext$bootstrapped_loadings
                # The loadings between seasaw 4 and seasaw > 0.5
                # Similar story with others (...)
                # The loadings show that the lack of significance from above is not problematic

    # Something about plugging something into the L and W matrixes afterwards (... fml) ----
      
## Evaluation of the structural model ----
      # 1) Collinearity ----
      # Important, as the path coefficients are based on OLS, and thus may be biased with collinearity.
            # How: VIF --> Create higher order constructs in case of problem
            summary_corp_rep_ext$vif_antecedents
            # Thresholds: 
                # VIF > 5 indicates probably collinearity issues
                # VIF = 3 - 5 can also imply collinearity

      # 2) Significance of structural model relationships (path coefficients) ----
            # How: Bootstrapping standard errors  as a basis for calculating t-values of path coefficients or alternatively confidence intervals
            summary_boot_corp_rep_ext$bootstrapped_paths
      
      # 3) Relevance structural model relationships (path coefficients) ----
            # Path coefficients usually range between -1 (strong negative) and + 1 (strong positive)
                # Values outside of this range are technically possible, e.g. in case of collinearity. 
                # They are not acceptable, and indicate that multicollinearity reduction methods are required
            # interpretation: Ceteris paribus, a a path coefficient of 0.505 indicates that when the predictor construct increases by one standard deviation unit, the endogenous construct will increase by 0.505 standard deviation units.
                # The research context is important when determining whether the size of a path coefficient is meaningful 
                # For a more comprehensive picture of the structural model relationships, we consider the total effects, 
                # i.e. the sum of the direct effect (if any) and all indirect effects linking one construct to another in the model.
            summary_boot_corp_rep_ext$bootstrapped_total_paths
      
      # 4) Examining explanatory power power (aka in-sample predictive power)
            # = The variance explained in each of the endogenous constructs 
            # How: R^2 n f-effect size
            # R^2 
                # Interpretation: R^2 ranges from 0 to 1, with higher values indicating a greater explanatory power. 
                    # Caution: the greater the number of predictor constructs, the higher the R2
                    # Therefore, the R2 should always be interpreted relative to the context of the study, based on the R2 values from related studies as well as models of similar complexity
                    # The adjusted R2 metric accounts for this by adjusting the R2 value based upon the number of explanatory variables in relation to the data size and is seen as a more conservative estimate of R2 
            summary_corp_rep_ext$paths
                # Threshold: 
                    # As a general guideline, R2 values of 0.75, 0.50, and 0.25 can be considered substantial, moderate, and weak, respectively, in many social science disciplines
                    # But acceptable R2 values are based on the research context, and in some disciplines, an R2 value as low as 0.10 is considered satisfactory, as for example, in predicting stock returns
                # F-effect size --> how the removal of a selected predictor construct affects an endogenous construct's R^2 value
                # Inspecting the effect sizes
            summary_corp_rep_ext$fSquare
      
      # 5) Examining predictive power power (aka out-of-sample predictive power) ----
              # How: 
                    # Estimating model on a training sample and evaluating the predictive performance on a holdout sample; K-fold CV
                    # When analyzing the prediction errors, the focus should be on the model’s key endogenous construct – and not on examining the prediction errors for the indicators of all endogenous constructs. 
                    # The most popular metric to quantify the degree of prediction error is the root-mean-square error (RMSE), the square root of the average of the squared differences between the predictions and the actual observations.
                    # Another popular metric is the mean absolute error (MAE), the average magnitude of the errors in a set of predictions without considering their direction (over or underestimation)
                        # more appropriate prediction statistic if the prediction error distribution is highly non-symmetric, as evidenced in a long left or right tail in the distribution of prediction errors
                    
              # Generate the model predictions
              predict_corp_rep_ext <- predict_pls(model = corp_rep_pls_model_ext,
                                          technique = predict_DA, noFolds = 10, 
                                          reps = 10)
            
              # Summarize the prediction results
              sum_predict_corp_rep_ext <- summary(predict_corp_rep_ext)
      
              # Analyze the distribution of prediction error
              par(mfrow=c(1,3))
              plot(sum_predict_corp_rep_ext, indicator = "cusl_1")
              plot(sum_predict_corp_rep_ext, indicator = "cusl_2")
              plot(sum_predict_corp_rep_ext, indicator = "cusl_3")
              par(mfrow=c(1,1))
      
              # Compute the prediction statistics
              sum_predict_corp_rep_ext
              
              # Interpretation: 
                    # These prediction statistics depend on the indicators’ measurement scales, so the absolute size of their raw values does not have much meaning.
                    # Thus, a linear regression model benchmark is used to compare these metrics;
                        # 1. If all indicators in the PLS-SEM analysis have lower RMSE (or MAE) values compared to the naïve LM benchmark, the model has high predictive power.
                        # 2. If the majority (or the same number) of indicators in the PLS-SEM analysis yields smaller prediction errors compared to the LM, this indicates a medium predictive power.
                        # 3. If a minority of the dependent construct’s indicators produce lower PLS-SEM prediction errors compared to the naïve LM benchmark, this indicates the model has low predictive power.
                        # 4. If the PLS-SEM analysis (compared to the LM) yields lower prediction errors in terms of the RMSE (or the MAE) for none of the indicators, this indicates the model lacks predictive power.
                # How to generate predictions given a mediator construct, i.e. both a predictor and itself an outcome of an antecedent. --> either
                        # Generate predictions using the direct antecedents (DAs), or
                        # Generate predictions using the earliest antecedents (EAs) 
                        # The former considers both antecedents and the mediator as predictors of outcome constructs, unlike the latter, where the mediator is excluded from the analysis. 
                        # Generally, the DA approach is recommended, as it has the on average higher accuracy.
              
      # 5) optional: comparison with alternative models ----
                # How: Bayesian Information Criterion (BIC), Geweke and Meese criterion (GM)
                    # These facilitate comparison would having to use a holdout sample
                    # BIC is considered easier to compute, thus more focus is placed there.
                # Interpretation:  while the differences in BIC and GM values are useful in ranking and selecting models, such differences can often be small in practice, leading to model selection uncertainty. 
                    # To resolve this issue, researchers can use the BIC (and GM) values to compute Akaike weights
                    # The higher the Akaike weights, the more likely that the selected model better represents the data generation model.
              
              # Estimate alternative models
              # Create measurement model
              measurement_model <- constructs(
                composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
                composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
                composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
                composite("ATTR", multi_items("attr_", 1:3), weights = mode_B),
                composite("COMP", multi_items("comp_", 1:3)),
                composite("LIKE", multi_items("like_", 1:3)),
                composite("CUSA", single_item("cusa")),
                composite("CUSL", multi_items("cusl_", 1:3)))
      
      # Create structural models
      # Model 1
      structural_model1 <- relationships(
        paths(from = c("QUAL","PERF","CSOR","ATTR"), to = c("COMP", "LIKE")),
        paths(from = c("COMP","LIKE"), to = c("CUSA", "CUSL")),
        paths(from = "CUSA", to = c("CUSL")))
      
      # Model 2
      structural_model2 <- relationships(
        paths(from = c("QUAL","PERF","CSOR","ATTR"), to = c("COMP", "LIKE", "CUSA")),
        paths(from = c("COMP","LIKE"), to = c("CUSA", "CUSL")),
        paths(from = "CUSA", to = c("CUSL")))
      
      # Model 3
      structural_model3 <- relationships(
        paths(from = c("QUAL","PERF","CSOR","ATTR"), to = c("COMP", "LIKE", "CUSA", "CUSL")),
        paths(from = c("COMP","LIKE"), to = c("CUSA", "CUSL")),
        paths(from = "CUSA", to = c("CUSL")))
      
      # Estimate and summarize the models
      pls_model1 <- estimate_pls(data = corp_rep_data, 
                                 measurement_model = measurement_model,
                                 structural_model = structural_model1,
                                 missing_value = "-99")
      sum_model1 <- summary(pls_model1)
      pls_model2 <- estimate_pls(data = corp_rep_data,
                                 measurement_model = measurement_model,
                                 structural_model = structural_model2,
                                 missing_value = "-99")
      sum_model2 <- summary(pls_model2)
      pls_model3 <- estimate_pls(data = corp_rep_data,
                                 measurement_model = measurement_model,
                                 structural_model = structural_model3,
                                 missing_value = "-99")
      sum_model3 <- summary(pls_model3)
      
      # Inspect the IT Criteria matrix of Model1
      sum_model1$it_criteria
      
      # Subset the matrix to only return the BIC row and CUSL column
      sum_model1$it_criteria["BIC", "CUSA"]
      
      # Collect the vector of BIC values for CUSL
      itcriteria_vector <- c(sum_model1$it_criteria["BIC","CUSA"],
                             sum_model2$it_criteria["BIC","CUSA"],
                             sum_model3$it_criteria["BIC","CUSA"])
      
      # Assign the model names to IT Criteria vector
      names(itcriteria_vector) <- c("Model1", "Model2", "Model3")
      
      # Inspect the IT Criteria vector for competing models
      itcriteria_vector
      
      # Calculate the model BIC Akaike weights
      compute_itcriteria_weights(itcriteria_vector)
      
## Mediation analysis ----
      
      # Mediation = when a "mediator construct" intervenes with two other related constructs.
            # Exogenous construct changes mediator construct, in turn changing the endogenous construct
      
      # Evaluating a mediation model requires all quality criteria of the measurement and structural model to be met
              # As usual the analysis begins with the assessment of the reflective and formative measurement models
      # Two types: Direct and indirect effect

      # Direct effects ----
      # = the relationship between two constructs with a single arrow
      summary_corp_rep_ext$paths
      
      # Confidence intervals for direct effects
      summary_boot_corp_rep_ext$bootstrapped_paths
          # From competence directly to customer loyalty is not significant, despite there being a significant indirect effect
          # Concl: customer satisfaction is completely mediating the relationship between competence and customer loyalty.
          # From likability to customer satisfaction there is also a significant direct effect
          # Concl: need to investigate the type of partial mediation
      
      # Indirect effects ----
      # = sequence of relationships with at least one intervening construct, 
          # i.e. two or more direct effects 
          # Visualized with multiple arrows
          # takes product of the two relationships
          # Total effect = sum of direct and indirect effect
      
      # Total indirect effects 
      summary_corp_rep_ext$total_indirect_effects
      
      # Specific indirect effects
      specific_effect_significance(boot_corp_rep_ext, from = "COMP", through = "CUSA", 
                                   to = "CUSL", alpha = 0.05)
          # This indirect effect is significant
      specific_effect_significance(boot_corp_rep_ext, from = "LIKE", through = "CUSA",
                                   to = "CUSL", alpha = 0.05)
          # This one too.
      
      # Total Effects (?) ----
      # = sum of direct and indirect effect
      
      # Calculate the sign of p1*p2*p3
      summary_corp_rep_ext$paths["LIKE", "CUSL"] *
        summary_corp_rep_ext$paths["LIKE","CUSA"] *
        summary_corp_rep_ext$paths["CUSA","CUSL"]
          # Same direction; completemenatry (as opposed to competitive (negative))
      
      # Moderation analysis ----

      # Preparation ----
      # Create measurement model; We introduce switching costs (SC), which is reflectively measured
      corp_rep_mm_mod <- constructs(
        composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
        composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
        composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
        composite("ATTR", multi_items("attr_", 1:3), weights = mode_B),
        composite("COMP", multi_items("comp_", 1:3), weights = mode_A),
        composite("LIKE", multi_items("like_", 1:3), weights = mode_A),
        composite("CUSA", single_item("cusa")),
        composite("SC",   multi_items("switch_", 1:4),  weights = mode_A),
        composite("CUSL", multi_items("cusl_", 1:3), weights = mode_A),
        interaction_term(iv = "CUSA", moderator = "SC", method = two_stage))
      
      # Create structural model; We include an interaction term in the final line, made possible by defining it above
      corp_rep_sm_mod <- relationships(
        paths(from = c("QUAL", "PERF", "CSOR", "ATTR"), to = c("COMP", "LIKE")),
        paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
        paths(from = c("CUSA","SC","CUSA*SC"), to = c("CUSL")))

      # Model Estimation ----
      corp_rep_pls_model_mod <- estimate_pls(data = corp_rep_data,
                                             measurement_model = corp_rep_mm_mod,
                                             structural_model = corp_rep_sm_mod,
                                             missing = mean_replacement,
                                             missing_value = "-99")
      
      # Summarizing the results
      summary_corp_rep_mod <- summary(corp_rep_pls_model_mod)
      
      # Bootstrap the model
      boot_corp_rep_mod <- bootstrap_model(seminr_model = corp_rep_pls_model_mod,
                                           nboot = 1000,cores = NULL,seed = 123)
      
      # Summarize the results of the bootstrap
      summary_boot_corp_rep_mod <- summary(boot_corp_rep_mod, alpha = 0.10)
      
      # Evaluation of the reflective measurement model (Same as done earlier; Loadings etc.)
      
      # Indicator reliability 
      # = how much of each indicator’s variance is explained by its construct
      # How: Square loading to get the bivariate correlation between indicator and construct
      summary_corp_rep_mod$loadings
      summary_corp_rep_mod$loadings^2
          # Threshold: Min 0.708 recommended, i.e. explaining at least 50% of indicator variance
          # Consider removing if over 0.4 only if 
              # 1) deletion leads to an increase in internal consistency reliability or convergent validity, 
              # 2) AND if content validity is not threatened severely.
      
      # Construct Validity 
      # = the extent to which indicators measuring the same construct are associated with each other
      # How: ideally with rho_A, i.e. the mid-point between the overly conservative Cronbach's alpha and the overly liberal rho_c
      summary_corp_rep_mod$reliability
      summary_corp_rep$reliability
      plot(summary_corp_rep$reliability)
      # rho_C thresholds:
          # rho_C values between 0.6 and 0.7 are "acceptable in exploratory research"
          # rho_C values between 0.7 and 0.9 range from "satisfactory to good"
          # values_C above 0.9 and definitively above 0.95 are problematic, implying indicators are redundant
      # Cronbach's alpha:
          # Same threshold as rho_C, but with limitation that it assumes all inidicators loadings are the same 
      
      # Convergent Validity 
      # = The extent to which the construct converges in order to explain the variance of its indicators
      # How: AVE (communality of constructs)
      # Mathematically AVE is the grand mean value of squared loadings of indicators associated with the construct (i.e. sum of squared loadings / number of indicators)
      summary_corp_rep_mod$validity$fl_criteria
          # AVE is on the diagnonal... don't get how to measure this
          # Threshold: Minimum 0., indicating the construct explains 50% or more of the indicators' variance making up the construct.

      # Discriminant Validity
      # = The extent to which a construct is empirically distinct from other constructs in the structural model
      # How: HTMT ratio of correlations 
      summary_corp_rep_mod$validity$htmt
      # HTMT Threshold:
          # Maximum 0.9 for  structural models with constructs that are conceptually very similar, such as cognitive satisfaction, affective satisfaction, and loyalty.
          # When constructs are conceptually more distinct, a lower, more conservative, threshold value is suggested, such as 0.85
          # Bootstrap confidence intervals with 1000 samples can be set-up to see if HTMT is significantly different from 1, or a lower threshold value, e.g. 0.85 - again dependent on study context. 
      
      # Extract the bootstrapped HTMT
      summary_boot_corp_rep_mod$bootstrapped_HTMT
      
      # Evaluation of the formative measurement model ----

      # Create measurement model
      ATTR_redundancy_mm <- constructs(
        composite("ATTR_F", multi_items("attr_", 1:3), weights = mode_B),
        composite("ATTR_G", single_item("attr_global")))
      
      # Create structural model
      ATTR_redundancy_sm <- relationships(
        paths(from = c("ATTR_F"), to = c("ATTR_G")))
      
      # Estimate the model
      ATTR_redundancy_pls_model <- estimate_pls(data = corp_rep_data,
                                                measurement_model = ATTR_redundancy_mm,
                                                structural_model = ATTR_redundancy_sm,
                                                missing = mean_replacement,
                                                missing_value = "-99")
      
      # Summarize the model
      sum_ATTR_red_model <- summary(ATTR_redundancy_pls_model)
      
      # CSOR
      # Create measurement model
      CSOR_redundancy_mm <- constructs(
        composite("CSOR_F", multi_items("csor_", 1:5), weights = mode_B),
        composite("CSOR_G", single_item("csor_global")))
      
      # Create structural model
      CSOR_redundancy_sm <- relationships(
        paths(from = c("CSOR_F"), to = c("CSOR_G")))
      
      # Estimate the model
      CSOR_redundancy_pls_model <- estimate_pls(data = corp_rep_data,
                                                measurement_model = CSOR_redundancy_mm,
                                                structural_model = CSOR_redundancy_sm,
                                                missing = mean_replacement,
                                                missing_value = "-99")
      
      # Summarize the model
      sum_CSOR_red_model <- summary(CSOR_redundancy_pls_model)
      
      # PERF
      # Create measurement model
      PERF_redundancy_mm <- constructs(
        composite("PERF_F", multi_items("perf_", 1:5), weights = mode_B),
        composite("PERF_G", single_item("perf_global")))
      
      # Create structural model
      PERF_redundancy_sm <- relationships(
        paths(from = c("PERF_F"), to = c("PERF_G")))
      
      # Estimate the model
      PERF_redundancy_pls_model <- estimate_pls(data = corp_rep_data,
                                                measurement_model = PERF_redundancy_mm,
                                                structural_model = PERF_redundancy_sm,
                                                missing = mean_replacement,
                                                missing_value = "-99")
      
      # Summarize the model
      sum_PERF_red_model <- summary(PERF_redundancy_pls_model)
      
      # QUAL
      # Create measurement model
      QUAL_redundancy_mm <- constructs(
        composite("QUAL_F", multi_items("qual_", 1:8), weights = mode_B),
        composite("QUAL_G", single_item("qual_global")))
      
      # Create structural model
      QUAL_redundancy_sm <- relationships(
        paths(from = c("QUAL_F"), to = c("QUAL_G")))
      
      # Estimate the model
      QUAL_redundancy_pls_model <- estimate_pls(data = corp_rep_data,
                                                measurement_model = QUAL_redundancy_mm,
                                                structural_model = QUAL_redundancy_sm,
                                                missing = mean_replacement,
                                                missing_value = "-99")
      
      # Summarize the model
      sum_QUAL_red_model <- summary(QUAL_redundancy_pls_model)
      
      # Convergent Validity ----
      # = The degree to which the formatively specified construct correlates with an alternative reflectively measured variable(s) of the same concept.
      # For this we need to included alternate measures of the formatively measured construct in the questionnaire
      # How: Check the path coefficients 
      sum_ATTR_red_model$paths
      sum_CSOR_red_model$paths
      sum_PERF_red_model$paths
      sum_QUAL_red_model$paths
      # Threshold: At least 0.708, i.e. the construct explains at least 50% of the alternative measure's variance
      
      # Collinearity analysis ----
      # = Check whether two or more indicators in a formative measurement model are highly correlated
      # This is important, because collinearity can increase the standard error of the weights, thus triggering type 2 errors (i.e. false negatives)
      # It can even trigger changes in indicator weights with more pronounced levels of collinearity
      # How: VIF
      summary_corp_rep_mod$validity$vif_items
      # VIF threshold:
          # 5 or above indicate collinearity problems --> eliminate or merge indicators, or establish a higher-order construct
          # Below 3 can also indicate issues
          # Hence, when the analysis produces unexpected sign changes in the indicator weights, the initial step is to compare the sign of the relationship using bivariate correlation. 
          # If the relationship sign differs from the correlation sign, researchers should revise the model setup, also by eliminating or merging indicators or establishing a higher-order construct.
      
      # Statistical significance and relevance of indicator weights ----
          # Indicator weights result from regressing each formatively measured construct on its associated indicators
          # Thus, represent each indicator's relative importance in the construct formation
          # How: Bootstrapping to compare t-values with critical values
          # i.e. calculating standard error; std. dev of estimated coef. across 1000 samples, in order to avoid relying on any distributional assumptions
      sum_boot_corp_rep_mod <- summary(boot_corp_rep_mod, alpha = 0.05)
          # alpha sets the specified level for significance, i.e. 0.05
      sum_boot_corp_rep_mod$bootstrapped_weights
          # Thresholds:
              # 5% significance level --> 1.96 (two-tailed)
              # or percentile method: confidence intervals without zero
      
      # What if an indicator weight is not significant; remove? 
          # it is not necessarily interpreted as evidence of poor measurement model quality
          # It is recommended to consider the absolute contribution of a formative indicator to the construct, given by the formative indicator's loading
          # At a minimum a formative indicator's loading should be statistically significant
      # Threshold:
          # 0.5 and higher suggest the indicator makes a sufficient absolute contribution to forming the construct, even if it lacks a significant relative contribution.
      
      # Remove? Caution: To delete or not to delete a formative indicator 
          # Formative indicator weights are a function of the number of indicators used to measure a construct; the more indicators used, the lower the average weight.
          # Content validity; the indicators need to fully capture the domain of a construct.
          # Thus, in contrast to reflective measurement models, formative indicators are not inter- changeable, and removing even one indicator can therefore reduce the measure- ment model’s content validity
      
      # Relevance
          # for this, standardize indicator weights between -1 and 1
      
      # Example: Are the weights significant?
          # Easiest way: examine confidence boundary  
          # --> qual2, qual3, qual4, and seasaw 2 and 4 are problematic due to insignificant weights
          # Thus we should look at their loadings (even though this is a formatively measured construct)
      
      # Inspect the bootstrapping results for indicator loadings
      sum_boot_corp_rep_mod$bootstrapped_loadings
          # The loadings between seasaw 4 and seasaw > 0.5
          # Similar story with others (...)
          # The loadings show that the lack of significance from above is not problematic
      
      # Something about plugging something into the L and W matrixes afterwards (... fml) ----

      # Moderation analysis ----
      
      # Inspect the bootstrapped structural paths
      sum_boot_corp_rep_mod$bootstrapped_paths
      
      # Simple slope analysis plot
      slope_analysis(moderated_model = corp_rep_pls_model_mod,dv = "CUSL",
                     moderator = "SC", iv = "CUSA", leg_place = "bottomright")
        # The slope increases with a standard deviation below the average, then ....

      # Multiple mediation analyses ----
      
      # Process:
          # Running a a single mediation analysis for each proposed mediator separately is a bad idea
          # Because the mediators in a multiple mediation model tend to be correlated, so instead:
          # 1) test the significance of the indirect effects (i.e., each specific and total indirect effects) and the direct effect between the exogenous construct and the endogenous construct. 
          # 2) Test whether the total indirect effect is significant. 
          # 3) To assess the significance of the specific indirect effects, the total indirect effect, and the direct effect, use the results of the bootstrap routine; 10,000 samples --> 95% CI
      # Interpretation:   
          # Ceteris paribus: In a multiple mediation model, a specific indirect effect can be interpreted as the indirect effect of Y1 on Y3 through a given mediator, while controlling for all other included mediators.