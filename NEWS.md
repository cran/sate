

sate v3.1.0 (Release date: TBD, est. 2025-11-05)
==============

Major Changes:

* Updated get_pG_by_k function. Implemented analytic method for obtaining guilty/death 
  verdict probabilities given jury size. The analytic solution is based on matrix algebra,
  not simulation or use of while loop. Improved function used by analysis functions for
  faster and more precise results.
* Added transition.matrix function. It returns a transition probability matrix
  for jury deliberating choice between NG and G verdicts.
* Added transition.matrix.ordered function. It generates a transition probability matrix
  for jury deliberating choice among three or more ordered verdict options.
* Added prob.ordered.verdicts function. It obtain the final (absorbing) state probabilities
  for jury deliberating among three or more ordered verdict options. This function calls 
  the transition.matrix.ordered function (which may also be called as stand alone function).
* Added prob_ord_from_pool function. Given verdict preferences in population from which 
  jurors are selected, it returns deliberated verdict probabilities for trial jury 
  deliberating among three or more ordered verdict options. This function calls 
  the transition.matrix.ordered and prob.ordered.verdicts functions (which may also 
  be called as stand alone functions).


sate v2.5.0 (Release date: 2025-03-04)
==============

Minor Changes:

* Updated weights_for_population function to handle survey data without missing respondent data.


sate v2.4.0 (Release date: 2024-03-04, approx.)
==============

Minor Changes:

* Added observed.deliberation dataset, a compilation of observed deliberation outcomes
  that is useful to estimate relationship between initial jury poll and trial verdicts.


sate v2.3.0 (Release date: 2024-09-11)
==============

Minor Changes:

* Added compare.juror.stats function for comparing and evaluating difference between juror 
  verdict preferences in actual and hypothetical trial conditions.
* Added three functions to assist in calculating sampling weights to represent key demographic
  characteristics of a state or USA. Apply the encode.cloud.respondents.variables function to 
  respondent information reported by Cloud Research. Use target.population.demographics to obtain
  demographic statistics for target population. Use weights_for_population to calculate sampling 
  weights that balance respondent demographics relative to target population. 
* Added basic.plot.grid function to create basic plot element that can then be customized.
* Improved reporting of estimated values in tables.
* Improved documentation of functions.


sate v2.2.0 (Release date: 2024-09-11)
==============

Minor Changes:

* Update the P(G|k) values supplied by the helper function get.model.values. Updated
  values based on matrix algebra rather than computer simulation. Differences extremely
  minor, but additional jury sizes added.
* Introduced get_pG_by_k function. This function allows user to calculate P(G|k) values
  for juries with 3 or more members. 
* Improved formatting of results table returned by as.jury.stats, compare.jury.stats,  
  sim.as.jury.stats, and sim.compare.jury.stats functions.
* Added margin of error (MOE) to results tables reported by as.jury.stats, 
  compare.jury.stats, sim.as.jury.stats, and sim.compare.jury.stats functions.
* Fixed minor bug with randomly draw proportions falling outside [0, 1] interval.
* Improved documentation of functions.


sate v2.1.0 (Release date: 2023-12-20)
==============

Major Changes:

* Major revision of all existing functions. 
* Graphing functions introduced, allowing users to plot the estimated probability
  of guilty verdict in single trial condition, or plot the effect of a trial error
  as difference between actual and hypothetical trial conditions.
* Deliberation process modeled using a deliberate function which reduces standard
  errors of estimates. Users have option to estimate values using empirical model
  and standard errors generated through computational methods.
* Account for selection of small trial jury from large jury pool.
* Introduced optional arguments to account for varying jury size and peremptory strikes.


sate v1.1.0 (Release date: 2020-03-17)
==============

Major Changes:

* Initial release of Scientific Analysis of Trial Errors (SATE) R Package.
