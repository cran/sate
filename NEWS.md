
sate v2.2.0 (Release date: TBD 2024-09-11)
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
