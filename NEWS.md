# dirichletprocess 0.4.0.9000

* Added PriorFunction and PriorClusters to draw from the base measure. 
* Fixed a bug in the likelihood calculation (#21) by Filippo Fiocchi.
* Added hierarchical print statement method. 

# dirichletprocess 0.4.0

* Hierarchical Normal Models added by Giovanni Sighinolfi
* Added Giovanni Sighinolfi as a contributor. 
* Added params chain to Hidden Markov Models
* Updated the vignette for hierarchical normal models. 

# dirichletprocess 0.3.1

* Fixed matrix class checking for R 4.0.0
* Corrected typos in vignette
* Added a parameter for the number of initial clusters in DirichletProcessMvnormal.
* Various refactoring.


# dirichletprocess 0.3.0

* Added Hidden Markov models.
* Fixed bug in PosteriorClusters and PosteriorFunction.
* Added in new Beta mixture model for avoiding boundary. 
* Fixed a bug in ChangeObservations when using more than one dimension. 
* Added in Burn, Print and Diagnostic Plots.

# dirichletprocess 0.2.2

* Added a likelihood variable for the `dirichletprocess` class that is calculate with each fit iteration. 
* Added option to change how many Metropolis-Hasting steps are used in each iteration. 
* Added a likelihood calculation with each iteration. 
* Added and refactored some tests. 
* Updated vignette.
* Additional options to univariate plotting. 
* Added Kees Mulder as a contributor. 

# dirichletprocess 0.2.1

* Added AppVeyor, Travis-CI and codecov.io badges.
* Added penalised log-likelihood step for posterior cluster parameter inference.
* Added exponential mixture model `DirichletProcessExponential`. 
* Updated `plot`. Multivariate Gaussian models can now be plotted.
* Various bug fixes.
* Updated description.
 

# dirichletprocess 0.2.0

* First public release.
* Added a `NEWS.md` file to track changes to the package.



