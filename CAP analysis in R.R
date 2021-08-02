# CAP example: if it works on your data 
#library(BiodiversityR)

#cap <- CAPdiscrim(mont.spec.matrix.27 ~ Net_primary_productivity+ Relief 
+ Exposure + Mangrove_connectivity + Denseseagrass_connectivity + Coral+ Depth + Protected_status_text ,
data=shannon.montast.env.site, dist="bray", axes=2, m=0, mmax=10, add=FALSE, permutations=0)



CAPdiscrim(formula, data, dist="bray", axes=4, m=0, mmax=10, add=FALSE, permutations=0)





capscale {vegan}

Description
Distance-based redundancy analysis (dbRDA) is an ordination method similar to Redundancy Analysis (rda),
but it allows non-Euclidean dissimilarity indices, such as Manhattan or Bray-Curtis distance. 
Despite this non-Euclidean feature, the analysis is strictly linear and metric.
If called with Euclidean distance, the results are identical to rda, but dbRDA will be less efficient. 
Functions capscale and dbrda are constrained versions of metric scaling, a.k.a. principal coordinates analysis, 
which are based on the Euclidean distance but can be used, and are more useful, with other dissimilarity measures. 
The functions can also perform unconstrained principal coordinates analysis, optionally using extended dissimilarities.

Usage
capscale(formula, data, distance = "euclidean", sqrt.dist = FALSE,
         comm = NULL, add = FALSE,  dfun = vegdist, metaMDSdist = FALSE,
         na.action = na.fail, subset = NULL, ...)





vegdist(x, method="bray", binary=FALSE, diag=FALSE, upper=FALSE,
        na.rm = FALSE, ...) 

altGower	d[jk] = (1/NZ) sum(abs(x[ij] - x[ik]))
where NZ is the number of non-zero columns excluding double-zeros (Anderson et al. 2006).
binary: (A+B-2*J)/(A+B-J)
