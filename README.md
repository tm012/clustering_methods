# clustering_methods
Hierarchical Clustering and Multi-dimensional Scaling in R

https://tm012.shinyapps.io/Clustering_Methods_R/

Options from the tool: 

1. Choose CSV File - Distance Matrix :  The user will upload a distance matrix. Please look into this link to learn about the distance matrix (https://www.displayr.com/what-is-a-distance-matrix/). The matrix should be symmetric. 
2. Select a method for Distance Object : For some functions that are used in this tool (like 'hclust') we need to convert the matrix to distance object. You can select euclidean","manhattan","maximum","canberra","binary","minkowski methods.
3. Number of individuals who created the Matrix (optional) : To create a distance object for some specific matrix, the tool needs to know the number of individuals who were involve during creation of the matrix. If you have the number, please input the number. The tool will use the number to create a distance obkect if necessary.
4. Add rectangle(s) to the clusters :  An input controls the number of clusters after graph formation in dendrograms (see number 5); this creates a rectangle for each cluster on the dendrogram. A user can add or remove these rectangles from the dendrogram.
5. Minimum Rectangles - The number of rectangles.
6. Select a metric for 'agnes' and 'diana' clusterings : Select a metric for 'agnes' and 'diana' hierarchical clusterings; euclidean" or "manhattan.
7. Select the method for 'hclust' (1st method) : Select a method for 'hclust' - hierarchical clustering
8. Select another method for 'hclust' comparison (2nd method) : Select a method for 'hclust' - hierarchical clustering
9. Select the method for 'agnes' (1st method) : Select a method for 'agnes' - hierarchical clustering
10. Select another method for 'agnes' comparison (2nd method) : Select a method for 'agnes' - hierarchical clustering

The tool will use 7 and 8, 9 and 10 methods to compare hierarchical clusterings using tanglegram for the functions mentioned in the numbers. The first method is for actual hierarchical clustering and comparing with other types of hierarchical clusters, second method selection is for comparison with the first method of the same function.

11. k-value (Cluster Number)/You can also use this for MDS : Number of clusters.
12. Select metric for Fuzzy clustering (Fanny) :  For Fuzzy clustering, a user can select a metric (euclidean, manhattan, SqEuclidean).
13. Select memb.exp for Fuzzy clustering (Fanny) : For Fuzzy clustering, a user can select the value for memb.exp (membership exponent).
14. Select a metric for Partition around Mediods (PAM) : For PAM, a user can select two metrics; euclidean and Manhattan from the tool. 

To learn about clustering methods, go to this link - https://pages.mtu.edu/~shanem/psy5220/daily/Day18/Clustering_I.html

To learn about Multi-dimensional Scaling, go to this link - https://pages.mtu.edu/~shanem/psy5220/daily/Day16/MDS.html


