############# Import Packages #############

pacman::p_load(pacman, ggplot2) 

library(ggplot2)

############# Generate the Fake Dataset #############

# generate 10 samples (cells), where we measure 100 genes for each sample
data.matrix <- matrix(nrow = 100,
                      ncol = 10
                      )

# name the samples
colnames(data.matrix) <- c( # Retrieve or set the row or column names of a matrix-like object.
  paste("wt", 1:5, sep = ""), # first 5 samples will be 'wild type' (wt) samples
  paste("ko", 1:5, sep = "")  # last 5 samples are 'knock-out' (ko) samples
)

# name the genes
rownames(data.matrix) <- paste("gene", 1:100, sep = "")

# give the fake genes fake read counts (from 10 to 1000 using a poisson distribution)
for (i in 1:100) {
  wt.values <- rpois(5, lambda=sample(x=10:1000, size = 1))
  ko.values <- rpois(5, lambda=sample(x=10:1000, size = 1))
  
  data.matrix[i,] <- c(wt.values, ko.values)
}

head(data.matrix) # samples are columns, genes are rows



############# PCA on the data #############

# the goal is to draw a graph that shows how the samples are
# related (or not related) to each other
pca <- prcomp(t(data.matrix), # by default, samples are rows and genes columns
                              # thus, we need to transpose the matrix
              scale = TRUE)

# plot the first two Principal Components (PCs)
plot(pca$x[,1], # pca$x contains the principal components
     pca$x[,2])

# as shown, the samples cluster around 2 regions (wt and ko)
# to see how statistically significant this is, lets check how much
# variation in the original data PC1 accounts for
pca.var <- pca$sdev^2 # pca$sdev is standard deviation
# now calculate the percentage of variation that each PC accounts for 
pca.var.per <- round(pca.var/sum(pca.var)*100, # calculate in percentage
                     1 # round to 1 decimal
                     )

# now plot the influence of each PC
barplot(pca.var.per,
        main = "Scree Plot",
        xlab = "Principal Component",
        ylab = "Percent Variation"
        )

# use ggplot2 to make a better plot
# format data into a dataframe
pca.data <- data.frame(Sample = rownames(pca$x), # one column with the sample ids
                       X = pca$x[,1], # one column for the X coordinate
                       Y = pca$x[,2]  # one column for the Y coordinate
                         )

pca.data # we have one row per sample
         # each row has a sample ID and X/Y coordinates for that sample

ggplot(data = pca.data, # pass the pca.data dataframe into ggplot
       aes(x = X, y = Y, label = Sample)) + # specify which columns contain XY and labels
  geom_text() + # plot the labels, rather than dots
  xlab(paste("PC1 -", pca.var.per[1], "%", sep = "")) + # x-axis labels
  ylab(paste("PC2 -", pca.var.per[2], "%", sep = "")) + # y-axis labels
  theme_bw() + # background of the graph white
  ggtitle("My PCA Graph") # title of the graph

# Now, use the loading scores to determine which genes have the largest
# effect on where the samples are plotted in the PCA plot
loading_scores <- pca$rotation[, 1] # loading scores for PC1
gene_scores <- abs(loading_scores) # genes that ush samples to the left side of the graph
                                    # will have large negative values and genes that push
                                    # samples to the right will have large positive values
                                    # we are interest in both set of genes -> abs
gene_score_ranked <- sort(gene_scores, decreasing = TRUE) # sort loading scores, from high to low
top_10_genes <- names(gene_score_ranked[1:10]) # get top 10 genes with largest loading scores

top_10_genes

# to see which have negative or positive loading scores
pca$rotation[top_10_genes, 1]
# positive loading scores push the 'ko' samples to the right side of the graph
# negative loading scores push the 'wt' samples to the left side of the graph