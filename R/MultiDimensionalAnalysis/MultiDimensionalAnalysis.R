############# Import Packages #############

pacman::p_load(pacman, ggplot2) 

library(ggplot2)

############# Generate the Fake Dataset #############
# Note: this is the same dataset used for the PCA script

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
# For comparison, since we are using the same dataset as for the PCA script
# we will perform PCA on this dataset

# the goal is to draw a graph that shows how the samples are
# related (or not related) to each other
pca <- prcomp(t(data.matrix), # by default, samples are rows and genes columns
              # thus, we need to transpose the matrix
              scale = TRUE,
              center = TRUE)

pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
pca.var.per

pca.data <- data.frame(Sample = rownames(pca$x), # one column with the sample ids
                       X = pca$x[,1], # pca$x contains the principal components
                       Y = pca$x[,2]
                       )

pca.data

# Plot the PCA
ggplot(data = pca.data, # pass the pca.data dataframe into ggplot
       aes(x = X, y = Y, label = Sample)) + # specify which columns contain XY and labels
  geom_text() + # plot the labels, rather than dots
  xlab(paste("PC1 -", pca.var.per[1], "%", sep = "")) + # x-axis labels
  ylab(paste("PC2 -", pca.var.per[2], "%", sep = "")) + # y-axis labels
  theme_bw() + # background of the graph white
  ggtitle("PCA") # title of the graph

# we see that most of the differences are between the WT and the KO samples
# now let's see how this plot compares to the one obtained by the MDS approach 

############# MDS/PCoA on the data #############
distance.matrix <- dist(scale(t(data.matrix), # create a distance matrix
                                              # by default, samples are rows and genes columns
                                              # thus, we need to transpose the matrix
                              center = TRUE, # center and scale the measurements for each gene
                              scale = TRUE),
                        method = 'euclidean' # use the euclidean distance metric
                        )

# now perform multi-dimensional scaling on the distance matrix
mds.stuff <- cmdscale(distance.matrix, # use classical multi-dimensional scaling function
                      eig = TRUE, # return the eigenvalues 
                                  # uses to calculate how much variation each axis accounts for
                      x.ret = TRUE # return the doubly centered version of the distance matri
                                   # useful to do MDS using the eigen() function instead of the cmdscale()
                      )

# Calculate the amount of variation each axis in the MDS accounts for using eigen values
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100,
                     1
                     )

mds.var.per

# Now, format the data for ggplot
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample = rownames(mds.values),
                       X = mds.values[,1],
                       Y = mds.values[,2])

mds.data

# Plot the MDS
ggplot(data = mds.data, 
       aes(x=X, y=Y, label=Sample))  +
  geom_text() +
  theme_bw() +
  xlab(paste("PC1 -", pca.var.per[1], "%", sep = "")) + # x-axis labels
  ylab(paste("PC2 -", pca.var.per[2], "%", sep = "")) + # y-axis labels
  ggtitle("MDS using Euclidean distance") # title of the graph

# as we see, we obtain the same plot as for PCA
# this is because we used the euclidean distance to compute the distance matrix
# Now, lets try to use a different metric to compute the distance matrix
# lets use the average of the absolute value of the log fold change

# calculate the log2 values of the measurements for each gene
log2.data.matrix <- log2(data.matrix)

# Since the average of asolute values of the log-fold change isn't one of the
# distance metrics built into the dist() function, we'll create our own distance matrix by hand
log2.distance.matrix <- matrix(0, # create an empty matrix
                               nrow=ncol(log2.data.matrix),
                               ncol=ncol(log2.data.matrix),
                               dimnames=list(colnames(log2.data.matrix),
                                             colnames(log2.data.matrix)
                                             )
                               )

log2.distance.matrix

# Fill the matrix with the average of the absolute values of the log fold changes
for(i in 1:ncol(log2.distance.matrix)) {
  for(j in 1:i)  {
    log2.distance.matrix[i,j] <- mean(abs(log2.data.matrix[,i] - log2.data.matrix[,j]))
  }
}

log2.distance.matrix

# Now perform MDS on the new distance matrix
mds.stuff <- cmdscale(as.dist(log2.distance.matrix),
                      eig = TRUE,
                      x.ret = TRUE
                      )

# calculate the amount of variation each axis in the MDS plot
# accounts for using the eigen values
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100,
                     1)

mds.var.per

# format the data for ggplot
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2])
mds.data

# Plot the MDS
ggplot(data = mds.data, 
       aes(x=X, y=Y, label=Sample))  +
  geom_text() +
  theme_bw() +
  xlab(paste("PC1 -", pca.var.per[1], "%", sep = "")) + # x-axis labels
  ylab(paste("PC2 -", pca.var.per[2], "%", sep = "")) + # y-axis labels
  ggtitle("MDS using avg(logFC) as distance") # title of the graph

