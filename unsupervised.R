# Unsupervised Learning
# PCA and t-SNE

###============= t-SNE ==================
library(Rtsne)

train_unique = unique(trainDF_threshold)
train_matrix = as.matrix(sapply(train_unique[,-1], as.numeric))
train_matrix = scale(train_matrix)

set.seed(176) # for reproducibility
tsne = Rtsne(train_matrix, dims = 2, perplexity=25, verbose=TRUE, max_iter = 500, pca = TRUE)
tsneDF = as.data.frame(tsne$Y)

# visualizing in 2D
png("plots/tsne.png")
colors = rainbow(length(unique(train_unique$Class)))
names(colors) = unique(train_unique$Class)
plot(tsne$Y, t='n', main="t-SNE", xlab="1st Embedding", ylab="2nd Embedding")
text(tsne$Y, labels=train_unique$Class, col=colors[train_unique$Class])
dev.off()

# visualizing in 3D
library(plot3D)
library(rgl)

embeddings = as.data.frame(tsne$Y)
embeddings$Class = train_unique$Class
plot3d(x=embeddings$V1, y=embeddings$V2 ,z=embeddings$V3, col = as.numeric(embeddings$Class), pch = ".")
# is that separation between the two larger groups possibly due to the darker letters with lighter background and
# lighter letters with darker background??

###============= PCA =================
pca = princomp(train[,-1])
summary(pca)
plot(pca)

# first two components
png("plots/pca.png")
plot(pca$scores[,1:2], t='n', main="First 2 Components")
text(pca$scores[,1:2], labels=train$Class, col=colors()[train$Class])
dev.off()

# first three components
plot3d(pca$scores[,1:3], col=colors[train$Class])



