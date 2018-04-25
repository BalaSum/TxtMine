---
title: "Text Mining With R"
header-includes:
   - \usepackage{bbm}
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Corpus 
Is a collection of documents.


```{r message=FALSE, warning=FALSE}
library(tm)
# find path of the package and folder 
txt <- system.file("texts", "txt", package = "tm")
ovid <- VCorpus(DirSource(txt, encoding = "UTF-8"),
          readerControl = list(language = "lat"))
inspect(ovid[1:2])
inspect(ovid[[1]])
```

## Transformations
Now, we can modify the documents in the corpus, e.g., stemming, stopword removal,
Transformations are done via the tm_map() 
Basically, all transformations work on single text documents and tm_map() just applies them to all documents in a corpus.

```{r message=FALSE, warning=FALSE}
library(tm)
library(XML)
reut21578 <- system.file("texts", "crude", package = "tm")

library(dplyr)
reuters <- VCorpus(DirSource(reut21578), readerControl = list(reader = readReut21578XMLasPlain)) %>%
  tm_map(x = ., FUN = removePunctuation) %>% 
  tm_map(x = ., FUN = removeNumbers) %>%
  tm_map(x = ., FUN = removeWords, stopwords(kind = 'en')) %>%
  tm_map(x = ., FUN = stripWhitespace)
```
## Creating Term-Document Matrices
A common approach in text mining is to create a term-document matrix from a corpus. 
In the tm package the classes 
1)TermDocumentMatrix 
2) DocumentTermMatrix 
employ sparse matrices for corpora. 

```{r}
dtm <- DocumentTermMatrix(reuters)

# inspect data frame
inspect(dtm[5:15, 740:748])

# Frequent terms >10
findFreqTerms(dtm, 10)

# find Associations (i.e., terms which correlate)
findAssocs(dtm, "opec", 0.8)
```

##TFIDF -  Term Frequency - Inverse Document Frequency 
 
From the Document Term Matrix, it is possible to create a Term Frequency - Inverse Document Frequency matrix. 
Term frequency $\mathit{tf}_{i,j}$ counts the number of occurrences $n_{i,j}$ of a term $t_i$ in a document $d_j$. In the case of normalization, the term frequency $\mathit{tf}_{i,j}$ is divided by $∑_k n_{k,j}$.

Inverse document frequency for a term $t_i$ is defined as

$$\mathit{idf}_i = \log_2 \frac{|D|}{|\{d \mid t_i \in d\}|}$$

where $|D|$ denotes the total number of documents and where $|\{d \mid t_i \in d\}|$ is the number of documents where the term $t_i$ appears.

Term frequency - inverse document frequency is now defined as $\mathit{tf}_{i,j} \cdot \mathit{idf}_i$.

This gives additional weight to a term that is common in a given document, but comparably rare in the entire corpus. At the same time, terms that are common across many or all documents are penalized in frequency for any single document as they provide a smaller amount of unique information about a given document.


```{r}
tf_idf <- weightTfIdf(m = dtm, normalize = TRUE)
tf_idf_mat <- as.matrix(tf_idf)

```

Once the frequencies are calculated, a square matrix of all documents is processed as a distance lookup for clustering. 

```{r results='hide', message=FALSE, warning=FALSE}
library(ggplot2)
library(proxy)

#Calculate distance
tf_idf_dist <- dist(tf_idf_mat, method = 'cosine')
```
##Hierarchical Clustering
Show the cohesion among clusters at all levels since the hierarchical method preserves all intermediate clusters. The Ward Method of clustering is used to place a heavy weight on the cohesiveness of a formed cluster at each step of the process. 

```{r Fig1, echo=TRUE, fig.height=10, fig.width=15}
clust_h <- hclust(d = tf_idf_dist, method = 'ward.D2')
clust_h$labels <- as.matrix(unlist(tm_map(reuters, function(x) meta(x, "heading"))))
plot(clust_h,
    main = 'Cluster Dendrogram',
    xlab = '', ylab = '', sub = '')
```
## Cluster Validity

```{r}
library('clValid')
stab <- clValid(as.matrix(tf_idf_dist), 2:10, clMethods=c("kmeans"),
                validation="internal")
optimalScores(stab)
```


```{r}
tf_idf_norm <- tf_idf_mat / apply(tf_idf_mat, MARGIN = 1, FUN = function(x) sum(x^2)^0.5)
km_clust <- kmeans(x = tf_idf_norm, centers = 3, iter.max = 25)
```

```{r}
pca_comp <- prcomp(tf_idf_norm)
pca_rep <- data_frame(file_name = clust_h$labels[1],
                      pc1 = pca_comp$x[,1],
                      pc2 = pca_comp$x[,2],
                      clust_id = as.factor(km_clust$cluster))

ggplot(data = pca_rep, mapping = aes(x = pc1, y = pc2, color = clust_id)) +
    scale_color_brewer(palette = 'Set1') +
    geom_text(mapping = aes(label = file_name), size = 2.5, fontface = 'bold') +
    labs(title = 'K-Means Cluster: 5 clusters on PCA Features',
         x = 'Principal Component Analysis: Factor 1',
         y = 'Principal Component Analysis: Factor 2') +
    theme_grey() +
    theme(legend.position = 'right',
          legend.title = element_blank())
```

