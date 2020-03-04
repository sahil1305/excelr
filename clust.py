# -*- coding: utf-8 -*-
"""
Created on Fri Jan  3 12:13:44 2020

@author: sahil
"""

import pandas as pd
import matplotlib.pylab as plt 
Univ = pd.read_csv("H:\\all datasets\\Universities.csv")

# Normalization function 
def norm_func (i):
    x = (i-i.min())	/	(i.max()	-	i.min())
    return (x)
df_norm = norm_func(Univ.iloc[:,1:])
from scipy.cluster.hierarchy import linkage 
import scipy.cluster.hierarchy as sch # for creating dendograms
type(df_norm)
help(linkage)
z = linkage(df_norm, method="complete",metric="euclidean")
plt.figure(figsize=(15, 5));plt.title('Hierarchical Clustering Dendrogram');plt.xlabel('Index');plt.ylabel('Distance')
sch.dendrogram(
    z,
    leaf_rotation=0.,  # rotates the x axis labels
    leaf_font_size=8.,  # font size for the x axis labels
)
plt.show()
from sklearn.cluster import	AgglomerativeClustering 
h_complete	=	AgglomerativeClustering(n_clusters=3,	linkage='complete',affinity = "euclidean").fit(df_norm) 
cluster_labels=pd.Series(h_complete.labels_)
Univ['clust']=cluster_labels # creating a  new column and assigning it to the new column
Univ = Univ.iloc[:,[7,0,1,2,3,4,5,6]] #changing the last column place to the first column 
Univ.head()

# getting aggregate mean of each cluster
Univ.iloc[:,2:].groupby(Univ.clust).median()

# creating a csv file 
Univ.to_csv("University.csv",encoding="utf-8")
import os
os.getcwd()
