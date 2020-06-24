# -*- coding: utf-8 -*-
"""
Created on Tue Dec 11 12:13:40 2018

@author: moniy
"""

#####Import Libraries

import pandas as pd
import numpy as np
import networkx as nx
import matplotlib.pyplot as plt
from node2vec import Node2Vec
from sklearn.cluster import KMeans
from sklearn import cluster
from sklearn import metrics
import plotly
plotly.tools.set_credentials_file(username='moniyuv', api_key='w2NBdksdtxziaW5GHpbG')
import plotly.plotly as py
from functools import reduce

############################# Implementing the Node2vec algorithm ##################################

data = pd.read_csv("C:\\Users\\moniy\\Desktop\\GelLab\\Data\\EdgeList_NoAbsolute_combinedv1.csv") # Load data
data.describe()
data["V3"].fillna(0,inplace=True)
data["Weight"] = abs(data["V3"])
G=nx.from_pandas_dataframe(data, 'V1', 'V2', ['Weight'])

data_claims = pd.read_csv("C:\\Users\\moniy\\Desktop\\GelLab\\Data\\ClaimsEdgeList_NoAbsolute_combinedv1.csv")
data_claimamount = pd.read_csv("C:\\Users\\moniy\\Desktop\\GelLab\\Data\\ClaimAmountEdgeList_NoAbsolute_combinedv1.csv")
data_houseage = pd.read_csv("C:\\Users\\moniy\\Desktop\\GelLab\\Data\\HouseAgeEdgeList_NoAbsolute_combinedv1.csv")
data_precip = pd.read_csv("C:\\Users\\moniy\\Desktop\\GelLab\\Data\\PrecipEdgeList_NoAbsolute_combinedv1.csv")
data_credit_score = pd.read_csv("C:\\Users\\moniy\\Desktop\\GelLab\\Data\\CreditScoreEdgeList_NoAbsolute_combinedv1.csv")

data_claims["V3"].fillna(0,inplace=True)
data_claims["Weight"] = abs(data_claims["V3"])
data_claimamount["V3"].fillna(0,inplace=True)
data_claimamount["Weight"] = abs(data_claimamount["V3"])
data_houseage["V3"].fillna(0,inplace=True)
data_houseage["Weight"] = abs(data_houseage["V3"])
data_credit_score["V3"].fillna(0,inplace=True)
data_credit_score["Weight"] = abs(data_credit_score["V3"])
data_precip["V3"].fillna(0,inplace=True)
data_precip["Weight"] = abs(data_precip["V3"])

G_claims=nx.from_pandas_dataframe(data_claims, 'V1', 'V2', ['Weight'])
G_claimamount=nx.from_pandas_dataframe(data_claimamount, 'V1', 'V2', ['Weight']) ##Node added with 0 weighted edge
G_houseage=nx.from_pandas_dataframe(data_houseage, 'V1', 'V2', ['Weight'])
G_credit_score=nx.from_pandas_dataframe(data_credit_score, 'V1', 'V2', ['Weight'])
G_precip=nx.from_pandas_dataframe(data_precip, 'V1', 'V2', ['Weight']) #  3 nodes added with 0 weighted edge


node2vec = Node2Vec(G, dimensions=10, walk_length=15, num_walks=1000, p = 10,q=1,weight_key="V3")
model = node2vec.fit(window=50, min_count=1)
X = model[model.wv.vocab]

node2vec_claims = Node2Vec(G_claims, dimensions=10, walk_length=15, num_walks=1000, p = 3,q=1,weight_key="V3")
model_claims = node2vec_claims.fit(window=30, min_count=1)
X_claims = model_claims[model_claims.wv.vocab]

node2vec_claimamount = Node2Vec(G_claimamount, dimensions=10, walk_length=15, num_walks=1000, p = 3,q=1,weight_key="V3")
model_clmamt = node2vec_claimamount.fit(window=30, min_count=1)
X_clmamt = model_clmamt[model_clmamt.wv.vocab]

node2vec_houseage = Node2Vec(G_houseage, dimensions=10, walk_length=15, num_walks=1000, p = 3,q=1,weight_key="V3")
model_houseage = node2vec_houseage.fit(window=30, min_count=1)
X_houseage = model_houseage[model_houseage.wv.vocab]

node2vec_precip = Node2Vec(G_precip, dimensions=10, walk_length=15, num_walks=1000, p = 3,q=1,weight_key="V3")
model_precip = node2vec_precip.fit(window=30, min_count=1)
X_precip = model_precip[model_precip.wv.vocab]

node2vec_credit_score = Node2Vec(G_credit_score, dimensions=10, walk_length=15, num_walks=1000, p = 3,q=1,weight_key="V3")
model_creditscore = node2vec_credit_score.fit(window=30, min_count=1)
X_credit_score = model_creditscore[model_creditscore.wv.vocab]

X_final = np.c_[X_claims,X_houseage,X_credit_score,X_precip,X_clmamt]

test = list()

for key,value in model.wv.vocab.items():
    test.append(key)

X_nodes = np.c_[X_final,test]
X_nodes_df = pd.DataFrame(X_nodes)

################################# Clustering Algorithms ##############################

algorithms = {}
sse = {}

#################################### K means #######################################

###### Elbow Curve for K-Means ###############
for k in range(1, 500):
    kmeans = KMeans(n_clusters=k, max_iter=1000).fit(X_final)
    sse[k] = kmeans.inertia_ # Inertia: Sum of distances of samples to their closest cluster center
plt.figure()
plt.plot(list(sse.keys()), list(sse.values()))
plt.xlabel("Number of cluster")
plt.ylabel("SSE")
plt.savefig("C:\\Users\\moniy\\Desktop\\GelLab\\Results\\New folder\\KmeansElbowPLotp10q1w50separate")
plt.show()

################# Run K-means after looking at elbow curve ########################################

algorithms['kmeans'] = cluster.KMeans(n_clusters=8, init='k-means++', max_iter=100, n_init=5,verbose=1)
kmeans = algorithms['kmeans'].fit(X)
labels = algorithms['kmeans'].labels_ # Getting the cluster labels
Cluster_Output = pd.DataFrame(np.c_[test,labels])
Cluster_Output.columns = ["PostalCode","KmeansLabels"]
centroids = kmeans.cluster_centers_ # Centroid values

print("Silhouette Coefficient: %0.3f"
      % metrics.silhouette_score(X, algorithms['kmeans'].labels_, metric='sqeuclidean'))

print("Calinski Harabaz: %0.3f"
      % metrics.calinski_harabaz_score(X, algorithms['kmeans'].labels_)) ##Higher is better


########################## Plot on Canadain map #############################################

zipcodes = pd.read_csv("C:/Users/moniy/Desktop/GelLab/Data/CanadaZipCodes.csv")

X_aff = pd.DataFrame(np.c_[X,test])

## Join lat long data to tsne
X_aff.rename(columns={ X_aff.columns[50]: "PostalCode" },inplace = True)
latlong_aff = pd.merge(X_aff,zipcodes,how = 'left', left_on = 'PostalCode', right_on = 'Postal Code')
latlong_aff['cluster'] = algorithms['affinity'].labels_
