# **Recommender System Basket Analysis**

![](https://www.aarki.com/hubfs/ML-recommendation-engine-1.jpg)

Basket-Sensitive Random Walk & Factorization Machine Recommendations for Grocery Shopping. 
Item-based Collaborative Filtering (CF) using hybrid memory- and model-based methods with Factorization Machines and Alternative Least Squares.

R implementation from scratch inspired by paper [Li et al (2009)](https://www.researchgate.net/profile/Paulo-Lisboa/publication/221653590_Grocery_shopping_recommendations_based_on_basket-sensitive_random_walk/links/09e4150cb9fb091a30000000/Grocery-shopping-recommendations-based-on-basket-sensitive-random-walk.pdf).



---

## Introduction
While recommendation systems have been a hot topic for a long time now due to its success in business applications, it is still facing substantial challenges. As grocery shopping is most often considered as a real drudgery, many online stores provide a shopping recommendation system for their customers to facilitate this purchase process. However, there is still a large majority of people who still hesitate from doing their groceries online even though this form of shopping provides consumers with distinct advantages. Hence, the chasm between online retail and its brick-and-mortar counterpart keeps expanding in numbers, and people’s shopping preferences are evolving in turn, leaving retailers with little choice but to adapt.  

This has led to online grocery shopping becoming more and more prominent, and therefore resulted in radical adjustments within the marketing decision framework of many retailers. Thus, we investigate whether traditional collaborative filtering techniques are applicable in the domain of grocery shopping, and further improve its recommendations using more advanced models and machine learning techniques. Hence, various CF-based models have been constructed including your traditional similarity-based collaborative filtering models, a basket-sensitive random walk model, and a basket-sensitive factorization machine. Here, we found that our basket-sensitive factorizationmachine comes out on top when it comes to recommending less popular items. However, due to its computational time, it remains to be a question whether this model is applicable in practical use.

## Colab Notebook

Basket-Sensitive Random Walk & Factorization Machine Recommendation for Grocery Shopping in R:<br/>
[Google Colab]() | [Code]()

## Prerequisites
* Linux or macOS
* Rstudio 
* CPU or NVIDIA GPU + CUDA CuDNN

## Getting Started

### Installation
* Clone this repository.
```
git clone https://github.com/huytjuh/Recommender-System-Basket-Analysis
cd Recommender-System-Basket-Analysis
```
* Install R dependencies using `requirements.txt`.
```
#!./scripts/install_pkgs.sh
while IFS=" " read -r package version; 
do 
  Rscript -e "devtools::install_version('"$package"', version='"$version"')"; 
done < "requirements.txt"
```

### Run Recommender System
* Download a Basket Grocery dataset:
```

```
* Calculate similarity scores and train a model:
```

```
* adasd

## Acknowledgements

Code is inspired by [qqwweee/keras-yolo3](https://github.com/qqwweee/keras-yolo3).
