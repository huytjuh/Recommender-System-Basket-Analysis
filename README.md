# **Recommender System Basket Analysis**

Basket-Sensitive Random Walk & Factorization Machine Recommendations for Grocery Shopping. 
Item-based Collaborative Filtering (CF) using hybrid memory- and model-based methods with Factorization Machines and Alternative Least Squares.

R implementation from scratch inspired by paper [Li et al (2009)](https://www.researchgate.net/profile/Paulo-Lisboa/publication/221653590_Grocery_shopping_recommendations_based_on_basket-sensitive_random_walk/links/09e4150cb9fb091a30000000/Grocery-shopping-recommendations-based-on-basket-sensitive-random-walk.pdf).

![](https://www.aarki.com/hubfs/ML-recommendation-engine-1.jpg)

---

## Introduction
While recommendation systems have been a hot topic for a long time now due to its success in business applications, it is still facing substantial challenges. As grocery shopping is most often considered as a real drudgery, many online stores provide a shopping recommendation system for their customers to facilitate this purchase process. However, there is still a large majority of people who still hesitate from doing their groceries online even though this form of shopping provides consumers with distinct advantages. 

Hence, the chasm between online retail and its brick-and-mortar counterpart keeps expanding in numbers, and people’s shopping preferences are evolving in turn, leaving retailers with little choice but to adapt.  However, this has led to online grocery shopping becoming more and more prominent, and therefore resulted in radical adjustments within the marketing decision framework of many retailers. Thus, we investigate whether traditional collaborative filtering techniques are applicable in the domain of grocery shopping, and further improve its recommendations using more advanced models and machine learning techniques. Hence, various CF-based models have been constructed including your traditional similarity-based collaborative filtering models, a basket-sensitive random walk model, and a basket-sensitive factorization machine. Here, we found that our basket-sensitive factorizationmachine comes out on top when it comes to recommending less popular items. However, due to its computational time, it remains to be a question whether this model is applicable in practical use.

## Prerequisites

* python 3.8
* tensorflow 2.4
* Keras 2.1.4
* NVIDIA GPU + CUDA CuDNN
* Blender 2.92 (custom-built)

## Getting Started

### Installation Blender 2.92 Custom-Built
* Enable rendering of viewer nodes in background mode to properly update the pixels within Blender background.
* Open `source/blender/compositor/operations/COM_ViewerOperation.h` and change lines:
```
bool isOutputOperation(bool /*rendering*/) const { 
if (G.background) return false; return isActiveViewerOutput();
```
into:
```
bool isOutputOperation(bool /*rendering*/) const {return isActiveViewerOutput(); }
```
* Open `source/blender/compositor/operations/COM_PreviewOperation.h` and change line:
```
bool isOutputOperation(bool /*rendering*/) const { return !G.background; }
```
into:
```
bool isOutputOperation(bool /*rendering*/) const { return true; }
```

### Create Synthetic Images in Blender + Annotations
* Render 3D person images.
```
#!./scripts/run_blender.sh
"Blender Custom/blender.exe" --background --python "Data/Blender.py" -- 1
```
* Annotation files are saved in the respective `.txt` file with the same name and has the following format:
```
image_file_path min_x,min_y,max_x,max_y,class_id min_x,min_y,max_x,max_y,class_id ...
```

### Run YOLOv3 Blender synthetic model
* Run Trained Blender Synthetic Model.
```
#!./scripts/run_yolov3.sh
python3 scripts/yolo_video.py --image
python3 scripts/evaluation.py
```
* The bounding box predictions are saved in folder `output`.
* Performance scores and evaluation metrics are saved in `Evaluation` (Default is `overlap_threshold=0.5`).

## Custom Datasets for YOLOv3 Blender Training

### YOLOv3 Blender Training 
* Select & combine annotation files into a single `.txt` file as input for YOLOv3 training. Edit `Annotations/cfg.txt` accordingly.
```
!./scripts/run_annotations.sh
python3 Annotations/Annotation_synthetic2.py
```
* Specify the following three folders in your `Main/Model_<name>` folder required to train YOLOv3 model:
  * `Model_<name>/Model`: `synthetic_classes.txt` (class_id file) and `yolo_anchors.txt` (default anchors).
  * `Model_<name>/Train`: `DarkNet53.h5` (default .h5 weight) and `Model_Annotations.txt` (final annotation `.txt` file).
  * `Model_<name>/linux_logs`: Saves a `train.txt` logfile and includes training process and errors if there are any.
* Specify learning parameters and number of epochs in `train.py`. Defaults are:
  * Initial Stage (Freeze first 50 layers): `Adam(lr=1e-2)`, `Batch_size=8`, `Epochs=10`
  * Main Process (Unfreeze all layers): `Adam(lr=1e-3)`, `Batch_size=8`, `Epochs=100`
* Recompile anchor boxes using `kmeans.py` script (OPTIONAL)
* Configure settings and initialize paths in `Model_<name>/cfg.txt`
* Train YOLOv3 model.
```
!./scripts/run_train.sh
python3 train.py >Main/Model_Synth_Lab/linux_logs/train.log
```

### Benchmark & Evaluate All YOLOv3 Trained Models
* Obtain Precision-Recall (PR) curve and highest F1-scores by iterating through all `Main/Model_<name>/Evaluation` folders and calculate & combine all performance scores.
```
!./scripts/run_scores.sh
python3 scores_all.py
python3 Visualizations/create_graphs.py
python3 Results_IMGLabels/scores_IMGLabels.py
```
* Case-by-case AP-score Evaluation using `Main/scores_IMGLabels.py` (OPTIONAL)
  * Resulting case-by-case evaluation score can be found in `Main/Evaluation_IMGlabels-case.xlsx` with each tab corresponding to a feature kept fixed.

## Extracting RGB Images from Google OpenImages Database v6
Google’s OpenImages Database v6 dataset is used to collect negative non-person samples by extracting pre-annotated images that includes all kinds of objects and environments but without containing instances of persons.
* Non-person images are filtered and downloaded.
```
!./scripts/run_openimages.sh
python3 OpenImages.py > OpenImages/openimages.log
```
* Configure settings and initialize paths in `OpenImages/cfg.txt`.
* Annotation files are saved in the respective `.txt` file with the same name and has the following format:
```
image_file_path min_x,min_y,max_x,max_y,class_id min_x,min_y,max_x,max_y,class_id ...
```

> Source: https://storage.googleapis.com/openimages/web/download.html

## Acknowledgements

Code is inspired by [qqwweee/keras-yolo3](https://github.com/qqwweee/keras-yolo3).
