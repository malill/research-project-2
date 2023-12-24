# Assimilation Effects in Recommender Systems - Wiki

This repository contains all necessary information to run models, retrieve table and generate paper output.

> The paper is published and presented on [ICIS 2022](https://aisel.aisnet.org/icis2022/online_reviews/online_reviews/3/). For reference please use the following citation:

```
Lill, M., and Spann, M. 2022. "Influence of Assimilation Effects on Recommender Systems," Proceedings of the 43rd International Conference on Information Systems, Copenhagen.
```

## Data Retrieval Process

The data used for analysis are the following:

- `click_behavior.csv`: response from `reco-api` analysis endpoint.
- `img_sim.csv`: calculated by `reco-api` service `image_similarity`
- `items.csv`: retrieved from SAP Commerce flexible search query

After retrieving click behavior you need to run `reco_similarity` notebook to retrieve the processed data.

The processed data `click_behavior_processed.csv` can then be consumed by statistical inference models.

## Data Generation Process

The general data process is structured as follows:

1) Recommender System API endpoint -> **raw data**
2) Python Data Enrichment Process (can contain additional external data) ->  **processed data**
3) R Detailed Filtering Process -> **final data**

- The javascript library `reco2js` is uploaded locally to the FK Austria Wien shop system.
- `reco2js` provides recommendations and collects evidence.
- The endpoint `/analytics/get_csv` retrieves the evidence generated by the recommender API `reco-api`.
    - Currently the a the unpublished feature branch `feat/img` of `reco-api` contains necessary endpoint. Therefore, a
      local instance of the project needs to be executed and connected to the productive database. This can be removed
      as soon as the new version of `reco-api` is published.
- A document `click_behavior.csv` can be downloaded from endpoint.
- The filename of `click_behavior.csv` needs to be extended with current download date,
  e.g. `click_behavior_20220415.csv`
- Two CSV files `items_VERSION.csv` and `img_sim.csv` need to be provided in order to retrieve item attributes and
  calculate similarities. Both files should only be generated once, e.g. both steps only need to be executed once (at
  the beginning of the experiment):
    - `items_VERSION.csv` can be derived from shop system via custom search query.
    - `img_sim.csv` can be calculated through the python method `image_similarity.py` of the `reco-api`. Images need to
      be provided on same directory as method.
- The python notebook `reco_similarity.ipynb` needs to be executed to generate `click_behavior_processed_VERSION.csv`.
- `click_behavior_processed_VERSION.csv` can be consumed by the respective R scripts for descriptive and inductive
  statistics

## Data Description

- The output file `click_behavior_processed_VERSION.csv` still contains new items. Therefore, (if applicable) new items
  need to be removed prior to analysis. New items need to be detected from shop system. The R
  script `remove_new_items.r` can remove those and creates a new file `click_behavior_final_VERSION.csv`

Variables:

Processed Data Set:
- `focal_item`: ID of product that is visited (PDP)
- `click_item`: ID of recommended product the user has clicked on (0 if none)
- `click`: Did user click on any recommended product
- `click_pos`: Position of product in recommended list, user has clicked on
- `reco_item[x]`: ID of recommended product on position [x]
- `focal_item_price`: Price of the visited product (PDP)
- `reco_item[x]_price`: Price of recommended product on position [x]
- `f/r[x]_cat`: Categories of focal/recommended product
- `f_r[x]_cat`: Does focal product share categories with recommended product on position [x]
- `r[x]_r_cat`: Does the recommended product at [x] share at least one category with any of the other two recommended
  products
- `f_r_cat`: Number of shared categories between focal and all recommended products
- `c_r_cat`: Is any category of the focal product represented at least once in each of the recommended product
  categories
- `f_r[x]_price`: Price difference between focal and recommended product at position [x]
- `f_r_price`: Mean price difference between focal and recommended products
- `r_price_mean`: Average price of recommended products
- `f_r[x]_img`: Image similarity between focal and recommended product at position [x]
- `f_r_img`: Mean image similarity between focal and recommended products