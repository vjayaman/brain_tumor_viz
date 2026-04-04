## brain_tumor_viz

### Interactive visualization tool for final project in Comp 7920

 - *app.R* contains the UI and server logic, reads *brain_tumor_dataset.csv*, *functions.R*, and *global_variables.R*

 - *functions.R* is file of miscellaneous plot/ui/filter functions

 - *global_variables.R* is a file of elements useful for input controls like a list of color palettes

 - *dr/* is directory of the outputs from saved dimensionality reduction runs

    - *dr/tsne/*: outputs from varying perplexity values and input columns for t-SNE. User can vary number of iterations during new runs. Saved runs are for 3 dimensions, 3000 iterations. 

    - *dr/pca/*: outputs from varied input columns for PCA

    - *dr/umap/*: outputs from varied input columns for UMAP. Saved runs are for 3 or 2 dimensions, epoch 600 or 200 respectively.
