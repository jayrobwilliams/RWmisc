# -*- coding: utf-8 -*-
"""
author: Rob Williams
contact: jayrobwilliams@gmail.com
created: 12/03/18
updated: 12/06/18

this script uses openCV and scikit-learn to preprocess images using mean 
shift cluster analysis to reduce the complexity of each image.
"""

import os
import cv2
import numpy as np
from sklearn.cluster import MeanShift, estimate_bandwidth

# cluster labels:
# https://spin.atomicobject.com/2015/05/26/mean-shift-clustering/
# cluster means:
# https://jakevdp.github.io/PythonDataScienceHandbook/05.11-k-means.html

# function to mean shift images and return cluster center value for clusters
def msc(img):
    """
    function to use mean shift clustering to identify clusters and values
    """
    
    # convert from three dimensional array to three columns
    img_flat = np.reshape(img, [-1, 3])
    
    # estimate bandwidth
    bwd = estimate_bandwidth(img_flat, quantile=.1, n_samples=500)
    
    #
    if (bwd==0):
        return(img)
    
    # mean shift
    img_ms = MeanShift(bandwidth=bwd, n_jobs=-1, bin_seeding=True, min_bin_freq=5)
    
    # fit mean shift
    img_ms.fit(img_flat)
    
    # 
    img_clusters = img_ms.cluster_centers_[img_ms.predict(img_flat),:]
    
    # return mean shfited image
    return(np.reshape(img_clusters, img.shape))


# create directory to hold processed images
os.makedirs('proc/img_proc', exist_ok=True)

# get subdirectories in image directory, omitting hidden
dirs = [f for f in os.listdir('proc/vid_proc') if not f.startswith('.')]

# iterate through subdirectories
for i in dirs:
    
    # create subdirectory to hold processed images
    os.makedirs('proc/img_proc/' + i, exist_ok=True)
    
    # get images in directory, omitting hidden
    files = [f for f in os.listdir('proc/vid_proc/' + i) if not f.startswith('.')]
    
    # iterate through files in directory
    for j in files:
        
        # read in image in 3 color channel
        img = cv2.imread('proc/vid_proc/' + i + '/' + j, 1)
        
        # mean shift image
        img_ms = msc(img)
        
        # save mean shifted image
        cv2.imwrite('proc/img_proc/' + i + '/' + j, img_ms)

# quit script
quit()