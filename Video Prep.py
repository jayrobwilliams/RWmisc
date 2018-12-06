# -*- coding: utf-8 -*-
"""
author: Rob Williams
contact: jayrobwilliams@gmail.com
created: 12/05/18
updated: 12/06/18

this script uses openCV to split a video into a user specified number of 
images captured at evenly spaced intervals throughout the video
"""

import os
import cv2

# video capture https://stackoverflow.com/questions/22704936/reading-every-nth-frame-from-videocapture-in-opencv

def vid_split(file, outdir, frames):
    """
    function to split a video into images and save the images to an outdir
    """
    
    # get file name minus any directories
    file_n = file.split('/')[-1]
    
    # get directory structure before file name
    file_d = '/'.join(file.split('/')[:-1])
    
    # get file name for output directory
    file_s = file_n.split('.')[0]
    
    # create path for output files for video
    out_path = outdir + '/' + file_s + '/'
    
    # create subdirectory for saved frames
    os.makedirs(out_path, exist_ok=True)
    
    # open video connection
    cap = cv2.VideoCapture(file_d + '/' + file_n)
    
    # get total number of frames in video
    frame_cnt = int(cap.get(cv2.CAP_PROP_FRAME_COUNT))
    
    # get list of frame numbers to save
    frames_sv = list(range(0, frame_cnt, int(frame_cnt / frames)))
    
    # read first frame
    ret, frame = cap.read()
    
    while(ret):
        
        # get frame number
        frame_no = int(round(cap.get(1)))
        
        # read next frame
        ret, frame = cap.read()
        
        # write frame as image if frame no matches frame list
        if any(x == frame_no for x in frames_sv):
            cv2.imwrite(out_path + "frame%d.jpg" % frame_no,
                        cv2.resize(frame, (300, 300)))
    
    # release video connection
    cap.release()


# create directory to hold processed images
os.makedirs('proc/vid_proc', exist_ok=True)

# get subdirectories in image directory, omitting hidden
vids = [f for f in os.listdir('Videos') if not f.startswith('.')]

# iterate through videos
for i in vids:
    
    # split video i
    vid_split('Videos/' + i, 'proc/vid_proc', 20)

# quit script
quit()