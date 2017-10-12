#!/usr/bin/env python3

import cv2
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import argparse


def downscale(img, wantedpix):
    """Return downscaled img such that it has approx wantedpix pixels."""
    nrpix = img.shape[0]*img.shape[1]
    scale = 1 if wantedpix > nrpix else np.sqrt(wantedpix/nrpix)

    small = cv2.resize(img, (0, 0), fx=scale, fy=scale, interpolation=cv2.INTER_LANCZOS4)
    # smallpix=small.shape[0]*small.shape[1]
    return small


def vis(imgname, wantedpix):
    """Visualize the img by plotting its rbg space."""
    img = cv2.imread(imgname)
    small = downscale(img, wantedpix)

    cat = np.concatenate(small)/255
    r = cat[:, 0]
    g = cat[:, 1]
    b = cat[:, 2]

    fig = plt.figure()
    ax = Axes3D(fig)

    ax.scatter(r, g, b, facecolors=cat)
    plt.show()


def main():
    parser = argparse.ArgumentParser(
        description='Visualize an image by drawing its colorspace.')
    parser.add_argument('infile', help='File name of input')
    parser.add_argument('-nr', type=int, default=2000,
                        help="Number of pixels to scale to, i.e. points in output")

    args = parser.parse_args()

    vis(args.infile, args.nr)

if __name__ == '__main__':
    main()
