#!/usr/bin/env python3

# Note: was faster to do one full norm each iteration instead of masked
# cv.norm. Norming both numpy-sliced was even faster (unlike how it seemed in
# my early tests, weirdly), and should obviously scale better with larger
# images.

import numpy as np
import cv2
import argparse
import tqdm


def get_rng_index(img):
    """Get random x, y indexes that are in bounds of img."""
    x = np.random.randint(0, img.shape[0])
    y = np.random.randint(0, img.shape[1])
    return x, y


def get_random_color(img):
    """Pick a color randomly from img by random index."""
    x, y = get_rng_index(img)
    color = img[x, y]
    return color


def np_to_color(color):
    """Convert np triplet to acceptable color input."""
    assert color.shape == np.array([3])
    c = ((int(color[0]), int(color[1]), int(color[2])))
    return c


def place_circle(img, xy, color, radius=16):
    """Place a filled circle."""
    cv2.circle(img, xy, radius, color, thickness=-1)


def blob(infile, outfile, nr_blobs, radius):
    """Do the blobbing thing.

    Read infile, create nr_blobs blobs of radius radius that make the image
    more like the infile, and write the result to outfile."""

    # Ground truth
    gt = cv2.imread(infile)

    print("Infile", infile, "size:", gt.shape)

    # Two for "double buffering" (Strictly speaking we don't use it since we
    # need to use the newly drawn info, so always copy)
    new = np.zeros(gt.shape, dtype=gt.dtype)
    old = new.copy()

    assert(gt.shape == new.shape)
    assert(gt.shape == old.shape)

    tries = 0
    successes = 0
    bar = tqdm.tqdm(total=nr_blobs)
    while True:
        rngcolor = get_random_color(gt)
        x, y = get_rng_index(gt)

        mask = np.zeros((gt.shape[0], gt.shape[1]), dtype=np.uint8)
        place_circle(new, (y, x), np_to_color(rngcolor), radius)
        place_circle(mask, (y, x), 1, radius)

        r = radius
        minx = max(x-r, 0)
        maxx = min(x+r, gt.shape[0]-1)
        miny = max(y-r, 0)
        maxy = min(y+r, gt.shape[1]-1)

        a = cv2.norm(new[minx:maxx, miny:maxy],
                     gt[minx:maxx, miny:maxy],
                     cv2.NORM_L1)
        b = cv2.norm(old[minx:maxx, miny:maxy],
                     gt[minx:maxx, miny:maxy],
                     cv2.NORM_L1)
        if a < b:
            old = new.copy()
            successes += 1
            bar.update(1)
        else:
            new = old.copy()

        tries += 1
        if successes >= nr_blobs:
            break

    bar.close()

    cv2.imwrite(outfile, new)

    print("Finished! Wrote", outfile, "after", tries, "tries.")


def main():
    parser = argparse.ArgumentParser(description='Blobify an image.')
    parser.add_argument('infile', help='File name of input')
    parser.add_argument('outfile', help='File name of output')
    parser.add_argument('-nr', type=int, default=1000, help="""Number of blobs
        (execution time scales exponentially or so with this)""")
    parser.add_argument('-r', type=int, default=16, help='Radius of blobs')

    args = parser.parse_args()

    blob(args.infile, args.outfile, args.nr, args.r)


if __name__ == '__main__':
    main()
