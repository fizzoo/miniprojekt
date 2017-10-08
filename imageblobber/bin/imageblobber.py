#!/usr/bin/env python3

# Note: was faster to do one full norm each iteration instead of masked
# cv.norm. Norming both numpy-sliced was even faster (unlike how it seemed in
# my early tests, weirdly), and should obviously scale better with larger
# images. Also allows us to only copy that patch.

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


def blob(infile, outfile, nr_blobs, radius, count_tries):
    """Do the blobbing thing.

    Read infile, create nr_blobs blobs of radius radius that make the image
    more like the infile, and write the result to outfile."""

    # Ground truth
    gt = cv2.imread(infile)

    print("Infile", infile, "size:", gt.shape)

    out = np.zeros(gt.shape, dtype=gt.dtype)

    tries = 0
    successes = 0
    bar = tqdm.tqdm(total=nr_blobs)
    while tries < nr_blobs if count_tries else successes < nr_blobs:
        tries += 1
        if count_tries:
            bar.update(1)

        rngcolor = get_random_color(gt)
        x, y = get_rng_index(gt)

        minx = max(x-radius, 0)
        maxx = min(x+radius, gt.shape[0]-1)
        miny = max(y-radius, 0)
        maxy = min(y+radius, gt.shape[1]-1)

        # Test patch for drawing on without ruining the out image
        test_patch = out[minx:maxx, miny:maxy].copy()

        # x-minx, for example, is simply r unless x < radius, then x
        place_circle(test_patch, (y-miny, x-minx), np_to_color(rngcolor),
                     radius)

        a = cv2.norm(test_patch,
                     gt[minx:maxx, miny:maxy],
                     cv2.NORM_L2)
        b = cv2.norm(out[minx:maxx, miny:maxy],
                     gt[minx:maxx, miny:maxy],
                     cv2.NORM_L2)
        if a < b:
            place_circle(out, (y, x), np_to_color(rngcolor), radius)
            successes += 1
            if not count_tries:
                bar.update(1)

    bar.close()

    cv2.imwrite(outfile, out)

    print("Finished! Wrote", outfile, "after", tries, "tries.")


def main():
    parser = argparse.ArgumentParser(description='Blobify an image.')
    parser.add_argument('infile', help='File name of input')
    parser.add_argument('outfile', help='File name of output')
    parser.add_argument('-nr', type=int, default=1000,
                        help="""Number of blobs/tries (execution time scales
        exponentially or so with this)""")
    parser.add_argument('-tries', action='store_true',
                        help='Use tries instead in -nr')
    parser.add_argument('-r', type=int, default=16, help='Radius of blobs')

    args = parser.parse_args()

    blob(args.infile, args.outfile, args.nr, args.r, args.tries)


if __name__ == '__main__':
    main()
