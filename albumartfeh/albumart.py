#!/usr/bin/env python

# Copyright (c) 2015, doggone

# Permission to use, copy, modify, and distribute this software for any purpose with
# or without fee is hereby granted, provided that the above copyright notice and this
# permission notice appear in all copies.

# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO
# THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO
# EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
# DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
# IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
# CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.



# Most code courtesy of above person, edited to just change the background

# Requires python-mpd2
from subprocess import call
import mpd, sys
import glob, re

# Required variables
MUSICDIR = "/home/fizzo/musik/"
PATTERN = "front.*"


# Looks for albumart according to PATTERN in MUSICDIR/<song's directory>/
def get_albumart(song):
    albumArt = None
    if(song != "STOPPED"):
        aaDir = re.sub(r"[^/]*$", "", song["file"])
        for albumArt in glob.glob(glob.escape(MUSICDIR + aaDir) + PATTERN):
            break
    return(albumArt)

# Returns the currently playing song or "STOPPED" if playback is stopped
def get_song():
    if(client.status()["state"] == "stop"):
        return("STOPPED")
    else:
        return(client.currentsong())





# Connect with mpd server
try:
    client = mpd.MPDClient()
    client.connect("localhost", 6600)
except(mpd.ConnectionError):
    print("Could not connect to MPD. Exiting.")
    sys.exit(1)

# Monitors mpd for changes and if so, changes image
while True:
    client.idle() #Waits for song change/other
    albumart = get_albumart(get_song())
    if albumart is not None:
        call(["feh", "--bg-max", albumart])
