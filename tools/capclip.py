#!/usr/bin/python
import sys
import gi
gi.require_version("Gtk", "3.0")
from gi.repository import Gtk, Gdk


if len(sys.argv) != 2:
    print("Only specify the target file.")
    sys.exit(1)

fname = sys.argv[1]

clipboard = Gtk.Clipboard.get(Gdk.SELECTION_CLIPBOARD)
image = clipboard.wait_for_image()

if image is not None:
    image.savev(fname, "png", [], [])
    print(f"PNG image saved to file {fname}")

else:
    print("No image in clipboard found")
    sys.exit(1)
