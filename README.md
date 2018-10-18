# hs-hdri

HDRI (.hdr) files are a common image format for high dynamic range photographs
and environment probes.

This library is a simple (and rather slow) attempt at loading such
files into vectors of uncompressed pixel data.

Unlike the JuicyPixels library, pixels can be converted to either
half or float RGB triples, or the user can supply an arbitrary RGBE to pixel
converter.
