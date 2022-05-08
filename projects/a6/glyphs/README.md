# glyphs for multivariate information

An R package for creating glyphs for use in visualization by Jack (Jiahua) Liu and Wayne Oldford

Any plot produced by the graphics package or the grid package can be turned into a glyph.

Makes glyphs using a pixel-oriented data visualization technique. 

Includes methods for space-filling glyphs such as:

- Hilbert-Peano curves
- Morton curves
- Daniel Keim's recursive rectangular layout curves (excellent for time-ordered data).

The package can map data values to pixel colors and order them in a plane in a specific way such as Hilbert curve 
to get a glyph for each dimension of the data sets. Then, one can plot them in the device. 

The mapping from data values to colors has a default function and also a plot function, 
but one can provide their own function as well. 

The core of the package is to make a list of glyphs given a multidimensional data set.
