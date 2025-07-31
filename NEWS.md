# duet 0.2.0

*   This is a new version of the package.
*   Added new function `op_summarise()` for producing summary statistics of the time-series data.
*   Added new function `op_compute_motionenergy()` to produce motion energy i.e. frame-differencing.
*   Added new function `op_compute_coherence()` utilises the motion energt data to produce coherence measure.
*   Bug fix in `op_plot_openpose`, previous versions lines = TRUE did not work.
*   Improved performance of `op_create_csv`, it now utilises either the OpenPose tracking or derives participants by static seating position.