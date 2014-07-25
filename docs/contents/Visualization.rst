.. _Visualization:

=================
Visualization
=================

Launch
-----------------

In order to visualize the script results a web server needs to be launched.
::
  > ngless visualizemode
  $ Launching WebServer at: 8000
  $ You can acess it at: http://localhost:8000   

To change the port from the default 8000, as to be used the flag **-p** [int] or --port=[Int].

If everything went correctly, you should see something like this.
    
.. image:: ../images/keeperOverview.png

Visualize Runs
-----------------
All past executions of NGLess can be consulted, and are organized in a drop down list fashion.

.. image:: ../images/listScripts.png

Each box has information outside as the data set name that we used and the date and time of the execution.

If we open one box, there can be seen three main things: 

- The executed script
- The quality control at the begining
- The quality control after the pre-processing (this one being optional).

Is crucial to save the script that lead to some results. For that reason, scripts are allways associated with the 
results that they generate, allowing this way to easily reproduce an experiment. The script can be 
consulted has shown in the next figure.

.. image:: ../images/scriptNgless.png

Before Quality Control
----------------------
This quality control is always present and it allows to visualize the quality and basic info about a given data set. 

The basic information provided is:

- The original file path
- The percentage of guanine and citosine in the whole data set.
- Encoding prediction.
- The number of sequences.
- The minimum and maximum sequence length of the whole data set.

Not only basic information is provided, but also statistical calculations are made. These statistics are made in relation
to each base pair, and are presented in a interactive plot that allows show/hide these metrics. 

The used statistical measures are in relation to each base pair and are:

- Mean
- Median
- Lower Quartile (25%)
- Upper Quartile (75%)

The Quality control can be accessed in the web interface through the tab **before QC** and a vertical menu allows to
choose which dataset in question. Can be seen an example next:

.. image:: ../images/beforeQC.png

The plot can be adjusted to show one statistic at a time, and the plot limits adapt to the presented values. An image
of only the upper quartile shown looks like the following:

.. image:: ../images/beforeQCUQ.png


After Quality Control
----------------------


Visualize results
----------------------
