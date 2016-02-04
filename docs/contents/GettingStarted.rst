.. _GettingStarted:

===============
Getting Started
===============

Example
-------

This example will use data from a real experiment stored at EMBL-EBI. The data
can be accessed at http://www.ebi.ac.uk/ena/data/view/SRP023199 and represent
**HeLa cells**. The idea is to preprocess the data set, map it against the
human genome and count the reads that overlap with known genes.

We will use the fastQ file
ftp://ftp.sra.ebi.ac.uk/vol1/fastq/SRR867/SRR867735/SRR867735.fastq.gz that can
be accessed in the table, on column **Sample accession**, with value
SAMN02179475.


Create your script
------------------

Since the web server is now open you can start to create your script. Start by
clicking in the tab **Create Script** from the navigation menu, as in the
following image:

.. image:: ../images/nglessKeeperEmptyClickCreateScripts.png

After clicking, a page as the following should be displayed.

.. image:: ../images/createScript.png

As you can see, there are two ways to create your scripts. At the left there is
a text editor that allows to edit and create your scripts and at the right is a
wizard that generates a script with little to no effort.

.. image:: ../images/createScriptDivision.png

Choose the mode that will make you feel more comfortable with.

Text Editor
-------------


Load fastQ file
~~~~~~~~~~~~~~~

Before creating the whole script lets start by understanding our data set. This
first step will allow you to perform quality control.

Using the **text editor**, type:

::

	ngless "0.0"

	/* load the data set */
	input = fastq('SRR867735.fastq.gz')

You can now save the script (as **test.ngl** for example) to the directory
where the file ``SRR867735.fastq.gz`` is and run ngless::

	$ ngless test.ngl

Using a web browser, you can open the file ``test.output_ngless/index.html`` to
see information about a data set and the ngless job. At 'Before QC' there will
be the result of the execution.

.. image:: ../images/resultBeforeQC.png

We can now see that the data set has:
	
	- +- 50% of guanine and cytosine.

	- Follows the Encoding Sanger.

	- Has 32456161 sequences

	- And all sequences have the same length (50).

Also, by analyzing the plot we can see that the first 3 base pairs, on average, have the lowest quality (31.0). So, a good preprocess starts by removing the first 3 base pairs. 

Feel free to explore all the available statistics.

Preprocess
~~~~~~~~~~

For the preprocess we will:

	- Remove the first **3** base pairs.

	- Substrim with a minimum quality of **15**.

	- Discard if the length of a read is **smaller than 20**.

Let's add the following code to the already existent code in the Text Editor.

::
	
	preprocess(input) using |read|:
            read = read [3:] // Discard from position 0 until 3 (excluded).
    	    read = substrim(read, min_quality=15)
    	    if len(read) < 20:
	         discard


This will generate quality control that will be detailed at the execute section.

Map
~~~

After adding the preprocess code to the Text Editor, it's time to map against the human genome. Since the human genome is provided by default, you can simply do:

::

	/* reference genome */
	human = 'hg19'
	mapped = map(input, reference=human)


Annotate
~~~~~~~~

We are only interested in the human genes so lets annotate the mapping results
to the corresponding genes.  Since we used a genome provided by NGLess, we do
not need to specify which annotation file to use (it'll be built in)::

	/* features to annotate */
	feats = [{gene}]
	counts = count(mapped, strand=false, mode={union}, keep_ambiguous=false, features=feats)


Write to disk
~~~~~~~~~~~~~

Finally, we write the results to a file::

	/* write counts to disk */
	write(counts, ofile="samples/CountsResult.txt")

Final Script
~~~~~~~~~~~~~~

At the end, your Text Editor should have the following code:

.. image:: ../images/teScript1.png

Jump to section **Execute** to run the script and see the results.

Wizard
--------

With the wizard you should start like in the following image:

.. image:: ../images/wizard1.png

Load fastQ file
~~~~~~~~~~~~~~~

To load the file 'SRR867735.fastq.gz' you should click 'add file' which will open a box that you can type the file name.

.. image:: ../images/wizard2.png

Important to notice in the image also that the script is created in real time. This means that you can see the modifications while making them.

You can now click in 'Next Section' to go to Pre-Process.

Preprocess
~~~~~~~~~~~~~~

At the preprocess we are going to:

	a) Remove the first **3** base pairs.

	b) Substrim with a minimum quality of **15**.

	c) Discard if the length of a read is **smaller than 20**.

To do a), since we want to remove the first 3 base pairs, we need to make a left trim of 3. You can see at the bottom the script changing while making the modifications.

.. image:: ../images/wizard3.png

For b) we want to make a substrim and for that you should update the field as shown in the following image:

.. image:: ../images/wizard4.png

For c) we want to make a discard and since this is a common operation our wizard as a special field for that:

.. image:: ../images/wizard5.png

Now, after filling all the values, the script displayed at the bottom should look as follows:

.. image:: ../images/wizard6.png

Since the unique operation is not applied, click twice in 'Next Section' to jump to Map.


Map
~~~~~~~~~~~~~~

Since the human genome is provided by default, you can simply fill the reference field with 'hg19':

.. image:: ../images/wizard7.png

Your script should look exactly like the script at the end of the previous image.

You can now click in 'Next Section' to go to Annotate.


Annotate
~~~~~~~~~~~~~~

We are only interested in the human genes so lets annotate the map results with only the genes as features.

.. image:: ../images/wizard8.png


**Do not allow** ambiguity when deciding a feature.

.. image:: ../images/wizard10.png

Allow the matches to be in any strand, positive or negative.

.. image:: ../images/wizard11.png


Your script should look exactly like this:

.. image:: ../images/wizard12.png

You can now click in 'Next Section' to go to Count.

Count
~~~~~~~~~~~~~~~

We are only interested in counting genes. It's quite redundant choosing 'gene' again but while using the wizard this field is mandatory.

.. image:: ../images/wizard13.png

You can now click in 'Next Section' to go to Write.


Write
~~~~~~

In the write you should fill the file destiny as 'CountResults.txt' and your final script should look like the following image:

.. image:: ../images/wizard14.png

You can now click in 'Next Section' to complete the script generation.


Final Script
~~~~~~~~~~~~

At the end you should see the final script:

.. image:: ../images/wizard15.png

Jump to section **Execute** to run the script and see the results.

Execute
--------

You can now save the script (as **test.ngl** for example) to the directory where the file 'SRR867735.fastq.gz' is and run ngless.
::

	$ ngless test.ngl

As a result of the execution, should be returned the following:

.. code-block:: bash

	Total reads: 31654060
	Total reads aligned: 28095945[88.76%]
	Total reads Unique map: 22434229[79.85%]
	Total reads Non-Unique map: 5661716[20.15%]
	Total reads without enough qual: 0

These are statistics of the map of the file against the human genome.

All other results can be accessed through the web server by doing. As you might already be running a webserver from the previous execution, open a new webserver at port 8080 or close the one used before:

::

	$ ngless visualizemode test -p 8080


The results are in the following (sub)sections.

After quality control
~~~~~~~~~~~~~~~~~~~~~

As the function preprocess was used, quality control is generated.

It can be visualized at the tab 'After QC' as shown next:

.. image :: ../images/resultAfterQC.png

As can be seen the quality has increased and the minimum quality is now 34.
Also, the minimum and maximum ([min,max]) sequence decreased from [50,50] to
[20,47] which implies a decrease in both the sequence length minimum and
maximum, 30 and 3 respectively.

Counts
~~~~~~~~~~~~~~~~~~~~

In order to access the top gene counts, you can use the 'Visualize' tab in the navigation menu.

You should be able to see a table with all results.

You should be able to see a list of all files at the column on the left. Click on the one named 'CountResults' that is representative of the annotation results of the script.

By clicking on the **counts column** you will be able to sort the counts in descending order. By default are shown 20 genes at a time, but you can define the amount to either 10, 25, 50 or 100.

If you sort in descending order and select to be displayed 10 results, you should be able to see the top 10 results with most counts. If everything went well they should be:

=============== =======
Gene name       Counts
=============== =======
ENSG00000210082	2901346
ENSG00000265150	182390
ENSG00000269900	179083
ENSG00000202198	175199
ENSG00000211459	165836
ENSG00000259001	116589
ENSG00000269028	98050
ENSG00000187608	95884
ENSG00000126709	94874
ENSG00000067225	82878
=============== =======


Also if you want to edit the file directly you can by opening the file
**'CountResults.txt'** with your preferred text editor.

