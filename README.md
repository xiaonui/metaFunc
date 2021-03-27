# metaFunc

An R package for comprehensive visualization of functional annotations by combining taxonomy

### Introduction
&emsp;&emsp;metaFunc is mainly used to display and interpret the functional annotation of metagenomic data. It will sort out the taxonomic profiling of all functional genes in the microbiome to get the community structure. Then, for each function, the corresponding genes will be grouped according to the taxonomic classification. Next, calculate the number of genes in different samples. Finally, the community structure and functions will be combined and showed in a complex combination block chart. The combination of them provides a full view that helps researchers gain actionable insights. metaFunc provides two usage modes: visual interface and function call. The user-friendly interface will enable users to manipulate the data and customize plot charts.

###  Installing R/RStudio
&emsp;&emsp;If you do not already have R/RStudio installed, please do as follows:

* Install [R](https://www.r-project.org/)
* Install [RStudio](https://www.rstudio.com/)

### Installation

&emsp;&emsp;Check or install packages
```
packages <- c("DT", "ggplot2", "ggrepel", "networkD3", "shiny", "tools", "devtools")
lapply(packages, function(x) {
	if(!require(x, character.only = TRUE)) {
		install.packages(x, dependencies = TRUE)
	}})
```

&emsp;&emsp;Install metaFunc and build vignettes
```
devtools::install_github("xiaonui/metaFunc", build_vignettes = TRUE)
```


### Quick Start

```

# Load the library
library(metaFunc)

# Run browser
blockShiny()

# You can also call the function directly
data(simple_demo)
blockPlot(func_data = simple_demo$func, tax_data = simple_demo$tax, gene_data = simple_demo$gene)

```

### Input data format

&emsp;&emsp;Before starting, you need to prepare two files: `functional annotation`, `taxonomic classification` and `gene profile`. Example files for these two datasets can be found in https://github.com/xiaonui/metaFunc/tree/master/extdata. 

&emsp;&emsp;The `taxonomic classification` needs to contain at least three columns, the first column is the gene, and the rest are the taxonomic classification. There should be no duplication of genes. Unknown taxonomic classification label `Unknown`. If a gene in a certain taxonomic rank is `Unknown`, the lower taxonomic rank should be `Unkown` too. The structure of the data is shown below:

|gene|KO|
|---|---|
|1_GL0070533|K01734|
|1_GL0080019|K01961|
|1_GL0081307|K00382|
|1_GL0089373|K01962;K01963|
|1_GL0089857|K01734|
|...|...|


&emsp;&emsp;The `taxonomic classification` needs to contain at least three columns, the first column is the gene, and the rest are the taxonomic classification. There should be no duplication of genes. Unknown taxonomic classification label `Unknown`. If a gene in a certain taxonomic rank is `Unknown`, the lower taxonomic rank should be `Unkown` too. The structure of the data is shown below:

|gene|superkingdom|phylum|class|order|family|genus|species|
|---|---|---|---|---|---|---|---|
|39_GL0024070|Bacteria|Firmicutes|Negativicutes|Selenomonadales|Acidaminococcaceae|Acidaminococcus|Acidaminococcus intestini|
|39_GL0126084|Bacteria|Firmicutes|Clostridia|Clostridiales|Lachnospiraceae|Unknown|Unknown|
|39_GL0173546|Bacteria|Unknown|Unknown|Unknown|Unknown| Unknown|
|...|...|...|...|...|...|...|...|

&emsp;&emsp;The `gene profile` should contain at least two columns, the first column is the gene name, and the rest are gene profile. There should be no duplication of genes. `0` will be considered to be absence, and positive numbers means the gene is be present. The structure of the data is shown below:

|gene|Sample1|Sample2|Sample3|Sample4|Sample5|
|---|---|---|---|---|---|
|10_GL0000131 |3.877686e-05 |4.179069e-05 |2.623480e-05 |2.595388e-05 |4.729010e-05|
|10_GL0000704 |2.481456e-06 |1.667823e-06 |3.718647e-06 |3.267785e-06 |0.000000e+00|
|10_GL0000737 |4.889229e-05 |5.077160e-05 |3.067506e-05 |3.158846e-05 |6.150183e-05|
|10_GL0002188 |4.750236e-06 |6.460417e-07 |2.673729e-06 |1.762167e-06 |1.597184e-06|
|10_GL0002472 |2.713468e-06 |1.304702e-05 |1.605089e-05 |2.859085e-05 |5.978519e-06|
|10_GL0003279 |2.338997e-06 |6.998244e-06 |6.673882e-06 |8.360981e-06 |4.583229e-06|
|...|...|...|...|...|...|


### Run browser
&emsp;&emsp;The user interface contains three tabs:"Upload Data", "Overview", and "Combination Block Chart".

#### Upload Data
&emsp;&emsp;To begin the analysis, you need to upload `functional annotation`, `taxonomic classification` and `gene_profile` (comma-separated (.csv) or tab-separated (.txt) format). 

<center>
<figure>
<img src='vignettes/figure/Fig1.png' width='30%'>

Fig 1. Interface for uploading data
</center>
</figure>

If you do not have datasets, you can use the demo data file by clicking on the "Load Demo (Xiao L et al.)" button. The demo is a part of the entire data for demonstration. It contains genes related to propionate metabolism in the non-redundant gene catalog. After the data is uploaded and checked, it will be displayed on the right, and the result tabs will automatically appear.

<center>
<figure>
<img src='vignettes/figure/Fig2.png' width='70%'>

Fig 2. Successfully uploaded data
</center>
</figure>

#### Overview
&emsp;&emsp;At first, you will be greeted with a data summary section: a bar plot showing the functions and the number of corresponding genes. You can download the picture (.pdf) by clicking "Download Plot".

<center>
<figure>
<img src='vignettes/figure/Fig3.png' width='70%'>

Fig 3. Data summary section
</center>
</figure>

If you double click in a brush on the plot, the chart will be zoomed to the brush bounds. And double-clicking again (outside brush) will reset the zoom.

<center>
<figure>
<img src='vignettes/figure/Fig4-1.png' width='35%'>
<img src='vignettes/figure/Fig4-2.png' width='35%'>

Fig 4. Zoom the plot
</center>
</figure>


&emsp;&emsp;The data will be shown in the table below. You need to select the functions of interest. The selected data will be displayed on the right. 

<center>
<figure>
<img src='vignettes/figure/Fig5.png' width='70%'>

Fig 5. Data selection
</center>
</figure>


#### Combination Block Chart
&emsp;&emsp;For the functions selected in the table, the corresponding genes' data and their taxonomic classification will be extracted. The page will load with a complex combination block chart. You can download the picture (.pdf) by clicking "Download Plot". 

<center>
<figure>
<img src='vignettes/figure/Fig6.png' width='70%'>

Fig 6. Combination block chart (split percentage = 10%)
</center>
</figure>

&emsp;&emsp;You can change the "Tax Split Percentage" to adjust the taxonomic block.

<center>
<figure>
<img src='vignettes/figure/Fig7.png' width='70%'>

Fig 6. Combination block chart (split percentage = 60%)
</center>
</figure>

&emsp;&emsp;You can hide the lollipop chart by adjusting the transparency.

<center>
<figure>
<img src='vignettes/figure/Fig8-1.png' width='15%'>
<img src='vignettes/figure/Fig8-2.png' width='55%'>

Fig 7. Combination block chart
</center>
</figure>

&emsp;&emsp;Besides using brush and double-click to zoom, you can also click the point in the figure
 
<center>
<figure>
<img src='vignettes/figure/Fig9.png' width='70%'>

Fig 8. Click the point
</center>
</figure>

&emsp;&emsp;The detailed information will be displayed below. 

<center>
<figure>
<img src='vignettes/figure/Fig10.png' width='70%'>

Fig 9. The detailed information of point
</center>
</figure>

&emsp;&emsp;The detailed taxonomic annotations of all the corresponding genes can be viewed in the table.

<center>
<figure>
<img src='vignettes/figure/Fig11.png' width='70%'>

Fig 10. The data of genes
</center>
</figure>

&emsp;&emsp;The more taxonomic classification will be shown in a sankey plot.

<center>
<figure>
<img src='vignettes/figure/Fig12.png' width='70%'>

Fig 11. A sankey plot
</center>
</figure>


## Reference
 Xiao L, Sonne SB, Feng Q, et al. High-fat feeding rather than obesity drives taxonomical and functional changes in the gut microbiota in mice. Microbiome. 2017;5(1):43.


