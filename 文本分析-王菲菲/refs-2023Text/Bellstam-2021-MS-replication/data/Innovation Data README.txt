Innovation data readme.
If you use these data please cite:

  Bellstam, G., S. Bhagat, J. A. Cookson, 2021, 
    A text-based analysis of corporate innovation, 
    Management Science, 67 (7): 4004-4031.

which develops, validates, and uses the disagreement measures in this file.

Data Structure:
Observations are at the firm-year level for firm i that has analyst reports written about it in year t.

Format: The format is .csv with four data fields (headers in the first row).

Data Fields:
gvkey = gvkey firm classifier 
fyear = filing year.
tb_innov = the main innovation measure in our paper, constructed from the 15-topic LDA.
tb_innov_neg = an alternative innovation from our paper, which captures when analysts write about the innovation topic, but in a negative tone.  

Important Notes: 

1) Both innovation measures have been standardized to have a mean of 0 and standard deviation of 1 across the observations in our sample. For exact definitions of the innovation measures refer to the above paper.
