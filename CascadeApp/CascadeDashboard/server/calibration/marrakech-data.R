# In here, write some functions that pull out the Marrakech data.
# We will then use these to MakeAssumptions() based on data.

# For instance, we know that 57% of PLHIV were in CARE in 2015 (From Marrakech Data)
# So we can calculate that value over time to get an estimate of those in care prior.

# For # diagnosed (missing from Marrakech data), so use data from neighbouring countries.
# Ethiopia = 0.6436 of PLHIV
# Tanzania = 0.5859 of PLHIV
# Uganda = 0.6274 of PLHIV
# mean = 0.6436

# We need to import Marrakech data
# Tease it apart so that we know DISCRETE CATEGORIES
# Run some tests to make sure it is all solid?

# By end of day Friday, have an imported dataset containing Marrakech data, previous data etc.
# Ready for model to be updated and fit to.
# Weighting is just the sum of the errors between model and data.
# We then adjust the contribution of individual errors to total error to make them more or less important.
