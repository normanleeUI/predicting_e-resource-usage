This is a repository created by Norman Lee for the Fall 2024 semester of Wildlife 553, taught by Prof. Simona Picardi.

Date created: 2024-09-03

Note: 'code' file is borked due to OneDrive upload weirdness. Will not allow me to make a file named 'code' as part of this repo. figure out a workaround.

The main goals of this repository are:
1. connect several datasets related to e-resource usage:
    1. e-resource usage
    2. enrollment by college
    3. research outputs produced by college
    4. grant expenditures by college
    5. e-resource college relevance
2. make those datasets ready for future analysis
    1. connect them together via a combination of two keys: (college + fiscal year AND college relevance)
    2. clean the data: 
        1. convert so that all data is at college level and fiscal year
        2. generate primary id's
        3. generate/simulate data as needed
3. record a reproducible workflow for generating a clean datasets
4. begin some forms of data exploration, a priori power analysis