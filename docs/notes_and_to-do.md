# Data quality issues/choices
- This data will not include things which are sub-sub units. So, it does not include extension.
- The dates in VERSO only have the calendar year, so I'll just have to pick which fiscal year those go into, or do multiple analyses.
- This data does not include non-degree student enrollment

# TO DO:
## ORED research reports:
- delete XLSX monstrosities

- ~~convert ored reports from thousands of dollars to dollars~~
- ~~normalize unit names~~
- ~~generate primary id's~~
~~decide what to do about xlsx monstrosities~~

## Research outputs:
- standardize dates
- create sums of number of research outputs by college, unit, and type
    - make sure you don't double-count research outputs with the same organization listed twice

- ~~Get extension documents? (Parent unit is allowed to be University of Idaho Extension. Then that just goes to cals)~~
    - ~~decided i don't care enough at this preliminary stage~~
- ~~roll-up data so that assets with multiple affiliations only take up one row, with each affiliation in its own cell (OpenRefine)~~
~~- lots of assets have blanks for "asset published date" include most of the ETD's it looks like. Go back and get dates for these.~~
- ~~go back and get COGS & Law publications~~
- ~~find out why Law and COGS doesn't have any pubs~~
    - ~~because I selected based on the parent of the organizational unit, rather than the unit itself. Law doesn't have any sub-units, so the parent was U of I. I deleted all assets with U of I affiliation.~~
- ~~decide how to deal with assets with multiple unit affiliations. Do they count for both units? Or just one? If just one, how is that selected?~~
    - ~~both~~
- ~~normalize organization names~~

## Enrollment:
- ~~roll-up enrollment numbers into college (OpenRefine)
- convert terms into fiscal years
- get spring data and combine with fall data

## Platform usage:
- match platforms to subjects via tags on databases A-Z list
- calculate usage per unique title

## Aggregation:
- assign unique ID's based on fiscal year + college matches
- figure out row and column formatting for usage data
- trim data to accommodate date range of shortest (longitudinally) dataset (looks like FY 2014?)
- decide how to convert the AY stats into FY stats for research assets
    - this will have to be done after the entire dataset is completed via robustness analysis later, systematically varying which ones are assigned which fiscal year, since improving the data is impractical.
- convert all xlsx to csv? before or after analysis?

# Thoughts:
The **final fields** for each of these things should be:

**ORED research reports**:
    columns: fiscal year, college, number of proposals, research expenditures
    keys: fiscal year + college

**Research outputs**:
    columns: fiscal year, college, sum of asset type 1, sum of asset type 2,...
    keys: fiscal year + college

**Enrollment**:
    columns: college, fiscal year, enrollment
    keys: fiscal year + college

**Platform usage**:
    columns: fiscal year, normalized platform name, usage per unique title, relevant college(s)
    keys: fiscal year + college + platform


So, the final excel spreadsheet is going to look something like this:

Columns:
keys:

