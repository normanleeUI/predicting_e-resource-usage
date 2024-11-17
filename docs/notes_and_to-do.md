# Data quality issues/choices
- This data will not include things which are sub-sub units. So, it does not include extension.
- The dates in VERSO only have the calendar year, so I'll just have to pick which fiscal year those go into, or do multiple analyses.
- This data does not include non-degree student enrollment

# TO DO:

## Aggregation:
- assign unique ID's based on fiscal year + college matches
- figure out row and column formatting for usage data
- trim data to accommodate date range of shortest (longitudinally) dataset (looks like FY 2014?)
- decide how to convert the AY stats into FY stats for research assets
    - this will have to be done after the entire dataset is completed via robustness analysis later, systematically varying which ones are assigned which fiscal year, since improving the data is impractical.
- convert all xlsx to csv? before or after analysis?
    - csv would be better because it's not proprietary, but they're in the gitignore so it's not a problem with future commits. It would only be a problem for interoperability/shareability.

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

