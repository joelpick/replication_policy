# replication_policy
Data and code for 'What do ecology and evolutionary biology journal websites communicate about their policies and preferences regarding replication studies?'

**Authors:** Leyla Cabugos, Joel L. Pick, Marija Purgar

**Abstract:** The replication of prior research is a cornerstone of the scientific process and valued as such in ecology and evolutionary biology. However, it appears replication studies are nowhere near as prevalent in the published literature as might be expected if they were a routine part of building confidence in research findings. This may be due in part to the widespread perception of replication studies as a low-status activity. Journal policies have an important role in shaping researchers’ behaviour and willingness to conduct and publish replications. Many journals project a preference for novel methods, findings, or ideas,  which discourages, and sometimes overtly precludes, submission of replication studies, while others actively invite them. Given this variety, authors have an interest in determining the extent to which replication studies are welcomed by their target journal, but this can be hard to identify from journal websites. We examined what information about journal policy on replication studies could be found on the websites of 233 ecology/evolutionary biology journals. We found that only 31 of 226 eligible journals (13.7%) provided any information about replication studies on their websites. Among journals that provided information on replications, discouraging or not accepting replication studies was more common than actively encouraging them, with only four journals (1.8%) explicitly encouraging replication studies. In addition, 75 (33%) journals used novelty-related language (i.e., language that may implicitly devalue replication work) on their websites. Together, our findings suggest that explicit policies on replication studies remain rare, and that unclear or discouraging signals about replication research are common in ecology and evolutionary biology journals. We therefore provide recommendations to improve the clarity and transparency of journal policies regarding replication studies.

**Data License:** CC-BY 4.0 (https://creativecommons.org/licenses/by/4.0/)

**Code License:** MIT in R/LICENSE

# Repository organisation Structure

## Data/

### replication.csv
Data from 

**Timestamp:** time and date of extraction
**Extractor:** initials of extractor
**Journal:** journal title
**info_found:** whether information on replication was found, or whether journal was out of scope (three level factor)
**policy_location:** where policy was found
**policy_text1:** example text of policy 
**policy_URL1:** url for example
**policy_text2:** example text of policy, if second text found
**policy_URL2:** url for example, if second text found
**replications_mention:** whether replications were mentioned directly
**replication_policy_mentions:** what the policy is, if replication are directly mentioned,, categories "Accept and encourage", "Accept with neutral/no position", "Accept but discourage", "Not accept"
**accept_category_mentions:** if policy was accept, in what form are the replications accepted, categories "Generally accepts", "Special article type", "TOP 3: RR"
**replication_policy_not:** what the policy is, if replication not directly mentioned, categories "Accept and encourage", "Accept with neutral/no position", "Accept but discourage", "Not accept"
**accept_category_not:** if policy was accept, in what form are the replications accepted, categories "Generally accepts", "Special article type", "TOP 3: RR"
**novelty:** whether novelty language was used
**novelty_text:** example text of novelty language
**novelty_URL:** location of novelty language
**Translation:** whether translation software was used
**JIF:** 2023 journal impact factor 


## R/

### policy_figs.R
Generates summary statistics reported in the manuscript, figures 1-3 and the results from the journal impact factor model. 
File assumes that the root directory when running the code is the 'replication_policy' directory.


### Software Used
R v4.4.0

### Packages used 
viridis_0.6.5
viridisLite_0.4.2
beeswarm_0.4.0
scales_1.4.0


## Figures
Figure_replication_policy.png
Figure_novel_policy.png
Figure_JIF.png


