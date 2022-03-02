# NCGA session ban data

To examine the effectiveness of North Carolina's ban on some types of political contributions during legislative sessions, WRAL News analyzed [campaign finance reports](https://www.ncsbe.gov/Campaign-Finance/report-search) filed with the State Board of Elections and Ethics Enforcement for the first and second quarters of 2018 as of mid-July.

We focused our analysis on donations to candidate committees for members of the General Assembly. Although the legislature has 170 members, only about 150 candidate committees submitted campaign finance data for the first or second quarters. Remaining members either received no contributions, didn't receive enough to require electronic filing of their donation data, or in some cases, violated rules requiring e-filing their data for cumulative donations of more than $10,000.

The data also includes seven party committees that can receive donations during legislative sessions:

* N.C. Democratic Party
* N.C. Democratic Party building fund
* N.C. Republican Party
* N.C. Republican Party building fund
* N.C. Senate Majority Fund
* N.C. House Republican Campaign Committee
* N.C. Republican Council of State Committee
 
Some of these Republican committees had no Democratic counterpart.

Political contributions often change hands between candidate and party committees as politicians raise money and adjust strategies for campaigning, leading to inflated contribution totals. To prevent this double-counting, our analysis removed all contributions between candidate and party committees, giving us the most conservative estimate of political contributions.

Refunds to donors were also excluded from the total.

The analysis reveals that candidate committees drew in about $1.2 million during the legislative short session from May 16 to June 29. The seven party committees brought in about $2.3 million.

To tally up total contributions from the first half of the year from specific donors, WRAL News used the [Open Refine](http://openrefine.org/) software to detect and normalize variations in spellings of contributor names, which are often messy in raw campaign finance data (the Blue Cross Blue Shield PAC, for example, appears in the data as "BLUEPAC" and several other ways). In the data provided, these normalized spellings are contained in the "name-clean" field.

## Get the data

* [Donors to candidate and party committees, 1/3/2018-7/1/2018 (Excel)](https://github.com/wraldata/session-ban-data/raw/master/donors0103-0701.xlsx)
* [Donors to candidate and party committees, 1/3/2018-7/1/2018 (CSV)](https://github.com/wraldata/session-ban-data/raw/master/donors0103-0701.csv)

## Read the story

* [CEOs gave heavily during legislative session, exposing loophole in NC's fundraising ban](http://wral.com/17716104)
* [Methodology](http://wral.com/17745001)
