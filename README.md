# About 

The 2021 RLadies Philly datathon aims to connect and enable R enthusiasts in the Philadelphia region (and beyond!) to learn and collaborate while also making a difference in the broader Philadelphia community. This year, we have partnered with the Judge Accountability Table (JAT) to explore judicial patterns in Philadelphia courts and better understand trends through data. Everyone is welcome to participate, and there is no minimum requirement for knowledge about the court system or R proficiency.  

The [Judge Accountability Table (JAT)](https://judgeaccountabilitytable.org/) is a coalition of organizations working toward a shared mission of holding judicial candidates and judges accountable to our community’s vision of justice. The organizations of the JAT include: Reclaim Philadelphia, Project SAFE, Philadelphia DSA – LILAC, 215 People’s Alliance, Philadelphia Community Bail Fund, Youth Art and Self-Empowerment Project, Amistad Law Project, Coalition to Abolish Death By Incarceration (CADBI), The Center for Carceral Communities, ICE out of Courts, Pennsylvania Student Power Network, Make The Road Pennsylvania, DecarceratePA, Black Lives Matter Philadelphia. For this datathon, JAT have made available a dataset of de-identified information related to public court dockets for defendants in Philadelphia County, PA. We hope that this datathon will further their efforts to:

- Explore and describe this rich, messy data more completely (not only by answering specific questions, but also by discovering what additional information would be required to be able to answer these questions)
- Understand best approaches to stratifying and analyzing data
- Understanding predictive factors in carceral outcomes

# Structure

- **March 17, 2021, 6-8pm: [Kickoff Meetup](https://www.meetup.com/rladies-philly/events/276365040/):** 
    - First hour: R-Ladies and JAT intros, problem background, Q&A with partner
    - Second hour:  Logistics, data overview, breakout rooms by teams, where participants will be able to join a team and set up a plan of action
    - [[Slides](https://docs.google.com/presentation/d/1iMutdI_1nyvegZ1lKVgd4EW4TguGAUMklNsFt--L8NU/edit?usp=sharing)]
    - [[Recording](https://www.youtube.com/watch?v=sV4RylIrZ6w)]

- **March 18 - April 19**: 
    - Teams work together using Slack, GitHub, and any other online platform of their choosing (zoom, google meet, etc.). RLadies Philly organizers can set up meetings for teams by request (just contact us on Slack!)
    - Questions to JAT will be compiled into an ongoing [Google Doc](https://docs.google.com/document/d/1UdI0oRUrbXchwpvy8b0CZcKv_3rgBV0YUF1cUix2lyo/edit?usp=sharing) that will be shared with JAT; JAT will answer questions asynchronously; RLadies Philly organizers will be available on Slack to answer any non-JAT questions
    - While we recommend weekly meetings for teams, teams have the flexibility to set up their workflow as they see fit

 - **April 20:** 
     - Teams should finalize their work and draft a short presentation (approx. 10-15 mins; TBA) of their most important findings for JAT (see [presentation guidelines and template](https://github.com/rladiesPHL/2021_datathon/tree/main/administrative)
     - Final reports should be drafted (see [final report guidelines and template](https://github.com/rladiesPHL/2021_datathon/tree/main/administrative)

- **April 28, 2021, 6-8pm: [Conclusion Meetup](https://www.meetup.com/rladies-philly/events/276365054/):** 
    - Teams will present their results and discuss their experiences. Each team will have the freedom to choose one or more group members to present the team's results, or to request that an R-Ladies Philly organizer presents the team's results if no group members are available to present. The latter option requires some advance coordination.
    - Teams will finalize their reports

# Teams

- **Team 1: Data Visualization**: Visualize judicial patterns at various levels of abstraction, and if possible, create an interactive dashboard:
    - Levels of abstraction: court type, judge name, defendant demographics, crime severity
    - Patterns of interest: 
        - Process: time to disposition 
        - Bail: amount, % misdemeanor cases with return on recognizance (ROR), % of cases that received reductions in bail or changes in bail type
        - Disposition: sentence length
        - Time trends: punitive sentencing, bail decisions for particular types of offenses


- **Team 2: Defining and quantifying judge "harshness**: Conduct research and analyze data to propose metrics and a process for quantifying how harsh a judge is, considering that judges are presented with different kinds of cases (e.g., offense severity). Consider:
    - if and how to stratify cases by severity (or some other characteristic)
    - how to achieve a fair comparison between judges


- **Team 3: Understanding Systemic Trends**: Examine how things have changed in the courts since [Larry Krasner](https://en.wikipedia.org/wiki/Larry_Krasner) took office in January 2018. Some example trends of interest:
    - Number of people charged overall and by type of charge
    - Harshness in bail and/or sentencing
    - Number of defendants who fail to pay low monetary bail
    - Trends in case dismissals, probation, parole, etc.

# Data

The data includes public court docket information between 2010-2020. Because of the large volume of data, the datasets are available as large total files, or as files per year. Please go to the [data page](https://github.com/rladiesPHL/2021_datathon/blob/main/data/data_description.md) to get more context, data description, and the data download links. The following 3 datasets serve as the basis for this datathon:

- **defendant_docket_details.csv**: contains details about a [docket](#docket) (i.e., a "case"), including basic demographics on the defendant as well as the history of the case at a broad level (eg when the defendant was arrested, the court and municipality where the case was processed, whether the case is still active or has been adjudicated, and what type of representation the defendant had). 
- **offenses_dispositions.csv**: provides information on the offenses on any given docket, and the dispositions associated with each sentence. One docket can include one or more offenses, of which one or more can receive a disposition (ie, a decision). The disposition can also include a sentence type, a sentence duration, and a minimum and maximum sentence duration
- **bail.csv**: includes information on the [bail](#bail) history of a docket. Bail is set per docket (not per individual offense). There are multiple actions that occur in relation to bail, e.g. bail is first set, bail is increased/decreased, bail type is changed, or revoked, etc. 


# Code of Conduct

R-Ladies is dedicated to providing a harassment-free experience for everyone. We do not tolerate harassment of participants in any form. Please refer to our [code of conduct](https://github.com/rladies/starter-kit/wiki/Code-of-Conduct) for more information. Please do not hesitate to contact R-Ladies Philly organizers at philly@rladies.org or via Slack if you have any questions or concerns. 

# FAQ

## Judicial System

**Where can I learn more about the PA Court System?**

- Learn about [the PA court system, judicial elections, and the judge disciplinary system](https://docs.google.com/presentation/d/1t05PTMmmt31qIkAIigQJDdabs7eaA-JULlhYkBDNfAo/edit#slide=id.p)

**What if I have questions about the data or about court processes/legal terms?**

- JAT have offered to answer questions to the best of their ability via a [google sheet](https://docs.google.com/document/d/1UdI0oRUrbXchwpvy8b0CZcKv_3rgBV0YUF1cUix2lyo/edit). Please write your questions in the shared google doc, and JAT volunteers will answer them asynchronously (but regularly). 

## Tools

**I am new to R. How do I get started?**

- [Install R and R Studio](https://rstudio-education.github.io/hopr/starting.html)
- Watch the ["Intro to R" webinar by Tess Cherlin](https://youtu.be/80VIvZZegY8?t=1297) on R-Ladies Philly YouTube Channel
- [Download the data](https://github.com/rladiesPHL/2021_datathon/blob/main/data/data_links.md)
- Refer to [Rstudio Cheatsheets](https://rstudio.com/resources/cheatsheets/) for quick solutions to coding questions
- Ask for help on the [RLadies-Philly Slack](https://bit.ly/join-rladies-slack-2020) #help channel

**I've never used Git and GitHub before. What is it and how do I use it?**

- Use this guide to [get started with Git and GitHub](https://happygitwithr.com/index.html)
- Use this [step-by-step guide](https://docs.google.com/document/d/1vF7uWo2ITXcifyNoLd8ZTJdNPx0Pd4eXXrdkKVfbAkY/edit) to start participating in the collaborative work for this datathon 
- Ask for help on the [RLadies-Philly Slack](https://bit.ly/join-rladies-slack-2020) #help channel

**I have a question and I can't find the answer in these resources. Where do I go for help?**

- Ask for help on the [RLadies-Philly Slack](https://bit.ly/join-rladies-slack-2020) #help channel or in the datathon slack channel #datathon2021

## Process

**I missed the kickoff event. Can I still participate?**

If you weren't able to attend our [Kick-off Meetup](https://www.meetup.com/rladies-philly/events/276365040/), here's how to get involved:

1. Watch the kickoff event [video](https://youtu.be/sV4RylIrZ6w) and take a look at the [slides](https://docs.google.com/presentation/d/1iMutdI_1nyvegZ1lKVgd4EW4TguGAUMklNsFt--L8NU/edit?usp=sharing) introducing the project
2. Take a look at the [github repo](https://github.com/rladiesPHL/2021_datathon) for the project. Read the [readme](README.md)!
3. If you don’t already have one, create a [github account](https://github.com/join) (see our [github workflow recommendations](https://docs.google.com/document/d/1vF7uWo2ITXcifyNoLd8ZTJdNPx0Pd4eXXrdkKVfbAkY/edit?usp=sharing)).
4. If you haven’t already, install [R](https://www.r-project.org/) and [R-Studio](https://www.rstudio.com/products/rstudio/download/#download) on your computer.
5. If you haven’t already, join the [R-Ladies Philly slack](https://join.slack.com/t/rladies-philly/shared_invite/zt-92p8xec5-XOHRmHtmhYQRaVqmrshCcA).
    + In the R-Ladies Philly slack, join the **#2021_datathon** channel (to join channels in slack, click on the *channels* title in the left side bar).
6. Add your details under the team you want to join in [this google doc](https://docs.google.com/document/d/1U6tHOrF_ikBtdFJSj0aT9yfp9jnFqZYIsxcgKBOH9qU/edit?usp=sharing). Don't worry about group size; the more the merrier!
7. Join your team’s specific slack channel (listed in the google doc above) and introduce yourself to your team members! :smiley: Share your ideas or ask how you can get started.

If you have any questions don't hesitate to get in touch. Send a message in the **#2021_datathon* slack channel or email the organisers (philly@rladies.org)!

**How do I contact a team?**

- Participants will add their names, RLadies Philly Slack IDs and GitHub IDs to [this list](https://docs.google.com/document/d/1U6tHOrF_ikBtdFJSj0aT9yfp9jnFqZYIsxcgKBOH9qU/edit?usp=sharing) (new rows can be added), according to the team they are in. Once you join the RLadies Philly slack channel, you can reach out to any of these members directly, or join the #datathon2021 channel and address all participants.  

