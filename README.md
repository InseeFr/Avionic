**Avionic Model**

The Avionic model is an input/output model that was developed at INSEE's National Accounts Department. This department produces summary tables that describe the national economy, such as the input-output table (IOT), which analyses each of the economy's products according to its origin (national production or imports) and its destination (final consumption, exports, investments). At its core, the IOT (input-output table) identifies the intermediate consumption of each branch of the economy in each product of the economy. It is therefore a valuable tool for tracing the interrelationships within the economy. In addition, over the last fifteen years or so, international organisations have developed inter-country IOTs by reconciling foreign trade data, thus making it possible to analyse global value chains.
The programs have been developed in R, source data and other tables are in CSV or R format.
 
Contents :

- R programs. The main one is tuto_avionic.R. It calls the other programs when required
- license
- avionic.pdf : a french tutorial to learn how to use the programs
- data from insee.fr that can be used for testing tuto_avionic.R
- Unfortunately, data from OECD is too heavy to be uploaded here, if you want a R version of these tables, let us know, we'll work it out

There is also a tutorial video here : https://www.youtube.com/watch?v=0zQVsNSFp3c&feature=youtu.be

**How to use it**

You can use the programs directly. You can find here data you can use to test the model. To help you, we made both a written tutorial and a video tutorial. There are both in French for now. You can apply the fonctions on all sorts of data, feel free to try on any country for instance. You just need an input-output table to make it work.

**Who maintains it**

We are a team at Insee (the French national statistical institute), working on national accounts.

**What's next**

Our next steps : 

- Releasing a full R package for Avionic
- Translating tutorials 
- Working on mixed model to simulate a variation of production

**How to get in touch**

You can contact us by email or here on github, feel free to ask us questions and please, share your work with us


