# MSPANDA-1.1.0
MS-PANDA is an untargeted omics data analysis tool.
It makes it possible to build a set of reference peptides or metabolites,
detected by CE/MS or LC/MS, from the analysis samples of a given biological liquid (plasma, urine, etc.).
This reference set is then used to match and normalize peptides or metabolites from a new sample.
It makes it possible to compare protein expression between different conditions,
to look for possible biomarkers associated with a pathology, a condition, a treatment
 
## The MS-PANDA tool consists of two parts:
1. New reference map: allows the development of a new peptide or metabolite reference map of a given biological liquid.
This reference map is then used to identify a set of intensity-stable particles.
2. Analysis new sample: a second part that allows to detect, match and normalize the peptides or metabolites of a new sample on the reference map.

## How to launch MSPANDA-1.1.0
### Prerequisites:
1. Download and install R and RStudio Desktop.
2. Download the MSPANDA app from the GitHub page.
3. Download the following applications:
	MSDIAL ver.4.80 Windows: https://zenodo.org/records/12540725.
	dotnet-sdk-6.0.301-win-x64.exe: https://dotnet.microsoft.com/en-us/download/dotnet/6.0.
4. Change Vautre Computer Locale to: Etat-unis

### Launch the MSPANDA application:
1. Install Microsoft dotnet-sdk-6.0.301-win-x64.exe.
2. Unzip the downloaded MSPANDA-1.1.0 application.
3. Unzip the MSDIAL ver.4.80 Windows application, then create a folder (MSDIAL) in MSPANDA-1.1.0 lib and copy in "MSDIAL ver.4.80 Windows".
4. Launch MSPANDA-1.1.0.Rproj: MSPANDA-1.1.0 MSPANDA-1.1.0.Rproj.

