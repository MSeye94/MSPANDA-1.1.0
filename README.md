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

## How to launch the MSPANDA app
### Prérequis :
1. Télécharger et installer R et RStudio Desktop.
2. Télécharger l'application MSPANDA depuis la page du GitHub
3. Télécharger les applications suivants :
	MSDIAL ver.4.80 Windows : https://zenodo.org/records/12540725
	dotnet-sdk-6.0.301-win-x64.exe : https://dotnet.microsoft.com/en-us/download/dotnet/6.0
4. Changer les paramètres régionaux de vautre ordinateur en : Etats-Unis

### Lancer l'application MSPANDA :
1. Installer l'application Microsoft dotnet-sdk-6.0.301-win-x64.exe.
2. Dézipper l'application MSPANDA-1.1.0 téléchargée.
3. Dézipper l'application MSDIAL ver.4.80 Windows, puis crées un dossier (MSDIAL) dans MSPANDA-1.1.0\lib et copier dedans "MSDIAL ver.4.80 Windows".
4. Lancer le fichier MSPANDA-1.1.0.Rproj : MSPANDA-1.1.0\MSPANDA-1.1.0.Rproj

