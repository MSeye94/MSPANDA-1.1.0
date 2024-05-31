import os
import fnmatch as fnmatch


def exportMsdialParam(export_param,
output_export_param,
MS1_type,
MS2_type,
ion, 
rt_begin, 
rt_end, 
mz_range_begin, 
mz_range_end, 
mz_tolerance_centroid_MS1, 
mz_tolerance_centroid_MS2,
maxCharge, 
min_Peakwidth, 
min_PeakHeight, 
mass_slice_width, 
min_PeaksMassif,
Adduct_list,
bw,
rt_tolerance,
mass_tolerance,
min_iset,
cutrat,
Ref):
  paramMsdial_Used = open(export_param, 'r')
  # if(not paramMsdial.closed):
  #   print(paramMsdial.name)
  fileContent = paramMsdial_Used.readlines()
  fileContent[0] = fileContent[0].replace("Type-ref",Ref)
  fileContent[5] = fileContent[5].replace("MS1_type",MS1_type)
  fileContent[6] = fileContent[6].replace("MS2_type",MS2_type)
  fileContent[7] = fileContent[7].replace("ion", ion)
  fileContent[8] = fileContent[8].replace("rt_begin",rt_begin)
  fileContent[9] = fileContent[9].replace("rt_end",rt_end)
  fileContent[10] = fileContent[10].replace("mz_range_begin",mz_range_begin)
  fileContent[11] = fileContent[11].replace("mz_range_end",mz_range_end)
  fileContent[14] = fileContent[14].replace("mz_tolerance_centroid_MS1",mz_tolerance_centroid_MS1)
  fileContent[15] = fileContent[15].replace("mz_tolerance_centroid_MS2",mz_tolerance_centroid_MS2)
  fileContent[18] = fileContent[18].replace("maxCharge",maxCharge)
  fileContent[19] = fileContent[19].replace("min_Peakwidth",min_Peakwidth)
  fileContent[20] = fileContent[20].replace("min_PeakHeight",min_PeakHeight)
  fileContent[21] = fileContent[21].replace("mass_slice_width",mass_slice_width)
  fileContent[22] = fileContent[22].replace("min_PeaksMassif",min_PeaksMassif)
  fileContent[25] = fileContent[25].replace("[M+H]+", ",".join(Adduct_list))
  # fileContent[33] = fileContent[33].replace("[M+H]+,[M+Na]+,[M+K]+", ", ".join(Adduct_list))
  fileContent[32] = fileContent[32].replace("bw", bw)
  fileContent[33] = fileContent[33].replace("mass_tolerance", mass_tolerance)
  fileContent[36] = fileContent[36].replace("rt_tolerance", rt_tolerance)
  fileContent[39] = fileContent[39].replace("min_iset", min_iset)
  fileContent[40] = fileContent[40].replace("cutrat", cutrat)
  
  
  with open(output_export_param, 'w') as temp_file:
    for item in fileContent:
      temp_file.write("%s" % item)
  #file = open('output_param', 'r')
  #print(file.read())
  #file.close() 
  paramMsdial_Used.close() 
  
  

