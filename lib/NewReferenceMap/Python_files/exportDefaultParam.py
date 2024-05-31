import os
import fnmatch as fnmatch


def exportDefaultParam(Input_DefaultParam,
output_DefaultParam,
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
mass_tolerance):
  DefaultParam = open(Input_DefaultParam, 'r')
  # if(not paramMsdial.closed):
  #   print(paramMsdial.name)
  fileContent = DefaultParam.readlines()
  fileContent[3] = fileContent[3].replace("rt_begin",rt_begin)
  fileContent[4] = fileContent[4].replace("rt_end",rt_end)
  fileContent[5] = fileContent[5].replace("mz_range_begin",mz_range_begin)
  fileContent[6] = fileContent[6].replace("mz_range_end",mz_range_end)
  fileContent[9] = fileContent[9].replace("mz_tolerance_centroid_MS1",mz_tolerance_centroid_MS1)
  fileContent[10] = fileContent[10].replace("mz_tolerance_centroid_MS2",mz_tolerance_centroid_MS2)
  fileContent[11] = fileContent[11].replace("maxCharge",maxCharge)
  fileContent[12] = fileContent[12].replace("min_Peakwidth",min_Peakwidth)
  fileContent[13] = fileContent[13].replace("min_PeakHeight",min_PeakHeight)
  fileContent[14] = fileContent[14].replace("mass_slice_width",mass_slice_width)
  fileContent[15] = fileContent[15].replace("min_PeaksMassif",min_PeaksMassif)
  fileContent[16] = fileContent[16].replace("[M+H]+", ",".join(Adduct_list))
  fileContent[20] = fileContent[20].replace("bw",bw)
  fileContent[21] = fileContent[21].replace("mass_tolerance",mass_tolerance)
  
  
  with open(output_DefaultParam, 'w') as temp_file:
    for item in fileContent:
      temp_file.write("%s" % item)
  #file = open('output_param', 'r')
  #print(file.read())
  #file.close() 
  DefaultParam.close() 
