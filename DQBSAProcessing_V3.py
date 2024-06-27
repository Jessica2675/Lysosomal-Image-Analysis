#@ File    (label = "Input directory", style = "directory") srcFile
#@ File    (label = "Output directory", style = "directory") dstFile
#@ String  (label = "File extension", value=".tif") ext
#@ String  (label = "File name contains", value = "") containString
#@ boolean (label = "Keep directory structure when saving", value = true) keepDirectories

# See also Process_Folder.ijm for a version of this code
# in the ImageJ 1.x macro language.

import os
from ij import IJ, ImagePlus
from ij import IJ, ImagePlus
from ij.plugin import ChannelSplitter
from ij.gui import Roi, Overlay
from ij.plugin.frame import RoiManager
from ij.measure import ResultsTable
from ij import WindowManager

def run():
  srcDir = srcFile.getAbsolutePath()
  dstDir = dstFile.getAbsolutePath()
  for root, directories, filenames in os.walk(srcDir):
    filenames.sort();
    for filename in filenames:
      # Check for file extension
      if not filename.endswith(ext):
        continue
      # Check for file name pattern
      if containString not in filename:
        continue
      process(srcDir, dstDir, root, filename, keepDirectories)
 
def process(srcDir, dstDir, currentDir, fileName, keepDirectories):
  print "Processing:"
   
  # Opening the image
  print "Open image file", fileName
  image = IJ.openImage(os.path.join(currentDir, fileName))
  channels = ChannelSplitter.split(image)
    
    # Process the first channel by deleting the second and third slices

  imp = image.duplicate()
  imp.show()
  IJ.run("Next Slice [>]")
  IJ.run("Delete Slice")
  IJ.run("Next Slice [>]")
  IJ.run("Delete Slice")
  # Put your processing commands here!


# Step 1: Open the desired confocal image (.lsm file format)
#file_path = IJ.getFilePath("Select a .lsm file")
#imp = IJ.openImage(file_path)
#imp.show()

# Get the current image
  
  IJ.run(imp, "Subtract Background...", "rolling=12")


  name =  imp.getTitle()
  print(str(name))
# Define the macro as a string
  macro = """
  run("Command From Macro", "command=[de.csbdresden.stardist.StarDist2D], args=['input':""" + str(name) + """, 'modelChoice':'Versatile (fluorescent nuclei)', 'normalizeInput':'true', 'percentileBottom':'1.0', 'percentileTop':'99.8', 'probThresh':'0.05', 'nmsThresh':'0.4', 'outputType':'Both', 'nTiles':'1', 'excludeBoundary':'2', 'roiPosition':'Automatic', 'verbose':'false', 'showCsbdeepProgress':'false', 'showProbAndDist':'false'], process=[false]");
  """

# Run the macro
  IJ.runMacro(macro)


  IJ.selectWindow(str(name))
# Get the ROI Manager
  roi_manager = RoiManager.getInstance()
  if roi_manager is None:
    roi_manager = RoiManager()

# Show all ROIs
  roi_manager.runCommand("Show All")

# Save the ROIs to a file
  roi_save_path = "D:/Jessica/Drug Discovery/dqbsa processing/results/plate 13/"+str(name)+"RoiSet.zip"
  roi_manager.runCommand("Save", roi_save_path)

# Measure the ROIs
  roi_manager.runCommand("Measure")
  
  #add image name to the table for the rois
  num_rois = roi_manager.getCount()
  results_table = ResultsTable.getResultsTable()
  if results_table is not None:
    for i in range(results_table.size()-num_rois, results_table.size(), 1):
        results_table.setValue("Image Name", i, name)
  
  if roi_manager is not None:
    roi_manager.reset()

  # Get a list of all open windows
  window_list = WindowManager.getIDList()
 
  # Close the label image window if it exists
  if window_list is not None:
    for window_id in window_list:
        window = WindowManager.getImage(window_id)
        if window is not None and window.getTitle() == "Label Image":
            window.close()
            break  # Exit the loop after closing the window

  # Saving the image
  saveDir = currentDir.replace(srcDir, dstDir) if keepDirectories else dstDir
  if not os.path.exists(saveDir):
    os.makedirs(saveDir)
  print "Saving to", saveDir
  IJ.saveAs(imp, "Tiff", os.path.join(saveDir, fileName));
  imp.close()
 
run()
# Save the results to a CSV file
results_save_path = "D:/Jessica/Drug Discovery/dqbsa processing/results/plate 13/Results.csv"
results_table = ResultsTable.getResultsTable()
results_table.save(results_save_path)
print("csv code run")