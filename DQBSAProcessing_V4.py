#@ File    (label = "Input directory", style = "directory") srcFile
#@ File    (label = "Output directory", style = "directory") dstFile
#@ String  (label = "File extension", value=".tif") ext
#@ String  (label = "File name contains", value = "") containString
#@ boolean (label = "Keep directory structure when saving", value = true) keepDirectories

import os
from ij import IJ, ImagePlus
from ij.gui import Roi, Overlay
from ij.plugin.frame import RoiManager
from ij.measure import ResultsTable
from ij import WindowManager

def run():
    srcDir = srcFile.getAbsolutePath()
    dstDir = dstFile.getAbsolutePath()
    for root, directories, filenames in os.walk(srcDir):
        filenames.sort()
        for filename in filenames:
            # Check for file extension
            if not filename.endswith(ext):
                continue
            # Check for file name pattern
            if containString not in filename:
                continue
            process(srcDir, dstDir, root, filename, keepDirectories)

def process(srcDir, dstDir, currentDir, fileName, keepDirectories):
    print("Processing:")
    
    # Opening the image
    print("Open image file", fileName)
    image = IJ.openImage(os.path.join(currentDir, fileName))
    
    # make duplicate of the original image to avoid changing the original
    imp = image.duplicate()
    name = imp.getTitle()
    print(str(name))
    
    # get stack and define dqbsa and dapi slices as ImagePlus objects
    stack = imp.getStack()
    dqbsa = ImagePlus("dqbsa", stack.getProcessor(1))
    dapi = ImagePlus("dapi", stack.getProcessor(3))
    
    # rolling ball background subtraction for dqbsa slice  
    dqbsa.show()
    IJ.run(dqbsa, "Subtract Background...", "rolling=12")
    dqbsa.setCalibration(imp.getCalibration())  # Set calibration from the original image 'imp' (idk why i have to manually specify this for dqbsa but not dapi image)
        
    # Saving the background subtracted dqbsa processed image
    saveDir = currentDir.replace(srcDir, dstDir) if keepDirectories else dstDir
    if not os.path.exists(saveDir):
        os.makedirs(saveDir)
    print("Saving to", saveDir)
    IJ.saveAs(dqbsa, "Tiff", os.path.join(saveDir, fileName))

    # lysosome analysis
    lysosome_num = lysosome_measure(dqbsa.getTitle(), name, saveDir)
    
    # nuclei analysis
    dapi.show()
    nuclei_num = nuclei_count(dapi.getTitle(), name, saveDir)
    
    # per cell analysis
    per_cell = lysosome_num / nuclei_num if nuclei_num != 0 else 0
    
    # add per cell results to results table
    per_cell_results.incrementCounter()  # add a new row to the ResultsTable and increment the internal row counter by one
    per_cell_results.addValue("Lysosomes", lysosome_num)
    per_cell_results.addValue("Nuclei", nuclei_num)
    per_cell_results.addValue("Per Cell", per_cell)
    per_cell_results.addValue("Image Name", name)
    per_cell_results.show("Per Cell Results")

	# close windows
    dapi.close()
    dqbsa.close()
	
def lysosome_measure(window, image, roi_save_path):  # takes name of window, name of image, destination file path as input
    # Define the macro to run stardist as a string
    macro = """
    run("Command From Macro", "command=[de.csbdresden.stardist.StarDist2D], args=['input':'""" + window + """', 'modelChoice':'Versatile (fluorescent nuclei)', 'normalizeInput':'true', 'percentileBottom':'1.0', 'percentileTop':'99.8', 'probThresh':'0.05', 'nmsThresh':'0.4', 'outputType':'Both', 'nTiles':'1', 'excludeBoundary':'2', 'roiPosition':'Automatic', 'verbose':'false', 'showCsbdeepProgress':'false', 'showProbAndDist':'false'], process=[false]");"""
    # Run the macro
    IJ.runMacro(macro)
    IJ.selectWindow(window)  # select the right window
    # Get the ROI Manager
    roi_manager = RoiManager.getInstance()
    if roi_manager is None:
        roi_manager = RoiManager()
    # Show all ROIs
    roi_manager.runCommand("Show All")
    # Save the ROIs to a file
    roi_manager.runCommand("Save", os.path.join(roi_save_path, image + "DQBSA_RoiSet.zip"))
    # Measure the ROIs
    IJ.run("Set Measurements...", "area mean")

    max_area = 0.02

    filtered_roi_count = 0
    # Measure the area of each ROI and filter based on area
    for i in range(roi_manager.getCount()):
        roi_manager.select(i)
        IJ.run("Measure")
        rt = ResultsTable.getResultsTable()
        area = rt.getValue("Area", rt.size() - 1)  # Get the last measured area value
        print("yay im at least getting the last area value which is", area)
    
        if area <= max_area:
            filtered_results.incrementCounter()  # add a new row to the ResultsTable and increment the internal row counter by one
            for col in range(rt.getLastColumn() + 1):
                heading = rt.getColumnHeading(col)
                value = rt.getValueAsDouble(col, rt.size() - 1)
                filtered_results.addValue(heading, value)
                filtered_results.addValue("Image Name", image)
            filtered_roi_count += 1  # Increment the count of filtered ROIs in this image

    IJ.selectWindow("Results")
    IJ.run("Close")  # close the unfiltered results table    
    filtered_results.show("Filtered Results")  # show the filtered results
    
    if roi_manager is not None:
        roi_manager.reset()
        
    # Get a list of all open windows and close the label image window if it exists
    window_list = WindowManager.getIDList()
    if window_list is not None:
        for window_id in window_list:
            window = WindowManager.getImage(window_id)
            if window is not None and window.getTitle() == "Label Image":
                window.close()
                break  # Exit the loop after closing the window
                        
    return filtered_roi_count
    
def nuclei_count(window, image, roi_save_path):
    # Define the macro to run stardist as a string
    macro = """
    run("Command From Macro", "command=[de.csbdresden.stardist.StarDist2D], args=['input':'""" + window + """', 'modelChoice':'Versatile (fluorescent nuclei)', 'normalizeInput':'true', 'percentileBottom':'1.0', 'percentileTop':'99.8', 'probThresh':'0.6', 'nmsThresh':'0.9', 'outputType':'Both', 'nTiles':'1', 'excludeBoundary':'2', 'roiPosition':'Automatic', 'verbose':'false', 'showCsbdeepProgress':'false', 'showProbAndDist':'false'], process=[false]");"""
    # Run the macro
    IJ.runMacro(macro)
    IJ.selectWindow(window)
    # Get the ROI Manager
    roi_manager = RoiManager.getInstance()
    if roi_manager is None:
        roi_manager = RoiManager()
    # Show all ROIs
    roi_manager.runCommand("Show All")
    # Save the ROIs to a file
    roi_manager.runCommand("Save", os.path.join(roi_save_path, image + "nuclei_RoiSet.zip"))
    # Measure the ROIs
    IJ.run("Set Measurements...", "area")

    min_area = 0.170

    # Variables to count filtered ROIs
    nuclei_count = 0
    
    # Measure the area of each ROI and filter based on area
    for i in range(roi_manager.getCount()):
        roi_manager.select(i)
        IJ.run("Measure")
        rt = ResultsTable.getResultsTable()
        area = rt.getValue("Area", rt.size() - 1)  # Get the last measured area value
    
        if area >= min_area:
            nuclei_count += 1  # Increment the count of filtered ROIs

    IJ.selectWindow("Results")
    IJ.run("Close")  # close the unfiltered results table
    
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
        
    return nuclei_count
    

filtered_results = ResultsTable()  # set up a results table for filtered lysosomes
per_cell_results = ResultsTable()  # set up a results table for the no. of lysosomes per cell in each image
run()

# Save the results to CSV files
results_save_path = dstFile.getAbsolutePath()
filtered_results.save(results_save_path + "/filtered_lysosome_intensities.csv")
per_cell_results.save(results_save_path + "/lysosomes_per_cell.csv")
print("csv files saved")
# Save the results to a CSV file
results_save_path = "D:/Jessica/Drug Discovery/dqbsa processing/results/plate 13/Results.csv"
results_table = ResultsTable.getResultsTable()
results_table.save(results_save_path)
print("csv code run")
