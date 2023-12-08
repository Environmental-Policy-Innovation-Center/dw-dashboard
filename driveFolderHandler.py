

#currently a dummy test folder, needs to be updated to include each state folder for each year
folder_ids = {
    'year1/AL':'16GVk_dJuAlg42fo7u5rcCdyr725Y6hVj'
}

import gdown
def downloadTempDriveFolder(path):
    gdown.download_folder(id= folder_ids.get(path), 
                          output= path + "/temp-docs", 
                          quiet= False, 
                          use_cookies= False)
    print("Files stored in " + path + '/temp-docs')
    

import shutil
def deleteTempDriveFolder(path):
    shutil.rmtree(path + '/temp-docs')
    print("Deleted folder and all files within " + path + '/temp-docs')