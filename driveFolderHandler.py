import gdown
def downloadTempDriveFolder(folderID, path):
    '''
    Takes the folder ID of a publicly accessible Drive folder and 
    downloads the files within to /temp-docs to the provided path.
    Drive folder ID is found by copying the sharing link and removing everything 
    except the string of characters between 'https://drive.google.com/drive/folders/' and '?usp=drive_link' 
    '''
    gdown.download_folder(id= folderID, 
                          output= path, 
                          quiet= False, 
                          use_cookies= False)
    print("Files stored in " + path)
    

import shutil
def deleteTempDriveFolder(path):
    '''Deletes the folder and files created by downloadTempDriveFolder()'''
    shutil.rmtree(path)
    print("Deleted folder and all files within " + path)