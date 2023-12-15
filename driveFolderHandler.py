import gdown
def downloadTempDriveFolder(folderID):
    '''
    Takes the folder ID of a publicly accessible Drive folder and 
    downloads the files within to /temp-docs one folder up from the current directory.
    Drive folder ID is found by copying the sharing link and removing everything 
    except the string of characters between 'https://drive.google.com/drive/folders/' and '?usp=drive_link' 
    '''
    gdown.download_folder(id= folderID, 
                          output= "../temp-docs", 
                          quiet= False, 
                          use_cookies= False)
    print("Files stored in /temp-docs")
    

import shutil
def deleteTempDriveFolder():
    '''Deletes the folder and files created by downloadTempDriveFolder()'''
    shutil.rmtree('../temp-docs')
    print("Deleted folder and all files within /temp-docs")