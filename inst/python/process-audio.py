import os
import os.path
import pandas



def py_check_audio(audio_path):
    # Verify the path is valid
    check_file = os.path.isfile(audio_path) and os.path.exists(audio_path)
    return(check_file)
