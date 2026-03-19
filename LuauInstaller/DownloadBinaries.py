import urllib.request
import zipfile
import os
import sys

url = "https://github.com/luau-lang/luau/releases/download/0.712/luau-windows.zip"
zip_path = "luau-windows.zip"
extract_folder = "Binaries"

def progress_bar(block_num, block_size, total_size):
    downloaded = block_num * block_size
    percent = min(downloaded / total_size, 1.0)

    bar_length = 40
    filled = int(bar_length * percent)
    bar = "#" * filled + "-" * (bar_length - filled)

    sys.stdout.write(f"\r[{bar}] {int(percent * 100)}%")
    sys.stdout.flush()

urllib.request.urlretrieve(url, zip_path, reporthook=progress_bar)


print()

os.makedirs(extract_folder, exist_ok=True)


with zipfile.ZipFile(zip_path, 'r') as zip_ref:
    zip_ref.extractall(extract_folder)

# Delete ZIP
os.remove(zip_path)