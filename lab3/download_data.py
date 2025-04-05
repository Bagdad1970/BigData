import kagglehub

# Download latest version
path = kagglehub.dataset_download("the-guardian/olympic-games")

print("Path to dataset files:", path)
