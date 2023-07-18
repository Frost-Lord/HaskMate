$destinationDir = "$env:APPDATA\Haskmate"
$sourceExe1 = ".\Haskmate.exe"
$sourceExe2 = ".\.stack-work\dist\274b403a\build\HaskMate\Haskmate.exe"

# Check if Haskmate directory is already in the PATH environment variable
$existingPath = [Environment]::GetEnvironmentVariable("PATH", "User")
$hasHaskmatePath = $existingPath -like "*$destinationDir*"

# Create the Haskmate directory if it doesn't exist
if (-not (Test-Path $destinationDir)) {
    New-Item -ItemType Directory -Path $destinationDir -Force | Out-Null
}

# Check if Haskmate.exe already exists in the destination directory
$destinationExe = Join-Path -Path $destinationDir -ChildPath "Haskmate.exe"
if (Test-Path $destinationExe) {
    Write-Host "Replacing existing Haskmate.exe in the Haskmate directory."
    Remove-Item -Path $destinationExe -Force
}

# Copy the Haskmate executable to the destination directory
$sourcePath = $sourceExe1
if (-not (Test-Path $sourcePath)) {
    Write-Host "Running from development environment."
    $sourcePath = $sourceExe2
}

if (Test-Path $sourcePath) {
    Copy-Item -Path $sourcePath -Destination $destinationExe -Force
} else {
    Write-Host "Unable to find the Haskmate.exe file in the expected locations."
    Write-Host "Please make sure the Haskmate executable is present and try again."
    pause
    exit
}

# Add the Haskmate directory to the PATH environment variable if not already present
if (-not $hasHaskmatePath) {
    $newPath = $existingPath + ";" + $destinationDir
    [Environment]::SetEnvironmentVariable("PATH", $newPath, "User")
    Write-Host "Haskmate directory added to the PATH environment variable."
}

# Display a message to confirm the setup
Write-Host "Haskmate setup complete."
Write-Host "Haskmate executable is located in: $destinationDir"
Write-Host "You can now run Haskmate from anywhere in the command prompt."
pause