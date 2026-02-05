$ErrorActionPreference = "Stop"

$url = "https://sourceforge.net/projects/gnucobol/files/gnucobol/3.2/gnucobol-3.2_win.zip/download"
$zipPath = "gnucobol.zip"
$extractPath = "tools\gnucobol"

Write-Host "Downloading GnuCOBOL from $url..."
Invoke-WebRequest -Uri $url -OutFile $zipPath -UserAgent "Mozilla/5.0"

if (-not (Test-Path $zipPath)) {
    Write-Error "Download failed!"
    exit 1
}

Write-Host "Extracting to $extractPath..."
if (Test-Path $extractPath) {
    Remove-Item $extractPath -Recurse -Force
}
Expand-Archive -Path $zipPath -DestinationPath "tools" -Force

# Rename if extracted folder has version number
$extractedItems = Get-ChildItem "tools" | Where-Object { $_.PSIsContainer }
if ($extractedItems.Count -eq 1 -and $extractedItems.Name -ne "gnucobol") {
    Rename-Item $extractedItems.FullName "gnucobol"
}

Remove-Item $zipPath

Write-Host "GnuCOBOL setup complete in $extractPath"
Write-Host "Binaries should be in $extractPath\bin"
