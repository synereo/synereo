Add-Type -AssemblyName System.IO.Compression.FileSystem

# RabbitMQ
# https://github.com/appveyor/ci/issues/307

$rabbitExePath = "$($env:USERPROFILE)\rabbitmq-server-2.7.1.exe"

if (!(Test-Path -Path $rabbitExePath)) {
    Write-Host "Downloading RabbitMQ..." -ForegroundColor Cyan
    (New-Object Net.WebClient).DownloadFile('http://www.rabbitmq.com/releases/rabbitmq-server/v2.7.1/rabbitmq-server-2.7.1.exe', $rabbitExePath)
}

Write-Host "Installing RabbitMQ..."
cmd /c start /wait $rabbitExePath /S

$rabbitPath = 'C:\Program Files (x86)\RabbitMQ Server\rabbitmq_server-2.7.1'

Write-Host "Installing service..."
Start-Process -Wait "$rabbitPath\sbin\rabbitmq-service.bat" "install"

Write-Host "Starting service..."
Start-Process -Wait "$rabbitPath\sbin\rabbitmq-service.bat" "start"

Get-Service "RabbitMQ"

Write-Host "RabbitMQ installed and started" -ForegroundColor Green

# SBT

$sbtZipPath = "$($env:USERPROFILE)\sbt-0.13.12.zip"

if (!(Test-Path -Path $sbtZipPath)) {
    Write-Host "Downloading sbt.." -ForegroundColor Cyan
    (new-object System.Net.WebClient).DownloadFile('https://dl.bintray.com/sbt/native-packages/sbt/0.13.12/sbt-0.13.12.zip', $sbtZipPath)
}

Write-Host "Installing sbt..."

[System.IO.Compression.ZipFile]::ExtractToDirectory($sbtZipPath, "C:\")

Write-Host "sbt installed" -ForegroundColor Green